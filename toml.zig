const std = @import("std");
const ascii = std.ascii;
const fmt = std.fmt;
const mem = std.mem;

const testing = std.testing;
const expect = testing.expect;

pub const Node = union {
    String: []const u8,
    Int: i64,
    Bool: bool,
};

pub const ParseError = error {
    FailedToParse,
};

pub const Parser = struct {
    allocator: *std.mem.Allocator,

    pub fn init(allocator: *std.mem.Allocator) !*Parser {
        var parser = try allocator.create(Parser);
        parser.allocator = allocator;
        return parser;
    }

    fn skipSpaces(self: *Parser, input: []const u8, offset: usize) usize {
        var i = offset;
        while (i < input.len and (input[i] == ' ' or input[i] == '\t')) : (i+=1) {}
        return i;
    }

    fn parseTrue(self: *Parser, input: []const u8, offset: usize, node: *Node) ParseError!usize {
        if (offset+4 > input.len) {
            return ParseError.FailedToParse;
        }
        if (!mem.eql(u8, "true", input[offset..(offset+4)])) {
            return ParseError.FailedToParse;
        }
        if (offset+4 < input.len and ascii.isAlNum(input[offset+4])) {
            return ParseError.FailedToParse;
        }
        node.* = Node{.Bool = true};
        return offset+4;
    }
    fn parseFalse(self: *Parser, input: []const u8, offset: usize, node: *Node) ParseError!usize {
        if (offset+5 > input.len) {
            return ParseError.FailedToParse;
        }        
        if (!mem.eql(u8, "false", input[offset..(offset+5)])) {
            return ParseError.FailedToParse;
        }
        if (offset+5 < input.len and ascii.isAlNum(input[offset+5])) {
            return ParseError.FailedToParse;
        }
        node.* = Node{.Bool = false};
        return offset+5;
    }
    fn parseBool(self: *Parser, input: []const u8, offset: usize, node: *Node) ParseError!usize {
        return self.parseTrue(input, offset, node) catch self.parseFalse(input, offset, node);
    }

    fn parseInt(self: *Parser, input: []const u8, offset: usize, node: *Node) !usize {
        var i = offset;
        var start = offset;
        var initial = true;
        var underscore_count: u64 = 0;
        var base: u8 = 10;
        while (i < input.len) : (i+=1) {
            if (initial and (input[i] == '+' or input[i] == '-')) {
                initial = false;
                continue;
            }
            if (initial and input[i] == '0' and i+1 < input.len) {
                var n = ascii.toLower(input[i+1]);
                if (n == 'o' or n == 'x' or n == 'b') {
                    initial = false;
                    i += 1;
                    start+=2;
                    base = switch (n) {
                        'x' => 16,
                        'o' => 8,
                        'b' => 2,
                        else => undefined,
                    };
                    continue;
                }
            }
            initial = false;
            if (input[i] == '_') {
                underscore_count += 1;
                continue;
            }

            const c = input[i];
            if (ascii.isDigit(c) and (c - '0') < base) {
                continue;
            }
            if (base > 10 and ascii.isAlpha(c) and (ascii.toLower(c) - 'a' + 10) < base) {
                continue;
            }
            if (c == ' ' or c == '\t')
                break;
            return ParseError.FailedToParse;
        }
        if (i == offset) {
            return ParseError.FailedToParse;
        }
        var buf: []const u8 = undefined;
        var buf_for_cleanup: []u8 = undefined;
        var buf_allocated = (underscore_count > 0);
        defer if (buf_allocated) {
            self.allocator.free(buf_for_cleanup);
        };
        if (buf_allocated) {
            buf_for_cleanup = try self.allocator.alloc(u8, i-start-underscore_count);
            var j = start;
            var p: usize = 0;
            while (j < i) : (j+=1) {
                if (input[j] != '_') {
                    buf_for_cleanup[p] = input[j];
                    p+=1;
                }
            }
            buf = buf_for_cleanup;
        } else {
            buf = input[start..i];
        }
        node.* = Node{.Int = try fmt.parseInt(i64, buf, base)};
        return i;
    }

    fn parseString(self: *Parser, input: []const u8, offset: usize, node: *Node) !usize {
        if (input[offset] != '"') {
            return ParseError.FailedToParse;
        }
        var start = offset+1;
        var end = start;
        while (end < input.len) : (end += 1) {
            if (input[end] == '"') {
                node.* = Node{.String = input[start..end]};
                return end+1;
            }
        }
        return ParseError.FailedToParse;
    }

    const tokenResult = struct {
        token: []const u8,
        offset: usize
    };
    fn parseToken(self: *Parser, input: []const u8, offset_in: usize) !tokenResult {
        var offset = self.skipSpaces(input, offset_in);
        var i: usize = offset;
        while (i < input.len) : (i+=1) {
            if (!((input[i] >= 'A' and input[i] <= 'Z') or (input[i] >= 'a' and input[i] <= 'z'))) {
                break;
            }
        }
        if (i == offset) {
            return ParseError.FailedToParse;
        }
        return tokenResult{.token = input[offset..i], .offset = i};
    }

    fn parseAssign(self: *Parser, input: []const u8, offset_in: usize, data: *std.StringHashMap(Node)) !usize {
        var key_result = try self.parseToken(input, offset_in);
        var offset = self.skipSpaces(input, key_result.offset);
        if (input[offset] != '=') {
            return ParseError.FailedToParse;
        }
        offset = self.skipSpaces(input, offset+1);
        var n = Node{.Bool = false};
        offset = try (
            self.parseString(input, offset, &n) catch
            self.parseInt(input, offset, &n) catch 
            self.parseBool(input, offset, &n));
        _ = try data.put(key_result.token, n);
        offset = self.skipSpaces(input, offset);
        if (offset < input.len and input[offset] == '\n') {
            offset+=1;
        }
        return offset;
    }

    pub fn parse(self: *Parser, input: []const u8) !std.StringHashMap(Node) {
        var data = std.StringHashMap(Node).init(self.allocator);
        var offset: usize = 0;
        while (offset < input.len) loop: {
            offset = try self.parseAssign(input, offset, &data);
        }
        return data;
    }
};

test "empty" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse("");
    expect(parsed.size == 0);
}

test "simple-kv" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(" \t foo\t=  42   \n");
    defer parsed.deinit();
    expect(parsed.size == 1);
    expect(parsed.get("foo").?.value.Int == 42);
}

test "string-kv" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(" \t foo\t=  \"bar\"   \n");
    defer parsed.deinit();
    expect(parsed.size == 1);
    expect(mem.eql(u8, parsed.get("foo").?.value.String, "bar"));
}

test "parseBool" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var n: Node = .{.Int = 0};
    expect((try parser.parseBool("true", 0, &n)) == 4);
    expect(n.Bool);
    expect((try parser.parseBool("true   ", 0, &n)) == 4);
    expect(n.Bool);
    expect(parser.parseBool("trues", 0, &n) catch |err| e: {
        expect(err == ParseError.FailedToParse);
        break :e 0;
    } == 0);

    expect((try parser.parseBool("false", 0, &n)) == 5);
    expect(!n.Bool);
    expect((try parser.parseBool("false   ", 0, &n)) == 5);
    expect(!n.Bool);
    expect(parser.parseBool("falses", 0, &n) catch |err| e: {
        expect(err == ParseError.FailedToParse);
        break :e 0;
    } == 0);
}

test "parseInt" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var n: Node = .{.Bool = false};
    expect((try parser.parseInt("123", 0, &n)) == 3);
    expect(n.Int == 123);

    expect((try parser.parseInt("-123", 0, &n)) == 4);
    expect(n.Int == -123);

    expect((try parser.parseInt("+123_456_789", 0, &n)) == 12);
    expect(n.Int == 123456789);

    expect((try parser.parseInt("0XFF", 0, &n)) == 4);
    expect(n.Int == 255);

    expect((try parser.parseInt("0Xa", 0, &n)) == 3);
    expect(n.Int == 10);

    expect((try parser.parseInt("0o20", 0, &n)) == 4);
    expect(n.Int == 16);

    expect((try parser.parseInt("0b0100", 0, &n)) == 6);
    expect(n.Int == 4);

    // hexadecimal with underscore.
    expect((try parser.parseInt("0xa_1", 0, &n)) == 5);
    expect(n.Int == 161);

    // invalid octal.
    expect(parser.parseInt("0o9", 0, &n) catch |err| e: {
        expect(err == ParseError.FailedToParse);
        break :e 0;
    } == 0);

    // invalid binary.
    expect(parser.parseInt("0b2", 0, &n) catch |err| e: {
        expect(err == ParseError.FailedToParse);
        break :e 0;
    } == 0);

    // invalid hexadecimal.
    expect(parser.parseInt("0xQ", 0, &n) catch |err| e: {
        expect(err == ParseError.FailedToParse);
        break :e 0;
    } == 0);

    // invalid prefix.
    expect(parser.parseInt("0q0", 0, &n) catch |err| e: {
        expect(err == ParseError.FailedToParse);
        break :e 0;
    } == 0);

    // signs can't be combined with prefix.
    expect(parser.parseInt("+0xdeadbeef", 0, &n) catch |err| e: {
        expect(err == ParseError.FailedToParse);
        break :e 0;
    } == 0);
}

test "parseString" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var n: Node = .{.Bool = false};
    expect((try parser.parseString("\"foo\"", 0, &n)) == 5);
    expect(mem.eql(u8, n.String, "foo"));
}