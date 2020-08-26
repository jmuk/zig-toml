const std = @import("std");
const ascii = std.ascii;
const fmt = std.fmt;
const mem = std.mem;

const testing = std.testing;
const expect = testing.expect;

pub const ValueTag = enum {
    String, Int, Float, Bool, Object, Array,
};

pub const Value = union(ValueTag) {
    String: std.ArrayList(u8),
    Int: i64,
    Float: f64,
    Bool: bool,
    Object: std.StringHashMap(Value),
    Array: std.ArrayList(Value),

    pub fn deinit(self: Value) void {
        switch (self) {
            .String => |str| str.deinit(),
            .Object => |data| {
                var i = data.iterator();
                while (i.next()) |kv| {
                    kv.value.deinit();
                }
                data.deinit();
            },
            .Array => |objs| {
                for (objs.items) |item| {
                    item.deinit();
                }
                objs.deinit();
            },
            else => {},
        }
    }
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

    fn skipComment(self: *Parser, input: []const u8, offset: usize) usize {
        if (offset >= input.len) {
            return offset;
        }
        if (input[offset] != '#') {
            return offset;
        }
        var i = offset;
        while (i < input.len and input[i] != '\n') : (i+=1) {}
        if (i < input.len) {
            return i+1;
        }
        return i;
    }

    fn parseTrue(self: *Parser, input: []const u8, offset: usize, value: *Value) ParseError!usize {
        if (offset+4 > input.len) {
            return ParseError.FailedToParse;
        }
        if (!mem.eql(u8, "true", input[offset..(offset+4)])) {
            return ParseError.FailedToParse;
        }
        if (offset+4 < input.len and ascii.isAlNum(input[offset+4])) {
            return ParseError.FailedToParse;
        }
        value.* = Value{.Bool = true};
        return offset+4;
    }
    fn parseFalse(self: *Parser, input: []const u8, offset: usize, value: *Value) ParseError!usize {
        if (offset+5 > input.len) {
            return ParseError.FailedToParse;
        }        
        if (!mem.eql(u8, "false", input[offset..(offset+5)])) {
            return ParseError.FailedToParse;
        }
        if (offset+5 < input.len and ascii.isAlNum(input[offset+5])) {
            return ParseError.FailedToParse;
        }
        value.* = Value{.Bool = false};
        return offset+5;
    }
    fn parseBool(self: *Parser, input: []const u8, offset: usize, value: *Value) ParseError!usize {
        return self.parseTrue(input, offset, value) catch self.parseFalse(input, offset, value);
    }

    fn parseInt(self: *Parser, input: []const u8, offset: usize, value: *Value) !usize {
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
            if (ascii.isSpace(c) or c == '#')
                break;
            return ParseError.FailedToParse;
        }
        if (i == start) {
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
        value.* = Value{.Int = try fmt.parseInt(i64, buf, base)};
        return i;
    }

    fn parseFloat(self: *Parser, input: []const u8, offset: usize, value: *Value) !usize {
        var i = offset;
        var has_e = false;
        var has_num = false;
        var has_dot = false;
        var allow_sign = true;
        while (i < input.len) : (i+=1) {
            const c = input[i];
            if (allow_sign and (c == '+' or c == '-')) {
                allow_sign = false;
                continue;
            }
            allow_sign = false;
            if (has_num and !has_e and ascii.toLower(c) == 'e') {
                has_e = true;
                allow_sign = true;
                continue;
            }

            if (ascii.isDigit(c)) {
                has_num = true;
                continue;
            }

            if (!has_dot and !has_e and c == '.') {
                has_dot = true;
                continue;
            }
            return ParseError.FailedToParse;
        }
        if (i == offset) {
            return ParseError.FailedToParse;
        }
        value.* = Value{.Float = try fmt.parseFloat(f64, input[offset..i])};
        return i;
    }

    fn parseString(self: *Parser, input: []const u8, offset: usize, value: *Value) !usize {
        if (input[offset] != '"') {
            return ParseError.FailedToParse;
        }
        var start = offset+1;
        var end = start;
        while (end < input.len) : (end += 1) {
            if (input[end] == '"') {
                var a = std.ArrayList(u8).init(self.allocator);
                _ = try a.appendSlice(input[start..end]);
                value.* = Value{.String = a};
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

    fn parseAssign(self: *Parser, input: []const u8, offset_in: usize, data: *std.StringHashMap(Value)) !usize {
        var offset = self.skipSpaces(input, offset_in);
        var key_result = try self.parseToken(input, offset);
        offset = self.skipSpaces(input, key_result.offset);
        if (input[offset] != '=') {
            return ParseError.FailedToParse;
        }
        offset = self.skipSpaces(input, offset+1);
        var n = Value{.Bool = false};
        offset = try (
            self.parseString(input, offset, &n) catch
            self.parseInt(input, offset, &n) catch 
            self.parseFloat(input, offset, &n) catch
            self.parseBool(input, offset, &n));
        _ = try data.put(key_result.token, n);
        return offset;
    }

    const parseBracketResult = struct{
        data: *std.StringHashMap(Value),
        offset: usize
    };
    fn parseBracket(self: *Parser, input: []const u8, offset_in: usize, data: *std.StringHashMap(Value)) !parseBracketResult {
        var offset = self.skipSpaces(input, offset_in);
        if (input[offset] != '[') {
            return ParseError.FailedToParse;
        }
        var count: usize = 1;
        if (input[offset+count] == '[') {
            count = 2;
        }
        var key_result = try self.parseToken(input, offset+count);
        offset = self.skipSpaces(input, key_result.offset);
        var i: usize = 0;
        while (i < count) : (i += 1) {
            if (input[offset+i] != ']') {
                return ParseError.FailedToParse;
            }
        }
        offset+=count;
        var result = try data.getOrPut(key_result.token);
        var subdata: ?*std.StringHashMap(Value) = null;
        if (count == 1) {
            if (result.found_existing) {
                return ParseError.FailedToParse;
            }
            result.kv.value = Value{.Object = std.StringHashMap(Value).init(self.allocator)};
            subdata = &result.kv.value.Object;
        } else {
            if (!result.found_existing) {
                result.kv.value = Value{.Array = std.ArrayList(Value).init(self.allocator)};
            }
            _ = try result.kv.value.Array.append(Value{.Object = std.StringHashMap(Value).init(self.allocator)});
            subdata = &result.kv.value.Array.items[result.kv.value.Array.items.len-1].Object;
        }
        return parseBracketResult{
            .data = subdata.?,
            .offset = offset,
        };
    }

    pub fn parse(self: *Parser, input: []const u8) !Value {
        var top = std.StringHashMap(Value).init(self.allocator);
        errdefer (Value{.Object = top}).deinit();
        var offset: usize = 0;
        var data = &top;
        while (offset < input.len) {
            offset = self.skipSpaces(input, offset);
            offset = self.skipComment(input, offset);
            if (offset >= input.len) {
                break;
            }
            if (self.parseBracket(input, offset, &top)) |result| {
                data = result.data;
                offset = result.offset;
            } else |_| {
                offset = try self.parseAssign(input, offset, data);
            }
            offset = self.skipSpaces(input, offset);
            offset = self.skipComment(input, offset);
            if (offset < input.len and input[offset] == '\n') {
                offset += 1;
            }
        }
        return Value{.Object = top};
    }
};

test "empty" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse("");
    expect(parsed.Object.size == 0);
}

test "simple-kv" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(" \t foo\t=  42 # comment1  \nbar= \t42.");
    defer parsed.deinit();
    expect(parsed.Object.size == 2);
    expect(parsed.Object.get("foo").?.value.Int == 42);
    expect(parsed.Object.get("bar").?.value.Float == 42);
}

test "string-kv" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse("#test\n \t foo\t=  \"bar\"   \n");
    defer parsed.deinit();
    expect(parsed.Object.size == 1);
    expect(mem.eql(u8, parsed.Object.get("foo").?.value.String.items, "bar"));
}

test "bracket-kv" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse("[foo]\nbar=42\nbaz=true\n[quox]\nbar=96\n");
    defer parsed.deinit();
    expect(parsed.Object.size == 2);
    var o1 = parsed.Object.get("foo").?.value.Object;
    expect(o1.size == 2);
    expect(o1.get("bar").?.value.Int == 42);
    expect(o1.get("baz").?.value.Bool);
    var o2 = parsed.Object.get("quox").?.value.Object;
    expect(o2.size == 1);
    expect(o2.get("bar").?.value.Int == 96);
}

test "double-bracket" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse("[[foo]]\nbar=42\nbaz=true\n[[foo]]\nbar=96\n");
    defer parsed.deinit();
    expect(parsed.Object.size == 1);
    var a = parsed.Object.get("foo").?.value.Array;
    expect(a.items.len == 2);
    var o1 = a.items[0].Object;
    expect(o1.size == 2);
    expect(o1.get("bar").?.value.Int == 42);
    expect(o1.get("baz").?.value.Bool);
    var o2 = a.items[1].Object;
    expect(o2.size == 1);
    expect(o2.get("bar").?.value.Int == 96);
}

test "parseBool" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var n: Value = .{.Int = 0};
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

    var n: Value = .{.Bool = false};
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

test "parseFloat" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var n: Value = .{.Bool = false};
    expect((try parser.parseFloat("+1", 0, &n)) == 2);
    expect(n.Float == 1);
    expect((try parser.parseFloat("1.5", 0, &n)) == 3);
    expect(n.Float == 1.5);
    expect((try parser.parseFloat("-1e2", 0, &n)) == 4);
    expect(n.Float == -1e2);
    expect((try parser.parseFloat(".2e-1", 0, &n)) == 5);
    expect(n.Float == 0.2e-1);
    expect((try parser.parseFloat("1.e+2", 0, &n)) == 5);
    expect(n.Float == 1e+2);
}

test "parseString" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var n: Value = .{.Bool = false};
    defer n.deinit();
    expect((try parser.parseString("\"foo\"", 0, &n)) == 5);
    expect(mem.eql(u8, n.String.items, "foo"));
}

test "parseBracket" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var data = std.StringHashMap(Value).init(allocator);
    defer data.deinit();
    var result = try parser.parseBracket("[foo]", 0, &data);
    expect(result.offset == 5);
    expect(data.size == 1);
    expect(&data.get("foo").?.value.Object == result.data);
}