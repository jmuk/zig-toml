const std = @import("std");
const ascii = std.ascii;
const fmt = std.fmt;
const mem = std.mem;
const unicode = std.unicode;

const testing = std.testing;
const expect = testing.expect;

pub const ValueTag = enum {
    String, Int, Float, Bool, Table, Array,
};

pub const Key = std.ArrayList(u8);

pub const Table = std.HashMap(Key, Value, hashKey, eqlKey);

fn eqlKey(a: Key, b: Key) bool {
    return std.mem.eql(u8, a.items, b.items);
}

fn hashKey(a: Key) u32 {
    return std.hash_map.hashString(a.items);
}

fn getS(tbl: Table, key: []const u8) ?*Table.KV {
    var a = Key.init(tbl.allocator);
    defer a.deinit();
    if (a.appendSlice(key)) |_| {
        return tbl.get(a);
    } else |err| {
        return null;
    }
}

pub const Value = union(ValueTag) {
    String: std.ArrayList(u8),
    Int: i64,
    Float: f64,
    Bool: bool,
    Table: Table,
    Array: std.ArrayList(Value),

    pub fn deinit(self: Value) void {
        switch (self) {
            .String => |str| str.deinit(),
            .Table => |data| {
                var i = data.iterator();
                while (i.next()) |kv| {
                    kv.key.deinit();
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
    DuplicatedKey,
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

    fn skipSpacesAndReturns(self: *Parser, input: []const u8, offset: usize) usize {
        var i = offset;
        while (ascii.isSpace(input[i])) : (i+=1) {}
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
            if (ascii.isAlpha(c)) {
                return ParseError.FailedToParse;
            }
            break;
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
            if (ascii.isAlpha(c))
                return ParseError.FailedToParse;
            break;
        }
        if (i == offset) {
            return ParseError.FailedToParse;
        }
        if (!has_dot and !has_e) {
            return ParseError.FailedToParse;
        }
        value.* = Value{.Float = try fmt.parseFloat(f64, input[offset..i])};
        return i;
    }

    fn hasPrefix(s: []const u8, p: []const u8) bool {
        if (s.len < p.len) {
            return false;
        }
        for (p) |c, i| {
            if (s[i] != c) {
                return false;
            }
        }
        return true;
    }

    fn parseString(self: *Parser, input: []const u8, offset: usize, value: *Value) !usize {
        if (input[offset] != '"') {
            return ParseError.FailedToParse;
        }
        var start = offset+1;
        var multiLine = false;
        if (hasPrefix(input[offset..], "\"\"")) {
            start+=2;
            multiLine = true;
        }
        var end = start;
        var a = std.ArrayList(u8).init(self.allocator);
        var inSkippingSpaces = false;
        var upperSurrogate: ?u21 = null;
        errdefer a.deinit();
        while (end < input.len) : (end += 1) {
            if (input[end] == '\\') {
                if (end >= input.len-1) {
                    return ParseError.FailedToParse;
                }
                end+=1;
                _ = switch (input[end]) {
                    'b' => try a.append('\x08'),
                    't' => try a.append('\t'),
                    'n' => try a.append('\n'),
                    'f' => try a.append('\x0c'),
                    'r' => try a.append('\r'),
                    '"' => try a.append('\"'),
                    '\\' => try a.append('\\'),
                    '\n' => {
                        if (!multiLine) {
                            return ParseError.FailedToParse;
                        }
                        inSkippingSpaces = true;
                    },
                    'u' => {
                        end+=1;
                        if (end+4>input.len) {
                            return ParseError.FailedToParse;
                        }
                        var chr = try fmt.parseInt(u21, input[end..(end+4)], 16);
                        if (chr >= 0xd800 and chr <= 0xdfff) {
                            // handling of surrogate pair.
                            if (upperSurrogate) |us| {
                                chr = (((us & 0x3ff) + 0x40) << 10) | (chr & 0x3ff);
                                upperSurrogate = null;
                            } else {
                                upperSurrogate = chr;
                                end+=3;
                                continue;
                            }
                        }
                        var buf: []u8 = try self.allocator.alloc(u8, try unicode.utf8CodepointSequenceLength(chr));
                        defer self.allocator.free(buf);
                        _ = try unicode.utf8Encode(chr, buf);
                        _ = try a.appendSlice(buf);
                        end+=3;
                    },
                    'U' => {
                        end+=1;
                        if (end+8>input.len) {
                            return ParseError.FailedToParse;
                        }
                        var chr = try fmt.parseInt(u21, input[end..(end+8)], 16);
                        var buf: []u8 = try self.allocator.alloc(u8, try unicode.utf8CodepointSequenceLength(chr));
                        defer self.allocator.free(buf);
                        _ = try unicode.utf8Encode(chr, buf);
                        _ = try a.appendSlice(buf);
                        end+=7;
                    },
                    else => return ParseError.FailedToParse,
                };
            } else if (!multiLine and input[end] == '"') {
                value.* = Value{.String = a};
                return end+1;
            } else if (multiLine and hasPrefix(input[end..], "\"\"\"")) {
                value.* = Value{.String = a};
                return end+3;
            } else if (multiLine and inSkippingSpaces and (ascii.isSpace(input[end]) or input[end] == '\n')) {
                // do nothing, skipping
                continue;
            } else if (input[end] == '\n' and !multiLine) {
                return ParseError.FailedToParse;
            } else {
                inSkippingSpaces = false;
                _ = try a.append(input[end]);
            }
        }
        return ParseError.FailedToParse;
    }

    fn parseLiteralString(self: *Parser, input: []const u8, offset: usize, value: *Value) !usize {
        if (input[offset] != '\'') {
            return ParseError.FailedToParse;
        }
        var start = offset+1;
        var multiLine = false;
        if (hasPrefix(input[start..], "''")) {
            start+=2;
            multiLine = true;
            if (input[start] == '\n') {
                start+=1;
            }
        }
        var end = start;
        while (end < input.len) : (end += 1) {
            if (!multiLine and input[end] == '\n') {
                return ParseError.FailedToParse;
            }
            if ((multiLine and hasPrefix(input[end..], "'''")) or (!multiLine and input[end] == '\'')) {
                var a = std.ArrayList(u8).init(self.allocator);
                errdefer a.deinit();
                _ = try a.appendSlice(input[start..end]);
                value.* = Value{.String = a};
                if (multiLine) {
                    return end+3;
                }
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
            if (!((input[i] >= 'A' and input[i] <= 'Z') or (input[i] >= 'a' and input[i] <= 'z') or
                    (input[i] >= '0' and input[i] <= '9') or input[i] == '_' or input[i] == '-')) {
                break;
            }
        }
        if (i == offset) {
            return ParseError.FailedToParse;
        }
        return tokenResult{.token = input[offset..i], .offset = i};
    }

    const keyResult = struct {
        keys: std.ArrayList(std.ArrayList(u8)),
        offset: usize,
        fn init(allocator: *std.mem.Allocator) keyResult {
            return .{ .keys = std.ArrayList(std.ArrayList(u8)).init(allocator), .offset = 0};
        }
        fn deinit(self: keyResult) void {
            for (self.keys.items) |key| {
                key.deinit();
            }
            self.keys.deinit();
        }
    };
    fn parseKey(self: *Parser, input: []const u8, offset_in: usize) !keyResult {
        var result = keyResult.init(self.allocator);
        errdefer result.deinit();
        var offset = offset_in;
        while (offset < input.len) : (offset+=1) {
            offset = self.skipSpaces(input, offset);
            var token = try parseToken(self, input, offset);
            var tokenArr = std.ArrayList(u8).init(self.allocator);
            tokenArr.appendSlice(token.token) catch |err| {
                tokenArr.deinit();
                return err;
            };
            _ = try result.keys.append(tokenArr);
            offset = self.skipSpaces(input, token.offset);
            if (offset >= input.len or input[offset] != '.') {
                break;
            }
        }
        result.offset = offset;
        return result;
    }

    fn parseArray(self: *Parser, input: []const u8, offset_in: usize, v: *Value) anyerror!usize {
        if (input[offset_in] != '[') {
            return ParseError.FailedToParse;
        }
        var offset = offset_in+1;
        var arr = std.ArrayList(Value).init(self.allocator);
        errdefer arr.deinit();
        while (true) {
            offset = self.skipSpacesAndReturns(input, offset);
            var e = Value{.Bool = false};
            offset = try self.parseValue(input, offset, &e);
            _ = try arr.append(e);
            offset = self.skipSpacesAndReturns(input, offset);
            if (input[offset] == ']') {
                v.* = Value{.Array = arr};
                return offset+1;
            } else if (input[offset] != ',') {
                return ParseError.FailedToParse;
            }
            offset = self.skipSpacesAndReturns(input, offset+1);
        }
        return ParseError.FailedToParse;
    }

    fn parseValue(self: *Parser, input: []const u8, offset_in: usize, v: *Value) anyerror!usize {
        var offset = self.skipSpaces(input, offset_in);
        if (input[offset] == '"') {
            return self.parseString(input, offset, v);
        } else if (input[offset] == '\'') {
            return self.parseLiteralString(input, offset, v);
        } else if (input[offset] == 'f') {
            return self.parseFalse(input, offset, v);
        } else if (input[offset] == 't') {
            return self.parseTrue(input, offset, v);
        } else if (input[offset] == '[') {
            return self.parseArray(input, offset, v);
        }
        return self.parseFloat(input, offset, v) catch self.parseInt(input, offset, v);
    }

    fn getOrPutRecursive(self: *Parser, data: *Table, keys: std.ArrayList(std.ArrayList(u8))) !Table.GetOrPutResult {
        var curr = data;
        for (keys.items) |key, i| {
            var gpr = curr.getOrPut(key) catch |err| {
                var j = i;
                while (j < keys.items.len) : (j+=1) {
                    keys.items[j].deinit();
                }
                return err;
            };
            if (gpr.found_existing) {
                key.deinit();
            }
            if (i == keys.items.len - 1) {
                return gpr;
            }

            if (gpr.found_existing) {
                if (gpr.kv.value != .Table) {
                    return ParseError.DuplicatedKey;
                }
            } else {
                gpr.kv.value = Value{.Table = Table.init(self.allocator)};
            }
            curr = &gpr.kv.value.Table;
        }
        return ParseError.FailedToParse;
    }

    fn parseAssign(self: *Parser, input: []const u8, offset_in: usize, data: *Table) !usize {
        var keys = try self.parseKey(input, offset_in);
        defer keys.keys.deinit();
        if (input[keys.offset] != '=') {
            keys.deinit();
            return ParseError.FailedToParse;
        }
        var gpr = try self.getOrPutRecursive(data, keys.keys);
        if (gpr.found_existing) {
            return ParseError.DuplicatedKey;
        }
        return self.parseValue(input, keys.offset+1, &gpr.kv.value);
    }

    const parseBracketResult = struct{
        data: *Table,
        offset: usize
    };
    fn parseBracket(self: *Parser, input: []const u8, offset_in: usize, data: *Table) !parseBracketResult {
        var offset = self.skipSpaces(input, offset_in);
        if (input[offset] != '[') {
            return ParseError.FailedToParse;
        }
        var count: usize = 1;
        if (input[offset+count] == '[') {
            count = 2;
        }
        var keys = try self.parseKey(input, offset+count);
        defer keys.keys.deinit();
        var i: usize = 0;
        while (i < count) : (i += 1) {
            if (input[keys.offset+i] != ']') {
                keys.keys.deinit();
                return ParseError.FailedToParse;
            }
        }
        offset = keys.offset+count;
        var gpr = try self.getOrPutRecursive(data, keys.keys);
        var subdata: ?*Table = null;
        if (count == 1) {
            if (gpr.found_existing and gpr.kv.value == .Array) {
                subdata = &gpr.kv.value.Array.items[gpr.kv.value.Array.items.len-1].Table;
            } else {
                if (!gpr.found_existing) {
                    gpr.kv.value = Value{.Table = Table.init(self.allocator)};
                }
                subdata = &gpr.kv.value.Table;
            }
        } else {
            if (!gpr.found_existing) {
                gpr.kv.value = Value{.Array = std.ArrayList(Value).init(self.allocator)};
            }
            _ = try gpr.kv.value.Array.append(Value{.Table = Table.init(self.allocator)});
            subdata = &gpr.kv.value.Array.items[gpr.kv.value.Array.items.len-1].Table;
        }
        return parseBracketResult{
            .data = subdata.?,
            .offset = offset,
        };
    }

    pub fn parse(self: *Parser, input: []const u8) !Value {
        var top = Table.init(self.allocator);
        errdefer (Value{.Table = top}).deinit();
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
        return Value{.Table = top};
    }
};

test "empty" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse("");
    expect(parsed.Table.size == 0);
}

test "simple-kv" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(" \t foo\t=  42 # comment1  \nbar= \t42.");
    defer parsed.deinit();
    expect(parsed.Table.size == 2);
    expect(getS(parsed.Table, "foo").?.value.Int == 42);
    expect(getS(parsed.Table, "bar").?.value.Float == 42);
}

test "string-kv" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse("#test\n \t foo\t=  \"bar\"   \n");
    defer parsed.deinit();
    expect(parsed.Table.size == 1);
    expect(mem.eql(u8, getS(parsed.Table, "foo").?.value.String.items, "bar"));
}

test "bracket-kv" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse("[foo]\nbar=42\nbaz=true\n[quox]\nbar=96\n");
    defer parsed.deinit();
    expect(parsed.Table.size == 2);
    var o1 = getS(parsed.Table, "foo").?.value.Table;
    expect(o1.size == 2);
    expect(getS(o1, "bar").?.value.Int == 42);
    expect(getS(o1, "baz").?.value.Bool);
    var o2 = getS(parsed.Table, "quox").?.value.Table;
    expect(o2.size == 1);
    expect(getS(o2, "bar").?.value.Int == 96);
}

test "double-bracket" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse("[[foo]]\nbar=42\nbaz=true\n[[foo]]\nbar=96\n");
    defer parsed.deinit();
    expect(parsed.Table.size == 1);
    var a = getS(parsed.Table, "foo").?.value.Array;
    expect(a.items.len == 2);
    var o1 = a.items[0].Table;
    expect(o1.size == 2);
    expect(getS(o1, "bar").?.value.Int == 42);
    expect(getS(o1, "baz").?.value.Bool);
    var o2 = a.items[1].Table;
    expect(o2.size == 1);
    expect(getS(o2, "bar").?.value.Int == 96);
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

test "parseString-escape" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var n: Value = .{.Bool = false};
    defer n.deinit();
    expect((try parser.parseString("\"\\b\\t\\n\\f\\r\\\"\\\\\"", 0, &n)) == 16);
    expect(mem.eql(u8, n.String.items, "\x08\t\n\x0c\r\"\\"));
}

test "parseString-escape-numerical" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var n: Value = .{.Bool = false};
    defer n.deinit();
    expect((try parser.parseString("\"a\\u3042b\\U0001F600\\uD83D\\uDE00\"", 0, &n)) == 32);
    expect(mem.eql(u8, n.String.items, "a\xe3\x81\x82b\xF0\x9F\x98\x80\xF0\x9F\x98\x80"));
}

test "parseString-multi" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var n: Value = .{.Bool = false};
    defer n.deinit();
    expect((try parser.parseString("\"\"\"aaa\nbbb\\\n  \n ccc\"\"\"", 0, &n)) == 22);
    expect(mem.eql(u8, n.String.items, "aaa\nbbbccc"));
}

test "parseLiteralString" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var n: Value = .{.Bool = false};
    errdefer n.deinit();
    expect((try parser.parseLiteralString("'\\\\ServerX\\admin$\\system32\\'", 0, &n)) == 28);
    expect(mem.eql(u8, n.String.items, "\\\\ServerX\\admin$\\system32\\"));
    n.deinit();

    expect((try parser.parseLiteralString("'''\nThe first newline is\ntrimmed in raw strings.\n   All other whitespace\n   is preserved.\n'''", 0, &n)) == 93);
    expect(mem.eql(u8, n.String.items, "The first newline is\ntrimmed in raw strings.\n   All other whitespace\n   is preserved.\n"));
    n.deinit();

    expect((try parser.parseLiteralString("''''That's still pointless', she said.'''", 0, &n)) == 41);
    expect(mem.eql(u8, n.String.items, "'That's still pointless', she said."));
    n.deinit();
}

test "parseKey" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var keys = try parser.parseKey("abc.123.a-z = 42", 0);
    defer keys.deinit();
    expect(keys.offset == 12);
    expect(keys.keys.items.len == 3);
    expect(mem.eql(u8, keys.keys.items[0].items, "abc"));
    expect(mem.eql(u8, keys.keys.items[1].items, "123"));
    expect(mem.eql(u8, keys.keys.items[2].items, "a-z"));
}

test "parseBracket" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var data = Table.init(allocator);
    defer data.deinit();
    defer {
        var i = data.iterator();
        while (i.next()) |kv| {
            kv.key.deinit();
            kv.value.deinit();
        }
    }
    var result = try parser.parseBracket("[foo]", 0, &data);
    expect(result.offset == 5);
    expect(data.size == 1);
    expect(&getS(data, "foo").?.value.Table == result.data);
}

test "parseArray" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var n: Value = .{.Bool = false};
    defer n.deinit();
    expect((try parser.parseArray("[ 42, 43.0,\n true, false ]", 0, &n)) == 26);
    expect(n.Array.items.len == 4);
    expect(n.Array.items[0].Int == 42);
    expect(n.Array.items[1].Float == 43.0);
    expect(n.Array.items[2].Bool);
    expect(!n.Array.items[3].Bool);
}