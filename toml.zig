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

pub fn getS(tbl: Table, key: []const u8) ?*Table.KV {
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
    IncorrectDataType,
    UnknownReturnType,
    FieldNotFound,
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
        while (i < input.len and ascii.isSpace(input[i])) : (i+=1) {}
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

    fn skipSpacesAndComments(self: *Parser, input: []const u8, offset: usize) usize {
        var i = offset;
        while (i < input.len) : (i+=1) {
            if (input[i] == '#') {
                while (i < input.len and input[i] != '\n') : (i+=1) {}
            } else if (!ascii.isSpace(input[i])) {
                return i;
            }
        }
        return i;
    }

    fn parseTrue(self: *Parser, input: []const u8, offset: usize, value: *bool) ParseError!usize {
        if (offset+4 > input.len) {
            return ParseError.FailedToParse;
        }
        if (!mem.eql(u8, "true", input[offset..(offset+4)])) {
            return ParseError.FailedToParse;
        }
        if (offset+4 < input.len and ascii.isAlNum(input[offset+4])) {
            return ParseError.FailedToParse;
        }
        value.* = true;
        return offset+4;
    }
    fn parseFalse(self: *Parser, input: []const u8, offset: usize, value: *bool) ParseError!usize {
        if (offset+5 > input.len) {
            return ParseError.FailedToParse;
        }        
        if (!mem.eql(u8, "false", input[offset..(offset+5)])) {
            return ParseError.FailedToParse;
        }
        if (offset+5 < input.len and ascii.isAlNum(input[offset+5])) {
            return ParseError.FailedToParse;
        }
        value.* = false;
        return offset+5;
    }
    fn parseBool(self: *Parser, input: []const u8, offset: usize, value: *bool) ParseError!usize {
        return self.parseTrue(input, offset, value) catch self.parseFalse(input, offset, value);
    }

    fn parseInt(self: *Parser, input: []const u8, offset: usize, value: *i64) !usize {
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
        value.* = try fmt.parseInt(i64, buf, base);
        return i;
    }

    fn parseFloat(self: *Parser, input: []const u8, offset: usize, value: *f64) !usize {
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
        value.* = try fmt.parseFloat(f64, input[offset..i]);
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

    fn parseQuotedString(self: *Parser, input: []const u8, offset: usize, s: *Key) !usize {
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
        var inSkippingSpaces = false;
        var upperSurrogate: ?u21 = null;
        while (end < input.len) : (end += 1) {
            if (input[end] == '\\') {
                if (end >= input.len-1) {
                    return ParseError.FailedToParse;
                }
                end+=1;
                _ = switch (input[end]) {
                    'b' => try s.append('\x08'),
                    't' => try s.append('\t'),
                    'n' => try s.append('\n'),
                    'f' => try s.append('\x0c'),
                    'r' => try s.append('\r'),
                    '"' => try s.append('\"'),
                    '\\' => try s.append('\\'),
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
                        _ = try s.appendSlice(buf);
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
                        _ = try s.appendSlice(buf);
                        end+=7;
                    },
                    else => return ParseError.FailedToParse,
                };
            } else if (!multiLine and input[end] == '"') {
                return end+1;
            } else if (multiLine and hasPrefix(input[end..], "\"\"\"")) {
                return end+3;
            } else if (multiLine and inSkippingSpaces and (ascii.isSpace(input[end]) or input[end] == '\n')) {
                // do nothing, skipping
                continue;
            } else if (input[end] == '\n' and !multiLine) {
                return ParseError.FailedToParse;
            } else {
                inSkippingSpaces = false;
                _ = try s.append(input[end]);
            }
        }
        return ParseError.FailedToParse;
    }

    fn parseLiteralString(self: *Parser, input: []const u8, offset: usize, s: *Key) !usize {
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
                _ = try s.appendSlice(input[start..end]);
                if (multiLine) {
                    return end+3;
                }
                return end+1;
            }
        }
        return ParseError.FailedToParse;
    }

    fn parseString(self: *Parser, input: []const u8, offset_in: usize, s: *Key) !usize {
        if (input[offset_in] == '"') {
            return self.parseQuotedString(input, offset_in, s);
        }
        return self.parseLiteralString(input, offset_in, s);
    }

    const tokenResult = struct {
        token: []const u8,
        offset: usize
    };
    fn parseToken(self: *Parser, input: []const u8, offset_in: usize, s: *Key) !usize {
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
        _ = try s.appendSlice(input[offset..i]);
        return i;
    }

    const keyResult = struct {
        keys: std.ArrayList(Key),
        offset: usize,
        fn init(allocator: *std.mem.Allocator) keyResult {
            return .{ .keys = std.ArrayList(Key).init(allocator), .offset = 0};
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
            var tokenArr = Key.init(self.allocator);
            {
                errdefer tokenArr.deinit();
                if (input[offset] == '"') {
                    offset = try self.parseQuotedString(input, offset, &tokenArr);
                } else if (input[offset] == '\'') {
                    offset = try self.parseLiteralString(input, offset, &tokenArr);
                } else {
                    offset = try self.parseToken(input, offset, &tokenArr);
                }
            }
            _ = try result.keys.append(tokenArr);
            offset = self.skipSpaces(input, offset);
            if (offset >= input.len or input[offset] != '.') {
                break;
            }
        }
        result.offset = offset;
        return result;
    }

    fn parseArray(self: *Parser, input: []const u8, offset_in: usize, comptime T: type, v: *std.ArrayList(T)) anyerror!usize {
        if (input[offset_in] != '[') {
            return ParseError.FailedToParse;
        }
        var offset = offset_in+1;
        while (true) {
            offset = self.skipSpacesAndComments(input, offset);
            var e = try self.createT(T);
            errdefer self.destroyT(T, e);
            offset = try self.parseValue(input, offset, T, &e);
            _ = try v.append(e);
            offset = self.skipSpacesAndComments(input, offset);
            if (input[offset] == ']') {
                return offset+1;
            } else if (input[offset] != ',') {
                return ParseError.FailedToParse;
            }
            offset = self.skipSpacesAndComments(input, offset+1);
        }
        return ParseError.FailedToParse;
    }

    fn parseValue(self: *Parser, input: []const u8, offset_in: usize, comptime T: type, v: *T) !usize {
        var offset = self.skipSpaces(input, offset_in);
        if (T == Value) {
            if (input[offset] == '"' or input[offset] == '\'') {
                var s = Key.init(self.allocator);
                errdefer s.deinit();
                offset = try self.parseString(input, offset, &s);
                v.* = Value{.String = s};
                return offset;
            } else if (input[offset] == 'f' or input[offset] == 't') {
                var b = false;
                offset = try self.parseBool(input, offset, &b);
                v.* = Value{.Bool = b};
                return offset;
            } else if (input[offset] == '[') {
                var a = std.ArrayList(Value).init(self.allocator);
                errdefer a.deinit();
                offset = try self.parseArray(input, offset, Value, &a);
                v.* = Value{.Array = a};
                return offset;
            }
            var f: f64 = 0.0;
            if (self.parseFloat(input, offset, &f)) |offset_out| {
                v.* = Value{.Float = f};
                return offset_out;
            } else |err| {
                var i: i64 = 0;
                offset = try self.parseInt(input, offset, &i);
                v.* = Value{.Int = i};
                return offset;
            }
        }
        if (T == Key) {
            var s = Key.init(self.allocator);
            errdefer s.deinit();
            offset = try self.parseString(input, offset, &s);
            v.* = Value{.String = s};
            return offset;
        }
        switch (@typeInfo(T)) {
            .Bool => return self.parseBool(input, offset, v),
            .Int => return self.parseInt(input, offset, v),
            .Float => return self.parseFloat(input, offset, v),
            .Array => |arr| {
                if (arr.child_type == u8) {
                    var s = Key.init(self.allocator);
                    errdefer s.deinit();
                    offset = try self.parseString(input, offset, &s);
                    v.* = s.items;
                    return offset;
                }
            },
            else => return ParseError.IncorrectDataType,
            // TODO: struct
        }

    }

    fn lookupAndParseValue(self: *Parser, input: []const u8, offset_in: usize, keys: []Key, comptime T: type, v: *T) anyerror!usize {
        var key = keys[0];
        errdefer for (keys[1..(keys.len)]) |k| {
            k.deinit();
        };
        if (keys.len == 1) {
            if (T == Value) {
                if (v.* != .Table) {
                    return ParseError.IncorrectDataType;
                }
                var gpr = try v.*.Table.getOrPut(key);
                if (gpr.found_existing) {
                    key.deinit();
                    return ParseError.DuplicatedKey;
                }
                return self.parseValue(input, offset_in, Value, &gpr.kv.value);
            } else {
                var ti = @typeInfo(T);
                if (ti != .Struct)
                    return ParseError.IncorrectDataType;
                inline for (structInfo.fields) |field| {
                    if (mem.eql(u8, key.items, field.name)) {
                        var fti = @typeInfo(field.field_type);
                        if (fti == .Pointer) {
                            var f = self.allocator.create(fti.child_type);
                            errdefer self.allocator.destroy(f);
                            var offset = try self.parseValue(input, offset_in, fti.child_type, f);
                            @field(v, field.name) = v;
                            return offset;
                        }
                        return self.parseValue(input, offset_in, field.field_type, &(@field(v, field.name)));
                    }
                }
                return ParseError.FieldNotFound;
            }
        }
        if (T == Value) {
            if (v.* == .Table) {
                var gpr = try v.*.Table.getOrPut(key);
                if (gpr.found_existing) {
                    key.deinit();
                } else {
                    gpr.kv.value = Value{.Table = Table.init(self.allocator)};
                }
                return self.lookupAndParseValue(input, offset_in, keys[1..(keys.len)], Value, &gpr.kv.value);
            }
            return ParseError.IncorrectDataType;
        }
        var ti = @typeInfo(T);
        if (ti = .Struct)
            return ParseError.IncorrectDataType;
        inline for (structInfo.fields) |field| {
            if (mem.eql(u8, key.items, field.name)) {
                var fti = @typeInfo(field.field_type);
                if (fti == .Pointer) {
                    var f = self.allocator.create(fti.child_type);
                    errdefer self.allocator.destroy(f);
                    var offset = try self.lookupAndParseValue(input, offset_in, keys[1..(keys.len)], fti.child_type, f);
                    @field(v, field.name) = f;
                    return offset;
                }
                return self.lookupAndParseValue(input, offset_in, keys[1..(keys.len)], field.field_type, &(@field(v, field.name)));
            }
        }
        return ParseError.FIeldNotFound;
    }

    fn parseAssign(self: *Parser, input: []const u8, offset_in: usize, comptime T: type, data: *T) !usize {
        var keys = try self.parseKey(input, offset_in);
        defer keys.keys.deinit();
        if (input[keys.offset] != '=') {
            keys.deinit();
            return ParseError.FailedToParse;
        }
        return self.lookupAndParseValue(input, keys.offset+1, keys.keys.items, T, data);
    }

    const parseBracketResult = struct{
        data: *Table,
        offset: usize
    };

    fn lookupAndParseKVs(self: *Parser, input: []const u8, offset_in: usize, keys: []Key, is_array: bool, comptime T: type, data: *T) !usize {
        var key = keys[0];
        errdefer for (keys[1..(keys.len)]) |k| {
            k.deinit();
        };
        if (keys.len == 1) {
            if (T == Value) {
                if (data.* != .Table) {
                    return ParseError.IncorrectDataType;   
                }
                var gpr = try data.Table.getOrPut(key);
                if (gpr.found_existing) {
                    key.deinit();
                    if (gpr.kv.value == .Array) {
                        if (!is_array) {
                            return ParseError.IncorrectDataType;
                        }
                        var v = Value{.Table = Table.init(self.allocator)};
                        errdefer v.deinit();
                        var offset = try self.parseKVs(input, offset_in, Value, &v);
                        _ = try gpr.kv.value.Array.append(v);
                        return offset;
                    }
                    return ParseError.IncorrectDataType;
                }
                if (is_array) {
                    var v = Value{.Table = Table.init(self.allocator)};
                    errdefer v.deinit();
                    var offset = try self.parseKVs(input, offset_in, Value, &v);
                    var a = std.ArrayList(Value).init(self.allocator);
                    errdefer a.deinit();
                    _ = try a.append(v);
                    gpr.kv.value = Value{.Array = a};
                    return offset;
                }
                gpr.kv.value = Value{.Table = Table.init(self.allocator)};
                errdefer gpr.kv.value.deinit();
                return self.parseKVs(input, offset_in, Value, &gpr.kv.value);
            } else {
                return ParseError.IncorrectDataType;
            }
        }

        if (T == Value) {
            if (data.* != .Table) {
                return ParseError.IncorrectDataType;   
            }
            var gpr = try data.Table.getOrPut(key);

            if (gpr.found_existing) {
                key.deinit();
                if (gpr.kv.value == .Array) {
                    var idx = gpr.kv.value.Array.items.len-1;
                    return self.lookupAndParseKVs(input, offset_in, keys[1..(keys.len)], is_array, T, &gpr.kv.value.Array.items[idx]);
                }
                return self.lookupAndParseKVs(input, offset_in, keys[1..(keys.len)], is_array, T, &gpr.kv.value);
            }
            gpr.kv.value = Value{.Table = Table.init(self.allocator)};
            errdefer gpr.kv.value.deinit();
            return self.lookupAndParseKVs(input, offset_in, keys[1..(keys.len)], is_array, T, &gpr.kv.value);
        } else {
            return ParseError.IncorrectDataType;
        }
    }

    fn parseBracket(self: *Parser, input: []const u8, offset_in: usize, comptime T: type, data: *T) !usize {
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
        return self.lookupAndParseKVs(input, offset, keys.keys.items, count > 1, T, data);
    }

    fn createT(self: *Parser, comptime T: type) !T {
        if (T == Value) {
            return Value{.Table = Table.init(self.allocator)};
        } else if (@hasDecl(T, "init")) {
            var ft = @typeInfo(@TypeOf(T.init)).Fn;
            if (ft.return_type == T and ft.args.len == 1 and ft.args[0].arg_type.? == std.mem.allocator) {
                return T.init(self.allocator);
            }
        } else {
            if (@typeInfo(T).Pointer) |ptr| {
                return self.allocator.create(ptr.child);
            }
        }
        return ParseError.UnknownReturnType;
    }

    fn destroyT(self: *Parser, comptime T: type, v: T) void {
        if (T == Value) {
            return v.deinit();
        } else if (@hasDecl(T, "deinit")) {
            if (@typeInfo(@TypeOf(v.deinit)).Fn) |f| {
                if (f.args.len == 0) {
                    v.deinit();
                }
            }
        } else {
            if (@typeInfo(T).Pointer) |ptr| {
                self.allocator.destroy(v);
            }
        }
    }

    pub fn parseKVs(self: *Parser, input: []const u8, offset_in: usize, comptime T: type, value: *T) !usize {
        if (T == Value and value.* != .Table) {
            return ParseError.IncorrectDataType;
        }
        var offset: usize = offset_in;
        while (offset < input.len) {
            offset = self.skipSpacesAndComments(input, offset);
            if (offset >= input.len or input[offset] == '[') {
                return offset;
            }
            offset = try self.parseAssign(input, offset, T, value);
        }
        return offset;
    }

    pub fn parse(self: *Parser, comptime T: type, input: []const u8) !T {
        var top = try self.createT(T);
        errdefer self.destroyT(T, top);
        var offset = try self.parseKVs(input, 0, T, &top);
        while (offset < input.len) {
            offset = self.skipSpacesAndComments(input, offset);
            if (offset >= input.len) {
                break;
            }
            offset = try self.parseBracket(input, offset, T, &top);
        }
        return top;
    }
};

// helper for tests.
fn checkS(tbl: Table, key: []const u8, e: []const u8) bool {
    if (getS(tbl, key)) |kv| {
        if (kv.value == .String) {
            return mem.eql(u8, kv.value.String.items, e);
        } else {
            std.debug.warn("value {} is not a string\n", .{kv.value});
        }
    } else {
        std.debug.warn("key {} not found\n", .{key});
    }
    return false;
}

test "empty" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(Value, "");
    expect(parsed.Table.size == 0);
}

test "simple-kv" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(Value, " \t foo\t=  42 # comment1  \nbar= \t42.");
    defer parsed.deinit();
    expect(parsed.Table.size == 2);
    expect(getS(parsed.Table, "foo").?.value.Int == 42);
    expect(getS(parsed.Table, "bar").?.value.Float == 42);
}

test "simple-kv-struct" {
    const st = struct {
        foo: i64,
        bar: f64,
    };
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(st, " \t foo\t=  42 # comment1  \nbar= \t42.");
    expect(st.foo == 42);
    expect(st.bar == 42.0);
}

test "string-kv" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(Value, "#test\n \t foo\t=  \"bar\"   \n");
    defer parsed.deinit();
    expect(parsed.Table.size == 1);
    expect(checkS(parsed.Table, "foo", "bar"));
}

test "bracket-kv" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(Value, "[foo]\nbar=42\nbaz=true\n[quox]\nbar=96\n");
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
    var parsed = try parser.parse(Value, "[[foo]]\nbar=42\nbaz=true\n[[foo]]\nbar=96\n");
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

test "double-bracket-subtable" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(Value,
        \\[[fruit]]
        \\  name = "apple"
        \\  [fruit.physical] # subtable
        \\    color = "red"
        \\    shape = "round"
        \\
        \\  [[fruit.variety]]
        \\    name = "red delicious"
        \\  [[fruit.variety]]
        \\    name = "granny smith"
        \\
        \\[[fruit]]
        \\  name = "banana"
        \\  [[fruit.variety]]
        \\    name = "plantain"
    );
    defer parsed.deinit();

    expect(parsed.Table.size == 1);
    var fruits = getS(parsed.Table, "fruit").?.value.Array;
    expect(fruits.items.len == 2);
    var apple = fruits.items[0].Table;
    expect(apple.size == 3);
    expect(checkS(apple, "name", "apple"));
    var physical = getS(apple, "physical").?.value.Table;
    expect(checkS(physical, "color", "red"));
    expect(checkS(physical, "shape", "round"));
    var varieties = getS(apple, "variety").?.value.Array;
    expect(varieties.items.len == 2);
    expect(checkS(varieties.items[0].Table, "name", "red delicious"));
    expect(checkS(varieties.items[1].Table, "name", "granny smith"));

    var banana = fruits.items[1].Table;
    expect(banana.size == 2);
    expect(checkS(banana, "name", "banana"));
    varieties = getS(banana, "variety").?.value.Array;
    expect(varieties.items.len == 1);
    expect(checkS(varieties.items[0].Table, "name", "plantain"));
}

test "parseBool" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var b = false;
    expect((try parser.parseBool("true", 0, &b)) == 4);
    expect(b);
    expect((try parser.parseBool("true   ", 0, &b)) == 4);
    expect(b);
    expect(parser.parseBool("trues", 0, &b) catch |err| e: {
        expect(err == ParseError.FailedToParse);
        break :e 0;
    } == 0);

    expect((try parser.parseBool("false", 0, &b)) == 5);
    expect(!b);
    expect((try parser.parseBool("false   ", 0, &b)) == 5);
    expect(!b);
    expect(parser.parseBool("falses", 0, &b) catch |err| e: {
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

    var n: i64 = 0;
    expect((try parser.parseInt("123", 0, &n)) == 3);
    expect(n == 123);

    expect((try parser.parseInt("-123", 0, &n)) == 4);
    expect(n == -123);

    expect((try parser.parseInt("+123_456_789", 0, &n)) == 12);
    expect(n == 123456789);

    expect((try parser.parseInt("0XFF", 0, &n)) == 4);
    expect(n == 255);

    expect((try parser.parseInt("0Xa", 0, &n)) == 3);
    expect(n == 10);

    expect((try parser.parseInt("0o20", 0, &n)) == 4);
    expect(n == 16);

    expect((try parser.parseInt("0b0100", 0, &n)) == 6);
    expect(n == 4);

    // hexadecimal with underscore.
    expect((try parser.parseInt("0xa_1", 0, &n)) == 5);
    expect(n == 161);

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

    var f: f64 = 0.0;
    expect((try parser.parseFloat("1.5", 0, &f)) == 3);
    expect(f == 1.5);
    expect((try parser.parseFloat("-1e2", 0, &f)) == 4);
    expect(f == -1e2);
    expect((try parser.parseFloat(".2e-1", 0, &f)) == 5);
    expect(f == 0.2e-1);
    expect((try parser.parseFloat("1.e+2", 0, &f)) == 5);
    expect(f == 1e+2);
}

test "parseString" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var s = Key.init(allocator);
    defer s.deinit();
    expect((try parser.parseString("\"foo\"", 0, &s)) == 5);
    expect(mem.eql(u8, s.items, "foo"));
}

test "parseString-escape" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var s = Key.init(allocator);
    defer s.deinit();
    expect((try parser.parseString("\"\\b\\t\\n\\f\\r\\\"\\\\\"", 0, &s)) == 16);
    expect(mem.eql(u8, s.items, "\x08\t\n\x0c\r\"\\"));
}

test "parseString-escape-numerical" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var s = Key.init(allocator);
    defer s.deinit();
    expect((try parser.parseString("\"a\\u3042b\\U0001F600\\uD83D\\uDE00\"", 0, &s)) == 32);
    expect(mem.eql(u8, s.items, "a\xe3\x81\x82b\xF0\x9F\x98\x80\xF0\x9F\x98\x80"));
}

test "parseString-multi" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var s = Key.init(allocator);
    defer s.deinit();
    expect((try parser.parseString("\"\"\"aaa\nbbb\\\n  \n ccc\"\"\"", 0, &s)) == 22);
    expect(mem.eql(u8, s.items, "aaa\nbbbccc"));
}

test "parseLiteralString" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var s = Key.init(allocator);
    defer s.deinit();
    expect((try parser.parseLiteralString("'\\\\ServerX\\admin$\\system32\\'", 0, &s)) == 28);
    expect(mem.eql(u8, s.items, "\\\\ServerX\\admin$\\system32\\"));
    _ = try s.resize(0);

    expect((try parser.parseLiteralString("'''\nThe first newline is\ntrimmed in raw strings.\n   All other whitespace\n   is preserved.\n'''", 0, &s)) == 93);
    expect(mem.eql(u8, s.items, "The first newline is\ntrimmed in raw strings.\n   All other whitespace\n   is preserved.\n"));
    _ = try s.resize(0);

    expect((try parser.parseLiteralString("''''That's still pointless', she said.'''", 0, &s)) == 41);
    expect(mem.eql(u8, s.items, "'That's still pointless', she said."));
    _ = try s.resize(0);
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

test "parseKey-quoted" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var keys = try parser.parseKey("site.\"google.com\" = true", 0);
    defer keys.deinit();
    expect(keys.offset == 18);
    expect(keys.keys.items.len == 2);
    expect(mem.eql(u8, keys.keys.items[0].items, "site"));
    expect(mem.eql(u8, keys.keys.items[1].items, "google.com"));
}

// test "parseBracket" {
//     var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
//     defer tester.validate() catch {};
//     var allocator = &tester.allocator;
//     var parser = try Parser.init(allocator);
//     defer allocator.destroy(parser);

//     var data = Table.init(allocator);
//     defer data.deinit();
//     defer {
//         var i = data.iterator();
//         while (i.next()) |kv| {
//             kv.key.deinit();
//             kv.value.deinit();
//         }
//     }
//     var result = try parser.parseBracket("[foo]", 0, &data);
//     expect(result.offset == 5);
//     expect(data.size == 1);
//     expect(&getS(data, "foo").?.value.Table == result.data);
// }

test "parseArray" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var a = std.ArrayList(Value).init(allocator);
    defer a.deinit();
    expect((try parser.parseArray("[ 42, 43.0,\n true, false ]", 0, Value, &a)) == 26);
    expect(a.items.len == 4);
    expect(a.items[0].Int == 42);
    expect(a.items[1].Float == 43.0);
    expect(a.items[2].Bool);
    expect(!a.items[3].Bool);
}