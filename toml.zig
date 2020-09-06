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

const VisitType = enum {
    None,
    Keyvalue,
    Bracket,
};

const VisitedNode = struct {
    visit_type: VisitType,
    children: std.HashMap(Key, VisitedNode, hashKey, eqlKey),
    allocator: *mem.Allocator,

    fn init(allocator: *mem.Allocator) VisitedNode {
        return .{
            .visit_type = .None,
            .children = std.HashMap(Key, VisitedNode, hashKey, eqlKey).init(allocator),
            .allocator = allocator,
        };
    }

    fn deinit(self: VisitedNode) void {
        var i = self.children.iterator();
        while (i.next()) |kv| {
            kv.key.deinit();
            kv.value.deinit();
        }
        self.children.deinit();
    }

    fn visitedType(self: *VisitedNode, key: Key) VisitType {
        if (self.children.get(key)) |kv| {
            return kv.value.visit_type;
        }
        return .None;
    }

    fn markVisited(self: *VisitedNode, key: Key, visit_type: VisitType) !void {
        var node = VisitedNode.init(self.allocator);
        node.visit_type = visit_type;
        var copiedKey = Key.init(self.allocator);
        errdefer copiedKey.deinit();
        _ = try copiedKey.appendSlice(key.items);
        var out = try self.children.put(copiedKey, node);
    }

    fn idxToKey(self: *VisitedNode, idx: usize) !Key {
        var key = Key.init(self.allocator);
        try std.fmt.formatInt(idx, 16, false, std.fmt.FormatOptions{}, key.outStream());
        return key;
    }

    fn markVisitedIdx(self: *VisitedNode, idx: usize, visit_type: VisitType) !void {
        var key = try self.idxToKey(idx);
        defer key.deinit();
        return self.markVisited(key, visit_type);
    }

    fn getChild(self: *VisitedNode, key: Key) ?*VisitedNode {
        if (self.children.get(key)) |kv| {
            return &kv.value;
        }
        return null;
    }

    fn getChildIdx(self: *VisitedNode, idx: usize) ?*VisitedNode {
        var key = self.idxToKey(idx) catch return null;
        defer key.deinit();
        return self.getChild(key);
    }
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

    fn checkSignedPattern(self: *Parser, input: []const u8, offset: usize, pattern: []const u8) ?usize {
        var i = offset;
        if (input[i] == '+' or input[i] == '-')
            i+=1;
        if (i+pattern.len > input.len)
            return null;
        if (!mem.eql(u8, input[i..(i+pattern.len)], pattern))
            return null;
        if (i+pattern.len == input.len or !ascii.isAlNum(input[i+pattern.len]))
            return i+pattern.len;
        return null;
    }

    fn parseFloat(self: *Parser, input: []const u8, offset: usize, value: *f64) !usize {
        if (self.checkSignedPattern(input, offset, "inf")) |out| {
            if (input[offset] == '-') {
                value.* = -std.math.inf(f64);
            } else {
                value.* = std.math.inf(f64);
            }
            return out;
        }
        if (self.checkSignedPattern(input, offset, "nan")) |out| {
            value.* = std.math.nan(f64);
            return out;
        }
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

    fn parseArray(self: *Parser, input: []const u8, offset_in: usize, visited: *VisitedNode, comptime T: type, v: *std.ArrayList(T)) anyerror!usize {
        if (input[offset_in] != '[') {
            return ParseError.FailedToParse;
        }
        var offset = offset_in+1;
        while (true) {
            offset = self.skipSpacesAndComments(input, offset);
            if (input[offset] == ']')
                return offset+1;
            var e = try self.createT(T);
            errdefer self.destroyT(T, e);
            offset = try self.parseValue(input, offset, visited, T, &e);
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

    fn parseInlineTable(self: *Parser, input: []const u8, offset_in: usize, visited: *VisitedNode, comptime T: type, v: *T) !usize {
        if (input[offset_in] != '{')
            return ParseError.FailedToParse;
        
        var offset = offset_in+1;
        while (true) {
            offset = self.skipSpacesAndComments(input, offset);
            if (input[offset] == '}')
                return offset+1;
            offset = try self.parseAssign(input, offset, visited, T, v);
            offset = self.skipSpacesAndComments(input, offset);
            if (input[offset] == '}') {
                return offset+1;
            } else if (input[offset] != ',') {
                return ParseError.FailedToParse;
            }
            offset+=1;
        }
        return ParseError.FailedToParse;
    }

    fn parseValue(self: *Parser, input: []const u8, offset_in: usize, visited: *VisitedNode, comptime T: type, v: *T) !usize {
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
                offset = try self.parseArray(input, offset, visited, Value, &a);
                v.* = Value{.Array = a};
                return offset;
            } else if (input[offset] == '{') {
                v.* = Value{.Table = Table.init(self.allocator)};
                errdefer v.deinit();
                return self.parseInlineTable(input, offset, visited, Value, v);
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
            .Pointer => |ptr| {
                if (ptr.size == .One) {
                    v.* = try self.allocator.create(ptr.child);
                    errdefer self.allocator.destroy(v.*);
                    return self.parseValue(input, offset, visited, ptr.child, v.*);
                }
                var allocator = if (ptr.size == .C)
                    std.heap.c_allocator
                else
                    self.allocator
                ;
                var a = std.ArrayList(ptr.child).init(allocator);
                errdefer a.deinit();
                offset = try if (ptr.child == u8)
                    self.parseString(input, offset, &a)
                else
                    self.parseArray(input, offset, visited, ptr.child, &a)
                ;
                v.* = a.items;
                return offset;
            },
            .Array => |arr| {
                var a = sstd.ArrayList(arr.child).init(self.allocator);
                defer a.deinit();
                if (arr.child_type == u8) {
                    offset = try self.parseString(input, offset, &a);
                } else {
                    offset = try self.parseArray(input, offset, visited, ptr.child, &a);
                }
                for (a.items) |item, i| {
                    if (i < arr.len) {
                        v.*[i] = item;
                    }
                }
                return offset;
            },
            .Struct =>
                return self.parseInlineTable(input, offset, visited, T, v),
            else => return ParseError.IncorrectDataType,
        }
    }

    fn lookupAndParseValue(self: *Parser, input: []const u8, offset_in: usize, keys: []Key, visited: *VisitedNode, comptime T: type, v: *T) anyerror!usize {
        var key = keys[0];
        errdefer for (keys[1..(keys.len)]) |k| {
            k.deinit();
        };
        if (keys.len == 1) {
            if (visited.visitedType(key) != .None)
                return ParseError.DuplicatedKey;
            if (T == Value) {
                if (v.* != .Table) {
                    return ParseError.IncorrectDataType;
                }
                try visited.markVisited(key, VisitType.Keyvalue);
                var nv = Value{.Bool = false};
                var offset = try self.parseValue(input, offset_in, visited, Value, &nv);
                _ = try v.*.Table.put(key, nv);
                return offset;
            } else {
                defer key.deinit();
                comptime var ti = @typeInfo(T);
                if (ti != .Struct)
                    return ParseError.IncorrectDataType;
                inline for (ti.Struct.fields) |field| {
                    if (mem.eql(u8, key.items, field.name)) {
                        try visited.markVisited(key, VisitType.Keyvalue);
                        return self.parseValue(input, offset_in, visited, field.field_type, &(@field(v, field.name)));
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
                    try visited.markVisited(key, VisitType.Keyvalue);
                }
                return self.lookupAndParseValue(
                    input, offset_in, keys[1..(keys.len)], visited.getChild(key).?,
                    Value, &gpr.kv.value);
            }
            return ParseError.IncorrectDataType;
        }
        defer key.deinit();
        comptime var ti = @typeInfo(T);
        if (ti != .Struct)
            return ParseError.IncorrectDataType;
        inline for (ti.Struct.fields) |field| {
            if (mem.eql(u8, key.items, field.name)) {
                comptime var fti = @typeInfo(field.field_type);
                if (fti == .Pointer and fti.Pointer.size == .One) {
                    if (visited.visitedType(key) != .None) {
                        return self.lookupAndParseValue(
                            input, offset_in, keys[1..(keys.len)], visited.getChild(key).?,
                            fti.Pointer.child, @field(v, field.name));
                    }
                    try visited.markVisited(key, VisitType.Keyvalue);
                    var f = try self.allocator.create(fti.Pointer.child);
                    errdefer self.allocator.destroy(f);
                    var offset = try self.lookupAndParseValue(
                        input, offset_in, keys[1..(keys.len)], visited, fti.Pointer.child, f);
                    @field(v, field.name) = f;
                    return offset;
                }
                if (visited.visitedType(key) == .None)
                    try visited.markVisited(key, VisitType.Keyvalue);
                return self.lookupAndParseValue(
                    input, offset_in, keys[1..(keys.len)], visited.getChild(key).?,
                    field.field_type, &(@field(v, field.name)));
            }
        }
        return ParseError.FieldNotFound;
    }

    fn parseAssign(self: *Parser, input: []const u8, offset_in: usize, visited: *VisitedNode, comptime T: type, data: *T) !usize {
        var keys = try self.parseKey(input, offset_in);
        defer keys.keys.deinit();
        if (input[keys.offset] != '=') {
            keys.deinit();
            return ParseError.FailedToParse;
        }
        return self.lookupAndParseValue(input, keys.offset+1, keys.keys.items, visited, T, data);
    }

    const parseBracketResult = struct{
        data: *Table,
        offset: usize
    };

    fn lookupAndParseKVs(
            self: *Parser, input: []const u8, offset_in: usize,
            keys: []Key, is_array: bool, visited: *VisitedNode, comptime T: type, data: *T) !usize {
        var key = keys[0];
        errdefer for (keys[1..(keys.len)]) |k| {
            k.deinit();
        };
        var vtype = visited.visitedType(key);
        if (keys.len == 1) {
            if (vtype == .Keyvalue)
                return ParseError.DuplicatedKey;
            if (vtype != .None and !is_array)
                return ParseError.DuplicatedKey;
            if (T == Value) {
                if (data.* != .Table) {
                    return ParseError.IncorrectDataType;   
                }
                var gpr = try data.Table.getOrPut(key);
                if (gpr.found_existing) {
                    defer key.deinit();
                    if (gpr.kv.value != .Array)
                        return ParseError.IncorrectDataType;
                    if (!is_array)
                        return ParseError.IncorrectDataType;
                    var v = Value{.Table = Table.init(self.allocator)};
                    errdefer v.deinit();
                    var vchild = visited.getChild(key).?;
                    var idx = gpr.kv.value.Array.items.len;
                    try vchild.markVisitedIdx(idx, .Bracket);
                    var offset = try self.parseKVs(
                        input, offset_in, vchild.getChildIdx(idx).?, Value, &v);
                    _ = try gpr.kv.value.Array.append(v);
                    return offset;
                }
                try visited.markVisited(key, VisitType.Bracket);
                if (is_array) {
                    var v = Value{.Table = Table.init(self.allocator)};
                    errdefer v.deinit();
                    var vchild = visited.getChild(key).?;
                    try vchild.markVisitedIdx(0, .Bracket);
                    var offset = try self.parseKVs(
                        input, offset_in, vchild.getChildIdx(0).?, Value, &v);
                    var a = std.ArrayList(Value).init(self.allocator);
                    errdefer a.deinit();
                    _ = try a.append(v);
                    gpr.kv.value = Value{.Array = a};
                    return offset;
                }
                gpr.kv.value = Value{.Table = Table.init(self.allocator)};
                errdefer gpr.kv.value.deinit();
                return self.parseKVs(
                    input, offset_in, visited.getChild(key).?, Value, &gpr.kv.value);
            }
            defer key.deinit();
            if (@typeInfo(T) != .Struct)
                return ParseError.IncorrectDataType;
            inline for (@typeInfo(T).Struct.fields) |field| {
                if (mem.eql(u8, key.items, field.name)) {
                    switch (@typeInfo(field.field_type)) {
                        .Pointer => |ptr| {
                            if (ptr.size == .One) {
                                if (is_array)
                                    return ParseError.IncorrectDataType;
                                if (vtype == .None)
                                    try visited.markVisited(key, VisitType.Bracket);
                                @field(data, field.name) = try self.createT(field.field_type);
                                errdefer self.destroyT(field.field_type, @field(data, field.name));
                                return self.parseKVs(
                                    input, offset_in, visited.getChild(key).?,
                                    ptr.child, @field(data, field.name));
                            }
                            if (!is_array)
                                return ParseError.IncorrectDataType;
                            var l = if (vtype == VisitType.None) 0 else @field(data, field.name).len;
                            if (l == 0) {
                                try visited.markVisited(key, VisitType.Bracket);
                                @field(data, field.name) = try self.allocator.alloc(ptr.child, 1);
                            } else {
                                @field(data, field.name) = try self.allocator.realloc(@field(data, field.name), l+1);
                            }
                            @field(data, field.name)[l] = try self.createT(ptr.child);
                            var vchild = visited.getChild(key).?;
                            try vchild.markVisitedIdx(l, .Bracket);
                            return self.parseKVs(
                                input, offset_in, vchild.getChildIdx(l).?,
                                ptr.child, &(@field(data, field.name)[l]));
                        },
                        .Struct => |str| {
                            if (vtype == .None)
                                try visited.markVisited(key, VisitType.Bracket);
                            return self.parseKVs(
                                input, offset_in, visited.getChild(key).?,
                                field.field_type, &(@field(data, field.name)));
                        },
                        else => {},
                    }
                }
            }
            return ParseError.IncorrectDataType;
        }

        if (vtype == .Keyvalue)
            return ParseError.DuplicatedKey;

        if (T == Value) {
            if (data.* != .Table) {
                return ParseError.IncorrectDataType;   
            }
            var gpr = try data.Table.getOrPut(key);

            if (gpr.found_existing) {
                defer key.deinit();
                if (gpr.kv.value == .Array) {
                    var idx = gpr.kv.value.Array.items.len-1;
                    return self.lookupAndParseKVs(
                        input, offset_in, keys[1..(keys.len)], is_array,
                        visited.getChild(key).?.getChildIdx(idx).?,
                        T, &gpr.kv.value.Array.items[idx]);
                }
                return self.lookupAndParseKVs(
                    input, offset_in, keys[1..(keys.len)], is_array,
                    visited.getChild(key).?, T, &gpr.kv.value);
            }
            try visited.markVisited(key, VisitType.Bracket);
            gpr.kv.value = Value{.Table = Table.init(self.allocator)};
            errdefer gpr.kv.value.deinit();
            return self.lookupAndParseKVs(
                input, offset_in, keys[1..(keys.len)], is_array,
                visited.getChild(key).?,
                T, &gpr.kv.value);
        }
        defer key.deinit();
        if (@typeInfo(T) != .Struct)
            return ParseError.IncorrectDataType;
        inline for (@typeInfo(T).Struct.fields) |field| {
            if (mem.eql(u8, key.items, field.name)) {
                switch (@typeInfo(field.field_type)) {
                    .Pointer => |ptr| {
                        if (ptr.size == .One) {
                            if (vtype == .Bracket) {
                                return self.lookupAndParseKVs(
                                    input, offset_in, keys[1..(keys.len)], is_array,
                                    visited.getChild(key).?,
                                    ptr.child, @field(data, field.name));
                            }
                            try visited.markVisited(key, VisitType.Bracket);
                            @field(data, field.name) = try self.createT(field.field_type);
                            errdefer self.destroyT(field.field_type, @field(data, field.name));
                            return self.lookupAndParseKVs(
                                input, offset_in, keys[1..(keys.len)], is_array,
                                visited.getChild(key).?, ptr.child,
                                @field(data, field.name));
                        }
                        if (vtype == .None)
                            return ParseError.FailedToParse;
                        var idx = @field(data, field.name).len - 1;
                        return self.lookupAndParseKVs(
                            input, offset_in, keys[1..(keys.len)], is_array,
                            visited.getChild(key).?.getChildIdx(idx).?,
                            ptr.child, &(@field(data, field.name)[idx]));
                    },
                    .Struct => |str| {
                        try visited.markVisited(key, VisitType.Bracket);
                        return self.lookupAndParseKVs(
                            input, offset_in, keys[1..(keys.len)], is_array,
                            visited.getChild(key).?,
                            field.field_type, &(@field(data, field.name)));
                    },
                    else => {},
                }
            }
        }
        return ParseError.IncorrectDataType;
    }

    fn parseBracket(self: *Parser, input: []const u8, offset_in: usize, visited: *VisitedNode, comptime T: type, data: *T) !usize {
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
        return self.lookupAndParseKVs(input, offset, keys.keys.items, count > 1, visited, T, data);
    }

    fn createT(self: *Parser, comptime T: type) !T {
        if (T == Value) {
            return Value{.Table = Table.init(self.allocator)};
        } else if (@typeInfo(T) == .Struct and @hasDecl(T, "init")) {
            comptime var ft = @typeInfo(@TypeOf(T.init)).Fn;
            if (ft.return_type == T and ft.args.len == 1 and ft.args[0].arg_type.? == *std.mem.Allocator) {
                return T.init(self.allocator);
            }
        } else {
            switch (@typeInfo(T)) {
                .Pointer => |ptr| return self.allocator.create(ptr.child),
                else => return undefined,
            }
        }
        return ParseError.UnknownReturnType;
    }

    fn destroyT(self: *Parser, comptime T: type, v: T) void {
        if (T == Value) {
            return v.deinit();
        } else if (@typeInfo(T) == .Struct and @hasDecl(T, "deinit")) {
            switch (@typeInfo(@TypeOf(v.deinit))) {
                .BoundFn => |f| if (f.args.len == 0) {
                    v.deinit();
                },
                else => {},
            }
        } else {
            switch (@typeInfo(T)) {
                .Pointer => |ptr| self.allocator.destroy(v),
                else => {},
            }
        }
    }

    pub fn parseKVs(self: *Parser, input: []const u8, offset_in: usize, visited: *VisitedNode, comptime T: type, value: *T) !usize {
        if (T == Value and value.* != .Table) {
            return ParseError.IncorrectDataType;
        }
        var offset: usize = offset_in;
        while (offset < input.len) {
            offset = self.skipSpacesAndComments(input, offset);
            if (offset >= input.len or input[offset] == '[') {
                return offset;
            }
            offset = try self.parseAssign(input, offset, visited, T, value);
        }
        return offset;
    }

    pub fn parse(self: *Parser, comptime T: type, input: []const u8) !T {
        var visited = VisitedNode.init(self.allocator);
        defer visited.deinit();
        var top = try self.createT(T);
        errdefer self.destroyT(T, top);
        var offset = try switch (@typeInfo(T)) {
            .Pointer => |ptr| self.parseKVs(input, 0, &visited, ptr.child, top),
            else => self.parseKVs(input, 0, &visited, T, &top),
        };
        while (offset < input.len) {
            offset = self.skipSpacesAndComments(input, offset);
            if (offset >= input.len) {
                break;
            }
            offset = try switch(@typeInfo(T)) {
                .Pointer => |ptr| self.parseBracket(input, offset, &visited, ptr.child, top),
                else => self.parseBracket(input, offset, &visited, T, &top),
            };
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

test "simple-struct" {
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
    expect(parsed.foo == 42);
    expect(parsed.bar == 42.0);
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

test "string-struct" {
    const st = struct {
        foo: []u8,
    };

    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(st, "#test\n \t foo\t=  \"bar\"   \n");
    defer allocator.free(parsed.foo);
    expect(mem.eql(u8, parsed.foo, "bar"));
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

test "bracket-struct" {
    const foo = struct {
        bar: i64,
        baz: bool,
    };
    const quox = struct {
        bar: i64,
    };
    const st = struct {
        foo: *foo,
        quox: quox,
    };
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(st, "[foo]\nbar=42\nbaz=true\n[quox]\nbar=96\n");
    defer allocator.destroy(parsed.foo);
    expect(parsed.foo.bar == 42);
    expect(parsed.foo.baz);
    expect(parsed.quox.bar == 96);
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

const subtable_text =
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
;

test "double-bracket-subtable" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(Value, subtable_text);
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

const fruit_physical = struct {
    color: []u8,
    shape: []u8,
};

const fruit_variety = struct {
    name: []u8,
};

const fruit = struct {
    name: []u8,
    physical: fruit_physical,
    variety: []fruit_variety,
    allocator: *mem.Allocator,

    fn init(allocator: *mem.Allocator) fruit {
        return .{
            .name = "",
            .physical = .{.color = "", .shape = ""},
            .variety = &[0]fruit_variety{},
            .allocator = allocator};
    }

    fn deinit(self: fruit) void {
        self.allocator.free(self.name);
        self.allocator.free(self.physical.color);
        self.allocator.free(self.physical.shape);
        for (self.variety) |v| {
            self.allocator.free(v.name);
        }
        self.allocator.free(self.variety);
    }
};

const tfruits = struct {
    fruit: []fruit,
    allocator: *mem.Allocator,

    fn init(allocator: *mem.Allocator) tfruits {
        return .{.fruit = &[0]fruit{}, .allocator = allocator};
    }

    fn deinit(self: tfruits) void {
        for (self.fruit) |f| {
            f.deinit();
        }
        self.allocator.free(self.fruit);
    }
};

test "double-bracket-subtable-struct" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(tfruits, subtable_text);
    defer allocator.free(parsed.fruit);
    defer for (parsed.fruit) |f| {
        f.deinit();
    };

    expect(parsed.fruit.len == 2);
    var apple = parsed.fruit[0];
    expect(mem.eql(u8, apple.name, "apple"));
    expect(mem.eql(u8, apple.physical.color, "red"));
    expect(mem.eql(u8, apple.physical.shape, "round"));
    expect(apple.variety.len == 2);
    expect(mem.eql(u8, apple.variety[0].name, "red delicious"));
    expect(mem.eql(u8, apple.variety[1].name, "granny smith"));

    var banana = parsed.fruit[1];
    expect(mem.eql(u8, banana.name, "banana"));
    expect(banana.variety.len == 1);
    expect(mem.eql(u8, banana.variety[0].name, "plantain"));
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
    expect((try parser.parseFloat("inf", 0, &f)) == 3);
    expect(f == std.math.inf(f64));
    expect((try parser.parseFloat("-inf", 0, &f)) == 4);
    expect(f == -std.math.inf(f64));
    expect((try parser.parseFloat("nan", 0, &f)) == 3);
    expect(std.math.isNan(f));
    expect((try parser.parseFloat("+nan", 0, &f)) == 4);
    expect(std.math.isNan(f));
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

test "parseArray" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var a = std.ArrayList(Value).init(allocator);
    defer a.deinit();
    var v = VisitedNode.init(allocator);
    defer v.deinit();
    expect((try parser.parseArray("[ 42, 43.0,\n true, false ]", 0, &v, Value, &a)) == 26);
    expect(a.items.len == 4);
    expect(a.items[0].Int == 42);
    expect(a.items[1].Float == 43.0);
    expect(a.items[2].Bool);
    expect(!a.items[3].Bool);
}

test "parseInlineTable" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var t = Value{.Table = Table.init(allocator)};
    defer t.deinit();
    var v = VisitedNode.init(allocator);
    defer v.deinit();
    expect((try parser.parseInlineTable("{ x = 1, y = 2 }",
        0, &v, Value, &t)) == 16);
    expect(t.Table.size == 2);
    expect(getS(t.Table, "x").?.value.Int == 1);
    expect(getS(t.Table, "y").?.value.Int == 2);
}