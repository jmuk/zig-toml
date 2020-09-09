const std = @import("std");
const ascii = std.ascii;
const fmt = std.fmt;
const mem = std.mem;
const unicode = std.unicode;

const testing = std.testing;
const expect = testing.expect;

// Key is a key of hash tables to be used in this package.
pub const Key = std.ArrayList(u8);

// Table is a table from a key to a TOML value.
pub const Table = std.HashMap(Key, Value, hashKey, eqlKey);

fn eqlKey(a: Key, b: Key) bool {
    return std.mem.eql(u8, a.items, b.items);
}

fn hashKey(a: Key) u32 {
    return std.hash_map.hashString(a.items);
}

pub const ValueTag = enum {
    String, Int, Float, Bool, Table, Array,
};

// Value represents a TOML value.
// TODO: support date/datetime format.
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

    pub fn get(self: Value, key: []const u8) ?Value {
        if (self != .Table)
            return null;
        var a = Key.init(self.Table.allocator);
        defer a.deinit();
        if (a.appendSlice(key)) |_| {
            if (self.Table.get(a)) |kv| {
                return kv.value;
            } else {
                return null;
            }
        } else |err| {
            return null;
        }
    }

    pub fn idx(self: Value, i: usize) ?Value {
        if (self != .Array)
            return null;
        if (i >= self.Array.items.len)
            return null;
        return self.Array.items[i];
    }

    // size returns the number of children in the value.
    // If self is a string, it returns the string length.
    // It returns 0 for other leaf values.
    pub fn size(self: Value) usize {
        switch (self) {
            .Table => |tbl| return tbl.size,
            .Array => |arr| return arr.items.len,
            .String => |str| return str.items.len,
            else => return 0,
        }
    }
};

// ParseError is the possible error during parsing.
pub const ParseError = error {
    // Generic failure of parsing, incorrect syntax.
    FailedToParse,

    // The key name conflict.
    DuplicatedKey,

    // The receiving data type is not as expected.
    IncorrectDataType,

    // The return type is not supported.
    UnknownReturnType,

    // No related field is found for a key.
    FieldNotFound,

    // Expected to see a linebeak but could not found.
    LinebreakExpected,
};

const VisitType = enum {
    // Not visited
    None,

    // Unable to extend through bracket
    CantExtend,

    // Unable to extend through both bracket and keyvalue
    CantExtendKeyvalue,

    // Can extend by bracket
    Extendable,
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
        var copiedKey = Key.init(self.allocator);
        _ = copiedKey.appendSlice(key.items) catch |err| {
            copiedKey.deinit();
            return err;
        };
        var gpr = self.children.getOrPut(copiedKey) catch |err| {
            copiedKey.deinit();
            return err;
        };
        if (gpr.found_existing) {
            copiedKey.deinit();
            gpr.kv.value.visit_type = visit_type;
        } else {
            var v = VisitedNode.init(self.allocator);
            v.visit_type = visit_type;
            gpr.kv.value = v;
        }
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

// Parser is a parser of TOML. Use 'parse' method to create the parsed result.
// It takes an allocator as a parameter, and the parse results and its internal
// structure will be allocated by this allocator.
// If the specified type has init() and deinit() functions, it will use these
// methods for allocating/deallocating the object.
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

    fn skipSpacesAndCommentsWithLinebreak(self: *Parser, input: []const u8, offset: usize, has_linebreak: *bool) usize {
        var i = offset;
        while (i < input.len) : (i+=1) {
            if (input[i] == '#') {
                while (i < input.len and input[i] != '\n') : (i+=1) {}
                has_linebreak.* = true;
            } else if (input[i] == '\n') {
                has_linebreak.* = true;
            } else if (!ascii.isSpace(input[i])) {
                return i;
            }
        }
        return i;
    }

    fn skipSpacesAndComments(self: *Parser, input: []const u8, offset: usize) usize {
        var has_linebreak = false;
        return self.skipSpacesAndCommentsWithLinebreak(input, offset, &has_linebreak);
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

    fn parseInt(self: *Parser, input: []const u8, offset: usize, comptime T: type, value: *T) !usize {
        var i = offset;
        var start = offset;
        var initial = true;
        var underscore_count: u64 = 0;
        var base: u8 = 10;
        while (i < input.len) : (i+=1) {
            if (initial and (input[i] == '+' or input[i] == '-')) {
                // Negative value is set for unsigned types.
                if (!@typeInfo(T).Int.is_signed and input[i] == '-')
                    return ParseError.IncorrectDataType;
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
        value.* = try fmt.parseInt(T, buf, base);
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

    fn parseFloat(self: *Parser, input: []const u8, offset: usize, comptime T: type, value: *T) !usize {
        if (self.checkSignedPattern(input, offset, "inf")) |out| {
            if (input[offset] == '-') {
                value.* = -std.math.inf(T);
            } else {
                value.* = std.math.inf(T);
            }
            return out;
        }
        if (self.checkSignedPattern(input, offset, "nan")) |out| {
            value.* = std.math.nan(T);
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
        value.* = try fmt.parseFloat(T, input[offset..i]);
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
        if (hasPrefix(input[start..], "\"\"")) {
            start+=2;
            multiLine = true;
            if (start < input.len and input[start] == '\n')
                start+=1;
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
                end+=3;
                if (end < input.len and input[end] == '"') {
                    _ = try s.append('"');
                    end+=1;
                    if (end < input.len and input[end] == '"') {
                        _ = try s.append('"');
                        end+=1;
                    }
                }
                return end;
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
            if (start < input.len and input[start] == '\n')
                start+=1;
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
            if (self.parseFloat(input, offset, f64, &f)) |offset_out| {
                v.* = Value{.Float = f};
                return offset_out;
            } else |err| {
                var i: i64 = 0;
                offset = try self.parseInt(input, offset, i64, &i);
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
            .Int => return self.parseInt(input, offset, T, v),
            .Float => return self.parseFloat(input, offset, T, v),
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
        var key = Key.init(self.allocator);
        try key.appendSlice(keys[0].items);

        if (keys.len == 1) {
            if (visited.visitedType(key) != .None) {
                key.deinit();
                return ParseError.DuplicatedKey;
            }
            if (T == Value) {
                errdefer key.deinit();
                if (v.* != .Table) {
                    return ParseError.IncorrectDataType;
                }
                try visited.markVisited(key, .CantExtendKeyvalue);
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
                        try visited.markVisited(key, .CantExtendKeyvalue);
                        return self.parseValue(input, offset_in, visited, field.field_type, &(@field(v, field.name)));
                    }
                }
                return ParseError.FieldNotFound;
            }
        }
        var newvtype = if (keys.len > 2) VisitType.Extendable else VisitType.CantExtend;
        var vtype = visited.visitedType(key);
        if (vtype == .CantExtendKeyvalue) {
            key.deinit();
            return ParseError.DuplicatedKey;
        }
        if (T == Value) {
            if (v.* == .Table) {
                var gpr = try v.*.Table.getOrPut(key);
                defer if (gpr.found_existing)
                    key.deinit();
                if (gpr.found_existing) {
                    if (gpr.kv.value != .Table) {
                        return ParseError.IncorrectDataType;
                    }
                } else {
                    gpr.kv.value = Value{.Table = Table.init(self.allocator)};
                }
                if (vtype == .None or newvtype == .Extendable)
                    try visited.markVisited(key, newvtype);
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
                    try visited.markVisited(key, newvtype);
                    var f = try self.allocator.create(fti.Pointer.child);
                    errdefer self.allocator.destroy(f);
                    var offset = try self.lookupAndParseValue(
                        input, offset_in, keys[1..(keys.len)], visited, fti.Pointer.child, f);
                    @field(v, field.name) = f;
                    return offset;
                }
                if (vtype == .None or newvtype == .Extendable)
                    try visited.markVisited(key, newvtype);
                return self.lookupAndParseValue(
                    input, offset_in, keys[1..(keys.len)], visited.getChild(key).?,
                    field.field_type, &(@field(v, field.name)));
            }
        }
        return ParseError.FieldNotFound;
    }

    fn parseAssign(self: *Parser, input: []const u8, offset_in: usize, visited: *VisitedNode, comptime T: type, data: *T) !usize {
        var keys = try self.parseKey(input, offset_in);
        defer keys.deinit();
        if (input[keys.offset] != '=') {
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
        var key = Key.init(self.allocator);
        try key.appendSlice(keys[0].items);
        var vtype = visited.visitedType(key);
        if (keys.len == 1) {
            if (vtype == .CantExtend or vtype == .CantExtendKeyvalue) {
                key.deinit();
                return ParseError.DuplicatedKey;
            }
            if (!is_array and vtype != .None) {
                key.deinit();
                return ParseError.DuplicatedKey;
            }
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
                    try vchild.markVisitedIdx(idx, .Extendable);
                    var offset = try self.parseKVs(
                        input, offset_in, vchild.getChildIdx(idx).?, Value, &v);
                    _ = try gpr.kv.value.Array.append(v);
                    return offset;
                }
                try visited.markVisited(key, .Extendable);
                if (is_array) {
                    var v = Value{.Table = Table.init(self.allocator)};
                    errdefer v.deinit();
                    var vchild = visited.getChild(key).?;
                    try vchild.markVisitedIdx(0, .Extendable);
                    var offset = try self.parseKVs(
                        input, offset_in, vchild.getChildIdx(0).?, Value, &v);
                    var a = std.ArrayList(Value).init(self.allocator);
                    errdefer a.deinit();
                    _ = try a.append(v);
                    gpr.kv.value = Value{.Array = a};
                    return offset;
                }
                gpr.kv.value = Value{.Table = Table.init(self.allocator)};
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
                                    try visited.markVisited(key, .Extendable);
                                @field(data, field.name) = try self.createT(field.field_type);
                                errdefer self.destroyT(field.field_type, @field(data, field.name));
                                return self.parseKVs(
                                    input, offset_in, visited.getChild(key).?,
                                    ptr.child, @field(data, field.name));
                            }
                            if (!is_array)
                                return ParseError.IncorrectDataType;
                            var l = if (vtype == .None) 0 else @field(data, field.name).len;
                            if (l == 0) {
                                try visited.markVisited(key, .Extendable);
                                @field(data, field.name) = try self.allocator.alloc(ptr.child, 1);
                            } else {
                                @field(data, field.name) = try self.allocator.realloc(@field(data, field.name), l+1);
                            }
                            @field(data, field.name)[l] = try self.createT(ptr.child);
                            var vchild = visited.getChild(key).?;
                            try vchild.markVisitedIdx(l, .Extendable);
                            return self.parseKVs(
                                input, offset_in, vchild.getChildIdx(l).?,
                                ptr.child, &(@field(data, field.name)[l]));
                        },
                        .Struct => |str| {
                            if (vtype == .None)
                                try visited.markVisited(key, .Extendable);
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

        if (vtype == .CantExtend or vtype == .CantExtendKeyvalue)
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
            try visited.markVisited(key, .Extendable);
            gpr.kv.value = Value{.Table = Table.init(self.allocator)};
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
                            if (vtype == .Extendable) {
                                return self.lookupAndParseKVs(
                                    input, offset_in, keys[1..(keys.len)], is_array,
                                    visited.getChild(key).?,
                                    ptr.child, @field(data, field.name));
                            }
                            try visited.markVisited(key, .Extendable);
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
                        try visited.markVisited(key, .Extendable);
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
        defer keys.deinit();
        var i: usize = 0;
        while (i < count) : (i += 1) {
            if (input[keys.offset+i] != ']')
                return ParseError.FailedToParse;
        }
        offset = keys.offset+count;
        var has_linebreak = false;
        offset = self.skipSpacesAndCommentsWithLinebreak(input, offset, &has_linebreak);
        if (offset >= input.len)
            return offset;
        if (!has_linebreak)
            return ParseError.LinebreakExpected;
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

    fn parseKVs(self: *Parser, input: []const u8, offset_in: usize, visited: *VisitedNode, comptime T: type, value: *T) !usize {
        if (T == Value and value.* != .Table) {
            return ParseError.IncorrectDataType;
        }
        var offset: usize = offset_in;
        offset = self.skipSpacesAndComments(input, offset);
        while (offset < input.len) {
            if (input[offset] == '[') {
                return offset;
            }
            offset = try self.parseAssign(input, offset, visited, T, value);
            var has_linebreak = false;
            offset = self.skipSpacesAndCommentsWithLinebreak(input, offset, &has_linebreak);
            if (offset < input.len and !has_linebreak)
                return ParseError.LinebreakExpected;
        }
        return offset;
    }

    // parse parses the given input as a TOML data.
    pub fn parse(self: *Parser, comptime T: type, input: []const u8) !T {
        var visited = VisitedNode.init(self.allocator);
        defer visited.deinit();
        var top = try self.createT(T);
        errdefer self.destroyT(T, top);
        var offset = try switch (@typeInfo(T)) {
            .Pointer => |ptr| self.parseKVs(input, 0, &visited, ptr.child, top),
            else => self.parseKVs(input, 0, &visited, T, &top),
        };
        offset = self.skipSpacesAndComments(input, offset);
        while (offset < input.len) {
            offset = try switch(@typeInfo(T)) {
                .Pointer => |ptr| self.parseBracket(input, offset, &visited, ptr.child, top),
                else => self.parseBracket(input, offset, &visited, T, &top),
            };
            offset = self.skipSpacesAndComments(input, offset);
        }
        return top;
    }
};

// helper for tests.
fn checkS(v: Value, key: []const u8, e: []const u8) bool {
    if (v.get(key)) |r| {
        if (r == .String) {
            return mem.eql(u8, r.String.items, e);
        } else {
            std.debug.warn("value {} is not a string\n", .{r});
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
    expect(parsed.get("foo").?.Int == 42);
    expect(parsed.get("bar").?.Float == 42.0);
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
    expect(parsed.size() == 1);
    expect(checkS(parsed, "foo", "bar"));
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
    expect(parsed.size() == 2);
    var o1 = parsed.get("foo").?;
    expect(o1.size() == 2);
    expect(o1.get("bar").?.Int == 42);
    expect(o1.get("baz").?.Bool);
    var o2 = parsed.get("quox").?;
    expect(o2.size() == 1);
    expect(o2.get("bar").?.Int == 96);
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
    expect(parsed.size() == 1);
    var a = parsed.get("foo").?;
    expect(a.size() == 2);
    var o1 = a.idx(0).?;
    expect(o1.size() == 2);
    expect(o1.get("bar").?.Int == 42);
    expect(o1.get("baz").?.Bool);
    var o2 = a.idx(1).?;
    expect(o2.size() == 1);
    expect(o2.get("bar").?.Int == 96);
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

    expect(parsed.size() == 1);
    var fruits = parsed.get("fruit").?;
    expect(fruits.size() == 2);
    var apple = fruits.idx(0).?;
    expect(apple.size() == 3);
    expect(checkS(apple, "name", "apple"));
    var physical = apple.get("physical").?;
    expect(checkS(physical, "color", "red"));
    expect(checkS(physical, "shape", "round"));
    var varieties = apple.get("variety").?;
    expect(varieties.size() == 2);
    expect(checkS(varieties.idx(0).?, "name", "red delicious"));
    expect(checkS(varieties.idx(1).?, "name", "granny smith"));

    var banana = fruits.idx(1).?;
    expect(banana.size() == 2);
    expect(checkS(banana, "name", "banana"));
    varieties = banana.get("variety").?;
    expect(varieties.size() == 1);
    expect(checkS(varieties.idx(0).?, "name", "plantain"));
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

const example_data =
    \\# This is a TOML document
    \\
    \\title = "TOML Example"
    \\
    \\[owner]
    \\name = "Tom Preston-Werner"
    \\#datetime is not supported yet
    \\#dob = 1979-05-27T07:32:00-08:00
    \\
    \\[database]
    \\enabled = true
    \\ports = [ 8001, 8001, 8002 ]
    \\data = [ ["delta", "phi"], [3.14] ]
    \\temp_targets = { cpu = 79.5, case = 72.0 }
    \\
    \\[servers]
    \\
    \\[servers.alpha]
    \\ip = "10.0.0.1"
    \\role = "frontend"
    \\
    \\[servers.beta]
    \\ip = "10.0.0.2"
    \\role = "backend"
;

test "example" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);
    var parsed = try parser.parse(Value, example_data);
    defer parsed.deinit();

    expect(parsed.size() == 4);
    expect(checkS(parsed, "title", "TOML Example"));
    var owner = parsed.get("owner").?;
    expect(owner.size() == 1);
    expect(checkS(owner, "name", "Tom Preston-Werner"));

    var database = parsed.get("database").?;
    expect(database.get("enabled").?.Bool);
    var ports = database.get("ports").?;
    expect(ports.size() == 3);
    expect(ports.idx(0).?.Int == 8001);
    expect(ports.idx(1).?.Int == 8001);
    expect(ports.idx(2).?.Int == 8002);
    var data = database.get("data").?;
    expect(data.size() == 2);
    expect(mem.eql(u8, data.idx(0).?.idx(0).?.String.items, "delta"));
    expect(mem.eql(u8, data.idx(0).?.idx(1).?.String.items, "phi"));
    expect(data.idx(1).?.idx(0).?.Float == 3.14);
    var temp_targets = database.get("temp_targets").?;
    expect(temp_targets.size() == 2);
    expect(temp_targets.get("cpu").?.Float == 79.5);
    expect(temp_targets.get("case").?.Float == 72.0);

    var servers = parsed.get("servers").?;
    expect(servers.size() == 2);
    var alpha = servers.get("alpha").?;
    expect(checkS(alpha, "ip", "10.0.0.1"));
    expect(checkS(alpha, "role", "frontend"));
    var beta = servers.get("beta").?;
    expect(checkS(beta, "ip", "10.0.0.2"));
    expect(checkS(beta, "role", "backend"));
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
    expect((try parser.parseInt("123", 0, i64, &n)) == 3);
    expect(n == 123);

    expect((try parser.parseInt("-123", 0, i64, &n)) == 4);
    expect(n == -123);

    expect((try parser.parseInt("+123_456_789", 0, i64, &n)) == 12);
    expect(n == 123456789);

    expect((try parser.parseInt("0XFF", 0, i64, &n)) == 4);
    expect(n == 255);

    expect((try parser.parseInt("0Xa", 0, i64, &n)) == 3);
    expect(n == 10);

    expect((try parser.parseInt("0o20", 0, i64, &n)) == 4);
    expect(n == 16);

    expect((try parser.parseInt("0b0100", 0, i64, &n)) == 6);
    expect(n == 4);

    // hexadecimal with underscore.
    expect((try parser.parseInt("0xa_1", 0, i64, &n)) == 5);
    expect(n == 161);

    // invalid octal.
    if (parser.parseInt("0o9", 0, i64, &n) catch |err| e: {
        expect(err == ParseError.FailedToParse);
        break :e 0;
    } == 0);

    // invalid binary.
    expect(parser.parseInt("0b2", 0, i64, &n) catch |err| e: {
        expect(err == ParseError.FailedToParse);
        break :e 0;
    } == 0);

    // invalid hexadecimal.
    expect(parser.parseInt("0xQ", 0, i64, &n) catch |err| e: {
        expect(err == ParseError.FailedToParse);
        break :e 0;
    } == 0);

    // invalid prefix.
    expect(parser.parseInt("0q0", 0, i64, &n) catch |err| e: {
        expect(err == ParseError.FailedToParse);
        break :e 0;
    } == 0);

    // signs can't be combined with prefix.
    expect(parser.parseInt("+0xdeadbeef", 0, i64, &n) catch |err| e: {
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

    comptime var types = [2]type{f64, f32};
    inline for (types) |t| {
        var f: t = 0.0;
        expect((try parser.parseFloat("1.5", 0, t, &f)) == 3);
        expect(f == 1.5);
        expect((try parser.parseFloat("-1e2", 0, t, &f)) == 4);
        expect(f == -1e2);
        expect((try parser.parseFloat(".2e-1", 0, t, &f)) == 5);
        expect(f == 0.2e-1);
        expect((try parser.parseFloat("1.e+2", 0, t, &f)) == 5);
        expect(f == 1e+2);
        expect((try parser.parseFloat("inf", 0, t, &f)) == 3);
        expect(f == std.math.inf(t));
        expect((try parser.parseFloat("-inf", 0, t, &f)) == 4);
        expect(f == -std.math.inf(t));
        expect((try parser.parseFloat("nan", 0, t, &f)) == 3);
        expect(std.math.isNan(f));
        expect((try parser.parseFloat("+nan", 0, t, &f)) == 4);
        expect(std.math.isNan(f));
    }
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
    expect(t.size() == 2);
    expect(t.get("x").?.Int == 1);
    expect(t.get("y").?.Int == 2);
}

// examples are from https://toml.io/en/v1.0.0-rc.1.
test "examples1" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var parsed = try parser.parse(Value,
        \\# This is a full-line comment
        \\key = "value"  # This is a comment at the end of a line
        \\another = "# This is not a comment"
    );
    defer parsed.deinit();
    expect(checkS(parsed, "key", "value"));
    expect(checkS(parsed, "another", "# This is not a comment"));
}

test "examples2" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var parsed = try parser.parse(Value,
        \\key = "value"
        \\bare_key = "value"
        \\bare-key = "value"
        \\1234 = "value"
        \\"127.0.0.1" = "value"
        \\"character encoding" = "value"
        \\"ʎǝʞ" = "value"
        \\'key2' = "value"
        \\'quoted "value"' = "value"
    );
    defer parsed.deinit();
    expect(checkS(parsed, "key", "value"));
    expect(checkS(parsed, "bare_key", "value"));
    expect(checkS(parsed, "bare-key", "value"));
    expect(checkS(parsed, "1234", "value"));
    expect(checkS(parsed, "127.0.0.1", "value"));
    expect(checkS(parsed, "character encoding", "value"));
    expect(checkS(parsed, "ʎǝʞ", "value"));
    expect(checkS(parsed, "key2", "value"));
    expect(checkS(parsed, "quoted \"value\"", "value"));
}

test "example3" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var parsed = try parser.parse(Value,
        \\"" = "blank"     # VALID but discouraged
    );
    defer parsed.deinit();
    expect(checkS(parsed, "", "blank"));
}

test "example4" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var parsed = try parser.parse(Value,
        \\name = "Orange"
        \\physical.color = "orange"
        \\physical.shape = "round"
        \\site."google.com" = true
    );
    defer parsed.deinit();
    expect(checkS(parsed, "name", "Orange"));
    expect(checkS(parsed.get("physical").?, "color", "orange"));
    expect(checkS(parsed.get("physical").?, "shape", "round"));
    expect(parsed.get("site").?.get("google.com").?.Bool);
}

test "example5" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var parsed = try parser.parse(Value, "3.14159 = \"pi\"");
    defer parsed.deinit();
    expect(checkS(parsed.get("3").?, "14159", "pi"));
}

test "example6" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var parsed = try parser.parse(Value,
        \\# This makes the key "fruit" into a table.
        \\fruit.apple.smooth = true
        \\
        \\# So then you can add to the table "fruit" like so:
        \\fruit.orange = 2
    );
    defer parsed.deinit();
    expect(parsed.get("fruit").?.get("apple").?.get("smooth").?.Bool);
    expect(parsed.get("fruit").?.get("orange").?.Int == 2);
}

fn deepEqual(v1: Value, v2: Value) bool {
    switch (v1) {
        .Int => |i| return (v2 == .Int) and i == v2.Int,
        .Float => |f1| return (v2 == .Float) and f1 == v2.Float,
        .Bool => |b1| return (v2 == .Bool) and b1 == v2.Bool,
        .String => |s1| return (v2 == .String) and mem.eql(u8, s1.items, v2.String.items),
        .Array => |a1| {
            if (v2 != .Array)
                return false;
            if (a1.items.len != v2.Array.items.len)
                return false;
            for (a1.items) |e, i| {
                if (!deepEqual(e, v2.Array.items[i]))
                    return false;
            }
            return true;
        },
        .Table => |t1| {
            if (v2 != .Table)
                return false;
            if (t1.size != v2.Table.size)
                return false;
            var iter = t1.iterator();
            while (iter.next()) |kv| {
                if (v2.Table.get(kv.key)) |kv2| {
                    if (!deepEqual(kv.value, kv2.value))
                        return false;
                } else {
                    return false;
                }
            }
            return true;
        },
    }
}

test "example7" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var p1 = try parser.parse(Value,
        \\apple.type = "fruit"
        \\orange.type = "fruit"
        \\
        \\apple.skin = "thin"
        \\orange.skin = "thick"
        \\
        \\apple.color = "red"
        \\orange.color = "orange"
    );
    defer p1.deinit();
    var p2 = try parser.parse(Value,
        \\apple.type = "fruit"
        \\apple.skin = "thin"
        \\apple.color = "red"
        \\
        \\orange.type = "fruit"
        \\orange.skin = "thick"
        \\orange.color = "orange"
    );
    defer p2.deinit();
    expect(deepEqual(p1, p2));
}

test "exapmle8" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var parsed = try parser.parse(Value,
        \\str = "I'm a string. \"You can quote me\". Name\tJos\u00E9\nLocation\tSF."
        \\str1 = """
        \\Roses are red
        \\Violets are blue"""
        \\# On a Unix system, the above multi-line string will most likely be the same as:
        \\str2 = "Roses are red\nViolets are blue"
        \\
        \\# On a Windows system, it will most likely be equivalent to:
        \\str3 = "Roses are red\r\nViolets are blue"
        \\str4 = "The quick brown fox jumps over the lazy dog."
        \\
        \\str5 = """
        \\The quick brown \
        \\
        \\
        \\  fox jumps over \
        \\    the lazy dog."""
        \\
        \\str6 = """\
        \\       The quick brown \
        \\       fox jumps over \
        \\       the lazy dog.\
        \\       """
        \\str7 = """Here are two quotation marks: "". Simple enough."""
        \\# str5 = """Here are three quotation marks: """."""  # INVALID
        \\str8 = """Here are three quotation marks: ""\"."""
        \\str9 = """Here are fifteen quotation marks: ""\"""\"""\"""\"""\"."""
        \\
        \\# "This," she said, "is just a pointless statement."
        \\str10 = """"This," she said, "is just a pointless statement.""""
        \\winpath  = 'C:\Users\nodejs\templates'
        \\winpath2 = '\\ServerX\admin$\system32\'
        \\quoted   = 'Tom "Dubs" Preston-Werner'
        \\regex    = '<\i\c*\s*>'
        \\regex2 = '''I [dw]on't need \d{2} apples'''
        \\lines  = '''
        \\The first newline is
        \\trimmed in raw strings.
        \\   All other whitespace
        \\   is preserved.
        \\'''
        \\quot15 = '''Here are fifteen quotation marks: """""""""""""""'''
        \\
        \\# apos15 = '''Here are fifteen apostrophes: ''''''''''''''''''  # INVALID
        \\apos15 = "Here are fifteen apostrophes: '''''''''''''''"
        \\
        \\# 'That's still pointless', she said.
        \\str11 = ''''That's still pointless', she said.'''
    );
    defer parsed.deinit();

    expect(checkS(parsed, "str", "I'm a string. \"You can quote me\". Name\tJos\xC3\xA9\nLocation\tSF."));
    expect(checkS(parsed, "str1", "Roses are red\nViolets are blue"));
    expect(checkS(parsed, "str2", "Roses are red\nViolets are blue"));
    expect(checkS(parsed, "str3", "Roses are red\r\nViolets are blue"));
    expect(checkS(parsed, "str4", "The quick brown fox jumps over the lazy dog."));
    expect(checkS(parsed, "str5", "The quick brown fox jumps over the lazy dog."));
    expect(checkS(parsed, "str6", "The quick brown fox jumps over the lazy dog."));
    expect(checkS(parsed, "str7", "Here are two quotation marks: \"\". Simple enough."));
    expect(checkS(parsed, "str8", "Here are three quotation marks: \"\"\"."));
    expect(checkS(parsed, "str9", "Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\"."));
    expect(checkS(parsed, "str10", "\"This,\" she said, \"is just a pointless statement.\""));
    expect(checkS(parsed, "winpath", "C:\\Users\\nodejs\\templates"));
    expect(checkS(parsed, "winpath2", "\\\\ServerX\\admin$\\system32\\"));
    expect(checkS(parsed, "quoted", "Tom \"Dubs\" Preston-Werner"));
    expect(checkS(parsed, "regex", "<\\i\\c*\\s*>"));
    expect(checkS(parsed, "regex2", "I [dw]on't need \\d{2} apples"));
    expect(checkS(parsed, "lines",
        \\The first newline is
        \\trimmed in raw strings.
        \\   All other whitespace
        \\   is preserved.
        \\
    ));
    expect(checkS(parsed, "quot15", "Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\""));
    expect(checkS(parsed, "apos15", "Here are fifteen apostrophes: '''''''''''''''"));
    expect(checkS(parsed, "str11", "'That's still pointless', she said."));
}

test "example9" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var parsed = try parser.parse(Value,
        \\int1 = +99
        \\int2 = 42
        \\int3 = 0
        \\int4 = -17
        \\int5 = 1_000
        \\int6 = 5_349_221
        \\int7 = 1_2_3_4_5     # VALID but discouraged 
        \\# hexadecimal with prefix `0x`
        \\hex1 = 0xDEADBEEF
        \\hex2 = 0xdeadbeef
        \\hex3 = 0xdead_beef
        \\
        \\# octal with prefix `0o`
        \\oct1 = 0o01234567
        \\oct2 = 0o755 # useful for Unix file permissions
        \\
        \\# binary with prefix `0b`
        \\bin1 = 0b11010110
    );
    defer parsed.deinit();
    expect(parsed.get("int1").?.Int == 99);
    expect(parsed.get("int2").?.Int == 42);
    expect(parsed.get("int3").?.Int == 0);
    expect(parsed.get("int4").?.Int == -17);
    expect(parsed.get("int5").?.Int == 1000);
    expect(parsed.get("int6").?.Int == 5349221);
    expect(parsed.get("int7").?.Int == 12345);
    expect(parsed.get("hex1").?.Int == 0xdeadbeef);
    expect(parsed.get("hex2").?.Int == 0xdeadbeef);
    expect(parsed.get("hex3").?.Int == 0xdeadbeef);
    expect(parsed.get("oct1").?.Int == 0o01234567);
    expect(parsed.get("oct2").?.Int == 0o755);
    expect(parsed.get("bin1").?.Int == 0b11010110);
}

test "examples10" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var parsed = try parser.parse(Value,
        \\integers = [ 1, 2, 3 ]
        \\colors = [ "red", "yellow", "green" ]
        \\nested_array_of_int = [ [ 1, 2 ], [3, 4, 5] ]
        \\nested_mixed_array = [ [ 1, 2 ], ["a", "b", "c"] ]
        \\string_array = [ "all", 'strings', """are the same""", '''type''' ]
        \\
        \\# Mixed-type arrays are allowed
        \\numbers = [ 0.1, 0.2, 0.5, 1, 2, 5 ]
        \\contributors = [
        \\  "Foo Bar <foo@example.com>",
        \\  { name = "Baz Qux", email = "bazqux@example.com", url = "https://example.com/bazqux" }
        \\]
        \\integers2 = [
        \\  1, 2, 3
        \\]
        \\
        \\integers3 = [
        \\  1,
        \\  2, # this is ok
        \\]
    );
    defer parsed.deinit();

    expect(parsed.get("integers").?.idx(0).?.Int == 1);
    expect(parsed.get("integers").?.Array.items[1].Int == 2);
    expect(parsed.get("integers").?.Array.items[2].Int == 3);
    expect(mem.eql(u8, parsed.get("colors").?.Array.items[0].String.items, "red"));
    expect(mem.eql(u8, parsed.get("colors").?.Array.items[1].String.items, "yellow"));
    expect(mem.eql(u8, parsed.get("colors").?.Array.items[2].String.items, "green"));
    expect(parsed.get("nested_array_of_int").?.Array.items[0].Array.items[0].Int == 1);
    expect(parsed.get("nested_array_of_int").?.Array.items[0].Array.items[1].Int == 2);
    expect(parsed.get("nested_array_of_int").?.Array.items[1].Array.items[0].Int == 3);
    expect(parsed.get("nested_array_of_int").?.Array.items[1].Array.items[1].Int == 4);
    expect(parsed.get("nested_array_of_int").?.Array.items[1].Array.items[2].Int == 5);
    expect(parsed.get("nested_mixed_array").?.Array.items[0].Array.items[0].Int == 1);
    expect(parsed.get("nested_mixed_array").?.Array.items[0].Array.items[1].Int == 2);
    expect(mem.eql(u8, parsed.get("nested_mixed_array").?.Array.items[1].Array.items[0].String.items, "a"));
    expect(mem.eql(u8, parsed.get("nested_mixed_array").?.Array.items[1].Array.items[1].String.items, "b"));
    expect(mem.eql(u8, parsed.get("nested_mixed_array").?.Array.items[1].Array.items[2].String.items, "c"));
    expect(mem.eql(u8, parsed.get("string_array").?.Array.items[0].String.items, "all"));
    expect(mem.eql(u8, parsed.get("string_array").?.Array.items[1].String.items, "strings"));
    expect(mem.eql(u8, parsed.get("string_array").?.Array.items[2].String.items, "are the same"));
    expect(mem.eql(u8, parsed.get("string_array").?.Array.items[3].String.items, "type"));
    expect(parsed.get("numbers").?.Array.items[0].Float == 0.1);
    expect(parsed.get("numbers").?.Array.items[1].Float == 0.2);
    expect(parsed.get("numbers").?.Array.items[2].Float == 0.5);
    expect(parsed.get("numbers").?.Array.items[3].Int == 1);
    expect(parsed.get("numbers").?.Array.items[4].Int == 2);
    expect(parsed.get("numbers").?.Array.items[5].Int == 5);
    expect(mem.eql(u8, parsed.get("contributors").?.Array.items[0].String.items, "Foo Bar <foo@example.com>"));
    expect(mem.eql(u8, parsed.get("contributors").?.Array.items[1].get("name").?.String.items, "Baz Qux"));
    expect(mem.eql(u8, parsed.get("contributors").?.Array.items[1].get("email").?.String.items, "bazqux@example.com"));
    expect(mem.eql(u8, parsed.get("contributors").?.Array.items[1].get("url").?.String.items, "https://example.com/bazqux"));
    expect(parsed.get("integers2").?.Array.items[0].Int == 1);
    expect(parsed.get("integers2").?.Array.items[1].Int == 2);
    expect(parsed.get("integers2").?.Array.items[2].Int == 3);
    expect(parsed.get("integers3").?.Array.items[0].Int == 1);
    expect(parsed.get("integers3").?.Array.items[1].Int == 2);
    expect(parsed.get("integers3").?.Array.items.len == 2);
}

test "example11" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var parsed = try parser.parse(Value,
        \\[table-1]
        \\key1 = "some string"
        \\key2 = 123
        \\
        \\[table-2]
        \\key1 = "another string"
        \\key2 = 456
        \\[dog."tater.man"]
        \\type.name = "pug"
        \\[fruit]
        \\apple.color = "red"
        \\apple.taste.sweet = true
        \\
        \\# [fruit.apple]  # INVALID
        \\# [fruit.apple.taste]  # INVALID
        \\
        \\[fruit.apple.texture]  # you can add sub-tables
        \\smooth = true
    );
    defer parsed.deinit();
    var tbl1 = parsed.get("table-1").?;
    expect(checkS(tbl1, "key1", "some string"));
    expect(tbl1.get("key2").?.Int == 123);
    var tbl2 = parsed.get("table-2").?;
    expect(checkS(tbl2, "key1", "another string"));
    expect(tbl2.get("key2").?.Int == 456);
    var taterman = parsed.get("dog").?.get("tater.man").?;
    expect(checkS(taterman.get("type").?, "name", "pug"));
    var apple = parsed.get("fruit").?.get("apple").?;
    expect(checkS(apple, "color", "red"));
    expect(apple.get("taste").?.get("sweet").?.Bool);
}

test "example12" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var parsed = try parser.parse(Value,
        \\name = { first = "Tom", last = "Preston-Werner" }
        \\point = { x = 1, y = 2 }
        \\animal = { type.name = "pug" }
    );
    defer parsed.deinit();
    expect(checkS(parsed.get("name").?, "first", "Tom"));
    expect(checkS(parsed.get("name").?, "last", "Preston-Werner"));
    expect(parsed.get("point").?.get("x").?.Int == 1);
    expect(parsed.get("point").?.get("y").?.Int == 2);
    expect(checkS(parsed.get("animal").?.get("type").?, "name", "pug"));
}

test "examples-invalid" {
    var tester = testing.LeakCountAllocator.init(std.heap.page_allocator);
    defer tester.validate() catch {};
    var allocator = &tester.allocator;
    var parser = try Parser.init(allocator);
    defer allocator.destroy(parser);

    var invalids = [_][]const u8 {
        "key = # INVALID",
        "first = \"Tom\" last = \"Preston-Werner\" # INVALID",
        "= \"no key name\"  # INVALID",
        \\# DO NOT DO THIS
        \\name = "Tom"
        \\name = "Pradyun"
        ,
        \\# This defines the value of fruit.apple to be an integer.
        \\fruit.apple = 1
        \\
        \\# But then this treats fruit.apple like it's a table.
        \\# You can't turn an integer into a table.
        \\fruit.apple.smooth = true
        ,
        \\[fruit]
        \\apple = "red"
        \\
        \\[fruit]
        \\orange = "orange"
        ,
        \\[fruit]
        \\apple = "red"
        \\
        \\[fruit.apple]
        \\texture = "smooth"
        ,
        \\[fruit]
        \\apple.color = "red"
        \\apple.taste.sweet = true
        \\[fruit.apple]  # INVALID
        \\test = true
        ,
        \\[fruit]
        \\apple.color = "red"
        \\apple.taste.sweet = true
        \\[fruit.apple.taste]  # INVALID
        \\test = true
        ,
        \\[product]
        \\type = { name = "Nail" }
        \\type.edible = false  # INVALID
        ,
        \\[product]
        \\type.name = "Nail"
        \\type = { edible = false }  # INVALID
        ,
        \\[fruit.physical]  # subtable, but to which parent element should it belong?
        \\  color = "red"
        \\  shape = "round"
        \\
        \\[[fruit]]
        \\  name = "apple"
        ,
        \\fruit = []
        \\
        \\[[fruit]] # Not allowed
        \\foo = "bar"
        ,
        \\[[fruit]]
        \\  name = "apple"
        \\
        \\  [[fruit.variety]]
        \\    name = "red delicious"
        \\
        \\  # INVALID: This table conflicts with the previous array of tables
        \\  [fruit.variety]
        \\    name = "granny smith"
        ,
        \\[[fruit]]
        \\  name = "apple"
        \\
        \\  [[fruit.variety]]
        \\    name = "red delicious"
        \\
        \\  [fruit.physical]
        \\    color = "red"
        \\    shape = "round"
        \\
        \\  # INVALID: This array of tables conflicts with the previous table
        \\  [[fruit.physical]]
        \\    color = "green"
    };
    for (invalids) |invalid| {
        if (parser.parse(Value, invalid)) |parsed| {
            std.debug.warn("Unexpectedly success of parsing for {} as {}\n", .{invalid, parsed});
            parsed.deinit();
            expect(false);
        } else |err| {}
    }
}