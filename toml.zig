const std = @import("std");
const testing = std.testing;
const expect = testing.expect;

pub const Node = union {
    String: []const u8
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
        var i: usize = offset;
        while (i < input.len and (input[i] == ' ' or input[i] == '\t')) : (i+=1) {}
        return i;
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
        var value_result = try self.parseToken(input, offset+1);
        _ = try data.put(key_result.token, .{.str = value_result.token});
        offset = self.skipSpaces(input, value_result.offset);
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
    var parsed = try parser.parse(" \t foo\t=  bar   \n");
    defer parsed.deinit();
    expect(parsed.size == 1);
    expect(std.mem.eql(u8, parsed.get("foo").?.value.str, "bar"));
}