const std = @import("std");
const testing = std.testing;
const expect = testing.expect;

pub const Node = struct {
};

pub const Parser = struct {
    allocator: *std.mem.Allocator,

    pub fn init(allocator: *std.mem.Allocator) !*Parser {
        var parser = try allocator.create(Parser);
        parser.allocator = allocator;
        return parser;
    }

    pub fn parse(self: *Parser, input: []u8) !std.StringHashMap(*Node) {
        return std.StringHashMap(*Node).init(self.allocator);
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