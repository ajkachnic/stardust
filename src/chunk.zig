const std = @import("std");
const common = @import("common.zig");

const Value = @import("value.zig").Value;

pub const OpCode = enum {
    constant,
    pop,
    get_global,
    set_global,
    define_global,
    get_local,
    set_local,
    /// Push null onto the stack.
    null,

    /// Push true onto the stack.
    true,

    /// Push false onto the stack.
    false,
    equal,
    greater,
    greater_equal,
    less,
    less_equal,
    add,
    subtract,
    multiply,
    divide,
    not,
    negate,
    concat,
    /// Jump the instruction pointer [arg] forward.
    jump,
    /// Pop and if not truthy then jump the instruction pointer [arg] forward.
    jump_if,
    /// If the top of the stack is false, jump [arg] forward. Otherwise pop and continue.
    @"and",
    /// If the top of the stack is non-false, jump [arg] forward. Otherwise pop and continue.
    @"or",

    @"return",
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    lines: std.ArrayList(u32), // TODO: Use RLE
    constants: std.ArrayList(Value),

    pub fn init(alloc: std.mem.Allocator) Chunk {
        return Chunk{
            .code = std.ArrayList(u8).init(alloc),
            .lines = std.ArrayList(u32).init(alloc),
            .constants = std.ArrayList(Value).init(alloc),
        };
    }

    pub fn deinit(self: Chunk) void {
        self.code.deinit();
        self.lines.deinit();
    }

    pub fn write(self: *Chunk, byte: u8, line: u32) void {
        self.code.append(byte) catch common.oom();
        self.lines.append(line) catch common.oom();
    }

    pub fn addConstant(self: *Chunk, value: Value) usize {
        self.constants.append(value) catch common.oom();
        return self.constants.items.len - 1;
    }
};
