const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    var constant = chunk.code.items[offset + 1];
    std.debug.print(
        "{s: <16} {d: >4} {any}\n",
        .{ name, constant, chunk.constants.items[constant] },
    );
    return offset + 2;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    std.debug.print("{d:0>4}", .{offset});

    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d: >4} ", .{chunk.lines.items[offset]});
    }

    return switch (@intToEnum(OpCode, chunk.code.items[offset])) {
        OpCode.constant => constantInstruction("OP_CONSTANT", chunk, offset),
        OpCode.null => simpleInstruction("OP_NIL", offset),
        OpCode.true => simpleInstruction("OP_TRUE", offset),
        OpCode.false => simpleInstruction("OP_FALSE", offset),
        OpCode.equal => simpleInstruction("OP_EQUAL", offset),
        OpCode.greater => simpleInstruction("OP_GREATER", offset),
        OpCode.greater_equal => simpleInstruction("OP_GREATER_EQ", offset),
        OpCode.less => simpleInstruction("OP_LESS", offset),
        OpCode.less_equal => simpleInstruction("OP_LESS_EQ", offset),
        OpCode.@"return" => simpleInstruction("OP_RETURN", offset),
        OpCode.add => simpleInstruction("OP_ADD", offset),
        OpCode.subtract => simpleInstruction("OP_SUBTRACT", offset),
        OpCode.multiply => simpleInstruction("OP_MULTIPLY", offset),
        OpCode.divide => simpleInstruction("OP_DIVIDE", offset),
        OpCode.concat => simpleInstruction("OP_CONCAT", offset),
        OpCode.not => simpleInstruction("OP_NOT", offset),
        OpCode.negate => simpleInstruction("OP_NEGATE", offset),
        // else => blk: {
        //     std.debug.print("unknown opcode {d}\n", .{chunk.code.items[offset]});
        //     break :blk offset + 1;
        // },
    };

    // var offset: usize = 0;
    // while (offset < chunk.code.items.len) {
    //     offset = disassembleInstruction(chunk, offset);
    // }
}