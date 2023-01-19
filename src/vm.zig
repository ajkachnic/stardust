const std = @import("std");

const _chunk = @import("chunk.zig");
const Compiler = @import("compiler.zig");
const common = @import("common.zig");
const _value = @import("value.zig");

const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;
const Value = _value.Value;
const Obj = _value.Obj;

const STACK_MAX = 256;

const log = std.log.scoped(.vm);

const InternMap = std.HashMap(*Obj.String, void, struct {
    pub fn hash(self: @This(), s: *Obj.String) u64 {
        _ = self;
        return @intCast(u64, s.hash);
    }

    pub fn eql(self: @This(), a: *Obj.String, b: *Obj.String) bool {
        _ = self;
        return std.mem.eql(u8, a.chars, b.chars);
    }
}, std.hash_map.default_max_load_percentage);

pub const Engine = struct {
    // compiler: Compiler,
    allocator: std.mem.Allocator,
    chunk: Chunk,
    ip: []u8,
    stack: [STACK_MAX]Value,
    // TODO: Switch to a slice once I figure out how go backwards with a slice
    stack_top: [*]Value,

    globals: std.StringHashMap(Value),
    strings: InternMap,
    objects: ?*Obj,

    compiler: Compiler,

    const InterpretError = error{
        CompileError,
        RuntimeError,
    };

    pub fn init(allocator: std.mem.Allocator) Engine {
        return Engine{
            .allocator = allocator,
            .chunk = undefined,
            .ip = undefined,
            .stack = [_]Value{undefined} ** 256,
            .stack_top = undefined,
            .globals = std.StringHashMap(Value).init(allocator),
            .strings = InternMap.init(allocator),
            .compiler = Compiler.init(allocator),
            .objects = null,
        };
    }

    pub fn resetStack(self: *Engine) void {
        self.stack_top = @ptrCast([*]Value, &self.stack);
    }

    fn runtimeError(self: *Engine, comptime format: []const u8, args: anytype) void {
        const instruction = @ptrToInt(self.ip.ptr) - @ptrToInt(self.chunk.code.items.ptr) - 1;
        const line = self.chunk.lines.items[instruction];

        log.err("[line {d}] " ++ format, .{line} ++ args);
    }

    /// Clean up all the left over memory
    pub fn deinit(self: *Engine) void {
        self.chunk.deinit();
        self.globals.deinit();
        self.strings.deinit();

        // Free all leftover objects
        var obj = self.objects;
        while (obj) |object| {
            var next = object.next;
            object.deinit(self.allocator);
            obj = next;
        }
    }

    pub fn interpret(self: *Engine, source: []const u8) InterpretError!void {
        self.resetStack();
        var chunk = Chunk.init(self.allocator);

        self.compiler.had_error = false; // Reset every time we run code

        try self.compiler.compile(source, &chunk);

        self.chunk = chunk;
        self.ip = chunk.code.items;

        return self.run();
    }

    // Push a value to the top of the stack
    fn push(self: *Engine, value: Value) void {
        // Check for stack overflow
        var overhead = (@ptrToInt(self.stack_top) - @ptrToInt(&self.stack)) / @sizeOf(Value);

        if (overhead >= STACK_MAX) std.debug.panic("compile error: stack overflow", .{});

        self.stack_top[0] = value;
        self.stack_top += 1;
    }

    fn pop(self: *Engine) Value {
        // The line is the only thing preventing us from making the stack_top
        // a slice, which would be a win for safety with little runtime cost
        self.stack_top -= 1;
        return self.stack_top[0];
    }

    fn peek(self: *Engine, distance: usize) Value {
        return (self.stack_top - 1 - distance)[0];
    }

    /// The main interpreter loop for stardust. *Currently implemented as a switch statement*.
    ///
    /// There are many other options out there to explore, like [computed goto](https://eli.thegreenplace.net/2012/07/12/computed-goto-for-efficient-dispatch-tables), [dispatch tables](https://en.wikipedia.org/wiki/Dispatch_table), and [tail calls](https://blog.reverberate.org/2021/04/21/musttail-efficient-interpreters.html)
    ///
    /// However, these options are much more complex and harder to read when
    /// compared to the switch statement. The most viable option at the moment
    /// appears to be tailcall support, but it's not wise to switch to them
    /// without proper benchmarking
    pub fn run(self: *Engine) InterpretError!void {
        while (true) {
            if (common.debug_trace_execution) {
                std.debug.print("          ", .{});
                var slot: [*]Value = &self.stack;
                while (@ptrToInt(slot) < @ptrToInt(self.stack_top)) : (slot += 1) {
                    std.debug.print("[ {any} ] ", .{slot[0]});
                }
                std.debug.print("\n", .{});
            }
            const instruction = self.readByte();

            switch (@intToEnum(OpCode, instruction)) {
                OpCode.@"return" => {
                    std.debug.print("{any}\n", .{self.pop()});
                    return;
                },

                // Jumping instructions
                OpCode.jump_if => {
                    var offset = self.readShort();
                    var condition = self.pop();

                    if (condition.isFalsey()) {
                        self.ip = self.ip[offset..];
                    }
                },
                OpCode.jump => {
                    var offset = self.readShort();
                    self.ip = self.ip[offset..];
                },
                OpCode.@"and" => {
                    var offset = self.readShort();
                    var condition = self.peek(0);

                    if (condition.isFalsey()) {
                        // Short-circuit rhs
                        self.ip = self.ip[offset..];
                    } else {
                        _ = self.pop(); // Discord condition
                    }
                },
                OpCode.@"or" => {
                    var offset = self.readShort();
                    var condition = self.peek(0);

                    if (condition.isFalsey()) {
                        _ = self.pop(); // Discord condition
                    } else {
                        // Short-circuit rhs
                        self.ip = self.ip[offset..];
                    }
                },

                OpCode.pop => _ = self.pop(),
                OpCode.get_local => {
                    var slot = self.readByte();
                    self.push(self.stack[slot]);
                },
                OpCode.set_local => {
                    var slot = self.readByte();
                    self.stack[slot] = self.peek(0);
                },
                OpCode.get_global => {
                    var name = self.readString();
                    if (self.globals.get(name.chars)) |global| {
                        self.push(global);
                    } else {
                        self.runtimeError("Undefined variables {s}", .{name.chars});
                        return error.RuntimeError;
                    }
                },
                OpCode.set_global => {
                    var name = self.readString();
                    if (self.globals.contains(name.chars)) {
                        self.globals.put(name.chars, self.peek(0)) catch common.oom();
                    } else {
                        self.runtimeError("Undefined variables {s}", .{name.chars});
                        return error.RuntimeError;
                    }
                },
                OpCode.define_global => {
                    var name = self.readString();
                    self.globals.put(name.chars, self.peek(0)) catch common.oom();
                    _ = self.pop();
                },
                OpCode.constant => {
                    var constant = self.readConstant();
                    self.push(constant);
                },
                OpCode.null => self.push(Value.null),
                OpCode.true => self.push(Value{ .bool = true }),
                OpCode.false => self.push(Value{ .bool = false }),
                OpCode.equal => {
                    var b = self.pop();
                    var a = self.pop();
                    self.push(Value{ .bool = Value.equals(a, b) });
                },
                // TODO: Eliminate repeated code for binary operators
                OpCode.greater => {
                    if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return InterpretError.RuntimeError;
                    }
                    var b = self.pop().number;
                    var a = self.pop().number;
                    self.push(Value{ .bool = a > b });
                },
                OpCode.greater_equal => {
                    if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return InterpretError.RuntimeError;
                    }
                    var b = self.pop().number;
                    var a = self.pop().number;
                    self.push(Value{ .bool = a >= b });
                },
                OpCode.less => {
                    if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return InterpretError.RuntimeError;
                    }
                    var b = self.pop().number;
                    var a = self.pop().number;
                    self.push(Value{ .bool = a < b });
                },
                OpCode.less_equal => {
                    if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return InterpretError.RuntimeError;
                    }
                    var b = self.pop().number;
                    var a = self.pop().number;
                    self.push(Value{ .bool = a <= b });
                },
                // binary operators
                OpCode.add => {
                    if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return InterpretError.RuntimeError;
                    }
                    var b = self.pop().number;
                    var a = self.pop().number;
                    self.push(Value{ .number = a + b });
                },
                OpCode.subtract => {
                    if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return InterpretError.RuntimeError;
                    }
                    var b = self.pop().number;
                    var a = self.pop().number;
                    self.push(Value{ .number = a - b });
                },
                OpCode.multiply => {
                    if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return InterpretError.RuntimeError;
                    }
                    var b = self.pop().number;
                    var a = self.pop().number;
                    self.push(Value{ .number = a * b });
                },
                OpCode.divide => {
                    if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return InterpretError.RuntimeError;
                    }
                    var b = self.pop().number;
                    var a = self.pop().number;
                    self.push(Value{ .number = a / b });
                },
                OpCode.concat => {
                    if (!self.peek(0).isString() or !self.peek(1).isString()) {
                        self.runtimeError("Operands must be strings.", .{});
                        return InterpretError.RuntimeError;
                    }
                    var b = self.pop().obj.asString();
                    var a = self.pop().obj.asString();

                    var concated = std.mem.concat(self.allocator, u8, &.{ a.chars, b.chars }) catch common.oom();
                    var str = Obj.String.init(self, concated);
                    self.push(Value{ .obj = &str.obj });
                },
                OpCode.not => self.push(Value{ .bool = self.pop().isFalsey() }),
                OpCode.negate => {
                    if (!self.peek(0).isNumber()) {
                        self.runtimeError("Operand must be a number.", .{});
                        return InterpretError.RuntimeError;
                    }
                    self.push(Value{ .number = -(self.pop().number) });
                },
            }
        }
    }

    inline fn readConstant(self: *Engine) Value {
        return self.chunk.constants.items[self.readByte()];
    }

    inline fn readString(self: *Engine) *Obj.String {
        return self.readConstant().obj.asString();
    }

    inline fn readByte(self: *Engine) u8 {
        const ip = self.ip;
        self.ip = self.ip[1..];
        return ip[0];
    }

    inline fn readShort(self: *Engine) u16 {
        const ip = self.ip;
        self.ip = self.ip[2..];

        return @intCast(u16, ip[0]) << 8 | @intCast(u16, ip[1]);
    }
};
