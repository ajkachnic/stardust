const std = @import("std");

const common = @import("common.zig");
const debug = @import("debug.zig");
const _chunk = @import("chunk.zig");
const _scanner = @import("scanner.zig");
const _value = @import("value.zig");
const vm = @import("vm.zig");

const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;
const Scanner = _scanner.Scanner;
const TokenType = _scanner.TokenType;
const Token = _scanner.Token;
const Obj = _value.Obj;
const Value = _value.Value;

const Self = @This();

alloc: std.mem.Allocator,
scanner: Scanner,
current: Token,
previous: Token,
compiling_chunk: *Chunk,

had_error: bool = false,
panic_mode: bool = false,

const Precedence = enum(u8) {
    none = 0,
    assignment, // =
    @"or", // or
    @"and", // and
    equality, // == !=
    comparison, // < > <= >=
    term, // + -
    factor, // * /
    unary, // ! -
    call, // . ()
    primary,

    pub fn increment(self: Precedence) Precedence {
        var i = @enumToInt(self);
        if (i < @enumToInt(Precedence.primary)) {
            return @intToEnum(Precedence, i + 1);
        }
        return self;
    }
};

const ParseFn = *const fn (self: *Self) void;

const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = Precedence.none,

    pub fn default() ParseRule {
        return .{ .prefix = null, .infix = null, .precedence = Precedence.none };
    }

    pub fn init(comptime prefix: ?ParseFn, comptime infix: ?ParseFn, comptime precedence: Precedence) ParseRule {
        return .{ .prefix = prefix, .infix = infix, .precedence = precedence };
    }
};

pub fn init(alloc: std.mem.Allocator) Self {
    return Self{
        .alloc = alloc,
        // We leave scanner undefined as we need source code to initialzie it
        .scanner = undefined,
        .current = undefined,
        .previous = undefined,
        .compiling_chunk = undefined,
    };
}

pub fn compile(self: *Self, source: []const u8, chunk: *Chunk) !void {
    self.scanner = Scanner.init(source);
    self.compiling_chunk = chunk;

    self.advance();

    while (!self.match(TokenType.eof)) {
        self.declaration();
    }
    // self.expression();
    // self.consume(TokenType.eof, "Expect end of expression.");

    self.endCompiler();
}

fn advance(self: *Self) void {
    self.previous = self.current;

    while (true) {
        self.current = self.scanner.scanToken();
        if (self.current.t != TokenType.@"error") break;

        self.errorAtCurrent(self.current.data);
    }
}

fn consume(self: *Self, t: TokenType, message: []const u8) void {
    if (self.current.t == t) {
        self.advance();
    } else {
        self.errorAtCurrent(message);
    }
}

fn match(self: *Self, t: TokenType) bool {
    if (self.check(t)) {
        self.advance();
        return true;
    } else {
        return false;
    }
}

inline fn check(self: *Self, t: TokenType) bool {
    return self.current.t == t;
}

fn emitByte(self: *Self, byte: u8) void {
    self.compiling_chunk.write(byte, self.previous.line);
}

fn emitOp(self: *Self, op: OpCode) void {
    self.compiling_chunk.write(@enumToInt(op), self.previous.line);
}

fn emitBytes(self: *Self, bytes: []const u8) void {
    for (bytes) |byte| {
        self.emitByte(byte);
    }
}

fn emitReturn(self: *Self) void {
    self.emitOp(OpCode.@"return");
}

fn makeConstant(self: *Self, value: Value) u8 {
    var constant = self.compiling_chunk.addConstant(value);
    if (constant > 256) {
        self.err("Too many constant in one chunk.");
        return 0;
    }

    return @intCast(u8, constant);
}

fn emitConstant(self: *Self, value: Value) void {
    self.emitBytes(&[_]u8{ @enumToInt(OpCode.@"constant"), self.makeConstant(value) });
}

fn endCompiler(self: *Self) void {
    self.emitReturn();
    if (common.debug_print_code) {
        debug.disassembleChunk(self.compiling_chunk, "code");
    }
}

// Actual parsing/compiling
fn binary(self: *Self) void {
    var operatorType = self.previous.t;

    var rule = rules.get(operatorType);
    self.parsePrecedence(rule.precedence.increment()); // Compile the operand

    switch (operatorType) {
        TokenType.bang_equal => self.emitBytes(&[_]u8{ @enumToInt(OpCode.equal), @enumToInt(OpCode.not) }),
        TokenType.equal_equal => self.emitOp(OpCode.equal),
        TokenType.greater => self.emitOp(OpCode.greater),
        TokenType.greater_equal => self.emitOp(OpCode.greater_equal),
        TokenType.less => self.emitOp(OpCode.less),
        TokenType.less_equal => self.emitOp(OpCode.less_equal),
        TokenType.plus => self.emitOp(OpCode.add),
        TokenType.minus => self.emitOp(OpCode.subtract),
        TokenType.star => self.emitOp(OpCode.multiply),
        TokenType.slash => self.emitOp(OpCode.divide),
        TokenType.dot_dot => self.emitOp(OpCode.concat),

        else => unreachable,
    }
}

fn literal(self: *Self) void {
    switch (self.previous.t) {
        TokenType.false => self.emitOp(OpCode.false),
        TokenType.null => self.emitOp(OpCode.null),
        TokenType.true => self.emitOp(OpCode.true),
        else => unreachable,
    }
}

fn grouping(self: *Self) void {
    self.expression();
    self.consume(.rightParen, "Expected ')' after expression.");
}

fn number(self: *Self) void {
    // a value which doesn't parse as a valid float is a scanner bug
    var value = std.fmt.parseFloat(f64, self.previous.data) catch {
        self.err("Number failed to parse with std.fmt.parseFloat(), likely a scanner bug.");
        return;
    };
    self.emitConstant(Value{ .number = value });
}

fn string(self: *Self) void {
    // Take advantage of the fact that the engine always stores the compiler
    // WARN: if this assumption is no longer true, this will break things
    var engine = @fieldParentPtr(vm.Engine, "compiler", self);

    var str = Obj.String.initCopy(engine, self.previous.data[1 .. self.previous.data.len - 1]);
    self.emitConstant(
        Value{ .obj = &str.obj },
    );
}

fn unary(self: *Self) void {
    var operatorType = self.previous.t;

    self.parsePrecedence(Precedence.unary); // Compile the operand

    switch (operatorType) {
        TokenType.bang => self.emitOp(OpCode.not),
        TokenType.minus => self.emitOp(OpCode.negate),
        else => unreachable,
    }
}

// const rules = std.EnumArray(TokenType, ParseRule).initFill(ParseRule.default());

const rules = std.EnumArray(TokenType, ParseRule).initDefault(ParseRule.default(), .{
    .leftParen = ParseRule.init(&grouping, null, .none),

    .minus = ParseRule.init(&unary, &binary, .term),
    .plus = ParseRule.init(null, &binary, .term),
    .dot_dot = ParseRule.init(null, &binary, .term),

    .slash = ParseRule.init(null, &binary, .factor),
    .star = ParseRule.init(null, &binary, .factor),
    .bang = ParseRule.init(&unary, null, .none),
    .bang_equal = ParseRule.init(null, &binary, .equality),

    .equal_equal = ParseRule.init(null, &binary, .equality),
    .greater = ParseRule.init(null, &binary, .comparison),
    .greater_equal = ParseRule.init(null, &binary, .comparison),
    .less = ParseRule.init(null, &binary, .comparison),
    .less_equal = ParseRule.init(null, &binary, .comparison),

    .false = ParseRule.init(&literal, null, .none),
    .true = ParseRule.init(&literal, null, .none),
    .null = ParseRule.init(&literal, null, .none),

    .string = ParseRule.init(&string, null, .none),
    .number = ParseRule.init(&number, null, .none),
});

fn parsePrecedence(self: *Self, precedence: Precedence) void {
    self.advance();

    var prefixRule: ?ParseFn = rules.get(self.previous.t).prefix;

    if (prefixRule) |rule| {
        rule(self);

        while (@enumToInt(precedence) <= @enumToInt(rules.get(self.current.t).precedence)) {
            self.advance();
            var infixRule: ?ParseFn = rules.get(self.previous.t).infix;

            infixRule.?(self);
        }
    } else {
        self.err("Expect expression.");
        return;
    }
}
fn expression(self: *Self) void {
    self.parsePrecedence(Precedence.assignment);
}

fn declaration(self: *Self) void {
    self.statement();
}

fn statement(self: *Self) void {
    self.expressionStatement();
    // if (self.match(TokenType.print)) {
    //     self.printStatement();
    // }
}

fn expressionStatement(self: *Self) void {
    self.expression();
    self.consume(TokenType.semicolon, "Expected ';' after expression.");
    self.emitOp(OpCode.pop);
}

// Error reporting functions
fn errorAtCurrent(self: *Self, message: []const u8) void {
    return self.errorAt(self.current, message);
}

fn err(self: *Self, message: []const u8) void {
    return self.errorAt(self.previous, message);
}

fn errorAt(self: *Self, token: Token, message: []const u8) void {
    if (self.panic_mode) return;
    self.panic_mode = true;
    std.debug.print("[line {d}] Error", .{token.line});

    if (token.t == .eof) {
        std.debug.print("at end", .{});
    } else if (token.t == .@"error") {
        // nothing
    } else {
        std.debug.print("at {s}", .{token.data});
    }

    std.debug.print(": {s}\n", .{message});
}
