// const hash = @import("hash.zig");
const std = @import("std");

pub const TokenType = enum {
    leftParen,
    rightParen,
    leftBrace,
    rightBrace,

    semicolon,
    comma,
    minus,
    plus,
    slash,
    star,
    // One or two character tokens.
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,
    dot,
    dot_dot,
    // Literals.
    identifier,
    string,
    number,
    // Keywords.
    @"and",
    class,
    @"const",
    @"else",
    false,
    @"for",
    fun,
    @"if",
    let,
    null,
    @"or",
    print,
    @"return",
    //   TOKEN_SUPER,
    //   TOKEN_THIS,
    true,
    @"while",

    @"error",
    eof,
};

pub const Token = struct {
    t: TokenType,
    data: []const u8,
    line: u32,
};

const ident_map = std.ComptimeStringMap(TokenType, .{
    .{ "and", TokenType.@"and" },
    .{ "class", TokenType.class },
    .{ "const", TokenType.@"const" },
    .{ "else", TokenType.@"else" },
    .{ "false", TokenType.false },
    .{ "for", TokenType.@"for" },
    .{ "fun", TokenType.fun },
    .{ "if", TokenType.@"if" },
    .{ "let", TokenType.let },
    .{ "null", TokenType.null },
    .{ "or", TokenType.@"or" },
    .{ "print", TokenType.print },
    .{ "return", TokenType.@"return" },
    .{ "true", TokenType.@"true" },
    .{ "while", TokenType.@"while" },
});

// const idents = hash.perfectHash(&.{
//     "and",
//     "class",
//     "const",
//     "else",
//     "false",
//     "for",
//     "fun",
//     "if",
//     "let",
//     "null",
//     "or",
//     "print",
//     "return",
//     "true",
//     "while",
// });

pub const Scanner = struct {
    start: []const u8,
    current: []const u8,
    line: u32,

    pub fn init(source: []const u8) Scanner {
        return Scanner{
            .start = source,
            .current = source,
            .line = 1,
        };
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();
        self.start = self.current;

        if (self.isAtEnd()) return self.makeToken(TokenType.eof);

        var ch = self.advance();

        switch (ch) {
            '(' => return self.makeToken(TokenType.leftParen),
            ')' => return self.makeToken(TokenType.rightParen),
            '{' => return self.makeToken(TokenType.leftBrace),
            '}' => return self.makeToken(TokenType.rightBrace),

            ';' => return self.makeToken(TokenType.semicolon),
            ',' => return self.makeToken(TokenType.comma),
            '-' => return self.makeToken(TokenType.minus),
            '+' => return self.makeToken(TokenType.plus),
            '/' => return self.makeToken(TokenType.slash),
            '*' => return self.makeToken(TokenType.star),
            // One or two character tokens
            '!' => return self.makeToken(if (self.match('=')) .bang_equal else .bang),
            '=' => return self.makeToken(if (self.match('=')) .equal_equal else .equal),
            '>' => return self.makeToken(if (self.match('=')) .greater_equal else .greater),
            '<' => return self.makeToken(if (self.match('=')) .less_equal else .equal),
            '.' => return self.makeToken(if (self.match('.')) .dot_dot else .dot),

            '"' => return self.string(),
            // return self.makeToken(if (self.match('=')) .equal_equal else .equal)
            else => {
                if (isAlpha(ch)) return self.identifier();
                if (isDigit(ch)) return self.number();

                return self.errorToken("Unexpected character.");
            },
        }
    }

    // Utility methods
    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isAtEnd(self: Scanner) bool {
        return self.current.len == 0;
    }
    fn advance(self: *Scanner) u8 {
        var current = self.current;
        self.current = self.current[1..];
        return current[0];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.current[0] != expected) return false;

        self.current = self.current[1..];
        return true;
    }

    inline fn peek(self: *Scanner) u8 {
        return self.current[0];
    }

    fn peekNext(self: *Scanner) ?u8 {
        return if (self.isAtEnd()) null else self.current[1];
    }

    fn makeToken(self: *Scanner, t: TokenType) Token {
        return Token{
            .t = t,
            .data = self.start[0 .. self.start.len - self.current.len],
            .line = self.line,
        };
    }

    fn errorToken(self: Scanner, message: []const u8) Token {
        return Token{
            .t = TokenType.@"error",
            .data = message,
            .line = self.line,
        };
    }

    fn skipWhitespace(self: *Scanner) void {
        while (true) {
            if (self.isAtEnd()) return;
            var ch = self.peek();
            switch (ch) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == @intCast(u8, '/')) {
                        while (!self.isAtEnd() and self.peek() != '\n') {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    // Lexer methods
    fn identifier(self: *Scanner) Token {
        while (!self.isAtEnd() and (isAlpha(self.peek()) or isDigit(self.peek()))) {
            _ = self.advance();
        }

        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Scanner) TokenType {
        return ident_map.get(
            self.start[0 .. self.start.len - self.current.len],
        ) orelse TokenType.identifier;
    }

    fn number(self: *Scanner) Token {
        while (!self.isAtEnd() and isDigit(self.peek()))
            _ = self.advance();

        // Look for a fractional part.
        if (!self.isAtEnd() and self.peek() == '.' and isDigit(self.peekNext() orelse 0)) {
            // Consume the ".".
            _ = self.advance();

            while (!self.isAtEnd() and isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        return self.makeToken(TokenType.number);
    }

    fn string(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n')
                self.line += 1;
            _ = self.advance();
        }

        if (self.isAtEnd())
            return self.errorToken("Unterminated string.");

        // The closing quote.
        _ = self.advance();
        return self.makeToken(TokenType.string);
    }
};
