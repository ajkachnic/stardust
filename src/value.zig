const builtin = @import("builtin");
const std = @import("std");

const common = @import("common.zig");
const vm = @import("vm.zig");

pub const Value = union(Type) {
    pub const Type = enum { bool, null, number, obj };

    bool: bool,
    number: f64,
    null: void,
    obj: *Obj,

    pub fn isBool(self: Value) bool {
        return @as(Type, self) == Type.bool;
    }

    pub fn isNumber(self: Value) bool {
        return @as(Type, self) == Type.number;
    }

    pub fn isNil(self: Value) bool {
        return @as(Type, self) == Type.nil;
    }

    pub fn isString(self: Value) bool {
        return @as(Type, self) == Type.obj and self.obj.t == .string;
    }

    // null and false are falsey
    pub fn isFalsey(self: Value) bool {
        return self == .null or (self == .bool and self.bool == false);
    }

    pub fn equals(a: Value, b: Value) bool {
        if (@as(Type, a) != @as(Type, b)) return false;

        switch (a) {
            .bool => return a.bool == b.bool,
            .null => return true,
            .number => return a.number == b.number,
            .obj => return Obj.equals(a.obj, b.obj),
        }
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            Type.bool => |b| try writer.print("{}", .{b}),
            Type.number => |n| try writer.print("{}", .{n}),
            Type.null => try writer.print("<null>", .{}),
            Type.obj => |o| try o.format("", .{}, writer),
        }
    }
};

pub const Obj = struct {
    pub const Type = enum {
        string,
    };

    pub const String = struct {
        obj: Obj,
        chars: []const u8,
        hash: u32,

        /// Heap allocates a new string object and copies a source string with it.
        ///
        /// Caller owns memory
        pub fn initCopy(engine: *vm.Engine, chars: []const u8) *String {
            var hash = std.hash.Fnv1a_32.hash(chars);
            var obj = Obj{ .t = .string, .next = engine.objects };

            if (engine.strings.getKey(&String{ .chars = chars, .hash = hash, .obj = obj })) |str| {
                std.log.debug("already interned", .{});
                // The string has already been interned
                return str;
            }

            var self = engine.allocator.create(String) catch common.oom();
            var copy = engine.allocator.dupe(u8, chars) catch common.oom();

            self.obj = Obj{ .t = .string, .next = engine.objects };
            self.chars = copy;
            self.hash = hash;

            engine.objects = &self.obj;

            engine.strings.put(self, {}) catch common.oom();

            return self;
        }

        pub fn init(engine: *vm.Engine, chars: []const u8) *String {
            var hash = std.hash.Fnv1a_32.hash(chars);
            var obj = Obj{ .t = .string, .next = engine.objects };

            if (engine.strings.getKey(&String{ .chars = chars, .hash = hash, .obj = obj })) |str| {
                // The string has already been interned
                return str;
            }

            var self = engine.allocator.create(String) catch common.oom();

            self.obj = Obj{ .t = .string, .next = engine.objects };
            self.chars = chars;
            self.hash = hash;

            engine.objects = &self.obj;

            engine.strings.put(self, {}) catch common.oom();

            return self;
        }
    };

    t: Type,
    next: ?*Obj,

    pub fn deinit(self: *Obj, allocator: std.mem.Allocator) void {
        switch (self.t) {
            .string => {
                var string = self.asString();
                allocator.free(string.chars);
                allocator.destroy(string);
            },
        }
    }

    /// Cast a given object to a string object. Safety checked in debug mode
    pub fn asString(self: *Obj) *String {
        if (builtin.mode == .Debug and self.t != .string) {
            std.debug.panic("invalid cast of obj type {} to string", .{self.t});
        }
        return @fieldParentPtr(String, "obj", self);
    }

    pub fn equals(a: *Obj, b: *Obj) bool {
        if (a.t != b.t) return false;

        switch (a.t) {
            .string => return std.mem.eql(u8, a.asString().chars, b.asString().chars),
            // else => return false,
        }
    }

    pub fn format(
        self: *Obj,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self.t) {
            .string => {
                var str = Obj.asString(self);
                try writer.print("\"{s}\"", .{str.chars});
            },
            // else => {},
        }
    }
};
