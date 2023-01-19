const std = @import("std");

const vm = @import("vm.zig");
const chunk = @import("chunk.zig");
const debug = @import("debug.zig");

fn color(comptime str: []const u8, comptime clr: []const u8) []const u8 {
    return "\x1b[" ++ clr ++ "m" ++ str ++ "\x1b[0m";
}

// Define root.log to override the std implementation
pub fn log(comptime level: std.log.Level, comptime scope: @TypeOf(.EnumLiteral), comptime format: []const u8, args: anytype) void {
    // ignore all non-error logging from other sources
    const scope_prefix = "(" ++ switch (scope) {
        .default, .vm => @tagName(scope),
        else => if (@enumToInt(level) <= @enumToInt(std.log.Level.err))
            @tagName(scope)
        else
            return,
    } ++ "): ";

    const clr = switch (level) {
        .err => "31",
        .info => "96",
        .warn => "33",
        .debug => "94",
    };

    // const prefix = std.fmt.comptimePrint("[\x1b[{s}m{s}\x1b[0m]{s}", .{ comptime clr, comptime level.asText(), scope_prefix });
    const prefix = "[\x1b[" ++ clr ++ "m" ++ comptime level.asText() ++ "\x1b[0m]" ++ scope_prefix;
    // @compileLog("hello " ++ prefix);
    // break :blk "[" ++ std.fmt.comptimePrint("\x1b[{d}m{s}\x1b[0m", .{ c, comptime level.asText() }) ++ "]" ++ scope_prefix;
    // } else {
    // break :blk "[" ++ comptime level.asText() ++ "]" ++ scope_prefix;
    // }

    std.debug.getStderrMutex().lock();
    defer std.debug.getStderrMutex().unlock();
    const stderr = std.io.getStdErr().writer();
    nosuspend stderr.print(prefix ++ format ++ "\n", args) catch return;
}

fn repl(alloc: std.mem.Allocator) !void {
    const repl_log = std.log.scoped(.repl);
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    defer stdin.close();
    defer stdout.close();

    var reader = stdin.reader();
    var writer = stdout.writer();

    var engine = vm.Engine.init(gpa.allocator());
    defer engine.deinit();

    while (true) {
        _ = try writer.write("> ");
        var line = try reader.readUntilDelimiterAlloc(alloc, '\n', 4096);

        engine.interpret(line) catch |e| {
            repl_log.err("Engine error {}, continuing for repl", .{e});
        };
    }
}

fn runFile(alloc: std.mem.Allocator, path: []u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(alloc, 4096 * 4096);
    defer alloc.free(source);

    var engine = vm.Engine.init(gpa.allocator());
    defer engine.deinit();

    engine.interpret(source) catch {
        std.log.scoped(.cli).err("Fatal error, exiting program", .{});
        std.os.exit(1);
    };
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var args = try std.process.argsWithAllocator(arena.allocator());

    _ = args.skip(); // Skip executable name

    if (args.next()) |arg| {
        const file_path = try std.fs.path.resolve(arena.allocator(), &[_][]const u8{arg});

        try runFile(arena.allocator(), file_path);
    } else {
        try repl(arena.allocator());
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
