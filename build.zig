const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const debug_trace_execution = b.option(bool, "debug_trace_execution", "Enable tracing execution for debugging purposes") orelse false;
    const debug_print_code = b.option(bool, "debug_print_code", "Print generated bytecode (for compiler debugging") orelse false;

    const exe = b.addExecutable("stardust-zig", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const build_options = b.addOptions();
    build_options.addOption(bool, "debug_trace_execution", debug_trace_execution);
    build_options.addOption(bool, "debug_print_code", debug_print_code);

    exe.addOptions("build_options", build_options);

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_tests = b.addTest("src/main.zig");
    exe_tests.setTarget(target);
    exe_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&exe_tests.step);
}
