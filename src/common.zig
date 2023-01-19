const std = @import("std");
const build_options = @import("build_options");

pub const debug_trace_execution = build_options.debug_trace_execution;
pub const debug_print_code = build_options.debug_print_code;

pub const max_locals = 256;

pub fn oom() noreturn {
    @panic("out of memory");
}
