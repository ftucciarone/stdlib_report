# stdlib_report

Error reports for Fortran libraries built on [fortran-lang stdlib](https://github.com/fortran-lang/stdlib):
where a report comes from (the call graph) and where it goes (the user's logger).
Used by stdlib_NetCDF and stdlib_Integrate, so one call configures both.

## Where reports go: sinks

A library procedure that fails with no error argument to return the failure in
can only stop the program; before it does, it sends its report to the active
sink. stdlib's `stdlib_logger` writes only to Fortran units and has no hook for
other loggers, so this package adds one:

```fortran
use stdlib_report
call set_report_sink(stdlib_logger_sink(my_logger))   ! a stdlib logger_type, declared with `target`
call set_report_sink(stderr_sink())                   ! plain text on the error unit
call set_report_sink(procedure_sink(my_routine))      ! any subroutine (level, message, module, procedure)
call set_report_sink(my_sink)                         ! your own type, extends(report_sink)
```

With no sink registered, reports go to stdlib's `global_logger` if it has a log
file or unit, and are dropped otherwise (the stop message on stderr carries them
anyway). A library sends a report with
`call send_report(message, module=, procedure=, level=)`.

## Where reports come from: the call graph

Procedures that declare a scope appear in the call graph that reports carry:

```fortran
subroutine write_output(it)
    type(call_scope) :: scope
    call scope%enter("write_output", context="it=" // to_string(it))
    ...                                    ! call_graph() = "main::time_loop::write_output[it=120]"
end subroutine                             ! the scope's finaliser pops the frame
```

The stack is per thread (`!$omp threadprivate`). Do not initialise a scope in
its declaration (it would become `save` and never be finalised); `scope%leave()`
pops explicitly where finalisation is not reliable (gfortran before 13).

## API

| Call graph | Sinks |
|---|---|
| `call_scope` (`enter(name, context)`, `leave()`) | `report_sink` (abstract: `report(level, message, module, procedure)`) |
| `call_graph()`, `call_depth()`, `reset_call_graph()` | `stdlib_logger_sink`, `stderr_sink`, `procedure_sink` (`report_procedure` interface) |
| | `set_report_sink`, `reset_report_sink`, `send_report`, `report_level_name` |

Levels are stdlib_logger's (`debug_level` ... `error_level`), from `stdlib_logger`.

## Tests

`fpm test`: sinks (user extension, procedure, stdlib logger on its own logger and
on `global_logger`), level names, and the call-graph stack (nesting, contexts,
return/early return/`block`/function exits, self-repair after a scope that was
never finalised).
