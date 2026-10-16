module test_report
    !! Tests for stdlib_report's sinks: a user extension of report_sink, a
    !! subroutine through procedure_sink, stdlib_logger_sink on a given
    !! logger and on global_logger (the default), and level names; and
    !! the call-graph stack (call_scope): nesting, contexts, every way out
    !! of a scope, and self-repair after a scope that was never finalised.
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use stdlib_logger, only: logger_type, global_logger, &
                             debug_level, information_level, warning_level, error_level
    use stdlib_report
    implicit none
    private

    public :: collect_report

    character(len=*), parameter :: log_file = "test_report_scratch.log"

    ! What the capturing sinks last received; sinks are copied on
    ! registration, so they record into module variables.
    integer :: n_reports = 0
    integer :: last_level = -1
    character(len=:), allocatable :: last_message, last_module, last_procedure

    type, extends(report_sink) :: capture_sink
    contains
        procedure :: report => capture_report
    end type capture_sink

    ! A logger_type the sink points at: must be a target outliving the registration.
    type(logger_type), target, save :: own_logger

contains

    subroutine collect_report(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("user-sink", test_user_sink), &
            new_unittest("procedure-sink", test_procedure_sink), &
            new_unittest("stdlib-logger-sink", test_stdlib_logger_sink), &
            new_unittest("default-global-logger", test_default_global_logger), &
            new_unittest("level-names", test_level_names), &
            new_unittest("scope-nesting", test_scope_nesting), &
            new_unittest("scope-exits", test_scope_exits), &
            new_unittest("scope-repair", test_scope_repair) &
            ]
    end subroutine collect_report

    subroutine capture_report(self, level, message, module, procedure)
        class(capture_sink), intent(inout) :: self
        integer, intent(in) :: level
        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: module
        character(len=*), intent(in) :: procedure

        call capture_proc(level, message, module, procedure)
    end subroutine capture_report

    subroutine capture_proc(level, message, module, procedure)
        integer, intent(in) :: level
        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: module
        character(len=*), intent(in) :: procedure

        n_reports = n_reports + 1
        last_level = level
        last_message = message
        last_module = module
        last_procedure = procedure
    end subroutine capture_proc

    subroutine clear_capture()
        n_reports = 0
        last_level = -1
        last_message = ""
        last_module = ""
        last_procedure = ""
    end subroutine clear_capture

    ! Whole content of a text file, lines joined with new_line.
    function read_text(path) result(text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: text
        character(len=1024) :: line
        integer :: unit, ios

        text = ""
        open(newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            text = text // trim(line) // new_line('a')
        end do
        close(unit, status="delete")
    end function read_text

    subroutine test_user_sink(error)
        type(error_type), allocatable, intent(out) :: error

        call clear_capture()
        call set_report_sink(capture_sink())
        call send_report("disk full", module="mylib", procedure="save_state")
        call reset_report_sink()

        call check(error, n_reports, 1, "one report")
        if (allocated(error)) return
        call check(error, last_level, error_level, "level defaults to error_level")
        if (allocated(error)) return
        call check(error, last_message, "disk full")
        if (allocated(error)) return
        call check(error, last_module, "mylib")
        if (allocated(error)) return
        call check(error, last_procedure, "save_state")
        if (allocated(error)) return

        ! Absent module/procedure arrive as empty strings.
        call set_report_sink(capture_sink())
        call send_report("note", level=warning_level)
        call reset_report_sink()
        call check(error, last_level, warning_level)
        if (allocated(error)) return
        call check(error, len(last_module) + len(last_procedure), 0, "empty names")
    end subroutine test_user_sink

    subroutine test_procedure_sink(error)
        type(error_type), allocatable, intent(out) :: error

        call clear_capture()
        call set_report_sink(procedure_sink(capture_proc))
        call send_report("callback", procedure="p")
        call reset_report_sink()

        call check(error, n_reports, 1, "one report")
        if (allocated(error)) return
        call check(error, last_message, "callback")
        if (allocated(error)) return
        call check(error, last_procedure, "p")
    end subroutine test_procedure_sink

    ! Registered before its log file is added: the sink points at the
    ! logger, so the file still receives the report.
    subroutine test_stdlib_logger_sink(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: unit, stat
        character(len=:), allocatable :: text

        call set_report_sink(stdlib_logger_sink(own_logger))
        call own_logger%add_log_file(log_file, unit, action="write", status="replace", stat=stat)
        call check(error, stat, 0, "add_log_file")
        if (allocated(error)) return

        call send_report("cannot write", module="mylib", procedure="write_data")
        call send_report("just so you know", procedure="flush_data", level=warning_level)
        call reset_report_sink()
        call own_logger%remove_log_unit(unit, close_unit=.true.)

        text = read_text(log_file)
        call check(error, index(text, "mylib % write_data: ERROR: cannot write") > 0, &
                   "error line in log, got: " // text)
        if (allocated(error)) return
        call check(error, index(text, "flush_data: WARN: just so you know") > 0, &
                   "warning line in log, got: " // text)
    end subroutine test_stdlib_logger_sink

    ! With no sink registered, reports go to stdlib's global_logger once it
    ! has a log unit. (Without one they are dropped, rather than echoed to
    ! stdout; not checked here, as it would mean capturing stdout.)
    subroutine test_default_global_logger(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: unit, stat
        character(len=:), allocatable :: text

        call reset_report_sink()
        call check(error, global_logger%log_units_assigned(), 0, "global_logger starts without units")
        if (allocated(error)) return
        call global_logger%add_log_file(log_file, unit, action="write", status="replace", stat=stat)
        call check(error, stat, 0, "add_log_file")
        if (allocated(error)) return

        call send_report("via global", module="m", procedure="p")
        call global_logger%remove_log_unit(unit, close_unit=.true.)

        text = read_text(log_file)
        call check(error, index(text, "m % p: ERROR: via global") > 0, "global_logger got: " // text)
    end subroutine test_default_global_logger

    subroutine test_level_names(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, report_level_name(debug_level), "DEBUG")
        if (allocated(error)) return
        call check(error, report_level_name(information_level), "INFO")
        if (allocated(error)) return
        call check(error, report_level_name(warning_level), "WARN")
        if (allocated(error)) return
        call check(error, report_level_name(error_level), "ERROR")
        if (allocated(error)) return
        call check(error, report_level_name(error_level + 10), "ERROR")
    end subroutine test_level_names

    subroutine test_scope_nesting(error)
        type(error_type), allocatable, intent(out) :: error
        type(call_scope) :: scope
        character(len=:), allocatable :: inner_graph

        call reset_call_graph()
        call scope%enter("a")
        call level_b(inner_graph)
        call check(error, inner_graph, "a::b[it=3]::c", "graph at the innermost scope")
        if (allocated(error)) return
        call check(error, call_graph(), "a", "b and c popped on return")
        if (allocated(error)) return
        call scope%leave()
        call check(error, call_depth(), 0, "empty after leave")

    contains

        subroutine level_b(graph)
            character(len=:), allocatable, intent(out) :: graph
            type(call_scope) :: scope

            call scope%enter("b", context="it=3")
            call level_c(graph)
        end subroutine level_b

        subroutine level_c(graph)
            character(len=:), allocatable, intent(out) :: graph
            type(call_scope) :: scope

            call scope%enter("c")
            graph = call_graph()
        end subroutine level_c
    end subroutine test_scope_nesting

    ! Early return, end of a block, a function result.
    subroutine test_scope_exits(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: depth_in_block

        call reset_call_graph()
        call early_return(.true.)
        call check(error, call_depth(), 0, "after an early return")
        if (allocated(error)) return
        call early_return(.false.)
        call check(error, call_depth(), 0, "after a normal return")
        if (allocated(error)) return
        block
            type(call_scope) :: scope
            call scope%enter("block")
            depth_in_block = call_depth()
        end block
        call check(error, depth_in_block, 1, "inside the block")
        if (allocated(error)) return
        call check(error, call_depth(), 0, "after the block")
        if (allocated(error)) return
        call check(error, traced_function(), 1, "inside a function")
        if (allocated(error)) return
        call check(error, call_depth(), 0, "after the function")

    contains

        subroutine early_return(leave_early)
            logical, intent(in) :: leave_early
            type(call_scope) :: scope

            call scope%enter("early_return")
            if (leave_early) return
            call scope%enter("early_return", context="re-entered")
        end subroutine early_return

        integer function traced_function()
            type(call_scope) :: scope

            call scope%enter("traced_function")
            traced_function = call_depth()
        end function traced_function
    end subroutine test_scope_exits

    ! A frame whose scope is never finalised (here: a `save` scope) is
    ! dropped when the next outer scope exits.
    subroutine test_scope_repair(error)
        type(error_type), allocatable, intent(out) :: error

        call reset_call_graph()
        call outer()
        call check(error, call_depth(), 0, "repaired by the outer exit")
        if (allocated(error)) return
        call reset_call_graph()

    contains

        subroutine outer()
            type(call_scope) :: scope

            call scope%enter("outer")
            call leaky()
            call check(error, call_graph(), "outer::leaky", "leaky frame still there")
        end subroutine outer

        subroutine leaky()
            type(call_scope), save :: scope

            call scope%enter("leaky")
        end subroutine leaky
    end subroutine test_scope_repair

end module test_report
