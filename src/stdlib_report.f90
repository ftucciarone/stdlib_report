module stdlib_report
    !! Error reports for library code: where they come from (the call graph)
    !! and where they go (a sink, i.e. the user's logger).
    !!
    !! **Call graph.** Fortran has no "who called me" query, so procedures
    !! record themselves on a per-thread stack with a `call_scope` local:
    !!
    !!     subroutine write_output(f, T, it)
    !!         type(call_scope) :: scope
    !!         call scope%enter("write_output", context="it=" // to_string(it))
    !!         ...
    !!     end subroutine                ! scope finalised: frame popped
    !!
    !! `call_graph()` then gives "main::time_loop::write_output[it=120]".
    !! Procedures without a scope are simply absent from the graph. Do not
    !! initialise a scope in its declaration (that makes it `save`, so it is
    !! never finalised); a scope in the main program is never finalised and
    !! stays as the root. `scope%leave()` pops explicitly, for compilers
    !! that do not finalise locals. Not for `pure` code: the stack is module
    !! state (`!$omp threadprivate`, so each thread has its own).
    !!
    !! **Sinks.** A library procedure that fails with no error argument to
    !! return the failure in has no caller left to hand its message to, so
    !! before `error stop` it sends the report with `send_report` to the
    !! active *sink*. A sink is any extension of `report_sink`; three are
    !! provided, and users plug in their own logger with a few lines:
    !!
    !!     call set_report_sink(stdlib_logger_sink())            ! stdlib's global_logger
    !!     call set_report_sink(stdlib_logger_sink(my_logger))   ! a logger_type with the `target` attribute
    !!     call set_report_sink(stderr_sink())                   ! plain text on the error unit
    !!     call set_report_sink(procedure_sink(my_routine))      ! a subroutine, see report_procedure
    !!     call set_report_sink(my_sink)                         ! type, extends(report_sink)
    !!
    !! fortran-lang stdlib's `stdlib_logger` writes only to Fortran units and
    !! has no such hook; this module adds it, with stdlib's logger as the
    !! default destination.
    !!
    !! `set_report_sink` keeps a copy: a sink that must share state with the
    !! caller (a logger object, a counter) holds a pointer to it, as
    !! stdlib_logger_sink does. Set the sink once, outside parallel regions.
    !! Reports are serialised (`!$omp critical`), so a sink needs no locking
    !! of its own; it must not call send_report itself, nor stop the program.
    !!
    !! With no sink registered, reports go to `global_logger` only if it has
    !! log units (`add_log_file`/`add_log_unit`); otherwise they are dropped,
    !! since the caller's `error stop` message on stderr already carries them
    !! (an unconfigured global_logger would echo them to stdout, into the
    !! program's output). An explicitly registered stdlib_logger_sink() always
    !! writes, to stdout if global_logger has no units.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use stdlib_logger, only: logger_type, global_logger, &
                             debug_level, information_level, warning_level, error_level
    implicit none
    private

    public :: report_sink
    public :: stdlib_logger_sink
    public :: stderr_sink
    public :: procedure_sink
    public :: report_procedure
    public :: set_report_sink
    public :: reset_report_sink
    public :: send_report
    public :: report_level_name
    public :: call_scope
    public :: call_graph
    public :: call_depth
    public :: reset_call_graph

    type, abstract :: report_sink
        !! Destination of error reports. Extend it and implement `report`.
    contains
        procedure(sink_report_i), deferred :: report
    end type report_sink

    abstract interface
        !> Write one report. `level` is one of stdlib_logger's debug_level,
        !> information_level, warning_level, error_level (take them from
        !> stdlib_logger; not re-exported here, to keep these common names
        !> out of `use stdlib_NetCDF`); `module` and
        !> `procedure` name the reporting library and procedure, and may be
        !> empty. `message` is complete (never truncated) and may span lines.
        subroutine sink_report_i(self, level, message, module, procedure)
            import :: report_sink
            class(report_sink), intent(inout) :: self
            integer, intent(in) :: level
            character(len=*), intent(in) :: message
            character(len=*), intent(in) :: module
            character(len=*), intent(in) :: procedure
        end subroutine sink_report_i

        !> A plain subroutine usable as a sink through procedure_sink.
        subroutine report_procedure(level, message, module, procedure)
            integer, intent(in) :: level
            character(len=*), intent(in) :: message
            character(len=*), intent(in) :: module
            character(len=*), intent(in) :: procedure
        end subroutine report_procedure
    end interface

    type, extends(report_sink) :: stdlib_logger_sink
        !! Forwards to a fortran-lang stdlib_logger `logger_type`
        !! (log_error, log_warning, log_information, log_debug by level).
        !! `stdlib_logger_sink()` uses `global_logger`;
        !! `stdlib_logger_sink(my_logger)` points at `my_logger`, which must
        !! be a `target` that outlives the registration. Being a pointer, log
        !! files added to the logger after registering still receive reports.
        type(logger_type), pointer :: logger => null()
    contains
        procedure :: report => stdlib_logger_report
    end type stdlib_logger_sink

    type, extends(report_sink) :: stderr_sink
        !! Writes "LEVEL: module % procedure: message" to the error unit.
    contains
        procedure :: report => stderr_report
    end type stderr_sink

    type, extends(report_sink) :: procedure_sink
        !! Calls a subroutine with the report_procedure interface:
        !! `procedure_sink(my_routine)`. For loggers kept in module variables;
        !! a sink that needs state of its own should extend report_sink.
        procedure(report_procedure), pointer, nopass :: proc => null()
    contains
        procedure :: report => procedure_report
    end type procedure_sink

    type :: call_scope
        !! One frame of the call graph, pushed by `enter` and popped when the
        !! scope is finalised (or by `leave`). Popping restores the depth
        !! recorded at `enter`, so a frame whose scope was never finalised is
        !! dropped by the next outer exit.
        integer, private :: depth_at_entry = -1
    contains
        procedure :: enter => scope_enter
        procedure :: leave => scope_leave
        final :: scope_final
    end type call_scope

    type :: call_frame
        character(len=63) :: name = ""
        character(len=:), allocatable :: context
    end type call_frame

    !> The call-graph stack of this thread: frames(1:depth).
    type(call_frame), allocatable, save :: frames(:)
    integer, save :: depth = 0
    !$omp threadprivate(frames, depth)

    !> The registered sink; unallocated means the default (see send_report).
    class(report_sink), allocatable, save :: active_sink

contains

    !> Push a frame named `name` (63 characters kept), shown as
    !> "name[context]" when a context is given. Entering an already entered
    !> scope first leaves it.
    subroutine scope_enter(self, name, context)
        class(call_scope), intent(inout) :: self
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: context

        type(call_frame), allocatable :: grown(:)

        call self%leave()
        if (.not. allocated(frames)) allocate(frames(16))
        if (depth == size(frames)) then
            allocate(grown(2 * size(frames)))
            grown(1:depth) = frames(1:depth)
            call move_alloc(grown, frames)
        end if

        self%depth_at_entry = depth
        depth = depth + 1
        frames(depth)%name = name
        if (present(context)) then
            frames(depth)%context = context
        else if (allocated(frames(depth)%context)) then
            deallocate(frames(depth)%context)
        end if
    end subroutine scope_enter

    !> Pop this scope's frame, and any left above it, now.
    subroutine scope_leave(self)
        class(call_scope), intent(inout) :: self

        if (self%depth_at_entry >= 0) depth = min(depth, self%depth_at_entry)
        self%depth_at_entry = -1
    end subroutine scope_leave

    subroutine scope_final(self)
        type(call_scope), intent(inout) :: self

        call self%leave()
    end subroutine scope_final

    !> The current call graph, "a::b[context]::c"; empty if no scope is open.
    function call_graph() result(graph)
        character(len=:), allocatable :: graph
        integer :: i

        graph = ""
        do i = 1, depth
            if (i > 1) graph = graph // "::"
            graph = graph // trim(frames(i)%name)
            if (allocated(frames(i)%context)) graph = graph // "[" // frames(i)%context // "]"
        end do
    end function call_graph

    !> Number of open scopes on this thread.
    integer function call_depth()
        call_depth = depth
    end function call_depth

    !> Empty this thread's stack (for tests, or after recovering from an error
    !> that skipped finalisation).
    subroutine reset_call_graph()
        depth = 0
    end subroutine reset_call_graph

    !> Make `sink` (a copy of it) the destination of all later reports.
    subroutine set_report_sink(sink)
        class(report_sink), intent(in) :: sink

        if (allocated(active_sink)) deallocate(active_sink)
        allocate(active_sink, source=sink)
    end subroutine set_report_sink

    !> Go back to the default: global_logger if it has log units, else nothing.
    subroutine reset_report_sink()
        if (allocated(active_sink)) deallocate(active_sink)
    end subroutine reset_report_sink

    !> Send a report to the active sink. `level` defaults to error_level.
    !> With no sink registered, the report goes to global_logger if it has
    !> log units, and is dropped otherwise.
    subroutine send_report(message, module, procedure, level)
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: module
        character(len=*), intent(in), optional :: procedure
        integer, intent(in), optional :: level

        type(stdlib_logger_sink) :: default_sink
        integer :: lvl
        character(len=:), allocatable :: mod, proc

        lvl = error_level
        if (present(level)) lvl = level
        mod = ""
        if (present(module)) mod = module
        proc = ""
        if (present(procedure)) proc = procedure

        !$omp critical (stdlib_report_send)
        if (allocated(active_sink)) then
            call active_sink%report(lvl, message, mod, proc)
        else if (global_logger%log_units_assigned() > 0) then
            call default_sink%report(lvl, message, mod, proc)
        end if
        !$omp end critical (stdlib_report_send)
    end subroutine send_report

    !> "DEBUG", "INFO", "WARN" or "ERROR" (stdlib_logger's prefixes) for a level (the nearest one
    !> below it, so any level above error_level is "ERROR").
    pure function report_level_name(level) result(name)
        integer, intent(in) :: level
        character(len=:), allocatable :: name

        if (level >= error_level) then
            name = "ERROR"
        else if (level >= warning_level) then
            name = "WARN"
        else if (level >= information_level) then
            name = "INFO"
        else
            name = "DEBUG"
        end if
    end function report_level_name

    subroutine stdlib_logger_report(self, level, message, module, procedure)
        class(stdlib_logger_sink), intent(inout) :: self
        integer, intent(in) :: level
        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: module
        character(len=*), intent(in) :: procedure

        if (associated(self%logger)) then
            call forward(self%logger)
        else
            call forward(global_logger)
        end if

    contains

        ! stdlib_logger prints present-but-empty module/procedure as " % :",
        ! so empty names are passed as absent.
        subroutine forward(logger)
            type(logger_type), intent(in) :: logger

            if (len(module) > 0 .and. len(procedure) > 0) then
                call log_at(logger, module=module, procedure=procedure)
            else if (len(module) > 0) then
                call log_at(logger, module=module)
            else if (len(procedure) > 0) then
                call log_at(logger, procedure=procedure)
            else
                call log_at(logger)
            end if
        end subroutine forward

        subroutine log_at(logger, module, procedure)
            type(logger_type), intent(in) :: logger
            character(len=*), intent(in), optional :: module
            character(len=*), intent(in), optional :: procedure

            if (level >= error_level) then
                call logger%log_error(message, module=module, procedure=procedure)
            else if (level >= warning_level) then
                call logger%log_warning(message, module=module, procedure=procedure)
            else if (level >= information_level) then
                call logger%log_information(message, module=module, procedure=procedure)
            else
                call logger%log_debug(message, module=module, procedure=procedure)
            end if
        end subroutine log_at
    end subroutine stdlib_logger_report

    subroutine stderr_report(self, level, message, module, procedure)
        class(stderr_sink), intent(inout) :: self
        integer, intent(in) :: level
        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: module
        character(len=*), intent(in) :: procedure

        character(len=:), allocatable :: origin

        origin = ""
        if (len(module) > 0) origin = module
        if (len(module) > 0 .and. len(procedure) > 0) origin = origin // " % "
        if (len(procedure) > 0) origin = origin // procedure
        if (len(origin) > 0) origin = origin // ": "
        write(error_unit, '(a)') report_level_name(level) // ": " // origin // message
        flush(error_unit)
    end subroutine stderr_report

    subroutine procedure_report(self, level, message, module, procedure)
        class(procedure_sink), intent(inout) :: self
        integer, intent(in) :: level
        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: module
        character(len=*), intent(in) :: procedure

        if (associated(self%proc)) call self%proc(level, message, module, procedure)
    end subroutine procedure_report

end module stdlib_report
