module fplus_progressbar
    use, intrinsic :: ISO_C_BINDING
    use, intrinsic :: iso_fortran_env, only : output_unit
#ifdef __INTEL_COMPILER
    use ifport
#endif
    use fplus_datetime
    implicit none
    private

    ! public procedures
    public new_progressbar

    type, public :: progressbar
        integer (kind=8) :: nmax
        integer (kind=8) :: width
        integer (kind=8) :: last_print_pos = -1
        character(len=200) :: prefix
        logical :: timing
        type(datetime) :: start_time
        real (kind=8) :: last_remaining_time = -1
    contains
        !$FP generic, public :: update => progressbar_update
    end type

    !$FP interface template new_progressbar

contains

    ! construct a new progressbar
    !$FP template new_progressbar
        !$FP do i = {4} {8}
            function new_progressbar(nmax, width, prefix, timing) result (res)
                integer (kind=${i}), intent(in) :: nmax
                integer, optional, intent(in) :: width
                character(len=*), optional, intent(in) :: prefix
                logical, optional, intent(in) :: timing

                type(progressbar) :: res
                res%nmax = nmax
                if (present(width)) then
                    res%width = width
                else
                    res%width = 100
                end if
                if (present(prefix)) then
                    res%prefix = prefix
                else
                    res%prefix = ""
                end if
                if (present(timing)) then
                    res%timing = timing
                else
                    res%timing = .false.
                end if
                if (res%timing) then
                    res%start_time = new_datetime()
                end if
            end function
        !$FP end do
    !$FP end template

    ! update the current state of the progressbar and print it to the screen
    !$FP template progressbar_update
        !$FP do i = {4} {8}
            subroutine progressbar_update(this, npos, overwrite)
                class(progressbar) :: this
                integer (kind=${i}) :: npos
                logical, optional :: overwrite
                
                ! local variables
                integer (kind=8) :: print_pos, print_pos_perc, i, days, hours, minutes, seconds
                logical :: overwrite_intern
                type(datetime) :: now
                real (kind=8) :: remaining_time
                character (len=18) :: time_prefix

                ! update
                print_pos = (this%width*npos)/this%nmax
                print_pos_perc = (100*npos)/this%nmax
                ! update timing?
                remaining_time = -1
                if (this%timing .and. npos > 0) then
                    now = new_datetime()
                    if (npos == this%nmax) then
                        remaining_time = anint(now%time_in_sec1970 - this%start_time%time_in_sec1970)
                    else
                        remaining_time = real(npos,8) / this%nmax
                        remaining_time = anint((now%time_in_sec1970 - this%start_time%time_in_sec1970) / remaining_time * (1-remaining_time))
                    end if
                end if
                if (this%last_print_pos /= print_pos_perc .or. this%last_remaining_time /= remaining_time) then
                    this%last_print_pos = print_pos_perc
                    this%last_remaining_time = remaining_time
                else
                    return
                end if

                ! print it
                write(*, "(A,A)", advance="no") trim(this%prefix), " ["
                do i = 1, print_pos
                    write(*, "(A)", advance="no") "="
                end do
                do i = print_pos+1, this%width
                    write(*, "(A)", advance="no") " "
                end do
                write(*, "(A,I4,A)", advance="no") "]", print_pos_perc, "%"
                if (this%timing) then
                    days = int(remaining_time,8) / 86400
                    remaining_time = remaining_time - days * 86400
                    hours = int(remaining_time,8) / 3600
                    remaining_time = remaining_time - hours * 3600
                    minutes = int(remaining_time,8) / 60
                    seconds = int(remaining_time,8) - minutes * 60
                    if (npos == this%nmax) then
                        time_prefix = ", elapsed time:   "
                    else
                        time_prefix = ", estimated time: "
                    end if
                    if (days > 0) then
                        write(*, "(A,I3,A,I0.2,A,I0.2,A,I0.2)", advance="no"), time_prefix, days, " days, ", hours, ":", minutes, ":", seconds
                    else
                        write(*, "(A,I0.2,A,I0.2,A,I0.2,A)", advance="no"), time_prefix, hours, ":", minutes, ":", seconds, "          "
                    end if                    
                end if

                ! move the cursor back one line
                if (present(overwrite) .and. npos /= this%nmax .and. isatty(output_unit)) then 
                    write(*, "(A)", advance="no") char(13)
                else
                    write(*, "(A)") " "
                end if
            end subroutine
        !$FP end do
    !$FP end template
end module