module fplus_progressbar
    use, intrinsic :: ISO_C_BINDING
    use, intrinsic :: iso_fortran_env, only : output_unit
    implicit none
    private

    ! public procedures
    public new_progressbar

    type, public :: progressbar
        integer (kind=8) :: nmax
        integer (kind=8) :: width
        integer (kind=8) :: last_print_pos = -1
    contains
        !$FP generic, public :: update => progressbar_update
    end type

    !$FP interface template new_progressbar

contains

    ! construct a new progressbar
    !$FP template new_progressbar
        !$FP do i = {4} {8}
            function new_progressbar(nmax, width) result (res)
                integer (kind=${i}) :: nmax
                integer, optional :: width
                type(progressbar) :: res
                res%nmax = nmax
                if (present(width)) then
                    res%width = width
                else
                    res%width = 100
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
                integer (kind=8) :: print_pos, print_pos_perc, i
                logical :: overwrite_intern

                ! update
                print_pos = (this%width*npos)/this%nmax
                print_pos_perc = (100*npos)/this%nmax
                if (this%last_print_pos /= print_pos_perc) then
                    this%last_print_pos = print_pos_perc
                else
                    return
                end if

                ! print it
                write(*, "(A)", advance="no") " ["
                do i = 1, print_pos
                    write(*, "(A)", advance="no") "="
                end do
                do i = print_pos+1, this%width
                    write(*, "(A)", advance="no") " "
                end do
                write(*, "(A,I4,A)", advance="no") "]", print_pos_perc, "%"

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