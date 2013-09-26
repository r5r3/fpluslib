!> @brief   Some functions to handel errors during programm execution
module fplus_error
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use fplus_strings
    implicit none
    private

    ! public precedures
    public :: fplus_error_print

    ! constants for error types
    integer, parameter, public :: FPLUS_ERR   = 1
    integer, parameter, public :: FPLUS_WARN = 2
    integer, parameter, public :: FPLUS_FATAL   = 3

    interface
        function raise(sig) result (res) bind (c, name="raise")
            import :: c_int
            integer (kind=c_int) :: res
            integer (kind=c_int), value :: sig
        end function
    end interface

contains

    !> @brief       Print an error message to STDERR
    !> @param[in]   msg         The message to print.
    !> @param[in]   procname    The name of the procedure where the error occured, optional.
    !> @paran[in]   errtype     The type of the error, optional.
    subroutine fplus_error_print(msg, procname, errtype)
        character (len=*) :: msg
        character (len=*), optional :: procname
        integer, optional :: errtype

        ! local variables
        character (len=:), allocatable :: err_type_text
        integer :: ierrtype, ierr

        ! the default error type if FPLUS_ERROR
        if (present(errtype)) then
            ierrtype = errtype
        else
            ierrtype = FPLUS_ERR
        end if

        ! select the error type text depending on the error type
        select case (ierrtype)
            case (FPLUS_ERR)
                err_type_text = "ERROR"
            case (FPLUS_WARN)
                err_type_text = "WARNING"
            case (FPLUS_FATAL)
                err_type_text = "FATAL ERROR"
            case default
                err_type_text = "UNKNOWN ERROR TYPE " // type_to_string(ierrtype)
        end select

        if (present(procname)) then
            write(error_unit, "(A,A,A,A,A)") err_type_text, " in ", trim(procname), ": ", trim(msg)
        else
            write(error_unit, "(A,A,A)") err_type_text, ": ", trim(msg)
        end if

        ! exit the program
        select case (ierrtype)
            case (FPLUS_ERR, FPLUS_FATAL)
                ierr = raise(15) !SIGTERM
                call exit(-1)
            case (FPLUS_WARN)
                ! do nothing
            case default
                ierr = raise(15) !SIGTERM
                call exit(-1)
        end select
    end subroutine

end module fplus_error
