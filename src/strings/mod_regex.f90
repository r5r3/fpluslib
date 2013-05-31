!> @brief   An interface to the POSIX regex library
module mod_regex
    use, intrinsic :: ISO_C_BINDING
    implicit none
    private

    interface
        subroutine C_reg_alloc(prog) bind(C,name="C_reg_alloc")
            import :: C_ptr
            type(C_ptr), intent(out) :: prog
        end subroutine
        function C_regcomp(prog, pattern, cflags) bind(C,name="regcomp")
            import :: C_ptr, C_char, C_int
            type(C_ptr) :: prog
            character (kind=C_char) :: pattern(*)
            integer (kind=C_int), value :: cflags
            integer (kind=C_int) :: C_regcomp
        end function
    end interface

    !> @brief   The compiled regular expression program
    type, public :: regex
        type(C_ptr), private :: prog
        logical, private :: initialized = .false.
        integer :: error = 0
    contains
        !> @brief   Returns .trur. if this object contains a vaild regex program
        procedure, public :: is_valid => regex_is_valid
        !> @brief   Returns the error code of the last function call
        procedure, public :: last_error => regex_last_error
        !> @brief   returns an array of match objects
        !procedure, public :: match => regex_match
    end type

    ! make the constructor public
    public new_regex

contains

    !> @brief       The constructor for a regular expression program.
    function new_regex(pattern) result(res)
        type(regex) :: res
        character (len=*) :: pattern

        ! allocate the c-pointer to the regex_t struct
        call C_reg_alloc(res%prog)

        ! compile the program
        res%error = C_regcomp(res%prog, pattern//C_NULL_CHAR, 0)

        ! mark the object as initialized
        res%initialized = .true.
    end function

    !> @brief       Returns .trur. if this object contains a vaild regex program
    function regex_is_valid(this) result(res)
        class(regex) :: this
        logical :: res
        if (this%error == 0 .and. this%initialized .eqv. .true.) then
            res = .true.
        else
            res = .false.
        end if
    end function

    !> @brief       Returns the error code of the last function call
    integer function regex_last_error(this) result (res)
        class(regex) :: this
        res = this%error
    end function
end module
