!> @brief   An interface to the POSIX regex library
!> @author  Robert Schuster
!> @todo    Implement to_string and hashcode methods.
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
            type(C_ptr), intent(in), value :: prog
            character (kind=C_char) :: pattern(*)
            integer (kind=C_int), value :: cflags
            integer (kind=C_int) :: C_regcomp
        end function
        subroutine C_regfree(prog) bind(C,name="regfree")
            import :: C_ptr
            type(C_ptr), intent(in), value :: prog
        end subroutine
        function C_regexec(prog, string, nmatchmax, beginnings, endings, nmatch, cflags) bind(C,name="C_regexec_helper")
            import :: C_ptr, C_char, C_int, C_size_t
            type(C_ptr), intent(in), value :: prog
            character (kind=C_char) :: string(*)
            integer(kind=C_size_t), value :: nmatchmax
            integer(kind=C_size_t) :: nmatch
            integer(kind=C_size_t) :: beginnings(*), endings(*)
            integer (kind=C_int), value :: cflags
            integer (kind=C_int) :: C_regexec
        end function
        function C_regerror(errcode, prog, errbuf, errbuf_size) bind(C,name="regerror")
            import :: C_int, C_size_t, C_ptr, C_char
            integer (kind=C_int), value :: errcode
            type(C_ptr), intent(in), value :: prog
            character (kind=C_char) :: errbuf(*)
            integer (kind=C_size_t), value :: errbuf_size
            integer (kind=C_size_t) :: C_regerror
        end function
        function C_regex_get_REG_EXTENDED() bind(C,name="C_regex_get_REG_EXTENDED")
            import :: C_int
            integer (kind=C_int) :: C_regex_get_REG_EXTENDED
        end function
        function C_regex_get_REG_ICASE() bind(C,name="C_regex_get_REG_ICASE")
            import :: C_int
            integer (kind=C_int) :: C_regex_get_REG_ICASE
        end function
        function C_regex_get_REG_NOSUB() bind(C,name="C_regex_get_REG_NOSUB")
            import :: C_int
            integer (kind=C_int) :: C_regex_get_REG_NOSUB
        end function
        function C_regex_get_REG_NEWLINE() bind(C,name="C_regex_get_REG_NEWLINE")
            import :: C_int
            integer (kind=C_int) :: C_regex_get_REG_NEWLINE
        end function
    end interface

    !> @brief   The compiled regular expression program
    type, public :: regex
        type(C_ptr), private :: prog
        logical, private :: initialized = .false.
        integer (kind=C_int) :: error = 0
    contains
        !> @brief   Returns .true. if this object contains a vaild regex program
        procedure, public :: is_valid => regex_is_valid
        !> @brief   Returns the error code of the last function call
        procedure, public :: last_error => regex_last_error
        !> @brief   Returns an string with the last error message
        procedure, public :: last_error_msg => regex_last_error_msg
        !> @brief   returns an array of match objects
        procedure, public :: matches => regex_matches
        !> @brief   release all internaly used memory
        procedure, public :: release => regex_release
    end type

    !> @brief   The return type of regex%match
    type, public :: match
        !> @brief   the index of the first character that belongs to the match
        integer (kind=8) :: first
        !> @brief   the index of the last character that belongs to the match
        integer (kind=8) :: last
        !> @brief   the part of the original string that belongs to the match.
        !>          This variable is only set, if the length of the match is <= 100.
        !> @todo    change type to len=:
        character (len=100) :: string
    end type

    ! make the constructor public
    public new_regex

contains

    !> @public
    !> @brief       The constructor for a regular expression program.
    !> @param[in]   REG_EXTENDED    set to .true. to enable the extended POSIX regular expression syntax, default = .true.
    !> @param[in]   REG_ICASE       set to .true. to enable case insensitive matching, default = .false.
    !> @param[in]   REG_NOSUB       set to .true. to disable the reporting of position of matches, default = .false.
    !> @param[in]   REG_NEWLINE     set to .true. to disable a newline match of the Match-any-character operator, default = .false.
    !> @result      The initialized regex object with the compiled program.
    function new_regex(pattern, REG_EXTENDED, REG_ICASE, REG_NOSUB, REG_NEWLINE) result(res)
        type(regex) :: res
        character (len=*) :: pattern
        logical, optional :: REG_EXTENDED, REG_ICASE, REG_NOSUB, REG_NEWLINE

        ! local variables
        integer :: flags

        ! calculate the flag from the given arguments
        flags = 0
        if (.not. present(REG_EXTENDED) .or. REG_EXTENDED .eqv. .true.) flags = ior(flags, C_regex_get_REG_EXTENDED())
        if (present(REG_ICASE)    .and. REG_ICASE    .eqv. .true.) flags = ior(flags, C_regex_get_REG_ICASE())
        if (present(REG_NOSUB)    .and. REG_NOSUB    .eqv. .true.) flags = ior(flags, C_regex_get_REG_NOSUB())
        if (present(REG_NEWLINE)  .and. REG_NEWLINE  .eqv. .true.) flags = ior(flags, C_regex_get_REG_NEWLINE())

        ! allocate the c-pointer to the regex_t struct
        call C_reg_alloc(res%prog)

        ! compile the program
        res%error = C_regcomp(res%prog, pattern//C_NULL_CHAR, flags)

        ! mark the object as initialized
        res%initialized = .true.
    end function

    !> @public
    !> @brief       Returns .true. if this object contains a vaild regex program
    !> @param[in]   this    reference to the regex object, automatically set by fortran
    function regex_is_valid(this) result(res)
        class(regex) :: this
        logical :: res
        if (this%error == 0 .and. this%initialized .eqv. .true.) then
            res = .true.
        else
            res = .false.
        end if
    end function

    !> @public
    !> @brief       Returns the error code of the last function call. 0 means no error.
    !> @param[in]   this    reference to the regex object, automatically set by fortran
    integer function regex_last_error(this) result (res)
        class(regex) :: this
        res = this%error
    end function

    !> @public
    !> @brief       Returns an string with the last error message
    !> @param[in]   this    reference to the regex object, automatically set by fortran
    function regex_last_error_msg(this) result (res)
        class(regex) :: this
        character (len=:), allocatable :: res

        ! local variables
        character (len=1000) :: buffer
        integer (kind=C_size_t) :: msgsize

        ! ask the posix lib for the error message
        if (this%error /= 0) then
            msgsize = 1000
            msgsize =  C_regerror(this%error, this%prog, buffer, msgsize)

            ! copy the message to the result
            res = buffer(1:msgsize)
        else
            res = "No error!"
        end if
    end function

    !> @public
    !> @brief       returns an array of match objects
    !> @param[in]   this    reference to the regex object, automatically set by fortran
    !> @param[out]  res     an array of type match and dimension number of matches.
    !> @todo        this subroutine should be a function, but there are problems with the 
    !>              ifort compiler. The returned array is not allocated.
    subroutine regex_matches(this, string, res)
        class(regex) :: this
        character (len=*) :: string
        type(match), dimension(:), intent(out), allocatable :: res

        ! local variables
        integer(kind=C_size_t) :: str_size, i, nmatch
        integer(kind=C_size_t), dimension(:), allocatable :: beginnings, endings

        ! allocate an arry for the result, it has to have the same size as the
        ! length of the string
        str_size = len_trim(string) + 1
        allocate(beginnings(str_size))
        allocate(endings(str_size))

        ! call the c wrapper of regexec
        this%error = C_regexec(this%prog, string // C_NULL_CHAR, str_size, beginnings, endings, nmatch, 0)

        ! create the match objects
        allocate(res(nmatch))
        do i = 1, nmatch
            res(i)%first = beginnings(i)+1
            res(i)%last = endings(i)
            if (res(i)%last-res(i)%first+1<100) res(i)%string = string(res(i)%first:res(i)%last)
        end do

        ! clean up beginnings and endings
        deallocate(beginnings)
        deallocate(endings)
    end subroutine

    !> @public
    !> @brief       release all internaly used memory
    !> @param[in]   this    reference to the regex object, automatically set by fortran
    subroutine regex_release(this)
        class(regex) :: this
        if (this%initialized) call C_regfree(this%prog)
    end subroutine
end module
