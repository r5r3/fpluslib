!> @brief   An interface to the POSIX regex library
!> @author  Robert Schuster
module fplus_regex
    use, intrinsic :: ISO_C_BINDING
    use fplus_strings
    use fplus_object
    use fplus_hashcode
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
    type, extends(object), public :: regex
        type(C_ptr), private :: prog
        logical, private :: initialized = .false.
        integer (kind=C_int), private :: error = 0
        !> @todo    The pattern is stored in this variable, replace by character (len=:) as soon as supported
        character (len=1000), private :: pattern
        logical :: flag_REG_EXTENDED, flag_REG_ICASE, flag_REG_NOSUB, flag_REG_NEWLINE
    contains
        !> @brief   Returns .true. if this object contains a valid regex program
        procedure, public :: is_valid => regex_is_valid
        !> @brief   Returns the error code of the last function call
        procedure, public :: last_error => regex_last_error
        !> @brief   Returns an string with the last error message
        procedure, public :: last_error_msg => regex_last_error_msg
        !> @brief   returns an array of match objects
        procedure, public :: matches => regex_matches
        !> @brief   release all internaly used memory
        procedure, public :: release => regex_release
        !> @brief   Returns a string representation of this object
        procedure, public :: to_string => regex_to_string
        !> @brief   Calculate the hash code for this object
        procedure, public :: hashcode => regex_hashcode
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
    contains
        !> @brief   get a numeric representation of the matched string
        !$FP generic, public :: get_numeric => match_get_numeric
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
        if (.not. present(REG_EXTENDED) .or. REG_EXTENDED .eqv. .true.) then
            flags = ior(flags, C_regex_get_REG_EXTENDED())
            res%flag_REG_EXTENDED = .true.
        else
            res%flag_REG_EXTENDED = .false.
        end if
        if (present(REG_ICASE)    .and. REG_ICASE    .eqv. .true.) then
            flags = ior(flags, C_regex_get_REG_ICASE())
            res%flag_REG_ICASE = .true.
        else
            res%flag_REG_ICASE = .false.
        end if
        if (present(REG_NOSUB)    .and. REG_NOSUB    .eqv. .true.) then
            flags = ior(flags, C_regex_get_REG_NOSUB())
            res%flag_REG_NOSUB = .true.
        else
            res%flag_REG_NOSUB = .false.
        end if
        if (present(REG_NEWLINE)  .and. REG_NEWLINE  .eqv. .true.) then
            flags = ior(flags, C_regex_get_REG_NEWLINE())
            res%flag_REG_NEWLINE = .true.
        else
            res%flag_REG_NEWLINE = .false.
        end if

        ! allocate the c-pointer to the regex_t struct
        call C_reg_alloc(res%prog)

        ! compile the program
        res%error = C_regcomp(res%prog, pattern//C_NULL_CHAR, flags)

        ! mark the object as initialized
        res%initialized = .true.

        ! store the pattern
        res%pattern = pattern
    end function

    !> @public
    !> @brief       Returns a string  representation of this object
    !> @param[in]   this    reference to the regex object, automatically set by fortran
    function regex_to_string(this) result (res)
        class(regex) :: this
        character (len=:), allocatable :: res
        res = "Regex, pattern: " // trim(this%pattern) // char(10) // char(10)
        res = res // " Flags: " // char(10)
        res = res // "    => REG_EXTENDED = " // type_to_string(this%flag_REG_EXTENDED) // char(10)
        res = res // "    => REG_ICASE    = " // type_to_string(this%flag_REG_ICASE) // char(10)
        res = res // "    => REG_NOSUB    = " // type_to_string(this%flag_REG_NOSUB) // char(10)
        res = res // "    => REG_NEWLINE  = " // type_to_string(this%flag_REG_NEWLINE) // char(10)
    end function

    !> @public
    !> @brief       Calculate the hash code for this object
    !> @param[in]   this    reference to the regex object, automatically set by fortran
    function regex_hashcode(this) result(res)
        class(regex) :: this
        integer (kind=8) :: res

        ! local variables
        character (len=:), allocatable :: str
        integer (kind=1), dimension(:), allocatable :: chars
        integer :: l

        str = "Regex" // type_to_string(this%flag_REG_EXTENDED) // type_to_string(this%flag_REG_ICASE) // &
                         type_to_string(this%flag_REG_NOSUB) // type_to_string(this%flag_REG_NEWLINE) // &
                         trim(this%pattern)

        l = len(str)
        allocate(chars(l))
        chars = transfer(str, chars)
        call C_sdbm(chars, l, res)
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
    function regex_matches(this, string) result(res)
        class(regex) :: this
        character (len=*) :: string
        type(match), dimension(:), allocatable :: res

        ! local variables
        integer(kind=C_size_t) :: str_size, i, nmatch
        integer(kind=C_size_t), dimension(:), allocatable :: beginnings, endings

        ! allocate an array for the result, it has to have the same size as the
        ! length of the string
        str_size = len_trim(string) + 1
        allocate(beginnings(str_size))
        allocate(endings(str_size))

        ! call the c wrapper of regexec
        this%error = C_regexec(this%prog, string // C_NULL_CHAR, str_size, beginnings, endings, nmatch, 0)
        if (this%error /= 0) nmatch = 0

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
    end function

    !> @public
    !> @brief       release all internally used memory
    !> @param[in]   this    reference to the regex object, automatically set by fortran
    subroutine regex_release(this)
        class(regex) :: this
        if (this%initialized) call C_regfree(this%prog)
    end subroutine

    !$FP template match_get_numeric
        !$FP T = {real (kind=4)} {real (kind=8)} {integer (kind=4)} {integer (kind=8)}
        !$FP F = {"(F)"} {"(F)"} {"(I)"} {"(I)"}
        !$FP do i = 1, 4
            !> @brief       get a numeric representation of the matched string
            !> @param[in]   this    reference to the match object, automatically set by fortran
            !> @param[out]  value   the ${T} representation of the matched string.
            subroutine match_get_numeric(this, value)
                class(match) :: this
                ${T(i)}, intent(out) :: value

                ! local variables
                integer :: i
                character (len=100) :: str
                ! find and replace commas
                str = this%string
                do i = 1, len_trim(str)
                    if (str(i:i) == ",") str(i:i) = "."
                end do
                ! read the value
                read (str, *) value
            end subroutine
        !$FP end do
    !$FP end template
end module
