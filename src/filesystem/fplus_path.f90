!> @brief   A representation of file of directory path names
!> @author  Robert Schuster
module fplus_path
    use fplus_object
    use fplus_hashcode
    use fplus_container
    use fplus_error
    use fplus_strings
    use, intrinsic :: ISO_C_BINDING
    implicit none
    private

    type, extends (object), public :: path
        !> @brief   The name of the representated file or directory
        character (len=1000), public :: name
    contains
        !> @brief   Returns a string representation of this object
        procedure, public :: to_string => path_to_string
        !> @brief   Calculate the hash code for this object
        procedure, public :: hashcode => path_hashcode
        !> @brief   Tests whether the file or directory denoted by this abstract pathname exists.
        procedure, public :: exists => path_exists
        !> @brief   Returns the name as c-compatible string
        procedure, public :: get_cstr_name => path_get_cstr_name
        !> @brief   Returns the extension of the file, if any.
        procedure, public :: get_extension => path_get_extension
        !> @brief   Returns the path without the extension
        procedure, public :: get_basename => path_get_basename
        !> @brief   Returns the content of a text file as character array
        procedure, public :: get_lines => path_get_lines
    end type

    ! public procedures
    public :: new_path, to_path

contains

    !> @public
    !> @brief       The constructor for a path name representation
    function new_path(pathname) result(res)
        type (path) :: res
        character (len=*) :: pathname
        res%name = pathname
    end function

    !> @public
    !> @brief       Returns a string representation of this object
    function path_to_string(this) result (res)
        class(path) :: this
        character (len=:), allocatable :: res
        res = "Path: " // trim(this%name)
    end function

    !> @public
    !> @brief       Calculate the hash code for this object
    function path_hashcode(this) result (res)
        class (path) :: this
        integer (kind=8) :: res

        ! local variables
        character (len=:), allocatable :: str
        integer (kind=1), dimension(:), allocatable :: chars
        integer :: l

        str = "Path" // trim(this%name)

        l = len(str)
        allocate(chars(l))
        chars = transfer(str, chars)
        call C_sdbm(chars, l, res)
    end function

    !> @brief       Tests whether the file or directory denoted by this abstract pathname exists.
    logical function path_exists(this) result(res)
        class(path) :: this
        inquire(file=this%name, exist=res)
    end function

    !> @public
    !> @brief       Returns the name as c-compatible string
    function path_get_cstr_name(this) result(res)
        class(path) :: this
        character (len=:), allocatable :: res
        res = trim(this%name) // C_NULL_CHAR
    end function

    !> @public
    !> @brief       Returns the extension of the file, if any.
    function path_get_extension(this) result (res)
        class(path) :: this
        character (len=:), allocatable :: res

        !local variables
        integer :: i1, i2

        ! find the last dot
        i1 = index(this%name, ".", back=.true.)
        i2 = index(this%name, "/", back=.true.)
        if (i1==0 .or. i1 <= i2 .or. i1 == len_trim(this%name)) then
            res = ""
        else
            res = trim(this%name(i1+1:))
        end if
    end function

    !> @public
    !> @brief   Returns the path without the extension
    function path_get_basename(this) result (res)
        class(path) :: this
        character (len=:), allocatable :: res

        !local variables
        integer :: i1, i2

        ! find the last dot
        i1 = index(this%name, ".", back=.true.)
        i2 = index(this%name, "/", back=.true.)
        if (i1==0 .or. i1 <= i2 .or. i1 == len_trim(this%name)) then
            res = this%name
        else
            res = trim(this%name(:i1-1))
        end if
    end function

    !> @public 
    !> @brief       Returns the content of a text file as character array
    !> @param       as_path     set to true if the content of the file is one filename per line. 
    !>                          the returned list will then contain path objects.       
    function path_get_lines(this, as_path) result (res)
        class(path) :: this
        logical, optional :: as_path
        type(list) :: res

        ! local variables
        logical :: as_path_intern
        integer :: unit, iostat, reclen
        character(len=80) :: iomsg
        character(len=1000) :: line
        type(string) :: line_str
        type(path) :: path_str

        as_path_intern = present(as_path)
        if (as_path_intern) as_path_intern = as_path .eqv. .true.

        ! a new list for the result
        res = new_list()

        ! open the input file
        open(newunit=unit, file=this%name, status="OLD", action="READ", iostat=iostat, iomsg=iomsg)
        ! any errors? 
        if (iostat /= 0) call fplus_error_print(iomsg, "path%get_lines")

        ! read as long as no errors occure
        do
            read (unit, "(a)", iostat=iostat,iomsg=iomsg) line
            if (iostat /= 0) exit
            if (as_path_intern) then
                path_str%name = line
                call res%add(path_str, copy=.true.)
            else
                line_str%chars = line
                call res%add(line_str, copy=.true.)
            end if
        end do

        ! close the file again
        close(unit)
    end function

    !> @public
    !> @brief       Cast an class(*) type to path
    !> @param       arg     class(*) type to convert
    function to_path(arg) result (res)
        class(*) :: arg
        type(path) :: res
        select type (arg)
            type is (path)
                res = arg
            class default
                call fplus_error_print("wrong datatype in cast to path", "to_path")
        end select
    end function

end module fplus_path
