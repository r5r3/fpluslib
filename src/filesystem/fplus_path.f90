!> @brief   A representation of file of directory path names
!> @author  Robert Schuster
module fplus_path
    use fplus_object
    use fplus_hashcode
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
    end type


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

end module fplus_path
