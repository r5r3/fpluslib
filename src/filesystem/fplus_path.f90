!> @brief   A representation of file of directory path names
!> @author  Robert Schuster
module fplus_path
    use fplus_object
    use fplus_hashcode
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

end module fplus_path
