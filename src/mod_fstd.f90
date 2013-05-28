! This Module conatins some basic typs and subroutines used by different
! parts of the library
module mod_fstd
    use mod_fillvalue
    implicit none
    private

    ! this is a basic type for objects, all classes should extend this type
    ! as it contains usefull common functions
    type, abstract, public :: object
    contains
        procedure (abstract_hashcode), deferred :: hashcode
        procedure (abstract_tostring), deferred :: tostring
    end type

    !> @brief   a simple string type used by all types that extend object
    type, extends(object), public :: string
        ! a workaround, should be character (len=:), but this is not yet supported by gfortran 4.8
        integer (kind=1), dimension(:), pointer, private :: bytes
    contains
        procedure, public :: hashcode => string_hashcode
        procedure, public :: tostring => string_tostring
    end type
    interface string
        module procedure string_constructor
    end interface

    abstract interface
        function abstract_hashcode(this)
            import :: object
            class(object) :: this
            integer (kind=8) :: abstract_hashcode
        end function
        function abstract_tostring(this)
            import :: object
            import :: string
            class(object), target :: this
            class(string), pointer :: abstract_tostring
        end function
    end interface

    ! these functions are used to work around a bug in the transfer function of ifort
    interface
        subroutine C_float2intarray(flt, inta) bind(C,name="float2intarray")
            real (kind=4) :: flt
            integer (kind=1), dimension(4) :: inta
        end subroutine
        subroutine C_double2intarray(dbl, inta) bind(C,name="double2intarray")
            real (kind=8) :: dbl
            integer (kind=1), dimension(8) :: inta
        end subroutine
        subroutine C_int2intarray(i, inta) bind(C,name="int2intarray")
            integer (kind=4) :: i
            integer (kind=1), dimension(4) :: inta
        end subroutine
        subroutine C_long2intarray(l, inta) bind(C,name="long2intarray")
            integer (kind=8) :: l
            integer (kind=1), dimension(8) :: inta
        end subroutine
        subroutine C_sdbm(inta, l, hash) bind(C,name="sdbm")
            integer (kind=4) :: l
            integer (kind=1), dimension(l) :: inta
            integer (kind=8) :: hash
        end subroutine
    end interface

    ! public non type-bound procedures
    public :: calculateHash

contains

    ! procedures that are not bound to a type ---------------------------------

    ! a very simple hash function, see sdbm on http://www.cse.yorku.ca/~oz/hash.html
    function calculateHash(key)
        integer (kind=8) :: calculateHash
        class(*), intent(in) :: key

        !local variables for character key
        integer :: l = 0
        integer (kind=1), dimension(:), allocatable :: chars

        ! use sdbm
        ! transfer the content of the key to an integer array and call C_sdbm
        ! the usage of the c-functions seems to be needed to work around an error in ifort transfer function
        calculateHash = 0
        select type (key)
            type is (character (len=*))
                l = len_trim(key)
                allocate(chars(l))
                chars = transfer(key, chars)
                call C_sdbm(chars, l, calculateHash)
            type is (real (kind=4))
                allocate(chars(4))
                !chars = transfer(real(key,4), chars)
                call C_float2intarray(key, chars)
                !print*, chars(1), chars(2), chars(3), chars(4)
                call C_sdbm(chars, 4, calculateHash)
            type is (real (kind=8))
                allocate(chars(8))
                !chars = transfer(real(key,8), chars)
                call C_double2intarray(key, chars)
                call C_sdbm(chars, 8, calculateHash)
            type is (integer (kind=4))
                allocate(chars(4))
                !chars = transfer(int(key,4), chars)
                call C_int2intarray(key, chars)
                call C_sdbm(chars, 4, calculateHash)
            type is (integer (kind=8))
                allocate(chars(8))
                !chars = transfer(int(key,8), chars)
                call C_long2intarray(key, chars)
                call C_sdbm(chars, 8, calculateHash)
            class default
                ! not yet implemented for other types
                write (0, "(A)") "Unable to calculate a hash code for the given type of key!"
                call exit(1)
        end select
        ! free the memory
        if (allocated(chars)) then
            deallocate(chars)
        end if
    end function

    ! procedure for the type string -------------------------------------------

    !> @brief   Create a new initialized string
    function string_constructor(text)
        class(string), pointer :: string_constructor
        class(*), pointer :: text
        ! local variable
        integer :: l

        ! allocate the string object
        allocate(string_constructor)

        ! copy the text to the byte array
        select type (text)
            type is (character(len=*))
                l = len_trim(text)
                allocate (string_constructor%bytes(l))
                string_constructor%bytes = transfer(text, string_constructor%bytes)
            class default
                write (0, "(A)") "The type of text in the string constructor is not yet supported!"
                call exit(1)
        end select
    end function

    !> @brief   As this type is already a string, this functions returns a pointer
    !>          to itself
    function string_tostring(this)
        class(string), target :: this
        class(string), pointer :: string_tostring
        string_tostring => this
    end function


    !> @brief   calculates a hash code for this object. it is the same then for the
    !>          underlying character variable
    function string_hashcode(this)
        class(string) :: this
        integer (kind=8) :: string_hashcode
        if (associated(this%bytes)) then
            call C_sdbm(this%bytes, size(this%bytes), string_hashcode)
        else
            string_hashcode = 0
        end if
    end function

end module
