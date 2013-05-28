! This Module conatins some basic typs and subroutines used by different
! parts of the library
module mod_fstd
    use mod_hashcode
    implicit none
    private

    ! this is a basic type for objects, all classes should extend this type
    ! as it contains usefull common functions
!    type, abstract, public :: object
!    contains
!        procedure (abstract_hashcode), deferred :: hashcode
!        procedure (abstract_tostring), deferred :: tostring
!    end type

    !> @brief   a simple string type used by all types that extend object
    type, public :: string
        ! a workaround, should be character (len=:), but this is not yet supported by gfortran 4.8
        integer (kind=1), dimension(:), pointer, private :: bytes
    contains
        procedure, public :: hashcode => string_hashcode
        procedure, public :: tostring => string_tostring
    end type
    interface string
        module procedure string_constructor
    end interface

!    abstract interface
!        function abstract_hashcode(this)
!            import :: object
!            class(object) :: this
!            integer (kind=8) :: abstract_hashcode
!        end function
!        function abstract_tostring(this)
!            import :: object
!            import :: string
!            class(object), target :: this
!            class(string), pointer :: abstract_tostring
!        end function
!    end interface

contains

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
