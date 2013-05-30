! This Module conatins some basic typs and subroutines used by different
! parts of the library
module mod_fstd
    implicit none
    private

    ! @brief    this is a basic type for objects, all classes should extend this type
    !           as it contains usefull common functions
    type, abstract, public :: object
    contains
        procedure (abstract_hashcode), deferred :: hashcode
        procedure (abstract_to_string), deferred :: to_string
    end type

    abstract interface
        function abstract_hashcode(this)
            import :: object
            class(object) :: this
            integer (kind=8) :: abstract_hashcode
        end function
    end interface

    abstract interface
        function abstract_to_string(this)
            import :: object
            class(object) :: this
            character (len=:), allocatable :: abstract_to_string
        end function
    end interface

contains


end module
