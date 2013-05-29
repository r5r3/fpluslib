! This Module conatins some basic typs and subroutines used by different
! parts of the library
module mod_fstd
    use mod_hashcode
    implicit none
    private

    ! @brief    this is a basic type for objects, all classes should extend this type
    !           as it contains usefull common functions
    type, abstract, public :: object
    contains
        procedure (abstract_hashcode), deferred :: hashcode
!        procedure (abstract_tostring), deferred :: tostring
    end type

    abstract interface
        function abstract_hashcode(this)
            import :: object
            class(object) :: this
            integer (kind=8) :: abstract_hashcode
        end function
    end interface

!    abstract interface
!        function abstract_tostring(this)
!            import :: object
!            class(object) :: this
!            character (len=*), pointer :: abstract_tostring
!        end function
!    end interface

contains


end module
