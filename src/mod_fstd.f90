! This Module conatins some basic typs and subroutines used by different
! parts of the library
module mod_fstd
    use mod_fillvalue
    implicit none
    private

    ! this is a basic type for objects, all classes should extend this type
    ! as it contains usefull common functions
    type, public :: object

    end type

contains

end module
