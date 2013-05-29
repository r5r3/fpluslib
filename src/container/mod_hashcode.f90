!> @brief   some function for the calculation of hash codes
module mod_hashcode
    implicit none
    public

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

contains

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


end module mod_hashcode
