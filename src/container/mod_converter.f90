!> @brief   functions to cast different unlimited polymorphic variables to different types.
!> @author  Robert schuster
module mod_converter
    use mod_fillvalue
    implicit none
    public

contains

    function toInteger(value)
        integer :: toInteger
        class(*) :: value
        select type (value)
            type is (integer)
                toInteger = value
            class default
                toInteger = fstd_fill_int
        end select
    end function

    function toReal(value)
        real :: toReal
        class(*) :: value
        select type (value)
            type is (real (kind=4))
                toReal = value
            class default
                toReal = fstd_fill_real
        end select
    end function

    function toIntegerK8(value)
        integer (kind=8) :: toIntegerK8
        class(*) :: value
        select type (value)
            type is (integer (kind=8))
                toIntegerK8 = value
            class default
                toIntegerK8 = fstd_fill_intK8
        end select
    end function

    function toRealK8(value)
        real (kind=8) :: toRealK8
        class(*) :: value
        select type (value)
            type is (real (kind=8))
                toRealK8 = value
            class default
                toRealK8 = fstd_fill_realK8
        end select
    end function

end module mod_converter
