!> @brief   functions to cast different unlimited polymorphic variables to different types.
!> @author  Robert schuster
module fplus_converter
    use fplus_fillvalue
    implicit none
    public

contains

    function to_integer(value)
        integer :: to_integer
        class(*) :: value
        select type (value)
            type is (integer)
                to_integer = value
            class default
                to_integer = fplus_fill_int
        end select
    end function

    function to_real(value)
        real :: to_real
        class(*) :: value
        select type (value)
            type is (real (kind=4))
                to_real = value
            class default
                to_real = fplus_fill_real
        end select
    end function

    function to_integer8(value)
        integer (kind=8) :: to_integer8
        class(*) :: value
        select type (value)
            type is (integer (kind=8))
                to_integer8 = value
            class default
                to_integer8 = fplus_fill_intK8
        end select
    end function

    function to_real8(value)
        real (kind=8) :: to_real8
        class(*) :: value
        select type (value)
            type is (real (kind=8))
                to_real8 = value
            class default
                to_real8 = fplus_fill_realK8
        end select
    end function

end module fplus_converter
