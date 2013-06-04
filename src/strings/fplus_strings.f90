!> @brief   Functions and types for string manipulation
module fplus_strings
    use fplus_object
    implicit none

contains

    !> @brief   determine the number of digits in a number, a negative number has one digit more
    function ndigits_of_integer(number) result(ndigits)
        integer :: ndigits
        class(*) :: number
        select type (number)
            type is (integer (kind=4))
                if (number > 0) then
                    ndigits = floor(log10(real(number)))+1
                else if (number == 0) then
                    ndigits = 1
                else if (number < 0) then
                    ndigits = floor(log10(real(abs(number))))+2
                end if
            type is (integer (kind=8))
                if (number > 0) then
                    ndigits = floor(log10(real(number)))+1
                else if (number == 0) then
                    ndigits = 1
                else if (number < 0) then
                    ndigits = floor(log10(real(abs(number))))+2
                end if
            class default
                ndigits = 0
        end select
    end function

    !> @brief   Convert all number typs to strings
    function number_to_string(number, format) result(res)
        character (len=:), allocatable :: res
        character (len=*), intent(in), optional :: format
        class(*) :: number

        ! local variables
        character (len=100) :: temp
        integer :: ndigits

        ! create the number string depending on the format
        select type (number)
            type is (real (kind=4))
                if (present(format)) then
                    write(temp, format) number
                else
                    write(temp, *) number
                end if
            type is (real (kind=8))
                if (present(format)) then
                    write(temp, format) number
                else
                    write(temp, *) number
                end if
            type is (integer (kind=4))
                if (present(format)) then
                    write(temp, format) number
                else
                    write(temp, *) number
                end if
            type is (integer (kind=8))
                if (present(format)) then
                    write(temp, format) number
                else
                    write(temp, *) number
                end if
            class default
                res = "unknown type in number_to_string"
        end select
        res = trim(adjustl(temp))
    end function

    !> @brief   Returns a string representation of intrinsic types and types that extend
    !>          the abstract type object with the to_string method
    function type_to_string(val) result(res)
        character (len=:), allocatable :: res
        class(*) :: val

        select type (val)
            type is (real (kind=4))
                res = number_to_string(val)
            type is (real (kind=8))
                res = number_to_string(val)
            type is (integer (kind=4))
                res = number_to_string(val)
            type is (integer (kind=8))
                res = number_to_string(val)
            type is (character (len=*))
                res = val
            type is (logical)
                if (val .eqv. .true.) then
                    res = "T"
                else
                    res = "F"
                end if
            class is (object)
                res = val%to_string()
            class default
                res = "unknown type in type_to_string"
        end select
    end function
end module fplus_strings
