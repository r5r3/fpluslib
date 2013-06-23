!> @brief   A test for the template script
module mod_test_template

    interface template swap
    end interface

contains

    template swap
        replace :: T => real, dimension(:,:) ; integer
        replace :: AL => , allocatable ; nil
    contains

        !>@ brief   swap two values without a mask
        subroutine swap(a, b)
            T, intent(inout) :: a
            T, intent(inout) :: b
            T AL :: temp
            temp = a
            a = b
            b = temp
        end subroutine

        !>@ brief   swap two values with a mask
        subroutine swap_mask(a, b, mask)
            T, intent(inout) :: a
            T, intent(inout) :: b
            logical, intent(in) :: mask
            T AL :: temp
            if (mask) then
                temp = a
                a = b
                b = temp
            end if
        end subroutine
    end template

end module

program test_template
    use mod_test_template
    implicit none

    ! local variables
    integer :: a, b
    real, dimension(2,2) :: c, d

    ! at first swap two integers
    a = 1
    b = 2
    call swap(a, b)
    print*, a, b

    ! then swap two arrays
    c = 1
    d = 2
    call swap(c, d)
    print*, c
    print*, d

    ! now take the mask version
    call swap(a, b, .false.)
    print*, a, b

end program test_template
