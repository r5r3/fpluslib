!> @brief   additional usefull functions for fortran arrays
module fplus_array_tools_2
    use fplus_error
    implicit none

    !$FP interface template flip
    !$FP interface template flip_2d
    !$FP interface template qsort

contains

    !$FP numeric = {real (kind=4)} {real (kind=8)} {integer (kind=4)} {integer (kind=8)}
    
    !$FP template flip
        !$FP do i = 1, 4
            !> @public
            !> @brief   reverse an onedimensional array
            subroutine flip(array)
                ${numeric(i)}, dimension(:) :: array

                ! local variables
                integer :: i, n
                ${numeric(i)}, dimension(:), allocatable :: temp

                ! copy the input array in reverse order into an temporary array
                n = size(array)
                allocate(temp(n))
                do i = 1, n
                    temp(i) = array(n-i+1)
                end do
                ! copy the reversed array back to the input array
                array = temp
            end subroutine
        !$FP end do
    !$FP end template

    !$FP template flip_2d
        !$FP do i = 1, 4
            !> @public
            !> @brief           reverse an onedimensional array
            !> @param[inout]    array   the array to flip
            !> @param[in]       dim     the dimension along which the array should be flipped
            subroutine flip_2d(array, dim)
                ${numeric(i)}, dimension(:,:) :: array
                integer, intent(in) :: dim

                ! local variables
                integer :: n1, n2, x, y, i

                ! copy the input array in reverse order into an temporary array
                n1 = size(array, 1)
                n2 = size(array, 2)
                if (dim == 1) then                
                    do i = 1, n2
                        call flip(array(:,i))
                    end do
                else if (dim == 2) then
                    do i = 1, n1
                        call flip(array(i,:))
                    end do
                else
                    call fplus_error_print("dim should be 1 or 2", "flip_2d")
                end if
            end subroutine
        !$FP end do
    !$FP end template

    !$FP template qsort
        !$FP do i = 1, 4
            !> @public
            !> @brief           quick sort algorithm
            !> @param[inout]    array   array to be sorted (1d)
            subroutine qsort(array)
                ${numeric(i)}, dimension(:) :: array

                ! local variables
                integer :: n, r, l 
                ${numeric(i)} :: pivot, temp             

                n = size(array)
                if (n < 2) return

                pivot = array(n / 2);
                l = lbound(array,1)
                r = ubound(array,1)
                do while (l <= r)
                    if (array(l) < pivot) then
                        l = l + 1
                    else if (array(r) > pivot) then
                        r = r - 1
                    else 
                        temp = array(l)
                        array(l) = array(r)
                        array(r) = temp
                        l = l + 1
                        r = r - 1
                    end if
                end do
                call qsort(array(lbound(array,1):r))
                call qsort(array(l:ubound(array,1)))
            end subroutine
        !$FP end do
    !$FP end template


end module