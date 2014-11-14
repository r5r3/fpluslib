!> @brief   some useful procedures for fortran arrays
module fplus_array_tools
    use fplus_fillvalue
    use fplus_error
    use fplus_array_tools_2

    !>@ brief   Interface for the implementation of is_monotonic
    interface template rotate_array
    end interface

    !>@ brief   Interface for the implementation of is_monotonic
    interface template is_monotonic
    end interface

    !>@ brief   Interface for the implementation of closest_index
    interface template closest_index
    end interface

    !>@ brief   Interface for the implementation of value_in_range
    interface template value_in_range
    end interface

    !>@ brief   Interface for the implementation of valueindex_largest_ge
    interface template valueindex_largest_ge
    end interface

    !>@ brief   Interface for the implementation of valueindex_largest_le
    interface template valueindex_largest_le
    end interface

    !>@ brief   Interface for the implementation of valueindex_smallest_ge
    interface template valueindex_smallest_ge
    end interface

    !>@ brief   Interface for the implementation of valueindex_smallest_le
    interface template valueindex_smallest_le
    end interface

    !>@ brief   Interface for the implementations of valueindex
    interface template valueindex
    end interface

    !>@ brief   Interface for the implementations of swap
    interface template swap
    end interface

    !>@ brief   Interface for the implementations of array_copy
    interface template array_copy
    end interface

    !>@ brief   Interface for the implementations of array_copy
    interface template qsort
    end interface

contains

    template rotate_array
        replace :: T => numeric
        replace, foreach(T) :: DIMDOTS => nil ; ,: ; ,:,: ; ,:,:,:
    contains
        !> @brief       Take the first n points from the array and shift them to the end of the array
        !> @param[in]   array   the array to rotate
        !> @param[in]   n       the number of points by which the array will be rotated
        subroutine rotate_array(array, n)
            T, dimension(: DIMDOTS) :: array
            integer :: n
            ! local variables
            integer :: length
            T, dimension(: DIMDOTS), allocatable :: temp
            ! is n valid?
            if (n == 0) return
            if (n < 0 .or. n > size(array, 1)) call fplus_error_print("invalid number of points to rotate", "rotate_array")
            ! perform the shift
            temp = array
            length = size(array, 1)
            temp(length-n+2:length DIMDOTS) = array(:n-1 DIMDOTS)
            temp(1:length-n+1 DIMDOTS) = array(n: DIMDOTS)
            array = temp
        end subroutine
    end template

    template is_monotonic
        replace :: T => real (kind=4); real (kind=8); integer (kind=4); integer (kind=8)
    contains
        !> @brief       Check if an array is monotonicaly increasing or decreasing
        !> @param[in]   array   the array to check
        !> @param[out]  order   ascending = 1, descending = -1, this value is set after comparing the first two items of the array
        logical function is_monotonic(array, order) result (res)
            T, dimension(:), intent(in) :: array
            integer, intent(out), optional :: order
            ! local variables
            integer :: i, direction, new_direction
            direction = 0
            if (present(order)) order = direction
            res = .false.
            ! loop over all array elements
            do i = lbound(array,1)+1, ubound(array,1)
                if (array(i) > array(i-1)) new_direction = 1
                if (array(i) < array(i-1)) new_direction = -1
                if (direction == 0) then
                    direction = new_direction
                    if (present(order)) order = direction
                else
                    if (direction /= new_direction) then
                        ! the direction has changed, we can leave the function
                        return
                    end if
                end if
            end do
            ! the array is monotonic if we reace this point
            res = .true.
        end function
    end template

    template closest_index
        replace :: T => real (kind=4); real (kind=8); integer (kind=4); integer (kind=8)
    contains
        !> @brief       Find the index of an array where the stored value is the closest to an given value
        !> @param[in]   array   the array to search in
        !> @param[in]   value   the value to search for
        integer function closest_index(array, value) result (res)
            T, dimension(:), intent(in) :: array
            T, intent(in) :: value
            ! local variables
            T :: distance, smallest_distance
            integer :: i
            
            ! loop over all values
            smallest_distance = abs(array(1) - value)
            res = lbound(array, 1)
            do i = lbound(array, 1)+1, ubound(array, 1)
                distance = abs(array(i) - value)
                if (distance < smallest_distance) then
                    smallest_distance = distance
                    res = i
                end if
            end do
        end function
    end template

    template value_in_range
        replace :: T => real (kind=4); real (kind=8); integer (kind=4); integer (kind=8)
    contains
        !> @brief       Check if a given value is between the minimum and maximum value fromthe given array
        !> @param[in]   array       the array to search in
        !> @param[in]   value       the value to search for
        logical function value_in_range(array, value) result (res)
            T, dimension(:), intent(in) :: array
            T, intent(in) :: value
            res = .false.
            if (value < minval(array)) return
            if (value > maxval(array)) return
            res = .true.
        end function

        !> @brief       Check if a given value is between the minimum and maximum value fromthe given array
        !> @param[in]   array       the array to search in
        !> @param[in]   value       the value to search for
        function values_in_range(array, value) result (res)
            T, dimension(:), intent(in) :: array
            T, dimension(:), intent(in) :: value
            logical, dimension(:), allocatable :: res
            
            ! local variables
            T :: min_array, max_array
            integer :: i
            min_array = minval(array)
            max_array = maxval(array)
            ! create an result array
            allocate(res(size(value)))
            res = .true.
            ! loop over all values
            do i = lbound(value, 1), ubound(value, 1)
                if (value(i) < min_array .or. value(i) > max_array) res(i) = .false.
            end do 
        end function
    end template
    
    template valueindex_largest_ge
        replace :: T => real (kind=4); real (kind=8); integer (kind=4); integer (kind=8)
    contains
        !> @brief       Find the largest index that is less or equal to a given value
        !> @param[in]   array       the array to search in
        !> @param[in]   value       the value to search for
        function valueindex_largest_ge(array, value) result (res)
            T, dimension(:), intent(in) :: array
            T, intent(in) :: value
            integer :: res

            ! local variables
            integer :: ind
            res = fplus_fill_int

            ! loop over the array
            do ind = ubound(array, 1), lbound(array, 1), -1
                if (array(ind) >= value) then
                    res = ind
                    exit
                end if
            end do
        end function
    end template

    template valueindex_largest_le
        replace :: T => real (kind=4); real (kind=8); integer (kind=4); integer (kind=8)
    contains
        !> @brief       Find the largest index that is less or equal to a given value
        !> @param[in]   array       the array to search in
        !> @param[in]   value       the value to search for
        function valueindex_largest_le(array, value) result (res)
            T, dimension(:), intent(in) :: array
            T, intent(in) :: value
            integer :: res

            ! local variables
            integer :: ind
            res = fplus_fill_int

            ! loop over the array
            do ind = ubound(array, 1), lbound(array, 1), -1
                if (array(ind) <= value) then
                    res = ind
                    exit
                end if
            end do
        end function
    end template

    template valueindex_smallest_ge
        replace :: T => real (kind=4); real (kind=8); integer (kind=4); integer (kind=8)
    contains
        !> @brief       Find the smallest index that is greater or equal to a given value
        !> @param[in]   array       the array to search in
        !> @param[in]   value       the value to search for
        function valueindex_smallest_ge(array, value) result (res)
            T, dimension(:), intent(in) :: array
            T, intent(in) :: value
            integer :: res

            ! local variables
            integer :: ind
            res = fplus_fill_int

            ! loop over the array
            do ind = lbound(array, 1), ubound(array, 1)
                if (array(ind) >= value) then
                    res = ind
                    exit
                end if
            end do
        end function
    end template

    template valueindex_smallest_le
        replace :: T => real (kind=4); real (kind=8); integer (kind=4); integer (kind=8)
    contains
        !> @brief       Find the smallest index that is greater or equal to a given value
        !> @param[in]   array       the array to search in
        !> @param[in]   value       the value to search for
        function valueindex_smallest_le(array, value) result (res)
            T, dimension(:), intent(in) :: array
            T, intent(in) :: value
            integer :: res

            ! local variables
            integer :: ind
            res = fplus_fill_int

            ! loop over the array
            do ind = lbound(array, 1), ubound(array, 1)
                if (array(ind) <= value) then
                    res = ind
                    exit
                end if
            end do
        end function
    end template

    template valueindex
        replace :: T => real (kind=4); real (kind=8); integer (kind=4); integer (kind=8)
    contains
        !> @brief       Find the location of a value in an array
        !> @param[in]   array       the array to search in
        !> @param[in]   value       the value to serach for
        !> @param[in]   lowerbound  dont't look at locations below this value
        !> @param[in]   upperbound  dont't look at locations above this value
        !> @returns     the index of the value or fplus_fill_int if nothing was found
        recursive function valueindex(array, value, lowerbound, upperbound, sorted) result(res)
            T, dimension(:), intent(in) :: array
            T, intent(in) :: value
            integer, optional :: upperbound, lowerbound
            logical, optional :: sorted
            integer :: res

            ! local variables
            integer :: lb, ub, i, mid
            res = fplus_fill_int

            ! find the upper and lower bound of the array
            if (present(upperbound)) then
                ub = upperbound
            else
                ub = ubound(array, 1)
            end if
            if (present(lowerbound)) then
                lb = lowerbound
            else
                lb = lbound(array, 1)
            end if

            ! check if the value is larger or smaler than the first or last value in
            ! the array if the array is sorted
            if (present(sorted) .and. sorted) then
                ! out of range?
                if (value > array(ub) .or. value < array(lb)) return
                ! dived the dataset and search recursive
                if (ub - lb > 10) then
                    mid = (lb + ub)/2
                    if (array(mid) == value) then
                        res = mid
                        return
                    else if (array(mid) > value) then
                        res = valueindex(array, value, lb, mid, sorted)
                        return
                    else if (array(mid) < value) then
                        res = valueindex(array, value, mid, ub, sorted)
                        return
                    end if
                end if
            end if

            ! loop over the array
            do i = lb, ub
                if (array(i) == value) then
                    res = i
                    exit
                end if
            end do
        end function
    end template
    
    template qsort
        replace :: T => real (kind=4); real (kind=8); integer (kind=4); integer (kind=8)
    contains
        !> @brief   Sort an array with quicksort
        !> @details Sort an array arr by recursively applying the Fortran 90 pack intrinsic.
        !>          The method is similar to Quicksort, but this variant allows parallelization by the Fortran 90 compiler.
        !>          This subroutine is an adaption from Numerical Recepies
        recursive subroutine qsort(arr)
            T, dimension(:), intent(inout) :: arr
            ! local variables
            T :: a
            T, dimension(:), allocatable, save :: temp
            integer :: n,k,nl,nerr
            integer, save :: level=0
            logical, dimension(:), allocatable, save :: mask
            print*, level
            n=size(arr)
            if (n <= 1) return
            k=(1+n)/2
            call swap(arr(1),arr(k),arr(1)>arr(k))
            call swap(arr(k),arr(n),arr(k)>arr(n))
            call swap(arr(1),arr(k),arr(1)>arr(k))
            if (n <= 3) return
            level=level+1
            if (level == 1) allocate(mask(n),temp(n))
            a=arr(k)
            mask(1:n) = (arr <= a)
            mask(k) = .false.
            call array_copy(pack(arr,mask(1:n)),temp,nl,nerr)
            mask(k) = .true.
            temp(nl+2:n)=pack(arr,.not. mask(1:n))
            temp(nl+1)=a
            arr=temp(1:n)
            call qsort(arr(1:nl))
            call qsort(arr(nl+2:n))
            if (level == 1) deallocate(mask,temp)
            level=level-1
        end subroutine
    end template
    
    template array_copy
        replace :: T => numeric
    contains
        !> @brief   content of one array to another
        !> @details From Numerical Recepies
        subroutine array_copy(src,dest,n_copied,n_not_copied)
            T, dimension(:), intent(in) :: src
            T, dimension(:), intent(out) :: dest
            integer, intent(out) :: n_copied, n_not_copied
            n_copied = min(size(src), size(dest))
            n_not_copied = size(src)-n_copied
            dest(1:n_copied)=src(1:n_copied)
        end subroutine
    end template
    
    template swap
        replace :: T => numeric
    contains
        !> @brief   swap two values without a mask
        subroutine swap(a,b)
            T, intent(inout) :: a,b
            T :: temp
            temp = a
            a = b
            b = temp
        end subroutine

        !> @brief   swap two values with a mask
        !> @details the swap is only performed if mask is true
        subroutine swap_mask(a,b,mask)
            T, intent(inout) :: a,b
            logical, intent(IN) :: mask
            T :: temp
            if (mask) then
                temp = a
                a = b
                b = temp
            end if
        end subroutine
    end template
end module
