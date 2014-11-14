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

end module
