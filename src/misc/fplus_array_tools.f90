!> @brief   some useful procedures for fortran arrays
module fplus_array_tools
    use fplus_fillvalue
    public

contains
    
    !> @brief       Find the location of a value in an array
    !> @param[in]   array       the array to search in
    !> @param[in]   value       the value to serach for
    !> @param[in]   lowerbound  dont't look at locations below this value
    !> @param[in]   upperbound  dont't look at locations above this value
    !> @returns     the index of the value or fplus_fill_int if nothing was found
    recursive function valueindex(array, value, lowerbound, upperbound, sorted) result(res)
        real (kind=8), dimension(:), intent(in) :: array
        real (kind=8), intent(in) :: value
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
end module
