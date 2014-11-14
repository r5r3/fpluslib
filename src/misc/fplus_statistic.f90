!> @brief       A collection of statistical functions
!> @author      Robert Schuster
!> @details     all functions are only implemented for real(kind=8) values
module fplus_statistic
    use fplus_fillvalue
    use fplus_array_tools
    implicit none
    private
    
    ! list of public functions
    public :: cumulative_distribution_function, invers_cumulative_distribution_function
    
    ! cdf for 1d and 2d arrays
    interface cumulative_distribution_function
        module procedure cumulative_distribution_function_1d
        module procedure cumulative_distribution_function_2d
    end interface
    
    ! invers cdf for 1d and 2d arrays
    interface invers_cumulative_distribution_function
        module procedure invers_cumulative_distribution_function_1d
        module procedure invers_cumulative_distribution_function_2d
    end interface
    
contains

    ! The cumulative density function =============================================================

    !> @brief           Evaluates the cumulative distribution function at one point
    !> @param[in]       array   an array of values
    !> @paran[in]       value   the value for which the cdf sould be evaluated
    !> @returns         the fraction of the array with values smaller than the value argument
    real function cumulative_distribution_function_1d(array, value) result (res)
        real (kind=8), dimension(:), intent(in) :: array
        real (kind=8), intent(in) :: value
        res = cumulative_distribution_function_intern(array, size(array), value)
    end function

    !> @brief           Evaluates the cumulative distribution function at one point
    !> @param[in]       array   an array of values
    !> @paran[in]       value   the value for which the cdf sould be evaluated
    !> @returns         the fraction of the array with values smaller than the value argument
    real function cumulative_distribution_function_2d(array, value) result (res)
        real (kind=8), dimension(:,:), intent(in) :: array
        real (kind=8), intent(in) :: value
        res = cumulative_distribution_function_intern(array, product(shape(array)), value)        
    end function

    !> @brief           Evaluates the cumulative distribution function at one point
    !> @param[in]       array   an array of values
    !> @paran[in]       value   the value for which the cdf sould be evaluated
    !> @returns         the fraction of the array with values smaller than the value argument
    real function cumulative_distribution_function_intern(array, n, value) result (res)
        real (kind=8), dimension(*), intent(in) :: array
        integer :: n
        real (kind=8), intent(in) :: value
        
        !local variables
        integer :: i
        
        ! loop over all array elements
        res = 0.0_8
        do i = 1, n
            if (array(i) <= value) res = res + 1.0_8
        end do
        res = res / real(n, 8)
    end function

    
    
    ! The invers cumulative density function ======================================================
    
    !> @brief           Find the value in the array with a cdf as close as possible to the specified value
    !> @param[in]       array   an array of values
    !> @paran[in]       value   the value for which the cdf sould be evaluated
    real(kind=8) function invers_cumulative_distribution_function_1d(array, value) result (res)
        real (kind=8), dimension(:), intent(in) :: array
        real (kind=8), intent(in) :: value
        res = invers_cumulative_distribution_function_intern(array, size(array), value)
    end function

    !> @brief           Find the value in the array with a cdf as close as possible to the specified value
    !> @param[in]       array   an array of values
    !> @paran[in]       value   the value for which the cdf sould be evaluated
    real(kind=8) function invers_cumulative_distribution_function_2d(array, value) result (res)
        real (kind=8), dimension(:,:), intent(in) :: array
        real (kind=8), intent(in) :: value
        res = invers_cumulative_distribution_function_intern(array, product(shape(array)), value)        
    end function

    !> @brief           Find the value in the array with a cdf as close as possible to the specified value
    !> @param[in]       array   an array of values
    !> @paran[in]       value   the value for which the cdf sould be evaluated
    real(kind=8) recursive function invers_cumulative_distribution_function_intern(array, n, value, lb, ub) result (res)
        real (kind=8), dimension(*), intent(in) :: array
        integer :: n
        real (kind=8), intent(in) :: value
        integer, optional :: lb, ub 
        
        !local variables
        integer :: i, ilb, iub, i2, mid
        real (kind=8) :: amin, amax, cdf1, cdf2
        real (kind=8), dimension(:), allocatable, save :: temp
        integer, save :: level
        
        ! the bound are needed for the recursive algorithm 
        if (present(lb)) then
            ilb = lb
        else
            ilb = 1
        end if
        if (present(ub)) then
            iub = ub
        else
            iub = n
        end if
        
        ! find min and max of the array
        if (.not.present(lb) .and. .not.present(ub)) then
            amin = minval(array(1:n))
            amax = maxval(array(1:n))
            ! is the value within the range of values
            if (value >= 1.0_8) then
                res = amax
                return
            end if
            if (value <= 0.0_8) then
                res = amin
                return
            end if
        end if 
        
        ! create an temporal copy of the array and sort it
        if (.not. allocated(temp)) then
            temp = array(1:n)
            call qsort(temp)
            level = 1
        else
            level = level +1
        end if
        
        ! devide the array into two parts
        if (iub - ilb > 2) then
            mid = (ilb + iub) / 2
            cdf1 = cumulative_distribution_function(temp, temp(mid))
            ! we found the search value 
            if (cdf1 == value) then
                res = temp(mid)
                level = level - 1
                return
            end if
            ! take a look at the lower half of the array 
            if (cdf1 > value) then
                res = invers_cumulative_distribution_function_intern(array, n, value, ilb, mid)
            end if
            ! take a look at the upper half of the array
            if (cdf1 < value) then
                res = invers_cumulative_distribution_function_intern(array, n, value, mid, iub)
            end if
        else
        ! loop over all array elements
            cdf1 = fplus_fill_realk8
            do i = ilb, iub
                ! calculate the cdf for the current array element
                cdf2 = cumulative_distribution_function(temp, temp(i))
                !write(0,*) cdf2
                if (abs(cdf1-value) > abs(cdf2-value)) then 
                    cdf1 = cdf2
                    i2 = i
                end if
            end do
            res = temp(i2)
        end if
        
        ! reduce the level and deallocate the temporal array 
        level = level - 1
        if (level == 0 .and. allocated(temp)) deallocate(temp)      
    end function

end module
