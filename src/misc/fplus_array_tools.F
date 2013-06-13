!> @brief   some useful procedures for fortran arrays
module fplus_array_tools
    use fplus_fillvalue
    private

    ! list of public procedures
    public :: valueindex, swap, array_copy, qsort

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
