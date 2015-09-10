!> @brief   additional usefull functions for fortran arrays
module fplus_array_tools_2
    use fplus_error
    implicit none

    !$FP interface template flip
    !$FP interface template flip_2d
    !$FP interface template qsort
    !$FP interface template convolution2d
    !$FP interface template noise_sigma
    !$FP interface template signal_to_noise_ratio
    !$FP interface template ensure_allocation

contains

    !$FP numeric = {real (kind=4)} {real (kind=8)} {integer (kind=4)} {integer (kind=8)}
    !$FP numeric_kind = {4} {8} {4} {8}

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
            recursive subroutine qsort(array)
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

    !$FP template convolution2d
        !$FP do i = 1, 4
            !> @public
            !> @brief           linear 2d convolution
            !> @param[in]       input   input array
            !> @param[in]       kernel  the kernel to be applied
            !> @param[out]      result  the result array, same size as input
            subroutine convolution2d(input, kernel, result, fill_value)
                ${numeric(i)}, dimension(:,:), intent(in) :: input, kernel
                ${numeric(i)}, dimension(:,:), intent(out) :: result
                ${numeric(i)}, intent(in) :: fill_value

                ! local variables
                integer :: kCols, kRows, kCenterX, kCenterY, i, j, ii, jj, m, n, mm, nn, rows, cols

                ! sizes
                kCols = size(kernel, 1)
                kRows = size(kernel, 2)
                cols = size(input, 1)
                rows = size(input, 2)

                ! find center position of kernel (half of kernel size)
                kCenterX = kCols / 2 + 1
                kCenterY = kRows / 2 + 1

                result = 0
                do i=1, rows                        ! rows
                    col_loop: do j=1, cols                    ! columns
                        do m=1, kRows               ! kernel rows
                            mm = kRows - m + 1      ! row index of flipped kernel
                            do n = 1, kCols         ! kernel columns
                                nn = kCols - n + 1  ! column index of flipped kernel

                                ! index of input signal, used for checking boundary
                                ii = i + (m - kCenterY)
                                jj = j + (n - kCenterX)

                                ! ignore input samples which are out of bound
                                if (ii >= 1 .and. ii <= rows .and. jj >= 1 .and. jj <= cols) then
                                    ! missing value?
                                    if (input(jj,ii) .eq. fill_value) then
                                        result(j,i) = fill_value
                                        cycle col_loop
                                    end if
                                    result(j,i) = result(j,i) + input(jj,ii) * kernel(nn, mm)
                                end if
                            end do
                        end do
                    end do col_loop
                end do
            end subroutine
        !$FP end do
    !$FP end template

    !$FP template noise_sigma
        !$FP do i = 1, 4
            !> @public
            !> @brief           estimate noise level in 2d array based on Fast Noise Variance Estimation (IMMERKÆR 1996)
            !> @param[in]       input   input array
            !> @param[out]      result  sigma, real, precision is precision of input
            subroutine noise_sigma(input, result, fill_value)
                ${numeric(i)}, dimension(:,:), intent(in) :: input
                real (kind=${numeric_kind(i)}), intent(out) :: result
                ${numeric(i)}, intent(in) :: fill_value

                ! local variables
                integer :: i, j, rows, cols
                ${numeric(i)}, dimension(3,3) :: kernel                 ! kernel for convolution
                ${numeric(i)}, dimension(:,:), allocatable :: conv      ! temporal result array for convolution
                real (kind=${numeric_kind(i)}) :: pi, nvalid
                kernel(:,1) = (/1,-2,1/)
                kernel(:,2) = (/-2,4,-2/)
                kernel(:,3) = (/1,-2,1/)

                ! sizes
                cols = size(input, 1)
                rows = size(input, 2)

                ! apply convolution
                allocate(conv(cols, rows))
                call convolution2d(input, kernel, conv, fill_value)

                result = 0
                nvalid = (cols-2)*(rows-2)
                do i=2, rows-1                       ! rows
                    do j=2, cols-1                   ! columns
                        if (conv(j,i) == fill_value) then
                            nvalid = nvalid -1
                            cycle
                        end if
                        result = result + abs(conv(j,i))
                    end do
                end do
                !$FP if (numeric_kind(i) == 8) then
                pi = dacos(-1.0_8)
                !$FP else
                pi = acos(-1.0)
                !$FP end if
                ! do we have enough valid values?
                if (nvalid/(cols-2)*(rows-2) < 0.1) then
                    result = fill_value
                else
                    result = sqrt(pi/2.0) / (6*nvalid)*result
                end if
            end subroutine
        !$FP end do
    !$FP end template

    !$FP template signal_to_noise_ratio
        !$FP do i = 1, 4
            !> @public
            !> @brief           estimate signal to noise ratio in 2d array based on Fast Noise Variance Estimation (IMMERKÆR 1996)
            !> @param[in]       input   input array
            !> @param[out]      result  SNR, real, precision is precision of input
            subroutine signal_to_noise_ratio(input, result, fill_value)
                ${numeric(i)}, dimension(:,:), intent(in) :: input
                real (kind=${numeric_kind(i)}), intent(out) :: result
                ${numeric(i)}, intent(in) :: fill_value

                ! local variables
                integer :: i, j, cols, rows
                real (kind=${numeric_kind(i)}) :: mean, stddev, nvalid, stddev_noise

                ! sizes
                cols = size(input, 1)
                rows = size(input, 2)

                ! first step calculate mean
                mean = 0
                nvalid = 0
                do i=1, rows
                    do j=1, cols
                        if (input(j,i) /= fill_value) then
                            nvalid = nvalid + 1
                            mean = mean + input(j,i)
                        end if
                    end do
                end do
                if (nvalid .eq. 0) then
                    result = fill_value
                    return
                else
                    mean = mean / nvalid
                end if
                ! second step standard deviation
                stddev = 0
                do i=1, rows
                    do j=1, cols
                        if (input(j,i) /= fill_value) then
                            stddev = stddev + (input(j,i) - mean)**2
                        end if
                    end do
                end do
                stddev = sqrt(stddev/nvalid)
                ! third step, estimate standard deviation of noise
                call noise_sigma(input, stddev_noise, fill_value)
                if (stddev_noise .eq. fill_value) then
                    result = fill_value
                    return
                end if
                ! calculate SNR
                result = stddev_noise**2 / stddev**2
            end subroutine
        !$FP end do
    !$FP end template

    !$FP template ensure_allocation
        !$FP dims = {:} {:,:} {:,:,:} {:,:,:,:} {:,:,:,:,:}
        !$FP sspec = {shp(1)} {shp(1),shp(2)} {shp(1),shp(2),shp(3)} {shp(1),shp(2),shp(3),shp(4)} {shp(1),shp(2),shp(3),shp(4),shp(5)}
        !$FP do numtype = 1, 4
        !$FP do ndims = 1, 5
            !> @public
            !> @brief           check the dimensions of an array and re-allocate it, if the dimensions don't match
            !> @param[inout]    input   input array
            !> @param[in]       shape   1d array with the desired shape
            subroutine ensure_allocation(array, shp)
                ${numeric(numtype)}, dimension(${dims(ndims)}), allocatable :: array
                integer, dimension(${ndims}), intent(in) :: shp

                ! local variables
                integer, dimension(${ndims}) :: old_shp

                if (.not.allocated(array)) then
                    allocate(array(${sspec(ndims)}))
                else
                    old_shp = shape(array)
                    if (any(old_shp .ne. shp)) then
                        deallocate(array)
                        allocate(array(${sspec(ndims)}))
                    end if
                end if
            end subroutine
        !$FP end do
        !$FP end do
    !$FP end template
end module
