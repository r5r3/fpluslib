program test_filter
    use fplus_filter
    implicit none

    ! local variables
    integer :: i, f
    real (kind=8) :: pi

    ! array for test-data
    integer, parameter :: n = 10000
    real (kind=8), dimension(n) :: tdata 



    ! fill the array with data
    print*, "4th Order Butterworth filter with cutoff periods 100 and 200:"
    pi = ACOS(-1.)
    do f = 1, 100
        do i = 1, n
            tdata(i) = sin(pi/2*i/real(f))
        end do
        ! perform the filtering
        call BWBPASS(tdata,n,100.0_8,200.0_8,2)
        print*, "Period: ", f*4, "Sum: ", sum(abs(tdata))
    end do

end program