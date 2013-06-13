!> @brief       A test for the calculation of the cumulative distribution function
program test_cdf
    use fplus_statistic
    use fplus_strings
    implicit none
    
    ! some example data array
    real (kind=8), dimension(:), allocatable :: a
    integer :: i
    real (kind=8) :: b, c
    a = (/9,8,7,6,5,4,3,1,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,2,2,2,4,4,5,7,8,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8/)

    do i = 1, 9
        b = cumulative_distribution_function(a, real(i,8))
        c = invers_cumulative_distribution_function(a, b)
        write(*, "(A,I1,A,F4.2,A,F4.2,A,I1)") "cdf(", i, ") = ", b, "  =>  cdf^-1(", b, ") = ", int(c)   
    end do
    
end program