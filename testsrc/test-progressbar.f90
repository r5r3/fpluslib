program test_progressbar
    use fplus_progressbar
    implicit none

    type(progressbar) :: pb
    integer :: i

    pb = new_progressbar(20, 50, "test", .true.)
    do i = 1, 20
        call sleep(1)
        call pb%update(i, overwrite=.true.)
    end do

end program
