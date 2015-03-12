program test_progressbar
    use fplus_progressbar
    implicit none

    type(progressbar) :: pb
    integer :: i

    pb = new_progressbar(86420, 50, "test", .true.)
    do i = 1, 86420
        call sleep(1)
        call pb%update(i, overwrite=.true.)
    end do

end program
