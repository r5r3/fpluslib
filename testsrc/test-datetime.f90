program test_datetime
    use mod_datetime
    use mod_map
    implicit none

    type(datetime) :: now
    type(map) :: dates
    character(len=:), allocatable :: text
    integer :: i
    
    now = datetime()

    print*, "Date and time: ", now%to_string()
    print*, "hashcode:      ", now%hashcode()
    
    ! put the date into a map
    dates = map(10)
    call dates%add(now, 1, .true.)

    ! wait some time
    call sleep(5)

    now = datetime()

    print*, "Date and time: ", now%to_string()
    print*, "hashcode:      ", now%hashcode()
    call dates%add(now, 2, .true.)

    ! loop to check to_string for memory leaks
    do i = 1, 10000
        text = now%to_string()
    end do
    
    call dates%printContent()
end program test_datetime
