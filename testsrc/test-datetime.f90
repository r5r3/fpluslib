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
    call sleep(1)

    now = datetime()

    print*, "Date and time: ", now%to_string()
    print*, "hashcode:      ", now%hashcode()
    call dates%add(now, 2, .true.)
    print*, ""

    ! add some time
    print*, "Add one month..."
    call now%add(month=1)
    print*, "Date and time: ", now%to_string()
    print*, "hashcode:      ", now%hashcode()
    call dates%add(now, 3, .true.)
    print*, ""

    print*, "Substract 17 months and add 100 days..."
    call now%add(month=-17, day=100)
    print*, "Date and time: ", now%to_string()
    print*, "hashcode:      ", now%hashcode()
    call dates%add(now, 4, .true.)
    print*, ""
    
    print*, "Substract 60 minutes and add 1 year..."
    call now%add(year=1, minute=-60)
    print*, "Date and time: ", now%to_string()
    print*, "hashcode:      ", now%hashcode()
    call dates%add(now, 5, .true.)
    print*, ""

    ! create a new date with a given value
    now = datetime(hour=12, second=56)
    print*, "Date and time: ", now%to_string()
    print*, "hashcode:      ", now%hashcode()
    call dates%add(now, 6, .true.)
    print*, ""

    call dates%printContent()
end program test_datetime
