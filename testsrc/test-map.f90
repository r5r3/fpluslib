program test_map
    use mod_map
    implicit none

    ! a variable for the new map
    class(map), pointer :: testmap
    character (len=50), allocatable :: key, value
    real (kind=8) :: v2
    integer :: i

    allocate(key)
    allocate(value)

    ! create the map
    testmap => map(7)

    ! create key and value
    key = "the key ."
    value = "the value"

    ! add is to the map
    call testmap%printContent()
    v2 = 2.2
    call testmap%add(1.1, v2, .true.)
    call testmap%printContent()

    call testmap%add(0.1, v2, .true.)
    call testmap%printContent()

    call testmap%add(1.1, v2, .true.)
    call testmap%printContent()

    call testmap%add(2_8, v2, .true.)
    call testmap%printContent()

    call testmap%add(2, v2, .true.)
    call testmap%printContent()

    call testmap%add(7.3, v2, .true.)
    call testmap%printContent()

    ! any memory holes?
    do i = 1, 10000
        call testmap%add(22_8, v2, .true.)
    end do

    ! clean up
    call testmap%printContent()
    call testmap%removeAll()
    call testmap%printContent()


end program test_map
