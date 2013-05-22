program test_map
    use mod_map
    implicit none

    ! a variable for the new map
    class(map), pointer :: testmap
    character (len=50), allocatable :: key, value
    real (kind=8) :: k2, v2
    integer :: i

    allocate(key)
    allocate(value)

    ! create the map
    testmap => map(7)

    ! create key and value
    key = "the key"
    value = "the value"

    ! add is to the map
    call testmap%printContent()
    v2 = 2.2
    k2 = 1.1
    call testmap%add(k2, v2, .true.)
    call testmap%printContent()

    k2 = 0.1
    call testmap%add(k2, v2, .true.)
    call testmap%printContent()

    k2 = 1.1
    call testmap%add(k2, v2, .true.)
    call testmap%printContent()

    k2 = 0.2
    call testmap%add(k2, v2, .true.)
    call testmap%printContent()

    k2 = 0.1
    call testmap%add(k2, v2, .true.)
    call testmap%printContent()

    k2 = -1.3
    call testmap%add(k2, v2, .true.)
    call testmap%printContent()

    ! any memory holes?
    do i = 1, 10000
        k2 = 0.1
        call testmap%add(k2, v2, .true.)
    end do

    ! clean up
    call testmap%printContent()
    call testmap%removeAll()
    call testmap%printContent()


end program test_map
