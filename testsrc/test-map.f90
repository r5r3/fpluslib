program test_map
    use mod_map
    implicit none

    ! a variable for the new map
    class(map), pointer :: testmap
    character (len=50), allocatable :: key, value
    real (kind=8) :: k2, v2

    allocate(key)
    allocate(value)

    ! create the map
    testmap => map()

    ! create key and value
    key = "the key"
    value = "the value"

    ! add is to the map
    k2 = 1.1
    v2 = 2.2
    call testmap%add(k2, v2, .true.)
    call testmap%add(key, value, .true.)

end program test_map
