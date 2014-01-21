program test_map
    use fplus_container
    implicit none

    ! a variable for the new map
    type(map) :: testmap
    character (len=50), allocatable :: key, value
    real (kind=4) :: v2
    real (kind=8) :: v8
    integer :: i
    integer (kind=8) :: i8
    class(*), pointer :: testval

    allocate(key)
    allocate(value)

    ! create the map
    testmap = new_map(2)

    ! show the number of elements in the map
    print*, "Values in the map:", testmap%length()

    ! add is to the map
    call testmap%printContent()
    v2 = 2.2
    call testmap%add(1.1, v2, .true.)
    call testmap%printContent()

    call testmap%add(0.1, v2, .true.)
    call testmap%printContent()

    call testmap%add(1.1, 11.0, .true.)
    call testmap%printContent()

    call testmap%add(2_8, 97, .true.)
    call testmap%printContent()

    call testmap%add(2, 18_8, .true.)
    call testmap%printContent()

    call testmap%add(7.3, 73.0_8, .true.)
    call testmap%printContent()

    ! any memory holes?
    do i = 1, 10000
        call testmap%add(22_8, v2, .true.)
    end do

    ! get some objects from the map
    call testmap%printContent()
    testval => testmap%get(1.1)
    print*, associated(testval)
    v2 = to_real(testval)
    print*, "Key:",1.1,"Value:",v2
    v8 = to_real8(testmap%get(7.3))
    print*, "Key:",7.3,"Value:",v8
    i = to_integer(testmap%get(2_8))
    print*, "Key:",2_8,"Value:",i
    i8 = to_integer8(testmap%get(2))
    print*, "Key:",2,"Value:",i8

    ! remove an element
    call testmap%remove(1.1)
    call testmap%printContent()
    call testmap%remove(22_8)
    call testmap%printContent()
    call testmap%remove(2_8)
    call testmap%printContent()

    ! test of the to_string methode
    print*, testmap%to_string()
    print*, "Hashcode: ", testmap%hashcode()

    ! clean up
    call testmap%clear()
    call testmap%printContent()
    print*, testmap%to_string()


end program test_map
