program test_list
    use mod_container
    implicit none
    ! a variable for a new list
    class(list), pointer :: testlist
    class(*), pointer :: testvalue
    class(iterator), pointer :: testiter
    integer :: i
    real (kind=8) :: testnumber

    ! create a new list
    testlist => list()

    ! add some elements
    print*, ""
    print*, "Some elements are added to the list, the last is an integer (kind=8)"
    print*, ""
    call testlist%add(1)
    call testlist%add(2)
    call testlist%add(4)
    call testlist%add(7)
    call testlist%add(9)
    call testlist%add(3_8)


    ! get the first and the last value with the corresponding functions
    print*, "Show the first and the last element"
    print*, "The first element:", toInteger(testlist%first())
    print*, "The last element: ", toIntegerK8(testlist%last())
    print*, ""



    print*, "Use the slow get function to iterate through the list"
    do i = 0, testlist%length() +1
        testvalue => testlist%get(i)
        if (.not.associated(testvalue)) then
            print*, "No value at position ", i
        else
            print*, "The value at position", i, toInteger(testvalue)
        end if
    end do
    print*, ""



    print*, "Use an fast iterator to iterate through the list"
    print*, "-> at first forwards"
    testiter => testlist%getiterator()
    do while(testiter%hasnext())
        testvalue => testiter%next()
        print*, "Next value: ", toInteger(testvalue)
    end do
    deallocate(testiter)
    print*, ""

    print*, "-> then backwards"
    testiter => testlist%getiterator(1)
    do while(testiter%hasnext())
        testvalue => testiter%next()
        print*, "Next value: ", toInteger(testvalue)
    end do
    deallocate(testiter)
    print*, ""

    ! remove all elements
    call testlist%clear()

    ! create random numbers in a loop
    do i = 1, 5
        call random_number(testnumber)
        call testlist%add(testnumber)
        call testlist%add(testnumber, copy=.true.)
    end do
    print*, "Five random numbers, once added as pointers, once added as copy"
    testiter => testlist%getiterator()
    do while(testiter%hasnext())
        testvalue => testiter%next()
        print*, "Next element: ", toRealK8(testvalue)
    end do
    deallocate(testiter)
    print*, ""

    ! remove all elements before deallocation, finalization is not yet supported!
    call testlist%clear()
    deallocate(testlist)

end program test_list
