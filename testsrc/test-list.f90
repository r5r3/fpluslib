program test_list
    use mod_list
    implicit none
    ! a variable for a new list
    class(list), pointer :: testlist
    class(element), pointer :: testelement
    class(iterator), pointer :: testiter
    integer :: i
    real (kind=8) :: testnumber

    ! create a new list
    testlist => list()

    ! add two elements
    print*, ""
    print*, "Some elements are added to the list, the last is an integer (kind=8)"
    print*, ""
    call testlist%add(1)
    call testlist%add(2)
    call testlist%add(4)
    call testlist%add(7)
    call testlist%add(9)
    call testlist%add(3_8)

    print*, "Show the first and the last element"
    print*, "The first element:", testlist%firstElement%getInteger()
    print*, "The last element: ", testlist%lastElement%getIntegerK8()
    print*, ""

    print*, "Use the slow getElementAt function to iterate through the list"
    do i = 0, testlist%length +1
        testelement => testlist%getElementAt(i)
        if (.not.associated(testelement)) then
            print*, "No element at position ", i
        else
            print*, "The element at position", i, testelement%getInteger()
        end if
    end do
    print*, ""

    print*, "Use an fast iterator to iterate through the list"
    print*, "-> at first forwards"
    testiter => testlist%getIterator()
    do while(testiter%hasNext())
        testelement => testiter%getNext()
        print*, "Next element: ", testelement%getInteger()
    end do
    deallocate(testiter)
    print*, ""

    print*, "-> then backwards"
    testiter => testlist%getIterator(1)
    do while(testiter%hasNext())
        testelement => testiter%getNext()
        print*, "Next element: ", testelement%getInteger()
    end do
    deallocate(testiter)
    print*, ""

    ! remove all elements
    call testlist%removeAll()

    ! create random numbers in a loop
    do i = 1, 5
        call random_number(testnumber)
        call testlist%add(testnumber)
        call testlist%add(testnumber, copy=.true.)
    end do
    print*, "Five random numbers, once added as pointers, once added as copy"
    testiter => testlist%getIterator()
    do while(testiter%hasNext())
        testelement => testiter%getNext()
        print*, "Next element: ", testelement%getRealK8()
    end do
    deallocate(testiter)
    print*, ""

    ! remove all elements before deallocation, finalization is not yet supported!
    call testlist%removeAll()
    deallocate(testlist)

end program test_list
