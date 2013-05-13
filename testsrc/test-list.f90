program test_list
    use mod_list
    implicit none
    ! a variable for a new list
    class(list), pointer :: testlist
    class(element), pointer :: testelement
    class(iterator), pointer :: testiter
    integer :: i

    ! create a new list
    testlist => list()

    ! add two elements
    call testlist%add(1.2)
    call testlist%add(1.3)
    call testlist%add(1.5)
    call testlist%add(1.7)
    call testlist%add(1.8)
    call testlist%add(2.7)

    print*, "Show the first and the last element"
    print*, "The first element:", testlist%firstElement%getReal()
    print*, "The last element: ", testlist%lastElement%getReal()
    print*, ""

    print*, "Use the slow getElementAt function to iterate through the list"
    do i = 0, testlist%length +1
        testelement => testlist%getElementAt(i)
        if (.not.associated(testelement)) then
            print*, "No element at position ", i
        else
            print*, "The element at position", i, testelement%getReal()
        end if
    end do
    print*, ""

    print*, "Use an fast iterator to iterate through the list"
    print*, "-> at first forwards"
    testiter => testlist%getIterator()
    do while(testiter%hasNext())
        testelement => testiter%getNext()
        print*, "Next element: ", testelement%getReal()
    end do
    deallocate(testiter)
    print*, ""

    print*, "-> then backwards"
    testiter => testlist%getIterator(1)
    do while(testiter%hasNext())
        testelement => testiter%getNext()
        print*, "Next element: ", testelement%getReal()
    end do
    deallocate(testiter)
    print*, ""

    ! free the memory, finalize is not called automatically!
    call testlist%finalize()
    deallocate(testlist)

end program test_list
