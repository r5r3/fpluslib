program test_list
    use fplus_container
    implicit none
    ! a variable for a new list
    type(list) :: testlist
    class(*), pointer :: testvalue
    type(listiterator) :: testiter
    integer :: i
    real (kind=8) :: testnumber

    ! create a new list
    testlist = new_list()

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
    print*, "The first element:", to_integer(testlist%first())
    print*, "The last element: ", to_integer8(testlist%last())
    print*, ""



    print*, "Use the slow get function to iterate through the list"
    do i = 0, testlist%length() +1
        testvalue => testlist%get(i)
        if (.not.associated(testvalue)) then
            print*, "No value at position ", i
        else
            print*, "The value at position", i, to_integer(testvalue)
        end if
    end do
    print*, ""



    print*, "Use an fast iterator to iterate through the list"
    print*, "-> at first forwards"
    testiter = testlist%get_iterator()
    do while(testiter%hasnext())
        testvalue => testiter%next()
        print*, "Next value: ", to_integer(testvalue)
    end do
    print*, ""

    print*, "-> then backwards"
    testiter = testlist%get_iterator(1)
    do while(testiter%hasnext())
        testvalue => testiter%next()
        print*, "Next value: ", to_integer(testvalue)
    end do
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
    testiter = testlist%get_iterator()
    do while(testiter%hasnext())
        testvalue => testiter%next()
        print*, "Next element: ", to_real8(testvalue)
    end do
    print*, ""

    ! test of the to_string method
    print*, testlist%to_string()
    print*, "Hashcode:", testlist%hashcode()

    ! remove all elements before deallocation, finalization is not yet supported!
    print*, ""
    print*, "remove all elements"
    print*, ""
    call testlist%clear()

    ! test of the to_string method
    print*, testlist%to_string()
    print*, "Hashcode:", testlist%hashcode()


end program test_list
