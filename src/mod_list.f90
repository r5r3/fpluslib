module mod_list
    implicit none
    ! this module implements a linked list

    ! a type for the linked list itself
    type list
        ! variables
        class(element), pointer :: firstElement
        class(element), pointer :: lastElement
        integer :: length
        ! procedures
        contains
        procedure add                       ! add an element to the end of the list
        procedure getElementAt              ! returns an element of the list with a given position
        procedure getIterator               ! returns a iterator over all elements
        ! deallocate all elements
        procedure :: finalize => finalize_list
    end type
    ! define the constructor for list
    interface list
        procedure constructor_list
    end interface


    ! a iterator for all elements of a list
    type iterator
        class(list), pointer :: thelist
        class(element), pointer :: currentElement
        integer :: direction
        contains
        procedure hasNext
        procedure getNext
    end type


    ! a type for the elements
    type element
        class(*), pointer :: value
        class(element), pointer :: nextElement
        class(element), pointer :: prevElement
        ! procedures
        contains
        procedure getReal       ! return the value as real
    end type
    ! define the constructor for list
    interface element
        procedure constructor_element
    end interface


    ! implementation of the procedures
    contains

    ! at first the procedures for the list ------------------------------------

    ! a constructor for the list
    function constructor_list()
        class(list), pointer :: constructor_list
        ! allocate memory for the new list
        allocate(constructor_list)
        ! ensure that all pointers point to null
        constructor_list%firstElement => null()
        constructor_list%lastElement => null()
        constructor_list%length = 0
    end function

    ! add an element to the end of the list
    subroutine add(this, value)
        class(list) :: this
        class(*), target :: value

        ! create a new element
        if (this%length == 0) then
            this%firstElement => constructor_element(value, null(), null())
            this%lastElement => this%firstElement
        else
            this%lastElement%nextElement => constructor_element(value, this%lastElement, null())
            this%lastElement => this%lastElement%nextElement
        end if
        ! count the elements
        this%length = this%length + 1
    end subroutine

    ! get an element at a specified position within the list
    function getElementAt(this, ind)
        class(list) :: this
        integer :: ind
        class(element), pointer :: getElementAt
        integer :: i

        ! check if the index is valid
        if (ind > this%length .or. ind <= 0) then
            getElementAt => null()
            return
        end if

        ! check if ind is the first or the last index
        if (ind == 1) then
            getElementAt => this%firstElement
            return
        end if
        if (ind == this%length) then
            getElementAt => this%lastElement
            return
        end if

        ! find the element at the given position
        getElementAt => this%firstElement
        do i = 2, ind
            getElementAt => getElementAt%nextElement
        end do
    end function

    ! get an iterator over all elements
    function getIterator(this, dir)
        class(list), target :: this
        integer, optional :: dir
        class(iterator), pointer :: getIterator

        ! create the new iterator
        allocate(getIterator)

        ! set the settings of the iterator
        getIterator%thelist => this
        if (present(dir) .and. dir == 1) then
            ! go backwards through the list
            getIterator%direction = 1
            getIterator%currentElement => this%lastElement
        else
            ! go foreward through the list
            getIterator%direction = 0
            getIterator%currentElement => this%firstElement
        end if
    end function

    ! finalize the list, clean up the memory
    subroutine finalize_list(this)
        class(list) :: this
        class(iterator), pointer :: iter
        class(element), pointer :: elem

        ! are the elements in the list?
        if (this%length > 0) then
            iter => this%getIterator()
            do while(iter%hasNext())
                elem => iter%getNext()
                deallocate(elem)
            end do
            deallocate(iter)
        end if
    end subroutine

    ! then the procedures for the iterator ------------------------------------

    ! check if we are able to get an additional element
    function hasNext(this)
        class(iterator) :: this
        logical :: hasNext
        if (associated(this%currentElement)) then
            hasNext = .true.
        else
            hasNext = .false.
        end if
    end function

    ! get the next element
    function getNext(this)
        class(iterator) :: this
        class(element), pointer :: getNext

        ! assign null to the result
        getNext => null()

        if (this%hasNext()) then
            getNext => this%currentElement
            ! go to the next element
            if (this%direction == 0 .and. associated(getNext%nextElement)) then
                this%currentElement => getNext%nextElement
            else if (this%direction == 1 .and. associated(getNext%prevElement)) then
                this%currentElement => getNext%prevElement
            else
                this%currentElement => null()
            end if
        end if
    end function

    ! then the procedures for the elements ------------------------------------

    ! a constructor for the element
    function constructor_element(value, prev, next)
        class(*), target :: value
        class(element), pointer :: next, prev
        class(element), pointer :: constructor_element

        ! allocate memory for the new list
        allocate(constructor_element)
        ! initialize the pointers
        constructor_element%nextElement => next
        constructor_element%prevElement => prev
        constructor_element%value => value
    end function

    ! return the value stored in the element as real
    function getReal(this)
        class(element) :: this
        real :: getReal
        ! create a pointer to the value, the associate block in gfortran 4.8 don't works
        class(*), pointer :: value
        value => this%value
        ! check the type
        select type (value)
            type is (real)
                getReal = value
            class default
                print*, "ERROR: wrong element type in element%getReal"
                call exit(1)
        end select
        ! free the pointer
        nullify(value)
    end function


end module mod_list
