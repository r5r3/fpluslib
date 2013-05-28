!> @brief   An implementation of a double-linked list
!> @author  Robert Schuster
module mod_list
    use mod_iterator
    implicit none
    private

    ! a type for the linked list itself
    type, public :: list
        ! variables
        class(element), pointer, private :: firstElement
        class(element), pointer, private :: lastElement
        integer, private :: nelements
        ! procedures
    contains
        !> @brief   Appends the specified value to the end of this list.
        procedure, public :: add => list_add
        !> @brief   Returns the value at the specified position in this list.
        procedure, public :: get => list_get
        !> @brief   Returns the first value in the list.
        procedure, public :: first => list_first
        !> @brief   Returns the first value in the list.
        procedure, public :: last => list_last
        !> @brief   Returns an iterator over the values in this list in proper sequence.
        procedure, public :: getiterator => list_getiterator
        !> @brief   Removes all values from this list and deallocates all internal used memory
        procedure, public :: clear => list_clear
        !> @brief   Returns the number of elements in this list
        procedure, public :: length => list_length
    end type
    ! define the constructor for list
    interface list
        module procedure constructor_list
    end interface


    ! a iterator for all elements of a list
    type, public, extends (iterator) :: listiterator
        class(list), pointer, private :: thelist
        class(element), pointer, private :: currentElement
        integer, private :: direction
    contains
        procedure, public :: hasnext => listiterator_hasnext
        procedure, public :: next => listiterator_next
        procedure, private :: next_element => listiterator_nextelement
    end type
    
    
    ! a type for the elements
    type, private :: element
        class(*), pointer :: value
        class(element), pointer :: nextElement
        class(element), pointer :: prevElement
        logical :: valueIsCopy
    end type
    ! define the constructor for list
    interface element
        module procedure constructor_element
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
        constructor_list%nelements = 0
    end function

    !> @public
    !> @brief       Appends the specified value to the end of this list.
    !> @details     The specified value is added to the end of the list. It is possible to add a link
    !>              or a complete copy of the value.
    !> @param[in]   this    reference to the map object, automatically set by fortran
    !> @param[in]   value   value to be added
    !> @param[in]   copy    if present and .true., the value will be copied and not linked. The
    !>                      default operation is to store a pointer to the value.
    subroutine list_add(this, value, copy)
        class(list) :: this
        class(*), target :: value
        logical, optional :: copy
        logical :: copyValue

        ! copy or link?
        if (present(copy)) then
            copyValue = copy
        else
            copyValue = .false.
        end if

        ! create a new element
        if (this%nelements == 0) then
            this%firstElement => constructor_element(value, null(), null(), copyValue)
            this%lastElement => this%firstElement
        else
            this%lastElement%nextElement => constructor_element(value, this%lastElement, null(), copyValue)
            this%lastElement => this%lastElement%nextElement
        end if
        ! count the elements
        this%nelements = this%nelements + 1
    end subroutine

    !> @public
    !> @brief       Returns the value at the specified position in this list.
    !> @details     Returns the value at a given index, this function provides array-like
    !>              behavior of the list, but it is slow.
    !> @param[in]   this    reference to the map object, automatically set by fortran
    !> @param[in]   ind     the index of the searched value.
    !> @return      Returns the value if the index is in the list, otherwise a null-pointer.
    function list_get(this, ind)
        class(list) :: this
        integer :: ind
        class(*), pointer :: list_get
        class(element), pointer :: nextele
        integer :: i

        ! check if the index is valid
        if (ind > this%nelements .or. ind <= 0) then
            list_get => null()
            return
        end if

        ! check if ind is the first or the last index
        if (ind == 1) then
            list_get => this%firstElement%value
            return
        end if
        if (ind == this%nelements) then
            list_get => this%lastElement%value
            return
        end if

        ! find the element at the given position
        nextele => this%firstElement
        do i = 2, ind
            nextele => nextele%nextElement
        end do
        list_get => nextele%value
    end function

    !> @public
    !> @brief       Returns the first value in the list
    !> @details     Returns the first value in the list, this function is fast.
    !> @param[in]   this    reference to the map object, automatically set by fortran
    !> @return      Returns the first value if the list is not empty, otherwise a null-pointer.
    function list_first(this)
        class(list) :: this
        class(*), pointer :: list_first

        ! check if ind is the first or the last index
        if (this%nelements >= 1) then
            list_first => this%firstElement%value
            return
        else
            list_first => null()
        end if
    end function

    !> @public
    !> @brief       Returns the last value in the list
    !> @details     Returns the last value in the list, this function is fast.
    !> @param[in]   this    reference to the list object, automatically set by fortran
    !> @return      Returns the last value if the list is not empty, otherwise a null-pointer.
    function list_last(this)
        class(list) :: this
        class(*), pointer :: list_last

        ! check if ind is the first or the last index
        if (this%nelements >= 1) then
            list_last => this%lastElement%value
            return
        else
            list_last => null()
        end if
    end function

    !> @public
    !> @brief       Returns an iterator over the values in this list in proper sequence.
    !> @details     Returns an iterator that can be used to iterate over all values in this
    !>              list in forward or backward direction
    !> @param[in]   this    reference to the list object, automatically set by fortran
    !> @param[in]   dir     the direction in which the iterator should run. 0=forwards, 1=backwards.
    !> @return      Returns an initialized iterator object. It is the only way to create an
    !>              iterator object.
    function list_getiterator(this, dir)
        class(list), target :: this
        integer, optional :: dir
        class(listiterator), pointer :: list_getiterator

        ! create the new iterator
        allocate(list_getiterator)

        ! set the settings of the iterator
        list_getiterator%thelist => this
        if (present(dir) .and. dir == 1) then
            ! go backwards through the list
            list_getiterator%direction = 1
            list_getiterator%currentElement => this%lastElement
        else
            ! go foreward through the list
            list_getiterator%direction = 0
            list_getiterator%currentElement => this%firstElement
        end if
    end function

    !> @public
    !> @brief       Removes all values from this list and deallocates all internal used memory
    !> @details     This subroutine completely resets the list to its initial state and frees up all
    !>              the allocated memory. Call is before deallocation of a list object to ensure that
    !>              no memory is leaked.
    !> @param[in]   this    reference to the list object, automatically set by fortran
    subroutine list_clear(this)
        class(list) :: this
        class(listiterator), pointer :: iter
        class(element), pointer :: elem

        ! are the elements in the list?
        if (this%nelements > 0) then
            iter => this%getiterator()
            do while(iter%hasnext())
                elem => iter%next_element()
                ! deallocate the value, if it is a copy
                if (elem%valueIsCopy) then
                    deallocate(elem%value)
                end if
                deallocate(elem)
            end do
            deallocate(iter)

            ! set the pointers back to null
            this%firstElement => null()
            this%lastElement => null()
            this%nelements = 0
        end if
    end subroutine

    !> @public
    !> @brief   Returns the number of elements in this list
    !> @return  number of elements
    integer function list_length(this)
        class(list) :: this
        list_length = this%nelements
    end function

    ! then the procedures for the iterator ------------------------------------

    ! check if we are able to get an additional element
    function listiterator_hasnext(this)
        class(listiterator) :: this
        logical :: listiterator_hasnext
        if (associated(this%currentElement)) then
            listiterator_hasnext = .true.
        else
            listiterator_hasnext = .false.
        end if
    end function

    ! get the next element
    function listiterator_next(this)
        class(listiterator) :: this
        class(*), pointer :: listiterator_next
        class(element), pointer :: nextelement

        ! assign null to the result
        listiterator_next => null()

        ! get the next element object
        nextelement => this%next_element()

        ! assign the result
        if (associated(nextelement)) then
            listiterator_next => nextelement%value
        end if
    end function

    ! get the next element
    function listiterator_nextelement(this)
        class(listiterator) :: this
        class(element), pointer :: listiterator_nextelement

        ! assign null to the result
        listiterator_nextelement => null()

        if (this%hasnext()) then
            listiterator_nextelement => this%currentElement
            ! go to the next element
            if (this%direction == 0 .and. associated(this%currentElement%nextElement)) then
                this%currentElement => this%currentElement%nextElement
            else if (this%direction == 1 .and. associated(this%currentElement%prevElement)) then
                this%currentElement => this%currentElement%prevElement
            else
                this%currentElement => null()
            end if
        end if
    end function

    ! then the procedures for the elements ------------------------------------

    ! a constructor for the element
    function constructor_element(value, prev, next, copy)
        class(*), target :: value
        class(element), pointer :: next, prev
        class(element), pointer :: constructor_element
        logical, optional :: copy

        ! allocate memory for the new list
        allocate(constructor_element)
        ! initialize the pointers
        constructor_element%nextElement => next
        constructor_element%prevElement => prev
        ! copy the value or create a pointer to it
        if (present(copy) .and. copy .eqv. .true.) then
            allocate(constructor_element%value, source=value)
            constructor_element%valueIsCopy = .true.
        else
            constructor_element%value => value
            constructor_element%valueIsCopy = .false.
        end if
    end function

end module mod_list
