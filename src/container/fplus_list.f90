!> @brief   An implementation of a double-linked list
!> @author  Robert Schuster
module fplus_list
    use fplus_iterator
    use fplus_object
    use fplus_strings
    use fplus_hashcode
    use fplus_datetime
    use fplus_fillvalue
    use fplus_error
    implicit none
    private

    ! a type for the elements
    type :: element
        class(*), pointer :: value => null()
        class(element), pointer :: nextElement => null()
        class(element), pointer :: prevElement => null()
        logical :: valueIsCopy
    end type


    ! a type for the linked list itself
    type, extends (object), public :: list
        ! variables
        integer, private :: nelements
        class(element), pointer, private :: firstElement => null()
        class(element), pointer, private :: lastElement => null()
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
        procedure, public :: get_iterator => list_get_iterator
        !> @brief   Removes all values from this list and deallocates all internal used memory
        procedure, public :: clear => list_clear
        !> @brief   Returns the number of elements in this list
        procedure, public :: length => list_length
        !> @brief   Returns a string representation of the list
        procedure, public :: to_string => list_to_string
        !> @brief   Calculate a hashcode for the list
        procedure, public :: hashcode => list_hashcode
    end type
    ! make the constructor public
    public :: new_list


    ! a iterator for all elements of a list
    type, public, extends (iterator) :: listiterator
        class(list), pointer, private :: thelist => null()
        class(element), pointer, private :: currentElement => null()
        integer, private :: direction
        ! workaround for ifort bug. we count the number of time
        ! that the nextelement function is called. 
        integer, private :: counter
    contains
        procedure, public :: hasnext => listiterator_hasnext
        procedure, public :: next => listiterator_next
        procedure, private :: next_element => listiterator_nextelement
    end type

    ! some interfaces for the interoperability with arrays
    public :: assignment (=)
    interface assignment (=)
        module procedure list_to_realk8_array
        module procedure list_to_realk4_array
    end interface

! implementation of the procedures
contains

    ! at first the procedures for the list ------------------------------------

    !> @public
    !> @brief   Create a new initialzied list.
    function new_list()
        type(list) :: new_list

        ! ensure that all pointers point to null
        new_list%firstElement => null()
        new_list%lastElement => null()
        new_list%nelements = 0
    end function

    !> @public
    !> @brief       Returns a string representation of the list
    !> @param[in]   this    reference to the list object, automatically set by fortran
    !> @return      Information about the content of the list
    function list_to_string(this) result(res)
        class(list) :: this
        character (len=:), allocatable :: res

        ! local variables
        type(listiterator) :: iter
        character (len=10) :: iformat
        integer :: i
        class(*), pointer :: value

        ! it es a list and has a number of elements
        res = "List, number of elements: " // number_to_string(this%nelements) // char(10)
        
        ! loop over all elements
        if (this%length() > 0) then
            res = res // char(10)
            iter = this%get_iterator()
            i = 1
            write (iformat, "(A,I1,A)") "(I", ndigits_of_integer(this%nelements), ")"
            do while (iter%hasnext())
                value => iter%next()
                res = res // "    " // number_to_string(i, iformat) // ") " // type_to_string(value) // char(10)
                i = i + 1
            end do
        end if
    end function

    !> @public
    !> @brief       calculate a hashcode for the list
    !> @details     this function takes all elements within the list into account. The calculated
    !>              hashcode is the sum of all hashcodes from stored values
    !> @param[in]   this    reference to the list object, automatically set by fortran
    integer (kind=8) function list_hashcode(this) result (res)
        class(list) :: this

        ! local variables
        type(listiterator) :: iter
        integer (kind=8) :: i

        ! the hashcode of the word "list"
        res = calculateHash("list")

        ! loop over all elements
        if (this%length() > 0) then
            iter = this%get_iterator()
            i = 1_8
            do while (iter%hasnext())
                res = res + calculateHash(iter%next()) * i
                i = i + 1_8
            end do
        end if

    end function

    !> @public
    !> @brief       Appends the specified value to the end of this list.
    !> @details     The specified value is added to the end of the list. It is possible to add a link
    !>              or a complete copy of the value.
    !> @param[in]   this    reference to the list object, automatically set by fortran
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
            this%firstElement => new_element(value, null(), null(), copyValue)
            this%lastElement => this%firstElement
        else
            this%lastElement%nextElement => new_element(value, this%lastElement, null(), copyValue)
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
    function list_get_iterator(this, dir)
        class(list), target :: this
        integer, optional :: dir
        type(listiterator) :: list_get_iterator

        ! set the settings of the iterator
        list_get_iterator%thelist => this
        if (present(dir) .and. dir == 1) then
            ! go backwards through the list
            list_get_iterator%direction = 1
            list_get_iterator%currentElement => this%lastElement
        else
            ! go foreward through the list
            list_get_iterator%direction = 0
            list_get_iterator%currentElement => this%firstElement
        end if
        list_get_iterator%counter = 0
    end function

    !> @public
    !> @brief       Removes all values from this list and deallocates all internal used memory
    !> @details     This subroutine completely resets the list to its initial state and frees up all
    !>              the allocated memory. Call is before deallocation of a list object to ensure that
    !>              no memory is leaked.
    !> @param[in]   this    reference to the list object, automatically set by fortran
    subroutine list_clear(this)
        class(list) :: this
        type(listiterator) :: iter
        class(element), pointer :: elem

        ! are the elements in the list?
        if (this%nelements > 0) then
            iter = this%get_iterator()
            do while(iter%hasnext())
                elem => iter%next_element()
                ! deallocate the value, if it is a copy
                if (elem%valueIsCopy) then
                    deallocate(elem%value)
                end if
                deallocate(elem)
            end do

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

    !> @public
    !> @brief   Allow the assignment of a list to an allocatable array of the type real (kind=4)
    subroutine list_to_intk4_array(out_array, in_list)
        integer (kind=4), dimension(:), allocatable, intent(inout) :: out_array
        class (list), intent(in) :: in_list

        ! local variables
        type (listiterator), allocatable :: iter
        class (*), pointer :: value
        integer :: i

        ! allocate the array
        if (.not.allocated(out_array)) then
            allocate(out_array(in_list%length()))
        else
            if (size(out_array) /= in_list%length()) then
                deallocate(out_array)
                allocate(out_array(in_list%length()))
            end if
        end if

        ! iterate over the list
        iter = in_list%get_iterator()
        i = 1
        do while (iter%hasnext())
            value => iter%next()
            select type (value)
                type is (integer(kind=4))
                    out_array(i) = value
                class default
                    call fplus_error_print("Wrong data type in assignment!", "integer(kind=4), dimension(:) = list", FPLUS_WARN)
                    call fplus_error_print("Supported type is integer (kind=4)", "integer(kind=4), dimension(:) = list", FPLUS_WARN)
                    out_array(i) = fplus_fill_int
            end select
            i = i + 1
        end do
    end subroutine

    !> @public
    !> @brief   Allow the assignment of a list to an allocatable array of the type real (kind=4)
    subroutine list_to_intk8_array(out_array, in_list)
        integer (kind=8), dimension(:), allocatable, intent(inout) :: out_array
        class (list), intent(in) :: in_list

        ! local variables
        type (listiterator), allocatable :: iter
        class (*), pointer :: value
        integer :: i

        ! allocate the array
        if (.not.allocated(out_array)) then
            allocate(out_array(in_list%length()))
        else
            if (size(out_array) /= in_list%length()) then
                deallocate(out_array)
                allocate(out_array(in_list%length()))
            end if
        end if

        ! iterate over the list
        iter = in_list%get_iterator()
        i = 1
        do while (iter%hasnext())
            value => iter%next()
            select type (value)
                type is (integer(kind=4))
                    out_array(i) = value
                type is (integer(kind=8))
                    out_array(i) = value
                class default
                    call fplus_error_print("Wrong data type in assignment!", "integer(kind=8), dimension(:) = list", FPLUS_WARN)
                    call fplus_error_print("Supported types are integer (kind=4/8)", "integer(kind=8), dimension(:) = list", FPLUS_WARN)
                    out_array(i) = fplus_fill_intk8
            end select
            i = i + 1
        end do
    end subroutine

    !> @public
    !> @brief   Allow the assignment of a list to an allocatable array of the type real (kind=4)
    subroutine list_to_realk4_array(out_array, in_list)
        real (kind=4), dimension(:), allocatable, intent(inout) :: out_array
        class (list), intent(in) :: in_list

        ! local variables
        type (listiterator), allocatable :: iter
        class (*), pointer :: value
        integer :: i

        ! allocate the array
        if (.not.allocated(out_array)) then
            allocate(out_array(in_list%length()))
        else
            if (size(out_array) /= in_list%length()) then
                deallocate(out_array)
                allocate(out_array(in_list%length()))
            end if
        end if

        ! iterate over the list
        iter = in_list%get_iterator()
        i = 1
        do while (iter%hasnext())
            value => iter%next()
            select type (value)
                type is (real(kind=4))
                    out_array(i) = value
                class default
                    call fplus_error_print("Wrong data type in assignment!", "real(kind=4), dimension(:) = list", FPLUS_WARN)
                    call fplus_error_print("Supported type is real (kind=4)", "real(kind=4), dimension(:) = list", FPLUS_WARN)
                    out_array(i) = fplus_fill_real
            end select
            i = i + 1
        end do
    end subroutine

    !> @public
    !> @brief   Allow the assignment of a list to an allocatable array of the type real (kind=8)
    !> @brief   This operation is also implemented for lists with datetime objects
    subroutine list_to_realk8_array(out_array, in_list)
        real (kind=8), dimension(:), allocatable, intent(inout) :: out_array
        class (list), intent(in) :: in_list

        ! local variables
        type (listiterator), allocatable :: iter
        class (*), pointer :: value
        integer :: i

        ! allocate the array
        if (.not.allocated(out_array)) then
            allocate(out_array(in_list%length()))
        else
            if (size(out_array) /= in_list%length()) then
                deallocate(out_array)
                allocate(out_array(in_list%length()))
            end if
        end if

        ! iterate over the list
        iter = in_list%get_iterator()
        i = 1
        do while (iter%hasnext())
            value => iter%next()
            select type (value)
                class is (datetime)
                    out_array(i) = value%time_in_sec1970
                type is (real(kind=8))
                    out_array(i) = value
                class default
                    call fplus_error_print("Wrong data type in assignment!", "real(kind=8), dimension(:) = list", FPLUS_WARN)
                    call fplus_error_print("Supported types are real (kind=8) and datetime", "real(kind=8), dimension(:) = list", FPLUS_WARN)
                    out_array(i) = fplus_fill_realk8
            end select
            i = i + 1
        end do
    end subroutine

    ! then the procedures for the iterator ------------------------------------

    ! check if we are able to get an additional element
    function listiterator_hasnext(this) result (res)
        class(listiterator) :: this
        logical :: res
        ! TODO: why is the counter needed? ifort bug!
        if (associated(this%currentElement) .and. this%counter < this%thelist%nelements) then
            res = .true.
        else
            res = .false.
        end if
    end function

    ! get the next element
    function listiterator_next(this) result (res)
        class(listiterator) :: this
        class(*), pointer :: res
        class(element), pointer :: next_list_element

        ! assign null to the result
        res => null()

        ! get the next element object
        next_list_element => this%next_element()

        ! assign the result
        if (associated(next_list_element)) then
            res => next_list_element%value
        end if
    end function

    ! get the next element
    function listiterator_nextelement(this) result(res)
        class(listiterator) :: this
        class(element), pointer :: res
        logical :: test

        ! assign null to the result
        res => null()

        if (associated(this%currentElement)) then
            res => this%currentElement
            ! go to the next element
            if (this%direction == 0 .and. associated(this%currentElement%nextElement)) then
                this%currentElement => this%currentElement%nextElement
            else if (this%direction == 1 .and. associated(this%currentElement%prevElement)) then
                this%currentElement => this%currentElement%prevElement
            else
                nullify(this%currentElement)
            end if
        end if
        
        ! count this element
        this%counter = this%counter + 1
    end function

    ! then the procedures for the elements ------------------------------------

    ! a constructor for the element
    function new_element(value, prev, next, copy)
        class(*), target :: value
        class(element), pointer :: next, prev
        class(element), pointer :: new_element
        logical, optional :: copy

        ! allocate memory for the new list
        allocate(new_element)
        ! initialize the pointers
        new_element%nextElement => next
        new_element%prevElement => prev
        ! copy the value or create a pointer to it
        if (present(copy) .and. copy .eqv. .true.) then
            allocate(new_element%value, source=value)
            new_element%valueIsCopy = .true.
        else
            new_element%value => value
            new_element%valueIsCopy = .false.
        end if
    end function

end module fplus_list
