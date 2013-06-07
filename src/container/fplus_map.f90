!> @brief   A simple hash table implementation
!> @author  Robert Schuster
module fplus_map
    use, intrinsic :: ISO_C_Binding
    use fplus_strings
    use fplus_hashcode
    use fplus_object
    implicit none
    private

    ! the hash table is filled with nodes where each node can have children
    type node
        ! a pointer to the next node
        class(node), pointer :: next => null()
        ! the key used to insert the value
        class(*), pointer :: key => null()
        ! the value stored with the key
        class(*), pointer :: value => null()
        ! the hash code of the key
        integer (kind=8) :: hash
        ! is the value a copy? the key is always a copy
        logical :: valueIsCopy
    contains
        procedure release                   ! deallocate the memory of this node and all children
    end type


    ! the hash table itself consists of an array of nodepointers where each can point to an node object
    ! the object holds also the number of nodes
    type nodepointer
        class(node), pointer :: thenode => null()
        integer :: length
    end type


    !> @brief   The hash map type
    type, extends(object), public :: map
        class(nodepointer), dimension(:), pointer, private :: table => null()
        ! number of elements currently stored in the map
        integer, private :: nelements
        ! the initial size of the underlying array
        integer, private :: initialSize
        ! @brief    current capacity of the map underlying array
        integer, private :: capacity
    contains
        !> @brief   Associates the specified value with the specified key in this map.
        procedure, public :: add => map_add
        !> @brief   Add an existing node to the table, internally used
        procedure, private :: add_node => map_add_node
        !> @brief   Calculate the index of the hash key in the table
        procedure, private :: positionForHash
        !> @brief   Print the table structure for debuging
        procedure, public :: printContent => map_printcontent
        !> @brief   Returns the value to which the specified key is mapped, or null if this map contains no mapping for the key.
        procedure, public :: get => map_get
        !> @brief   Removes the mapping for the specified key from this map if present.
        procedure, public :: remove => map_remove
        !> @brief   Removes all of the mappings from this map and deallocates all internal used memory
        procedure, public :: clear => map_clear
        !> @brief   Returns the number of elements in this list
        procedure, public :: length => map_length
        !> @brief   Returns a string representation of the map
        procedure, public :: to_string => map_to_string
        !> @brief   Calculate the hashcode of the table
        procedure, public :: hashcode => map_hashcode
        !> @brief   Change the size of the underlying array, automatically called
        procedure, public :: resize => map_resize
        !> @brief   Returns .true. if this map contains a mapping for the specified key.
        procedure, public :: contains_key => map_contains_key
    end type
    ! make the constructor for the map public
    public :: new_map


! the implementation of the procedures follows
contains

    ! procedures of the map ---------------------------------------------------

    ! the constructor of the map
    function new_map(isize)
        type(map) :: new_map
        integer, intent(in), optional :: isize

        ! set the initial size of the table
        if (present(isize)) then
            new_map%initialSize = isize
        else
            new_map%initialSize = 100
        end if
        new_map%capacity = new_map%initialSize
        new_map%nelements = 0
    end function

    !> @public
    !> @brief       Returns a string representation of the map
    !> @param[in]   this    reference to the map object, automatically set by fortran
    function map_to_string(this) result (res)
        class(map) :: this
        character (len=:), allocatable :: res

        ! local variables
        integer :: i, ndigits, j
        character (len=10) :: iformat
        class(node), pointer :: currentnode

        ! it es a map and has a number of elements
        res = "Map, number of elements: " // number_to_string(this%nelements) // &
              ", capacity: " // number_to_string(this%capacity) // char(10) // char(10)

        ! are there any elements in the map?
        if (this%nelements == 0) then
            res = res // "The map is completely empty!"
            return
        end if

        ! format for the cell number
        ndigits = ndigits_of_integer(this%capacity)
        write (iformat, "(A,I1,A)") "(I", ndigits, ")"

        ! loop over the underlying table
        do i = 1, this%capacity
            res = res // "    Cell " // number_to_string(i, iformat) // ") "
            ! are there elements stored in this cell?
            if (this%table(i)%length > 0) then
                currentnode => this%table(i)%thenode
                j = 1
                do
                    if (j == 1) then
                        res = res // " => Node {" // char(10)
                    else
                        res = res // repeat(" ", ndigits + 11)  // " => Node {" // char(10)
                    end if
                    ! add the hashcode, key, and value
                    res = res // repeat(" ", ndigits + 19) // "key      = " // type_to_string(currentnode%key) // char(10)
                    res = res // repeat(" ", ndigits + 19) // "value    = " // type_to_string(currentnode%value) // char(10)
                    res = res // repeat(" ", ndigits + 19) // "hashcode = " // type_to_string(currentnode%hash) // char(10)
                    res = res // repeat(" ", ndigits + 15) // "}" // char(10)
                    ! leave the loop if no additional elements are there
                    if (.not. associated(currentnode%next)) then
                        exit
                    else
                        currentnode => currentnode%next
                        j = j + 1
                    end if
                end do
            else
                ! this cell is empty
                res = res // "empty"
            end if
            res = res // char(10)
        end do
    end function

    !> @public
    !> @brief       calculate a hashcode for the list
    !> @details     this function takes all elements within the map into account. The calculated
    !>              hashcode is the sum of all hashcodes from stored values and keys
    !> @param[in]   this    reference to the map object, automatically set by fortran
    integer (kind=8) function map_hashcode(this) result (res)
        class(map) :: this

        ! local variables
        class(node), pointer :: currentnode
        integer :: i

        ! the hashcode of the word "map" and the capacity
        res = calculateHash("map") * this%capacity

        ! loop over the underlying table
        do i = 1, this%capacity
            ! are there elements stored in this cell?
            if (this%table(i)%length > 0) then
                currentnode => this%table(i)%thenode
                do
                    res = res + (currentnode%hash + calculateHash(currentnode%value)) * i
                    if (.not. associated(currentnode%next)) then
                        exit
                    else
                        currentnode => currentnode%next
                    end if
                end do
            else
                res = res + i
            end if
        end do
    end function

    !> @public
    !> @brief       Associates the specified value with the specified key in this map.
    !> @details     If the map previously contained a mapping for the key, the old value is replaced.
    !>              A copy of the key is always stored within the map.
    !> @param[in]   this    reference to the map object, automatically set by fortran
    !> @param[in]   key     key with which the specified value is to be associated
    !> @param[in]   value   value to be associated with the specified key
    !> @param[in]   copy    if present and .true., the value will be copied and not linked. The
    !>                      default operation is to store a pointer to the value.
    subroutine map_add(this, key, value, copy)
        class(map) :: this
        class(*), intent(in) :: key
        class(*), target :: value
        logical, optional :: copy
        logical :: copyValue

        ! local variables
        integer :: pos
        class(node), pointer :: newnode

        ! copy or link?
        if (present(copy)) then
            copyValue = copy
        else
            copyValue = .false.
        end if

        ! create the root if not already present
        if (.not.associated(this%table)) then
            allocate(this%table(this%capacity))
            do pos = 1, this%capacity
                this%table(pos)%thenode => null()
                this%table(pos)%length = 0
            end do
        end if

        ! create a new node
        newnode => new_node(key, value, copyValue)

        ! add the node to the map
        call this%add_node(newnode)

        ! resize the table if it is filled to more then 200% percent
        if (this%nelements > this%capacity*2) then
            call this%resize(this%capacity*10)
        end if
    end subroutine

    !> @brief       Add an existing node to the table, internally used
    !> @param[in]   this    reference to the map object, automatically set by fortran
    !> @param[in]   newnode the node that should be added to the map
    subroutine map_add_node(this, newnode)
        class(map) :: this
        class(node), pointer :: newnode

        ! local variables
        integer :: pos = 0
        class(node), pointer :: currentnode, lastnode
        logical :: found


        ! calculate the position in the table
        pos = this%positionForHash(newnode%hash)

        ! place the new node into the table
        ! the first posibility is an empty cell of the table
        if (this%table(pos)%length == 0) then
            this%table(pos)%thenode => newnode
            this%table(pos)%length = 1
            ! count this element
            this%nelements = this%nelements + 1
        else
            ! the second posibility is that a node is already present at this position.
            ! has one of the existing nodes the same hash code? if so, replace it
            currentnode => this%table(pos)%thenode
            lastnode => null()
            found = .false.
            do
                ! the node found in the table has the same hash code, replace it
                if (currentnode%hash == newnode%hash) then
                    ! it is the first node, replace it
                    if (.not.associated(lastnode) .and. currentnode%hash == this%table(pos)%thenode%hash) then
                        this%table(pos)%thenode => newnode
                        newnode%next => currentnode%next
                    else
                        ! it is not the first node
                        lastnode%next => newnode
                        newnode%next => currentnode%next
                    end if
                    ! free the memory of the current node
                    currentnode%next => null()
                    call currentnode%release()
                    deallocate(currentnode)
                    ! leave the loop
                    found = .true.
                    exit
                end if
                ! leave the loop if no more nodes are present
                if (.not.associated(currentnode%next)) exit
                lastnode => currentnode
                currentnode => currentnode%next
            end do
            ! if the key was not found, append it to the last node
            if (found .eqv. .false.) then
                currentnode%next => newnode
                this%table(pos)%length = this%table(pos)%length + 1
                ! count this element
                this%nelements = this%nelements + 1
            end if
        end if
    end subroutine


    ! calculate the index within the hash table for a given hash code
    function positionForHash(this, hash)
        class(map) :: this
        integer (kind=8), intent(in) :: hash
        integer :: positionForHash
        ! calculate the modulo of the hash code
        positionForHash = modulo(hash, this%capacity) + 1
    end function

    !> @public
    !> @brief       Print the table structure for debuging
    !> @details     This subroutine lists everything stored in the map together with the
    !>              calculated hash codes and the position within the map.
    !> @param[in]   this    reference to the map object, automatically set by fortran
    subroutine map_printContent(this)
        class(map) :: this
        integer :: i
        class(node), pointer :: currentnode
        print*, ""
        ! is the table present?
        if (associated(this%table)) then
            ! print the number of elements in the map
            write (*, "(A,I8)") "Elements in the map:", this%nelements
            ! loop over all table cells
            do i = 1, this%capacity
                if (associated(this%table(i)%thenode)) then
                    currentnode => this%table(i)%thenode
                    write (*, "(A,I8,A,I20,A)") "cell:", i, " => node(", currentnode%hash, ")"
                    do while (associated(currentnode%next))
                        currentnode => currentnode%next
                        write (*, "(A,I20,A)") "              => node(", currentnode%hash, ")"
                    end do
                else
                    write (*, "(A,I8,A)") "cell:", i, " empty"
                end if
            end do
        else
            print*, "the map is completely empty"
        end if
        print*, ""
    end subroutine

    !> @public
    !> @brief       Removes all of the mappings from this map and deallocates all internal used memory
    !> @details     This subroutine completely resets the map to its initial state and frees up all
    !>              the allocated memory. Call is before deallocation of a map object to ensure that
    !>              no memory is leaked.
    !> @param[in]   this    reference to the map object, automatically set by fortran
    subroutine map_clear(this)
        class(map) :: this
        integer :: i
        ! loop over the complete table
        do i = 1, this%capacity
            ! is here an element?
            if (associated(this%table(i)%thenode)) then
                call this%table(i)%thenode%release()
                deallocate(this%table(i)%thenode)
                this%table(i)%length = 0
            end if
        end do
        ! remove the table itself
        deallocate (this%table)
        this%nelements = 0
        ! reset the capacity to the initial capacity
        this%capacity = this%initialSize
    end subroutine

    !> @public
    !> @brief       Returns the value to which the specified key is mapped, or null if this map contains no mapping for the key.
    !> @details     This function looks up the key in the map and returns a pointer to the stored value.
    !> @param[in]   this            reference to the map object, automatically set by fortran
    !> @param[in]   key             key with which the searched value is sould be associated
    !> @param[out]  contains_key    if present, this parameter is se to true, if the key was found in the map. Optional.
    !> @return      a pointer to the value associated to the key or a null-pointer if nothing was found.
    function map_get(this, key, contains_key)
        class(map) :: this
        class(*), intent(in) :: key
        class(*), pointer :: map_get
        logical, optional, intent(out) :: contains_key

        ! local variables
        integer (kind=8) :: hash
        integer :: pos
        class(node), pointer :: currentnode

        if (present(contains_key)) contains_key = .false.

        ! if nothing is found, return a null-pointer
        map_get => null()
        ! calculate the hash code for the key
        hash = calculateHash(key)
        ! find the value in the table
        pos = this%positionForHash(hash)
        if (this%table(pos)%length > 0) then
            currentnode => this%table(pos)%thenode
            do
                ! is the current node the search node
                if (currentnode%hash == hash) then
                    map_get => currentnode%value
                    if (present(contains_key)) contains_key = .true.
                    exit
                end if
                ! not found? try the next one
                if (.not.associated(currentnode%next)) then
                    exit
                else
                    currentnode => currentnode%next
                end if
            end do
        end if
    end function

    !> @public
    !> @brief       Removes the mapping for the specified key from this map if present.
    !> @details     This subroutine looks up the key in the map and removes the stored value. If the value was copied,
    !>              the used memory is deallocated.
    !> @param[in]   this    reference to the map object, automatically set by fortran
    !> @param[in]   key     key with which the searched value is sould be associated
    subroutine map_remove(this, key)
        class(map) :: this
        class(*), intent(in) :: key

        ! local variables
        integer :: pos = 0
        integer (kind=8) :: hash
        class(node), pointer :: currentnode, lastnode, nodetoremove

        ! calculate the position in the table
        hash = calculateHash(key)
        pos = this%positionForHash(hash)

        ! find the corresponding node in the list
        ! is there a node at the calculated position of the table?
        if (this%table(pos)%length == 0) then
            ! no, nothing found. the key is not in the map
            return
        else
            ! the second posibility is that a node is already present at this position.
            ! has one of the existing nodes the same hash code? if so, remove it
            currentnode => this%table(pos)%thenode
            lastnode => null()
            do
                ! the node found in the table has the same hash code, remove it
                if (currentnode%hash == hash) then
                    ! it is the first node, remove it
                    if (.not.associated(lastnode) .and. currentnode%hash == this%table(pos)%thenode%hash) then
                        nodetoremove => this%table(pos)%thenode
                        this%table(pos)%thenode => this%table(pos)%thenode%next
                        nodetoremove%next => null()
                        call nodetoremove%release()
                        deallocate(nodetoremove)
                    else
                        ! it is not the first node
                        lastnode%next => currentnode%next
                        currentnode%next => null()
                        call currentnode%release()
                        deallocate (currentnode)
                    end if
                    ! correct the number of elements
                    this%table(pos)%length = this%table(pos)%length -1
                    this%nelements = this%nelements -1
                    ! resize the underlying table if needed
                    if (this%nelements < this%capacity * 0.01) then
                        call this%resize(int(this%capacity * 0.01))
                    end if
                    ! leave the loop
                    exit
                end if
                ! leave the loop if no more nodes are present
                if (.not.associated(currentnode%next)) exit
                lastnode => currentnode
                currentnode => currentnode%next
            end do
        end if
    end subroutine

    !> @public
    !> @brief       Returns the number of elements in this list
    !> @param[in]   this    reference to the map object, automatically set by fortran
    !> @return      number of elements
    integer function map_length(this)
        class(map) :: this
        map_length = this%nelements
    end function

    !> @public
    !> @brief       Change the size of the underlying array, automatically called
    !> @detail      a new array is allocated and the content of the old table is transfered to the
    !>              new table.
    !> @param[in]   this    reference to the map object, automatically set by fortran
    !> @param[in]   newsize the new size of the underlying table
    subroutine map_resize(this, newsize)
        class(map) :: this
        integer, intent(in) :: newsize

        ! local variables
        class(nodepointer), dimension(:), pointer :: oldtable
        class(node), pointer :: currentnode, nextnode
        integer :: i, j, oldcapacity, newsize_intern

        ! is the new size smaller then the initial size?
        if (newsize < this%initialSize) then
            newsize_intern = this%initialSize
        else
            newsize_intern = newsize
        end if

        ! is the new size different from the current size?
        if (newsize_intern == this%capacity) return

        ! move the old table to the old table pointer and replace it with
        ! a new allocated table of the new size
        oldtable => this%table
        nullify(this%table)
        allocate(this%table(newsize_intern))
        oldcapacity = this%capacity
        this%capacity = newsize_intern
        this%nelements = 0

        ! transfer the content of the old table to the new table
        ! loop over the old table
        do i= 1, oldcapacity
            ! move all nodes from a table cell to the new table
            if (oldtable(i)%length > 1) then
                currentnode => oldtable(i)%thenode
                do j = 1, oldtable(i)%length
                    nextnode => currentnode%next
                    currentnode%next => null()
                    call this%add_node(currentnode)
                    currentnode => nextnode
                end do
            ! we only have one node, move it directly
            else if (oldtable(i)%length == 1) then
                currentnode => oldtable(i)%thenode
                nullify(oldtable(i)%thenode)
                call this%add_node(currentnode)
            end if
        end do
    end subroutine

    !> @public
    !> @brief       Returns true if this map contains a mapping for the specified key.
    !> @details     This method checks
    function map_contains_key(this, key) result (res)
        class(map) :: this
        class(*) :: key
        logical :: res

        ! local variables, not used
        class(*), pointer :: value_for_key

        ! is there a value for this key?
        value_for_key => this%get(key, contains_key=res)
    end function

    ! procedures of the node --------------------------------------------------

    ! the constructor
    function new_node(key, value, copy)
        class(node), pointer :: new_node
        class(*), intent(in) :: key
        class(*), target :: value
        logical, optional :: copy

        ! allocate the memory for the new node
        allocate(new_node)
        new_node%next => null()

        ! copy the key
        allocate(new_node%key, source=key)

        ! copy or link the value
        if (present(copy) .and. copy .eqv. .true.) then
            allocate(new_node%value, source=value)
            new_node%valueIsCopy = .true.
        else
            new_node%value => value
            new_node%valueIsCopy = .false.
        end if

        ! calculate the hash code
        new_node%hash = calculateHash(key)
    end function

    ! free the memory allocated inside of this node
    recursive subroutine release(this)
        class(node) :: this
        ! clean the node itself
        if (this%valueIsCopy) then
            deallocate(this%value)
        else
            nullify(this%value)
        end if
        deallocate(this%key)
        ! clean children of this node
        if (associated(this%next)) then
            call this%next%release()
            deallocate(this%next)
        end if
    end subroutine

end module fplus_map
