module mod_map
    implicit none
    ! this module implements a hash table

    ! the map itself
    type map
        class(nodepointer), dimension(:), pointer :: table
        integer :: length
        integer :: initialSize
        contains
        procedure add                       ! add an element to the new map
        procedure positionForHash           ! calculate the index of the hash key in the table
        procedure printContent              ! print the table structure for debuging
        ! deallocate all elements
        procedure removeAll
    end type
    ! define the constructor for the map
    interface map
        module procedure constructor_map
    end interface

    ! the hash table itself consists of an array of nodepointers where each can point to an node object
    ! the object holds also the number of nodes
    type nodepointer
        class(node), pointer :: thenode
        integer :: length
    end type

    ! the hash table is filled with nodes where each node can have children
    type node
        class(node), pointer :: next
        ! the key used to insert the value
        class(*), pointer :: key
        ! the value stored with the key
        class(*), pointer :: value
        ! the hash code of the key
        integer (kind=8) :: hash
        ! is the value a copy? the key is always a copy
        logical :: valueIsCopy
        contains
        procedure release                   ! deallocate the memory of this node and all children
    end type
    ! define the constructor for the node
    interface node
        module procedure constructor_node
    end interface

! the implementation of the procedures follows
contains

    ! procedures that are not bound to a type ---------------------------------
    ! a very simple hash function, see sdbm on http://www.cse.yorku.ca/~oz/hash.html
    function calculateHash(key)
        integer (kind=8) :: calculateHash
        class(*), intent(in) :: key

        !local variables for character key
        integer :: length
        integer (kind=1), dimension(:), allocatable :: chars

        select type (key)
            type is (character (len=*))
                ! use sdbm
                calculateHash = 0
                length = len_trim(key)
                ! transfer the contents of the string to an integer array
                allocate(chars(length))
                chars = transfer(key, chars)
                calculateHash = calculateSDBMhash(chars)
                ! free the memory
                deallocate(chars)
            type is (real (kind=8))
                ! use sdbm
                calculateHash = 0
                ! transfer the contents of the real to an integer array
                allocate(chars(8))
                chars = transfer(key, chars)
                calculateHash = calculateSDBMhash(chars)
                ! free the memory
                deallocate(chars)
            class default
                ! not yet implemented for other types
                call exit(1)
        end select
    end function

    ! calculate sdbm hash function from a byte array
    function calculateSDBMhash(intarray)
        integer (kind=8) :: calculateSDBMhash
        integer (kind=1), dimension(:), intent(in) :: intarray
        integer :: length, i
        length = size(intarray)
        do i = 1, length
            calculateSDBMhash = intarray(i) + shiftl(calculateSDBMhash, 6) + shiftl(calculateSDBMhash, 16) - calculateSDBMhash
        end do
    end function

    ! procedures of the map ---------------------------------------------------

    ! the constructor of the map
    function constructor_map(isize)
        class(map), pointer :: constructor_map
        integer, intent(in), optional :: isize
        ! allocate the memory for the new map
        allocate(constructor_map)
        constructor_map%table => null()
        ! set the initial size of the table
        if (present(isize)) then
            constructor_map%initialSize = isize
        else
            constructor_map%initialSize = 10000
        end if
        constructor_map%length = 0
    end function

    ! add an element to the map
    subroutine add(this, key, value, copy)
        class(map) :: this
        class(*), intent(in) :: key
        class(*), target :: value
        logical, optional :: copy
        logical :: copyValue

        ! local variables
        integer :: pos = 0
        class(node), pointer :: newnode, currentnode, lastnode
        logical :: found

        ! copy or link?
        if (present(copy)) then
            copyValue = copy
        else
            copyValue = .false.
        end if

        ! create the root if not already present
        if (.not.associated(this%table)) then
            allocate(this%table(this%initialSize))
            do pos = 1, this%initialSize
                this%table(pos)%thenode => null()
                this%table(pos)%length = 0
            end do
        end if

        ! create a new node
        newnode => node(key, value, copyValue)

        ! calculate the position in the table
        pos = this%positionForHash(newnode%hash)

        ! place the new node into the table
        ! the first posibility is an empty cell of the table
        if (this%table(pos)%length == 0) then
            this%table(pos)%thenode => newnode
            this%table(pos)%length = 1
            ! count this element
            this%length = this%length + 1
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
                this%length = this%length + 1
            end if
        end if

    end subroutine

    ! calculate the index within the hash table for a given hash code
    function positionForHash(this, hash)
        class(map) :: this
        integer (kind=8), intent(in) :: hash
        integer :: positionForHash
        ! calculate the modulo of the hash code
        positionForHash = modulo(hash, this%initialSize) + 1
    end function

    ! print the table structure for debuging
    subroutine printContent(this)
        class(map) :: this
        integer :: i
        class(node), pointer :: currentnode
        print*, ""
        ! is the table present?
        if (associated(this%table)) then
            ! print the number of elements in the map
            write (*, "(A,I8)") "Elements in the map:", this%length
            ! loop over all table cells
            do i = 1, this%initialSize
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

    ! finalize the list, clean up the memory
    subroutine removeAll(this)
        class(map) :: this
        integer :: i
        ! loop over the complete table
        do i = 1, this%initialSize
            ! is here an element?
            if (associated(this%table(i)%thenode)) then
                call this%table(i)%thenode%release()
                deallocate(this%table(i)%thenode)
            end if
        end do
        ! remove the table itself
        deallocate (this%table)
    end subroutine

    ! procedures of the nodepointer -------------------------------------------


    ! procedures of the node --------------------------------------------------

    ! the constructor
    function constructor_node(key, value, copy)
        class(node), pointer :: constructor_node
        class(*), intent(in) :: key
        class(*), target :: value
        logical, optional :: copy

        ! allocate the memory for the new node
        allocate(constructor_node)
        constructor_node%next => null()

        ! copy the key
        allocate(constructor_node%key, source=key)

        ! copy or link the value
        if (present(copy) .and. copy .eqv. .true.) then
            allocate(constructor_node%value, source=value)
            constructor_node%valueIsCopy = .true.
        else
            constructor_node%value => value
            constructor_node%valueIsCopy = .false.
        end if

        ! calculate the hash code
        constructor_node%hash = calculateHash(key)
    end function

    ! free the memory allocated inside of this node
    subroutine release(this)
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

end module mod_map
