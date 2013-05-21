module mod_map
    implicit none
    ! this module implements a hash table

    ! the map itself
    type map
        class(node), dimension(:), pointer :: table
        integer :: length
        integer :: initialSize
        contains
        procedure add                       ! add an element to the new map
        procedure positionForHash           ! calculate the index of the hash key in the table
    end type
    ! define the constructor for the map
    interface map
        module procedure constructor_map
    end interface

    ! the hash table consists of an arrayof nodes where each node can have children
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
        class(*) :: key

        !local variables for character key
        integer :: length, i
        integer (kind=1), dimension(:), allocatable :: chars

        select type (key)
            type is (character (len=*))
                ! use sdbm
                calculateHash = 0
                length = len_trim(key)
                ! transfer the contents of the string to an integer array
                allocate(chars(length))
                chars = transfer(key, chars)
                do i = 1, length
                    calculateHash = chars(i) + shiftl(calculateHash, 6) + shiftl(calculateHash, 16) - calculateHash
                end do
                ! free the memory
                deallocate(chars)
            type is (real (kind=8))
                calculateHash = transfer(key, calculateHash)
            class default
                ! not yet implemented for other types
                call exit(1)
        end select
    end function

    ! procedures of the map ---------------------------------------------------

    ! the constructor of the map
    function constructor_map(isize)
        class(map), pointer :: constructor_map
        integer, optional :: isize
        ! allocate the memory for the new map
        allocate(constructor_map)
        constructor_map%table => null()
        ! set the initial size of the table
        if (present(isize)) then
            constructor_map%initialSize = isize
        else
            constructor_map%initialSize = 10000
        end if
    end function

    ! add an element to the map
    subroutine add(this, key, value, copy)
        class(map) :: this
        class(*), target :: key
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
            allocate(this%table(this%initialSize))
        end if

        ! create a new node
        newnode => node(key, value, copyValue)

        ! calculate the position in the table
        pos = this%positionForHash(newnode%hash)

        ! count this element
    end subroutine

    ! calculate the index within the hash table for a given hash code
    function positionForHash(this, hash)
        class(map) :: this
        integer (kind=8) :: hash
        integer :: positionForHash
        ! calculate the modulo of the hash code
        positionForHash = modulo(hash, this%initialSize)
    end function

    ! procedures of the node --------------------------------------------------

    ! the constructor
    function constructor_node(key, value, copy)
        class(node), pointer :: constructor_node
        class(*), target :: key
        class(*), target :: value
        logical, optional :: copy
        class(*), pointer :: test

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

end module mod_map
