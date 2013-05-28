!> @brief   A simple hash table implementation
!> @author  Robert Schuster
module mod_map
    use, intrinsic :: ISO_C_Binding
    use mod_hashcode
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
    ! define the constructor for the node
    interface node
        module procedure constructor_node
    end interface


    ! the hash table itself consists of an array of nodepointers where each can point to an node object
    ! the object holds also the number of nodes
    type nodepointer
        class(node), pointer :: thenode => null()
        integer :: length
    end type


    !> @brief   The hash map type
    type, public :: map
        class(nodepointer), dimension(:), pointer, private :: table => null()
        integer, private :: nelements
        integer, private :: initialSize
    contains
        !> @brief   Associates the specified value with the specified key in this map.
        procedure, public :: add => map_add
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
    end type
    ! define the constructor for the map
    interface map
        module procedure map_constructor
    end interface


! the implementation of the procedures follows
contains

    ! procedures of the map ---------------------------------------------------

    ! the constructor of the map
    function map_constructor(isize)
        class(map), pointer :: map_constructor
        integer, intent(in), optional :: isize
        ! allocate the memory for the new map
        allocate(map_constructor)
        map_constructor%table => null()
        ! set the initial size of the table
        if (present(isize)) then
            map_constructor%initialSize = isize
        else
            map_constructor%initialSize = 10000
        end if
        map_constructor%nelements = 0
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
        positionForHash = modulo(hash, this%initialSize) + 1
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
        do i = 1, this%initialSize
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
    end subroutine

    !> @public
    !> @brief       Returns the value to which the specified key is mapped, or null if this map contains no mapping for the key.
    !> @details     This function looks up the key in the map and returns a pointer to the stored value.
    !> @param[in]   this    reference to the map object, automatically set by fortran
    !> @param[in]   key     key with which the searched value is sould be associated
    !> @return      a pointer to the value associated to the key or a null-pointer if nothing was found.
    function map_get(this, key)
        class(map) :: this
        class(*), intent(in) :: key
        class(*), pointer :: map_get

        ! local variables
        integer (kind=8) :: hash
        integer :: pos
        class(node), pointer :: currentnode

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
    !> @brief   Returns the number of elements in this list
    !> @return  number of elements
    integer function map_length(this)
        class(map) :: this
        map_length = this%nelements
    end function

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

end module mod_map
