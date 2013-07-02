!> @brief   Functions for the serialization of derived types
module fplus_serialization
    use, intrinsic :: ISO_C_BINDING
    implicit none
    private

    ! list of public procedures
    public :: new_buffer

    !> @brief   the buffer data type contains a byte (int kind 1) array and a cursor to track 
    !>          read and write operations
    type, public :: buffer
        integer(kind=1), dimension(:), pointer :: array
        integer(kind=C_size_t) :: cursor
    contains
        !> @brief   write an array of any type to the buffer
        generic, public :: put => template buffer_put
        !> @brief   read an array of any type from the buffer
        generic, public :: get => template buffer_get
        !> @brief   Move the curser back to the start position
        procedure, public :: rewind => buffer_rewind
        !> @brief   write an array of any type to the buffer and store its rank and dimensions
        generic, public :: put_with_bounds => template buffer_put_with_bounds
        !> @brief   read an array of any type from the buffer and restore its bounds. The array has to be allocatable.
        generic, public :: get_with_bounds => template buffer_get_with_bounds        
        !> @brief   calculate the number of bytes needed to store this variable. Bounds are ignored.
        generic, public :: byte_size => template buffer_byte_size
        !> @brief   calculate the number of bytes needed to store this array with its bounds in the buffer
        generic, public :: byte_size_with_bounds => template buffer_byte_size_with_bounds
    end type

    ! the C memcpy function is used to transfer data into an buffer array
    interface
        subroutine C_memcpy(destination, source, nbytes) bind(C,name="memcpy")
            import :: C_ptr, C_size_t
            type(C_ptr), value :: destination, source
            integer(C_size_t), value, intent(in) :: nbytes
        end subroutine
    end interface

contains

    ! functions for the buffer type ---------------------------------------------------------------

    !> @brief       Create a new buffer object 
    !> @param[in]   buffer_size     the size of the new buffer in bytes
    !> @result      an initialed and allocated buffer object
    function new_buffer(buffer_size) result (res)
        integer (kind=C_size_t) :: buffer_size
        type (buffer) :: res

        ! the cursor points to the first byte
        res%cursor = 1

        ! the data array is allocated with the given size
        allocate(res%array(buffer_size))
    end function

    !> @brief       Move the curser back to the start position
    !> @param[in]   this    reference to the buffer object, automatically set by fortran
    subroutine buffer_rewind(this)
        class(buffer) :: this
        this%cursor = 1
    end subroutine


    template buffer_byte_size_with_bounds
        replace :: D => (:); (:,:); (:,:,:); (:,:,:,:); (:,:,:,:,:)
        replace :: ND => 1; 2; 3; 4; 5
        replace, foreach(D,ND) :: T => numeric
    contains
        !> @brief       calculate the number of bytes needed to store this array with its bounds in the buffer
        !> @param[in]   this    reference to the buffer object, automatically set by fortran
        function buffer_byte_size_with_bounds(this, array) result(res)
            class(buffer) :: this
            T, dimension D :: array
            integer (kind=C_size_t) :: res

            ! the number of dimensions 
            res = 4

            ! the upper and lower bounds
            res = res + ND * C_size_t * 2

            ! the array itself
            res = res + size(array) * storage_size(array) / 8_8
        end function
    end template


    template buffer_byte_size
        replace :: D => nil; ,dimension(:); ,dimension(:,:); ,dimension(:,:,:); ,dimension(:,:,:,:); ,dimension(:,:,:,:,:)
        replace :: S => 1_8; size(array); size(array); size(array); size(array); size(array)
        replace, foreach(D,S) :: T => numeric
    contains
        !> @brief       calculate the number of bytes needed to store this variable. Bounds are ignored.
        !> @param[in]   this    reference to the buffer object, automatically set by fortran
        function buffer_byte_size(this, array) result (res)
            class(buffer) :: this
            T D :: array
            integer (kind=C_size_t) :: res
            res = S * storage_size(array) / 8_8
        end function
    end template

    
    template buffer_put
        replace :: D => nil; dimension(:),; dimension(:,:),; dimension(:,:,:),; dimension(:,:,:,:),; dimension(:,:,:,:,:),
        replace :: F => nil; (1); (1,1); (1,1,1); (1,1,1,1); (1,1,1,1,1)
        replace :: S => 1_8; size(array); size(array); size(array); size(array); size(array)
        replace, foreach(D,F,S) :: T => numeric
    contains
        !> @brief       write an array of any type to the buffer
        !> @param[in]   this    reference to the buffer object, automatically set by fortran
        subroutine buffer_put(this, array)
            class(buffer) :: this
            T, D target :: array

            !local variables
            integer (kind=C_size_t) :: nbytes
            T, pointer :: array_ptr
            integer (kind=1), pointer :: buffer_ptr

            ! we need a pointer to the first element of the array to store and to the element at the cursor position of the buffer
            array_ptr => array F
            buffer_ptr => this%array(this%cursor)

            ! calculate the number of bytes needed to store the input array. storage_size is a fortran 2008 function.
            nbytes = storage_size(array)/8_8 * S
            
            ! is there enough space in the buffer-array?
            if (this%cursor + nbytes > size(this%array)+1) then
                print*, "ERROR in buffer%put: the capacity of this buffer is to small!"
                call exit(-1)
            end if

            !perform the copy 
            call C_memcpy(c_loc(buffer_ptr), c_loc(array_ptr), nbytes)  

            ! move the cursor
            this%cursor = this%cursor + nbytes
        end subroutine
    end template


    template buffer_get
        replace :: D => nil; dimension(:),; dimension(:,:),; dimension(:,:,:),; dimension(:,:,:,:),; dimension(:,:,:,:,:),
        replace :: F => nil; (1); (1,1); (1,1,1); (1,1,1,1); (1,1,1,1,1)
        replace :: S => 1_8; size(array); size(array); size(array); size(array); size(array)
        replace, foreach(D,F,S) :: T => numeric
    contains
        !> @brief       read an array of any type from the buffer
        !> @param[in]   this    reference to the buffer object, automatically set by fortran
        subroutine buffer_get(this, array)
            class(buffer) :: this
            T, D target :: array

            !local variables
            integer (kind=C_size_t) :: nbytes
            T, pointer :: array_ptr
            integer (kind=1), pointer :: buffer_ptr

            ! we need a pointer to the first element of the array to restore and to the element at the cursor position of the buffer
            array_ptr => array F
            buffer_ptr => this%array(this%cursor)

            ! calculate the number of bytes needed to restore the array. storage_size is a fortran 2008 function.
            nbytes = storage_size(array)/8_8 * S
            
            ! is there enough space in the buffer-array?
            if (this%cursor + nbytes > size(this%array)+1) then
                print*, "ERROR in buffer%put: the capacity of this buffer is to small!"
                call exit(-1)
            end if

            !perform the copy 
            call C_memcpy(c_loc(array_ptr), c_loc(buffer_ptr), nbytes)  

            ! move the cursor
            this%cursor = this%cursor + nbytes
        end subroutine
    end template


    template buffer_put_with_bounds
        replace :: D => (:); (:,:); (:,:,:); (:,:,:,:); (:,:,:,:,:)
        replace :: F => (1); (1,1); (1,1,1); (1,1,1,1); (1,1,1,1,1)
        replace :: ND => 1; 2; 3; 4; 5
        replace, foreach(D,F,ND) :: T => numeric
    contains
        !> @brief       write an array of any type to the buffer and store its rank and dimensions
        !> @param[in]   this    reference to the buffer object, automatically set by fortran
        subroutine buffer_put_with_bounds(this, array)
            class(buffer) :: this
            T, dimension D :: array

            ! store the number of dimensions 
            call this%put(ND)

            ! store the lower and upper bounds of the array
            call this%put(lbound(array, kind=C_size_t))
            call this%put(ubound(array, kind=C_size_t))

            ! store the array itself
            call this%put(array)
        end subroutine
    end template


    template buffer_get_with_bounds
        replace :: D => (:); (:,:); (:,:,:); (:,:,:,:); (:,:,:,:,:)
        replace :: F => (1); (1,1); (1,1,1); (1,1,1,1); (1,1,1,1,1)
        replace :: ND => 1; 2; 3; 4; 5
        replace :: AL => lb(1):ub(1);   lb(1):ub(1), lb(2):ub(2);   lb(1):ub(1), lb(2):ub(2), lb(3):ub(3);   lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), lb(4):ub(4);    lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), lb(4):ub(4), lb(5):ub(5)
        replace, foreach(D,F,ND,AL) :: T => numeric
    contains
        !> @brief       read an array of any type from the buffer and restore its bounds. The array has to be allocatable.
        !> @param[in]   this    reference to the buffer object, automatically set by fortran
        subroutine buffer_get_with_bounds(this, array)
            class(buffer) :: this
            T, dimension D, allocatable :: array

            ! local variables
            integer :: ndims
            integer (kind=C_size_t), dimension(ND) :: lb, ub, lb_array, ub_array

            ! compare the number of dimensions 
            call this%get(ndims)
            if (ndims /= ND) then
                print*, "ERROR in buffer%get_with_bounds: rank mismatch"
                call exit(-1)
            end if

            ! read the lower and upper bounds of the array
            call this%get(lb)
            call this%get(ub)

            ! is the array already allocated? if not, do it now
            if (.not.allocated(array)) then
                allocate(array(AL))
            ! the array is already allocated, but are the array bounds correct?
            else
                lb_array = lbound(array, kind=C_size_t)
                ub_array = ubound(array, kind=C_size_t)
                if (any(lb /= lb_array) .or. any(ub /= ub_array)) then
                    deallocate(array)
                    allocate(array(AL))
                end if
            end if

            ! restore the array itself
            call this%get(array)
        end subroutine
    end template


end module
