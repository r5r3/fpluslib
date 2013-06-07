!> @brief   this module contains simple implementation of tuples for different data types
!> @author  Robert Schuster
module fplus_tuple
    use fplus_hashcode
    use fplus_strings
    implicit none
    private

    type, extends(object), public :: tuple
        !> @brief   The data itself
        class(*), allocatable :: a,b,c,d,e,f,g,h,i,j
    contains
        !> @brief   Return a string representation of the tuple
        procedure, public :: to_string => tuple_to_string
        !> @brief   Calculate the hashcode of the tuple
        procedure, public :: hashcode => tuple_hashcode
    end type

    public :: new_tuple
contains

    !> @brief   Create a new tuple with up to ten elements
    function new_tuple(a,b,c,d,e,f,g,h,i,j) result (res)
        class(*), optional :: a,b,c,d,e,f,g,h,i,j
        type(tuple), allocatable :: res

        allocate(res)

        if (present(a)) allocate(res%a, source=a)
        if (present(b)) allocate(res%b, source=b)
        if (present(c)) allocate(res%c, source=c)
        if (present(d)) allocate(res%d, source=d)
        if (present(e)) allocate(res%e, source=e)
        if (present(f)) allocate(res%f, source=f)
        if (present(g)) allocate(res%g, source=g)
        if (present(h)) allocate(res%h, source=h)
        if (present(i)) allocate(res%i, source=i)
        if (present(j)) allocate(res%j, source=j)
    end function

    !> @brief       Return a string representation of the tuple
    !> @param[in]   this    reference to the tuple object, automatically set by fortran
    function tuple_to_string(this) result (res)
        class(tuple) :: this
        character (len=:), allocatable :: res

        res = "Tuple: ("
        if (allocated(this%a)) res = res // type_to_string(this%a) // ", "
        if (allocated(this%b)) res = res // type_to_string(this%b) // ", "
        if (allocated(this%c)) res = res // type_to_string(this%c) // ", "
        if (allocated(this%d)) res = res // type_to_string(this%d) // ", "
        if (allocated(this%e)) res = res // type_to_string(this%e) // ", "
        if (allocated(this%f)) res = res // type_to_string(this%f) // ", "
        if (allocated(this%g)) res = res // type_to_string(this%g) // ", "
        if (allocated(this%h)) res = res // type_to_string(this%h) // ", "
        if (allocated(this%i)) res = res // type_to_string(this%i) // ", "
        if (allocated(this%j)) res = res // type_to_string(this%j)
        res = res // ")"
    end function

    !> @brief       Calculate the hashcode of the tuple
    !> @param[in]   this    reference to the tuple object, automatically set by fortran
    function tuple_hashcode(this) result (res)
        class(tuple) :: this
        integer (kind=8) :: res

        res = calculateHash("Tuple")
        if (allocated(this%a)) res = res + calculateHash(this%a)
        if (allocated(this%b)) res = res + calculateHash(this%b) * 2_8
        if (allocated(this%c)) res = res + calculateHash(this%c) * 3_8
        if (allocated(this%d)) res = res + calculateHash(this%d) * 4_8
        if (allocated(this%e)) res = res + calculateHash(this%e) * 5_8
        if (allocated(this%f)) res = res + calculateHash(this%f) * 6_8
        if (allocated(this%g)) res = res + calculateHash(this%g) * 7_8
        if (allocated(this%h)) res = res + calculateHash(this%h) * 8_8
        if (allocated(this%i)) res = res + calculateHash(this%i) * 9_8
        if (allocated(this%j)) res = res + calculateHash(this%j) * 10_8
    end function

end module fplus_tuple
