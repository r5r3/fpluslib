!> @brief This module contains type and interface definition for iterators.
module mod_iterator
    implicit none
    public

    !> @ brief  An abstract iterator class used by container classes to iterate over there values
    type, abstract :: iterator
    contains
        procedure (generic_hasnext), deferred :: hasnext
        procedure (generic_next), deferred :: next
    end type

    abstract interface
        function generic_hasnext(this)
            import :: iterator
            class(iterator) :: this
            logical :: generic_hasnext
        end function
        function generic_next(this)
            import :: iterator
            class(iterator) :: this
            class(*), pointer :: generic_next
        end function
    end interface

end module mod_iterator
