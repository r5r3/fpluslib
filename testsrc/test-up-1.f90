program test_up_1
    implicit none
    ! create an unlimited polymorphic pointer
    class(*), pointer :: up1
    ! create a real variable
    real, target :: test
    test = 7

    ! make the pointer point on the variable
    up1 => test

    ! print the number
    select type(up1)
        type is (real)
            print*, up1
    end select

end program test_up_1
