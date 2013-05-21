program test_up_2

    implicit none
    character(len=50) :: test

    test = "A test case"
    call allocate_test(test)

contains

    subroutine allocate_test(var)
        class(*) :: var
        class(*), pointer :: copyofvar
        allocate(copyofvar, source=var)
    end subroutine

end program test_up_2
