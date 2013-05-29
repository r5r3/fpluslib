program test_date
    use mod_date
    implicit none

    class(date), pointer :: now

    now => date()

end program test_date
