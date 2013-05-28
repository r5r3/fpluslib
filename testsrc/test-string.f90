program test_string
    use mod_fstd
    implicit none

    class(string), pointer :: str

    ! create a string object from a string
    str => string("test")

end program test_string
