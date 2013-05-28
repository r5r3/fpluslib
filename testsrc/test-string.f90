program test_string
    use mod_fstd
    implicit none

    class(string), pointer :: str
    character (len=100) :: test

    ! create a string object from a string
    test = "test"
    str => string("test")
    print*, str%hashcode()

end program test_string
