program test_regex
    use mod_regex
    implicit none

    type(regex) :: re
    re = new_regex("[a-z]+([0-9])+[a-z]+")
    print*, "The program is valid: ", re%is_valid()
    print*, "Last error code: ", re%last_error()
end program test_regex
