program test_regex
    use fplus_regex
    implicit none

    type(regex) :: re
    type(match), dimension(:), allocatable :: matches
    integer :: i
    character (len=:), allocatable :: string, pattern

    ! create an regex object
    pattern = "[a-z]+([0-9]+)"
    string = "agFfdg4657gfhfg"
    print*, "Pattern: ", pattern, " String: ", string
    re = new_regex(pattern)
    print*, re%to_string()
    print*, "Hashcode: ", re%hashcode()
    print*, "The program is valid: ", re%is_valid()
    print*, "Last error code: ", re%last_error(), " Message: ", re%last_error_msg()

    ! match a string
    matches = re%matches(string)
    print*, allocated(matches)
    print*, "Last error code: ", re%last_error(), " Message: ", re%last_error_msg()
    do i = 1, size(matches)
        print*, "Group:", i, "first:", matches(i)%first, "last:", matches(i)%last, "string: ", trim(matches(i)%string)
    end do
    call re%release()

    ! create a new case insensitive regex
    re = new_regex(pattern, REG_ICASE=.true.)
    print*, re%to_string()
    print*, "Hashcode: ", re%hashcode()
    matches = re%matches(string)
    print*, "Last error code: ", re%last_error(), " Message: ", re%last_error_msg()
    do i = 1, size(matches)
        print*, "Group:", i, "first:", matches(i)%first, "last:", matches(i)%last, "string: ", trim(matches(i)%string)
    end do
    call re%release()


end program test_regex
