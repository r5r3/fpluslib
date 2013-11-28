module fplus_arghelper
    use fplus_object
    use fplus_hashcode
    use fplus_container
    use fplus_path
    use fplus_error
    use fplus_strings

    !$FP interface template get_argument

contains

    !$FP template get_argument
        !$FP atype = {character(len=*)} {type(string)} {type(path)} {type(list)} {logical} {real, dimension(:), allocatable}
        !$FP do i = 1, 6
            !> @brief       get one or more values from a flag
            !> @param       flag    the flag to look for. Example: "--type"
            !> @param       values  get one or more values. If values is of type string or character, 
            !>              then only one value is searched. For a list more values are searched. 
            !>              for a logical no value is serached. It is only check if the flag is present or not.
            !> @param       eor     exit on error, default = .false.
            !> @param       lop     return a list as path objects instead of string objects
            !> @return      0 on success, 1 otherwise
            function get_argument(flag, values, eor, lop) result (res)
                character(len=*), intent(in) :: flag
                ${atype(i)} :: values
                logical, optional :: eor, lop
                integer :: res

                !local variables
                integer :: argc, i, nvalues
                logical :: found, eor_intern, lop_intern
                character(1000) :: temp
                type(string) :: temp2
                type(path) :: temp3
                type(list) :: temp4
                real :: temp5

                res = 0
                eor_intern = present(eor) 
                if (eor_intern) eor_intern = eor .eqv. .true.
                lop_intern = present(lop)
                if (lop_intern) lop_intern = lop .eqv. .true.

                ! get the number of arguments
                argc = command_argument_count()

                ! create an empty list for the values of type real
                temp4 = new_list()

                ! loop over all arguments
                found = .false.
                nvalues = 0
                !$FP if (i == 5) values = .false.
                do i = 1, argc
                    ! read the argument
                    call get_command_argument(i, temp)
                    if (found) then
                        ! next flag found?
                        if (temp(1:2) == "--" .and. temp /= flag) then
                            found = .false.
                            cycle
                        end if
                        ! it is a value
                        nvalues = nvalues + 1
                        !$FP if (i < 4) then
                            if (nvalues > 1) then
                                err = 1
                                if (eor_intern) call fplus_error_print("too many arguments for " // trim(flag), "get_argument")
                                return
                            else
                                !$FP if (i == 1) values = temp
                                !$FP if (i == 2) values%chars = temp
                                !$FP if (i == 3) values%name = temp
                            end if
                        !$FP end if
                        !$FP if (i == 4) then
                            if (.not. lop_intern) then
                                temp2%chars = temp
                                call values%add(temp2, copy=.true.)
                            else
                                temp3%name = temp
                                call values%add(temp3, copy=.true.)
                            end if
                        !$FP end if
                        !$FP if (i == 6) then
                            read (temp,*) temp5
                            call temp4%add(temp5, copy=.true.)
                            values = temp4
                        !$FP end if
                    end if
                    if (temp == flag) then 
                        found = .true.
                        !$FP if (i == 5) then 
                            values = .true.
                            return
                        !$FP end if
                    end if
                end do
                if (nvalues == 0) then 
                    res = 1
                    if (eor_intern) call fplus_error_print("flag or values not found for " // trim(flag), "get_argument")
                end if
            end function
        !$FP end do
    !$FP end template

end module
