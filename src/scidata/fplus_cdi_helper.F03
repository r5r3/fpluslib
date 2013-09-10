!> @brief   Some helper functions and type for the CDI api
module fplus_cdi_helper
    use, intrinsic :: ISO_C_BINDING
    use fplus_path
    use fplus_container
    use fplus_object
    use fplus_hashcode
    use fplus_strings
    use fplus_datetime
    use fplus_error
    use fplus_array_tools
    use fplus_geo_tools
    use mo_cdi
    implicit none
    private

    !> @brief   This objects represents collection of data file in netcdf or grib format
    type, public :: datapool
        !> @brief   A list of fileinfo objects
        type(list), allocatable, private :: files
        !> @brief   A list to track merged variables
        type(list), allocatable, private :: derived_variables
        !> @brief   open for reading ("r") or writing ("w")
        character (len=1), private :: status
    contains
        !> @brief   Get a pointer to the variable information for a given code and z-axsis.
        procedure, public :: get_variable => datapool_get_variable
        !> @brief   Create a new variable information object for writing
        procedure, public :: get_new_variable => datapool_get_new_variable
        !> @brief   Release the memory used by the datapool. That includes all variables
        procedure, public :: clear => datapool_clear
        !> @brief   Fix the variable definition of all included files
        procedure, public :: fix_variable_definition => datapool_fix_variable_definition
    end type

    !> @brief   This type is used internally from datapool and contains information about
    !>          open files.
    type :: fileinfo
        !> @brief   the name of the file
        type(path) :: filename
        !> @brief   the streamID of the file from streamOpenRead
        integer :: streamID = -1
        integer :: vlistID = -1
        integer :: nvars = -1
        integer :: taxisID = -1
        !> @brief   the index of the last defined timestep
        integer :: last_time_step = -1
        !> @brief   the contained variables, the list stores varinfo objects as pointers
        type(list), allocatable :: variables
        !> @brief   open for reading ("r") or writing ("w")
        character (len=1), private :: status
        !> @brief   the type of the file
        integer, private :: filetype
        !> @biref   the variable definition has be be finished before the first write operation
        logical :: fixed = .false.
    contains
        !> @brief   Fix the variable definition
        procedure, public :: fix_variable_definition => fileinfo_fix_variable_definition
        !> @brief   Release the memory used by the fileinfo. That includes all variables. Open streams are closed
        procedure, public :: clear => fileinfo_clear
    end type

    !> @brief   Some information about a variable in a file
    type, extends(object), public :: varinfo
        character (len=100), public :: name = "", standard_name = "", longname = "", units = ""
        ! the dimensions of this variable
        integer :: nlon = -1, nlat = -1, nzval = -1, ntime = 0
        !> @brief   An Array with the time in seconds since 1970
        real (kind=8), dimension(:), allocatable, public :: coord_time
        real (kind=8), dimension(:), allocatable, public :: coord_lon
        real (kind=8), dimension(:), allocatable, public :: coord_lat
        real (kind=8), dimension(:), allocatable, public :: coord_z
        integer, public :: code = -1, zaxistype = -1, gridtype = -1
        integer, private :: varID = -1
        integer, private :: zaxisID = -1
        integer, private :: gridID = -1
        real (kind=8), private :: fill_value = fplus_fill_realk8
        ! a pointer to the corresponding fileinfo object, only set if the variable belongs to one file only
        class(fileinfo), pointer, private :: finfo => null()
        ! a list of included variables for a merged variable
        type(list), allocatable, private :: merged_var_parts
        ! a pointer to the original variable if this is a derived variable
        class(varinfo), pointer :: parent => null()
        ! an array of indices used to cut this variable out of the parent variable
        ! memory order: (lon/lat/z, min/max)
        integer, dimension(:,:), allocatable :: parent_subscripts
        ! used to indicate that a derived variable is rotated in longitude direction
        integer :: rotated_by_npoints = 0
        ! is this variable constant in time? 
        logical :: is_constant = .false.
    contains
        !> @brief   Set the grib code for this variable
        procedure, public :: set_name => varinfo_set_name
        !> @brief   Set the grib code for this variable
        procedure, public :: set_code => varinfo_set_code
        !> @brief   Set the grid type of this variable, that will reset the coordinate arrays
        procedure, public :: set_gridtype => varinfo_set_gridtype
        !> @brief   Set the fill value for this variable
        procedure, public :: set_fill_value => varinfo_set_fill_value
        !> @brief       Get the fill value for this variable
        procedure, public :: get_fill_value => varinfo_get_fill_value
        !> @brief   Set a new zaxis
        !$FP generic, public :: set_zaxis => varinfo_set_zaxis
        !> @brief   Define a new time coordinate for this variable
        generic, public :: set_coord_time => varinfo_set_coord_time_real8, varinfo_set_coord_time_datetime_list
        procedure, private :: varinfo_set_coord_time_real8
        procedure, private :: varinfo_set_coord_time_datetime_list
        !> @brief   Has this variable multiplie time steps or is it a constant field like surface elevation?
        procedure, public :: set_constant => varinfo_set_constant
        !> @brief   The the index of a given level in the z-coordinate
        procedure, public :: get_index_of_level => varinfo_get_index_of_level
        !> @brief   Load the data for one time step from the input file, 2d or 3d field
        !$FP generic, public :: get_timestep_data => varinfo_get_timestep_data
        !> @brief   Save the data for one time step to the output file, 2d or 3d field
        !$FP generic, public :: set_timestep_data => varinfo_set_timestep_data
        !> @brief   Save the data for a constant 2d or 3d field to the output file
        !$FP generic, public :: set_constant_data => varinfo_set_constant_data
        procedure, private :: varinfo_set_timestep_data_intern
        ! precedures for the object type
        procedure, public :: to_string => varinfo_to_string
        procedure, public :: hashcode => varinfo_hashcode
        !> @brief   Release the memory used by the varinfo.
        procedure, public :: clear => varinfo_clear
        !> @brief   Add timesteps to the timeaxis
        generic, public :: add_timesteps => varinfo_add_timesteps_rk8, varinfo_add_timesteps_rk8dx
        procedure, private :: varinfo_add_timesteps_rk8
        procedure, private :: varinfo_add_timesteps_rk8dx
        !> @brief   Create a copy with the same properties as this object, but not associated with a file
        procedure, private :: derived_clone => varinfo_derived_clone
        !> @brief   Get the size of the underlying array in x-direction
        procedure, public :: get_x_size => varinfo_get_x_size
        !> @brief   Get the size of the underlying array in x-direction
        procedure, public :: get_y_size => varinfo_get_y_size        
    end type

    !list of public functions
    public :: new_datapool, to_varinfo

    ! interface to some function from the c api of CDI that are not already included in mo_cdi
    interface
        !> @private
        subroutine streamDefCompType(streamID,compType) bind(c,name='streamDefCompType')
            import :: c_int
            integer(c_int), value :: streamID
            integer(c_int), value :: compType
        end subroutine
        !> @private
        subroutine streamDefCompLevel(streamID,compLevel) bind(c,name='streamDefCompLevel')
            import :: c_int
            integer(c_int), value :: streamID
            integer(c_int), value :: compLevel
        end subroutine
    end interface

    ! @brief    it is possible to create a new datapool with one or more files.
    interface new_datapool
        module procedure new_datapool_1file, new_datapool_nfiles
    end interface

    ! a global list of created resources (grids, zaxis, etc.)
    !$FP do lst = list_zaxisID list_gridID list_varID list_taxisID list_vlistID
    class(list), allocatable :: ${lst}
    !$FP end do
contains

    ! procedures for the datapool object ==========================================================

    !> @brief       Create a new datapool from a array of path objects
    !> @details     Reading is possible from mulptiply files add ones, writing so fare only in single files
    !> @param[in]   infile      one path object that contains the filename
    !> @param[in]   status      open the files for reading ("r") or writing ("w"), optional, default = "r"
    function new_datapool_1file(infile, status) result (res)
        type(datapool) :: res
        type(path) :: infile
        character (len=1), intent(in), optional :: status
        res = new_datapool_nfiles((/infile/), status)
    end function

    !> @brief       Create a new datapool from a array of path objects
    !> @details     Reading is possible from mulptiply files add ones, writing so fare only in single files
    !> @param[in]   infiles     An Array of path objects that contain the filename(s)
    !> @param[in]   status      open the files for reading ("r") or writing ("w"), optional, default = "r"
    function new_datapool_nfiles(infiles, status) result (res)
        type(datapool) :: res
        type(path), dimension(:) :: infiles
        character (len=1), intent(in), optional :: status

        ! local variables
        integer :: i
        type(fileinfo), pointer :: finfo

        ! reading or writing
        if (present(status)) then
            res%status = status
        else
            res%status = "r"
        end if
        if (res%status /= "r" .and. res%status /= "w") then
            call fplus_error_print("unknown status: " // res%status, "new_datapool")
        end if

        ! so far, only one file for writing is supported
        if (size(infiles) > 1 .and. res%status == "w") call fplus_error_print("datapools with more than one file is so far only supported for reading", "new_datapool", FPLUS_ERR)

        ! create a new list for the files
        res%files = new_list()

        ! check at first if all infiles are present
        do i = 1, size(infiles)
            if (res%status == "r" .and. .not.infiles(i)%exists()) then
                call fplus_error_print("file not found: "//trim(infiles(i)%name), "new_datapool")
            end if

            ! put the fileinfo into the list
            call res%files%add(new_fileinfo(infiles(i), res%status))
        end do

        ! create lists to track cdi resources
        !$FP do lst = list_zaxisID list_gridID list_varID list_taxisID list_vlistID
        if (.not. allocated(${lst})) allocate(${lst})
        !$FP end do
    end function

    !> @brief       Get a pointer to the variable information for a given code, name, or standard_name and z-axsis.
    !> @details     if lon_range and/or lat range is present, then a subsection is created.
    !> @param[in]   this        reference to the datapool object, automatically set by fortran
    !> @param[in]   code        the grib code of the variable, optional
    !> @param[in]   zaxis       the z-axis type of the variable, optional
    !> @param[in]   name        the name of the variable, optional
    !> @param[in]   stdname     the standard_name of the variable, optional
    !> @param[in]   lon_range   an one dimensional array with the smallest and the largest lon value, optional
    !> @param[in]   lat_range   an one dimensional array with the smallest and the largest lat value, optional
    !> @return      null if nothing was found, else a pointer to a varinfo object
    function datapool_get_variable(this, code, zaxis, name, std_name, lon_range, lat_range) result (res)
        class(datapool) :: this
        integer, intent(in), optional :: code, zaxis
        character (len=*), intent(in), optional :: name, std_name
        real(kind=8), dimension(2), optional :: lon_range, lat_range
        class(varinfo), pointer :: res

        ! local variables
        type(listiterator), allocatable :: fileiter, variter
        class(fileinfo), pointer :: fileptr
        class(varinfo), pointer :: varptr
        class(*), pointer :: ptr
        type(list) :: matches
        integer :: n_qualifier, matching_qualifier, status
        logical, dimension(2) :: vinr
        real(kind=8), dimension(2) :: lon_range_temp
        real(kind=8), dimension(:), allocatable :: lon_temp
        integer, dimension(2) :: lon_type

        ! set the result to null in the case that nothing is found
        res => null()

	    ! check if any information about the variable is given
        n_qualifier = 0
        if (present(code)) n_qualifier = n_qualifier + 1
        if (present(zaxis)) n_qualifier = n_qualifier + 1
        if (present(name)) n_qualifier = n_qualifier + 1
        if (present(std_name)) n_qualifier = n_qualifier + 1
        if (n_qualifier == 0) then
            call fplus_error_print("no information about the variable given", "datapool%get_variable", FPLUS_WARN)
            return
        end if

        ! create a list for all matches
        matches = new_list()

        ! iterate over all files
        fileiter = this%files%get_iterator()
        do while (fileiter%hasnext())
            ptr => fileiter%next()
            fileptr => to_fileinfo(ptr)
            ! create an iterator over all variables of this file
            variter = fileptr%variables%get_iterator()
            do while (variter%hasnext())
                ptr => variter%next()
                varptr => to_varinfo(ptr)
                matching_qualifier = 0
                ! the zaxis if present has to match
                if (present(zaxis) .and. zaxis /= varptr%zaxistype) cycle
                ! one of the parameters name, code, or standard_name has to match
                if (present(code) .and. code == varptr%code) matching_qualifier = matching_qualifier + 1
                if (present(name) .and. trim(name) == trim(varptr%name)) matching_qualifier = matching_qualifier + 1
                if (present(std_name) .and. trim(std_name) == trim(varptr%standard_name)) matching_qualifier = matching_qualifier + 1
                if (matching_qualifier == 0) cycle
                ! add the variable to the list of matches
                call matches%add(varptr)
            end do
        end do

        ! if we only have one match, then return is
        if (matches%length() == 1) then
            ptr => matches%first()
            res => to_varinfo(ptr)
            call matches%clear()
        end if

        ! create a merged variable if more then one match was found
        if (matches%length() > 1) then
            variter = matches%get_iterator()
            do while (variter%hasnext())
                ptr => variter%next()
                varptr => to_varinfo(ptr)
                ! create a copy of the first variable
                if (.not.associated(res)) then
                    res => varptr%derived_clone()
                    ! create the list of merged parts within this variable
                    res%merged_var_parts = new_list()
                    call res%merged_var_parts%add(varptr)
                ! this is the second or an addition variable, add it to the merged variable
                else
                    ! the variables are only allowed to differ in the defined time steps
                    if (varptr%nlon /= res%nlon & 
                        .or. varptr%nlat /= res%nlat & 
                        .or. varptr%nzval /= res%nzval &
                        .or. trim(varptr%name) /= trim(res%name) &
                        .or. trim(varptr%standard_name) /= trim(res%standard_name) & 
                        .or. varptr%zaxistype /= res%zaxistype) then
                        call fplus_error_print("merged variables are only allowed to differ in their time steps", "datapool%get_variable_with_code_zaxis", FPLUS_WARN)
                        call res%clear()
                        deallocate(res)
                    end if
                    ! add this variable to the parts of the merged variable
                    call res%merged_var_parts%add(varptr)
                    ! merged the time axis
                    call res%add_timesteps(varptr%coord_time)
                end if
            end do
            ! add this variable to the list of merged variables
            if (.not.allocated(this%derived_variables)) this%derived_variables = new_list()
            call this%derived_variables%add(res)
        end if

        ! create a subsection if the variable was found and lon_range or lan_range are present
        if (associated(res) .and. (present(lon_range) .or. present(lat_range))) then
            ! create a copy of the original variable
            varptr => res%derived_clone(copy_llz=.false.)
            ! associate the parent pointer
            varptr%parent => res
            ! the new result if the copy
            res => varptr

            ! allocate the array for the subscripts
            allocate(res%parent_subscripts(3,2))

            ! determine the subscript indecies for lon
            lon_temp = res%parent%coord_lon
            if (present(lon_range)) then
                ! the longitude is a special case. it may be necessary to rotate the array before a subsection is created
                ! check at first if the range in complete within the coordinate range without changes anything
                vinr = value_in_range(lon_temp, lon_range)
                if (any(vinr .eqv. .false.)) then
                    ! longitude between 0 and 360 as well as -180 and 180 are used. can we solve the problem by switching between these representations?
                    ! which representation is used for the coordinate axis? 
                    if (any(lon_temp < 0)) then
                        lon_type(1) = -1
                    else
                        lon_type(1) = 1
                    end if
                    if (any(lon_range < 0)) then
                        lon_type(2) = -1
                    else
                        lon_type(2) = 1
                    end if
                    ! are the representation different? 
                    if (lon_type(1) /= lon_type(2)) then
                        ! convert the coordinate axis of the input array to the new type
                        if (lon_type(2) == -1) then
                            where (lon_temp > 180)
                                lon_temp = lon_temp - 360
                            end where
                        else
                            where (lon_temp < 0)
                                lon_temp = lon_temp + 360
                            end where                            
                        end if
                    end if
                end if
                ! check the order in which the values from the range occure
                if (closest_index(lon_temp, lon_range(1)) > closest_index(lon_temp, lon_range(2))) then
                    if (lon_is_cyclic(lon_temp)) then
                        call rot_lon_to_range(lon_range, lon_temp, lon_temp, npoints=res%rotated_by_npoints)
                    else
                        call fplus_error_print("the array needs to be rotated in longitude direction but is not cyclic!", "datapool%get_variable", FPLUS_ERR)
                    end if
                end if
                ! create the subsection
                if (res%parent%coord_lon(lbound(lon_temp, 1)) < res%parent%coord_lon(ubound(lon_temp, 1))) then
                    res%parent_subscripts(1,1) = valueindex_largest_le(lon_temp, lon_range(1))
                    res%parent_subscripts(1,2) = valueindex_smallest_ge(lon_temp, lon_range(2))
                else
                    res%parent_subscripts(1,1) = valueindex_largest_ge(lon_temp, lon_range(2))
                    res%parent_subscripts(1,2) = valueindex_smallest_le(lon_temp, lon_range(1))
                end if
            else
                res%parent_subscripts(1,1) = lbound(lon_temp, 1)
                res%parent_subscripts(1,2) = ubound(lon_temp, 1)
            end if

            ! determine the subscript indecies for lat
            if (present(lat_range)) then
                if (res%parent%coord_lat(lbound(res%parent%coord_lat, 1)) < res%parent%coord_lat(ubound(res%parent%coord_lat, 1))) then
                    res%parent_subscripts(2,1) = valueindex_largest_le(res%parent%coord_lat, lat_range(1))
                    res%parent_subscripts(2,2) = valueindex_smallest_ge(res%parent%coord_lat, lat_range(2))
                else
                    res%parent_subscripts(2,1) = valueindex_largest_ge(res%parent%coord_lat, lat_range(2))
                    res%parent_subscripts(2,2) = valueindex_smallest_le(res%parent%coord_lat, lat_range(1))
                end if
            else
                res%parent_subscripts(2,1) = lbound(res%parent%coord_lat, 1)
                res%parent_subscripts(2,2) = ubound(res%parent%coord_lat, 1)
            end if

            ! the indecies for z are fixed for the moment
            res%parent_subscripts(3,1) = lbound(res%parent%coord_z, 1)
            res%parent_subscripts(3,2) = ubound(res%parent%coord_z, 1)

            ! check for valid indices
            if (any(res%parent_subscripts(1,:) == fplus_fill_int)) call fplus_error_print("longitude subsection not available in input file!", "datapool%get_variable_with_code_zaxis", FPLUS_WARN)
            if (any(res%parent_subscripts(2,:) == fplus_fill_int)) call fplus_error_print("latitude subsection not available in input file!", "datapool%get_variable_with_code_zaxis", FPLUS_WARN)
            if (any(res%parent_subscripts(3,:) == fplus_fill_int)) call fplus_error_print("z level subsection not available in input file!", "datapool%get_variable_with_code_zaxis", FPLUS_WARN)
            if (any(res%parent_subscripts == fplus_fill_int)) then
                deallocate(res)
                return
            end if

            ! create the coordinate arrays
            res%coord_lon = lon_temp(res%parent_subscripts(1,1):res%parent_subscripts(1,2))
            res%coord_lat = res%parent%coord_lat(res%parent_subscripts(2,1):res%parent_subscripts(2,2))
            res%coord_z   = res%parent%coord_z(res%parent_subscripts(3,1):res%parent_subscripts(3,2))
            res%nlon = size(res%coord_lon)
            res%nlat = size(res%coord_lat)
            res%nzval = size(res%coord_z)
            ! if the derived variable if identical to the original variable, then return the original variable
            ! as the coordinates are created out of the parent coordinate, same size means same values. 
            if (size(res%parent%coord_lon) == size(res%coord_lon)           &
                .and. size(res%parent%coord_lat) == size(res%coord_lat)     &
                .and. size(res%parent%coord_z) == size(res%coord_z)) then
                varptr => res%parent
                deallocate(res)
                res => varptr
            else
                ! track the variable
                if (.not.allocated(this%derived_variables)) this%derived_variables = new_list()
                call this%derived_variables%add(res)
            end if
        end if
    end function

    !> @public
    !> @brief       Create a new variable information object for writing
    !> @details     A new varinfo object if created and assigned to the first open file.
    !>              The object is deallocaed when the file is closed
    !> @param[in]   this        reference to the datapool object, automatically set by fortran
    !> @param[in]   template    is present, the new variable will have the same coordniates, names and properties
    !>                          like the template variable. Optional.
    !> @param[in]   copy_time   copy the time coordinate from the template, default is true
    function datapool_get_new_variable(this, template, copy_time) result (res)
        class(datapool) :: this
        class(varinfo), pointer :: res
        class(varinfo), optional :: template
        logical, optional :: copy_time

        ! local variables
        class(fileinfo), pointer :: fileptr
        class(*), pointer :: ptr

        ! copy properties from the template
        if (present(template)) then
            res => template%derived_clone(copy_time=copy_time)
        else
            allocate(res)
        end if

        ! place the new variable into the first file
        ptr => this%files%first()
        fileptr => to_fileinfo(ptr)
        call fileptr%variables%add(res)
        res%finfo => fileptr
    end function

    !> @brief       Release the memory used by the datapool. That includes all variables
    !> @param[in]   this    reference to the datapool object, automatically set by fortran
    subroutine datapool_clear(this)
        class(datapool) :: this

        ! local variables
        type(listiterator), allocatable :: iter
        class(fileinfo), pointer :: ptr
        class(varinfo), pointer :: varptr
        class(*), pointer :: temp
        integer :: status
        character (len=100) :: errmsg

        ! loop over all files in this pool
        iter = this%files%get_iterator()
        do while (iter%hasnext())
            temp => iter%next()
            ptr => to_fileinfo(temp)
            call ptr%clear()
            deallocate(ptr, stat=status, errmsg=errmsg)
            if (status /= 0) call fplus_error_print(errmsg, "datapool_clear", FPLUS_WARN)
        end do
        call this%files%clear()
        deallocate(this%files)

        ! are there any merged variables within this pool
        if (allocated(this%derived_variables)) then
            iter = this%derived_variables%get_iterator()
            do while (iter%hasnext())
                temp => iter%next()
                varptr => to_varinfo(temp)
                call varptr%clear()
                deallocate(varptr)
            end do
            deallocate(this%derived_variables)
        end if
    end subroutine

    !> @brief   Fix the variable definition of all included files
    subroutine datapool_fix_variable_definition(this)
        class(datapool) :: this 

        ! local variables
        integer :: i
        class(*), pointer :: ptr
        class(fileinfo), pointer :: fptr

        ! loop over all variables
        do i = 1, this%files%length()
            ptr => this%files%get(i)
            fptr => to_fileinfo(ptr)
            call fptr%fix_variable_definition()
        end do
    end subroutine


    ! precedures for the fileinfo object ==========================================================

    !> @brief       Create an initiated fileinfo object that already contains the metadata from the file
    !>              if the file is open for reading
    !> @param[in]   filename    the abstract path name of the file to open
    !> @param[in]   status      open the files for reading ("r") or writing ("w"), optional, default = "r"
    !> @returns     A pointer to the new fileinfo object
    function new_fileinfo(filename, status) result (res)
        type(path) :: filename
        class(fileinfo), pointer :: res
        character (len=1), intent(in), optional :: status

        ! local variables
        type(list), allocatable :: ls
        class(varinfo), pointer :: info
        type(datetime) :: dt
        real (kind=8), dimension(:), allocatable :: timeaxis
        integer :: tsID, stat, vdate, vtime, varID, zaxisID, gridID, taxisID, nlon, nlat, nzval
        character (len=:), allocatable :: extension
        info => null()
        
        ! allocate memory for the new object
        nullify(res)
        allocate(fileinfo :: res)

        ! reading or writing
        if (present(status)) then
            res%status = status
        else
            res%status = "r"
        end if
        if (res%status /= "r" .and. res%status /= "w") then
            call fplus_error_print("unknown status: " // res%status, "new_fileinfo")
        end if

        ! store the filename
        res%filename = filename

        ! create the list for the variables
        res%variables = new_list()

        select case (status)
            ! open the file for READING
            case ("r")
                ! open the file with CDI
                res%streamID = streamOpenRead(filename%get_cstr_name())
                if (res%streamID < 0) then
                    call fplus_error_print(cdiStringError(res%streamID), "new_fileinfo")
                end if

                ! Get the variable list of the file
                res%vlistID = streamInqVlist(res%streamID)
                res%nvars = vlistNvars(res%vlistID)

                ! Get the Time axis form the variable list
                taxisID = vlistInqTaxis(res%vlistID)
                ls = new_list()

                ! loop over all timesteps to create a timeaxis variable
                tsID = 0
                do
                    ! Read the time step
                    stat = streamInqTimestep(res%streamID, tsID)
                    if ( stat == 0 ) exit

                    ! Get the verification date and time
                    vdate = taxisInqVdate(taxisID)
                    vtime = taxisInqVtime(taxisID)

                    ! convert to datetime object and store in a list
                    dt = new_datetime(idate=vdate, itime=vtime)
                    call ls%add(dt, copy=.true.)

                    tsID = tsID + 1
                end do
                timeaxis = ls

                ! release the memory of the list
                call ls%clear()
                deallocate(ls)

                ! read the variable infos in a loop over all files
                do varID = 0, res%nvars-1
                    info => null()
                    allocate(varinfo :: info)
                    ! get information from the input file
                    info%code = vlistInqVarCode(res%vlistID, varID)
                    call vlistInqVarName(res%vlistID, varID, info%name)
                    call ctrim(info%name)
                    call vlistInqVarLongname(res%vlistID, varID, info%longname)
                    call ctrim(info%longname)
                    call vlistInqVarUnits(res%vlistID, varID, info%units)
                    call ctrim(info%units)
                    call vlistInqVarStdname(res%vlistID, varID, info%standard_name)
                    call ctrim(info%standard_name)
                    info%fill_value = vlistInqVarMissval(res%vlistID, varID)

                    ! add the id of the variable
                    info%varID = varID

                    ! add the type of the zaxis and the timeaxis
                    zaxisID = vlistInqVarZaxis(res%vlistID, varID)
                    info%zaxistype = zaxisInqType(zaxisID)
                    info%coord_time = timeaxis
                    info%ntime = size(timeaxis)

                    ! read the grid
                    gridID = vlistInqVarGrid(res%vlistID, info%varID)

                    ! we only support lonlat and gaussian grids
                    info%gridtype = gridInqType(gridID)
                    if (info%gridtype /= GRID_LONLAT .and. info%gridtype /= GRID_GAUSSIAN .and. info%gridtype /= GRID_UNSTRUCTURED) then
                        call fplus_error_print("unsupported grid: " // trim(type_to_string(info%gridtype)), "new_fileinfo")
                    end if

                    ! allocate the lon and lat coordinate arrays
                    info%nlon = gridInqXsize(gridID)
                    info%nlat = gridInqYsize(gridID)
                    allocate(info%coord_lon(info%nlon))
                    allocate(info%coord_lat(info%nlat))

                    ! store the lat and lon coordinates
                    stat = gridInqXvals(gridID, info%coord_lon)
                    stat = gridInqYvals(gridID, info%coord_lat)

                    ! store the zaxis coordinates
                    info%nzval = zaxisInqSize(zaxisID)
                    allocate(info%coord_z(info%nzval))
                    call zaxisInqLevels(zaxisID, info%coord_z)

                    ! store a pointer to the fileinfo object
                    info%finfo => res

                    ! store the info in the variables list
                    call res%variables%add(info)
                    nullify(info)
                end do

            ! open the file for READING
            case ("w")
                ! try to determine the filetype based in the file extension
                extension = filename%get_extension()
                select case (extension)
                    case ("nc", "nc3")
                        res%filetype = FILETYPE_NC
                    case ("nc4")
                        res%filetype = FILETYPE_NC4
                    case ("grb")
                        res%filetype = FILETYPE_GRB
                    case ("grb2")
                        res%filetype = FILETYPE_GRB2
                    case default
                        res%filetype = FILETYPE_NC4
                        call fplus_error_print("unknown file extension, the created file will be netCDF4", "new_fileinfo", FPLUS_WARN)
                end select
                ! open type file
                res%streamID = streamOpenWrite(filename%get_cstr_name(), res%filetype)
                if (res%streamID < 0) then
                    call fplus_error_print(cdiStringError(res%streamID), "new_fileinfo")
                end if
                ! set the default compression level 5 for netcdf 4files
                if (res%filetype == FILETYPE_NC4) then
                    call streamDefCompType(res%streamID, COMPRESS_ZIP)
                    call streamDefCompLevel(res%streamID, 5)
                end if
        end select
    end function

    !> @brief   Convert a class(*) pointer to fileinfo pointer
    function to_fileinfo(input) result (res)
        class(fileinfo), pointer :: res
        class(*), pointer, intent(in) :: input
        res => null()
        select type (input)
            class is (fileinfo)
                res => input
            class default
                call fplus_error_print("wrong data type", "to_fileinfo")
        end select
    end function

    !> @brief       Fix the variable definition
    !> @param[in]   this    reference to the varinfo object, automatically set by fortran
    subroutine fileinfo_fix_variable_definition(this)
        class(fileinfo) :: this

        ! local variables
        type(listiterator), allocatable :: iter
        class(varinfo), pointer :: ptr, ptr2
        class(*), pointer :: temp
        integer :: gridsize
        integer :: i

        ! leave this subroutine directly if the variable definition is already fixed
        if (this%fixed) return

        ! Create a variable list
        this%vlistID = vlistCreate()
        call list_vlistID%add(this%vlistID, copy=.true.)

        ! create a time axis
        this%taxisID = taxisCreate(TAXIS_ABSOLUTE)
        call list_taxisID%add(this%taxisID, copy=.true.)
        call vlistDefTaxis(this%vlistID, this%taxisID)

        ! loop over all variables in this file
        iter = this%variables%get_iterator()
        do while (iter%hasnext())
            temp => iter%next()
            ptr => to_varinfo(temp)

            ! create the grid if not already done
            if (ptr%gridID == -1) then
                if (ptr%gridtype == -1) then
                    call fplus_error_print("the grid type has not yet been defined", "fileinfo%fix_variable_definition")
                else if (ptr%gridtype == GRID_LONLAT .or. ptr%gridtype == GRID_GAUSSIAN) then
                    ptr%gridID = gridCreate(GRID_LONLAT, ptr%nlon*ptr%nlat)
                    call list_gridID%add(ptr%gridID, copy=.true.)
                    call gridDefXsize(ptr%gridID, ptr%nlon)
                    call gridDefYsize(ptr%gridID, ptr%nlat)
                    if (allocated(ptr%coord_lon)) call gridDefXvals(ptr%gridID, ptr%coord_lon)
                    if (allocated(ptr%coord_lon)) call gridDefYvals(ptr%gridID, ptr%coord_lat)
                else if (ptr%gridtype == GRID_GENERIC) then
                    gridsize = 1
                    if (ptr%nlon > 0) gridsize = gridsize * ptr%nlon
                    if (ptr%nlat > 0) gridsize = gridsize * ptr%nlat                        
                    ptr%gridID = gridCreate(GRID_GENERIC, gridsize)
                    call list_gridID%add(ptr%gridID, copy=.true.)
                    if (ptr%nlon > 0) call gridDefXsize(ptr%gridID, ptr%nlon)
                    if (ptr%nlat > 0) call gridDefYsize(ptr%gridID, ptr%nlat)
                    if (allocated(ptr%coord_lon)) call gridDefXvals(ptr%gridID, ptr%coord_lon)
                    if (allocated(ptr%coord_lon)) call gridDefYvals(ptr%gridID, ptr%coord_lat)
                else if (ptr%gridtype == GRID_UNSTRUCTURED) then
                    if (ptr%nlon /= ptr%nlat .or. ptr%nlon == 0) then
                        call fplus_error_print("the size of the x and y coordinate of a unstructured grid has to match!", "fileinfo%fix_variable_definition")
                    end if
                    gridsize = ptr%nlon
                    ptr%gridID = gridCreate(GRID_UNSTRUCTURED, gridsize)
                    call list_gridID%add(ptr%gridID, copy=.true.)
                    call gridDefXsize(ptr%gridID, ptr%nlon)
                    call gridDefYsize(ptr%gridID, ptr%nlat)
                    if (allocated(ptr%coord_lon)) call gridDefXvals(ptr%gridID, ptr%coord_lon)
                    if (allocated(ptr%coord_lon)) call gridDefYvals(ptr%gridID, ptr%coord_lat)
                else
                    call fplus_error_print("unsupported grid type: " // trim(type_to_string(ptr%gridtype)), "fileinfo%fix_variable_definition")
                end if
            end if

            ! create a z-axis if needed
            if (ptr%zaxisID == -1) then
                if (ptr%zaxistype == -1) call fplus_error_print("zaxistype not set!", "fileinfo%fix_variable_definition")
                ptr%zaxisID = zaxisCreate(ptr%zaxistype, ptr%nzval)
                call list_zaxisID%add(ptr%zaxisID, copy=.true.)
                if (size(ptr%coord_z) /= ptr%nzval) call fplus_error_print("size of zaxis coordinate array and nzval don't fit together", "fileinfo%fix_variable_definition")
                call zaxisDefLevels(ptr%zaxisID, ptr%coord_z)
                ! are there other variables with the same zaxis?
                do i = 1, this%variables%length()
                    temp => this%variables%get(i)
                    ptr2 => to_varinfo(temp)
                    if (ptr2%zaxisID == -1 &
                        .and. ptr2%zaxistype == ptr%zaxistype &
                        .and. allocated(ptr2%coord_z) &
                        .and. allocated(ptr%coord_z) &
                        .and. size(ptr2%coord_z) == size(ptr%coord_z) & 
                        .and. all(ptr2%coord_z == ptr%coord_z)) then
                        ptr2%zaxisID = ptr%zaxisID
                    end if
                end do
            end if

            ! define the variable if needed
            if (ptr%varID == -1) then
                !call cdiDebug(2)
                ! create a new variable
                if (ptr%is_constant .eqv. .false.) then
                    ptr%varID = vlistDefVar(this%vlistID, ptr%gridID, ptr%zaxisID, 1) !TSTEP_INSTANT
                    call list_varID%add(ptr%varID, copy=.true.)
                else
                    ptr%varID = vlistDefVar(this%vlistID, ptr%gridID, ptr%zaxisID, 0) !TSTEP_CONSTANT
                    call list_varID%add(ptr%varID, copy=.true.)
                end if
                ! define the name
                if (len_trim(ptr%name) == 0 .and. ptr%code > 0) ptr%name = "var" // type_to_string(ptr%code)
                if (len_trim(ptr%name) == 0 .and. ptr%code <= 0) ptr%name = "var" // type_to_string(ptr%varID)
                call vlistDefVarName(this%vlistID, ptr%varID, trim(ptr%name) // C_NULL_CHAR)
                if (len_trim(ptr%longname) > 0) call vlistDefVarLongname(this%vlistID, ptr%varID, trim(ptr%longname) // C_NULL_CHAR)
                ! define the code
                if (ptr%code /= -1) call vlistDefVarCode(this%vlistID, ptr%varID, ptr%code)
                ! define units
                if (len_trim(ptr%units) > 0) call vlistDefVarUnits(this%vlistID, ptr%varID, trim(ptr%units) // C_NULL_CHAR)
                ! define the missing value
                call vlistDefVarMissval(this%vlistID, ptr%varID, ptr%fill_value)
            end if
        end do

        ! define the variables in the stream
        call streamDefVlist(this%streamID, this%vlistID)
        ! mark is file as fixed
        this%fixed = .true.
    end subroutine

    !> @brief       Release the memory used by the fileinfo. That includes all variables. Open streams are closed
    !> @param[in]   this    reference to the datapool object, automatically set by fortran
    subroutine fileinfo_clear(this)
        class(fileinfo) :: this

        ! local variables
        type(listiterator), allocatable :: iter
        class(varinfo), pointer :: ptr
        class(*), pointer :: temp
        integer :: status, index
        character (len=100) :: errmsg

        ! loop over all variables in this file
        iter = this%variables%get_iterator()
        do while (iter%hasnext())
            temp => iter%next()
            ptr => to_varinfo(temp)
            call ptr%clear()
            deallocate(ptr, stat=status, errmsg=errmsg)
            if (status /= 0) call fplus_error_print(errmsg, "fileinfo_clear", FPLUS_WARN)
        end do
        call this%variables%clear()
        deallocate(this%variables)

        ! close the stream
        if (this%streamID >= 0) call streamClose(this%streamID)
        this%streamID = -1

        ! destroy objects used for writing
        if (this%status == "w") then
            if (this%vlistID >= 0) then 
                index = list_vlistID%get_position(this%vlistID)
                if (index > 0) then
                    call vlistDestroy(this%vlistID)
                    call list_vlistID%remove(index)
                end if
            end if
            if (this%taxisID >= 0) then 
                index = list_taxisID%get_position(this%taxisID)
                if (index > 0) then
                    call taxisDestroy(this%taxisID)
                    call list_taxisID%remove(index)
                end if
            end if
        end if
    end subroutine


    ! procedures for the type varinfo =============================================================

    !> @public
    !> @brief       Set the name of the variable
    !> @param[in]   this        reference to the varinfo object, automatically set by fortran
    !> @param[in]   name        the new name of the variable
    !> @param[in]   longname    the new longname of the variable
    subroutine varinfo_set_name(this, name, longname, units)
        class(varinfo) :: this
        character(len=*), optional :: name, longname, units
        ! local variables
        integer :: i
        if (present(name)) then
            i = index(name, " ")
            if (i > 0) then
                call fplus_error_print("invalid variable name: " // name, "varinfo%set_name", FPLUS_WARN)
            else
                this%name = name
            end if
        end if
        if (present(longname)) this%longname = longname
        if (present(units)) this%units = units
    end subroutine

    !> @public
    !> @brief       Set the grib code for this variable
    !> @param[in]   this        reference to the varinfo object, automatically set by fortran
    !> @param[in]   new_code    the value of the new code. Valid are values between 0 and 255. Use -1 to unset the code.
    subroutine varinfo_set_code(this, new_code)
        class(varinfo) :: this
        integer, intent(in) :: new_code
        if (new_code < -1 .or. new_code > 255) then
            call fplus_error_print("invalid grib code: " // type_to_string(new_code), "varinfo%set_code", FPLUS_WARN)
        end if
        this%code = new_code
    end subroutine

    !> @public
    !> @brief       Set the grid type of this variable, that will reset the coordinate arrays
    !> @param[in]   this        reference to the varinfo object, automatically set by fortran
    !> @param[in]   new_type    the cdi grid type parameter. Allowed are GRID_GAUSSIAN, GRID_LONLAT, and GRID_UNSTRUCTURED
    subroutine varinfo_set_gridtype(this, new_type)
        class(varinfo) :: this
        integer, intent(in) :: new_type
        ! is the gridtype valid?
        if (new_type /= GRID_UNSTRUCTURED .and. new_type /= GRID_LONLAT .and. new_type /= GRID_GAUSSIAN) then
            call fplus_error_print("invalid grid type: " // type_to_string(new_type), "varinfo%set_gridtype", FPLUS_ERR)
        end if
        ! ok, the type is valid. remove the x- and y-coordinates
        this%gridtype = new_type
        if (allocated(this%coord_lon)) deallocate(this%coord_lon)
        if (allocated(this%coord_lat)) deallocate(this%coord_lat)
        this%nlon = -1
        this%nlat = -1
    end subroutine

    !> @public
    !> @brief       Set the fill value for this variable
    !> @param[in]   this        reference to the varinfo object, automatically set by fortran
    !> @param[in]   fill_value  the new fill_value
    subroutine varinfo_set_fill_value(this, fill_value)
        class(varinfo) :: this
        real (kind=8) :: fill_value

        ! check if the file containing this variable is already fixed
        if (associated(this%finfo) .and. this%finfo%fixed .eqv. .true.) then
            call fplus_error_print("fill_value for variable " // trim(this%name) // " not changed. file is already fixed!", "varinfo%set_fill_value")        
        end if

        ! store the fill_value
        this%fill_value = fill_value
    end subroutine

    !> @public
    !> @brief       Get the fill value for this variable
    !> @param[in]   this        reference to the varinfo object, automatically set by fortran
    function varinfo_get_fill_value(this) result (res)
        class(varinfo) :: this
        real (kind=8) :: res
        res = this%fill_value
    end function

    !$FP template varinfo_set_zaxis
        !$FP T = {real(kind=8)} {real(kind=8), dimension(:)}
        !$FP COORD_SIZE = 1 {size(coord_z)}
    
        !$FP do i = 1, 2
        !> @public
        !> @brief       Define a new zaxis for this variable
        !> @param[in]   this        reference to the varinfo object, automatically set by fortran
        subroutine varinfo_set_zaxis(this, coord_z, type_z)
            class(varinfo) :: this
            ${T(i)}, intent(in) :: coord_z
            integer :: type_z

            ! remove the old coordinate
            if (allocated(this%coord_z)) deallocate(this%coord_z)

            ! create a new coordinate axis
            allocate(this%coord_z(${COORD_SIZE(i)}))

            ! copy the values
            this%coord_z = coord_z

            ! store the type
            this%zaxistype = type_z
            this%nzval = ${COORD_SIZE(i)}
        end subroutine
        !$FP end do
    !$FP end template

    !> @public
    !> @brief       Has this variable multiplie time steps or is it a constant field like surface elevation?
    !> @parma[in]   this            reference to the varinfo object, automatically set by fortran
    !> @parma[in]   is_constant     .true. = no time steps, .false. = multiple time steps
    subroutine varinfo_set_constant(this, is_constant)
        class(varinfo) :: this
        logical, intent(in) :: is_constant
        this%is_constant = is_constant
    end subroutine

    !> @public
    !> @brief       Define a new time coordinate for this variable
    !> @param[in]   this            reference to the varinfo object, automatically set by fortran
    !> @param[in]   new_coord_time  an array with seconds since 1970
    subroutine varinfo_set_coord_time_real8(this, new_coord_time)
        class(varinfo) :: this
        real (kind=8), dimension(:), allocatable, intent(in) :: new_coord_time

        this%ntime = size(new_coord_time)
        this%coord_time = new_coord_time
    end subroutine

    !> @public
    !> @brief       Define a new time coordinate for this variable
    !> @param[in]   this            reference to the varinfo object, automatically set by fortran
    !> @param[in]   new_coord_time  a list with datetime objects
    subroutine varinfo_set_coord_time_datetime_list(this, new_coord_time)
        class(varinfo) :: this
        class(list), intent(in) :: new_coord_time

        this%ntime = new_coord_time%length()
        this%coord_time = new_coord_time
    end subroutine

    !> @brief       The the index of a given level in the z-coordinate
    !> @param[in]   this    reference to the varinfo object, automatically set by fortran
    !> @return      -1 if the level was not found, else the index in the coordinate array
    function varinfo_get_index_of_level(this, level) result (res)
        class(varinfo) :: this
        real (kind=8) :: level
        integer :: res

        ! local variables
        integer :: i

        ! find the level
        res = -1
        do i = 1, size(this%coord_z)
            if (this%coord_z(i) == level) then
                res = i
                return
            end if
        end do
    end function

    !$FP template varinfo_get_timestep_data
        !$FP D = {dimension(:,:)} {dimension(:,:,:)}
        !$FP D2 = 2 3
        !$FP ZLEVID = {} {,ind}
        !$FP AL = {this%get_x_size(), this%get_y_size()} {this%get_x_size(), this%get_y_size(), this%nzval}
        !$FP AL2 = {this%get_x_size(), this%get_y_size()} {this%get_x_size(), this%get_y_size(), size(levels)}
        !$FP AL_PARENT = {this%parent%get_x_size(), this%parent%get_y_size()} {this%parent%get_x_size(), this%parent%get_y_size(), this%parent%nzval}
        !$FP AL2_PARENT = {this%parent%get_x_size(), this%parent%get_y_size()} {this%parent%get_x_size(), this%parent%get_y_size(), size(levels)}
        !$FP PSUB3 = {} {,this%parent_subscripts(3,1):this%parent_subscripts(3,2)}
        !$FP PSUB32 = {} {, :}
        !$FP SHC = {res_shape(1) /= this%get_x_size() .or. res_shape(2) /= this%get_y_size()} {res_shape(1) /= this%get_x_size() .or. res_shape(2) /= this%get_y_size() .or. res_shape(3) /= this%nzval}
        !$FP SHC2 = {res_shape(1) /= this%get_x_size() .or. res_shape(2) /= this%get_y_size()} {res_shape(1) /= this%get_x_size() .or. res_shape(2) /= this%get_y_size() .or. res_shape(3) /= size(levels)}
    
        !$FP do i = 1, 2
        !> @public
        !> @brief       Load D2-d-field data for one time step from the input file
        !> @details     The timestep can be specified by its number, by its time in seconds since 1970, or by
        !>              a datetime object.
        !> @param[in]   this    reference to the varinfo object, automatically set by fortran
        !> @param[out]  res     the array where the data is stored
        !> @param[in]   tstep   the number of the time-step, first = 1, optional
        !> @param[in]   stime   the time in seconds since 1970 for this time step, optional
        !> @param[in]   dt      the time as a datetime object, optional
        !> @param[in]   levels  an array von level indices (1 .. nzval), only usable for 3d variables, optional
        recursive subroutine varinfo_get_timestep_data(this, res, tstep, stime, dt, levels)
            class(varinfo) :: this
            integer, intent(in), optional :: tstep
            real (kind=8), intent(in), optional :: stime
            class(datetime), intent(in), optional :: dt
            real (kind=8), ${D(i)}, allocatable :: res
            integer, dimension(:), optional :: levels

            ! local variables
            integer :: ind = fplus_fill_int
            integer :: ind2 = fplus_fill_int
            integer :: nmiss, status
            integer, dimension(${D2(i)}) :: res_shape
            integer :: streamID, varID
            type(listiterator) :: iter
            class(varinfo), pointer :: varptr
            class(*), pointer :: ptr
            real (kind=8), ${D(i)}, allocatable :: temp

            ! is this variable 2d or 3d?
            if (${D2(i)} == 2 .and. this%nzval > 1) call fplus_error_print(this%to_string() // " is not 2-dimensional!", "varinfo%get_timestep_data")
            if (${D2(i)} == 3 .and. this%nzval == 1) call fplus_error_print(this%to_string() // " is not 3-dimensional!", "varinfo%get_timestep_data")
            if (${D2(i)} == 2 .and. present(levels)) call fplus_error_print(this%to_string() // " is not 3-dimensional! Don't use the levels argument!", "varinfo%get_timestep_data")
            
            ! at least one valid parameter must be present
            if (.not.present(tstep) .and. .not.present(stime) .and. .not.present(dt)) then
                call fplus_error_print("no time step or time of the time step is given!", "varinfo%get_timestep_data")
            end if

            ! get the time step
            if (present(tstep)) ind = tstep
            if (present(dt)) ind = valueindex(this%coord_time, dt%time_in_sec1970, 1, this%ntime, sorted=.true.)
            if (ind == fplus_fill_int) call fplus_error_print("stime not yet implemented", "varinfo%get_timestep_data")

            ! allocate the array for the result
            ! only allocate the array, if it is not allocated or if it is allocated and the shape is wrong
            if (.not.present(levels)) then
                if (.not.allocated(res)) then
                    allocate(res(${AL(i)}))
                else
                    res_shape = shape(res)
                    if (${SHC(i)}) then
                        deallocate(res)
                        allocate(res(${AL(i)}))
                    end if
                end if
            else
                if (.not.allocated(res)) then
                    allocate(res(${AL2(i)}))
                else
                    res_shape = shape(res)
                    if (${SHC2(i)}) then
                        deallocate(res)
                        allocate(res(${AL2(i)}))
                    end if
                end if                
            end if

            ! is it a derived variable? if so, call this function recursivly
            if (associated(this%parent)) then
                ! create an temporal array for the data of the parent variable
                if (present(levels)) then
                    allocate(temp(${AL2_PARENT(i)}))
                else
                    allocate(temp(${AL_PARENT(i)}))
                end if
                ! get the data from the parent variable
                call this%parent%get_timestep_data(temp, tstep, stime, dt, levels)
                ! is there the need to rotate this array
                if (this%rotated_by_npoints /= 0) call rotate_array(temp, this%rotated_by_npoints)
                ! assign the subsection to the result array
                if (.not.present(levels)) then 
                    res = temp(this%parent_subscripts(1,1):this%parent_subscripts(1,2), this%parent_subscripts(2,1):this%parent_subscripts(2,2) ${PSUB3(i)})
                else
                    res = temp(this%parent_subscripts(1,1):this%parent_subscripts(1,2), this%parent_subscripts(2,1):this%parent_subscripts(2,2) ${PSUB32(i)})
                end if
                ! deallocate the temporal array
                deallocate(temp)
                return
            end if

            ! is it a merged variable?
            if (allocated(this%merged_var_parts)) then
                ! we have to find the variable to which this time step belongs
                iter = this%merged_var_parts%get_iterator()
                do while(iter%hasnext())
                    ptr => iter%next()
                    varptr => to_varinfo(ptr)
                    ! is this the right variable?
                    if (varptr%coord_time(1) <= this%coord_time(ind) .and. varptr%coord_time(varptr%ntime) >= this%coord_time(ind)) then
                        ind2 = valueindex(varptr%coord_time, this%coord_time(ind), 1, varptr%ntime, sorted=.true.)
                        if (ind2 /= fplus_fill_int) then
                            ind = ind2
                            streamID = varptr%finfo%streamID
                            varID = varptr%varID
                            exit
                        end if
                    end if
                end do
                if (ind2 == fplus_fill_int) call fplus_error_print("time step not found in merged variable parts", "varinfo%get_timestep_data")
            else
                streamID = this%finfo%streamID
                varID = this%varID
            end if

            ! move the stream to the timestep of interest
            status = streamInqTimestep(streamID, ind-1)
            if (status < 1) call fplus_error_print("no data for this time step", "varinfo%get_timestep_data")

            ! read the data
            if (.not.present(levels)) then
                call streamReadVar(streamID, varID, res, nmiss)
            else
                ! loop over all levels of interest
                do ind = lbound(levels,1), ubound(levels,1)
                    call streamReadVarSlice(streamID, varID, levels(ind)-1, res(:,: ${ZLEVID(i)}), nmiss)
                end do
            end if
        end subroutine
        !$FP end do
    !$FP end template

    !$FP template varinfo_set_constant_data
        !$FP D = {dimension(:)} {dimension(:,:)} {dimension(:,:,:)}

        !$FP do i = 1, 3
        !> @public
        !> @brief       Save the data for a constant 2d or 3d field to the output file. 
        !> @details     You have to write the constant fields at last (unsure why...)
        !> @param[in]   this    reference to the varinfo object, automatically set by fortran
        !> @param[out]  values  the array where the data is stored, the rank and shape has to match the information stored
        !>                      in the varinfo object
        subroutine varinfo_set_constant_data(this, values)
            class(varinfo) :: this
            real (kind=8), ${D(i)} :: values

            ! call the set_timestep_data subroutine to write the data
            call this%set_timestep_data(values, tstep=0)
        end subroutine
        !$FP end do
    !$FP end template

    !$FP template varinfo_set_timestep_data
        !$FP D = {dimension(:)} {dimension(:,:)} {dimension(:,:,:)}

        !$FP do i = 1, 3
        !> @public
        !> @brief       Save the data for one time step to the output file.
        !> @details     The timestep can be specified by its number, by its time in seconds since 1970, or by
        !>              a datetime object. This subroutine is for ${i}-dimensional data.
        !> @param[in]   this    reference to the varinfo object, automatically set by fortran
        !> @param[out]  values  the array where the data is stored, the rank and shape has to match the information stored
        !>                      in the varinfo object
        !> @param[in]   tstep   the number of the time-step, first = 1, optional
        !> @param[in]   stime   the time in seconds since 1970 for this time step, optional
        !> @param[in]   dt      the time as a datetime object, optional
        subroutine varinfo_set_timestep_data(this, values, tstep, stime, dt)
            class(varinfo) :: this
            integer, intent(in), optional :: tstep
            real (kind=8), intent(in), optional :: stime
            class(datetime), intent(in), optional :: dt
            real (kind=8), ${D(i)} :: values

            ! local variables
            integer, dimension(:), allocatable :: values_shape, values_shape_2
            integer :: values_rank

            ! the unstructured grid has a y-dimension that can be omitted
            if (this%gridtype == GRID_UNSTRUCTURED) then
                ! get the dimensions of the array
                values_shape_2 = shape(values)
                !$FP if (i == 1) then 
                allocate(values_shape(2))
                values_shape(1) = values_shape_2(1)
                values_shape(2) = 1
                values_rank = 2
                !$FP end if
                !$FP if (i == 2) then 
                if (this%nzval > 1) then
                    allocate(values_shape(3))
                    values_shape(1) = values_shape_2(1)    
                    values_shape(2) = 1    
                    values_shape(3) = values_shape_2(2)
                    values_rank = 3
                else
                    values_shape = values_shape_2
                    values_rank = 2
                end if    
                !$FP end if
                !$FP if (i == 3) then 
                values_shape = values_shape_2
                values_rank = 3
                !$FP end if
            else
                ! get the dimensions of the array
                values_shape = shape(values)
                values_rank = size(values_shape)          
            end if
            ! call the internal subroutine
            call this%varinfo_set_timestep_data_intern(values, values_rank, values_shape, tstep, stime, dt)
        end subroutine
        !$FP end do
    !$FP end template

    !> @brief       Save the data for one time step to the output file. This subroutine is called internally from varinfo_set_timestep_data
    subroutine varinfo_set_timestep_data_intern(this, values, values_rank, values_shape, tstep, stime, dt)
        class(varinfo) :: this
        integer, intent(in), optional :: tstep
        real (kind=8), intent(in), optional :: stime
        class(datetime), intent(in), optional :: dt
        real (kind=8), dimension(*) :: values
        integer, intent(in) :: values_rank
        integer, dimension(:), intent(in) :: values_shape

        ! local variables
        integer :: ind
        integer :: nmiss = 0, status, dim_count, idate, itime
        integer, dimension(:), allocatable :: nominal_shape
        real (kind=8), dimension(:), allocatable :: temp_coord_time
        type(datetime) :: dtnew

        ! check if there is a file
        if (.not.associated(this%finfo)) call fplus_error_print("the variable is not associated with a file!", "varinfo%set_timestep_data")

        ! how many dimensions do we have?
        dim_count = 0
        if (this%get_x_size() > 1) dim_count = 1
        if (this%get_y_size() > 1 .or. this%gridtype == GRID_UNSTRUCTURED) dim_count = dim_count + 1
        if (this%nzval > 1) dim_count = dim_count + 1
        if (dim_count /= values_rank) then 
            print*, "Nominal number of dimensions: ", dim_count
            print*, "Actual number of dimensions:  ", values_rank
            print*, "Variable Name: ", this%name
            print*, "variable Code: ", this%code
            call fplus_error_print("wrong number of dimension!", "varinfo%set_timestep_data")
        end if

        ! create an array with the nominal shape if the variable
        allocate(nominal_shape(dim_count))
        ind = 1
        if (this%get_x_size() > 1) then
            nominal_shape(ind) = this%get_x_size()
            ind = ind +1
        end if
        if (this%get_y_size() > 1 .or. this%gridtype == GRID_UNSTRUCTURED) then
            nominal_shape(ind) = this%get_y_size()
            ind = ind +1
        end if
        if (this%nzval > 1) then
            nominal_shape(ind) = this%nzval
            ind = ind +1
        end if
        ind = -1

        ! compare the shapes
        if (any(nominal_shape /= values_shape)) then
            print*, "Nominal shape: ", nominal_shape
            print*, "Actual shape:  ", values_shape
            print*, "Variable Name: ", this%name
            print*, "variable Code: ", this%code
            call fplus_error_print("wrong shape of values array!", "varinfo%set_timestep_data")
        end if

        ! check if the variables definition is fixed, of not do it now.
        if (.not.this%finfo%fixed) call this%finfo%fix_variable_definition()

        ! at least one valid parameter must be present
        if (.not.present(tstep) .and. .not.present(stime) .and. .not.present(dt)) then
            call fplus_error_print("no time step or time of the time step is given!", "varinfo%set_timestep_data")
        end if!

        ! get the time step
        if (present(tstep) .and. .not. this%is_constant) then
            ! check if we already have a time coordinate and if the step is present
            if (.not.allocated(this%coord_time)) call fplus_error_print("tstep parameter used but coord_time not allocated!", "varinfo%set_timestep_data")
            if (tstep > this%ntime) call fplus_error_print("tstep parameter used that is larger than ntime!", "varinfo%set_timestep_data")
            if (tstep < 1) call fplus_error_print("tstep parameter used that is smaler than one!", "varinfo%set_timestep_data")
            ind = tstep -1
        end if

        ! get the timestep from a datetime object
        if (present(dt) .or. present(stime)) then
            if (present(dt)) then
                dtnew = dt
            else if (present(stime)) then
                dtnew = new_datetime(stime=stime)
            end if
            call this%add_timesteps(dtnew%time_in_sec1970, nindex=ind)
            ind = ind -1
        end if
        if (ind == -1 .and. .not. this%is_constant) call fplus_error_print("problem with time step definition", "varinfo%set_timestep_data")
        if (this%finfo%last_time_step == -1 .and. ind > 0) call fplus_error_print("the first time step has be 0!", "varinfo%set_timestep_data")
        
        ! check is this time step is already defined
        if (.not. this%is_constant) then
            if (this%finfo%last_time_step >= ind) then
                ! it is not possible to write to an finished timestep
                if (this%finfo%last_time_step /= ind) then
                    call fplus_error_print("It is not possible to write to an already finished time step!", "varinfo%set_timestep_date", FPLUS_WARN)
                    call fplus_error_print("The data will be written for the current time step!", "varinf%set_timestep_date", FPLUS_WARN)
                end if
            else
                ! define the timestep
                dtnew = new_datetime(stime=this%coord_time(ind+1))
                call dtnew%get(idate=idate, itime=itime)
                call taxisDefVdate(this%finfo%taxisID, idate)
                call taxisDefVtime(this%finfo%taxisID, itime)
                status = streamDefTimestep(this%finfo%streamID, ind)
                this%finfo%last_time_step = ind
            end if
        end if

        ! write the data
        call streamWriteVar(this%finfo%streamID, this%varID, values, nmiss)
    end subroutine

    !> @brief   Get a string representation of the variable info
    function varinfo_to_string(this) result(res)
        class(varinfo) :: this
        character (len=:), allocatable :: res
        res = trim(this%name) // "(code=" // type_to_string(this%code) // ")"
    end function

    !> @brief   Get a hashcode of the varinfo
    function varinfo_hashcode(this) result(res)
        class(varinfo) :: this
        integer (kind=8) :: res
        res = calculateHash(this%code)
        res = res + calculateHash(this%zaxistype)
    end function

    !> @brief   Convert a class(*) pointer to varinfo pointer
    function to_varinfo(input) result (res)
        class(varinfo), pointer :: res
        class(*), pointer, intent(in) :: input
        res => null()
        select type (input)
            class is (varinfo)
                res => input
            class default
                call fplus_error_print("wrong data type", "to_varinfo")
        end select
    end function

    !> @brief       Release the memory used by the varinfo.
    !> @param[in]   this    reference to the varinfo object, automatically set by fortran
    subroutine varinfo_clear(this)
        class(varinfo) :: this
        integer :: index
        if (allocated(this%coord_time)) deallocate (this%coord_time)
        if (allocated(this%coord_lon))  deallocate (this%coord_lon)
        if (allocated(this%coord_lat))  deallocate (this%coord_lat)
        if (allocated(this%coord_z))    deallocate (this%coord_z)

        if (associated(this%finfo) .and. this%finfo%status == "w") then
            if (this%zaxisID >= 0) then 
                index = list_zaxisID%get_position(this%zaxisID)
                if (index > 0) then
                    call zaxisDestroy(this%zaxisID)
                    call list_zaxisID%remove(index)
                end if
            end if
            if (this%gridID >= 0) then 
                index = list_gridID%get_position(this%gridID)
                if (index > 0) then
                    call gridDestroy(this%gridID)
                    call list_gridID%remove(index)
                end if
            end if
        end if

        ! is this a merged variable?
        if (allocated(this%merged_var_parts)) then
            call this%merged_var_parts%clear()
            deallocate(this%merged_var_parts)
        end if
        
        ! is it a derived variable with subsections?
        if (allocated(this%parent_subscripts)) deallocate (this%parent_subscripts)
        if (associated(this%parent)) nullify (this%parent)
        
        ! is there a pointer to a file?
        if (associated(this%finfo)) nullify (this%finfo)
    end subroutine


    !> @public
    !> @brief       Add timesteps to the timeaxis
    !> @param[in]   this            reference to the varinfo object, automatically set by fortran
    !> @param[in]   new_timestep    one or more timesteps in seconds since 1970
    !> @param[in]   allow_earlier   set to true to allow adding time steps that are prior the last time step, optional
    !> @param[out]  nindex          if present, then the index of the new timestep is stored in this variable, optional.
    !>                              The variable is only used if a single timestep is added
    subroutine varinfo_add_timesteps_rk8(this, new_timestep, allow_earlier, nindex)
        class(varinfo) :: this
        real(kind=8), intent(in) :: new_timestep
        logical, optional :: allow_earlier
        integer, optional :: nindex

        ! local variables
        integer :: ind
        logical :: iallow_earlier
        real(kind=8), dimension(:), allocatable :: temp_coord_time

        ! do we allow to add earlier time steps?
        if (present(allow_earlier)) then
            iallow_earlier = allow_earlier
        else
            iallow_earlier = .false.
        end if

        ! is there space left in the coord_time array?
        if (.not.allocated(this%coord_time)) then
            allocate(this%coord_time(1000))
            this%coord_time = fplus_fill_realk8
            this%ntime = 1
            this%coord_time(1) = new_timestep
            ind = 1
        else
            ! we already have a time axis, we have to search the value within it.
            ind = valueindex(this%coord_time, new_timestep, 1, this%ntime, sorted=.true.)
            ! if not found, append the value.
            if (ind == fplus_fill_int) then
                ind = this%ntime
                if (.not.iallow_earlier .and. this%coord_time(ind) > new_timestep) then
                    call fplus_error_print("there is already a later timestep defined", "varinfo%add_timesteps", FPLUS_ERR)
                end if
                this%ntime = this%ntime + 1
                if (size(this%coord_time) < this%ntime) then
                    temp_coord_time = this%coord_time
                    deallocate(this%coord_time)
                    allocate(this%coord_time(this%ntime*2))
                    this%coord_time(1:ind) = temp_coord_time
                    this%coord_time(ind+2:) = fplus_fill_realk8
                    deallocate(temp_coord_time)
                end if
                this%coord_time(ind+1) = new_timestep
                ind = ind +1
            end if
        end if
        if (present(nindex)) nindex = ind
    end subroutine


    !> @public
    !> @brief       Add timesteps to the timeaxis
    !> @param[in]   this            reference to the varinfo object, automatically set by fortran
    !> @param[in]   new_timesteps    one or more timesteps in seconds since 1970
    subroutine varinfo_add_timesteps_rk8dx(this, new_timesteps)
        class(varinfo) :: this
        real(kind=8), dimension(:), intent(in) :: new_timesteps

        ! local variables
        integer :: nnew
        real(kind=8), dimension(:), allocatable :: temp_coord_time

        ! how many new timestepsß
        nnew = size(new_timesteps)
        if (nnew == 0) return

        ! is there space left in the coord_time array?
        if (.not.allocated(this%coord_time)) then
            this%coord_time = new_timesteps
            this%ntime = nnew
        else
            if (this%ntime + nnew > size(this%coord_time)) then
                temp_coord_time = this%coord_time
                deallocate(this%coord_time)
                allocate(this%coord_time(this%ntime + nnew))
                this%coord_time(1:this%ntime) = temp_coord_time
                this%coord_time(this%ntime+1:) = new_timesteps
                this%ntime = this%ntime + nnew
                ! ensure the the new time axis is sorted
                call qsort(this%coord_time)
            end if
        end if
    end subroutine

    !> @brief       Create a copy with the same properties as this object, but not associated with a file
    !> @param[in]   this        reference to the varinfo object, automatically set by fortran
    !> @param[in]   copy_time   .true. = copy the time coordinate, default is .true., optional
    !> @param[in]   copy_llz    .true. = copy the spatial coordinates, default is .true., optional
    function varinfo_derived_clone(this, copy_time, copy_llz) result(res)
        class(varinfo) :: this
        logical, optional :: copy_time, copy_llz
        class(varinfo), pointer :: res

        !allocate the result
        allocate(varinfo :: res)

        !copy the coordinates
        if (.not.present(copy_llz) .or. (copy_llz .eqv. .true.)) then
            if (allocated(this%coord_lon))  res%coord_lon   = this%coord_lon
            res%nlon        = this%nlon
            if (allocated(this%coord_lat))  res%coord_lat   = this%coord_lat
            res%nlat        = this%nlat
            if (allocated(this%coord_z))    res%coord_z     = this%coord_z
            res%nzval       = this%nzval
        end if
        if (.not.present(copy_time) .or. (copy_time .eqv. .true.)) then
            if (allocated(this%coord_time)) res%coord_time  = this%coord_time
            res%ntime       = this%ntime
        end if
        
        !copy attributes
        res%name            = this%name
        res%standard_name   = this%standard_name
        res%longname        = this%longname
        res%units           = this%units
        res%code            = this%code
        res%zaxistype       = this%zaxistype
        res%gridtype        = this%gridtype
        res%is_constant     = this%is_constant
    end function

    !> @public
    !> @brief       Get the size of the underlying array in x-direction
    !> @param[in]   this        reference to the varinfo object, automatically set by fortran
    !> @return      for most grid types, nlon is returned.
    function varinfo_get_x_size(this) result (res)
        class(varinfo) :: this
        integer :: res
        if (this%gridtype == GRID_LONLAT .or. this%gridtype == GRID_UNSTRUCTURED .or. this%gridtype == GRID_GAUSSIAN) then
            res = this%nlon
        else
            call fplus_error_print("unsupported grid: " // trim(type_to_string(this%gridtype)), "get_x_size")
        end if
    end function

    !> @public
    !> @brief       Get the size of the underlying array in y-direction
    !> @param[in]   this        reference to the varinfo object, automatically set by fortran
    !> @return      for most grid types, nlat is returned. 
    function varinfo_get_y_size(this) result (res)
        class(varinfo) :: this
        integer :: res
        if (this%gridtype == GRID_LONLAT .or. this%gridtype == GRID_GAUSSIAN) then
            res = this%nlat
        else if (this%gridtype == GRID_UNSTRUCTURED) then
            res = 1
        else
            call fplus_error_print("unsupported grid: " // trim(type_to_string(this%gridtype)), "get_y_size")
        end if
    end function

end module fplus_cdi_helper
