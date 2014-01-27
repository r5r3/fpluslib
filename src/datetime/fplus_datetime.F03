!> @brief   this module contains functions for calculation on dates
!> @author  Robert Schuster
module fplus_datetime
    use f_udunits_2
    use fplus_hashcode
    use fplus_object
    use fplus_fillvalue
    use fplus_error
    implicit none
    private

    !> @brief   A date and time representation, the time zone is so fare ignored
    type, extends(object), public :: datetime
        !> @brief   the time in seconds since 1970. This is the internal representation
        !>          of the time used by this type. Direct modification are allowed.
        real (kind=8), public :: time_in_sec1970
    contains
        !> @brief   calculates a hashcode for this datetime object
        procedure, public :: hashcode => datetime_hashcode
        !> @brief   returns a string containing the date and time in ISO format
        procedure, public :: to_string => datetime_to_string
        !> @brief   add time of a given unit
        procedure, public :: add => datetime_add
        !> @brief   set single parts of the date
        procedure, public :: set => datetime_set
        !> @brief   set single parts of the date
        procedure, public :: get => datetime_get
        !> @brief   set one single part of the date
        procedure, public :: get_field => datetime_get_field
        !> @brief   get the number of days in the given year
        procedure, public :: ndays_in_year => datetime_ndays_in_year
    end type

    ! make the constructor public
    public :: new_datetime, get_date_from_seconds_since_1970

    ! pointer to the unit-system of the udunits2 library used for time conversion
    type(UT_SYSTEM_PTR) utsystem
    
    ! pointer to the unit of the time used in this module
    type(UT_UNIT_PTR) sec1970
    type(UT_UNIT_PTR) secunit
    
    ! pointer to the converter used for time conversion
    type(CV_CONVERTER_PTR) sec_to_sec1970
    type(CV_CONVERTER_PTR) sec1970_to_sec    
    
    ! this varrable is set to true afert initialization
    logical :: isInitialized = .false.
    
    !> @brief   Constants for the usage in get_field
    integer, parameter, public :: DT_YEAR   = 1
    integer, parameter, public :: DT_MONTH  = 2
    integer, parameter, public :: DT_DAY    = 3
    integer, parameter, public :: DT_HOUR   = 4
    integer, parameter, public :: DT_MINUTE = 5
    integer, parameter, public :: DT_SECOND = 6

    !> @brief   assignment for datetime object
    public :: assignment (=), to_datetime
    !$FP interface template assignment (=)
    !    module procedure class_star_to_datetime
    !end interface
    
! the contents of the module follows
contains

    ! procedures for type data ------------------------------------------------

    !> @public
    !> @brief       Initializes a new datetime object. Use only this function to create new
    !>              datetime objects
    !> @details     If any of the optional arguments is given, then the new datetime object
    !>              will represent the date and time specified by the arguments. Missing
    !>              arguments are set to zero (year to 1970, month and day to 1).
    !>              If no argument is present, then the new datetime object will
    !>              represent the time of creation.
    !> @param[in]   year    the year of the new date, optional
    !> @param[in]   month   the month of the new date, optional
    !> @param[in]   day     the day of the new date, optional
    !> @param[in]   hour    the hour of the new date, optional
    !> @param[in]   minute  the minute of the new date, optional
    !> @param[in]   seconds the seconds of the new date, optional
    !> @param[in]   idate   date the the format YYYYMMDD, this format is used by the CDI library, optional
    !> @param[in]   itime   date the the format HHMMSS, this format is used by the CDI library, optional
    !> @param[in]   stime   date in seconds since 1970, optional
    !> @return      An initialized new datetime object.
    function new_datetime(year, month, day, hour, minute, second, idate, itime, stime) result(dt)
        type(datetime) :: dt
        integer, optional :: year, month, day, hour, minute, idate, itime
        real (kind=8), optional :: second, stime

        ! local variables
        real (kind=8) :: isecond
        integer :: now(8)

        ! init the module if needed
        if (.not. isInitialized) call timehelper_init()

        ! is the time in seconds since 1970 given?
        if (present(stime)) then
            dt%time_in_sec1970 = stime
            return
        end if

        ! set the specified time or the current time?
        if (present(year) .or. present(month) .or. present(day) .or. present(hour) .or. present(minute) .or. present(second) .or. present(idate) .or. present(itime)) then
            if (present(year)) then
                now(1) = year
            else
                now(1) = 1970
            end if
            if (present(month)) then
                now(2) = month
            else
                now(2) = 1
            end if
            if (present(day)) then
                now(3) = day
            else
                now(3) = 1
            end if
            if (present(hour)) then
                now(5) = hour
            else
                now(5) = 0
            end if
            if (present(minute)) then
                now(6) = minute
            else
                now(6) = 0
            end if
            if (present(second)) then
                isecond = second
            else
                isecond = 0.0_8
            end if
            if (present(idate)) then
                now(1) = idate / 10000
                now(2) = (idate - now(1) * 10000) / 100
                now(3) = idate - now(1) * 10000 - now(2) * 100
            end if
            if (present(itime)) then
                now(5) = itime / 10000
                now(6) = (itime - now(5) * 10000) / 100
                isecond = itime - now(5) * 10000 - now(6) * 100
            end if
        else
            ! get the curent date and time
            call date_and_time(values=now)
            isecond = real(now(7), 8)
        end if
        dt%time_in_sec1970 = get_seconds_since_1970(now(1),now(2),now(3),now(5),now(6),isecond)
    end function
    
    !> @public
    !> @brief   calculates a hashcode for this datetime object
    !> @details the hashcode is calculated for the time in seconds 
    !>          since 1970 plus the string datetime
    integer(kind=8) function datetime_hashcode(this) result(res)
        class(datetime) :: this
        integer (kind=1), dimension(16) :: chars
        
        call C_double2intarray(this%time_in_sec1970, chars(1:8))
        ! write the string "datetime" to the end of the array
        chars(9:16) = (/100, 97, 116, 101, 116, 105, 109, 101/)
        call C_sdbm(chars, 16, res)
    end function

    !> @public
    !> @brief   returns a string containing the date and time in ISO format
    function datetime_to_string(this) result(res)
        class(datetime) :: this
        character (len=:), allocatable :: res
        
        ! locale variables
        integer :: year,month,day,hour,minute
        real (kind=8) :: second
        
        ! allocate memory for the string
        allocate (character(len=19) :: res)

        ! calculate the parts of the data
        call get_date_from_seconds_since_1970(this%time_in_sec1970,year,month,day,hour,minute,second)
        write (res,"(I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)"), year, "-", month, "-", day, " ", hour, ":", minute, ":", int(second)    
    end function
    
    !> @public
    !> @brief       change the time stored in this object by adding time of a given unit
    !> @details     All arguments are optional, the stored time is not changed, if non
    !>              of the arguments is present.
    !>              Example:
    !>              @code
    !>              type(datetime) :: now
    !>              now = datetime()
    !>              now%add(year=1, month=-1)
    !>              @endcode
    !> @param[in]   this    reference to the datetime object, automatically set by fortran
    !> @param[in]   year    add a number of years, optional
    !> @param[in]   month   add a number of months, optional
    !> @param[in]   day     add a number of days, optional
    !> @param[in]   hour    add a number of hours, optional
    !> @param[in]   minute  add a number of minutes, optional
    !> @param[in]   seconds add a number of seconds, optional
    subroutine datetime_add(this, year, month, day, hour, minute, second)
        class(datetime) :: this
        integer, optional :: year, month, day, hour, minute
        real (kind=8), optional :: second

        ! local variables
        integer :: iyear, imonth, iday, ihour, iminute
        real (kind=8) :: isecond

        ! add a month or a year
        if (present(year) .or. present(month)) then
            ! calculate the currently stored date
            call get_date_from_seconds_since_1970(this%time_in_sec1970,iyear,imonth,iday,ihour,iminute,isecond)

            ! add a year if present
            if (present(year)) then
                iyear = iyear + year
            end if
            ! add a month if present
            if (present(month)) then
                imonth = imonth + month
                do while (imonth > 12)
                    iyear = iyear + 1
                    imonth = imonth - 12
                end do
                do while (imonth < 0)
                    iyear = iyear - 1
                    imonth = imonth + 12
                end do
            end if

            ! convert back to seconds since 1970
            this%time_in_sec1970 = get_seconds_since_1970(iyear,imonth,iday,ihour,iminute,isecond)
        end if

        ! add a day if present
        if (present(day)) then
            this%time_in_sec1970 = this%time_in_sec1970 + 86400 * day
        end if
        ! add a hour if present
        if (present(hour)) then
            this%time_in_sec1970 = this%time_in_sec1970 + 3600 * hour
        end if
        ! add a minute if present
        if (present(minute)) then
            this%time_in_sec1970 = this%time_in_sec1970 + 60 * minute
        end if
        ! add a second if present
        if (present(second)) then
            this%time_in_sec1970 = this%time_in_sec1970 + second
        end if

    end subroutine

    !> @public
    !> @brief       set single parts of the date
    !> @param[in]   this    reference to the datetime object, automatically set by fortran
    !> @param[in]   year    set the years, optional
    !> @param[in]   month   set the months, optional
    !> @param[in]   day     set the days, optional
    !> @param[in]   hour    set the hours, optional
    !> @param[in]   minute  set the minutes, optional
    !> @param[in]   seconds set the seconds, optional
    subroutine datetime_set(this, year, month, day, hour, minute, second)
        class(datetime) :: this
        integer, optional :: year, month, day, hour, minute
        real (kind=8), optional :: second

        ! local variables
        integer :: iyear, imonth, iday, ihour, iminute
        real (kind=8) :: isecond

        ! calculate the currently stored date
        call get_date_from_seconds_since_1970(this%time_in_sec1970,iyear,imonth,iday,ihour,iminute,isecond)

        ! set a year if present
        if (present(year)) then
            iyear = year
        end if
        ! set a month if present
        if (present(month)) then
            imonth = month
        end if
        ! add a day if present
        if (present(day)) then
            iday = day
        end if
        ! add a hour if present
        if (present(hour)) then
            ihour = hour
        end if
        ! add a minute if present
        if (present(minute)) then
            iminute = minute
        end if
        ! add a second if present
        if (present(second)) then
            isecond = second
        end if

        ! convert back to seconds since 1970
        this%time_in_sec1970 = get_seconds_since_1970(iyear,imonth,iday,ihour,iminute,isecond)
    end subroutine


    !> @public
    !> @brief       get single parts of the date
    !> @param[in]   this    reference to the datetime object, automatically set by fortran
    !> @param[out]  year    get the years, optional
    !> @param[out]  month   get the months, optional
    !> @param[out]  day     get the days, optional
    !> @param[out]  hour    get the hours, optional
    !> @param[out]  minute  get the minutes, optional
    !> @param[out]  seconds get the seconds, optional
    !> @param[out]  idate   get an integer representation of the date YYYYMMDD, optional
    !> @param[out]  itime   get an integer representation od the time HHMMSS, optional
    subroutine datetime_get(this, year, month, day, hour, minute, second, idate, itime)
        class(datetime) :: this
        integer, intent(out), optional :: year, month, day, hour, minute, idate, itime
        real (kind=8), intent(out), optional :: second

        ! local variables
        integer :: iyear, imonth, iday, ihour, iminute
        real (kind=8) :: isecond

        ! calculate the currently stored date
        call get_date_from_seconds_since_1970(this%time_in_sec1970,iyear,imonth,iday,ihour,iminute,isecond)

        ! get a year if present
        if (present(year)) then
            year = iyear
        end if
        ! get a month if present
        if (present(month)) then
            month = imonth
        end if
        ! get a day if present
        if (present(day)) then
            day = iday
        end if
        ! get a hour if present
        if (present(hour)) then
            hour = ihour
        end if
        ! get a minute if present
        if (present(minute)) then
            minute = iminute
        end if
        ! get a second if present
        if (present(second)) then
            second = isecond
        end if
        ! get an integer representation of the date
        if (present(idate)) then
            idate = iyear*10000 + imonth * 100 + iday
        end if
        ! get an integer representation of the time
        if (present(itime)) then
            itime = ihour * 10000 + iminute * 100 + isecond
        end if
    end subroutine

    !> @public
    !> @brief       set one single part of the date
    !> @param[in]   this    reference to the datetime object, automatically set by fortran
    !> @param[in]   field   the part of interest, valid values are DT_YEAR, DT_MONTH, DT_DAY, DT_HOUR, DT_MINUTE, DT_SECOND
    integer function datetime_get_field(this, field) result(res)
        class(datetime) :: this
        integer, intent(in) :: field
        ! local variables
        integer :: iyear, imonth, iday, ihour, iminute
        real (kind=8) :: isecond

        ! calculate the currently stored date
        call get_date_from_seconds_since_1970(this%time_in_sec1970,iyear,imonth,iday,ihour,iminute,isecond)
        select case (field)
            case (DT_YEAR)
                res = iyear
            case (DT_MONTH)
                res = imonth
            case (DT_DAY)
                res = iday
            case (DT_HOUR)
                res = ihour
            case (DT_MINUTE)
                res = iminute
            case (DT_SECOND)
                res = int(isecond)
            case default
                res = fplus_fill_int
        end select
    end function

    !> @public
    !> @brief   get the number of days in the given year
    function datetime_ndays_in_year(this) result (res)
        class(datetime) :: this
        integer :: res

        ! local variables
        type(datetime) :: dt1, dt2
        integer :: year
        real(kind=8) :: diff

        call this%get(year=year)
        dt1 = new_datetime(year=year, month=1, day=1)
        dt2 = new_datetime(year=year, month=12, day=31)
        diff = dt2%time_in_sec1970 - dt1%time_in_sec1970
        res = nint(diff/86400)+1
    end function

    ! procedure that don't belong to a type -----------------------------------

    ! initialize this module
    subroutine timehelper_init()
        use ISO_C_BINDING
        ! tell the udunits library not to print any error messages, use a c-function to do that
        interface
            subroutine f_udunits_2_ignore_error() bind(C,name='udunits_2_ignore_error')
            end subroutine
        end interface
        ! call the c-function
        call f_udunits_2_ignore_error()
        
        ! create the unit system
        utsystem = f_ut_read_xml("")
        ! create the unit seconds since ...
        sec1970 = f_ut_parse(utsystem,"seconds since 1970-01-01 00:00:00", UT_ASCII)
        ! create the base unit seconds
        secunit = f_ut_get_unit_by_name(utsystem, "second")
        ! create a converter from seconds to seconds since 1970
        sec_to_sec1970 = f_ut_get_converter(secunit,sec1970)
        ! create a seconds converter in the invers direction
        sec1970_to_sec = f_ut_get_converter(sec1970,secunit)
        
        ! the module is now usable
        isInitialized = .true.
    end subroutine
    
    ! calculate the seconds since 1970 from a given date
    real (kind=8) function get_seconds_since_1970(year,month,day,hour,minute,second)
        ! arguments to the function
        integer, intent(in) :: year,month,day,hour,minute
        real (kind=8), intent(in) :: second
        ! internal variables
        real (kind=8) :: basetime
        
        ! init the module if needed
        if (.not. isInitialized) call timehelper_init()

        ! calculate the time in the internal unit used by udunits
        basetime = f_ut_encode_time(year,month,day,hour,minute,second)
        
        ! convert the basetime to the unit seconds since 1970
        get_seconds_since_1970 = f_cv_convert_double(sec_to_sec1970, basetime)
    end function

    ! caluculate the date from a given amount of seconds since 1970
    subroutine get_date_from_seconds_since_1970(sec_since_1970,year,month,day,hour,minute,second)
        ! input variable
        real (kind=8), intent(in) :: sec_since_1970
        ! output variables
        integer, intent(out) :: year,month,day,hour,minute
        real (kind=8), intent(out) :: second
        ! interval variables
        real (kind=8) :: basetime, resolution

        ! init the module if needed
        if (.not. isInitialized) call timehelper_init()

        ! convert the seconds since 1970 to the internal unit used by udunits
        basetime = f_cv_convert_double(sec1970_to_sec, sec_since_1970)
        
        ! decode the basetime
        call f_ut_decode_time(basetime,year,month,day,hour,minute,second,resolution)
    end subroutine
    
    ! calculate the month from a given date in seconds since 1970
    integer function get_month_from_seconds_since_1970(sec_since_1970)
        ! input variable
        real (kind=8), intent(in) :: sec_since_1970
        ! local variables
        integer :: year,month,day,hour,minute
        real (kind=8) :: second
        ! calculate the date
        call get_date_from_seconds_since_1970(sec_since_1970,year,month,day,hour,minute,second)
        get_month_from_seconds_since_1970 = month
    end function

    ! calculate the year from a given date in seconds since 1970
    integer function get_year_from_seconds_since_1970(sec_since_1970)
        ! input variable
        real (kind=8), intent(in) :: sec_since_1970
        ! local variables
        integer :: year,month,day,hour,minute
        real (kind=8) :: second
        ! calculate the date
        call get_date_from_seconds_since_1970(sec_since_1970,year,month,day,hour,minute,second)
        get_year_from_seconds_since_1970 = year
    end function

    !$FP template assignment (=)
        !$FP types = {real (kind=8)} {type(datetime)}
        !$FP do i = 1, 2
            !> @public
            !> @brief   assignment other datetime objects and real k8 values to datetime objects
            subroutine assign_to_datetime(outval, inval)
                type(datetime), intent(inout) :: outval
                ${types(i)}, intent(in) :: inval
                !$FP if (i==1) outval%time_in_sec1970 = inval
                !$FP if (i==2) outval%time_in_sec1970 = inval%time_in_sec1970
            end subroutine
        !$FP end do
    !$FP end template

    !> @public
    !> @brief   convert other datetime objects and real k8 values to datetime objects
    function to_datetime(value) result (res)
        type(datetime) :: res
        class(*), intent(in) :: value
        select type(value)
            type is (real (kind=8))
                res%time_in_sec1970 = value
            type is (datetime)
                res%time_in_sec1970 = value%time_in_sec1970
            class default
                call fplus_error_print("wrong type in assignment to datetime object, only real (kind=8) and datetime are supported", "datetime = class(*)", FPLUS_ERR)
        end select
    end function

end module
