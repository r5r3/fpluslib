! this module contains functions for calculation on dates

module mod_date
    use f_udunits_2
    implicit none
	private
	
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
    
    !> @brief   A date and time representation, the time zone is so fare ignored
    type, public :: date
        real (kind=8), private :: time
    end type
    interface date
        module procedure date_constructor
    end interface

    ! the contents of the module follows
    contains
    
    ! procedures for type data ------------------------------------------------

    function date_constructor()
        class(date), pointer :: date_constructor
        integer :: now(8)

        ! init the module if needed
        if (.not. isInitialized) call timehelper_init()

        !create the date object
        allocate(date_constructor)

        ! get the curent date and time
        call date_and_time(values=now)
        date_constructor%time = get_seconds_since_1970(now(1),now(2),now(3),now(5),now(6),real(now(7), 8))
        print*, date_constructor%time
    end function


    ! procedure that don't belong to a type

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
end module
