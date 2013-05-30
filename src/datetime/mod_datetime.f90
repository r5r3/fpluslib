!> @brief   this module contains functions for calculation on dates
!> @author  Robert Schuster
module mod_datetime
    use f_udunits_2
    use mod_hashcode
    use mod_fstd
    implicit none
    private

    !> @brief   A date and time representation, the time zone is so fare ignored
    type, extends(object), public :: datetime
        real (kind=8), private :: time_in_sec1970
    contains
        !> @brief   calculates a hashcode for this datetime object
        procedure, public :: hashcode => datetime_hashcode
        !> @brief   returns a string containing the date and time in ISO format
        procedure, public :: to_string => datetime_to_string
    end type
    interface datetime
        module procedure datetime_constructor
    end interface

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
    

! the contents of the module follows
contains

   ! procedures for type data ------------------------------------------------

    function datetime_constructor()
        type(datetime) :: datetime_constructor
        integer :: now(8)

        ! init the module if needed
        if (.not. isInitialized) call timehelper_init()

        ! get the curent date and time
        call date_and_time(values=now)
        datetime_constructor%time_in_sec1970 = get_seconds_since_1970(now(1),now(2),now(3),now(5),now(6),real(now(7), 8))
    end function
    
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
