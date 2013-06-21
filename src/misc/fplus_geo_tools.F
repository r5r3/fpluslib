!> @brief   Some functions for georeferenced arrays
!> @author: Robert Schuster
!
! Created on 19. Juni 2013, 17:19
!
module fplus_geo_tools
    use fplus_error
    use fplus_array_tools
    implicit none
    private
    
    public :: rot_lon_to_range, lon_is_cyclic
    
    interface template rot_lon_to_range, rot_lon_to_range_with_data
    end interface    
    interface template lon_is_cyclic
    end interface    
    
contains

    template lon_is_cyclic
        replace :: T => real (kind=4); real (kind=8); integer (kind=4); integer (kind=8)
    contains
        !> @brief       Check if an longitude coordinate array goes around the glob
        !> @param[in]   coord_lon
        logical function lon_is_cyclic(coord_lon) result (res)
            T, dimension(:), intent(in) :: coord_lon 
            ! local variables
            real (kind=8) :: avg_diff
            ! calulate the average difference between two points
            avg_diff = abs(maxval(coord_lon) - minval(coord_lon)) / (size(coord_lon)-1)
            res = 360 - avg_diff*size(coord_lon) < avg_diff
        end function
    end template

    template rot_lon_to_range_with_data
        replace :: T => real (kind=4); real (kind=8); integer (kind=4); integer (kind=8)
        replace, foreach(T) :: DIMDOTS => : ; :,: ; :,:,: ; :,:,:,:
    contains
        !> @brief       Rotate a cyclic array as fas as needed to cover a given longitude range
        !> @details     for now, rotate only by 180°, this is needed to ensure that the coord-array remains monotonic
        !> @param[in]   old_coord_array     the old coordinate array, not rotated
        !> @param[out]  new_coord_array     the new coordinate array, the subroutine will create it
        !> @param[in]   old_array           the old data array, the longitude is the first dimension, optional
        !> @param[out]  new_array           the rotated array, it will be allocated as needed, optional
        !> @param[out]  npoint              the number of grid point by which the array was rocated. Grid points are
        !>                                  allways shifted to the left
        subroutine rot_lon_to_range_with_data(range, old_coord_array, new_coord_array, old_array, new_array, npoints)
            T, dimension(2) :: range
            T, dimension(:), intent(in) :: old_coord_array
            T, dimension(:), intent(inout), allocatable :: new_coord_array
            T, dimension(DIMDOTS), intent(in) :: old_array
            T, dimension(DIMDOTS), intent(inout), allocatable :: new_array
            integer, intent(out), optional :: npoints
 
            ! local variables
            integer :: np
            call rot_lon_to_range(range, old_coord_array, new_coord_array, np)
            
            ! rotate the data array
            new_array = old_array
            call rotate_array(new_array, np)
            if (present(npoints)) npoints = np
        end subroutine    
    end template
    
    
    template rot_lon_to_range
        replace :: T => real (kind=4); real (kind=8); integer (kind=4); integer (kind=8)
    contains        
        
        !> @brief       Rotate a cyclic array as fas as needed to cover a given longitude range
        !> @details     for now, rotate only by 180°, this is needed to ensure that the coord-array remains monotonic
        !> @param[in]   old_coord_array     the old coordinate array, not rotated
        !> @param[out]  new_coord_array     the new coordinate array, the subroutine will create it
        !> @param[out]  npoint              the number of grid point by which the array was rocated. Grid points are
        !>                                  allways shifted to the left
        subroutine rot_lon_to_range(range, old_coord_array, new_coord_array, npoints)
            T, dimension(2) :: range
            T, dimension(:), intent(in) :: old_coord_array
            T, dimension(:), intent(inout), allocatable :: new_coord_array
            integer, intent(out), optional :: npoints
            
            ! local variables
            integer, dimension(2) :: lon_type
            integer :: direction, i
            
            ! init npoints
            if (present(npoints)) npoints = 0
            
            ! check if the array is cyclic
            if (.not.lon_is_cyclic(old_coord_array)) then
                call fplus_error_print("unable to rotate non cyclic array", "rot_lon_to_range")
            end if
            
            ! which type of longitude representation is used? 0 .. 360 or -180 .. 180?
            if (any(old_coord_array < 0)) then
                lon_type(1) = -1
            else
                lon_type(1) = 1
            end if
            if (any(range < 0)) then
                lon_type(2) = -1
            else
                lon_type(2) = 1
            end if

            ! change the representation in the new array to the representation of range if needed
            new_coord_array = old_coord_array
            if (lon_type(1) /= lon_type(2) .and. any(value_in_range(new_coord_array, range) .eqv. .false.)) then
                if (lon_type(2) == -1) then
                    where (new_coord_array > 180)
                        new_coord_array = new_coord_array - 360
                    end where
                else
                    where (new_coord_array < 0)
                        new_coord_array = new_coord_array + 360
                    end where                            
                end if                
            end if
            
            ! check again if the range is within the array
            if (any(value_in_range(new_coord_array, range) .eqv. .false.)) then
                call fplus_error_print("unexpected error", "rot_lon_to_range")
            end if

            ! we dont need to rotate if the array is already monotonic
            i = 0
            if (.not.is_monotonic(new_coord_array, order=direction)) then
                ! find the first point where the array is not monotonic
                do i = lbound(new_coord_array, 1) +1, ubound(new_coord_array, 1)
                    if ((new_coord_array(i) > new_coord_array(i-1) .and. direction == -1) .or. &
                        (new_coord_array(i) < new_coord_array(i-1) .and. direction == 1)) then
                        ! the array will be rotated by i points
                        if (present(npoints)) npoints = i
                        call rotate_array(new_coord_array, i)
                        ! leave the loop 
                        exit
                    end if
                end do
            end if
        end subroutine
    end template

end module fplus_geo_tools
