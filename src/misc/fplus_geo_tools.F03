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
    
    public :: rot_lon_to_range, lon_is_cyclic, point_in_polygon, distance_to_polygon, geo_distance, lonrot_to_lon, latrot_to_lat, is_2d_grid_rotated
    
    !> @public
    !> @brief       Rotate a cyclic array as fas as needed to cover a given longitude range
    !$FP interface template rot_lon_to_range

    !> @public
    !> @brief       Check if an longitude coordinate array goes around the glob
    !$FP interface template lon_is_cyclic

    !> @public
    !> @brief       check if a point is located inside of a polygone
    !$FP interface template point_in_polygon

    !> @public
    !> @brief       check the distance between a point and a polygon
    !$FP interface template distance_to_polygon

    !> @public
    !> @brief       calculate the distance of two points on earth
    !$FP interface template geo_distance

    !> @public 
    !> @brief       convert a rotated lon value to real lon value
    !$FP interface template lonrot_to_lon

    !> @public 
    !> @brief       convert a rotated lat value to real lat value
    !$FP interface template latrot_to_lat

    !> @public 
    !> @brief       is a curvilinear grid really rotated?
    !$FP interface template is_2d_grid_rotated

contains

    !$FP template lon_is_cyclic
        !$FP do T = {real (kind=4)} {real (kind=8)} {integer (kind=4)} {integer (kind=8)}
    
            !> @public
            !> @brief       Check if an longitude coordinate array goes around the glob
            !> @param[in]   coord_lon
            logical function lon_is_cyclic(coord_lon) result (res)
                ${T}, dimension(:), intent(in) :: coord_lon 
                ! local variables
                real (kind=8) :: avg_diff
                ! calulate the average difference between two points
                avg_diff = abs(maxval(coord_lon) - minval(coord_lon)) / (size(coord_lon)-1)
                res = 360 - avg_diff*size(coord_lon) < avg_diff
            end function
        !$FP end do
    !$FP end template

    !$FP template rot_lon_to_range
        !$FP do T = {real (kind=4)} {real (kind=8)} {integer (kind=4)} {integer (kind=8)}
            !$FP do DIMDOTS = {:} {:,:} {:,:,:} {:,:,:,:}
        
                !> @public
                !> @brief       Rotate a cyclic array as fas as needed to cover a given longitude range
                !> @details     for now, rotate only by 180°, this is needed to ensure that the coord-array remains monotonic
                !> @param[in]   old_coord_array     the old coordinate array, not rotated
                !> @param[out]  new_coord_array     the new coordinate array, the subroutine will create it
                !> @param[in]   old_array           the old data array, the longitude is the first dimension, optional
                !> @param[out]  new_array           the rotated array, it will be allocated as needed, optional
                !> @param[out]  npoint              the number of grid point by which the array was rocated. Grid points are
                !>                                  allways shifted to the left
                subroutine rot_lon_to_range_with_data(range, old_coord_array, new_coord_array, old_array, new_array, npoints)
                    ${T}, dimension(2) :: range
                    ${T}, dimension(:), intent(in) :: old_coord_array
                    ${T}, dimension(:), intent(inout), allocatable :: new_coord_array
                    ${T}, dimension(${DIMDOTS}), intent(in) :: old_array
                    ${T}, dimension(${DIMDOTS}), intent(inout), allocatable :: new_array
                    integer, intent(out), optional :: npoints
         
                    ! local variables
                    integer :: np
                    call rot_lon_to_range(range, old_coord_array, new_coord_array, np)
                    
                    ! rotate the data array
                    new_array = cshift(old_array, np)
                    if (present(npoints)) npoints = np
                end subroutine
            !$FP end do
        !$FP end do

        !$FP do T = {real (kind=4)} {real (kind=8)} {integer (kind=4)} {integer (kind=8)}        
        
            !> @public
            !> @brief       Rotate a cyclic array as fas as needed to cover a given longitude range
            !> @details     for now, rotate only by 180°, this is needed to ensure that the coord-array remains monotonic
            !> @param[in]   old_coord_array     the old coordinate array, not rotated
            !> @param[out]  new_coord_array     the new coordinate array, the subroutine will create it
            !> @param[out]  npoint              the number of grid point by which the array was rocated. Grid points are
            !>                                  allways shifted to the left
            subroutine rot_lon_to_range(range, old_coord_array, new_coord_array, npoints)
                ${T}, dimension(2) :: range
                ${T}, dimension(:), intent(in) :: old_coord_array
                ${T}, dimension(:), intent(inout), allocatable :: new_coord_array
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
                    do i = lbound(new_coord_array, 1), ubound(new_coord_array, 1)-1
                        if ((new_coord_array(i+1) > new_coord_array(i) .and. direction == -1) .or. &
                            (new_coord_array(i+1) < new_coord_array(i) .and. direction == 1)) then
                            ! the array will be rotated by i points
                            if (present(npoints)) npoints = i
                            new_coord_array = cshift(new_coord_array, i)
                            ! leave the loop 
                            exit
                        end if
                    end do
                end if
            end subroutine
        !$FP end do
    !$FP end template

    !$FP template point_in_polygon
        !$FP do T = {real (kind=4)} {real (kind=8)} {integer (kind=4)} {integer (kind=8)}
    
            !> @public
            !> @brief       check if a point is located inside of a polygone
            !> @details     this function is based on source code from http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
            !> @param[in]   polyx   x-coordinate of the polygone points
            !> @param[in]   polyy   y-coordinate of the polygone points
            logical function point_in_polygon(polyx, polyy, testx, testy) result (res)
                ${T}, dimension(:), intent(in) :: polyx, polyy
                ${T}, intent(in) :: testx, testy
                ! local variables
                integer :: i, j
                ! the calculation
                res = .false.
                j = size(polyx)
                do i = 1, size(polyx)
                    if (((polyy(i)>testy) .neqv. (polyy(j)>testy)) .and. (testx < (polyx(j)-polyx(i)) * (testy-polyy(i)) / (polyy(j)-polyy(i)) + polyx(i))) res = .not. res;
                    j = i
                end do
            end function
        !$FP end do
    !$FP end template

    !$FP template geo_distance
        !$FP do T = {real (kind=4)} {real (kind=8)}
    
            !> @public
            !> @brief       Calculate the distance between two points in the glob. Not exact, only vague approximation!
            !> @param[in]   lon1    longitude of the first point
            !> @param[in]   lat1    latitude of the first point
            !> @param[in]   lon2    longitude of the second point
            !> @param[in]   lat2    latitude of the second point         
            !> @return      distance in kilometers   
            function geo_distance(lon1, lat1, lon2, lat2) result (res)
                ${T}, intent(in) :: lon1, lat1, lon2, lat2
                ${T} :: res

                ! local variables
                ${T} :: b, r_earth, xmult

                ! the actual calculation
                r_earth=6371.
                b = atan(1.0)/45.0
                xmult = cos((lat1+lat2)*b/2.)*cos((lat1+lat2)*b/2.)
                res = r_earth*sqrt((lon1-lon2)*(lon1-lon2)*b*b*xmult+(lat1-lat2)*(lat1-lat2)*b*b)
            end function
        !$FP end do
    !$FP end template


    !$FP template distance_to_polygon
        !$FP do T = {real (kind=4)} {real (kind=8)}

            !> @public
            !> @brief       find the sortest distance between a point a a polygon
            !> @param[in]   polyx   x-coordinate of the polygone points
            !> @param[in]   polyy   y-coordinate of the polygone points
            !> @param[in]   testx   x-coordinate of the point to test
            !> @param[in]   testy   y-coordinate of the point to test
            !> @param[in]   is_geo_distance     if true, the function returns the distance in kilometers
            function distance_to_polygon(polyx, polyy, testx, testy, is_geo_distance) result (res)
                ${T}, dimension(:), intent(in) :: polyx, polyy
                ${T}, intent(in) :: testx, testy
                logical, intent(in), optional :: is_geo_distance
                ${T} :: res
                ! local variables
                integer :: i, npoints, next_point
                logical :: is_geo_distance_intern
                ${T}, dimension(2) :: u, n, p, t, r0, ref
                ${T} :: dist, a, ul

                is_geo_distance_intern = .false.
                if (present(is_geo_distance)) is_geo_distance_intern = is_geo_distance

                ! the point to test
                p = (/testx, testy/)

                ! the calculation
                res = -1
                npoints = size(polyx)
                do i = 1, npoints
                    if (i < npoints) then 
                        next_point = i + 1
                    else
                        next_point = 1
                    end if
                    ! is this point different from the last point?
                    if (polyx(next_point) /= polyx(i) .and. polyy(next_point) /= polyy(i)) then
                        ! direction of line between two points
                        u = (/polyx(next_point) - polyx(i), polyy(next_point) - polyy(i)/)
                        r0 = (/polyx(i), polyy(i)/)
                        ! length of line
                        ul = sqrt(dot_product(u,u))
                        ! perpendicular direction
                        n = (/u(2)*(-1), u(1)/)
                        ! perpendicular point (lot)
                        t = p - dot_product(p-r0, n) / dot_product(n,n) * n - r0
                        a = sqrt(dot_product(t,t)) / ul

                        !x = polyx(next_point) - polyx(i)
                        !y = polyy(next_point) - polyy(i)
                        !l = x**2 + y**2
                        !u = ((testx - polyx(i))*x + (testy - polyy(i))*y)/l
                        if (a .lt. 0.0) then 
                            a = 0.0
                        else if (a .gt. 1) then
                            a = 1.0
                        end if 

                        ! refx and refy are the coordinates of the reference point of a perpendicular line from the test point to the segment of the polyline
                        ref = r0 + a*u
                    else
                        ref(1) = polyx(i)
                        ref(2) = polyy(i)                  
                    end if

                    if (is_geo_distance_intern) then
                        dist = geo_distance(testx, testy, ref(1), ref(2))
                    else
                        dist = sqrt((ref(1) - testx)**2 + (ref(2) - testy)**2)
                    end if

                    ! is this the shortest distance?
                    if (res == -1 .or. dist < res) res = dist
                end do
            end function
        !$FP end do
    !$FP end template


    !$FP template lonrot_to_lon
        !$FP T = {real (kind=4)} {real (kind=8)}
        !$FP do i = 1, 2
    
            !> @public
            !> @brief       Convert rotated longitude to real longitude
            !> @details     This function is based in CDO source code! 
            !>              This function converts lambda from one rotated system to lambda in another
            !>              system. If the optional argument polgam is present, the other system
            !>              can also be a rotated one, where polgam is the angle between the two
            !>              north poles.
            !>              If polgam is not present, the other system is the real geographical
            !>              system.
            !> @param[in]   phirot    latitude in the rotated system
            !> @param[in]   lamrot    longitude in the rotated system (E>0)
            !> @param[in]   polphi    latitude of the rotated north pole
            !> @param[in]   pollam    longitude of the rotated north pole
            !> @param[in]   polgam    angle between both north poles
            !> @return      longitude in the geographical system
            function lonrot_to_lon(phirot, lamrot, polphi, pollam, polgam) result (res)
                ${T(i)}, intent(in) :: phirot, lamrot, polphi, pollam, polgam
                ${T(i)} :: res

                ! local variables
                ${T(i)} :: zsinpol, zcospol, zlampol
                ${T(i)} :: zphirot, zlamrot, zarg1, zarg2
                ${T(i)} :: zgam, DEG2RAD, RAD2DEG

                !$FP if (i==1) then
                DEG2RAD = atan(1.0) / 45.0
                RAD2DEG = 45.0 / atan(1.0)
                !$FP else
                DEG2RAD = atan(1.0_8) / 45.0_8
                RAD2DEG = 45.0_8 / atan(1.0_8)
                !$FP end if

                zsinpol = sin(DEG2RAD*polphi)
                zcospol = cos(DEG2RAD*polphi)

                zlampol = DEG2RAD*pollam
                zphirot = DEG2RAD*phirot
                if ( lamrot > 180.0 ) then 
                    zlamrot = DEG2RAD*(lamrot-360.0)
                else
                    zlamrot = DEG2RAD*lamrot
                end if

                if ( polgam > 0 ) then
                    zgam  = DEG2RAD*polgam
                    zarg1 = sin(zlampol) * (-zsinpol*cos(zphirot) * (cos(zlamrot)*cos(zgam) - sin(zlamrot)*sin(zgam)) + zcospol*sin(zphirot)) - cos(zlampol)*cos(zphirot) * (sin(zlamrot)*cos(zgam) + cos(zlamrot)*sin(zgam))
                    zarg2 = cos(zlampol) * (-zsinpol*cos(zphirot) * (cos(zlamrot)*cos(zgam) - sin(zlamrot)*sin(zgam)) + zcospol*sin(zphirot)) + sin(zlampol)*cos(zphirot) * (sin(zlamrot)*cos(zgam) + cos(zlamrot)*sin(zgam))
                else
                    zarg1 = sin(zlampol)*(- zsinpol*cos(zlamrot)*cos(zphirot) + zcospol * sin(zphirot)) - cos(zlampol) * sin(zlamrot)*cos(zphirot)
                    zarg2 = cos(zlampol)*(- zsinpol*cos(zlamrot)*cos(zphirot) + zcospol * sin(zphirot)) + sin(zlampol) * sin(zlamrot)*cos(zphirot)
                end if

                if ( abs(zarg2) < 1.0e-20 ) zarg2 = 1.0e-20

                res = (RAD2DEG*atan2(zarg1, zarg2))
            end function
        !$FP end do
    !$FP end template


    !$FP template latrot_to_lat
        !$FP T = {real (kind=4)} {real (kind=8)}
        !$FP do i = 1, 2
    
            !> @public
            !> @brief       Convert rotated latitude to real latitude
            !> @details     This function converts phi from one rotated system to phi in another
            !>              system. If the optional argument polgam is present, the other system
            !>              can also be a rotated one, where polgam is the angle between the two
            !>              north poles.
            !>              If polgam is not present, the other system is the real geographical
            !>              system.
            !> @param[in]   phirot    latitude in the rotated system
            !> @param[in]   lamrot    longitude in the rotated system (E>0)
            !> @param[in]   polphi    latitude of the rotated north pole
            !> @param[in]   polgam    angle between the north poles of the systems
            !> @return      latitude in the geographical system
            function latrot_to_lat(phirot, lamrot, polphi, polgam) result (res)
                ${T(i)}, intent(in) :: phirot, lamrot, polphi, polgam
                ${T(i)} :: res

                ! local variables
                ${T(i)} :: zsinpol, zcospol
                ${T(i)} :: zphirot, zlamrot, zarg
                ${T(i)} :: zgam, DEG2RAD, RAD2DEG

                !$FP if (i==1) then
                DEG2RAD = atan(1.0) / 45.0
                RAD2DEG = 45.0 / atan(1.0)
                !$FP else
                DEG2RAD = atan(1.0_8) / 45.0_8
                RAD2DEG = 45.0_8 / atan(1.0_8)
                !$FP end if

                zsinpol = sin(DEG2RAD*polphi)
                zcospol = cos(DEG2RAD*polphi)

                zphirot = DEG2RAD*phirot
                if ( lamrot > 180.0 ) then 
                    zlamrot = DEG2RAD*(lamrot-360.0)
                else
                    zlamrot = DEG2RAD*lamrot
                end if

                if ( polgam > 0 ) then
                    zgam = DEG2RAD*polgam
                    zarg = zsinpol*sin(zphirot) + zcospol*cos(zphirot)*(cos(zlamrot)*cos(zgam) - sin(zgam)*sin(zlamrot))
                else
                    zarg = zcospol*cos(zphirot)*cos(zlamrot) + zsinpol*sin(zphirot)
                end if

                res = (RAD2DEG*asin(zarg))
            end function
        !$FP end do
    !$FP end template


    !$FP template is_2d_grid_rotated
        !$FP T = {real (kind=4)} {real (kind=8)}
        !$FP do i = 1, 2
    
            !> @public
            !> @brief       Check if the coordinates of a curvilinear grid belong to an unrotated lon-lat-grid
            !> @param[in]   lon2d     2D-longitude variable (x,y)
            !> @param[in]   lat2d     2D-latitude variable (x,y)
            !> @return      true if the grid is rotated, false if the grid is representable with a 1d coordinate
            function is_2d_grid_rotated(lon2d, lat2d) result (res)
                ${T(i)}, dimension(:,:), intent(in) :: lon2d, lat2d
                logical :: res

                ! local variables
                integer :: i, j, nx, ny
                ${T(i)} :: dx, dy

                ! get array dimensions
                nx = size(lon2d, 1)
                ny = size(lon2d, 2)
                dx = abs(lon2d(1,1) - lon2d(2,1))
                dy = abs(lat2d(1,1) - lat2d(1,2))
                if (nx /= size(lat2d, 1) .or. ny /= size(lat2d, 2)) call fplus_error_print("size of lon2d and lat2d differ!", "is_2d_grid_rotated")

                ! check longitudes
                do i = 1, nx
                    ! do the lon values depend on lat values?
                    if (all(abs(lon2d(i,:) - lon2d(i,1)) .lt. dx*0.0001)) cycle
                    ! yes, this grid is rotated!
                    res = .true.
                    return
                end do

                ! check latitude
                do i = 1, ny
                    ! do the lat values depend on lon values?
                    if (all(abs(lat2d(:,i) - lat2d(1,i)) .lt. dy*0.0001)) cycle
                    ! yes, this grid is rotated!
                    res = .true.
                    return
                end do

                ! the grid is not rotated!
                res = .false.
            end function
        !$FP end do
    !$FP end template
end module fplus_geo_tools
