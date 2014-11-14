program test_rotate
    use fplus_geo_tools

    real, dimension(4) :: lon, lon2, lon3
    real, dimension(:), allocatable :: lon4
    integer :: npoints

    lon = (/0,90,180,270/)

    where (lon > 180)
        lon2 = lon -360
    elsewhere
        lon2 = lon
    end where

    lon3 = cshift(lon2, 3) 
    call rot_lon_to_range((/-10.0,100.0/), lon, lon4, npoints)

    print*, lon
    print*, lon2
    print*, lon3
    print*, lon4, "n=", npoints

end program
