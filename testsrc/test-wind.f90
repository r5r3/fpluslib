program test_wind
    use fplus_wind
    implicit none

    real :: u, v, dir, speed
    real, dimension(4) :: ua, va, dira, speeda

    ! Wind from SW
    u = 1
    v = 1
    call u_v_2_dir_spd(u, v, dir, speed)
    print*, "u=",u,"v=",v,"dir=",dir,"speed=",speed

    ! Wind from SE
    u = -1
    v = 1
    call u_v_2_dir_spd(u, v, dir, speed)
    print*, "u=",u,"v=",v,"dir=",dir,"speed=",speed

    ! Wind from NE
    u = -1
    v = -1
    call u_v_2_dir_spd(u, v, dir, speed)
    print*, "u=",u,"v=",v,"dir=",dir,"speed=",speed

    ! Wind from NW
    u = 1
    v = -1
    call u_v_2_dir_spd(u, v, dir, speed)
    print*, "u=",u,"v=",v,"dir=",dir,"speed=",speed

    ! no wind
    u = 0
    v = 0
    call u_v_2_dir_spd(u, v, dir, speed)
    print*, "u=",u,"v=",v,"dir=",dir,"speed=",speed

    ! array calculation
    ua = (/1,-1,-1,1/)
    va = (/1,1,-1,-1/)
    call u_v_2_dir_spd(ua, va, dira, speeda)
    print*, "dirs=  ", dira
    print*, "speeds=", speeda

    ! Wind from SW
    dir = 225
    speed = sqrt(2.0)
    call dir_spd_2_u_v(dir, speed, u, v)
    print*, "u=",u,"v=",v,"dir=",dir,"speed=",speed

    ! Wind from SE
    dir = 135
    speed = sqrt(2.0)
    call dir_spd_2_u_v(dir, speed, u, v)
    print*, "u=",u,"v=",v,"dir=",dir,"speed=",speed

    ! Wind from NE
    dir = 45
    speed = sqrt(2.0)
    call dir_spd_2_u_v(dir, speed, u, v)
    print*, "u=",u,"v=",v,"dir=",dir,"speed=",speed

    ! Wind from NW
    dir = 315
    speed = sqrt(2.0)
    call dir_spd_2_u_v(dir, speed, u, v)
    print*, "u=",u,"v=",v,"dir=",dir,"speed=",speed

    ! no wind
    dir = 0
    speed = 0
    call dir_spd_2_u_v(dir, speed, u, v)
    print*, "u=",u,"v=",v,"dir=",dir,"speed=",speed

    ! array calculation
    dira = (/225,135,45,315/)
    speeda = sqrt(2.0)
    call dir_spd_2_u_v(dira, speeda, ua, va)
    print*, "u=", ua
    print*, "v=", va

    ! average calculation
    dira = (/355, 356, 4, 5/)
    speeda = (/1,1,2,2/)
    call wind_mean(dira, speeda, dir, speed)
    print*, "Mean of:"
    print*, "dir=  ", dira
    print*, "speed=", speeda
    print*, "result:"
    print*, "dir=  ", dir
    print*, "speed=", speed
end program