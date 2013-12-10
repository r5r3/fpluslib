!> @brief       A collection of functions related to calculations in wind speed and direction
!> @author      Robert Schuster
module fplus_wind
    use fplus_error
    implicit none
    private

    ! public procedures
    public dir_spd_2_u_v, u_v_2_dir_spd, wind_mean

    !$FP interface template dir_spd_2_u_v
    !$FP interface template u_v_2_dir_spd
    !$FP interface template wind_mean

contains
    
    !$FP template dir_spd_2_u_v
        !$FP dtypes = {real (kind=4)} {real (kind=8)}
        !$FP do i = 1, 2
            !> @brief       Convert wind speed and wind direction to u- und v- wind
            !> @param[in]   dir     direction from which the wind blows 
            !> @param[in]   speed   wind speed 
            !> @param[out]  u       wind speed in u-direction
            !> @param[out]  v       wind speed in v-direction
            elemental subroutine dir_spd_2_u_v(dir, speed, u, v)
                ${dtypes(i)}, intent(in) :: dir, speed
                ${dtypes(i)}, intent(out) :: u, v
                u = sin((dir - 180) / 45.0 * atan(1.0)) * speed
                v = cos((dir - 180) / 45.0 * atan(1.0)) * speed
            end subroutine
        !$FP end do
    !$FP end template

    !$FP template u_v_2_dir_spd
        !$FP dtypes = {real (kind=4)} {real (kind=8)}
        !$FP do i = 1, 2
            !> @brief       Convert wind speed and wind direction to u- und v- wind
            !> @param[in]   u       wind speed in u-direction
            !> @param[in]   v       wind speed in v-direction
            !> @param[out]  dir     direction from which the wind blows 
            !> @param[out]  speed   wind speed 
            elemental subroutine u_v_2_dir_spd(u, v, dir, speed)
                ${dtypes(i)}, intent(in) :: u, v
                ${dtypes(i)}, intent(out) :: dir, speed
                speed = sqrt(u**2 + v**2)
                dir = atan2(u, v) * 45.0 / atan(1.0) + 180.0
            end subroutine
        !$FP end do
    !$FP end template

    !$FP template wind_mean
        !$FP dtypes = {real (kind=4)} {real (kind=8)}
        !$FP do i = 1, 2
            !> @brief       Convert wind speed and wind direction to u- und v- wind
            !> @param[in]   dir_in      array of the directions
            !> @param[in]   speed_in    array of wind speed
            !> @param[out]  dir_out     mean wind direction 
            !> @param[out]  speed_out   mean wind speed
            subroutine wind_mean(dir_in, speed_in, dir_out, speed_out)
                ${dtypes(i)}, intent(in), dimension(:) :: dir_in, speed_in
                ${dtypes(i)}, intent(out) :: dir_out, speed_out

                ! local variables
                ${dtypes(i)}, dimension(:), allocatable :: u, v
                ${dtypes(i)} :: umean, vmean

                ! size of both input arrays must be equal
                if (size(dir_in) /= size(speed_in)) call fplus_error_print("different size of dir and speed arrays!", "wind_mean")

                ! calculate u and v wind components
                allocate(u(size(dir_in)), v(size(dir_in)))
                call dir_spd_2_u_v(dir_in, speed_in, u, v)
                umean = sum(u) / size(u)
                vmean = sum(v) / size(v)

                ! calculate the result
                call u_v_2_dir_spd(umean, vmean, dir_out, speed_out) 
            end subroutine
        !$FP end do
    !$FP end template
end module
