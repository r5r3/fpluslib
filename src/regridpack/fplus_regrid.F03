!> @brief       Functions for regridding of 1- to 4-dimensions fields
!> @details     Functions from the REGRIDPACK (John C. Adams, NCAR 1999) are used.
module fplus_regrid
    use mod_rgrd1
    use mod_rgrd2
    use mod_rgrd3
    use mod_rgrd4
    use mod_rgrd1u
    use mod_rgrd2u
    use mod_rgrd3u
    use mod_rgrd4u    
    use fplus_error
    implicit none
    private

    public :: regrid
    !$FP interface template regrid

contains 

    !$FP template regrid
        !$FP dims = {:} {:,:} {:,:,:} {:,:,:,:}
        !$FP do i = 1, 4

                            !> @brief   Interpolate a non uniform grid to a second non uniform grid. 
                            !> @param   inarray     the input-data with ${i} dimensions
                            !> @param   outarray    the output-data with ${i} dimensions
                            !> @param   inx         the 1st coordinate of the input-data, optional
            !$FP if (i > 1) !> @param   iny         the 2nd coordinate of the input-data, optional
            !$FP if (i > 2) !> @param   inz         the 3rd coordinate of the input-data, optional
            !$FP if (i > 3) !> @param   int         the 4th coordinate of the input-data, optional
                            !> @param   outx        the 1st coordinate of the output-data, optional
            !$FP if (i > 1) !> @param   outy        the 2nd coordinate of the output-data, optional
            !$FP if (i > 2) !> @param   outz        the 3rd coordinate of the output-data, optional
            !$FP if (i > 3) !> @param   outt        the 4th coordinate of the output-data, optional
                            !> @param   intpol      optional array with interpolation type for each dimension. 1=linear, 3=cubic, default=1.
                            !> @result  error code, 0 = success
            function regrid(inarray, outarray, inx &
                !$FP if (i > 1) ,iny &
                !$FP if (i > 2) ,inz &
                !$FP if (i > 3) ,int &
                ,outx &
                !$FP if (i > 1) ,outy &
                !$FP if (i > 2) ,outz &
                !$FP if (i > 3) ,outt &
                ,intpol) result (ierr)

                real (kind=8), dimension(${dims(i)}), intent(in) :: inarray
                real (kind=8), dimension(${dims(i)}), intent(out) :: outarray
                real (kind=8), intent(in), dimension(:), optional :: inx, outx
                !$FP if (i > 1) real (kind=8), intent(in), dimension(:), optional :: iny, outy
                !$FP if (i > 2) real (kind=8), intent(in), dimension(:), optional :: inz, outz
                !$FP if (i > 3) real (kind=8), intent(in), dimension(:), optional :: int, outt
                integer :: ierr
                integer, dimension(${i}), optional :: intpol

                ! local variables
                integer :: mx, my, mz, mt                                   ! size of the new coordinates
                integer :: nx, ny, nz, nt                                   ! size of the old coordinates
                real (kind=8), dimension(:), allocatable :: rworkspace      ! real workspace 
                integer :: lw, lwx, lwy, lwz, lwt                           ! size of the real workspace
                integer, dimension(:), allocatable :: iworkspace            ! integer workspace
                integer :: liw                                              ! size of integer workspace
                integer, dimension(${i}) :: intpol_intern                   ! type of interpolation
                logical :: uniform                                          ! are the arrays uniform and of the same size?

                ! get the type of interpolation
                if (present(intpol)) then
                    intpol_intern = intpol
                else
                    intpol_intern = 1
                end if

                ! check for uniformity
                uniform = .true.
                if (present(inx) .or. present(outx)) uniform = .false.
                !$FP if (i > 1) if (present(iny) .or. present(outy)) uniform = .false.
                !$FP if (i > 2) if (present(inx) .or. present(outz)) uniform = .false.
                !$FP if (i > 3) if (present(int) .or. present(outt)) uniform = .false.
                if (uniform .eqv. .false.) then
                    if (.not.present(inx) .or. .not.present(outx)) uniform = .true.
                    !$FP if (i > 1) if (.not.present(iny) .or. .not.present(outy)) uniform = .true.
                    !$FP if (i > 2) if (.not.present(inx) .or. .not.present(outz)) uniform = .true.
                    !$FP if (i > 3) if (.not.present(int) .or. .not.present(outt)) uniform = .true.
                    if (uniform) call fplus_error_print("please provide all coordinate-arrays for non uniform arrays", "regrid")
                end if


                ! get the size of the new coordinates
                mx = size(outarray,1)
                !$FP if (i > 1) my = size(outarray,2)
                !$FP if (i > 2) mz = size(outarray,3)
                !$FP if (i > 3) mt = size(outarray,4)

                ! get the size of the old coordinates
                nx = size(inarray,1)
                !$FP if (i > 1) ny = size(inarray,2)
                !$FP if (i > 2) nz = size(inarray,3)
                !$FP if (i > 3) nt = size(inarray,4)

                !$FP if (i == 2) then
                !print*, "mx=", mx, " my=",my
                !IF (outx(1) < inx(1) .OR. outx(mx) > inx(nx)) print*, outx(1), inx(1), outx(mx), inx(nx)
                !IF (outy(1) < iny(1) .OR. outy(my) > iny(ny)) print*, outy(1), iny(1), outy(my), iny(ny)
                !$FP end if

                ! create the local real workspace
                lwx = 4*mx
                !$FP if (i > 1) then
                    lwy = 4*(my+mx)
                !$FP else
                    lwy = 0
                !$FP end if
                !$FP if (i > 2) then
                    lwz = 4*(mx*my+mz)
                !$FP else
                    lwz = 0
                !$FP end if
                !$FP if (i > 2) then
                    lwt = 4*(mx*my*mz+mt)
                !$FP else
                    lwt = 0
                !$FP end if
                lw = lwx + lwy + lwz + lwt
                allocate(rworkspace(lw))

                ! create the local integer workspace
                liw = mx
                !$FP if (i > 1) liw = liw + my
                !$FP if (i > 2) liw = liw + mz
                !$FP if (i > 3) liw = liw + mt
                allocate(iworkspace(liw))

                ! perform the interpolation itself
                ! uniform grid:
                if (uniform .eqv. .true.) then
                    !$FP if (i == 1) call rgrd1u(nx,inarray,mx,outarray,intpol_intern(1),rworkspace,lw,iworkspace,liw,ierr)
                    !$FP if (i == 2) call rgrd2u(nx,ny,inarray,mx,my,outarray,intpol_intern,rworkspace,lw,iworkspace,liw,ierr)
                    !$FP if (i == 3) call rgrd3u(nx,ny,nz,inarray,mx,my,mz,outarray,intpol_intern,rworkspace,lw,iworkspace,liw,ierr)
                    !$FP if (i == 4) call rgrd4u(nx,ny,nz,nt,inarray,mx,my,mz,mt,outarray,intpol_intern,rworkspace,lw,iworkspace,liw,ierr)
                else
                    !$FP if (i == 1) call rgrd1(nx,inx,inarray,mx,outx,outarray,intpol_intern(1),rworkspace,lw,iworkspace,liw,ierr)
                    !$FP if (i == 2) call rgrd2(nx,ny,inx,iny,inarray,mx,my,outx,outy,outarray,intpol_intern,rworkspace,lw,iworkspace,liw,ierr)
                    !$FP if (i == 3) call rgrd3(nx,ny,nz,inx,iny,inz,inarray,mx,my,mz,outx,outy,outz,outarray,intpol_intern,rworkspace,lw,iworkspace,liw,ierr)
                    !$FP if (i == 4) call rgrd4(nx,ny,nz,nt,inx,iny,inz,int,inarray,mx,my,mz,mt,outx,outy,outz,outt,outarray,intpol_intern,rworkspace,lw,iworkspace,liw,ierr)
                end if
            end function
        !$FP end do
    !$FP end template
end module
