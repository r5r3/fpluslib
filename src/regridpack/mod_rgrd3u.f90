module mod_rgrd3u
    use mod_rgrd2u
    implicit none
    
contains
    !

    ! Code converted using TO_F90 by Alan Miller
    ! Date: 2013-09-18  Time: 12:23:25

    ! ... file rgrd3u.f

    !     this file contains documentation for subroutine rgrd3u followed by
    !     fortran code for rgrd3u and additional subroutines.

    ! ... author

    !     John C. Adams (NCAR 1999)

    ! ... subroutine rgrd3u(nx,ny,nz,p,mx,my,mz,q,intpol,w,lw,iw,liw,ier)

    ! ... purpose

    !     subroutine rgrd3u interpolates the nx by ny by nz array p onto
    !     the mx by my by mz array q.  it is assumed that p and q are
    !     values on uniform nx by ny by nz and mx by my by mz grids which
    !     are superimposed on the same box region (INCLUDING BOUNDARIES).
    !     if p and q are values on nonuniform orthogonal grids and/or
    !     if the grid on which q is defined lies within the p grid then
    !     subroutine rgrd3 (see file rgrd3.f) should be used.

    ! ... language

    !     coded in portable FORTRAN77 and FORTRAN90

    ! ... test program

    !     file trgrd3u.f on regridpack includes a test program for subroutine rgrd3u

    ! ... method

    !     linear or cubic interpolation (see intpol) is used in each
    !     direction for which the q grid is not a subgrid of the p grid.
    !     [the mx (my,mz) uniform grid is a subgrid of the nx (ny,nz) uniform
    !     grid if and only if mx-1 (my-1,nz-1) divides nx-1 (ny-1,nz-1)].
    !     Values are set directly without (the need for) interpolation
    !     in subgrid directions.


    ! ... required files

    !     files rgrd2u.f and rgrd1u.f must be loaded with rgrd3u.f.  they
    !     include subroutines called by the routines in rgrd3u.f

    ! ... efficiency

    !     inner most loops in regridpack software vectorize.  If the
    !     arguments mx,my,mz (see below) have different values, optimal
    !     vectorization will be achieved if mx > my > mz.


    ! *** input arguments ***


    ! ... nx

    !     the integer first dimension of p.  nx > 1 if intpol(1) = 1 or
    !     nx > 3 if intpol(1) = 3 is required (see ier = 2).

    ! ... ny

    !     the integer second dimension of p.  ny > 1 if intpol(2) = 1 or
    !     ny > 3 if intpol(2) = 3 is required (see ier = 2).


    ! ... nz

    !     the integer third dimension of p.  nz > 1 if intpol(3) = 1 or
    !     nz > 3 if intpol(3) = 3 is required (see ier = 2)

    ! ... p

    !     a real (kind=8) nx by ny by nz array of given values

    ! ... mx

    !     the integer first dimension of q.  mx > 1 is required (see ier = 1)

    ! ... my

    !     the integer second dimension of q. my > 1 is required (see ier = 1)

    ! ... mz

    !     the integer third dimension of q. mz > 1 is required (see ier = 1)

    ! ... intpol

    !     an integer vector of dimension 3 which sets linear or cubic
    !     interpolation in each of the x,y,z directions as follows:

    !        intpol(1) = 1 sets linear interpolation in the x direction
    !        intpol(1) = 3 sets cubic interpolation in the x direction.

    !        intpol(2) = 1 sets linear interpolation in the y direction
    !        intpol(2) = 3 sets cubic interpolation in the y direction.

    !        intpol(3) = 1 sets linear interpolation in the z direction
    !        intpol(3) = 3 sets cubic interpolation in the z direction.

    !     values other than 1 or 3 in intpol are not allowed (ier = 3).

    ! ... w

    !     a real (kind=8) work space of length at least lw which must be provided in the
    !     routine calling rgrd3u

    ! ... lw

    !     the integer length of the real (kind=8) work space w.

    !          let lwx = 1 if mx-1 divides nx-1; otherwise
    !          let lwx =   mx if intpol(1) = 1 or
    !          let lwx = 4*mx if intpol(1) = 3

    !          let lwy = 0 if my-1 divides ny-1; otherwise
    !          let lwy = my+2*mx if intpol(2) = 1 or
    !          let lwy = 4*(mx+my) if intpol(2) = 3

    !          let lwz = 0 if mz-1 divides nz-1; otherwise
    !          let lwz = 2*mx*my+mz if intpol(3) = 1 or
    !          let lwz = 4*(mx*my+mz) if intpol(3) = 3

    !     then lw must be greater than or equal to lwx+lwy+lwz

    ! ... iw

    !     an integer work space of length at least liw which must be provided in the
    !     routine calling rgrd3u

    ! ... liw

    !     the integer length of the integer work space iw.  liw must be greater than
    !     or equal to mx+my+mz


    ! *** output arguments ***


    ! ... q

    !     a real (kind=8) mx by my by mz array of values which are interpolated from p.

    ! ... ier

    !     an integer error flag set as follows:

    !     ier = 0 if no errors in input arguments are detected

    !     ier = 1 if  min0(mx,my,mz) < 2

    !     ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
    !                ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3 (or)
    !                nz < 2 when intpol(3)=1 or nz < 4 when intpol(3)=3.

    !     ier = 3 if any of intpol(1),intpol(2),intpol(3) is not equal to 1 or 3

    !     ier = 4 if lw or liw is too small (insufficient work space)


    ! ************************************************************************

    !     end of rgrd3u documentation, fortran code follows:

    ! ************************************************************************


    SUBROUTINE rgrd3u(nx,ny,nz,p,mx,my,mz,q,intpol,w,lw,iw,liw,ier)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: ny
    INTEGER, INTENT(IN)                      :: nz
    real (kind=8), INTENT(IN)                :: p(nx,ny,nz)
    INTEGER, INTENT(IN)                      :: mx
    INTEGER, INTENT(IN)                      :: my
    INTEGER, INTENT(IN)                      :: mz
    real (kind=8), INTENT(OUT)               :: q(mx,my,mz)
    INTEGER, INTENT(IN)                      :: intpol(3)
    real (kind=8), INTENT(INOUT)             :: w(lw)
    INTEGER, INTENT(IN)                      :: lw
    INTEGER, INTENT(INOUT)                   :: iw(liw)
    INTEGER, INTENT(IN)                      :: liw
    INTEGER, INTENT(OUT)                     :: ier


    INTEGER :: inmx,jnmy,knmz,isubx,jsuby,ksubz
    INTEGER :: lwx,lwy,lwz,mxmy,jy,kz
    INTEGER :: i2,i3,i4,i5
    INTEGER :: j2,j3,j4,j5,j6,j7,j8,j9
    INTEGER :: k2,k3,k4,k5,k6,k7,k8,k9

    !     check input arguments

    ier = 1

    !     check mx,my,mz

    IF (MIN0(mx,my,mz) < 1) RETURN

    !     check intpol

    ier = 3
    IF (intpol(1) /= 1 .AND. intpol(1) /= 3) RETURN
    IF (intpol(2) /= 1 .AND. intpol(2) /= 3) RETURN
    IF (intpol(3) /= 1 .AND. intpol(3) /= 3) RETURN

    !     check nx,ny,nz

    ier = 2
    IF (intpol(1) == 1 .AND. nx < 2) RETURN
    IF (intpol(1) == 3 .AND. nx < 4) RETURN
    IF (intpol(2) == 1 .AND. ny < 2) RETURN
    IF (intpol(2) == 3 .AND. ny < 4) RETURN
    IF (intpol(3) == 1 .AND. nz < 2) RETURN
    IF (intpol(3) == 3 .AND. nz < 4) RETURN

    !     set subgrid indicators

    inmx = (nx-1)/(mx-1)
    jnmy = (ny-1)/(my-1)
    knmz = (nz-1)/(mz-1)
    isubx = nx - inmx*(mx-1)
    jsuby = ny - jnmy*(my-1)
    ksubz = nz - knmz*(mz-1)

    !     check work space lengths

    ier = 4
    mxmy = mx*my
    lwx = 1
    IF (isubx /= 1) THEN
      IF (intpol(1) == 1) THEN
        lwx = mx
      ELSE
        lwx = 4*mx
      END IF
    END IF
    lwy = 0
    IF (jsuby /= 1) THEN
      IF (intpol(2) == 1) THEN
        lwy = (2*mx+my)
      ELSE
        lwy = 4*(mx+my)
      END IF
    END IF
    lwz = 0
    IF (ksubz /= 1) THEN
      IF (intpol(3) == 1) THEN
        lwz = (2*mxmy+mz)
      ELSE
        lwz = 4*(mxmy+mz)
      END IF
    END IF
    IF (lw < lwx+lwy+lwz) RETURN
    IF (liw < mx+my+mz) RETURN

    !     arguments o.k.

    ier = 0
    jy = mx+1
    kz = mx+my+1

    !     preset work space pointers

    k2 = 1
    k3 = 1
    k4 = 1
    k5 = 1
    k6 = 1
    k7 = 1
    k8 = 1
    k9 = 1
    j2 = 1
    j3 = 1
    j4 = 1
    j5 = 1
    j6 = 1
    j7 = 1
    j8 = 1
    j9 = 1
    i2 = 1
    i3 = 1
    i4 = 1
    i5 = 1

    IF (intpol(3) == 1) THEN
      IF (ksubz /= 1) THEN
        
    !     linearly interpolate in nz, set work space pointers
        
        k2 = 1
        k3 = k2
        k4 = k3+mz
        k5 = k4
        k6 = k5
        k7 = k6
        k8 = k7+mxmy
        k9 = k8+mxmy
        
    !     set z interpolation indices and scales
        
        CALL linmxu(nz,mz,iw(kz),w(k3))
        j2 = k9
        i2 = k9
      END IF
      
      IF (jsuby /= 1) THEN
        IF (intpol(2) == 1) THEN
    !     linear in y
          j3 = j2
          j4 = j3+my
          j5 = j4
          j6 = j5
          j7 = j6
          j8 = j7+mx
          j9 = j8+mx
          CALL linmxu(ny,my,iw(jy),w(j3))
          i2 = j9
        ELSE
    !     cubic in y
          j3 = j2+my
          j4 = j3+my
          j5 = j4+my
          j6 = j5+my
          j7 = j6+mx
          j8 = j7+mx
          j9 = j8+mx
          CALL cubnmxu(ny,my,iw(jy),w(j2),w(j3),w(j4),w(j5))
          i2 = j9+mx
        END IF
      END IF
      
      IF (isubx /= 1) THEN
        IF (intpol(1) == 1) THEN
    !     linear in x
          i3 = i2
          i4 = i3
          i5 = i4
          CALL linmxu(nx,mx,iw,w(i3))
        ELSE
    !     cubic in x
          i3 = i2+mx
          i4 = i3+mx
          i5 = i4+mx
          CALL cubnmxu(nx,mx,iw,w(i2),w(i3),w(i4),w(i5))
        END IF
      END IF
      
    !     linearly interpolate p onto q in z
      
      CALL lint3u(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,iw(kz),w(k3),  &
          w(k7),w(k8),iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),  &
          w(j8),w(j9),iw,w(i2),w(i3),w(i4),w(i5), inmx,jnmy,knmz,isubx,jsuby,ksubz)
      RETURN
      
    ELSE
      
    !     cubically interpolate in z
      
      IF (ksubz /= 1) THEN
        k2 = 1
        k3 = k2+mz
        k4 = k3+mz
        k5 = k4+mz
        k6 = k5+mz
        k7 = k6+mxmy
        k8 = k7+mxmy
        k9 = k8+mxmy
        CALL cubnmxu(nz,mz,iw(kz),w(k2),w(k3),w(k4),w(k5))
        j2 = k9+mxmy
        i2 = j2
      END IF
      
      IF (jsuby /= 1) THEN
        IF (intpol(2) == 1) THEN
          j3 = j2
          j4 = j3+my
          j5 = j4
          j6 = j5
          j7 = j6
          j8 = j7+mx
          j9 = j8+mx
          CALL linmxu(ny,my,iw(jy),w(j3))
          i2 = j9
        ELSE
          j3 = j2+my
          j4 = j3+my
          j5 = j4+my
          j6 = j5+my
          j7 = j6+mx
          j8 = j7+mx
          j9 = j8+mx
          CALL cubnmxu(ny,my,iw(jy),w(j2),w(j3),w(j4),w(j5))
          i2 = j9+mx
        END IF
      END IF
      
      IF (isubx /= 1) THEN
        IF (intpol(1) == 1) THEN
          i3 = i2
          i4 = i3
          i5 = i4
          CALL linmxu(nx,mx,iw,w(i3))
        ELSE
          i3 = i2+mx
          i4 = i3+mx
          i5 = i4+mx
          CALL cubnmxu(nx,mx,iw,w(i2),w(i3),w(i4),w(i5))
        END IF
      END IF
      
    !     cubically interpolate p onto q in z
      
      CALL cubt3u(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,  &
          iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),  &
          iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),  &
          iw,w(i2),w(i3),w(i4),w(i5), inmx,jnmy,knmz,isubx,jsuby,ksubz)
      
      RETURN
    END IF
    END SUBROUTINE rgrd3u

    SUBROUTINE lint3u(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,kz,  &
        dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
        inmx,jnmy,knmz,isubx,jsuby,ksubz)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: ny
    INTEGER, INTENT(IN)                      :: nz
    real (kind=8), INTENT(IN)                :: p(nx,ny,nz)
    INTEGER, INTENT(IN)                      :: mx
    INTEGER, INTENT(IN)                      :: my
    INTEGER, INTENT(IN)                      :: mxmy
    INTEGER, INTENT(IN)                      :: mz
    real (kind=8), INTENT(OUT)               :: q(mxmy,mz)
    INTEGER, INTENT(IN)                      :: intpol(3)
    INTEGER, INTENT(IN)                      :: kz(mz)
    real (kind=8), INTENT(IN)                :: dz(mz)
    real (kind=8), INTENT(OUT)               :: pk(mxmy)
    real (kind=8), INTENT(IN OUT)            :: pkp(mxmy)
    INTEGER, INTENT(IN OUT)                  :: jy(my)
    real (kind=8), INTENT(IN OUT)            :: dym(my)
    real (kind=8), INTENT(IN OUT)            :: dy(my)
    real (kind=8), INTENT(IN OUT)            :: dyp(my)
    real (kind=8), INTENT(IN OUT)            :: dypp(my)
    real (kind=8), INTENT(IN OUT)            :: pjm(mx)
    real (kind=8), INTENT(IN OUT)            :: pj(mx)
    real (kind=8), INTENT(IN OUT)            :: pjp(mx)
    real (kind=8), INTENT(IN OUT)            :: pjpp(mx)
    INTEGER, INTENT(IN OUT)                  :: ix(mx)
    real (kind=8), INTENT(IN OUT)            :: dxm(mx)
    real (kind=8), INTENT(IN OUT)            :: dx(mx)
    real (kind=8), INTENT(IN OUT)            :: dxp(mx)
    real (kind=8), INTENT(IN OUT)            :: dxpp(mx)
    INTEGER, INTENT(IN OUT)                  :: inmx
    INTEGER, INTENT(IN OUT)                  :: jnmy
    INTEGER, INTENT(IN)                      :: knmz
    INTEGER, INTENT(IN OUT)                  :: isubx
    INTEGER, INTENT(IN OUT)                  :: jsuby
    INTEGER, INTENT(IN)                      :: ksubz

    !     linearly interpolate in z direction



    INTEGER :: kk,k,iijj,ksave






    IF (intpol(2) == 1) THEN
      
    !     linear in y
      
      IF (ksubz == 1) THEN
        
    !     mz grid is subset of nz grid
        
        DO kk=1,mz
          k = knmz*(kk-1)+1
          CALL lint2u(nx,ny,p(1,1,k),mx,my,q(1,kk),intpol,jy,dy,  &
              pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
        END DO
        RETURN
      END IF
      
      ksave = -1
      DO kk=1,mz
        k = kz(kk)
        IF (k == ksave) THEN
          
    !     k pointer has not moved since last pass (no updates or interpolation)
          
        ELSE IF (k == ksave+1) THEN
          
    !     update k and interpolate k+1
          
          DO iijj=1,mxmy
            pk(iijj) = pkp(iijj)
          END DO
          CALL lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,  &
              pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
        ELSE
          
    !     interpolate k,k+1 in pk,pkp
          
          CALL lint2u(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,dy,  &
              pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
          CALL lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,  &
              pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
        END IF
        
    !     save k pointer for next pass
        
        ksave = k
        
    !     linearly interpolate q(ii,jj,k) from pk,pkp in z direction
        
        DO iijj=1,mxmy
          q(iijj,kk) = pk(iijj)+dz(kk)*(pkp(iijj)-pk(iijj))
        END DO
      END DO
      RETURN
    ELSE
      
    !     cubic in y
      
      IF (ksubz == 1) THEN
        
    !     mz grid is subset of nz grid
        
        DO kk=1,mz
          k = knmz*(kk-1)+1
          CALL cubt2u(nx,ny,p(1,1,k),mx,my,q(1,kk),intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,isubx,jsuby)
        END DO
        RETURN
      END IF
      
      ksave = -1
      DO kk=1,mz
        k = kz(kk)
        IF (k == ksave) THEN
          
    !     k pointer has not moved since last pass (no updates or interpolation)
          
        ELSE IF (k == ksave+1) THEN
          
    !     update k and interpolate k+1
          
          DO iijj=1,mxmy
            pk(iijj) = pkp(iijj)
          END DO
          CALL cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,isubx,jsuby)
        ELSE
          
    !     interpolate k,k+1 in pk,pkp
          
          CALL cubt2u(nx,ny,p(1,1,k),mx,my,pk,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,isubx,jsuby)
          CALL cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,isubx,jsuby)
        END IF
        
    !     save k pointer for next pass
        
        ksave = k
        
    !     linearly interpolate q(ii,jj,k) from pk,pkp in z direction
        
        DO iijj=1,mxmy
          q(iijj,kk) = pk(iijj)+dz(kk)*(pkp(iijj)-pk(iijj))
        END DO
      END DO
      RETURN
    END IF
    END SUBROUTINE lint3u

    SUBROUTINE cubt3u(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,  &
        kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,  &
        pjp,pjpp,ix,dxm,dx,dxp,dxpp, inmx,jnmy,knmz,isubx,jsuby,ksubz)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: ny
    INTEGER, INTENT(IN)                      :: nz
    real (kind=8), INTENT(IN)                :: p(nx,ny,nz)
    INTEGER, INTENT(IN)                      :: mx
    INTEGER, INTENT(IN)                      :: my
    INTEGER, INTENT(IN)                      :: mxmy
    INTEGER, INTENT(IN)                      :: mz
    real (kind=8), INTENT(OUT)               :: q(mxmy,mz)
    INTEGER, INTENT(IN)                      :: intpol(3)
    INTEGER, INTENT(IN)                      :: kz(mz)
    real (kind=8), INTENT(IN)                :: dzm(mz)
    real (kind=8), INTENT(IN)                :: dz(mz)
    real (kind=8), INTENT(IN OUT)            :: dzp(mz)
    real (kind=8), INTENT(IN OUT)            :: dzpp(mz)
    real (kind=8), INTENT(OUT)               :: pkm(mxmy)
    real (kind=8), INTENT(IN OUT)            :: pk(mxmy)
    real (kind=8), INTENT(IN OUT)            :: pkp(mxmy)
    real (kind=8), INTENT(IN OUT)            :: pkpp(mxmy)
    INTEGER, INTENT(IN OUT)                  :: jy(my)
    real (kind=8), INTENT(IN OUT)            :: dym(my)
    real (kind=8), INTENT(IN OUT)            :: dy(my)
    real (kind=8), INTENT(IN OUT)            :: dyp(my)
    real (kind=8), INTENT(IN OUT)            :: dypp(my)
    real (kind=8), INTENT(IN OUT)            :: pjm(mx)
    real (kind=8), INTENT(IN OUT)            :: pj(mx)
    real (kind=8), INTENT(IN OUT)            :: pjp(mx)
    real (kind=8), INTENT(IN OUT)            :: pjpp(mx)
    INTEGER, INTENT(IN OUT)                  :: ix(mx)
    real (kind=8), INTENT(IN OUT)            :: dxm(mx)
    real (kind=8), INTENT(IN OUT)            :: dx(mx)
    real (kind=8), INTENT(IN OUT)            :: dxp(mx)
    real (kind=8), INTENT(IN OUT)            :: dxpp(mx)
    INTEGER, INTENT(IN OUT)                  :: inmx
    INTEGER, INTENT(IN OUT)                  :: jnmy
    INTEGER, INTENT(IN)                      :: knmz
    INTEGER, INTENT(IN OUT)                  :: isubx
    INTEGER, INTENT(IN OUT)                  :: jsuby
    INTEGER, INTENT(IN)                      :: ksubz

    !     cubically interpolate in z



    INTEGER :: kk,k,iijj,ksave







    IF (intpol(2) == 1) THEN
      
    !     linear in y
      
      IF (ksubz == 1) THEN
        
    !     mz grid is subset of nz grid
        
        DO kk=1,mz
          k = knmz*(kk-1)+1
          CALL lint2u(nx,ny,p(1,1,k),mx,my,q(1,kk),intpol,jy,dy,  &
              pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
        END DO
        RETURN
      END IF
      
    !     mz not a subgrid of nz
      
      ksave = -3
      DO kk=1,mz
        k = kz(kk)
        IF (k == ksave) THEN
          
    !     k pointer has not moved since last pass (no updates or interpolation)
          
        ELSE IF (k == ksave+1) THEN
          
    !     update k-1,k,k+1 and interpolate k+2
          
          DO iijj=1,mxmy
            pkm(iijj) = pk(iijj)
            pk(iijj) = pkp(iijj)
            pkp(iijj) = pkpp(iijj)
          END DO
          CALL lint2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,  &
              dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
        ELSE IF (k == ksave+2) THEN
          
    !     update k-1,k and interpolate k+1,k+2
          
          DO iijj=1,mxmy
            pkm(iijj) = pkp(iijj)
            pk(iijj) = pkpp(iijj)
          END DO
          CALL lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,  &
              dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
          CALL lint2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,  &
              dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
        ELSE IF (k == ksave+3) THEN
          
    !     update k-1 and interpolate k,k+1,k+2
          
          DO iijj=1,mxmy
            pkm(iijj) = pkpp(iijj)
          END DO
          CALL lint2u(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,  &
              dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
          CALL lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,  &
              dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
          CALL lint2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,  &
              dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
        ELSE
          
    !     interpolate all four k-1,k,k+1,k+2
          
          CALL lint2u(nx,ny,p(1,1,k-1),mx,my,pkm,intpol,jy,  &
              dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
          CALL lint2u(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,  &
              dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
          CALL lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,  &
              dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
          CALL lint2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,  &
              dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
          
        END IF
        
    !     save k pointer for next pass
        
        ksave = k
        
    !     cubically interpolate q(ii,jj,kk) from pkm,pk,pkp,pkpp in z direction
        
        DO iijj=1,mxmy
          q(iijj,kk) = dzm(kk)*pkm(iijj) + dz(kk)*pk(iijj) +  &
              dzp(kk)*pkp(iijj) + dzpp(kk)*pkpp(iijj)
        END DO
      END DO
      RETURN
      
    ELSE
      
    !     cubic in y
      
      IF (ksubz == 1) THEN
        
    !     mz grid is subset of nz grid
        
        DO kk=1,mz
          k = knmz*(kk-1)+1
          CALL cubt2u(nx,ny,p(1,1,k),mx,my,q(1,kk),intpol,jy,dym,dy,  &
              dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp, inmx,jnmy,isubx,jsuby)
          
        END DO
        RETURN
      END IF
      
      ksave = -3
      DO kk=1,mz
        k = kz(kk)
        IF (k == ksave) THEN
          
    !     k pointer has not moved since last pass (no updates or interpolation)
          
        ELSE IF (k == ksave+1) THEN
          
    !     update k-1,k,k+1 and interpolate k+2
          
          DO iijj=1,mxmy
            pkm(iijj) = pk(iijj)
            pk(iijj) = pkp(iijj)
            pkp(iijj) = pkpp(iijj)
          END DO
          CALL cubt2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,isubx,jsuby)
          
        ELSE IF (k == ksave+2) THEN
          
    !     update k-1,k and interpolate k+1,k+2
          
          DO iijj=1,mxmy
            pkm(iijj) = pkp(iijj)
            pk(iijj) = pkpp(iijj)
          END DO
          CALL cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,isubx,jsuby)
          CALL cubt2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,isubx,jsuby)
        ELSE IF (k == ksave+3) THEN
          
    !     update k-1 and interpolate k,k+1,k+2
          
          DO iijj=1,mxmy
            pkm(iijj) = pkpp(iijj)
          END DO
          CALL cubt2u(nx,ny,p(1,1,k),mx,my,pk,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,isubx,jsuby)
          CALL cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,isubx,jsuby)
          CALL cubt2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,isubx,jsuby)
        ELSE
          
    !     interpolate all four k-1,k,k+1,k+2
          
          CALL cubt2u(nx,ny,p(1,1,k-1),mx,my,pkm,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,isubx,jsuby)
          CALL cubt2u(nx,ny,p(1,1,k),mx,my,pk,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,isubx,jsuby)
          CALL cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,isubx,jsuby)
          CALL cubt2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,isubx,jsuby)
        END IF
        
    !     save k pointer for next pass
        
        ksave = k
        
    !     cubically interpolate q(ii,jj,kk) from pkm,pk,pkp,pkpp in z direction
        
        DO iijj=1,mxmy
          q(iijj,kk) = dzm(kk)*pkm(iijj) + dz(kk)*pk(iijj) +  &
              dzp(kk)*pkp(iijj) + dzpp(kk)*pkpp(iijj)
        END DO
      END DO
      RETURN
    END IF

    END SUBROUTINE cubt3u
end module