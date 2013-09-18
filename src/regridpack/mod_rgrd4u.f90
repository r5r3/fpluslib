module mod_rgrd4u
    use mod_rgrd3u
    implicit none
    
contains
    !

    ! Code converted using TO_F90 by Alan Miller
    ! Date: 2013-09-18  Time: 12:23:37

    ! ... file rgrd4u.f

    !     this file contains documentation for subroutine rgrd4u followed by
    !     fortran code for rgrid4u and additional subroutines.

    ! ... author

    !     John C. Adams (NCAR 1999)

    ! ... subroutine rgrd4u(nx,ny,nz,nt,p,mx,my,mz,mt,q,intpol,w,lw,iw,liw,ier)

    ! ... purpose

    !     subroutine rgrd4u interpolates the nx by ny by nz by nt array p onto
    !     the mx by my by mz by mt array q.  it is assumed that p and q are
    !     values on uniform nx by ny by nz by nt and mx by my by mz by mt grids
    !     which are superimposed on the same box region (INCLUDING BOUNDARIES).
    !     if p and q are values on nonuniform orthogonal grids and/or if the grid
    !     on which q is defined lies within the p grid then subroutine rgrd4
    !     (see file rgrd4.f) should be used.

    ! ... language

    !     coded in portable FORTRAN77 and FORTRAN90

    ! ... test program

    !     file trgrd4u.f on regridpack includes a test program for subroutine rgrd4u

    ! ... method

    !     linear or cubic interpolation (see intpol) is used in each
    !     direction for which the q grid is not a subgrid of the p grid.
    !     [the mx (my,mz,mt) uniform grid is a subgrid of the nx (ny,nz,nt)
    !     uniform grid if and only if mx-1 (my-1,nz-1,nt-1) divides nx-1
    !     (ny-1,nz-1,nt-1)].  Values are set directly without (the need for)
    !     interpolation in subgrid directions.

    ! ... required files

    !     files rgrd3u.f,rgrd2u.f and rgrd1u.f must be loaded with rgrd4u.f.  they
    !     include subroutines called by the routines in rgrd4u.f

    ! ... efficiency

    !     inner most loops in regridpack software vectorize.  If the
    !     integer arguments mx,my,mz,mt (see below) have different values,
    !     optimal vectorization will be achieved if they are arranged so that
    !     mx > my > mz > mt.

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

    ! ... nt

    !     the integer fourth dimension of p.  nt > 1 if intpol(4) = 1 or
    !     nt > 3 if intpol(4) = 3 is required (see ier=2)

    ! ... p

    !     a real (kind=8) nx by ny by nz by nt array of given values

    ! ... mx

    !     the integer first dimension of q.  mx > 1 is required (see ier = 1)

    ! ... my

    !     the integer second dimension of q. my > 1 is required (see ier = 1)

    ! ... mz

    !     the integer third dimension of q. mz > 1 is required (see ier = 1)

    ! ... mt

    !     the integer fourth dimension of q. mt > 1 is required (see ier = 1)

    ! ... intpol

    !     an integer vector of dimension 4 which sets linear or cubic
    !     interpolation in each of the x,y,z,t directions as follows:

    !        intpol(1) = 1 sets linear interpolation in the x direction
    !        intpol(1) = 3 sets cubic interpolation in the x direction.

    !        intpol(2) = 1 sets linear interpolation in the y direction
    !        intpol(2) = 3 sets cubic interpolation in the y direction.

    !        intpol(3) = 1 sets linear interpolation in the z direction
    !        intpol(3) = 3 sets cubic interpolation in the z direction.

    !        intpol(4) = 1 sets linear interpolation in the t direction
    !        intpol(4) = 3 sets cubic interpolation in the t direction.

    !     values other than 1 or 3 in intpol are not allowed (ier = 3).

    ! ... w

    !     a real (kind=8) work space of length at least lw which must be provided in the
    !     routine calling rgrd4u

    ! ... lw

    !     the integer length of the work space w.

    !          let lwx = 1 if mx-1 divides nx-1; otherwise
    !          let lwx = mx if intpol(1) = 1 or
    !          let lwx = 4*mx if intpol(1) = 3

    !          let lwy = 0 if my-1 divides ny-1; otherwise
    !          let lwy = my+2*mx if intpol(2) = 1 or
    !          let lwy = 4*(mx+my) if intpol(2) = 3

    !          let lwz = 0 if mz-1 divides nz-1; otherwise
    !          let lwz = 2*mx*my+mz if intpol(3) = 1 or
    !          let lwz = 4*(mx*my+mz) if intpol(3) = 3

    !          let lwt = 0 if mt-1 divides nt-1; otherwise
    !          let lwt = 2*mx*my*mz+mt if intpol(4) = 1 or
    !          let lwt = 4*(mx*my*mz+mt) if intpol(4) = 3

    !     then lw must be greater than or equal to lwx+lwy+lwz+lwt

    ! ... iw

    !     an integer work space of length at least liw which must be provided in the
    !     routine calling rgrd4u

    ! ... liw

    !     the integer length of the integer work space iw.  liw must be at least
    !     mx+my+mz+mt


    ! *** output arguments ***


    ! ... q

    !     a real (kind=8) mx by my by mz by mt array of values which are interpolated
    !     from p.

    ! ... ier

    !     an integer error flag set as follows:

    !     ier = 0 if no errors in input arguments are detected

    !     ier = 1 if  min0(mx,my,mz,mt) < 2

    !     ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
    !                ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3 (or)
    !                nz < 2 when intpol(3)=1 or nz < 4 when intpol(3)=3 (or)
    !                nt < 2 when intpol(4)=1 or nt < 4 when intpol(4)=3.

    !     ier = 3 if any of intpol(1),intpol(2),intpol(3),intpol(4)  is not
    !             equal to 1 or 3.

    !     ier = 4 if lw or liw is too small (insufficient work space)

    ! ************************************************************************

    !     end of rgrd4u documentation, fortran code follows:

    ! ************************************************************************

    SUBROUTINE rgrd4u(nx,ny,nz,nt,p,mx,my,mz,mt,q,intpol, w,lw,iw,liw,ier)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: ny
    INTEGER, INTENT(IN)                      :: nz
    INTEGER, INTENT(IN)                      :: nt
    real (kind=8), INTENT(IN)                :: p(nx,ny,nz,nt)
    INTEGER, INTENT(IN)                      :: mx
    INTEGER, INTENT(IN)                      :: my
    INTEGER, INTENT(IN)                      :: mz
    INTEGER, INTENT(IN)                      :: mt
    real (kind=8), INTENT(OUT)               :: q(mx,my,mz,mt)
    INTEGER, INTENT(IN)                      :: intpol(4)
    real (kind=8), INTENT(INOUT)             :: w(lw)
    INTEGER, INTENT(IN)                      :: lw
    INTEGER, INTENT(IN OUT)                  :: iw(liw)
    INTEGER, INTENT(IN)                      :: liw
    INTEGER, INTENT(OUT)                     :: ier


    INTEGER :: inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt
    INTEGER :: mxmy,mxmymz,lwx,lwy,lwz,lwt,jy,kz,LT
    INTEGER :: i2,i3,i4,i5
    INTEGER :: j2,j3,j4,j5,j6,j7,j8,j9
    INTEGER :: k2,k3,k4,k5,k6,k7,k8,k9
    INTEGER :: l2,l3,l4,l5,l6,l7,l8,l9


    !     check input arguments

    ier = 1

    !     check mx,my,mz,mt

    IF (MIN0(mx,my,mz,mt) < 1) RETURN

    !     check intpol

    ier = 3
    IF (intpol(1) /= 1 .AND. intpol(1) /= 3) RETURN
    IF (intpol(2) /= 1 .AND. intpol(2) /= 3) RETURN
    IF (intpol(3) /= 1 .AND. intpol(3) /= 3) RETURN
    IF (intpol(4) /= 1 .AND. intpol(4) /= 3) RETURN

    !     check nx,ny,nz,nt

    ier = 2
    IF (intpol(1) == 1 .AND. nx < 2) RETURN
    IF (intpol(1) == 3 .AND. nx < 4) RETURN
    IF (intpol(2) == 1 .AND. ny < 2) RETURN
    IF (intpol(2) == 3 .AND. ny < 4) RETURN
    IF (intpol(3) == 1 .AND. nz < 2) RETURN
    IF (intpol(3) == 3 .AND. nz < 4) RETURN
    IF (intpol(4) == 1 .AND. nt < 2) RETURN
    IF (intpol(4) == 3 .AND. nt < 4) RETURN

    !     set subgrid indicators

    inmx = (nx-1)/(mx-1)
    jnmy = (ny-1)/(my-1)
    knmz = (nz-1)/(mz-1)
    lnmt = (nt-1)/(mt-1)
    isubx = nx - inmx*(mx-1)
    jsuby = ny - jnmy*(my-1)
    ksubz = nz - knmz*(mz-1)
    lsubt = nt - lnmt*(mt-1)

    !     check work space length input

    ier = 4
    mxmy = mx*my
    mxmymz = mxmy*mz
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
        lwy = 4*my+4*mx
      END IF
    END IF
    lwz = 0
    IF (ksubz /= 1) THEN
      IF (intpol(3) == 1) THEN
        lwz = (2*mxmy+mz)
      ELSE
        lwz = 4*mxmy+4*mz
      END IF
    END IF
    lwt = 0
    IF (lsubt /= 1) THEN
      IF (intpol(4) == 1) THEN
        lwt = (2*mxmymz+mt)
      ELSE
        lwt = 4*mxmymz+4*mt
      END IF
    END IF

    IF (lw < lwx+lwy+lwz+lwt) RETURN
    IF (liw < mx+my+mz+mt) RETURN

    !     arguments o.k.

    ier = 0
    jy = mx+1
    kz = mx+my+1
    LT = mx+my+mz+1

    IF (intpol(4) == 1) THEN
      
    !     linearly interpolate in nt, set work space pointers and scales
      
      
      l2 = 1
      l3 = l2
      l4 = l3+mt
      l5 = l4
      l6 = l5
      l7 = l6
      l8 = l7+mxmymz
      l9 = l8+mxmymz
      CALL linmxu(nt,mt,iw(LT),w(l3))
      k2 = l9
      IF (intpol(3) == 1) THEN
    !     linear in z
        k3 = k2
        k4 = k3+mz
        k5 = k4
        k6 = k5
        k7 = k6
        k8 = k7+mxmy
        k9 = k8+mxmy
        CALL linmxu(nz,mz,iw(kz),w(k3))
        j2 = k9
      ELSE
    !     cubic in z
        k3 = k2+mz
        k4 = k3+mz
        k5 = k4+mz
        k6 = k5+mz
        k7 = k6+mxmy
        k8 = k7+mxmy
        k9 = k8+mxmy
        CALL cubnmxu(nz,mz,iw(kz),w(k2),w(k3),w(k4),w(k5))
        j2 = k9+mxmy
      END IF
      
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
      
    !     linearly interpolate in t
      
      CALL lint4u(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,  &
          iw(LT),w(l3),w(l7),w(l8),  &
          iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),  &
          iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),  &
          iw,w(i2),w(i3),w(i4),w(i5), inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt)
      RETURN
      
    ELSE
      
    !     cubically interpolate in t
      
      l2 = 1
      l3 = l2+mt
      l4 = l3+mt
      l5 = l4+mt
      l6 = l5+mt
      l7 = l6+mxmymz
      l8 = l7+mxmymz
      l9 = l8+mxmymz
      CALL cubnmxu(nt,mt,iw(LT),w(l2),w(l3),w(l4),w(l5))
      k2 = l9+mxmymz
      
      IF (intpol(3) == 1) THEN
    !     linear in z
        k3 = k2
        k4 = k3+mz
        k5 = k4
        k6 = k5
        k7 = k6
        k8 = k7+mxmy
        k9 = k8+mxmy
        CALL linmxu(nz,mz,iw(kz),w(k3))
        j2 = k9
      ELSE
    !     cubic in z
        k3 = k2+mz
        k4 = k3+mz
        k5 = k4+mz
        k6 = k5+mz
        k7 = k6+mxmy
        k8 = k7+mxmy
        k9 = k8+mxmy
        CALL cubnmxu(nz,mz,iw(kz),w(k2),w(k3),w(k4),w(k5))
        j2 = k9+mxmy
      END IF
      
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
      
    !     set work space portion and indices which depend on x interpolation
      
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
      
    !     cubically interpolate in t
      
      CALL cubt4u(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,  &
          iw(LT),w(l2),w(l3),w(l4),w(l5),w(l6),w(l7),w(l8),w(l9),  &
          iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),  &
          iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),  &
          iw,w(i2),w(i3),w(i4),w(i5), inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt)
      RETURN
    END IF
    END SUBROUTINE rgrd4u

    SUBROUTINE lint4u(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,  &
        LT,dt,pt,ptp,kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,  &
        jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
        inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt)

    !     linearly interpolate in t direction


    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: ny
    INTEGER, INTENT(IN)                      :: nz
    INTEGER, INTENT(IN)                      :: nt
    real (kind=8), INTENT(IN)                :: p(nx,ny,nz,nt)
    INTEGER, INTENT(IN)                      :: mx
    INTEGER, INTENT(IN)                      :: my
    INTEGER, INTENT(IN)                      :: mz
    INTEGER, INTENT(IN)                      :: mt
    INTEGER, INTENT(IN OUT)                  :: mxmy
    INTEGER, INTENT(IN)                      :: mxmymz
    real (kind=8), INTENT(OUT)               :: q(mxmymz,mt)
    INTEGER, INTENT(IN)                      :: intpol(4)
    INTEGER, INTENT(IN)                      :: LT(mt)
    real (kind=8), INTENT(IN)                :: dt(mt)
    real (kind=8), INTENT(OUT)               :: pt(mxmymz)
    real (kind=8), INTENT(IN OUT)            :: ptp(mxmymz)
    INTEGER, INTENT(IN OUT)                  :: kz(mz)
    real (kind=8), INTENT(IN OUT)            :: dzm(mz)
    real (kind=8), INTENT(IN OUT)            :: dz(mz)
    real (kind=8), INTENT(IN OUT)            :: dzp(mz)
    real (kind=8), INTENT(IN OUT)            :: dzpp(mz)
    real (kind=8), INTENT(IN OUT)            :: pkm(mxmy)
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
    INTEGER, INTENT(IN OUT)                  :: knmz
    INTEGER, INTENT(IN)                      :: lnmt
    INTEGER, INTENT(IN OUT)                  :: isubx
    INTEGER, INTENT(IN OUT)                  :: jsuby
    INTEGER, INTENT(IN OUT)                  :: ksubz
    INTEGER, INTENT(IN)                      :: lsubt











    INTEGER :: l,ll,lsave,iijjkk

    IF (intpol(3) == 1) THEN
      
    !     linear in z
      
      IF (lsubt == 1) THEN
        
    !     mt grid is subset of nt grid
        
        DO ll=1,mt
          l = lnmt*(ll-1)+1
          CALL lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,q(1,ll),intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
        END DO
        RETURN
      END IF
      
      lsave = -1
      DO ll=1,mt
        l = LT(ll)
        IF (l == lsave) THEN
          
    !     l pointer has not moved since last pass (no updates or interpolation)
          
        ELSE IF (l == lsave+1) THEN
          
    !     update l and interpolate l+1
          
          DO iijjkk=1,mxmymz
            pt(iijjkk) = ptp(iijjkk)
          END DO
          CALL lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp  &
              ,inmx,jnmy,knmz,isubx,jsuby,ksubz)
        ELSE
          
    !     interpolate l,l+1 in pt,ptp on xx,yy,zz mesh
          
          CALL lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dz,  &
              pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
          CALL lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp  &
              ,inmx,jnmy,knmz,isubx,jsuby,ksubz)
        END IF
        
    !     save l pointer for next pass
        
        lsave = l
        
    !     linearly interpolate q(ii,jj,,kk,ll) from pt,ptp in t direction
        
        DO iijjkk=1,mxmymz
          q(iijjkk,ll) = pt(iijjkk)+dt(ll)*(ptp(iijjkk)-pt(iijjkk))
        END DO
      END DO
      RETURN
      
    ELSE
      
    !     cubic in z
      
      IF (lsubt == 1) THEN
        
    !     mt grid is subset of nt grid
        
        DO ll=1,mt
          l = lnmt*(ll-1)+1
          CALL cubt3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,q(1,ll),intpol,  &
              kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,  &
              pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp, inmx,jnmy,knmz,isubx,jsuby,ksubz)
        END DO
        RETURN
      END IF
      lsave = -1
      DO ll=1,mt
        l = LT(ll)
        IF (l == lsave) THEN
          
    !     l pointer has not moved since last pass (no updates or interpolation)
          
        ELSE IF (l == lsave+1) THEN
          
    !     update l and interpolate l+1
          
          DO iijjkk=1,mxmymz
            pt(iijjkk) = ptp(iijjkk)
          END DO
          CALL cubt3u(nx,ny,nt,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,  &
              kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
        ELSE
          
    !     interpolate l,l+1 in pt,ptp on xx,yy,zz mesh
          
          CALL cubt3u(nx,ny,nt,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,  &
              kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
          CALL cubt3u(nx,ny,nt,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,  &
              kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
        END IF
        
    !     save l pointer for next pass
        
        lsave = l
        
    !     linearly interpolate q(ii,jj,kk,ll) from pt,ptp in t direction
        
        DO iijjkk=1,mxmymz
          q(iijjkk,ll) = pt(iijjkk)+dt(ll)*(ptp(iijjkk)-pt(iijjkk))
        END DO
        
      END DO
      RETURN
      
    END IF
    END SUBROUTINE lint4u

    SUBROUTINE cubt4u(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,  &
        LT,dtm,dt,dtp,dtpp,ptm,pt,ptp,ptpp, kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,  &
        jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp, ix,dxm,dx,dxp,dxpp,  &
        inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt)


    !     cubically interpolate in t

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: ny
    INTEGER, INTENT(IN)                      :: nz
    INTEGER, INTENT(IN)                      :: nt
    real (kind=8), INTENT(IN)                :: p(nx,ny,nz,nt)
    INTEGER, INTENT(IN)                      :: mx
    INTEGER, INTENT(IN)                      :: my
    INTEGER, INTENT(IN)                      :: mz
    INTEGER, INTENT(IN)                      :: mt
    INTEGER, INTENT(IN OUT)                  :: mxmy
    INTEGER, INTENT(IN)                      :: mxmymz
    real (kind=8), INTENT(OUT)                        :: q(mxmymz,mt)
    INTEGER, INTENT(IN)                      :: intpol(4)
    INTEGER, INTENT(IN)                      :: LT(mt)
    real (kind=8), INTENT(IN)                :: dtm(mt)
    real (kind=8), INTENT(IN)                :: dt(mt)
    real (kind=8), INTENT(IN OUT)            :: dtp(mt)
    real (kind=8), INTENT(IN OUT)            :: dtpp(mt)
    real (kind=8), INTENT(OUT)               :: ptm(mxmymz)
    real (kind=8), INTENT(IN OUT)            :: pt(mxmymz)
    real (kind=8), INTENT(IN OUT)            :: ptp(mxmymz)
    real (kind=8), INTENT(IN OUT)            :: ptpp(mxmymz)
    INTEGER, INTENT(IN OUT)                  :: kz(mz)
    real (kind=8), INTENT(IN OUT)            :: dzm(mz)
    real (kind=8), INTENT(IN OUT)            :: dz(mz)
    real (kind=8), INTENT(IN OUT)            :: dzp(mz)
    real (kind=8), INTENT(IN OUT)            :: dzpp(mz)
    real (kind=8), INTENT(IN OUT)            :: pkm(mxmy)
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
    INTEGER, INTENT(IN OUT)                  :: knmz
    INTEGER, INTENT(IN)                      :: lnmt
    INTEGER, INTENT(IN OUT)                  :: isubx
    INTEGER, INTENT(IN OUT)                  :: jsuby
    INTEGER, INTENT(IN OUT)                  :: ksubz
    INTEGER, INTENT(IN)                      :: lsubt












    INTEGER :: l,ll,iijjkk,lsave

    IF (intpol(3) == 1) THEN
      
    !     linear in z
      
      IF (lsubt == 1) THEN
        
    !     mt grid is subset of nt grid
        
        DO ll=1,mt
          l = lnmt*(ll-1)+1
          CALL lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,q(1,ll),intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
        END DO
        RETURN
      END IF
      lsave = -3
      DO ll=1,mt
        l = LT(ll)
        IF (l == lsave) THEN
          
    !     l pointer has not moved since last pass (no updates or interpolation)
          
        ELSE IF (l == lsave+1) THEN
          
    !     update l-1,l,l+1 and interpolate l+2
          
          DO iijjkk=1,mxmymz
            ptm(iijjkk) = pt(iijjkk)
            pt(iijjkk) = ptp(iijjkk)
            ptp(iijjkk) = ptpp(iijjkk)
          END DO
          CALL lint3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
        ELSE IF (l == lsave+2) THEN
          
    !     update l-1,l and interpolate l+1,l+2
          
          DO iijjkk=1,mxmymz
            ptm(iijjkk) = ptp(iijjkk)
            pt(iijjkk) = ptpp(iijjkk)
          END DO
          CALL lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
          CALL lint3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
        ELSE IF (l == lsave+3) THEN
          
    !     update l-1 and interpolate l,l+1,l+2
          
          DO iijjkk=1,mxmymz
            ptm(iijjkk) = ptpp(iijjkk)
          END DO
          CALL lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dz,  &
              pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
          CALL lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
          CALL lint3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
        ELSE
          
    !     interpolate all four l-1,l,l+1,l+2
          
          CALL lint3u(nx,ny,nz,p(1,1,1,l-1),mx,my,mxmy,mz,ptm,intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
          CALL lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
          CALL lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
          CALL lint3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,  &
              inmx,jnmy,knmz,isubx,jsuby,ksubz)
        END IF
        
    !     save l pointer for next pass
        
        lsave = l
        
    !     cubically interpolate q(ii,jj,kk,ll) from ptm,pt,ptp,ptpp in t direction
        
        DO iijjkk=1,mxmymz
          q(iijjkk,ll) = dtm(ll)*ptm(iijjkk) + dt(ll)*pt(iijjkk) +  &
              dtp(ll)*ptp(iijjkk) + dtpp(ll)*ptpp(iijjkk)
        END DO
      END DO
      RETURN
      
    ELSE
      
    !     cubic in z
      
      IF (lsubt == 1) THEN
        
    !     mt grid is subset of nt grid
        
        DO ll=1,mt
          l = lnmt*(ll-1)+1
          CALL cubt3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,q(1,ll),intpol,  &
              kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,  &
              pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp, inmx,jnmy,knmz,isubx,jsuby,ksubz)
        END DO
        RETURN
      END IF
      lsave = -3
      DO ll=1,mt
        l = LT(ll)
        IF (l == lsave) THEN
          
    !     l pointer has not moved since last pass (no updates or interpolation)
          
        ELSE IF (l == lsave+1) THEN
          
    !     update l-1,l,l+1 and interpolate l+2
          
          DO iijjkk=1,mxmymz
            ptm(iijjkk) = pt(iijjkk)
            pt(iijjkk) = ptp(iijjkk)
            ptp(iijjkk) = ptpp(iijjkk)
          END DO
          CALL cubt3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,  &
              dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp  &
              ,ix,dxm,dx,dxp,dxpp, inmx,jnmy,knmz,isubx,jsuby,ksubz)
        ELSE IF (l == lsave+2) THEN
          
    !     update l-1,l and interpolate l+1,l+2
          
          DO iijjkk=1,mxmymz
            ptm(iijjkk) = ptp(iijjkk)
            pt(iijjkk) = ptpp(iijjkk)
          END DO
          CALL cubt3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,  &
              dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp  &
              ,ix,dxm,dx,dxp,dxpp, inmx,jnmy,knmz,isubx,jsuby,ksubz)
          CALL cubt3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,  &
              dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp  &
              ,ix,dxm,dx,dxp,dxpp, inmx,jnmy,knmz,isubx,jsuby,ksubz)
        ELSE IF (l == lsave+3) THEN
          
    !     update l-1 and interpolate l,l+1,l+2
          
          DO iijjkk=1,mxmymz
            ptm(iijjkk) = ptpp(iijjkk)
          END DO
          CALL cubt3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,  &
              dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp  &
              ,ix,dxm,dx,dxp,dxpp, inmx,jnmy,knmz,isubx,jsuby,ksubz)
          CALL cubt3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,  &
              dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp  &
              ,ix,dxm,dx,dxp,dxpp, inmx,jnmy,knmz,isubx,jsuby,ksubz)
          CALL cubt3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,  &
              dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp  &
              ,ix,dxm,dx,dxp,dxpp, inmx,jnmy,knmz,isubx,jsuby,ksubz)
        ELSE
          
    !     interpolate all four l-1,l,l+1,l+2
          
          CALL cubt3u(nx,ny,nz,p(1,1,1,l-1),mx,my,mxmy,mz,ptm,intpol,kz,  &
              dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp  &
              ,ix,dxm,dx,dxp,dxpp, inmx,jnmy,knmz,isubx,jsuby,ksubz)
          CALL cubt3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,  &
              dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp  &
              ,ix,dxm,dx,dxp,dxpp, inmx,jnmy,knmz,isubx,jsuby,ksubz)
          CALL cubt3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,  &
              dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp  &
              ,ix,dxm,dx,dxp,dxpp, inmx,jnmy,knmz,isubx,jsuby,ksubz)
          CALL cubt3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,  &
              dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp  &
              ,ix,dxm,dx,dxp,dxpp, inmx,jnmy,knmz,isubx,jsuby,ksubz)
        END IF
        
    !     save l pointer for next pass
        
        lsave = l
        
    !     cubically interpolate q(ii,jj,kk,ll) from ptm,pt,ptp,ptpp in t direction
        
        DO iijjkk=1,mxmymz
          q(iijjkk,ll) = dtm(ll)*ptm(iijjkk) + dt(ll)*pt(iijjkk) +  &
              dtp(ll)*ptp(iijjkk) + dtpp(ll)*ptpp(iijjkk)
        END DO
      END DO
      RETURN
    END IF

    END SUBROUTINE cubt4u
end module