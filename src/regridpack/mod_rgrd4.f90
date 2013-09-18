module mod_rgrd4
    use mod_rgrd3
    implicit none
    
contains
    !

    ! Code converted using TO_F90 by Alan Miller
    ! Date: 2013-09-18  Time: 12:23:33

    ! ... file rgrd4.f

    !     this file contains documentation for subroutine rgrd4 followed by
    !     fortran code for rgrd4 and additional subroutines.

    ! ... author

    !     John C. Adams (NCAR 1999)

    ! ... subroutine rgrd4(nx,ny,nz,nt,x,y,z,t,p,mx,my,mz,mt,xx,yy,zz,tt,q,
    !    +                 intpol,w,lw,iw,liw,ier)

    ! ... purpose

    !     subroutine rgrd4 interpolates the values p(i,j,k,l) on the orthogonal
    !     grid

    !       (x(i),y(j),z(k),t(l))

    !       for i=1,...,nx;j=1,...,ny;k=1,...,nz;l=1,...,nt


    !     onto q(ii,jj,kk,ll) on the orthogonal grid


    !       (xx(ii),yy(jj),zz(kk),tt(ll))

    !       for ii=1,...,mx;jj=1,...,my;kk=1,...,mz;ll=1,...,mt

    ! ... language

    !     coded in portable FORTRAN77 and FORTRAN90

    ! ... test program

    !     file trgrd4.f on regridpack includes a test program for subroutine rgrd4

    ! ... method

    !     linear or cubic interpolation is used (independently) in
    !     each direction (see argument intpol).

    ! ... required files

    !     files rgrd3.f,rgrd2.f and rgrd1.f must be loaded with rgrd4.f.  they
    !     include subroutines called by the routines in rgrd4.f

    ! ... requirements

    !     each of the x,y,z,t grids must be strictly montonically increasing
    !     and each of the xx,yy,zz,tt grids must be montonically increasing
    !     (see ier = 4).  in addition the (X,Y,Z,T) region of the q grid

    !      [xx(1),xx(mx)] X [yy(1),yy(my)] X [zz(1),zz(mz)] X [tt(1),tt(my)]

    !     must lie within the (X,Y,Z,T) region of the p grid

    !      [x(1),x(nx)] X [y(1),y(ny)] X [z(1),z(nz)] X [t(1),t(nt)].

    !     extrapolation is not allowed (see ier=3).  if these (X,Y,Z,T)
    !     regions are identical and the orthogonal grids are UNIFORM
    !     in each direction then subroutine rgrd4u (see file rgrd4u.f)
    !     should be used instead of rgrd4.

    ! ... efficiency

    !     inner most loops in regridpack software vectorize. If
    !     the integer arguments mx,my,mz,mt (see below) have different values,
    !     optimal vectorization will be achieved if they are arranged so that
    !     mx > my > mz > mt.

    ! *** input arguments

    ! ... nx

    !     the integer dimension of the grid vector x and the first dimension of p.
    !     nx > 1 if intpol(1) = 1 or nx > 3 if intpol(1) = 3 is required.

    ! ... ny

    !     the integer dimension of the grid vector y and the second dimension of p.
    !     ny > 1 if intpol(2) = 1 or ny > 3 if intpol(2) = 3 is required.

    ! ... nz

    !     the integer dimension of the grid vector z and the third dimension of p.
    !     nz > 1 if intpol(3) = 1 or nz > 3 if intpol(3) = 3 is required.

    ! ... nt

    !     the integer dimension of the grid vector t and the fourth dimension of p.
    !     nt > 1 if intpol(4) = 1 or nt > 3 if intpol(4) = 3 is required.

    ! ... x

    !     a real (kind=8) nx vector of strictly increasing values which defines the x
    !     portion of the orthogonal grid on which p is given

    ! ... y

    !     a real (kind=8) ny vector of strictly increasing values which defines the y
    !     portion of the orthogonal grid on which p is given

    ! ... z

    !     a real (kind=8) nz vector of strictly increasing values which defines the z
    !     portion of the orthogonal grid on which p is given

    ! ... t

    !     a real (kind=8) nt vector of strictly increasing values which defines the t
    !     portion of the orthogonal grid on which p is given

    ! ... p

    !     a real (kind=8) nx by ny by nz by nt array of values given on the (x,y,z,t) grid

    ! ... mx

    !     the integer dimension of the grid vector xx and the first dimension
    !     of q.  mx > 0 is required.

    ! ... my

    !     the integer dimension of the grid vector yy and the second dimension
    !     of q.  my > 0 is required.

    ! ... mz

    !     the integer dimension of the grid vector zz and the third dimension
    !     of q.  mz > 0 is required.

    ! ... mt

    !     the integer dimension of the grid vector tt and the fourth dimension
    !     of q.  mt > 0 is required.

    ! ... xx

    !     a real (kind=8) mx vector of increasing values which defines the x portion of the
    !     orthogonal grid on which q is defined.  xx(1) < x(1) or xx(mx) > x(nx)
    !     is not allowed (see ier = 3)

    ! ... yy

    !     a real (kind=8) my vector of increasing values which defines the y portion of the
    !     orthogonal grid on which q is defined.  yy(1) < y(1) or yy(my) > y(ny)
    !     is not allowed (see ier = 3)

    ! ... zz

    !     a real (kind=8) mz vector of increasing values which defines the z portion of the
    !     orthogonal grid on which q is defined.  zz(1) < z(1) or zz(mz) > z(nz)
    !     is not allowed (see ier = 3)

    ! ... tt

    !     a real (kind=8) mt vector of increasing values which defines the t portion of the
    !     orthogonal grid on which q is defined.  tt(1) < t(1) or tt(mt) > t(nt)
    !     is not allowed (see ier = 3)

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

    !     values other than 1 or 3 in intpol are not allowed (ier = 6).

    ! ... w

    !     a real (kind=8) work space of length at least lw which must be provided in the
    !     routine calling rgrd4

    ! ... lw

    !     the integer length of the real (kind=8) work space w.  let

    !          lwx =   mx            if intpol(1) = 1
    !          lwx = 4*mx            if intpol(1) = 3

    !          lwy = my+2*mx         if intpol(2) = 1
    !          lwy = 4*(my+mx)       if intpol(2) = 3

    !          lwz = 2*mx*my+mz      if intpol(3) = 1
    !          lwz = 4*(mx*my+mz)    if intpol(3) = 3

    !          lwt = 2*mx*my*mz+mt   if intpol(4) = 1
    !          lwt = 4*(mx*my*mz+mt) if intpol(4) = 3

    !     then lw must be greater than or equal to lwx+lwy+lwz+lwt

    ! ... iw

    !     an integer work space of length at least liw which must be provided in the
    !     routine calling rgrd4

    ! ... liw

    !     the integer length of the integer work space iw.  liw must be at least mx+my+mz+mt


    ! *** output arguments


    ! ... q

    !     a real (kind=8) mx by my by mz by mt array of values on the (xx,yy,zz,tt) grid
    !     which are interpolated from p on the (x,y,z,t) grid

    ! ... ier

    !     an integer error flag set as follows:

    !     ier = 0 if no errors in input arguments are detected

    !     ier = 1 if  min0(mx,my,mz,mt) < 1

    !     ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
    !                ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3 (or)
    !                nz < 2 when intpol(3)=1 or nz < 4 when intpol(3)=3 (or)
    !                nt < 2 when intpol(4)=1 or nt < 4 when intpol(4)=3

    !     ier = 3 if xx(1) < x(1) or x(nx) < xx(mx) (or)
    !                yy(1) < y(1) or y(ny) < yy(my) (or)
    !                zz(1) < z(1) or z(nz) < zz(mz) (or)
    !                tt(1) < t(1) or t(nt) < tt(mt)

    ! *** to avoid this flag when end points are intended to be the
    !     same but may differ slightly due to roundoff error, they
    !     should be set exactly in the calling routine (e.g., if both
    !     grids have the same y boundaries then yy(1)=y(1) and yy(my)=y(ny)
    !     should be set before calling rgrd4)

    !     ier = 4 if one of the grids x,y,z,t is not strictly monotonically
    !             increasing or if one of the grids xx,yy,zz,tt is not
    !             montonically increasing.  more precisely if:

    !             x(i+1) <= x(i) for some i such that 1 <= i < nx (or)

    !             y(j+1) <= y(j) for some j such that 1 <= j < ny (or)

    !             z(k+1) <= z(k) for some k such that 1 <= k < nz (or)

    !             t(l+1) <= t(l) for some l such that 1 <= l < nt (or)

    !             xx(ii+1) < xx(ii) for some ii such that 1 <= ii < mx (or)

    !             yy(jj+1) < yy(jj) for some jj such that 1 <= jj < my (or)

    !             zz(kk+1) < zz(k)  for some kk such that 1 <= kk < mz (or)

    !             tt(ll+1) < tt(l)  for some ll such that 1 <= ll < mt

    !     ier = 5 if lw or liw is too small (insufficient work space)

    !     ier = 6 if any of intpol(1),intpol(2),intpol(3),intpol(4)
    !             is not equal to 1 or 3

    ! ************************************************************************

    !     end of rgrd4 documentation, fortran code follows:

    ! ************************************************************************

    SUBROUTINE rgrd4(nx,ny,nz,nt,x,y,z,t,p,mx,my,mz,mt,xx,yy,zz,  &
        tt,q,intpol,w,lw,iw,liw,ier)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: ny
    INTEGER, INTENT(IN)                      :: nz
    INTEGER, INTENT(IN)                      :: nt
    real (kind=8), INTENT(IN)                :: x(nx)
    real (kind=8), INTENT(IN)                :: y(ny)
    real (kind=8), INTENT(IN)                :: z(nz)
    real (kind=8), INTENT(IN)                :: t(nt)
    real (kind=8), INTENT(IN)                :: p(nx,ny,nz,nt)
    INTEGER, INTENT(IN)                      :: mx
    INTEGER, INTENT(IN)                      :: my
    INTEGER, INTENT(IN)                      :: mz
    INTEGER, INTENT(IN)                      :: mt
    real (kind=8), INTENT(IN)                :: xx(mx)
    real (kind=8), INTENT(IN)                :: yy(my)
    real (kind=8), INTENT(IN)                :: zz(mz)
    real (kind=8), INTENT(IN)                :: tt(mt)
    real (kind=8), INTENT(OUT)               :: q(mx,my,mz,mt)
    INTEGER, INTENT(IN)                      :: intpol(4)
    real (kind=8), INTENT(INOUT)             :: w(lw)
    INTEGER, INTENT(IN)                      :: lw
    INTEGER, INTENT(IN OUT)                  :: iw(liw)
    INTEGER, INTENT(IN)                      :: liw
    INTEGER, INTENT(OUT)                     :: ier




    INTEGER :: l2,l3,l4,l5,l6,l7,l8,l9
    INTEGER :: k2,k3,k4,k5,k6,k7,k8,k9
    INTEGER :: j2,j3,j4,j5,j6,j7,j8,j9
    INTEGER :: i2,i3,i4,i5
    INTEGER :: lwx,lwy,lwz,lwt,mxmy,mxmymz
    INTEGER :: ii,jj,kk,ll,i,j,k,l
    INTEGER :: jy,kz,LT

    !     check input arguments

    ier = 1

    !     check (xx,yy,zz,tt) grid resolution

    IF (MIN0(mx,my,mz,mt) < 1) RETURN

    !     check intpol

    ier = 6
    IF (intpol(1) /= 1 .AND. intpol(1) /= 3) RETURN
    IF (intpol(2) /= 1 .AND. intpol(2) /= 3) RETURN
    IF (intpol(3) /= 1 .AND. intpol(3) /= 3) RETURN
    IF (intpol(4) /= 1 .AND. intpol(4) /= 3) RETURN

    !     check (x,y,z,t) grid resolution

    ier = 2
    IF (intpol(1) == 1 .AND. nx < 2) RETURN
    IF (intpol(1) == 3 .AND. nx < 4) RETURN
    IF (intpol(2) == 1 .AND. ny < 2) RETURN
    IF (intpol(2) == 3 .AND. ny < 4) RETURN
    IF (intpol(3) == 1 .AND. nz < 2) RETURN
    IF (intpol(3) == 3 .AND. nz < 4) RETURN
    IF (intpol(4) == 1 .AND. nt < 2) RETURN
    IF (intpol(4) == 3 .AND. nt < 4) RETURN

    !     check work space length input and set minimum

    ier = 5
    mxmy = mx*my
    mxmymz = mxmy*mz
    IF (intpol(1) == 1) THEN
      lwx = mx
    ELSE
      lwx = 4*mx
    END IF
    IF (intpol(2) == 1) THEN
      lwy = (my+2*mx)
    ELSE
      lwy = 4*(mx+my)
    END IF
    IF (intpol(3) == 1) THEN
      lwz = (2*mxmy+mz)
    ELSE
      lwz = 4*(mxmy+mz)
    END IF
    IF (intpol(4) == 1) THEN
      lwt = (2*mxmymz+mt)
    ELSE
      lwt = 4*(mxmymz+mt)
    END IF
    IF (lw < lwx+lwy+lwz+lwt) RETURN
    IF (liw < mx+my+mz+mt) RETURN

    !     check (xx,yy,zz,tt) grid contained in (x,y,z,t) grid

    ier = 3
    IF (xx(1) < x(1) .OR. xx(mx) > x(nx)) RETURN
    IF (yy(1) < y(1) .OR. yy(my) > y(ny)) RETURN
    IF (zz(1) < z(1) .OR. zz(mz) > z(nz)) RETURN
    IF (tt(1) < t(1) .OR. tt(mt) > t(nt)) RETURN

    !     check montonicity of grids

    ier = 4
    DO i=2,nx
      IF (x(i-1) >= x(i)) RETURN
    END DO
    DO j=2,ny
      IF (y(j-1) >= y(j)) RETURN
    END DO
    DO k=2,nz
      IF (z(k-1) >= z(k)) RETURN
    END DO
    DO l=2,nt
      IF (t(l-1) >= t(l)) RETURN
    END DO
    DO ii=2,mx
      IF (xx(ii-1) > xx(ii)) RETURN
    END DO
    DO jj=2,my
      IF (yy(jj-1) > yy(jj)) RETURN
    END DO
    DO kk=2,mz
      IF (zz(kk-1) > zz(kk)) RETURN
    END DO
    DO ll=2,mt
      IF (tt(ll-1) > tt(ll)) RETURN
    END DO

    !     arguments o.k.

    ier = 0

    !     set pointers for integer work space iw

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
      CALL linmx(nt,t,mt,tt,iw(LT),w(l3))
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
        CALL linmx(nz,z,mz,zz,iw(kz),w(k3))
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
        CALL cubnmx(nz,z,mz,zz,iw(kz),w(k2),w(k3),w(k4),w(k5))
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
        CALL linmx(ny,y,my,yy,iw(jy),w(j3))
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
        CALL cubnmx(ny,y,my,yy,iw(jy),w(j2),w(j3),w(j4),w(j5))
        i2 = j9+mx
      END IF
      
      IF (intpol(1) == 1) THEN
    !     linear in x
        i3 = i2
        i4 = i3
        i5 = i4
        CALL linmx(nx,x,mx,xx,iw,w(i3))
      ELSE
    !     cubic in x
        i3 = i2+mx
        i4 = i3+mx
        i5 = i4+mx
        CALL cubnmx(nx,x,mx,xx,iw,w(i2),w(i3),w(i4),w(i5))
      END IF
      
    !     linearly interpolate in t
      
      CALL lint4(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,  &
          iw(LT),w(l3),w(l7),w(l8),  &
          iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),  &
          iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),  &
          iw,w(i2),w(i3),w(i4),w(i5))
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
      CALL cubnmx(nt,t,mt,tt,iw(LT),w(l2),w(l3),w(l4),w(l5))
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
        CALL linmx(nz,z,mz,zz,iw(kz),w(k3))
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
        CALL cubnmx(nz,z,mz,zz,iw(kz),w(k2),w(k3),w(k4),w(k5))
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
        CALL linmx(ny,y,my,yy,iw(jy),w(j3))
        i2 = j9
      ELSE
        j3 = j2+my
        j4 = j3+my
        j5 = j4+my
        j6 = j5+my
        j7 = j6+mx
        j8 = j7+mx
        j9 = j8+mx
        CALL cubnmx(ny,y,my,yy,iw(jy),w(j2),w(j3),w(j4),w(j5))
        i2 = j9+mx
      END IF
      
    !     set work space portion and indices which depend on x interpolation
      
      IF (intpol(1) == 1) THEN
        i3 = i2
        i4 = i3
        i5 = i4
        CALL linmx(nx,x,mx,xx,iw,w(i3))
      ELSE
        i3 = i2+mx
        i4 = i3+mx
        i5 = i4+mx
        CALL cubnmx(nx,x,mx,xx,iw,w(i2),w(i3),w(i4),w(i5))
      END IF
      
    !     cubically interpolate in t
      
      CALL cubt4(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,  &
          iw(LT),w(l2),w(l3),w(l4),w(l5),w(l6),w(l7),w(l8),w(l9),  &
          iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),  &
          iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),  &
          iw,w(i2),w(i3),w(i4),w(i5))
      RETURN
      
    END IF
    END SUBROUTINE rgrd4

    SUBROUTINE lint4(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,  &
        LT,dt,pt,ptp,kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,  &
        jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)

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
    INTEGER :: lsave,ll,l,iijjkk









    IF (intpol(3) == 1) THEN
      
    !     linear in z
      
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
          CALL lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,  &
              kz,dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix, dxm,dx,dxp,dxpp)
        ELSE
          
    !     interpolate l,l+1 in pt,ptp on xx,yy,zz mesh
          
          CALL lint3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,  &
              dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
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
          CALL cubt3(nx,ny,nt,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,  &
              kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
        ELSE
          
    !     interpolate l,l+1 in pt,ptp on xx,yy,zz mesh
          
          CALL cubt3(nx,ny,nt,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,  &
              kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL cubt3(nx,ny,nt,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,  &
              kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
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
    END SUBROUTINE lint4

    SUBROUTINE cubt4(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,  &
        LT,dtm,dt,dtp,dtpp,ptm,pt,ptp,ptpp, kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,  &
        jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp, ix,dxm,dx,dxp,dxpp)

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
    real (kind=8), INTENT(OUT)               :: q(mxmymz,mt)
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
    INTEGER :: lsave,ll,l,iijjkk










    IF (intpol(3) == 1) THEN
      
    !     linear in z
      
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
          CALL lint3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dz,  &
              pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
        ELSE IF (l == lsave+2) THEN
          
    !     update l-1,l and interpolate l+1,l+2
          
          DO iijjkk=1,mxmymz
            ptm(iijjkk) = ptp(iijjkk)
            pt(iijjkk) = ptpp(iijjkk)
          END DO
          CALL lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dz,  &
              pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL lint3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dz,  &
              pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
        ELSE IF (l == lsave+3) THEN
          
    !     update l-1 and interpolate l,l+1,l+2
          
          DO iijjkk=1,mxmymz
            ptm(iijjkk) = ptpp(iijjkk)
          END DO
          CALL lint3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dz,  &
              pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dz,  &
              pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL lint3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dz,  &
              pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
        ELSE
          
    !     interpolate all four l-1,l,l+1,l+2
          
          CALL lint3(nx,ny,nz,p(1,1,1,l-1),mx,my,mxmy,mz,ptm,intpol,kz,dz,  &
              pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL lint3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dz,  &
              pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dz,  &
              pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL lint3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dz,  &
              pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
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
          CALL cubt3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dzm,  &
              dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,  &
              ix,dxm,dx,dxp,dxpp)
        ELSE IF (l == lsave+2) THEN
          
    !     update l-1,l and interpolate l+1,l+2
          
          DO iijjkk=1,mxmymz
            ptm(iijjkk) = ptp(iijjkk)
            pt(iijjkk) = ptpp(iijjkk)
          END DO
          CALL cubt3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dzm,  &
              dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,  &
              ix,dxm,dx,dxp,dxpp)
          CALL cubt3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dzm,  &
              dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,  &
              ix,dxm,dx,dxp,dxpp)
        ELSE IF (l == lsave+3) THEN
          
    !     update l-1 and interpolate l,l+1,l+2
          
          DO iijjkk=1,mxmymz
            ptm(iijjkk) = ptpp(iijjkk)
          END DO
          CALL cubt3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dzm,  &
              dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,  &
              ix,dxm,dx,dxp,dxpp)
          CALL cubt3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dzm,  &
              dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,  &
              ix,dxm,dx,dxp,dxpp)
          CALL cubt3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dzm,  &
              dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,  &
              ix,dxm,dx,dxp,dxpp)
        ELSE
          
    !     interpolate all four l-1,l,l+1,l+2
          
          CALL cubt3(nx,ny,nz,p(1,1,1,l-1),mx,my,mxmy,mz,ptm,intpol,kz,dzm,  &
              dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,  &
              ix,dxm,dx,dxp,dxpp)
          CALL cubt3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dzm,  &
              dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,  &
              ix,dxm,dx,dxp,dxpp)
          CALL cubt3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dzm,  &
              dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,  &
              ix,dxm,dx,dxp,dxpp)
          CALL cubt3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dzm,  &
              dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,  &
              ix,dxm,dx,dxp,dxpp)
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

    END SUBROUTINE cubt4
end module