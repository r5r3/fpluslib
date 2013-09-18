module mod_rgrd3
    use mod_rgrd2
    implicit none
    
contains
    !

    ! Code converted using TO_F90 by Alan Miller
    ! Date: 2013-09-18  Time: 12:23:20

    ! ... file rgrd3.f

    !     this file contains documentation for subroutine rgrd3 followed by
    !     fortran code for rgrd3 and additional subroutines.

    ! ... author

    !     John C. Adams (NCAR 1999)

    ! ... subroutine rgrd3(nx,ny,nz,x,y,z,p,mx,my,mz,xx,yy,zz,q,intpol,
    !    +                 w,lw,iw,liw,ier)

    ! ... purpose

    !     subroutine rgrd3 interpolates the values p(i,j,k) on the orthogonal
    !     grid (x(i),y(j),z(k)) for i=1,...,nx; j=1,...,ny; k=1,...,nz
    !     onto q(ii,jj,kk) on the orthogonal grid (xx(ii),yy(jj),zz(kk)) for
    !     ii=1,...,mx; jj=1,...,my; kk=1,...,mz.

    ! ... language

    !     coded in portable FORTRAN77 and FORTRAN90

    ! ... test program

    !     file trgrd3.f on regridpack includes a test program for subroutine rgrd3

    ! ... method

    !     linear or cubic interpolation is used (independently) in
    !     each direction (see argument intpol).

    ! ... required files

    !     files rgrd2.f and rgrd1.f must be loaded with rgrd3.f.  they
    !     include subroutines called by the routines in rgrd3.f

    ! ... requirements

    !     each of the x,y,z grids must be strictly montonically increasing
    !     and each of the xx,yy,zz grids must be montonically increasing
    !     (see ier = 4).  in addition the (X,Y,Z) region

    !          [xx(1),xx(mx)] X [yy(1),yy(my)] X [zz(1),zz(mz)]

    !     must lie within the (X,Y,Z) region

    !          [x(1),x(nx)] X [y(1),y(ny)] X [z(1),z(nz)].

    !     extrapolation is not allowed (see ier=3).  if these (X,Y,Z)
    !     regions are identical and the orthogonal grids are UNIFORM
    !     in each direction then subroutine rgrd3u (see file rgrd3u.f)
    !     should be used instead of rgrd3.

    ! ... efficiency

    !     inner most loops in regridpack software vectorize.  if
    !     the integer arguments mx,my,mz (see below) have different values,
    !     optimal vectorization will be achieved if mx > my > mz.

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

    ! ... x

    !     a real (kind=8) nx vector of strictly increasing values which defines the x
    !     portion of the orthogonal grid on which p is given

    ! ... y

    !     a real (kind=8) ny vector of strictly increasing values which defines the y
    !     portion of the orthogonal grid on which p is given

    ! ... z

    !     a real (kind=8) nz vector of strictly increasing values which defines the z
    !     portion of the orthogonal grid on which p is given

    ! ... p

    !     a real (kind=8) nx by ny by nz array of values given on the (x,y,z) grid

    ! ... mx

    !     the integer dimension of the grid vector xx and the first dimension of q.
    !     mx > 0 is required.

    ! ... my

    !     the integer dimension of the grid vector yy and the second dimension of q.
    !     my > 0 is required.

    ! ... mz

    !     the integer dimension of the grid vector zz and the third dimension of q.
    !     mz > 0 is required.

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

    ! ... intpol

    !     an integer vector of dimension 3 which sets linear or cubic
    !     interpolation in each of the x,y,z directions as follows:

    !        intpol(1) = 1 sets linear interpolation in the x direction
    !        intpol(1) = 3 sets cubic interpolation in the x direction.

    !        intpol(2) = 1 sets linear interpolation in the y direction
    !        intpol(2) = 3 sets cubic interpolation in the y direction.

    !        intpol(3) = 1 sets linear interpolation in the z direction
    !        intpol(3) = 3 sets cubic interpolation in the z direction.

    !     values other than 1 or 3 in intpol are not allowed (ier = 5).

    ! ... w

    !     a real (kind=8) work space of length at least lw which must be provided in the
    !     routine calling rgrd3


    ! ... lw

    !     the integer length of the real (kind=8) work space w.  let

    !          lwx =   mx            if intpol(1) = 1
    !          lwx = 4*mx            if intpol(1) = 3

    !          lwy = my+2*mx         if intpol(2) = 1
    !          lwy = 4*(mx+my)       if intpol(2) = 3

    !          lwz = 2*mx*my+mz      if intpol(3) = 1
    !          lwz = 4*(mx*my+mz)    if intpol(3) = 3

    !     then lw must be greater than or equal to lwx+lwy+lwz

    ! ... iw

    !     an integer work space of length at least liw which must be provided in the
    !     routine calling rgrd3

    ! ... liw

    !     the integer length of the integer work space iw.  liw must be at least mx+my+mz

    ! *** output arguments


    ! ... q

    !     a real (kind=8) mx by my by mz array of values on the (xx,yy,zz) grid which are
    !     interpolated from p on the (x,y,z) grid

    ! ... ier

    !     an integer error flag set as follows:

    !     ier = 0 if no errors in input arguments are detected

    !     ier = 1 if  min0(mx,my,mz) < 1

    !     ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
    !                ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3 (or)
    !                nz < 2 when intpol(3)=1 or nz < 4 when intpol(3)=3.

    !     ier = 3 if xx(1) < x(1) or x(nx) < xx(mx) (or)
    !                yy(1) < y(1) or y(ny) < yy(my) (or)
    !                zz(1) < z(1) or z(nz) < zz(mz)

    ! *** to avoid this flag when end points are intended to be the
    !     same but may differ slightly due to roundoff error, they
    !     should be set exactly in the calling routine (e.g., if both
    !     grids have the same y boundaries then yy(1)=y(1) and yy(my)=y(ny)
    !     should be set before calling rgrd3)

    !     ier = 4 if one of the grids x,y,z is not strictly monotonically
    !             increasing or if one of the grids xx,yy,zz is not
    !             montonically increasing.  more precisely if:

    !             x(i+1) <= x(i) for some i such that 1 <= i < nx (or)

    !             y(j+1) <= y(j) for some j such that 1 <= j < ny (or)

    !             z(k+1) <= z(k) for some k such that 1 <= k < nz (or)

    !             xx(ii+1) < xx(ii) for some ii such that 1 <= ii < mx (or)

    !             yy(jj+1) < yy(jj) for some jj such that 1 <= jj < my (or)

    !             zz(kk+1) < zz(k)  for some kk such that 1 <= kk < mz

    !     ier = 5 if lw or liw is too small (insufficient work space)

    !     ier = 6 if any of intpol(1),intpol(2),intpol(3) is not equal to 1 or 3

    ! ************************************************************************

    !     end of rgrd3 documentation, fortran code follows:

    ! ************************************************************************

    SUBROUTINE rgrd3(nx,ny,nz,x,y,z,p,mx,my,mz,xx,yy,zz,q,intpol, w,lw,iw,liw,ier)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: ny
    INTEGER, INTENT(IN)                      :: nz
    real (kind=8), INTENT(IN)                :: x(nx)
    real (kind=8), INTENT(IN)                :: y(ny)
    real (kind=8), INTENT(IN)                :: z(nz)
    real (kind=8), INTENT(IN)                :: p(nx,ny,nz)
    INTEGER, INTENT(IN)                      :: mx
    INTEGER, INTENT(IN)                      :: my
    INTEGER, INTENT(IN)                      :: mz
    real (kind=8), INTENT(IN)                :: xx(mx)
    real (kind=8), INTENT(IN)                :: yy(my)
    real (kind=8), INTENT(IN)                :: zz(mz)
    real (kind=8), INTENT(OUT)               :: q(mx,my,mz)
    INTEGER, INTENT(IN)                      :: intpol(3)
    real (kind=8), INTENT(INOUT)             :: w(lw)
    INTEGER, INTENT(IN)                      :: lw
    INTEGER, INTENT(INOUT)                   :: iw(liw)
    INTEGER, INTENT(IN)                      :: liw
    INTEGER, INTENT(OUT)                     :: ier

    INTEGER :: lwx,lwy,lwz,jy,kz,mxmy




    INTEGER :: i,ii,j,jj,k,kk
    INTEGER :: i2,i3,i4,i5
    INTEGER :: j2,j3,j4,j5,j6,j7,j8,j9
    INTEGER :: k2,k3,k4,k5,k6,k7,k8,k9

    !     check input arguments

    ier = 1

    !     check (xx,yy,zz) grid resolution

    IF (MIN0(mx,my,mz) < 1) RETURN

    !     check intpol

    ier = 6
    IF (intpol(1) /= 1 .AND. intpol(1) /= 3) RETURN
    IF (intpol(2) /= 1 .AND. intpol(2) /= 3) RETURN
    IF (intpol(3) /= 1 .AND. intpol(3) /= 3) RETURN

    !     check (x,y,z) grid resolution

    ier = 2
    IF (intpol(1) == 1 .AND. nx < 2) RETURN
    IF (intpol(1) == 3 .AND. nx < 4) RETURN
    IF (intpol(2) == 1 .AND. ny < 2) RETURN
    IF (intpol(2) == 3 .AND. ny < 4) RETURN
    IF (intpol(3) == 1 .AND. nz < 2) RETURN
    IF (intpol(3) == 3 .AND. nz < 4) RETURN

    !     check work space length input and set minimum

    ier = 5
    mxmy = mx*my
    IF (intpol(1) == 1) THEN
      lwx = mx
    ELSE
      lwx = 4*mx
    END IF
    IF (intpol(2) == 1) THEN
      lwy = (my+2*mx)
    ELSE
      lwy = 4*(my+mx)
    END IF
    IF (intpol(3) == 1) THEN
      lwz = (2*mxmy+mz)
    ELSE
      lwz = 4*(mxmy+mz)
    END IF
    IF (lw < lwx+lwy+lwz) RETURN
    IF (liw < mx+my+mz) RETURN

    !     check (xx,yy,zz) grid contained in (x,y,z) grid

    ier = 3
    IF (xx(1) < x(1) .OR. xx(mx) > x(nx)) RETURN
    IF (yy(1) < y(1) .OR. yy(my) > y(ny)) RETURN
    IF (zz(1) < z(1) .OR. zz(mz) > z(nz)) RETURN

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
    DO ii=2,mx
      IF (xx(ii-1) > xx(ii)) RETURN
    END DO
    DO jj=2,my
      IF (yy(jj-1) > yy(jj)) RETURN
    END DO
    DO kk=2,mz
      IF (zz(kk-1) > zz(kk)) RETURN
    END DO

    !     arguments o.k.

    ier = 0
    jy = mx+1
    kz = mx+my+1
    IF (intpol(3) == 1) THEN
      
    !     linearly interpolate in nz, set work space pointers and scales
      
      k2 = 1
      k3 = k2
      k4 = k3+mz
      k5 = k4
      k6 = k5
      k7 = k6
      k8 = k7+mxmy
      k9 = k8+mxmy
      CALL linmx(nz,z,mz,zz,iw(kz),w(k3))
      j2 = k9
      
    !     set indices and scales which depend on y interpolation
      
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
      
    !     set indices and scales which depend on x interpolation
      
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
      CALL lint3(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,iw(kz),  &
          w(k3),w(k7),w(k8),iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),  &
          w(j7),w(j8),w(j9),iw,w(i2),w(i3),w(i4),w(i5))
      RETURN
    ELSE
      
    !     cubically interpolate in z
      
      k2 = 1
      k3 = k2+mz
      k4 = k3+mz
      k5 = k4+mz
      k6 = k5+mz
      k7 = k6+mxmy
      k8 = k7+mxmy
      k9 = k8+mxmy
      CALL cubnmx(nz,z,mz,zz,iw(kz),w(k2),w(k3),w(k4),w(k5))
      j2 = k9+mxmy
      
    !     set indices which depend on y interpolation
      
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
      CALL cubt3(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,  &
          iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),  &
          iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),  &
          iw,w(i2),w(i3),w(i4),w(i5))
      RETURN
      
    END IF

    END SUBROUTINE rgrd3

    SUBROUTINE lint3(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,kz,  &
        dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)

    !     linearly interpolate in z direction


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







    INTEGER :: k,kk,iijj,ksave

    IF (intpol(2) == 1) THEN
      
    !     linear in y
      
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
          CALL lint2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,  &
              pj,pjp,ix,dxm,dx,dxp,dxpp)
        ELSE
          
    !     interpolate k,k+1 in pk,pkp on xx,yy mesh
          
          CALL lint2(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,dy,  &
              pj,pjp,ix,dxm,dx,dxp,dxpp)
          CALL lint2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,  &
              pj,pjp,ix,dxm,dx,dxp,dxpp)
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
          CALL cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
        ELSE
          
    !     interpolate k,k+1 in pk,pkp on xx,yy mesh
          
          CALL cubt2(nx,ny,p(1,1,k),mx,my,pk,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
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
    END SUBROUTINE lint3

    SUBROUTINE cubt3(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,  &
        kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,  &
        pjp,pjpp,ix,dxm,dx,dxp,dxpp)

    !     cubically interpolate in z

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
    INTEGER :: k,kk,ksave,iijj








    IF (intpol(2) == 1) THEN
      
    !       linear in y
      
      ksave = -3
      DO kk=1,mz
        k = kz(kk)
        IF (k == ksave) THEN
          
    !       k pointer has not moved since last pass (no updates or interpolation)
          
        ELSE IF (k == ksave+1) THEN
          
    !       update k-1,k,k+1 and interpolate k+2
          
          DO iijj=1,mxmy
            pkm(iijj) = pk(iijj)
            pk(iijj) = pkp(iijj)
            pkp(iijj) = pkpp(iijj)
          END DO
          CALL lint2(nx,ny,p(1,1,k+2),mx,my,pkpp,  &
              intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
        ELSE IF (k == ksave+2) THEN
          
    !       update k-1,k and interpolate k+1,k+2
          
          DO iijj=1,mxmy
            pkm(iijj) = pkp(iijj)
            pk(iijj) = pkpp(iijj)
          END DO
          CALL lint2(nx,ny,p(1,1,k+1),mx,my,pkp,  &
              intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
          CALL lint2(nx,ny,p(1,1,k+2),mx,my,pkpp,  &
              intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
        ELSE IF (k == ksave+3) THEN
          
    !       update k-1 and interpolate k,k+1,k+2
          
          DO iijj=1,mxmy
            pkm(iijj) = pkpp(iijj)
          END DO
          CALL lint2(nx,ny,p(1,1,k),mx,my,pk,  &
              intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
          CALL lint2(nx,ny,p(1,1,k+1),mx,my,pkp,  &
              intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
          CALL lint2(nx,ny,p(1,1,k+2),mx,my,pkpp,  &
              intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
        ELSE
          
    !       interpolate all four k-1,k,k+1,k+2
          
          CALL lint2(nx,ny,p(1,1,k-1),mx,my,pkm,  &
              intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
          CALL lint2(nx,ny,p(1,1,k),mx,my,pk,  &
              intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
          CALL lint2(nx,ny,p(1,1,k+1),mx,my,pkp,  &
              intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
          CALL lint2(nx,ny,p(1,1,k+2),mx,my,pkpp,  &
              intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
        END IF
        
    !       save k pointer for next pass
        
        ksave = k
        
    !       cubically interpolate q(ii,jj,kk) from pkm,pk,pkp,pkpp in z direction
        
        DO iijj=1,mxmy
          q(iijj,kk) = dzm(kk)*pkm(iijj) + dz(kk)*pk(iijj) +  &
              dzp(kk)*pkp(iijj) + dzpp(kk)*pkpp(iijj)
        END DO
      END DO
      RETURN
      
    ELSE
      
    !       cubic in y
      
      ksave = -3
      DO kk=1,mz
        k = kz(kk)
        IF (k == ksave) THEN
          
    !       k pointer has not moved since last pass (no updates or interpolation)
          
        ELSE IF (k == ksave+1) THEN
          
    !       update k-1,k,k+1 and interpolate k+2
          
          DO iijj=1,mxmy
            pkm(iijj) = pk(iijj)
            pk(iijj) = pkp(iijj)
            pkp(iijj) = pkpp(iijj)
          END DO
          CALL cubt2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dym,dy,  &
              dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
        ELSE IF (k == ksave+2) THEN
          
    !       update k-1,k and interpolate k+1,k+2
          
          DO iijj=1,mxmy
            pkm(iijj) = pkp(iijj)
            pk(iijj) = pkpp(iijj)
          END DO
          CALL cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dym,dy,  &
              dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL cubt2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dym,dy,  &
              dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
        ELSE IF (k == ksave+3) THEN
          
    !       update k-1 and interpolate k,k+1,k+2
          
          DO iijj=1,mxmy
            pkm(iijj) = pkpp(iijj)
          END DO
          CALL cubt2(nx,ny,p(1,1,k),mx,my,pk,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL cubt2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
        ELSE
          
    !     interpolate all four k-1,k,k+1,k+2
          
          CALL cubt2(nx,ny,p(1,1,k-1),mx,my,pkm,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL cubt2(nx,ny,p(1,1,k),mx,my,pk,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          CALL cubt2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,  &
              jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
          
        END IF
        
    !       save k pointer for next pass
        
        ksave = k
        
    !       cubically interpolate q(ii,jj,kk) from pkm,pk,pkp,pkpp in z direction
        
        DO iijj=1,mxmy
          q(iijj,kk) = dzm(kk)*pkm(iijj) + dz(kk)*pk(iijj) +  &
              dzp(kk)*pkp(iijj) + dzpp(kk)*pkpp(iijj)
        END DO
      END DO
      RETURN
    END IF

    END SUBROUTINE cubt3
end module