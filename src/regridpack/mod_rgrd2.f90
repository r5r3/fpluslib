module mod_rgrd2
    use mod_rgrd1
    implicit none
    
contains
    !

    ! Code converted using TO_F90 by Alan Miller
    ! Date: 2013-09-18  Time: 12:22:55

    ! ... file rgrd2.f

    !     this file contains documentation for subroutine rgrd2 followed by
    !     fortran code for rgrd2 and additional subroutines.

    ! ... author

    !     John C. Adams (NCAR 1999)

    ! ... subroutine rgrd2(nx,ny,x,y,p,mx,my,xx,yy,q,intpol,w,lw,iw,liw,ier)

    ! ... purpose

    !     subroutine rgrd2 interpolates the values p(i,j) on the orthogonal
    !     grid (x(i),y(j)) for i=1,...,nx and j=1,...,ny onto q(ii,jj) on the
    !     orthogonal grid (xx(ii),yy(jj)) for ii=1,...,mx and jj=1,...,my.

    ! ... language

    !     coded in portable FORTRAN77 and FORTRAN90

    ! ... test program

    !     file trgrd2.f on regridpack includes a test program for subroutine rgrd2

    ! ... method

    !     linear or cubic interpolation is used (independently) in
    !     each direction (see argument intpol).

    ! ... required files

    !     file rgrd1.f must be loaded with rgrd2.f.  it includes
    !     subroutines called by the routines in rgrd2.f

    ! ... requirements

    !     each of the x,y grids must be strictly montonically increasing
    !     and each of the xx,yy grids must be montonically increasing (see
    !     ier = 4).  in addition the (X,Y) region

    !          [xx(1),xx(mx)] X [yy(1),yy(my)]

    !     must lie within the (X,Y) region

    !          [x(1),x(nx)] X [y(1),y(ny)].

    !     extrapolation is not allowed (see ier=3).  if these (X,Y)
    !     regions are identical and the orthogonal grids are UNIFORM
    !     in each direction then subroutine rgrd2u (see file rgrd2u.f)
    !     should be used instead of rgrd2.

    ! ... efficiency

    !     inner most loops in regridpack software vectorize.
    !     If the arguments mx,my (see below) have different values, optimal
    !     vectorization will be achieved if mx > my.


    ! *** input argument


    ! ... nx

    !     the integer dimension of the grid vector x and the first dimension
    !     of p.  nx > 1 if intpol(1) = 1 or nx > 3 if intpol(1) = 3 is required.

    ! ... ny

    !     the integer dimension of the grid vector y and the second dimension
    !     of p.  ny > 1 if intpol(2) = 1 or ny > 3 if intpol(2) = 3 is required.

    ! ... x

    !     a real (kind=8) nx vector of strictly increasing values which defines the x
    !     portion of the orthogonal grid on which p is given

    ! ... y

    !     a real (kind=8) ny vector of strictly increasing values which defines the y
    !     portion of the orthogonal grid on which p is given

    ! ... p

    !     a real (kind=8) nx by ny array of values given on the orthogonal (x,y) grid

    ! ... mx

    !     the integer dimension of the grid vector xx and the first dimension
    !     of q.  mx > 0 is required.

    ! ... my

    !     the integer dimension of the grid vector yy and the second dimension
    !     of q.  my > 0 is required.

    ! ... xx

    !     a real (kind=8) mx vector of increasing values which defines the x portion of the
    !     orthogonal grid on which q is defined.  xx(1) < x(1) or xx(mx) > x(nx)
    !     is not allowed (see ier = 3)

    ! ... yy

    !     a real (kind=8) my vector of increasing values which defines the y portion of the
    !     orthogonal grid on which q is defined.  yy(1) < y(1) or yy(my) > y(ny)
    !     is not allowed (see ier = 3)

    ! ... intpol

    !     an integer vector of dimension 2 which sets linear or cubic
    !     interpolation in the x,y directions as follows:

    !        intpol(1) = 1 sets linear interpolation in the x direction
    !        intpol(1) = 3 sets cubic interpolation in the x direction.

    !        intpol(2) = 1 sets linear interpolation in the y direction
    !        intpol(2) = 3 sets cubic interpolation in the y direction.

    !     values other than 1 or 3 in intpol are not allowed (ier = 5).

    ! ... w

    !     a real (kind=8) work space of length at least lw which must be provided in the
    !     routine calling rgrd2

    ! ... lw

    !     the integer length of the real (kind=8) work space w.  let

    !          lwx =   mx            if intpol(1) = 1
    !          lwx = 4*mx            if intpol(1) = 3

    !          lwy = my+2*mx         if intpol(2) = 1
    !          lwy = 4*(mx+my)       if intpol(2) = 3

    !     then lw must be greater than or equal to lwx+lwy

    ! ... iw

    !     an integer work space of length at least liw which must be provided in the
    !     routine calling rgrd2

    ! ... liw

    !     the integer length of the integer work space iw.  liw must be at least mx+my

    ! *** output arguments


    ! ... q

    !     a real (kind=8) mx by my array of values on the (xx,yy) grid which are
    !     interpolated from p on the (x,y) grid

    ! ... ier

    !     an integer error flag set as follows:

    !     ier = 0 if no errors in input arguments are detected

    !     ier = 1 if  min0(mx,my) < 1

    !     ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
    !                ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3

    !     ier = 3 if xx(1) < x(1) or x(nx) < xx(mx) (or)
    !                yy(1) < y(1) or y(ny) < yy(my) (or)

    ! *** to avoid this flag when end points are intended to be the
    !     same but may differ slightly due to roundoff error, they
    !     should be set exactly in the calling routine (e.g., if both
    !     grids have the same y boundaries then yy(1)=y(1) and yy(my)=y(ny)
    !     should be set before calling rgrd2)

    !     ier = 4 if one of the grids x,y is not strictly monotonically
    !             increasing or if one of the grids xx,yy is not
    !             montonically increasing.  more precisely if:

    !             x(i+1) <= x(i) for some i such that 1 <= i < nx (or)

    !             y(j+1) <= y(j) for some j such that 1 <= j < ny (or)

    !             xx(ii+1) < xx(ii) for some ii such that 1 <= ii < mx (or)

    !             yy(jj+1) < yy(jj) for some jj such that 1 <= jj < my

    !     ier = 5 if lw or liw is to small (insufficient work space)

    !     ier = 6 if intpol(1) or intpol(2) is not equal to 1 or 3

    ! ************************************************************************

    !     end of rgrd2 documentation, fortran code follows:

    ! ************************************************************************

    SUBROUTINE rgrd2(nx,ny,x,y,p,mx,my,xx,yy,q,intpol,w,lw,iw,liw,ier)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: ny
    real (kind=8), INTENT(IN)                :: x(nx)
    real (kind=8), INTENT(IN)                :: y(ny)
    real (kind=8), INTENT(IN)                :: p(nx,ny)
    INTEGER, INTENT(IN)                      :: mx
    INTEGER, INTENT(IN)                      :: my
    real (kind=8), INTENT(IN)                :: xx(mx)
    real (kind=8), INTENT(IN)                :: yy(my)
    real (kind=8), INTENT(OUT)               :: q(mx,my)
    INTEGER, INTENT(IN)                      :: intpol(2)
    real (kind=8), INTENT(INOUT)             :: w(lw)
    INTEGER, INTENT(IN)                      :: lw
    INTEGER, INTENT(INOUT)                   :: iw(liw)
    INTEGER, INTENT(IN)                      :: liw
    INTEGER, INTENT(OUT)                     :: ier



    INTEGER :: i,ii,j,jj,j2,j3,j4,j5,j6,j7,j8,j9,i2,i3,i4,i5
    INTEGER :: jy,lwx,lwy

    !     check input arguments

    ier = 1

    !     check (xx,yy) grid resolution

    IF (MIN0(mx,my) < 1) RETURN

    !     check intpol

    ier = 6
    IF (intpol(1) /= 1 .AND. intpol(1) /= 3) RETURN
    IF (intpol(2) /= 1 .AND. intpol(2) /= 3) RETURN

    !     check (x,y) grid resolution

    ier = 2
    IF (intpol(1) == 1 .AND. nx < 2) RETURN
    IF (intpol(1) == 3 .AND. nx < 4) RETURN
    IF (intpol(2) == 1 .AND. ny < 2) RETURN
    IF (intpol(2) == 3 .AND. ny < 4) RETURN

    !     check work space lengths

    ier = 5
    IF (intpol(1) == 1) THEN
      lwx = mx
    ELSE
      lwx = 4*mx
    END IF
    IF (intpol(2) == 1) THEN
      lwy = my+2*mx
    ELSE
      lwy = 4*(mx+my)
    END IF
    IF (lw < lwx+lwy) RETURN
    IF (liw < mx+my) RETURN

    !     check (xx,yy) grid contained in (x,y) grid

    ier = 3
    IF (xx(1) < x(1) .OR. xx(mx) > x(nx)) RETURN
    IF (yy(1) < y(1) .OR. yy(my) > y(ny)) RETURN

    !     check montonicity of grids

    ier = 4
    DO i=2,nx
      IF (x(i-1) >= x(i)) RETURN
    END DO
    DO j=2,ny
      IF (y(j-1) >= y(j)) RETURN
    END DO
    DO ii=2,mx
      IF (xx(ii-1) > xx(ii)) RETURN
    END DO
    DO jj=2,my
      IF (yy(jj-1) > yy(jj)) RETURN
    END DO

    !     arguments o.k.

    ier = 0

    !     set pointer in integer work space

    jy = mx+1
    IF (intpol(2) == 1) THEN
      
    !     linearly interpolate in y
      
      j2 = 1
      j3 = j2
      j4 = j3+my
      j5 = j4
      j6 = j5
      j7 = j6
      j8 = j7+mx
      j9 = j8+mx
      
    !     set y interpolation indices and scales and linearly interpolate
      
      CALL linmx(ny,y,my,yy,iw(jy),w(j3))
      i2 = j9
      
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
      CALL lint2(nx,ny,p,mx,my,q,intpol,iw(jy),w(j3),  &
          w(j7),w(j8),iw,w(i2),w(i3),w(i4),w(i5))
      RETURN
      
    ELSE
      
    !     cubically interpolate in y, set indice pointers
      
      j2 = 1
      j3 = j2+my
      j4 = j3+my
      j5 = j4+my
      j6 = j5+my
      j7 = j6+mx
      j8 = j7+mx
      j9 = j8+mx
      CALL cubnmx(ny,y,my,yy,iw(jy),w(j2),w(j3),w(j4),w(j5))
      i2 =  j9+mx
      
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
      CALL cubt2(nx,ny,p,mx,my,q,intpol,iw(jy),w(j2),w(j3),  &
          w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),iw,w(i2),w(i3),w(i4),w(i5))
      RETURN
    END IF
    END SUBROUTINE rgrd2

    SUBROUTINE lint2(nx,ny,p,mx,my,q,intpol,jy,dy,pj,pjp, ix,dxm,dx,dxp,dxpp)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: ny
    real (kind=8), INTENT(IN)                :: p(nx,ny)
    INTEGER, INTENT(IN)                      :: mx
    INTEGER, INTENT(IN)                      :: my
    real (kind=8), INTENT(OUT)               :: q(mx,my)
    INTEGER, INTENT(IN)                      :: intpol(2)
    INTEGER, INTENT(IN)                      :: jy(my)
    real (kind=8), INTENT(IN)                :: dy(my)
    real (kind=8), INTENT(OUT)               :: pj(mx)
    real (kind=8), INTENT(IN OUT)            :: pjp(mx)
    INTEGER, INTENT(IN OUT)                  :: ix(mx)
    real (kind=8), INTENT(IN OUT)            :: dxm(mx)
    real (kind=8), INTENT(IN OUT)            :: dx(mx)
    real (kind=8), INTENT(IN OUT)            :: dxp(mx)
    real (kind=8), INTENT(IN OUT)            :: dxpp(mx)

    INTEGER :: jsave,j,jj,ii




    !     linearly interpolate in y

    IF (intpol(1) == 1) THEN
      
    !     linear in x
      
      jsave = -1
      DO jj=1,my
        j = jy(jj)
        IF (j == jsave) THEN
          
    !       j pointer has not moved since last pass (no updates or interpolation)
          
        ELSE IF (j == jsave+1) THEN
          
    !       update j and interpolate j+1
          
          DO ii=1,mx
            pj(ii) = pjp(ii)
          END DO
          CALL lint1(nx,p(1,j+1),mx,pjp,ix,dx)
        ELSE
          
    !       interpolate j,j+1in pj,pjp on xx mesh
          
          CALL lint1(nx,p(1,j),mx,pj,ix,dx)
          CALL lint1(nx,p(1,j+1),mx,pjp,ix,dx)
        END IF
        
    !       save j pointer for next pass
        
        jsave = j
        
    !       linearly interpolate q(ii,jj) from pjp,pj in y direction
        
        DO ii=1,mx
          q(ii,jj) = pj(ii)+dy(jj)*(pjp(ii)-pj(ii))
        END DO
      END DO
      
    ELSE
      
    !     cubic in x
      
      jsave = -1
      DO jj=1,my
        j = jy(jj)
        IF (j == jsave) THEN
          
    !       j pointer has not moved since last pass (no updates or interpolation)
          
        ELSE IF (j == jsave+1) THEN
          
    !       update j and interpolate j+1
          
          DO ii=1,mx
            pj(ii) = pjp(ii)
          END DO
          CALL cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
        ELSE
          
    !       interpolate j,j+1 in pj,pjp on xx mesh
          
          CALL cubt1(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp)
          CALL cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
        END IF
        
    !       save j pointer for next pass
        
        jsave = j
        
    !       linearly interpolate q(ii,jj) from pjp,pj in y direction
        
        DO ii=1,mx
          q(ii,jj) = pj(ii)+dy(jj)*(pjp(ii)-pj(ii))
        END DO
      END DO
      RETURN
    END IF
    END SUBROUTINE lint2

    SUBROUTINE cubt2(nx,ny,p,mx,my,q,intpol,jy,dym,dy,dyp,  &
        dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: ny
    real (kind=8), INTENT(IN)                :: p(nx,ny)
    INTEGER, INTENT(IN)                      :: mx
    INTEGER, INTENT(IN)                      :: my
    real (kind=8), INTENT(OUT)               :: q(mx,my)
    INTEGER, INTENT(IN)                      :: intpol(2)
    INTEGER, INTENT(IN)                      :: jy(my)
    real (kind=8), INTENT(IN)                :: dym(my)
    real (kind=8), INTENT(IN)                :: dy(my)
    real (kind=8), INTENT(IN)                :: dyp(my)
    real (kind=8), INTENT(IN OUT)            :: dypp(my)
    real (kind=8), INTENT(OUT)               :: pjm(mx)
    real (kind=8), INTENT(IN OUT)            :: pj(mx)
    real (kind=8), INTENT(IN OUT)            :: pjp(mx)
    real (kind=8), INTENT(IN OUT)            :: pjpp(mx)
    INTEGER, INTENT(IN OUT)                  :: ix(mx)
    real (kind=8), INTENT(IN OUT)            :: dxm(mx)
    real (kind=8), INTENT(IN OUT)            :: dx(mx)
    real (kind=8), INTENT(IN OUT)            :: dxp(mx)
    real (kind=8), INTENT(IN OUT)            :: dxpp(mx)

    INTEGER :: jsave,j,jj,ii





    IF (intpol(1) == 1) THEN
      
    !     linear in x
      
      jsave = -3
      DO jj=1,my
        
    !       load closest four j lines containing interpolate on xx mesh
    !       for j-1,j,j+1,j+2 in pjm,pj,pjp,pjpp
        
        j = jy(jj)
        IF (j == jsave) THEN
          
    !       j pointer has not moved since last pass (no updates or interpolation)
          
        ELSE IF (j == jsave+1) THEN
          
    !       update j-1,j,j+1 and interpolate j+2
          
          DO ii=1,mx
            pjm(ii) = pj(ii)
            pj(ii) = pjp(ii)
            pjp(ii) = pjpp(ii)
          END DO
          CALL lint1(nx,p(1,j+2),mx,pjpp,ix,dx)
        ELSE IF (j == jsave+2) THEN
          
    !     update j-1,j and interpolate j+1,j+2
          
          DO ii=1,mx
            pjm(ii) = pjp(ii)
            pj(ii) = pjpp(ii)
          END DO
          CALL lint1(nx,p(1,j+1),mx,pjp,ix,dx)
          CALL lint1(nx,p(1,j+2),mx,pjpp,ix,dx)
        ELSE IF (j == jsave+3) THEN
          
    !       update j-1 and interpolate j,j+1,j+2
          
          DO ii=1,mx
            pjm(ii) = pjpp(ii)
          END DO
          CALL lint1(nx,p(1,j),mx,pj,ix,dx)
          CALL lint1(nx,p(1,j+1),mx,pjp,ix,dx)
          CALL lint1(nx,p(1,j+2),mx,pjpp,ix,dx)
        ELSE
          
    !       interpolate all four j-1,j,j+1,j+2
          
          CALL lint1(nx,p(1,j-1),mx,pjm,ix,dx)
          CALL lint1(nx,p(1,j),mx,pj,ix,dx)
          CALL lint1(nx,p(1,j+1),mx,pjp,ix,dx)
          CALL lint1(nx,p(1,j+2),mx,pjpp,ix,dx)
        END IF
        
    !     save j pointer for next pass
        
        jsave = j
        
    !     cubically interpolate q(ii,jj) from pjm,pj,pjp,pjpp in y direction
        
        DO ii=1,mx
          q(ii,jj) = dym(jj)*pjm(ii)+dy(jj)*pj(ii)+dyp(jj)*pjp(ii)+  &
              dypp(jj)*pjpp(ii)
        END DO
      END DO
      RETURN
      
    ELSE
      
    !     cubic in x
      
      jsave = -3
      DO jj=1,my
        
    !       load closest four j lines containing interpolate on xx mesh
    !       for j-1,j,j+1,j+2 in pjm,pj,pjp,pjpp
        
        j = jy(jj)
        IF (j == jsave) THEN
          
    !         j pointer has not moved since last pass (no updates or interpolation)
          
        ELSE IF (j == jsave+1) THEN
          
    !         update j-1,j,j+1 and interpolate j+2
          
          DO ii=1,mx
            pjm(ii) = pj(ii)
            pj(ii) = pjp(ii)
            pjp(ii) = pjpp(ii)
          END DO
          CALL cubt1(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp)
        ELSE IF (j == jsave+2) THEN
          
    !         update j-1,j and interpolate j+1,j+2
          
          DO ii=1,mx
            pjm(ii) = pjp(ii)
            pj(ii) = pjpp(ii)
          END DO
          CALL cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
          CALL cubt1(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp)
        ELSE IF (j == jsave+3) THEN
          
    !         update j-1 and interpolate j,j+1,j+2
          
          DO ii=1,mx
            pjm(ii) = pjpp(ii)
          END DO
          CALL cubt1(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp)
          CALL cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
          CALL cubt1(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp)
        ELSE
          
    !         interpolate all four j-1,j,j+1,j+2
          
          CALL cubt1(nx,p(1,j-1),mx,pjm,ix,dxm,dx,dxp,dxpp)
          CALL cubt1(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp)
          CALL cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
          CALL cubt1(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp)
        END IF
        
    !       save j pointer for next pass
        
        jsave = j
        
    !       cubically interpolate q(ii,jj) from pjm,pj,pjp,pjpp in y direction
        
        DO ii=1,mx
          q(ii,jj) = dym(jj)*pjm(ii)+dy(jj)*pj(ii)+dyp(jj)*pjp(ii)+  &
              dypp(jj)*pjpp(ii)
        END DO
      END DO
      RETURN
    END IF
    END SUBROUTINE cubt2
end module