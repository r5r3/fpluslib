module mod_rgrd1
    implicit none
    
contains
    !

    ! Code converted using TO_F90 by Alan Miller
    ! Date: 2013-09-18  Time: 12:22:39

    ! ... file rgrd1.f

    !     this file contains documentation for subroutine rgrd1 followed by
    !     fortran code for rgrd1 and additional subroutines.

    ! ... author

    !     John C. Adams (NCAR 1999)

    ! ... subroutine rgrd1(nx,x,p,mx,xx,q,intpol,w,lw,iw,liw,ier)

    ! ... purpose

    !     subroutine rgrd1 interpolates the values p(i) on the grid x(i)
    !     for i=1,...,nx onto q(ii) on the grid xx(ii),ii=1,...,mx.

    ! ... language

    !     coded in portable FORTRAN90 and FORTRAN77

    ! ... test program

    !     file trgrd1.f on regridpack includes a test program for subroutine rgrd1

    ! ... method

    !     Linear interpolation (intpol=1)

    !       Given ii in the integer interval [1,mx], let i be the
    !       largest integer in the integer interval [1,nx-1] such
    !       that x(i) is less than or equal to xx(ii).  Then q(ii)
    !       at xx(ii) is linearly interpolated from p(i),p(i+1) at
    !       x(i),x(i+1).

    !     Cubic interpolation (intpol=3)

    !       Given ii in the integer interval [1,mx], let i be the
    !       largest integer in the integer interval [2,nx-2] such
    !       that x(i) is less than or equal to xx(ii).  Then q(ii)
    !       at xx(ii) is cubically interpolated from p(i-1),p(i),
    !       p(i+1),p(i+2) at x(i-1),x(i),x(i+1),x(i+2).

    ! ... required files

    !     none

    ! ... requirements

    !     x must be a strictly increasing grid and xx must be an increasing
    !     grid (see ier = 4).  in addition the interval

    !          [xx(1),xx(mx)]

    !     must lie within the interval

    !          [x(1),x(nx)].

    !     extrapolation is not allowed (see ier=3).  if these intervals
    !     are identical and the x and xx grids are UNIFORM then subroutine
    !     rgrd1u (see file rgrd1u.f) should be used in place of rgrd1.

    ! ... required files

    !     none

    ! ... efficiency

    !     inner most loops in regridpack software vectorize.


    ! *** input arguments


    ! ... nx

    !     the integer dimension of the grid vector x and the dimension of p.
    !     nx > 1 if intpol = 1 or nx > 3 if intpol = 3 is required.

    ! ... x

    !     a real (kind=8) nx vector of strictly increasing values which defines the x
    !     grid on which p is given.


    ! ... p

    !     a real (kind=8) nx vector of values given on the x grid

    ! ... mx

    !     the integer dimension of the grid vector xx and the dimension of q.
    !     mx > 0 is required.

    ! ... xx

    !     a real (kind=8) mx vector of increasing values which defines the
    !     grid on which q is defined.  xx(1) < x(1) or xx(mx) > x(nx)
    !     is not allowed (see ier = 3)

    ! ... intpol

    !     an integer which sets linear or cubic
    !     interpolation as follows:

    !        intpol = 1 sets linear interpolation
    !        intpol = 3 sets cubic interpolation

    !     values other than 1 or 3 in intpol are not allowed (ier = 6).

    ! ... w

    !     a real (kind=8) work space of length at least lw which must be provided in the
    !     routine calling rgrd1


    ! ... lw

    !     the integer length of the real work space w.  let

    !          lwmin =   mx            if intpol(1) = 1
    !          lwmin = 4*mx            if intpol(1) = 3

    !     then lw must be greater than or equal to lwmin

    ! ... iw

    !     an integer work space of length at least liw which must be provided in the
    !     routine calling rgrd1

    ! ... liw

    !     tne length of the integer work space iw. liw must be greater than or equal to mx.


    ! *** output arguments


    ! ... q

    !     a real (kind=8) mx vector of values on the xx grid which are
    !     interpolated from p on the x grid

    ! ... ier

    !     an integer error flag set as follows:

    !     ier = 0 if no errors in input arguments are detected

    !     ier = 1 if  mx < 1

    !     ier = 2 if nx < 2 when intpol=1 or nx < 4 when intpol=3

    !     ier = 3 if xx(1) < x(1) or x(nx) < xx(mx)

    ! *** to avoid this flag when end points are intended to be the
    !     same but may differ slightly due to roundoff error, they
    !     should be set exactly in the calling routine (e.g., if both
    !     grids have the same x boundaries then xx(1)=x(1) and xx(mx)=x(nx)
    !     should be set before calling rgrd1)

    !     ier = 4 if the x grid is not strictly monotonically increasing
    !             or if the xx grid is not montonically increasing.  more
    !             precisely if:

    !             x(i+1) <= x(i) for some i such that 1 <= i < nx (or)

    !             xx(ii+1) < xx(ii) for some ii such that 1 <= ii < mx

    !     ier = 5 if lw or liw is too small (insufficient work space)

    !     ier = 6 if intpol is not equal to 1 or 3

    ! ************************************************************************

    !     end of rgrd1 documentation, fortran code follows:

    ! ************************************************************************

    SUBROUTINE rgrd1(nx,x,p,mx,xx,q,intpol,w,lw,iw,liw,ier)
    !     dimension x(nx),p(nx),xx(mx),q(mx),w(lw)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    real (kind=8), INTENT(IN)                :: x(*)
    real (kind=8), INTENT(IN)                :: p(*)
    INTEGER, INTENT(IN)                      :: mx
    real (kind=8), INTENT(IN)                :: xx(*)
    real (kind=8), INTENT(OUT)               :: q(*)
    INTEGER, INTENT(IN)                      :: intpol
    real (kind=8), INTENT(INOUT)             :: w(*)
    INTEGER, INTENT(IN)                      :: lw
    INTEGER, INTENT(IN OUT)                  :: iw(*)
    INTEGER, INTENT(IN)                      :: liw
    INTEGER, INTENT(OUT)                     :: ier


    INTEGER :: i,ii,i1,i2,i3,i4

    !     check arguments for errors

    ier = 1

    !     check xx grid resolution

    IF (mx < 1) RETURN

    !     check intpol

    ier = 6
    IF (intpol /= 1 .AND. intpol /= 3) RETURN

    !     check x grid resolution

    ier = 2
    IF (intpol == 1 .AND. nx < 2) RETURN
    IF (intpol == 3 .AND. nx < 4) RETURN

    !     check xx grid contained in x grid

    ier = 3
    IF (xx(1) < x(1) .OR. xx(mx) > x(nx)) RETURN

    !     check montonicity of grids

    DO i=2,nx
      IF (x(i-1) >= x(i)) THEN
        ier = 4
        RETURN
      END IF
    END DO
    DO ii=2,mx
      IF (xx(ii-1) > xx(ii)) THEN
        ier = 4
        RETURN
      END IF
    END DO

    !     check minimum work space lengths

    IF (intpol == 1) THEN
      IF (lw < mx) RETURN
    ELSE
      IF (lw < 4*mx) RETURN
    END IF
    IF (liw < mx) RETURN

    !     arguments o.k.

    ier = 0

    IF (intpol == 1) THEN
      
    !     linear interpolation in x
      
      i1 = 1
      i2 = i1+mx
      CALL linmx(nx,x,mx,xx,iw,w)
      CALL lint1(nx,p,mx,q,iw,w)
      RETURN
    ELSE
      
    !     cubic interpolation in x
      
      i1 = 1
      i2 = i1+mx
      i3 = i2+mx
      i4 = i3+mx
      CALL cubnmx(nx,x,mx,xx,iw,w(i1),w(i2),w(i3),w(i4))
      CALL cubt1(nx,p,mx,q,iw,w(i1),w(i2),w(i3),w(i4))
      RETURN
    END IF
    END SUBROUTINE rgrd1

    SUBROUTINE lint1(nx,p,mx,q,ix,dx)
    !     dimension p(nx),q(mx),ix(mx),dx(mx)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    real (kind=8), INTENT(IN)                :: p(nx)
    INTEGER, INTENT(IN)                      :: mx
    real (kind=8), INTENT(OUT)               :: q(mx)
    INTEGER, INTENT(IN)                      :: ix(mx)
    real (kind=8), INTENT(IN)                :: dx(mx)
    INTEGER :: ii,i


    !     linearly interpolate p on x onto q on xx

    DO ii=1,mx
      i = ix(ii)
      q(ii) = p(i)+dx(ii)*(p(i+1)-p(i))
    END DO
    RETURN
    END SUBROUTINE lint1

    SUBROUTINE cubt1(nx,p,mx,q,ix,dxm,dx,dxp,dxpp)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    real (kind=8), INTENT(IN)                :: p(nx)
    INTEGER, INTENT(IN)                      :: mx
    real (kind=8), INTENT(OUT)               :: q(mx)
    INTEGER, INTENT(IN)                      :: ix(mx)
    real (kind=8), INTENT(IN)                :: dxm(mx)
    real (kind=8), INTENT(IN)                :: dx(mx)
    real (kind=8), INTENT(IN)                :: dxp(mx)
    real (kind=8), INTENT(IN)                :: dxpp(mx)
    INTEGER :: i,ii


    !     cubically interpolate p on x to q on xx

    DO ii=1,mx
      i = ix(ii)
      q(ii) = dxm(ii)*p(i-1)+dx(ii)*p(i)+dxp(ii)*p(i+1)+dxpp(ii)*p(i+2)
    END DO
    RETURN
    END SUBROUTINE cubt1

    SUBROUTINE linmx(nx,x,mx,xx,ix,dx)

    !     set x grid pointers for xx grid and interpolation scale terms

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    real (kind=8), INTENT(IN)                :: x(*)
    INTEGER, INTENT(IN)                      :: mx
    real (kind=8), INTENT(IN)                :: xx(*)
    INTEGER, INTENT(OUT)                     :: ix(*)
    real (kind=8), INTENT(OUT)               :: dx(*)

    INTEGER :: isrt,ii,i

    isrt = 1
    DO ii=1,mx
      
    !     find x(i) s.t. x(i) < xx(ii) <= x(i+1)
      
      DO i=isrt,nx-1
        IF (x(i+1) >= xx(ii)) THEN
          isrt = i
          ix(ii) = i
          GO TO 3
        END IF
      END DO
      3   CONTINUE
    END DO

    !     set linear scale term

    DO ii=1,mx
      i = ix(ii)
      dx(ii) = (xx(ii)-x(i))/(x(i+1)-x(i))
    END DO
    RETURN
    END SUBROUTINE linmx

    SUBROUTINE cubnmx(nx,x,mx,xx,ix,dxm,dx,dxp,dxpp)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    real (kind=8), INTENT(IN)                :: x(*)
    INTEGER, INTENT(IN)                      :: mx
    real (kind=8), INTENT(IN)                :: xx(*)
    INTEGER, INTENT(OUT)                     :: ix(*)
    real (kind=8), INTENT(OUT)               :: dxm(*)
    real (kind=8), INTENT(OUT)               :: dx(*)
    real (kind=8), INTENT(OUT)               :: dxp(*)
    real (kind=8), INTENT(OUT)               :: dxpp(*)

    INTEGER :: i,ii,isrt

    isrt = 1
    DO ii=1,mx
      
    !     set i in [2,nx-2] closest s.t.
    !     x(i-1),x(i),x(i+1),x(i+2) can interpolate xx(ii)
      
      DO i=isrt,nx-1
        IF (x(i+1) >= xx(ii)) THEN
          ix(ii) = MIN0(nx-2,MAX0(2,i))
          isrt = ix(ii)
          GO TO 3
        END IF
      END DO
      3   CONTINUE
    END DO

    !     set cubic scale terms

    DO ii=1,mx
      i = ix(ii)
      dxm(ii) = (xx(ii)-x(i))*(xx(ii)-x(i+1))*(xx(ii)-x(i+2))/  &
          ((x(i-1)-x(i))*(x(i-1)-x(i+1))*(x(i-1)-x(i+2)))
      dx(ii) = (xx(ii)-x(i-1))*(xx(ii)-x(i+1))*(xx(ii)-x(i+2))/  &
          ((x(i)-x(i-1))*(x(i)-x(i+1))*(x(i)-x(i+2)))
      dxp(ii) = (xx(ii)-x(i-1))*(xx(ii)-x(i))*(xx(ii)-x(i+2))/  &
          ((x(i+1)-x(i-1))*(x(i+1)-x(i))*(x(i+1)-x(i+2)))
      dxpp(ii) = (xx(ii)-x(i-1))*(xx(ii)-x(i))*(xx(ii)-x(i+1))/  &
          ((x(i+2)-x(i-1))*(x(i+2)-x(i))*(x(i+2)-x(i+1)))
    END DO
    RETURN
    END SUBROUTINE cubnmx
end module
