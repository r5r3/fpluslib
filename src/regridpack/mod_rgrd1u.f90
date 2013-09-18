module mod_rgrd1u
    implicit none
    
contains
    !

    ! Code converted using TO_F90 by Alan Miller
    ! Date: 2013-09-18  Time: 12:22:50

    ! ... file rgrd1u.f

    !     this file contains documentation followed by fortran code for
    !     subroutine rgrd1u and additional subroutines.

    ! ... author

    !     John C. Adams (NCAR 1999)

    ! ... subroutine rgrd1u(nx,p,mx,q,intpol,w,lw,iw,liw,ier)

    ! ... purpose

    !     subroutine rgrd1u interpolates the nx vector p onto
    !     the mx vector q. it is assumed that p and q are
    !     values on uniform nx and mx grids which subdivide
    !     the same interval (INCLUDING END POINTS).  if p and
    !     q are values on nonuniform grids and/or if q is defined
    !     on a grid which lies within the p grid then subroutine
    !     rgrd1 (see file rgrd1.f) should be used.

    ! ... language

    !     coded in portable FORTRAN77 and FORTRAN90

    ! ... test program

    !     file trgrd1u.f on regridpack includes a test program for subroutine rgrd1u

    ! ... method

    !     linear or cubic interpolation (see intpol) is used when the
    !     mx uniform grid is not a subgrid of the nx uniform grid (i.e.,
    !     whenever mx-1 does not divide nx-1).  q is set directly from
    !     p in the subgrid case.

    ! ... required files

    !     none

    ! ... efficiency

    !     inner most loops in regridpack software vectorize.


    ! *** input arguments ***


    ! ... nx

    !     the integer dimension of p.  nx > 1 if intpol = 1 or
    !     nx > 3 if intpol = 3 is required (see ier = 2).

    ! ... p

    !     a real (kind=8) nx dimensioned vector of given values

    ! ... mx

    !     the integer dimension of q.  mx > 1 is required (see ier = 1)


    ! ... intpol

    !     an integer which sets linear or cubic interpolation as follows:

    !        intpol = 1 sets linear interpolation
    !        intpol = 3 sets cubic interpolation

    !     values other than 1 or 3 in intpol are not allowed (ier = 4).

    ! ... w

    !     a real (kind=8) work space of length lw.

    ! ... lw

    !     the integer length of the work space w in the routine calling rgrd1u.
    !     if mx-1 divides nx-1 then the mx uniform grid is a subgrid of
    !     the nx uniform grid.  in this case let lwmin = 1.  otherwise
    !     let lwmin = mx if intpol = 1 or lwmin = mx if intpol = 3.
    !     then lw must be greater than or equal to lwmin (see ier=4).

    ! ... iw

    !     an integer work space of length liw

    ! ... liw

    !     the integer length of the integer work space iw in the routine calling rgrd1u.
    !     liw must be greater than or equal to mx.


    ! *** output arguments ***


    ! ... q

    !     a real (kind=8) mx dimensioned vector of values which are interpolated from p.

    ! ... ier

    !     an integer error flag set as follows:

    !     ier = 0 if no errors in input arguments are detected

    !     ier = 1 if  mx < 2

    !     ier = 2 if nx < 2 when intpol=1 or nx < 4 when intpol=3.

    !     ier = 3 if intpol is not equal to 1 or 3

    !     ier = 4 if lw or liw is too small (insufficient work space)


    ! ************************************************************************

    !     end of rgrd1u documentation, fortran code follows:

    ! ************************************************************************


    SUBROUTINE rgrd1u(nx,p,mx,q,intpol,w,lw,iw,liw,ier)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    real (kind=8), INTENT(IN)                :: p(nx)
    INTEGER, INTENT(IN)                      :: mx
    real (kind=8), INTENT(OUT)               :: q(mx)
    INTEGER, INTENT(IN)                      :: intpol
    real (kind=8), INTENT(INOUT)             :: w(lw)
    INTEGER, INTENT(IN)                      :: lw
    INTEGER, INTENT(INOUT)                   :: iw(liw)
    INTEGER, INTENT(IN)                      :: liw
    INTEGER, INTENT(OUT)                     :: ier
    INTEGER :: inmx,isubx
    INTEGER :: i2,i3,i4,i5,lwmin


    !     check input arguments

    ier = 1

    !     check mx

    IF (mx < 2) RETURN

    !     check intpol

    ier = 3
    IF (intpol /= 1 .AND. intpol /= 3) RETURN

    !     check nx

    ier = 2
    IF (intpol == 1 .AND. nx < 2) RETURN
    IF (intpol == 3 .AND. nx < 4) RETURN

    !     set subgrid integer indicator

    inmx = (nx-1)/(mx-1)
    isubx = nx - inmx*(mx-1)

    !     set minimum and check work space

    ier = 4
    IF (isubx /= 1) THEN
      IF (intpol == 1) lwmin = mx
      IF (intpol == 3) lwmin = 4*mx
    ELSE
      lwmin = 1
    END IF
    IF (lw < lwmin) RETURN
    IF (liw < mx) RETURN

    !     input arguments o.k.

    ier = 0

    !     preset pointers

    i2 = 1
    i3 = 1
    i4 = 1
    i5 = 1
    IF (intpol == 1) THEN
      
    !     linear interpolation in x
      
      IF (isubx /= 1) THEN
        CALL linmxu(nx,mx,iw,w)
      END IF
      CALL lint1u(nx,p,mx,q,iw,w,inmx,isubx)
      RETURN
    ELSE
      
    !     cubic interpolation in x
      
      IF (isubx /= 1) THEN
        i2 = 1
        i3 = i2+mx
        i4 = i3+mx
        i5 = i4+mx
        CALL cubnmxu(nx,mx,iw,w(i2),w(i3),w(i4),w(i5))
      END IF
      CALL cubt1u(nx,p,mx,q,iw,w(i2),w(i3),w(i4),w(i5),inmx,isubx)
      RETURN
    END IF
    END SUBROUTINE rgrd1u

    SUBROUTINE lint1u(nx,p,mx,q,ix,dx,inmx,isubx)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    real (kind=8), INTENT(IN)                :: p(nx)
    INTEGER, INTENT(IN)                      :: mx
    real (kind=8), INTENT(OUT)               :: q(mx)
    INTEGER, INTENT(IN)                      :: ix(mx)
    real (kind=8), INTENT(IN)                :: dx(mx)
    INTEGER, INTENT(IN)                      :: inmx
    INTEGER, INTENT(IN)                      :: isubx
    INTEGER :: i,ii


    IF (isubx == 1) THEN
      
    !     mx grid is subset of nx grid so q can be set directly
      
      DO ii=1,mx
        i = inmx*(ii-1)+1
        q(ii) = p(i)
      END DO
      RETURN
    ELSE
      
    !     linearly interpolate
      
      DO ii=1,mx
        i = ix(ii)
        q(ii) = p(i)+dx(ii)*(p(i+1)-p(i))
      END DO
      RETURN
    END IF
    END SUBROUTINE lint1u

    SUBROUTINE cubt1u(nx,p,mx,q,ix,dxm,dx,dxp,dxpp,inmx,isubx)

    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    real (kind=8), INTENT(IN)                :: p(nx)
    INTEGER, INTENT(IN)                      :: mx
    real (kind=8), INTENT(OUT)               :: q(mx)
    INTEGER, INTENT(IN)                      :: ix(mx)
    real (kind=8), INTENT(IN OUT)            :: dxm(mx)
    real (kind=8), INTENT(IN)                :: dx(mx)
    real (kind=8), INTENT(IN)                :: dxp(mx)
    real (kind=8), INTENT(IN OUT)            :: dxpp(mx)
    INTEGER, INTENT(IN)                      :: inmx
    INTEGER, INTENT(IN)                      :: isubx
    INTEGER :: i,ii



    IF (isubx == 1) THEN
      
    !     mx grid is subset of nx grid so q can be set directly
      
      DO ii=1,mx
        i = inmx*(ii-1)+1
        q(ii) = p(i)
      END DO
      RETURN
      
    ELSE
      
    !     cubically interpolate on uniform grid
      
      DO ii=1,mx
        i = ix(ii)
        q(ii)=(dxm(ii)*p(i-1)+dx(ii)*p(i)+dxp(ii)*p(i+1)+ dxpp(ii)*p(i+2))
      END DO
      RETURN
    END IF
    END SUBROUTINE cubt1u

    SUBROUTINE linmxu(nx,mx,ix,dx)

    !     set linear interpolation terms


    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: mx
    INTEGER, INTENT(OUT)                     :: ix(mx)
    real (kind=8), INTENT(OUT)               :: dx(mx)
    INTEGER :: i,ii
    real (kind=8) :: dnx,dmx,x,xx

    !     set "virtual" uniform increments

    dnx = 1.0/(nx-1)
    dmx = 1.0/(mx-1)

    !     set ix(ii) = i  s.t. i,i+1 can interpolate for ii

    DO ii=1,mx
      xx = (ii-1)*dmx
      ix(ii) = MIN0(INT(xx/dnx)+1,nx-1)
      
    !     set scale term for linear
      
      i = ix(ii)
      x = (i-1)*dnx
      dx(ii) = (xx-x)/dnx
    END DO
    RETURN
    END SUBROUTINE linmxu

    SUBROUTINE cubnmxu(nx,mx,ix,dxm,dx,dxp,dxpp)

    !     set cubic interpolation terms


    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: mx
    INTEGER, INTENT(OUT)                     :: ix(mx)
    real (kind=8), INTENT(OUT)               :: dxm(mx)
    real (kind=8), INTENT(OUT)               :: dx(mx)
    real (kind=8), INTENT(OUT)               :: dxp(mx)
    real (kind=8), INTENT(OUT)               :: dxpp(mx)
    INTEGER :: i,ii
    real (kind=8) :: dnx,dmx,odnx3
    real (kind=8) :: xx,xim,xi,xip,xipp

    !     set "virtual" uniform increments

    dnx = 1.0/(nx-1)
    dmx = 1.0/(mx-1)
    odnx3 = 1.0/(6.*dnx*dnx*dnx)

    !     set i=ix(ii) in [2,nx-2] such that
    !     i-1,i,i+1,i+2 can be used to interpolate at ii

    DO ii=1,mx
      xx = (ii-1)*dmx
      ix(ii) = MIN0(MAX0(INT(xx/dnx)+1,2),nx-2)
      i = ix(ii)
      
    !     set scale terms for cubic
      
      xi = (i-1)*dnx
      xim = xi-dnx
      xip = xi+dnx
      xipp = xip+dnx
      dxm(ii) = -(xx-xi)*(xx-xip)*(xx-xipp)*odnx3
      dx(ii) = 3.*(xx-xim)*(xx-xip)*(xx-xipp)*odnx3
      dxp(ii) = -3.*(xx-xim)*(xx-xi)*(xx-xipp)*odnx3
      dxpp(ii) = (xx-xim)*(xx-xi)*(xx-xip)*odnx3
    END DO
    RETURN
    END SUBROUTINE cubnmxu
end module