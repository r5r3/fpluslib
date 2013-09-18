module mod_rgrd2u
    use mod_rgrd1u
    implicit none
    
contains
    !

    ! Code converted using TO_F90 by Alan Miller
    ! Date: 2013-09-18  Time: 12:23:04

    ! ... file rgrd2u.f

    !     this file contains documentation followed by fortran code for
    !     subroutine rgrd2u and additional subroutines.

    ! ... author

    !     John C. Adams (NCAR 1999)

    ! ... subroutine rgrd2u(nx,ny,p,mx,my,q,intpol,w,lw,iw,liw,ier)

    ! ... purpose

    !     subroutine rgrd2u interpolates the nx by ny array p onto
    !     the mx by my array q.  linear or cubic interpolation is
    !     used in each direction (see aargumentintpol).  it is assumed
    !     that p and q are values on uniform nx by ny and mx by my grids
    !     superimposed on the same rectangle (INCLUDING BOUNDARIES).
    !     if p and q are values on nonuniform orthogonal grids and/or
    !     if the grid on which q is defined lies within the p grid
    !     then subroutine rgrd2 (see file rgrd2.f) should be used.

    ! ... language

    !     coded in portable FORTRAN77 and FORTRAN90

    ! ... test program

    !     file trgrd2u.f on regridpack includes a test program for subroutine rgrd2u


    ! ... method

    !     linear or cubic interpolation (see intpol) is used in each
    !     direction for which the q grid is not a subgrid of the p grid.
    !     [the mx (my) uniform grid is a subgrid of the nx (ny) uniform
    !     grid if and only if mx-1 (my-1) divides nx-1 (ny-1)].
    !     values are set directly without (the need for) interpolation
    !     in subgrid directions.

    ! ... required files

    !     file rgrd1u.f must be loaded with rgrd2u.f.  it includes
    !     subroutines called by the routines in rgrd2u.f

    ! ... efficiency

    !     inner most loops in regridpack software vectorize.  If
    !     the arguments mx,my (see below) have different values, optimal
    !     vectorization will be achieved if mx > my.


    ! *** input arguments ***


    ! ... nx

    !     the integer first dimension of p.  nx > 1 if intpol(1) = 1 or
    !     nx > 3 if intpol(1) = 3 is required (see ier = 2).

    ! ... ny

    !     the integer second dimension of p.  ny > 1 if intpol(2) = 1 or
    !     ny > 3 if intpol(2) = 3 is required (see ier = 2).

    ! ... p

    !     a real (kind=8) nx by ny array of given values

    ! ... mx

    !     the integer first dimension of q.  mx > 1 is required (see ier = 1)

    ! ... my

    !     the integer second dimension of q. my > 1 is required (see ier = 1)

    ! ... intpol

    !     an integer vector of dimension 2 which sets linear or cubic
    !     interpolation in each of the x,y directions as follows:

    !        intpol(1) = 1 sets linear interpolation in the x direction
    !        intpol(1) = 3 sets cubic interpolation in the x direction.

    !        intpol(2) = 1 sets linear interpolation in the y direction
    !        intpol(2) = 3 sets cubic interpolation in the y direction.

    !     values other than 1 or 3 in intpol are not allowed (ier = 3).

    ! ... w

    !     a real (kind=8) work space of length at least lw which must be provided in the
    !     routine calling rgrd2u

    ! ... lw

    !     the integer length of the work space w.

    !          let lwx = 1 if mx-1 divides nx-1; otherwise
    !          let lwx =   mx if intpol(1) = 1 or
    !          let lwx = 4*mx if intpol(1) = 3

    !          let lwy = 0 if my-1 divides ny-1; otherwise
    !          let lwy = 2*mx+my if intpol(2) = 1 or
    !          let lwy = 4*(mx+my)  if intpol(2) = 3

    !     then lw must be greater than or equal to lwx+lwy

    ! ... iw

    !     an integer work space of length at least liw which must be provided in the
    !     routine calling rgrd2u

    ! ... liw

    !     the integer length of the integer work space iw.  liw must be greater than
    !     or equal to mx+my.

    ! *** output arguments ***


    ! ... q

    !     a real (kind=8) mx by my array of values which are interpolated from p.

    ! ... ier

    !     an integer error flag set as follows:

    !     ier = 0 if no errors in input arguments are detected

    !     ier = 1 if  min0(mx,my) < 2

    !     ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
    !                ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3.

    !     ier = 3 if intpol(1) or intpol(2) is not equal to 1 or 3

    !     ier = 4 if lw or liw is to small (insufficient work space)

    ! ************************************************************************

    !     end of rgrd2u documentation, fortran code follows:

    ! ************************************************************************

    SUBROUTINE rgrd2u(nx,ny,p,mx,my,q,intpol,w,lw,iw,liw,ier)

    !     two dimensional linear or cubic interpolation of p onto q
    !     assuming p and q lie on nx by ny and and mx by my uniform  grids
    !     which subdivide the same rectangle (including boundaries)


    IMPLICIT NONE
    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: ny
    real (kind=8), INTENT(IN)                :: p(nx,ny)
    INTEGER, INTENT(IN)                      :: mx
    INTEGER, INTENT(IN)                      :: my
    real (kind=8), INTENT(OUT)               :: q(mx,my)
    INTEGER, INTENT(IN)                      :: intpol(2)
    real (kind=8), INTENT(INOUT)             :: w(lw)
    INTEGER, INTENT(IN)                      :: lw
    INTEGER, INTENT(INOUT)                   :: iw(liw)
    INTEGER, INTENT(IN)                      :: liw
    INTEGER, INTENT(OUT)                     :: ier


    INTEGER :: inmx,jnmy,isubx,jsuby,lwx,lwy,jy
    INTEGER :: j2,j3,j4,j5,j6,j7,j8,j9,i2,i3,i4,i5

    !     check input aarguments

    ier = 1

    !     check mx,my

    IF (MIN0(mx,my) < 2) RETURN

    !     check intpol

    ier = 3
    IF (intpol(1) /= 1 .AND. intpol(1) /= 3) RETURN
    IF (intpol(2) /= 1 .AND. intpol(2) /= 3) RETURN

    !     check nx,ny

    ier = 2
    IF (intpol(1) == 1 .AND. nx < 2) RETURN
    IF (intpol(1) == 3 .AND. nx < 4) RETURN
    IF (intpol(2) == 1 .AND. ny < 2) RETURN
    IF (intpol(2) == 3 .AND. ny < 4) RETURN

    !     set subgrid indicators

    inmx = (nx-1)/(mx-1)
    jnmy = (ny-1)/(my-1)
    isubx = nx - inmx*(mx-1)
    jsuby = ny - jnmy*(my-1)

    !     check work space length input

    ier = 4
    lwx = 1
    lwy = 0
    IF (isubx /= 1) THEN
      IF (intpol(1) == 1) THEN
        lwx = mx
      ELSE
        lwx = mx
      END IF
    END IF
    IF (jsuby /= 1) THEN
      IF (intpol(2) == 1) THEN
        lwy = (my+2*mx)
      ELSE
        lwy = 4*(mx+my)
      END IF
    END IF
    IF (lw < lwx+lwy) RETURN
    IF (liw < mx+my) RETURN

    !     input arguments o.k.

    ier = 0
    jy = mx+1

    !     preset work space pointers

    j2 = 1
    j3 = j2
    j4 = j3
    j5 = j4
    j6 = j5
    j7 = j6
    j8 = j7
    j9 = j8
    i2 = j9
    i3 = i2
    i4 = i3
    i5 = i4

    IF (intpol(2) == 1) THEN
      
    !     linearly interpolate in y
      
      IF (jsuby /= 1) THEN
        j2 = 1
        j3 = j2
        j4 = j3+my
        j5 = j4
        j6 = j5
        j7 = j6
        j8 = j7+mx
        j9 = j8+mx
        
    !     set y interpolation indices and scales and linearly interpolate
        
        CALL linmxu(ny,my,iw(jy),w(j3))
        i2 = j9
      END IF
      
    !     set work space portion and indices which depend on x interpolation
      
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
      CALL lint2u(nx,ny,p,mx,my,q,intpol,iw(jy),w(j3),w(j7),  &
          w(j8),iw,w(i2),w(i3),w(i4),w(i5),inmx,jnmy,isubx,jsuby)
      RETURN
      
    ELSE
      
    !     cubically interpolate in y, set indice pointers
      
      IF (jsuby /= 1) THEN
        j2 = 1
        j3 = j2+my
        j4 = j3+my
        j5 = j4+my
        j6 = j5+my
        j7 = j6+mx
        j8 = j7+mx
        j9 = j8+mx
        
    !     set y interpolation indices and scales and cubically interpolate in y
        
        CALL cubnmxu(ny,my,iw(jy),w(j2),w(j3),w(j4),w(j5))
        i2 =  j9+mx
      END IF
      
    !     set work space portion and indices which depend on x interpolation
      
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
      CALL cubt2u(nx,ny,p,mx,my,q,intpol,iw(jy),w(j2),w(j3),w(j4),  &
          w(j5),w(j6),w(j7),w(j8),w(j9),iw,w(i2),w(i3),w(i4),w(i5),  &
          inmx,jnmy,isubx,jsuby)
      RETURN
    END IF
    END SUBROUTINE rgrd2u

    SUBROUTINE lint2u(nx,ny,p,mx,my,q,intpol,jy,dy,pj,pjp,ix,dxm,dx,  &
        dxp,dxpp,inmx,jnmy,isubx,jsuby)

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
    INTEGER, INTENT(IN OUT)                  :: inmx
    INTEGER, INTENT(IN)                      :: jnmy
    INTEGER, INTENT(IN OUT)                  :: isubx
    INTEGER, INTENT(IN)                      :: jsuby

    INTEGER :: j,jj,ii,jsave




    !     linearly interpolate p onto q in y

    IF (intpol(1) == 1) THEN
      
    !     linear in x
      
      IF (jsuby == 1) THEN
        
    !     my grid is subset of ny grid
        
        DO jj=1,my
          j = jnmy*(jj-1)+1
          CALL lint1u(nx,p(1,j),mx,q(1,jj),ix,dx,inmx,isubx)
        END DO
        RETURN
      END IF
      
      jsave = -1
      DO jj=1,my
        j = jy(jj)
        IF (j == jsave) THEN
          
    !     pointer has not moved, no interpolation in pj,pjp necessary
          
        ELSE IF (j == jsave+1) THEN
          DO ii=1,mx
            pj(ii) = pjp(ii)
          END DO
          CALL lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
        ELSE
          CALL lint1u(nx,p(1,j),mx,pj,ix,dx,inmx,isubx)
          CALL lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
        END IF
        
    !     update pointer
        
        jsave = j
        DO ii=1,mx
          q(ii,jj) = pj(ii)+dy(jj)*(pjp(ii)-pj(ii))
        END DO
      END DO
      
      RETURN
      
    ELSE
      
    !     cubic in x
      
      IF (jsuby == 1) THEN
        
    !     my grid is subset of ny grid
        
        DO jj=1,my
          j = jnmy*(jj-1)+1
          CALL cubt1u(nx,p(1,j),mx,q(1,jj),ix,dxm,dx,dxp,dxpp, inmx,isubx)
        END DO
        RETURN
      END IF
      
      jsave = -1
      DO jj=1,my
        j = jy(jj)
        IF (j == jsave) THEN
          
    !     no interpolation in pj,pjp necessary
          
        ELSE IF (j == jsave+1) THEN
          DO ii=1,mx
            pj(ii) = pjp(ii)
          END DO
          CALL cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp, inmx,isubx)
        ELSE
          CALL cubt1u(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp, inmx,isubx)
          CALL cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp, inmx,isubx)
        END IF
        
    !     update  pointer
        
        jsave = j
        DO ii=1,mx
          q(ii,jj) = pj(ii)+dy(jj)*(pjp(ii)-pj(ii))
        END DO
      END DO
      RETURN
      
    END IF

    END SUBROUTINE lint2u

    SUBROUTINE cubt2u(nx,ny,p,mx,my,q,intpol,jy,dym,dy,dyp,dypp,  &
        pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)

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
    real (kind=8), INTENT(IN OUT)            :: dyp(my)
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
    INTEGER, INTENT(IN OUT)                  :: inmx
    INTEGER, INTENT(IN)                      :: jnmy
    INTEGER, INTENT(IN OUT)                  :: isubx
    INTEGER, INTENT(IN)                      :: jsuby

    INTEGER :: j,jj,ii,jsave





    !     cubically interpolate p onto q in y

    IF (intpol(1) == 1) THEN
      
    !     linear in x
      
      IF (jsuby == 1) THEN
        
    !     my grid is subset of ny grid
        
        DO jj=1,my
          j = jnmy*(jj-1)+1
          CALL lint1u(nx,p(1,j),mx,q(1,jj),ix,dx,inmx,isubx)
        END DO
        RETURN
      END IF
      
      jsave = -3
      DO jj=1,my
        j = jy(jj)
        
    !     load pjm,pj,pjp,pjpp
        
        IF (j == jsave) THEN
    !     no updates or x interpolation necessary
        ELSE IF (j == jsave+1) THEN
          DO ii=1,mx
            pjm(ii) = pj(ii)
            pj(ii) = pjp(ii)
            pjp(ii) = pjpp(ii)
          END DO
          CALL lint1u(nx,p(1,j+2),mx,pjpp,ix,dx,inmx,isubx)
        ELSE IF (j == jsave+2) THEN
          DO ii=1,mx
            pjm(ii) = pjp(ii)
            pj(ii) = pjpp(ii)
          END DO
          CALL lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
          CALL lint1u(nx,p(1,j+2),mx,pjpp,ix,dx,inmx,isubx)
        ELSE IF (j == jsave+3) THEN
          DO ii=1,mx
            pjm(ii) = pjpp(ii)
          END DO
          CALL lint1u(nx,p(1,j),mx,pj,ix,dx,inmx,isubx)
          CALL lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
          CALL lint1u(nx,p(1,j+2),mx,pjpp,ix,dx,inmx,isubx)
        ELSE
    !     load all four (no updates)
          CALL lint1u(nx,p(1,j-1),mx,pjm,ix,dx,inmx,isubx)
          CALL lint1u(nx,p(1,j),mx,pj,ix,dx,inmx,isubx)
          CALL lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
          CALL lint1u(nx,p(1,j+2),mx,pjpp,ix,dx,inmx,isubx)
        END IF
        
    !     update pointer
        
        jsave = j
        DO ii=1,mx
          q(ii,jj) = dym(jj)*pjm(ii) + dy(jj)*pj(ii) +  &
              dyp(jj)*pjp(ii) + dypp(jj)*pjpp(ii)
        END DO
      END DO
      RETURN
      
    ELSE
      
    !     cubic in x
      
      
      IF (jsuby == 1) THEN
        
    !     my grid is subset of ny grid
        
        DO jj=1,my
          j = jnmy*(jj-1)+1
          CALL cubt1u(nx,p(1,j),mx,q(1,jj),ix,dxm,dx,dxp,dxpp, inmx,isubx)
        END DO
        RETURN
      END IF
      
      jsave = -3
      DO jj=1,my
        j = jy(jj)
        
    !     load pjm,pj,pjp,pjpp
        
        IF (j == jsave) THEN
    !     no updates or x interpolation necessary
        ELSE IF (j == jsave+1) THEN
          DO ii=1,mx
            pjm(ii) = pj(ii)
            pj(ii) = pjp(ii)
            pjp(ii) = pjpp(ii)
          END DO
          CALL cubt1u(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp, inmx,isubx)
        ELSE IF (j == jsave+2) THEN
          DO ii=1,mx
            pjm(ii) = pjp(ii)
            pj(ii) = pjpp(ii)
          END DO
          CALL cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp, inmx,isubx)
          CALL cubt1u(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp, inmx,isubx)
        ELSE IF (j == jsave+3) THEN
          DO ii=1,mx
            pjm(ii) = pjpp(ii)
          END DO
          CALL cubt1u(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp, inmx,isubx)
          CALL cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp, inmx,isubx)
          CALL cubt1u(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp, inmx,isubx)
        ELSE
    !     load all four (no updates)
          CALL cubt1u(nx,p(1,j-1),mx,pjm,ix,dxm,dx,dxp,dxpp, inmx,isubx)
          CALL cubt1u(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp, inmx,isubx)
          CALL cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp, inmx,isubx)
          CALL cubt1u(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp, inmx,isubx)
        END IF
        
    !     update pointer
        
        jsave = j
        DO ii=1,mx
          q(ii,jj) = dym(jj)*pjm(ii) + dy(jj)*pj(ii) +  &
              dyp(jj)*pjp(ii) + dypp(jj)*pjpp(ii)
        END DO
      END DO
      RETURN
      
    END IF

    END SUBROUTINE cubt2u
end module
