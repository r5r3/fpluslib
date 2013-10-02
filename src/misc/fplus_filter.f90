module fplus_filter
    implicit none

contains

    !***********************************************************************
    !     ZEITREIHENFILTERUNG MIT REKURSIVEM FILTER NACH BUTTERWORTH       *
    !     (siehe Stearns: Digitale verarbeitung analoger Systeme, 1984)    *
    !     BEACHTE: LAENGE DER ZEITREIHE BLEIBT KONSTANT                    *
    !                                                                      *
    !     XTS         : ORIGINALZEITREIHEN U. GEF. ZR AM ENDE D. ROUTINE   *
    !     NTERMIN     : ANZAHL DER DATENPUNKTE                             *
    !     TLOW,THIGH  : Half Power Cutin/Cutoff-Perioden                   *
    !     NS          : 2*NS = Ordnung des Butterworth filters             *
    !     ZRRF,TREV    : Hilfsfelder                                       *
    !***********************************************************************
    ! aus filter.f von A. Fink. 
    SUBROUTINE BWBPASS(Xts,Ntermin,Tlow,Thigh,Ns)
        INTEGER :: i , j , k , kk , l , Ns , NT , Ntermin
        REAL (kind=8) :: Thigh , Tlow
        REAL (kind=8) :: Xts(Ntermin)
        REAL (kind=8) :: trev(Ntermin) , zrev(Ntermin) , f(6,5)
        REAL (kind=8) :: a(5) , b(5) , c(5) , d(5) , e(5) , f1 , f2 , temp
        REAL (kind=8) :: cs , p , q , r , s , deltat , wc , w1 , w2 , x , pi

        !      REAL A(5),B(5),C(5),D(5),E(5)
        !      REAL CS,P,Q,R,S,DELTAT,WC,W1,W2,X,F1,F2

        pi = ACOS(-1.)
        !PRINT * , '*** BANDPASSFILTER BUTTERWORTH ***'
        !PRINT * , 'TLOW= ' , Tlow , 'THIGH ' , Thigh
        f1 = 1./Thigh
        f2 = 1./Tlow
        deltat = 1.
 
        !***********************************************************************
        ! Berechnung der Filterkoeffizienten A(K)-E(K)                        *
        !***********************************************************************
 
        w1 = SIN(f1*pi*deltat)/COS(f1*pi*deltat)
        w2 = SIN(f2*pi*deltat)/COS(f2*pi*deltat)
        wc = w2 - w1
        q = wc*wc + 2.*w1*w2
        s = w1*w1*w2*w2
        DO k = 1 , Ns
            cs = COS(FLOAT(2*(k+Ns)-1)*pi/FLOAT(4*Ns))
            p = -2.*wc*cs
            r = p*w1*w2
            x = 1. + p + q + r + s
            a(k) = wc*wc/x
            b(k) = (-4.-2.*p+2.*r+4.*s)/x
            c(k) = (6.-2.*q+6.*s)/x
            d(k) = (-4.+2.*p-2.*r+4.*s)/x
            e(k) = (1.-p+q-r+s)/x
        ENDDO
!      PRINT * , 'Filterkoeffizienten A(K)-B(K)'
!      PRINT * , '  '
!      DO i = 1 , Ns
!         WRITE (*,99001) i , a(i) , b(i) , c(i) , d(i) , e(i)
!99001    FORMAT (5(I3,5E18.10))
!      ENDDO
 
 
        !***********************************************************************
        ! Hin/Rueckrekursion. Filter besteht aus NS Filtern in Reihe           *
        ! (Rekursion nach Stearns, S. 345)                                      *
        !***********************************************************************
         
        !***********************************************************************
        ! Initialisierung F(NS+1,5):                                           *
        ! F(1,5): present value, input to filter section 1                     *
        ! F(2,5): input to filter section 2                                    *
        !    "  :        "                                                     *
        ! F(6,5): output of filter section 5 = filtered value                  *
        ! F(1,4): previous value of F(1,5)                                     *
        ! F(2,3): previous value of F(1,4)                                     *
        !    "  :        "                                                     *
        !***********************************************************************
 
 
        DO i = 1 , Ns + 1
            DO j = 1 , 5
                f(i,j) = 0.
            ENDDO
        ENDDO
 
        DO i = 1 , Ntermin
            f(1,5) = Xts(i)
            DO j = 1 , Ns
                temp = a(j)*(f(j,5)-2.*f(j,3)+f(j,1))
                f(j+1,5) = temp - b(j)*f(j+1,4) - c(j)*f(j+1,3) - d(j) *f(j+1,2) - e(j)*f(j+1,1)
            ENDDO
            DO j = 1 , Ns + 1
                DO kk = 1 , 4
                    f(j,kk) = f(j,kk+1)
                ENDDO
            ENDDO
            zrev(i) = f(Ns+1,5)
        ENDDO

        !***********************************************************************
        ! Zeitumkehr und Filterung des Outputs                                 *
        !***********************************************************************
 
        DO l = 1 , Ntermin
            trev(Ntermin-l+1) = zrev(l)
        ENDDO
 
        DO i = 1 , Ns + 1
            DO j = 1 , 5
                f(i,j) = 0.
            ENDDO
        ENDDO
 
        DO i = 1 , Ntermin
            f(1,5) = trev(i)
            DO j = 1 , Ns
                temp = a(j)*(f(j,5)-2.*f(j,3)+f(j,1))
                f(j+1,5) = temp - b(j)*f(j+1,4) - c(j)*f(j+1,3) - d(j)      &
                         & *f(j+1,2) - e(j)*f(j+1,1)
            ENDDO
            DO j = 1 , Ns + 1
                DO kk = 1 , 4
                   f(j,kk) = f(j,kk+1)
                ENDDO
            ENDDO
            trev(i) = f(Ns+1,5)
        ENDDO
 
        !***********************************************************************
        ! Erneute Zeitumkehr, abspeichern des gefilterten Sihnales             *
        !***********************************************************************
 
 
        DO l = 1 , Ntermin
           Xts(Ntermin-l+1) = trev(l)
        ENDDO
    END subroutine

end module
