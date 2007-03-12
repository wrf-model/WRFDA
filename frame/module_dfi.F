MODULE module_dfi

      USE module_domain
      USE module_configure
      USE module_wrf_error

      implicit none

      real, allocatable,dimension(:),public :: H

      real, private :: da_dot

CONTAINS

 SUBROUTINE jcdfi_init_coef ( grid , config_flags )
 
  IMPLICIT NONE
 
  !  Input data.

  TYPE(domain) , intent(in) :: grid
  TYPE (grid_config_rec_type), intent(in) :: config_flags
  integer :: nsteps
 
   if (mod ( INT(config_flags%interval_seconds / config_flags%dt) , 2 ).ne.0) &
      CALL wrf_error_fatal( 'Total # of time steps should be EVEN' )

   nsteps = config_flags%interval_seconds / config_flags%dt / 2 

   CALL JCDFI_COEF ( 0 , NSTEPS , config_flags%dt , grid%jcdfi_tauc , 7 )

 END SUBROUTINE jcdfi_init_coef
!
!
!
 SUBROUTINE jcdfi_zero_ad ( grid )

  IMPLICIT NONE

  !  Input data.

  TYPE(domain) , intent(inout) :: grid
 
   grid%em_a_u_2  = 0.0
   grid%em_a_v_2  = 0.0
   grid%em_a_t_2  = 0.0
   grid%em_a_mu_2 = 0.0

END SUBROUTINE jcdfi_zero_ad
!
!
!
 SUBROUTINE jcdfi_tl ( grid ,i)

  IMPLICIT NONE

  !  Input data.

  TYPE(domain) , intent(inout) :: grid
 
  integer, intent(in) :: i
  
   grid%em_a_u_2  = grid%em_a_u_2  + h(i)*grid%em_g_u_2
   grid%em_a_v_2  = grid%em_a_v_2  + h(i)*grid%em_g_v_2
   grid%em_a_t_2  = grid%em_a_t_2  + h(i)*grid%em_g_t_2
   grid%em_a_mu_2 = grid%em_a_mu_2 + h(i)*grid%em_g_mu_2

END SUBROUTINE jcdfi_tl
!
!
! 
 SUBROUTINE jcdfi_output_forcing ( grid , config_flags )

  IMPLICIT NONE

  TYPE ( domain ) , intent(inout) :: grid
  type(grid_config_rec_type) , intent(in) :: config_flags
  real :: jc

   grid%em_g_u_1  = grid%em_g_u_2
   grid%em_g_u_2  = 1.0/grid%jcdfi_error_wind * grid%em_a_u_2

   grid%em_g_v_1  = grid%em_g_v_2
   grid%em_g_v_2  = 1.0/grid%jcdfi_error_wind * grid%em_a_v_2

   grid%em_g_t_1  = grid%em_g_t_2
   grid%em_g_t_2  = 1.0/grid%jcdfi_error_t * grid%em_a_t_2

   grid%em_g_mu_1 = grid%em_g_mu_2
   grid%em_g_mu_2 = 1.0/grid%jcdfi_error_mu * grid%em_a_mu_2


   call med_hist_out ( grid , 3, config_flags )

   grid%em_g_u_2  = grid%em_g_u_1
   grid%em_g_v_2  = grid%em_g_v_1
   grid%em_g_t_2  = grid%em_g_t_1
   grid%em_g_mu_2 = grid%em_g_mu_1

 END SUBROUTINE jcdfi_output_forcing
!
!
!
 SUBROUTINE jcdfi_add_forcing ( grid ,i )

  IMPLICIT NONE

  TYPE ( domain ) , intent(inout) :: grid
  integer , intent(in) :: i
  integer :: jcdfi_flag

   jcdfi_flag = 0
   if ( grid%jcdfi_onoff ) jcdfi_flag = 1
   grid%em_a_u_2  = grid%em_a_u_2  + jcdfi_flag * grid%jcdfi_gama * 1.0/grid%jcdfi_error_wind * h(i) * grid%em_g_u_1 
   grid%em_a_v_2  = grid%em_a_v_2  + jcdfi_flag * grid%jcdfi_gama * 1.0/grid%jcdfi_error_wind * h(i) * grid%em_g_v_1
   grid%em_a_t_2  = grid%em_a_t_2  + jcdfi_flag * grid%jcdfi_gama * 1.0/grid%jcdfi_error_t * h(i) * grid%em_g_t_1 
   grid%em_a_mu_2 = grid%em_a_mu_2 + jcdfi_flag * grid%jcdfi_gama * 1.0/grid%jcdfi_error_mu * h(i) * grid%em_g_mu_1 


 END SUBROUTINE jcdfi_add_forcing
!
!
!
 SUBROUTINE jcdfi_input_forcing ( grid ,config_flags )

  IMPLICIT NONE

  TYPE ( domain ) , intent(inout) :: grid
  type(grid_config_rec_type) , intent(in) :: config_flags

   call med_auxinput3dfi_in ( grid , config_flags )

   grid%em_g_u_1  = grid%em_g_u_2
   grid%em_g_v_1  = grid%em_g_v_2
   grid%em_g_t_1  = grid%em_g_t_2
   grid%em_g_mu_1 = grid%em_g_mu_2


   grid%em_g_u_2  = 0.0
   grid%em_g_v_2  = 0.0
   grid%em_g_t_2  = 0.0
   grid%em_g_mu_2 = 0.0

 END SUBROUTINE jcdfi_input_forcing
!
!
 SUBROUTINE jcdfi_finalize
     deallocate (H)
 END SUBROUTINE jcdfi_finalize
!
      SUBROUTINE JCDFI_COEF (MYPE,NSTEPS,DT,TAUC,NFILT)
!min---C
!min---C     calculate filter weights with selected window.
!min---C
!min---C       peter lynch and xiang-yu huang
!min---C
!min---C       ref: see hamming, r.w., 1989: digital filters,
!min---C                prentice-hall international. 3rd edition.
!min---C
!min---C       input:      nsteps  -  number of timesteps
!min---C                              forward or backward.
!min---C                   dt      -  time step in seconds.
!min---C                   tauc    -  cut-off period in seconds.
!min---C                   nfilt   -  indicator for selected filter.
!min---C
!min---C       output:     h       -  array(0:nsteps) with the
!min---C                              required filter weights
!min---C
!min---C------------------------------------------------------------
!min---C
 
      implicit none

      integer, intent(in)   :: mype, nsteps, nfilt
      real   , intent(in)   :: dt, tauc
      
!min--- Local data

      integer               :: n
      real                  :: pi, omegac, x, window, deltat
      real                  :: hh(0:nsteps)

      allocate ( h(0:nsteps*2) )
!min---

      pi=4*ATAN(1.)
      deltat=ABS(dt)
!min---
!min---------------------------------------------------------------
!min---
!min---     windows are defined by a call and held in hh.
!min---
      if ( nfilt .eq. -1) call debug    (nsteps,h)

      IF ( NFILT .EQ. 0 ) CALL UNIFORM  (NSTEPS,HH)
      IF ( NFILT .EQ. 1 ) CALL LANCZOS  (NSTEPS,HH)
      IF ( NFILT .EQ. 2 ) CALL HAMMING  (NSTEPS,HH)
      IF ( NFILT .EQ. 3 ) CALL BLACKMAN (NSTEPS,HH)
      IF ( NFILT .EQ. 4 ) CALL KAISER   (NSTEPS,HH)
      IF ( NFILT .EQ. 5 ) CALL POTTER2  (NSTEPS,HH)
      IF ( NFILT .EQ. 6 ) CALL DOLPHWIN (NSTEPS,HH)
!min---
!min---------------------------------------------------------------
!min---
      IF ( NFILT .LE. 6 ) THEN     ! sinc-windowed filters
!min---
!min---        calculate the cutoff frequency
         OMEGAC = 2.*PI/TAUC
!min---
         DO N=0,NSTEPS
            WINDOW = HH(N)
            IF ( N .EQ. 0 ) THEN
              X = (OMEGAC*DELTAT/PI)
            ELSE
              X = SIN(N*OMEGAC*DELTAT)/(N*PI)
            ENDIF
            HH(N) = X*WINDOW
         ENDDO

!min---        
!min---        normalize the sums to be unity
         CALL NORMLZ(HH,NSTEPS)

         DO N=0,NSTEPS
            H(NSTEPS+N) = HH(N)
            H(NSTEPS-N) = HH(N)
         ENDDO
!        IF(MYPE.EQ.0) THEN
!           DO N=0,NSTEPS*2
!              WRITE(6,'(A,I3,1X,F10.7)') 'DF COEF: N, H =',N,H(N)
!           ENDDO
!        ENDIF

      ELSEIF ( NFILT .EQ. 7 ) THEN     ! dolph filter

         CALL DOLPH(MYPE,DT,TAUC,NSTEPS,H)

      ELSEIF ( NFILT .EQ. 8 ) THEN     ! 2nd order, 2nd type quick start filter (RHO)

         CALL RHOFIL(DT,TAUC,2,NSTEPS*2,2,H,NSTEPS*2)
         IF(MYPE.EQ.0) THEN 
            DO N=0,NSTEPS*2 
               WRITE(6,'(A,I3,1X,F10.7)') 'RHO COEF: N, H =',N,H(N)
            ENDDO
         ENDIF
      ENDIF

      DO N=0,NSTEPS*2
         H(N) = -1.0*H(N)
         IF (N.EQ.NSTEPS) H(N) = 1.0 + H(N)
      ENDDO

      RETURN
      END subroutine jcdfi_coef 

!min---
      SUBROUTINE NORMLZ(HH,NMAX)
!min---
!min---     normalize the sum of hh to be unity
      implicit none
	  
	  integer, intent(in)                       :: nmax
	  real   , dimension(0:nmax), intent(out)   :: hh

!min--- local data
          real     :: sumhh
          integer  :: n

      SUMHH = HH(0)
      DO N=1,NMAX
        SUMHH = SUMHH + 2*HH(N)
      ENDDO
      DO N=0,NMAX
        HH(N)  = HH(N)/SUMHH
      ENDDO
!min---
      RETURN
      END subroutine normlz



      subroutine debug(nsteps, ww)

      implicit none

      integer, intent(in)                        :: nsteps
      real   , dimension(0:nsteps), intent(out)  :: ww
      integer :: n

      do n=0,nsteps
      ww(n)=0
      enddo

      ww(int(nsteps/2))=1

      return
      end subroutine debug


      SUBROUTINE UNIFORM(NSTEPS,WW)
!min---
!min---     define uniform or rectangular window function.
!min---
      implicit none

	  integer, intent(in)                        :: nsteps
	  real   , dimension(0:nsteps), intent(out)  :: ww
          
          integer          :: n

      DO N=0,NSTEPS
        WW(N) = 1.
      ENDDO
      RETURN
      END subroutine uniform




      SUBROUTINE LANCZOS(NSTEPS,WW)
!min---
!min---     define (genaralised) lanczos window function.
!min---
      implicit none 

      integer,  parameter                      :: nmax = 1000
      integer,  intent(in)                     :: nsteps
      real   ,  dimension(0:nmax), intent(out) :: ww
      integer  ::  n
      real     :: power, pi, w

!min---
!min---     (for the usual lanczos window, power = 1 )
      POWER = 1
!min---
      PI=4*ATAN(1.)
      DO N=0,NSTEPS
         IF ( N .EQ. 0 ) THEN
            W = 1.0
         ELSE
            W = SIN(N*PI/(NSTEPS+1)) / ( N*PI/(NSTEPS+1))
         ENDIF
         WW(N) = W**POWER
      ENDDO
      RETURN
      END  subroutine lanczos




      SUBROUTINE HAMMING(NSTEPS,WW)
!min---
!min---     define (genaralised) hamming window function.
!min---
      implicit none

      integer, intent(in)           :: nsteps
      real, dimension(0:nsteps)    :: ww
      integer   ::   n
      real      :: alpha, pi, w
	  
!min---
!min---     (for the usual hamming window, alpha=0.54,
!min---          for the hann window, alpha=0.50).
      ALPHA=0.54
!min---
      PI=4*ATAN(1.)
      DO N=0,NSTEPS
         IF ( N .EQ. 0 ) THEN
            W = 1.0
         ELSE
            W = ALPHA + (1-ALPHA)*COS(N*PI/(NSTEPS))
         ENDIF
         WW(N) = W
      ENDDO
      RETURN
      END  subroutine hamming
  
  
  
  
      SUBROUTINE BLACKMAN(NSTEPS,WW)
!min---
!min---     define blackman window function.
!min---
      implicit none

      integer, intent(in)           :: nsteps
       real, dimension(0:nsteps)    :: ww
       integer  :: n
!min---
      real :: pi, w

      PI=4*ATAN(1.)
      DO N=0,NSTEPS
         IF ( N .EQ. 0 ) THEN
            W = 1.0
         ELSE
            W = 0.42 + 0.50*COS(  N*PI/(NSTEPS))   &
                     + 0.08*COS(2*N*PI/(NSTEPS))
         ENDIF
         WW(N) = W
      ENDDO
      RETURN
      END  subroutine blackman





      SUBROUTINE KAISER(NSTEPS,WW)
!min---
!min---     define kaiser window function.
!min---
      implicit none

      integer, intent(in)           :: nsteps
      real, dimension(0:nsteps)    :: ww
      integer   :: n
      real      :: alpha, xi0a, xn, as
      real, external :: bessi0

      ALPHA = 1
!min---
      XI0A =  BESSI0(ALPHA)
      DO N=0,NSTEPS
         XN = N
         AS = ALPHA*SQRT(1.-(XN/NSTEPS)**2)
         WW(N) = BESSI0(AS) / XI0A
      ENDDO
      RETURN
      END  subroutine kaiser




      SUBROUTINE POTTER2(NSTEPS,WW)
!min---
!min---     define potter window function.
!min---     modified (by me) to fall off over twice the range.
!min---
      implicit none
      integer, intent(in)                       :: nsteps
	  real, dimension(0:nsteps),intent(out)     ::  ww
      integer  :: n
      real     :: ck, sum, arg
!min---     local data

      real, dimension(0:3)   :: d 
      real                   :: pi
      integer                :: ip

      d(0) = 0.35577019
	  d(1) = 0.2436983
	  d(2) = 0.07211497
	  d(3) = 0.00630165
 
      PI=4*ATAN(1.)
!min---
      CK = 1.0
      DO N=0,NSTEPS
         IF(N.EQ.NSTEPS) CK = 0.5
         ARG = PI*FLOAT(N)/FLOAT(NSTEPS)
!min---        modification in next statement
         ARG = ARG/2.
!min---        end of modification
         SUM = D(0)
         DO IP=1,3
            SUM = SUM + 2.*D(IP)*COS(ARG*FLOAT(IP))
         ENDDO
         WW(N) = CK*SUM
      ENDDO
      RETURN
      END SUBROUTINE POTTER2


      SUBROUTINE  DOLPHWIN(M,WINDOW)
!min---
!min---     calculation of dolph-chebyshev window or, for short,
!min---     dolph window, using the expression in the reference:
!min---
!min----     antoniou, andreas, 1993: digital filters: analysis,
!min----     design and applications. mcgraw-hill, inc., 689pp.
!min----
!min----     the dolph window is optimal in the following sense:
!min----     for a given main-lobe width, the stop-band attenuation
!min----     is minimal; for a given stop-band level, the main-lobe
!min----     width is minimal.
!min----
!min----     it is possible to specify either the ripple-ratio r
!min----     or the stop-band edge thetas.
!min----
      implicit none
      integer, intent(in)                  ::  m
      real, dimension(0:m),intent(out)     ::  window

!min---- local data
      real, dimension(0:2*m)               :: t
      real, dimension(0:m)                 :: w, time
      real  :: pi, thetas, x0, term1, term2, rr, r, db, sum, arg
      integer :: n, nm1, nt, i

!min----
      PI = 4*ATAN(1.D0)
!min----
      THETAS = 2*PI/(M)

!min----
      N = 2*M+1
      NM1 = N-1
      X0 = 1/COS(THETAS/2)
!min----
      TERM1 = (X0 + SQRT(X0**2-1))**(FLOAT(N-1))
      TERM2 = (X0 - SQRT(X0**2-1))**(FLOAT(N-1))
      RR = 0.5*(TERM1+TERM2)
      R = 1/RR
      DB = 20*LOG10(R)
      WRITE(*,'(1X,''DOLPH: M,N='',2I8)')M,N
!CIVY---      CALL MESAGE(MYPE,NPRPE,MES)
      WRITE(*,'(1X,''DOLPH: THETAS (STOP-BAND EDGE)='',F10.3)')THETAS
!CIVY---      CALL MESAGE(MYPE,NPRPE,MES)
      WRITE(*,'(1X,''DOLPH: R,DB='',2F10.3)')R, DB
!min----
      DO NT=0,M
        SUM = RR
        DO I=1,M
          ARG = X0*COS(I*PI/N)
          CALL CHEBY(T,NM1,ARG)
          TERM1 = T(NM1)
          TERM2 = COS(2*NT*PI*I/N)
          SUM = SUM + 2*TERM1*TERM2
        ENDDO
        W(NT) = SUM/N
        TIME(NT) = NT
      ENDDO
!min----
!min------   fill up the array for return
      DO NT=0,M
        WINDOW(NT) = W(NT)
      ENDDO
!min----
      RETURN
      END subroutine dolphwin
!min----
!min----<A NAME="DOLPH">
!min----
      SUBROUTINE  DOLPH(MYPE, DELTAT, TAUS, M, WINDOW)
!min----
!min----     calculation of dolph-chebyshev window or, for short,
!min----     dolph window, using the expression in the reference:
!min----
!min----     antoniou, andreas, 1993: digital filters: analysis,
!min----     design and applications. mcgraw-hill, inc., 689pp.
!min----
!min----     the dolph window is optimal in the following sense:
!min----     for a given main-lobe width, the stop-band attenuation
!min----     is minimal; for a given stop-band level, the main-lobe
!min----     width is minimal.
!min----
!min----     mype         ! current pe number (used only for message)
!min----
!min----------------------------------------------------------------
!min----
      implicit none

      integer, intent(in)                  ::  m
      real, dimension(0:m),intent(out)     ::  window
      integer, intent(in)                  :: mype
      real, intent(in)                     :: deltat, taus



!min---- local data
      integer, PARAMETER        ::  NMAX = 5000
      REAL, dimension(0:NMAX)   :: t, w, time
      real, dimension(0:2*nmax) :: w2
      INTEGER                   :: NPRPE=0        ! no of pe
      CHARACTER*80              :: MES
!min----
      real    :: pi, thetas, x0, term1, term2, rr, r,db, sum, arg, sumw
      integer :: n, nm1, i, nt
      
      PI = 4*ATAN(1.D0)

        print *, 'in dfcoef, deltat = ', deltat, 'taus=',taus


!min----
      N = 2*M+1
      NM1 = N-1
!min----
      THETAS = 2*PI*ABS(DELTAT/TAUS)
      X0 = 1/COS(THETAS/2)
      TERM1 = (X0 + SQRT(X0**2-1))**(FLOAT(N-1))
      TERM2 = (X0 - SQRT(X0**2-1))**(FLOAT(N-1))
      RR = 0.5*(TERM1+TERM2)
      R = 1/RR
      DB = 20*LOG10(R)


      WRITE(*,'(1X,''DOLPH: M,N='',2I8)')M,N
!CIVY---      CALL MESAGE(MYPE,NPRPE,MES)
      WRITE(*,'(1X,''DOLPH: THETAS (STOP-BAND EDGE)='',F10.3)')THETAS
!CIVY---      CALL MESAGE(MYPE,NPRPE,MES)
      WRITE(*,'(1X,''DOLPH: R,DB='',2F10.3)')R, DB
!CIVY---      CALL MESAGE(MYPE,NPRPE,MES)
!min----
!min----------------------------------------------------------------
!min----
      DO NT=0,M
         SUM = 1
         DO I=1,M
            ARG = X0*COS(I*PI/N)
            CALL CHEBY(T,NM1,ARG)
            TERM1 = T(NM1)
            TERM2 = COS(2*NT*PI*I/N)
            SUM = SUM + R*2*TERM1*TERM2
         ENDDO
         W(NT) = SUM/N
         TIME(NT) = NT
         WRITE(*,'(1X,''DOLPH: TIME, W='',F10.6,2X,E17.7)')  &
           TIME(NT), W(NT)
!CIVY---            CALL MESAGE(MYPE,NPRPE,MES)
      ENDDO
!min----     fill in the negative-time values by symmetry.
      DO NT=0,M
         W2(M+NT) = W(NT)
         W2(M-NT) = W(NT)
      ENDDO
!min----
!min------   fill up the array for return
      SUMW = 0.
      DO NT=0,2*M
         SUMW = SUMW + W2(NT)
      ENDDO
      WRITE(*,'(1X,''DOLPH: SUM OF WEIGHTS W2='',F10.4)')SUMW
!CIVY---      CALL MESAGE(MYPE,NPRPE,MES)
!min----
      DO NT=0,2*M
         WINDOW(NT) = W2(NT)
      ENDDO
!min----
      RETURN
      END subroutine dolph
!min----
!min----<A NAME="CHEBY">
!min----
      SUBROUTINE CHEBY(T,N,X)
!min----
!min----     calculate all chebyshev polynomials up to order n
!min----     for the argument value x.
!min----
!min----     reference: numerical recipes, page 184, recurrence
!min----         t_n(x) = 2xt_{n-1}(x) - t_{n-2}(x) ,  n>=2.
!min----
      implicit none

	  integer, intent(in) :: n
	  real   , intent(in) :: x
	  real, dimension(0:n):: t
 
          integer  :: nn
!min----
      T(0) = 1
      T(1) = X
      IF(N.LT.2) RETURN
      DO NN=2,N
         T(NN) = 2*X*T(NN-1) - T(NN-2)
      ENDDO
!min----
      RETURN
      END subroutine cheby
!min----
!min-<A NAME="RHO">
!min-



        SUBROUTINE RHOFIL(DT,TAUC,NORDER,NSTEP,ICTYPE,FROW,NOSIZE)
!min---c
!min---c       RHO = recurssive high order.
!min---c
!min----       This routine calculates and returns the 
!min----       Last Row, FROW, of the FILTER matrix.
!min----
!min----       Input Parameters:
!min----              DT  :  Time Step in seconds
!min----            TAUC  :  Cut-off period (hours)
!min----          NORDER  :  Order of QS Filter
!min----           NSTEP  :  Number of step/Size of row.
!min----          ICTYPE  :  Initial Conditions
!min----          NOSIZE  :  Max. side of FROW.
!min----
!min----       Working Fields:
!min----           ACOEF  :  X-coefficients of filter
!min----           BCOEF  :  Y-coefficients of filter
!min----          FILTER  :  Filter Matrix.
!min----
!min----       Output Parameters:
!min----            FROW  : Last Row of Filter Matrix.
!min----
!min----       Note: Two types of initial conditions are permitted.
!min----          ICTYPE = 1 : Order increasing each row to NORDER.
!min----          ICTYPE = 2 : Order fixed at NORDER throughout.
!min----
!min----       DOUBLE PRECISION USED THROUGHOUT.
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!min----
        DOUBLE PRECISION MUC
!min----
!min----       N.B. Single Precision for List Parameters.
        REAL, intent(in)    ::  DT,TAUC
!min----       Space for the last row of FILTER.
        integer, intent(in) ::  norder, nstep, ictype, nosize
        REAL   , dimension(0:nosize), intent(out)::  FROW
!min----
!min----       Arrays for rho filter.
        integer, PARAMETER  :: NOMAX=100
        real   , dimension(0:NOMAX) :: acoef, bcoef
        real   , dimension(0:NOMAX,0:NOMAX) :: filter
!min----       Working space.
        real   , dimension(0:NOMAX) :: alpha, beta
!kjw----
        real   :: DTT
        integer :: kk, k, NN, NORD, NR, NC, NROW, LL
	COMPLEX                  :: IOTA
!min----
!min-------------------------------------------------------------------
	DTT = ABS(DT)
!	DT = ABS(DT)
        PI = 2*DASIN(1.D0)
        IOTA = CMPLX(0.,1.)
!min-------------------------------------------------------------------
!min----
!min---       Filtering Parameters (derived).
        THETAC = 2*PI*DTT/(TAUC)
!        THETAC = 2*PI*DT/(TAUC)
        MUC = tan(THETAC/2) 
        FC = THETAC/(2*PI)
!min---
!min---------------------------------------------------------------
!min---
!min---     Clear the arrays.
      DO NC=0,NOMAX
         ACOEF(NC) = 0.
         BCOEF(NC) = 0.
         ALPHA(NC) = 0.
         BETA (NC) = 0.
         FROW (NC) = 0.
         DO NR=0,NOMAX
            FILTER(NR,NC) = 0.
         ENDDO
      ENDDO
!min---
!min----     Fill up the Filter Matrix.
      FILTER(0,0) = 1.
!min---
!min---     Get the coefficients of the Filter.
      IF ( ICTYPE.eq.2 ) THEN
         CALL RHOCOF(NORDER,NOMAX,MUC, ACOEF,BCOEF)
      ENDIF
!min----
      DO 100 NROW=1,NSTEP
!min----
            IF ( ICTYPE.eq.1 ) THEN
               NORD = MIN(NROW,NORDER)
               IF ( NORD.le.NORDER) THEN
                  CALL RHOCOF(NORD,NOMAX,MUC, ACOEF,BCOEF)
               ENDIF
            ENDIF
!min---
         DO K=0,NROW
            ALPHA(K) = ACOEF(NROW-K)
            IF(K.lt.NROW) BETA(K) = BCOEF(NROW-K)
         ENDDO
!min----
!min----        Correction for terms of negative index.
      IF ( ICTYPE.eq.2 ) THEN
         IF ( NROW.lt.NORDER ) THEN
            CN = 0.
            DO NN=NROW+1,NORDER
               CN = CN + (ACOEF(NN)+BCOEF(NN))
            ENDDO
!min----CC         ACOEF(NROW) = ACOEF(NROW) + CN
            ALPHA(0) = ALPHA(0) + CN
         ENDIF
      ENDIF
!min----
!min----       Check sum of ALPHAs and BETAs = 1
        SUMAB = 0.
        DO NN=0,NROW
          SUMAB = SUMAB + ALPHA(NN)
            IF(NN.lt.NROW) SUMAB = SUMAB + BETA(NN)
        ENDDO
!min---
         DO KK=0,NROW-1
            SUMBF = 0.
            DO LL=0,NROW-1
               SUMBF = SUMBF + BETA(LL)*FILTER(LL,KK)
            ENDDO
            FILTER(NROW,KK) = ALPHA(KK)+SUMBF
         ENDDO
         FILTER(NROW,NROW) = ALPHA(NROW)
!min---
!min----       Check sum of row elements = 1
        SUMROW = 0.
        DO NN=0,NROW
          SUMROW = SUMROW + FILTER(NROW,NN)
        ENDDO
!min----
  100 CONTINUE
!min----
        DO NC=0,NSTEP
          FROW(NC) = FILTER(NSTEP,NC)
        ENDDO
!min----
        RETURN
        END subroutine RHOFIL
!min----
!min---***************************************************************
!min---
        SUBROUTINE RHOCOF(NORD,NOMAX,MUC,CA,CB)
!min---
!min---       Get the coefficients of the RHO Filter 
!min---
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!min---
        integer, intent(in)      :: nord, nomax
        real, dimension(0:nomax) :: ca, cb
        integer                  :: NN
!min---
        COMPLEX                  :: IOTA
!min---   REAL MUC
!min---   COMPLEX ZN
        DOUBLE PRECISION         :: MUC, ZN
!min---
        DOUBLE PRECISION         :: CNR
!min---
        PI = 2*ASIN(1.)
        ROOT2 = SQRT(2.)
        IOTA = (0.,1.)
!min---
              RN = 1./FLOAT(NORD)
              SIGMA = 1./( SQRT(2.**RN-1.) )
!min---
              GAIN  = (MUC*SIGMA/(1+MUC*SIGMA))**NORD
              ZN = (1-MUC*SIGMA)/(1+MUC*SIGMA)
!min---
        DO NN=0,NORD
          CA(NN) = CNR(NORD,NN)*GAIN
          IF(NN.gt.0) CB(NN) = -CNR(NORD,NN)*(-ZN)**NN
        ENDDO
!min---C       Check sum of coefficients = 1
        SUMCOF = 0.
        DO NN=0,NORD
          SUMCOF = SUMCOF + CA(NN)
          IF(NN.gt.0) SUMCOF = SUMCOF + CB(NN)
        ENDDO
!min---
        RETURN
        END subroutine RHOCOF

END MODULE module_dfi
!min---***************************************************************
        DOUBLE PRECISION FUNCTION CNR(N,R)
!min--- 
!min---        Binomial Coefficient (n,r).
        IMPLICIT DOUBLE PRECISION(C,X)
!min--- 
        INTEGER , intent(in)    :: n, R
        IF ( R.eq.0 ) THEN
           CNR = 1.0
           RETURN
        ENDIF
        Coeff = 1.0
        XN = DFLOAT(N)
        XR = DFLOAT(R)
        DO K=1,R
          XK = DFLOAT(K)
          COEFF = COEFF * ( (XN-XR+XK)/XK )
        ENDDO
        CNR = COEFF
        RETURN
        END FUNCTION
!min---***************************************************************



      real FUNCTION BESSI0(X)
!min---
!min---   from numerical recipes (press, et al.)
!min---
      implicit none

	  real(8)      :: Y
	  real(8)      :: P1 = 1.0d0
	  real(8)      ::          P2 = 3.5156230D0
          real(8)      ::		 P3 = 3.0899424D0
          real(8)      ::	 P4 = 1.2067492D0
          real(8)      ::	 P5 = 0.2659732D0
	  real(8)      ::        P6 = 0.360768D-1
          real(8)      ::	 P7 = 0.45813D-2

	  real*8  	  :: Q1 = 0.39894228D0
	  real*8  	  ::	                 Q2 = 0.1328592D-1
	  real*8  	  ::					 Q3 = 0.225319D-2
	  real*8  	  ::					 Q4 = -0.157565D-2
	  real*8  	  ::					 Q5 = 0.916281D-2
	  real*8  	  ::					 Q6 = -0.2057706D-1
	  real*8  	  ::					 Q7 = 0.2635537D-1
	  real*8  	  ::					 Q8 = -0.1647633D-1
	  real*8  	  ::					 Q9 = 0.392377D-2

         real            :: x, ax


      IF (ABS(X).LT.3.75) THEN
        Y=(X/3.75)**2
        BESSI0=P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7)))))
      ELSE
        AX=ABS(X)
        Y=3.75/AX
        BESSI0=(EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4    &
           +Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
      ENDIF
      RETURN
      END function bessi0

      real function da_dot(x,y,n)
!
!     forms the dot product of two vectors.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!
      implicit none

      integer, intent(in)               :: n
      real, dimension(n), intent(in)    :: x,y

      real dtemp1,dtemp1x
      integer i,m,mp1
      integer ierror     ! MPI error code.
!
      da_dot = 0.0
      if(n <= 0)return
      dtemp1 = 0.0
!
!        code for both increments equal to 1
!
      if(n > 0 ) then
        m = mod(n,5)
        if( m /= 0 ) then
          do i = 1,m
            dtemp1 = dtemp1 + x(i)*y(i)
          end do
        end if
        if( n >= 5 ) then
          mp1 = m + 1
          do i = mp1,n,5
            dtemp1 = dtemp1 + x(i    )*y(i    ) + x(i + 1)*y(i + 1) + &
                              x(i + 2)*y(i + 2) + x(i + 3)*y(i + 3) + &
                              x(i + 4)*y(i + 4)
          end do
        end if
      end if
      da_dot = dtemp1
      end function da_dot

