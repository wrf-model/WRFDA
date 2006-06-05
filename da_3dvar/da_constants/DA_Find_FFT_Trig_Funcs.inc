SUBROUTINE DA_Find_FFT_Trig_Funcs( n, trig_functs )

!------------------------------------------------------------------------------
!  PURPOSE: Set up constants required for Fourier, sine and cosine transforms.
!
!  METHOD:  Calculate sin, cos of points on grid for trig_functs array.
!
!  HISTORY: 1988     - Clive Temperton~s original.
!           1995     - Modified by Andrew Lorenc at UKMO.
!           02/08/00 - Modified to f90 WRF format.
!-----------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN)        :: n
   REAL, INTENT(OUT)          :: trig_functs(:)

   INTEGER                    :: k, nil, nhl
   REAL                       :: del, angle

!------------------------------------------------------------------------------
!  [1.0] Trig functions for real periodic transform:
!------------------------------------------------------------------------------

   trig_functs(:) = 0.0

   DEL=4.0*ASIN(1.0)/FLOAT(N)
   NIL=0
   NHL=(N/2)-1

   DO K=NIL,NHL
      ANGLE=FLOAT(K)*DEL
      trig_functs(2*K+1)=COS(ANGLE)
      trig_functs(2*K+2)=SIN(ANGLE)
   END DO

!  [1.1] Extra trig functions for cosine transform:

   DEL=0.5*DEL
   DO K=1,NHL
      ANGLE=FLOAT(K)*DEL
      trig_functs(2*N+K)=SIN(ANGLE)
   END DO
  
!  [1.2] Extra trig functions for shifted cosine transform:

   DEL=0.5*DEL
   DO K=1,N
      ANGLE=FLOAT(K)*DEL
      trig_functs(N+K)=SIN(ANGLE)
   END DO

END SUBROUTINE DA_Find_FFT_Trig_Funcs
