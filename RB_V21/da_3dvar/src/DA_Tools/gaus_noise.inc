REAL FUNCTION GAUS_NOISE (KDUM)

!---------------------------------------------------------------------
!
!                       ROUTINE GAUS_NOISE
!                     *********************
!
!  PURPOSE :
!  -------
!     RETURN A NORMALLY (GAUSSIAN) DISTRIBUTED RANDOM VARIABLE CTIONS
!     WITH 0 MEAN AND 1 STANDARD DEVIATION
!
!   METHOD :
!   ------
!
!   INPUT :
!   -----
!      ARGUMENT
!         KDUM             : SEED FOR RANDOM NUMBER GENERATOR 
!                            (SET TO A LARGE NEGATIVE NUMBER ON ENTRY)
!      COMMON              : NONE
!
!   OUTPUT :
!   ------
!      ARGUMENT
!         GAUSS_NOISE      : Gaussian random betwen 
!      COMMON              : NONE
!
!   WORKSPACE :              NONE
!   ---------
!      LOCAL :
!
!   EXTERNAL :  
!   --------
!      UNIFVA     
!
!   REFERENCE :
!   ---------
!      NUMERICAL RECIPES IN FORTRAN. THE ART OF SCIENTIFIC COMPUTING.
!      SECOND EDITION.  CAMBRIDGE UNIVERSITY PRESS.
!      PRESS ET. AL., 1986.
!
!   MODIFICATIONS:
!   --------------
!       ORIGINAL  : 95-01 (F. VANDENBERGHE)
!       ADDITION  : 96-06 (A. WEAVER)
!---------------------------------------------------------------------
 
   IMPLICIT NONE
 
   INTEGER, INTENT(IN) :: KDUM

   INTEGER             :: NISET
   REAL                :: GSET
   SAVE NISET,GSET
   DATA NISET/0/

   REAL ZFAC,ZRSQ,ZV1,ZV2

!  BEGIN MAIN
!  ----------

   IF (NISET.EQ.0) THEN

1000  CONTINUE
      ZV1   = 2.*UNIFVA (KDUM) - 1.
      ZV2   = 2.*UNIFVA (KDUM) - 1.
      ZRSQ  = ZV1**2 + ZV2**2
          
      IF ( (ZRSQ.GE.1.).OR.(ZRSQ.EQ.0.) ) GO TO 1000

      ZFAC  = SQRT(-2.*LOG(ZRSQ)/ZRSQ)
      GSET  = ZV1*ZFAC
      GAUS_NOISE = ZV2*ZFAC
      NISET = 1
   ELSE
      GAUS_NOISE = gset
      NISET = 0
   ENDIF
      
   RETURN

END FUNCTION GAUS_NOISE

