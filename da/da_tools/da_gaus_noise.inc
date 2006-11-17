real function da_gaus_noise (KDUM)

!---------------------------------------------------------------------
!
!                       RoutinE GAUS_NOISE
!                     *********************
!
!  PURPOSE :
!  -------
!     return A NORMALLY (GAUSSIAN) DISTRIBUTED RANdoM VARIABLE CTIONS
!     WITH 0 MEAN AND 1 STANDARD DEVIATION
!
!   METHOD :
!   ------
!
!   inPUT :
!   -----
!      ARGUMENT
!         KDUM             : SEED FOR RANdoM NUMBER GENERATOR 
!                            (SET TO A LARGE NEGATIVE NUMBER ON ENTRY)
!      COMMON              : NONE
!
!   outPUT :
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
!      UNifVA     
!
!   REFERENCE :
!   ---------
!      NUMERICAL RECIPES in FORTRAN. THE ART OF SCIENTifIC COMPUTinG.
!      SECOND EDITION.  CAMBRIDGE UNIVERSITY PRESS.
!      PRESS ET. AL., 1986.
!
!   MODifICATIONS:
!   --------------
!       ORIGinAL  : 95-01 (F. VANDENBERGHE)
!       ADDITION  : 96-06 (A. WEAVER)
!---------------------------------------------------------------------
 
   implicit none
 
   integer, intent(in) :: KDUM

   integer             :: NISET
   real                :: GSET
   save NISET,GSET
   DATA NISET/0/

   real ZFAC,ZRSQ,ZV1,ZV2

!  BEGin MAin
!  ----------

   if (NISET.EQ.0) then

1000  continue
      ZV1   = 2.*DA_UNifVA (KDUM) - 1.
      ZV2   = 2.*DA_UNifVA (KDUM) - 1.
      ZRSQ  = ZV1**2 + ZV2**2
          
      if ( (ZRSQ.GE.1.).OR.(ZRSQ.EQ.0.) ) goto 1000

      ZFAC  = sqrt(-2.*LOG(ZRSQ)/ZRSQ)
      GSET  = ZV1*ZFAC
      DA_GAUS_NOISE = ZV2*ZFAC
      NISET = 1
   else
      DA_GAUS_NOISE = gset
      NISET = 0
   end if
      
   return

end function da_gaus_noise

