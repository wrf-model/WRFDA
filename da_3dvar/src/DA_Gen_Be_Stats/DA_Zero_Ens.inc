Subroutine DA_Zero_Ens ( ensmean)

!-------------------------------------------------------------------------
!  PURPOSE: Initialization of Ensemble Mean
!
!  HISTORY: 09/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

   IMPLICIT NONE

   type (xb_type), intent(inout)    :: ensmean

   ensmean%u = 0.0
   ensmean%v = 0.0
   ensmean%t = 0.0
   ensmean%q = 0.0

   ensmean%h = 0.0
   ensmean%p = 0.0
   ensmean%rh= 0.0

   ensmean%psfc= 0.0

END SUBROUTINE DA_Zero_Ens


