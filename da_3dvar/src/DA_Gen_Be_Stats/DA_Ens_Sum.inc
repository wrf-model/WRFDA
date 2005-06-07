Subroutine DA_Ens_Sum ( xb, ensmean)

!-------------------------------------------------------------------------
!  PURPOSE: Sum of Each Ensemble
!
!  HISTORY: 09/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

   IMPLICIT NONE

   type (xb_type), intent(in)       :: xb
   type (xb_type), intent(inout)    :: ensmean

   ensmean%u =ensmean%u  + xb%u
   ensmean%v =ensmean%v  + xb%v
   ensmean%t =ensmean%t  + xb%t
   ensmean%q =ensmean%q  + xb%q

   ensmean%h =ensmean%h  + xb%h
   ensmean%p =ensmean%p  + xb%p
   ensmean%rh=ensmean%rh + xb%rh

   ensmean%psfc=ensmean%psfc + xb%psfc

END SUBROUTINE DA_Ens_Sum


