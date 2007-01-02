Subroutine DA_Ens_Magnify ( ensmean, factors)

!-------------------------------------------------------------------------
!  HISTORY: 09/01/2004                                         Mi-Seon Lee
!-------------------------------------------------------------------------

   IMPLICIT NONE

   type (xb_type), intent(inout)    :: ensmean
   real          , intent(in)       :: factors

   ensmean%u =ensmean%u  * factors
   ensmean%v =ensmean%v  * factors
   ensmean%t =ensmean%t  * factors
   ensmean%q =ensmean%q  * factors

   ensmean%h =ensmean%h  * factors
   ensmean%p =ensmean%p  * factors
   ensmean%rh=ensmean%rh * factors

   ensmean%psfc=ensmean%psfc * factors

END SUBROUTINE DA_Ens_Magnify


