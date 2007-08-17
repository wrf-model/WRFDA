subroutine da_evapo(DT,SCR3,SCR5,QV,PRE,SCR6,kts,kte,kms,kme)

   !-----------------------------------------------------------------------
   ! Purpose: Rainwater evaporation
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                   :: kts, kte, kms, kme
   real, dimension(kms:kme), intent(in)  :: DT, SCR3, SCR5, SCR6, QV
   real, dimension(kms:kme), intent(out) :: PRE

   integer                   :: k
   real                      :: beta, QRTH

   QRTH = 1.e-6
   beta = 0.0486   ! original

   do K = kts, kte
      if ( DT(k) <= 0. ) cycle

      if ( SCR3(K) > QRTH .and. QV(k) < SCR5(k) ) then
         PRE(k) = beta * ( QV(K)-SCR5(K) ) * ( SCR6(k)*SCR3(K) )**0.65
      else if ( SCR3(K) <= QRTH .and. SCR3(k) > 0. &
                          .and. QV(k) < SCR5(k) ) then
         PRE(k) = beta * ( QV(K)-SCR5(K) ) * ( SCR6(k)*QRTH )**0.65
      else
         PRE(k) = 0.
      end if

     ! PRE(k) = AMAX1( -SCR3(k)/DT(k), PRE(k) )
     if ( PRE(k) < -SCR3(k)/DT(k) ) then
        PRE(k) = -SCR3(k) / DT(k)
     end if
   end do

end subroutine da_evapo

