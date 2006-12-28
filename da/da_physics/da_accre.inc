subroutine da_accre(DT,SCR2,SCR3,PRA,kts,kte,kms,kme)

   !-----------------------------------------------------------------------
   ! Purpose: Accretion (Collection)
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                   :: kts, kte, kms, kme
   real, dimension(kms:kme), intent(in)  :: DT, SCR2, SCR3
   real, dimension(kms:kme), intent(out) :: PRA

   integer   :: k
   real      :: gamma, QRTH

   gamma = 0.002
   QRTH  = 1.e-6

   do K = kts, kte

      if ( DT(k) <= 0. ) cycle

      if ( SCR2(k) > 0. .and. SCR3(k) > QRTH ) then
         PRA(k) = gamma * SCR2(k) * (SCR3(k)*1.e3)**0.875
      else if ( SCR2(k) > 0. .and. SCR3(k) <= QRTH ) then
         PRA(k) = gamma * SCR2(k) * (QRTH*1.e3)**0.875 
      else
         PRA(k) = 0.
      end if

   end do

end subroutine da_accre

