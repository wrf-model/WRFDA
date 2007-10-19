subroutine da_accre_adj(DT,SCR2,SCR3,PRA,SCR29,SCR39,PRA9,kts,kte,kms,kme)

   !-----------------------------------------------------------------------
   ! Purpose: Accretion (Collection)
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                     :: kts, kte, kms, kme
   real, dimension(kms:kme), intent(in)    :: DT, SCR2, SCR3
   real, dimension(kms:kme), intent(in)    :: PRA, PRA9
   real, dimension(kms:kme), intent(inout) :: SCR29, SCR39

   !  local
   integer   :: k
   real      :: gamma, QRTH

   gamma = 0.002
   QRTH = 1.0e-6

   do K = kts, kte

      if ( DT(k) <= 0.0) cycle

      if ( SCR2(k) > 0.0 .and. SCR3(k) > QRTH ) then
         SCR39(K) = SCR39(K) + gamma * 0.875 * SCR2(k) * (SCR3(K)*1.0e3)**(-0.125 ) * 1.0e3 * PRA9(K)
         SCR29(K) = SCR29(K) + gamma * (SCR3(K)*1.0e3)**0.875 * PRA9(K)
      else if (SCR2(k) > 0.0 .and. SCR3(k) <= QRTH ) then
         SCR29(k) = SCR29(k) + gamma * (QRTH*1.0e3)**0.875 * PRA9(k)
      end if

   end do

end subroutine da_accre_adj
