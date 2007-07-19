subroutine da_accre_lin(DT,SCR2,SCR3,PRA,SCR29,SCR39,PRA9,kts,kte,kms,kme)


   !-----------------------------------------------------------------------
   ! Purpose: Accretion (Collection)
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                   :: kts, kte, kms, kme
   real, dimension(kms:kme), intent(in)  :: DT, SCR2, SCR3, SCR29, SCR39
   real, dimension(kms:kme), intent(out) :: PRA, PRA9

   integer   :: k
   real      :: gamma, QRTH

   gamma = 0.002
   QRTH = 1.0e-6

   do K = kts, kte

      if ( DT(k) <= 0.0 ) cycle

      if (SCR2(k) > 0.0 .and. SCR3(k) > QRTH ) then
         PRA9(K) = gamma * 0.875 * SCR2(k) * (SCR3(K)*1.0e3)**(-0.125) * 1.0e3 * SCR39(K)  &
                      + gamma * SCR29(k) * (SCR3(K)*1.0e3)**0.875
         PRA(k) = gamma * SCR2(k) * (SCR3(k)*1.0e3)**0.875
      else if (SCR2(k) > 0.0 .and. SCR3(k) <= QRTH ) then
         PRA9(K) = gamma * SCR29(k) * (QRTH*1.0e3)**0.875
         PRA(k) = gamma * SCR2(k) * (QRTH*1.0e3)**0.875
      else
         PRA9(K) = 0.0
         PRA(k) = 0.0
      end if

   end do

end subroutine da_accre_lin
