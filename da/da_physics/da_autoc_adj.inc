subroutine da_autoc_adj(DT,SCR2,PRC,SCR29,PRC9,kts,kte,kms,kme)

   !-----------------------------------------------------------------------
   ! Purpose: Auto-conversion
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                     :: kts, kte, kms, kme
   real, dimension(kms:kme), intent(in)    :: DT, SCR2
   real, dimension(kms:kme), intent(in)    :: PRC, PRC9
   real, dimension(kms:kme), intent(inout) :: SCR29

   integer      :: k
   real         :: QCTH, alpha

   QCTH = 0.5E-3
   alpha = 0.001

   do k = kts, kte
      if ( DT(k) <= 0.0 ) cycle

      if ( SCR2(k) >= QCTH ) then
         SCR29(K) = SCR29(K) + alpha * PRC9(K)
      end if
   end do

end subroutine da_autoc_adj
