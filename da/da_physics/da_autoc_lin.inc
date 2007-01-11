subroutine da_autoc_lin(DT,SCR2,PRC,SCR29,PRC9,kts,kte,kms,kme)

   !-----------------------------------------------------------------------
   ! Purpose: Auto-conversion
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                   :: kts, kte, kms, kme
   real, dimension(kms:kme), intent(in)  :: DT, SCR2, SCR29
   real, dimension(kms:kme), intent(out) :: PRC, PRC9

   integer      :: k
   real         :: QCTH, alpha

   QCTH = 0.5E-3
   alpha = 0.001

   do k = kts, kte

      if ( DT(k) <= 0. ) cycle

      if ( SCR2(k) >= QCTH ) then
         PRC9(K) = alpha * SCR29(K)
         PRC(k) = alpha * ( SCR2(k) - QCTH )
      else
         PRC9(K) = 0.
         PRC(k) = 0.
      end if
   end do

end subroutine da_autoc_lin


