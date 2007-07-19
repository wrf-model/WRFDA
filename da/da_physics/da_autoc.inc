subroutine da_autoc(DT,SCR2,PRC,kts,kte,kms,kme)

   !-----------------------------------------------------------------------
   ! Purpose: Auto-conversion
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                   :: kts, kte, kms, kme
   real, dimension(kms:kme), intent(in)  :: DT, SCR2
   real, dimension(kms:kme), intent(out) :: PRC

   integer      :: k
   real         :: QCTH, alpha

   QCTH = 0.5E-3   
   alpha = 0.001

   do k = kts, kte

      if ( DT(k) <= 0.0 ) cycle

      if ( SCR2(k) >= QCTH ) then
         PRC(k) = alpha * ( SCR2(k) - QCTH )
      else
         PRC(k) = 0.0
      end if
   end do

end subroutine da_autoc

