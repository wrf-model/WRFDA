subroutine da_autoc_lin(DT,SCR2,PRC,SCR29,PRC9,kts,kte)

   !-----------------------------------------------------------------------
   ! Purpose: Auto-conversion
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                   :: kts, kte
   real, dimension(kts:kte), intent(in)  :: DT, SCR2
   real, dimension(kts:kte), intent(out)  :: PRC
   real, dimension(kts:kte), intent(in)  :: SCR29
   real, dimension(kts:kte), intent(out) :: PRC9

   integer                   :: k

   real, parameter          :: QCTH=0.5E-3, QCK1=1.0E-3

   do k=kts,kte
      if (DT(k) <= 0.) cycle

      if (SCR2(k) >= QCTH) then
         PRC9(K) = QCK1 * SCR29(K)
         PRC(k) = QCK1*(SCR2(k)-QCTH)
      else
         PRC9(K) = 0.
         PRC(k) = 0.
      end if
   end do

end subroutine da_autoc_lin


