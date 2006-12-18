subroutine da_autoc(DT,SCR2,PRC,kts,kte)

   !  Auto-conversion

   implicit none

   integer, intent(in)       :: kts, kte
   real, dimension(kts:kte), intent(in)  :: DT, SCR2
   real, dimension(kts:kte), intent(out) :: PRC

   integer       :: k

   real, parameter           :: QCTH=0.5E-3, QCK1=1.0E-3

   do k=kts,kte

      if (DT(k) <= 0.) cycle

      if (SCR2(k) >= QCTH) then
         PRC(k) = QCK1*(SCR2(k)-QCTH)
      else
         PRC(k) = 0.
      end if
   end do

end subroutine da_autoc


