SUBROUTINE da_autoc(DT,SCR2,PRC,kts,kte)

!  Auto-conversion

   implicit none

   integer                   :: kts, kte, k
   real, dimension(kts:kte)  :: DT, SCR2, PRC
   real, parameter           :: QCTH=0.5E-3, QCK1=1.0E-3

   DO k=kts,kte

      if (DT(k) <= 0.) cycle

      if (SCR2(k) >= QCTH) then
         PRC(k) = QCK1*(SCR2(k)-QCTH)
      else
         PRC(k) = 0.
      endif
   ENDDO

END SUBROUTINE da_autoc

