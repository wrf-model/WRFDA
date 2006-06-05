SUBROUTINE da_autoc_lin(DT,SCR2,PRC,SCR29,PRC9,kts,kte)

!  Auto-conversion

   implicit none

   integer                   :: kts, kte, k
   real, dimension(kts:kte)  :: DT, SCR2, PRC
   real, dimension(kts:kte)  :: SCR29, PRC9
   real, parameter           :: QCTH=0.5E-3, QCK1=1.0E-3

   DO k=kts,kte

      if (DT(k) <= 0.) cycle

      if (SCR2(k) >= QCTH) then
         PRC9(K) = QCK1 * SCR29(K)
         PRC(k) = QCK1*(SCR2(k)-QCTH)
      else
         PRC9(K) = 0.
         PRC(k) = 0.
      endif
   ENDDO

END SUBROUTINE da_autoc_lin
