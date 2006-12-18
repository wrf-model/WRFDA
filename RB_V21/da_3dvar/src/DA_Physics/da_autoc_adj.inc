SUBROUTINE da_autoc_adj(DT,SCR2,PRC,SCR29,PRC9,kts,kte)

!  Auto-conversion

   implicit none

   integer                   :: kts, kte, k
   real, dimension(kts:kte)  :: DT, SCR2, PRC
   real, dimension(kts:kte)  :: SCR29, PRC9
   real, parameter           :: QCTH=0.5E-3, QCK1=1.0E-3

   DO k=kts,kte

      if (DT(k) <= 0.) cycle

      if (SCR2(k) >= QCTH) then
         SCR29(K)=SCR29(K)+QCK1 * PRC9(K)
      endif

   ENDDO

END SUBROUTINE da_autoc_adj
