subroutine da_autoc_adj(DT,SCR2,PRC,SCR29,PRC9,kts,kte)

   ! Auto-conversion

   implicit none

   integer, intent(in)       :: kts, kte
   real, dimension(kts:kte), intent(in)  :: DT, SCR2, PRC
   real, dimension(kts:kte), intent(out)  :: SCR29
   real, dimension(kts:kte), intent(in)  :: PRC9

   integer       :: k

   real, parameter           :: QCTH=0.5E-3, QCK1=1.0E-3

   do k=kts,kte
      if (DT(k) <= 0.) cycle

      if (SCR2(k) >= QCTH) then
         SCR29(K)=SCR29(K)+QCK1 * PRC9(K)
      end if
   end do

end subroutine da_autoc_adj


