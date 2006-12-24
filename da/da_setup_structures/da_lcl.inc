subroutine da_lcl(P0, Z0, T0, Q0, PLCL, ZLCL, TLCL, QLCL)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)   :: P0, Z0, T0, Q0
   real, intent(out)  :: PLCL, ZLCL, TLCL, QLCL

   real   :: DP, QS, EPS

   DP=5.
   PLCL=300.

   do
      TLCL=T0*((PLCL/P0)**0.286)

      call da_qfrmrh(PLCL, TLCL, 100., QS)

      EPS=QS-Q0

      if (EPS >= 0.) then
         ZLCL=(1004./9.81)*(T0-TLCL)+Z0
         QLCL=QS
         return
      else
         PLCL=PLCL+DP

         if (PLCL >= P0) then
            ZLCL=Z0
            QLCL=Q0
            PLCL=P0
            return
        end if
      end if
   end do

end subroutine da_lcl


