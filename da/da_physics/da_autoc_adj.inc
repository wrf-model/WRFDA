subroutine da_autoc_adj(dt,scr2,prc,scr29,prc9,kts,kte,kms,kme)

   !-----------------------------------------------------------------------
   ! Purpose: Auto-conversion
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: kts, kte, kms, kme
   real,    intent(in)    :: dt(kms:kme)
   real,    intent(in)    :: scr2(kms:kme)
   real,    intent(in)    :: prc(kms:kme)
   real,    intent(in)    :: prc9(kms:kme)
   real,    intent(inout) :: scr29(kms:kme)

   integer :: k
   real    :: qcth, alpha

   if (trace_use) call da_trace_entry("da_autoc_adj")

   qcth = 0.5e-3
   alpha = 0.001

   do k = kts, kte
      if ( dt(k) <= 0.0 ) cycle

      if ( scr2(k) >= qcth ) then
         scr29(k) = scr29(k) + alpha * prc9(k)
      end if
   end do

   if (trace_use) call da_trace_exit("da_autoc_adj")

end subroutine da_autoc_adj


