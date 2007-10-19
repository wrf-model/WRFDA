subroutine da_autoc_lin(dt,scr2,prc,scr29,prc9,kts,kte,kms,kme)

   !-----------------------------------------------------------------------
   ! Purpose: Auto-conversion
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: kts, kte, kms, kme
   real,    intent(in)  :: dt(kms:kme)
   real,    intent(in)  :: scr29(kms:kme)
   real,    intent(in)  :: scr2(kms:kme)
   real,    intent(out) :: prc(kms:kme)
   real,    intent(out) :: prc9(kms:kme)

   integer :: k
   real    :: qcth, alpha

   if (trace_use) call da_trace_entry("da_autoc_lin")

   qcth = 0.5e-3
   alpha = 0.001

   do k = kts, kte
      if (dt(k) <= 0.0) cycle

      if (scr2(k) >= qcth) then
         prc9(k) = alpha * scr29(k)
         prc(k) = alpha * (scr2(k) - qcth)
      else
         prc9(k) = 0.0
         prc(k) = 0.0
      end if
   end do

   if (trace_use) call da_trace_exit("da_autoc_lin")

end subroutine da_autoc_lin


