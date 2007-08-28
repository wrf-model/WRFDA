subroutine da_evapo(DT,SCR3,SCR5,QV,PRE,SCR6,kts,kte,kms,kme)

   !-----------------------------------------------------------------------
   ! Purpose: Rainwater evaporation
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: kts, kte, kms, kme
   real,    intent(in)  :: dt(kms:kme)
   real,    intent(in)  :: scr3(kms:kme)
   real,    intent(in)  :: scr5(kms:kme)
   real,    intent(in)  :: scr6(kms:kme)
   real,    intent(in)  :: qv(kms:kme)
   real,    intent(out) :: pre(kms:kme)

   integer                   :: k
   real                      :: beta, qrth

   if (trace_use) call da_trace_entry("da_evapo")

   qrth = 1.0e-6
   beta = 0.0486   ! original

   do k = kts, kte
      if (dt(k) <= 0.0) cycle

      if (scr3(k) > qrth .and. qv(k) < scr5(k)) then
         pre(k) = beta * (qv(k)-scr5(k) ) * (scr6(k)*scr3(k))**0.65
      else if (scr3(k) <= qrth .and. scr3(k) > 0.0 .and. qv(k) < scr5(k)) then
         pre(k) = beta * (qv(k)-scr5(k)) * (scr6(k)*qrth)**0.65
      else
         pre(k) = 0.0
      end if

      ! pre(k) = amax1( -scr3(k)/dt(k), pre(k) )
      if ( pre(k) < -scr3(k)/dt(k) ) then
         pre(k) = -scr3(k) / dt(k)
      end if
   end do

   if (trace_use) call da_trace_exit("da_evapo")

end subroutine da_evapo

