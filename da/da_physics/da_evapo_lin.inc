subroutine da_evapo_lin(DT,SCR3,SCR5,QV_B,PRE,SCR6,  &
                        SCR39,SCR59,QV_A,PRE9,SCR69, &
                        kts,kte,kms,kme)

   !-----------------------------------------------------------------------
   ! Purpose: Rainwater evaporation
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                   :: kts, kte, kms, kme
   real, dimension(kms:kme), intent(in)  :: DT, SCR3, SCR5, SCR6, QV_B
   real, dimension(kms:kme), intent(out) :: PRE, PRE9
   real, dimension(kms:kme), intent(in)  :: SCR39, SCR59, SCR69, QV_A

   integer           :: k
   real              :: beta, QRTH
   real              :: TMP, TMP2

   QRTH = 1.0e-6
   beta = 0.0486   ! original

   do K = kts, kte

      if (DT(k) <= 0.0) cycle

      if ( SCR3(k) > QRTH .and. QV_B(k) < SCR5(k) ) then
         TMP  = beta * ( QV_B(k)-SCR5(k) )* 0.65 * ( SCR6(k)*SCR3(k) )**(-0.35)
         TMP2 = beta * ( SCR6(k)*SCR3(k) )**0.65
         PRE9(k) = TMP * ( SCR69(k)*SCR3(k)+SCR6(k)*SCR39(k) ) + &
                   TMP2 * ( QV_A(k)-SCR59(k) )
         PRE(k)  = beta * ( QV_B(k)-SCR5(k) ) * ( SCR6(k)*SCR3(k) )**0.65
      else if ( SCR3(k) <= QRTH .and. SCR3(k) > 0.0 .and. QV_B(k) < SCR5(k) ) then
         TMP  = beta * ( QV_B(k)-SCR5(k) ) * 0.65 * ( SCR6(k)*QRTH )**(-0.35)
         TMP2 = beta * ( SCR6(k)*QRTH )**0.65
         PRE9(k) = TMP * ( SCR69(k)*QRTH ) + &
                   TMP2 * ( QV_A(k)-SCR59(k) )
         PRE(k)  = beta * ( QV_B(k)-SCR5(k) ) * ( SCR6(k)*QRTH )**0.65
      else
         PRE9(k) = 0.0
         PRE(k) = 0.0
      end if

      if ( PRE(k) < -SCR3(k)/DT(k) ) then
         PRE9(k) = -SCR39(k) / DT(k)
         PRE(k)  = -SCR3(k) / DT(k)
      end if

   end do

end subroutine da_evapo_lin

