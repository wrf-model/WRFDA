subroutine da_evapo_adj(DT,SCR3,SCR5,QV_B,PRE,SCR6,  &
                        SCR39,SCR59,QV_A,PRE9,SCR69, &
                        kts,kte,kms,kme)


   !-----------------------------------------------------------------------
   ! Purpose: Rainwater evaporation
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                     :: kts, kte, kms, kme
   real, dimension(kms:kme), intent(in)    :: DT, SCR3, SCR5, SCR6, QV_B
   real, dimension(kms:kme), intent(inout) :: PRE, PRE9
   real, dimension(kms:kme), intent(inout) :: QV_A
   real, dimension(kms:kme), intent(out)   :: SCR39, SCR59, SCR69

   integer             :: k
   real                :: beta, QRTH
   real                :: TMP, TMP2

   if (trace_use) call da_trace_entry("da_evapo_adj")

   QRTH = 1.0E-6
   beta = 0.0486   ! original

   do K = kts, kte
 
      if ( DT(k) <= 0.0 ) cycle
 
      if ( SCR3(K) > QRTH .and. QV_B(k) < SCR5(k) ) then
         PRE(k) = beta * ( QV_B(K)-SCR5(K) ) * ( SCR6(k)*SCR3(K) )**0.65
      else if ( SCR3(K) <= QRTH .and. SCR3(k) > 0.0 &
                          .and. QV_B(k) < SCR5(k) ) then
         PRE(k) = beta * ( QV_B(K)-SCR5(K) ) * ( SCR6(k)*QRTH )**0.65
      else
         PRE(k) = 0.0
      end if

   end do

   do K = kts, kte

      if (DT(k) <= 0.0) cycle

      SCR39(k) = 0.0
      if ( PRE(k) < -SCR3(k)/DT(k) ) then
         SCR39(k) = -PRE9(k) / DT(k)
         PRE9(k)  = 0.0
      end if

      SCR59(k) = 0.0
      SCR69(k) = 0.0
      if ( SCR3(k) > QRTH .and. QV_B(k) < SCR5(k) ) then
         TMP  = beta * ( QV_B(k)-SCR5(k) ) * 0.65 * ( SCR6(k)*SCR3(k) )**(-0.35)
         TMP2 = beta * ( SCR6(k)*SCR3(k) )**0.65

         QV_A(k) = QV_A(k) + TMP2 * PRE9(k)
         SCR59(k) = -TMP2 * PRE9(k)
         SCR39(k) = SCR39(k) + TMP * SCR6(k) * PRE9(k)
         SCR69(k) = TMP * SCR3(k) * PRE9(k)
      else if (SCR3(k) <= QRTH .and. SCR3(k) > 0.0 .and. QV_B(k) < SCR5(k) ) then
         TMP  = beta * ( QV_B(k)-SCR5(k) ) * 0.65 * ( SCR6(k)*QRTH )**(-0.35)
         TMP2 = beta * ( SCR6(k)*QRTH )**0.65

         QV_A(k) = QV_A(k) + TMP2 * PRE9(k)
         SCR59(k) = -TMP2 * PRE9(k)
         SCR69(k) = TMP * QRTH * PRE9(k)
      end if

   end do

   if (trace_use) call da_trace_exit("da_evapo_adj")

end subroutine da_evapo_adj
