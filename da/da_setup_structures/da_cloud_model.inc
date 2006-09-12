subroutine da_cloud_model ( TB, PB, QB, QCWB, QRNB, ZB, ZFB, DT, kts, kte )

   !-----------------------------------------------------------------
   ! PURPOSE: Calculate DT (=dz/w) using cumulus parameterization 
   !          of a one-dimensional cloud model.
   !-----------------------------------------------------------------

   ! Calculate DT

   implicit none

   integer, intent(IN)                     :: kts, kte
   real, intent(IN),  dimension(kts:kte)   :: TB, PB, QB, QCWB, QRNB, ZB
   real, intent(IN),  dimension(kts:kte+1) :: ZFB
   real, intent(OUT), dimension(kts:kte)   :: DT

   integer                    :: k
   real                       :: P0, Z0, T0, Q0
   real                       :: PLCL, ZLCL, TLCL, QLCL
   integer                    :: KCB, KCT
   real                       :: PCT, ZCT
   real, dimension(kts:kte)   :: ZC, TC, QC, PP, QT
   real, dimension(kts:kte)   :: TCV, TBV, B
   real                       :: G, ALPHA, RC, MU, XX, YY
   real, dimension(kts:kte+1) :: W0, W

   IF (trace_use) call da_trace_entry("da_cloud_model")

   G=9.81
   ALPHA=0.5
   RC=100.0
   MU=0.183/RC

   DO k = kts, kte+1
      W0(k)=0.0  
      W(k)=0.0  
   ENDDO

   DO k = kts, kte
      PP(k)=PB(k)/100.0
      DT(k)=0.0
   ENDDO

   P0 = PP(kts)
   Z0 = ZB(kts)
   T0 = MAX(TB(kts),303.)

   call da_qfrmrh ( P0, T0, 95., Q0 )

   call da_lcl ( P0, Z0, T0, Q0, PLCL, ZLCL, TLCL, QLCL )

   call da_qfrmrh ( PLCL, TLCL, 95., QLCL )

   call da_cumulus ( ZLCL, TLCL, QLCL, PLCL, PP, TB,            &
                  ZC, TC, QC, KCB, KCT, PCT, ZCT, kts, kte )

   DO k = KCB, KCT
      TCV(k) = TC(k) * ( 1. + 0.608 * QC(k) )
      TBV(k) = TB(k) * ( 1. + 0.608 * QB(k) )
   
      B(k) = ( TCV(k)-TBV(k) ) / TBV(k)

      QT(k) = QC(k) + QCWB(k) + QRNB(k)
   ENDDO

   W0(KCB) = 0.0
   DO k = KCB+1, KCT+1
      XX = 1.0+2.0*MU*(ZFB(k)-ZFB(k-1))
      YY = 2.0*G*( B(k-1)/(1.+ALPHA) - QT(k-1) ) * ( ZFB(k)-ZFB(k-1) )
      W0(k) =  ( W0(k-1)+YY ) / XX
   ENDDO
     
   DO k = KCB, KCT+1
      IF ( W0(k) >= 0. ) THEN
         W(k) = SQRT( W0(k) )
      ENDIF
   ENDDO


   DO k = KCT, KCB+1, -1
      IF ( W(k) >= 0.01 ) THEN
         DT(k) = (ZB(k)-ZB(k-1))/W(k)
      ELSE
         DT(k) = 0.0
      ENDIF
   ENDDO

   IF (trace_use) call da_trace_exit("da_cloud_model")

end subroutine da_cloud_model


