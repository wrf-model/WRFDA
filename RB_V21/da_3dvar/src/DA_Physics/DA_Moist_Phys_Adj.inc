SUBROUTINE DA_Moist_Phys_Adj( xb, xa, xp,    &
                ids,ide, jds,jde, kds,kde,  &
                ims,ime, jms,jme, kms,kme,  &
                its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Partition of the hydrometeors via the moist explicit scheme.
!           A warm rain process is used in this subroutine. 
!           This is the adjoint code of the scheme.
!
!  METHOD: The warm rain process is according to Hsie and Anthes (1984)
!          and Dudhia (1989)
!
!  ASSUMPTIONS: 1) Model level stored top down.
!
!  HISTORY: 08/22/2003 - Creation of F90 version. Qingnong Xiao
!  HISTORY: July 2004 - Creation in WRF space. Jianfeng Gu
!------------------------------------------------------------------------------

   implicit none

   TYPE (xb_type), INTENT(IN)    :: xb           ! First guess structure.
   TYPE (x_type), INTENT(INOUT)  :: xa           ! Increment structure.
   TYPE (xpose_type), INTENT(INOUT) :: xp        ! Dimensions and xpose buffers.

   integer         :: ids,ide, jds,jde, kds,kde
   integer         :: ims,ime, jms,jme, kms,kme
   integer         :: its,ite, jts,jte, kts,kte

   REAL, dimension(ims:ime,jms:jme,kms:kme) :: T_OLD,T_NEW
   REAL, dimension(ims:ime,jms:jme,kms:kme) :: Q_OLD,Q_NEW
   REAL, dimension(ims:ime,jms:jme,kms:kme) :: QCW_OLD,QCW_NEW
   REAL, dimension(ims:ime,jms:jme,kms:kme) :: QRN_OLD,QRN_NEW

   real, dimension(kms:kme)                 :: EES, QVSS
   real, dimension(kms:kme)                 :: EES9, QVSS9

   real, dimension(kms:kme)                   :: DT
   real, dimension(kms:kme)                   :: QVT,QCT,QRT,TTT
   real, dimension(kms:kme)                   :: QVT9,QCT9,QRT9,TTT9
   real, dimension(kms:kme) :: SCR2,SCR3,SCR4,SCR5,SCR6,SCR7,SCR8
   real, dimension(kms:kme) :: DUM11,DUM21,DUM31,DUM41
   real, dimension(kms:kme) :: PRA,PRC,PRD,PRE
   real, dimension(kms:kme) :: SCR31,SCR41,SCR42,SCR71
   real, dimension(kms:kme) :: DUM111,DUM112,DUM113,DUM211,DUM411
   real, dimension(kms:kme) :: PRA1,PRC1,PRE1,PRA2,PRC2

   real, dimension(kms:kme) :: SCR29,SCR39,SCR49,SCR59,SCR69,SCR79
   real, dimension(kms:kme) :: DUM119,DUM219,DUM319,DUM419
   real, dimension(kms:kme) :: PRA9,PRC9,PRD9,PRE9
   real, dimension(kms:kme) :: SCR319,SCR419,SCR429,SCR719
   real, dimension(kms:kme) :: DUM1119,DUM1129,DUM1139,DUM2119,DUM4119
   real, dimension(kms:kme) :: PRA19,PRC19,PRE19
   real, dimension(kms:kme) :: TMP

   integer :: i, j, k
   real    :: G3PB,G5PB
   real    :: PRAC,PREC1,PREC2

!  Constants

   G3PB=DA_GAMMA(3.+BVT)
   G5PB=DA_GAMMA(0.5*(5.+BVT))
   PRAC=pi*N0R*AVT*G3PB*0.25
   PREC1=2.*pi*N0R*0.78
   PREC2=2.*pi*N0R*0.32*AVT**0.5*G5PB

   DO k=kts,kte
      DO j=jts,jte
      DO i=its,ite
         QRN_NEW(i,j,k) = xa % qrn (i,j,k) 
         QRN_OLD(i,j,k) = xa % qrn (i,j,k)
         QCW_NEW(i,j,k) = xa % qcw (i,j,k)
         QCW_OLD(i,j,k) = xa % qcw (i,j,k)
         Q_NEW(i,j,k) = xa % q (i,j,k)
         Q_OLD(i,j,k) = xa % q (i,j,k)
         T_NEW(i,j,k) = xa % t (i,j,k)
         T_OLD(i,j,k) = xa % t (i,j,k)
      ENDDO
      ENDDO
   ENDDO

   call da_filter_adj(t_new, xp)
   call da_filter_adj(q_new, xp)
   call da_filter_adj(qcw_new, xp)
   call da_filter_adj(qrn_new, xp)

   DO k=kts,kte
      DO j=jts,jte
      DO i=its,ite
         xa % qrn (i,j,k) = QRN_NEW(i,j,k)
         QRN_OLD(i,j,k) = QRN_OLD(i,j,k) - QRN_NEW(i,j,k)
         xa % qcw (i,j,k) = QCW_NEW(i,j,k)
         QCW_OLD(i,j,k) = QCW_OLD(i,j,k) - QCW_NEW(i,j,k)
         xa % q (i,j,k) = Q_NEW(i,j,k)
         Q_OLD(i,j,k) = Q_OLD(i,j,k) - Q_NEW(i,j,k)
         xa % t (i,j,k) = T_NEW(i,j,k)
         T_OLD(i,j,k) = T_OLD(i,j,k) - T_NEW(i,j,k)

      ENDDO
      ENDDO

   ENDDO

   DO j=jts,jte
   DO i=its,ite

!  Preparation

     if ( DT_cloud_model ) then
       DO K=kts,kte
         DT(k) = xb%delt(i,j,k)
       ENDDO
     else
      CALL da_wdt(xb%h(I,J,:),xb%w(I,J,:),xb%terr(I,J),DT,kts,kte)
     endif

      DO K=kts,kte

         if (DT(k) <= 0.) cycle

         IF( xb%t(I,J,K) > TO )THEN
            EES(K)=SVP1*EXP(SVP2*(xb%t(I,J,K)-SVPT0)/(xb%t(I,J,K)-SVP3))
         ELSE
            EES(K)=.611*EXP(22.514-6.15E3/xb%t(I,J,K))
         ENDIF

         QVSS(K)=622.*EES(K)/(xb%p(I,J,K)-EES(K))

         SCR4(K)=xb%q(I,J,K)/QVSS(K)

      ENDDO

      DO K=kts,kte

         if (DT(k) <= 0.) cycle

         IF(xb%qcw(I,J,K) > 0.) THEN
            SCR2(K)=xb%qcw(I,J,K)
         ELSE
            SCR2(K)=0.
         ENDIF
         IF(xb%qrn(I,J,K) > 1.E-25) THEN
            SCR3(K)=xb%qrn(I,J,K)
         ELSE
            SCR3(K)=1.E-25
         ENDIF
         SCR5(K)=xb%q(I,J,K)/SCR4(K)

         SCR6(K)=xb%p(I,J,K)/(gas_constant*xb%t(I,J,K))

         IF( xb%t(I,J,K) > TO ) THEN
            SCR7(K)=(SCR6(K)*SCR3(K)*PPI*0.001)**0.25
         ELSE
            SCR7(K)=(SCR6(K)*SCR3(K)*PPIS*0.001)**0.25
         ENDIF

         DUM11(K)=8.794E-5*xb%t(I,J,K)**1.81/xb%p(I,J,K)

         DUM21(K)=1.496E-6*xb%t(I,J,K)**1.5/(xb%t(I,J,K)+120.)
         DUM31(K)=3.1484E6-XLV1*xb%t(I,J,K)
         SCR41(K)=1.414E3*DUM21(K)
         IF(xb%t(I,J,K) > TO)THEN
            PRA1(K)=DUM31(K)
         ELSE
            PRA1(K)=XLS
         ENDIF
         DUM41(K)=SCR6(K)*PRA1(K)*PRA1(K)/(SCR41(K)*  &
                  gas_constant_v*xb%t(I,J,K)*xb%t(I,J,K))+1./(SCR5(K)*DUM11(K))
         PRE1(K)=DUM21(K)/SCR6(K)
         PRC1(K)=PRE1(K)/DUM11(K)
         DUM111(K)=PRC1(K)**.3333/PRE1(K)**.5
      ENDDO

      CALL da_autoc(DT,SCR2,PRC,kts,kte)
      DO k=kts,kte
         PRC2(K)=PRC(K)
      ENDDO

      CALL da_accre(DT,SCR2,SCR3,SCR7,PRAC,BVT3,PRA,kts,kte)
      DO k=kts,kte
         PRA2(K)=PRA(K)
      ENDDO

      CALL da_evapo(DT,SCR3,SCR5,SCR7,DUM111,DUM41,xb%q(I,J,:),  &
                 PREC1,PREC2,PRE,kts,kte)

      DO K=kts, kte

      if (DT(k) <= 0.) cycle

!  Readjust

         DUM112(K)=(PRC(k)+PRA(k))*DT(k)
         if (DUM112(K) > SCR2(k)) then
            PRC(k)=SCR2(K)*PRC(K)/DUM112(K)
            PRA(k)=SCR2(K)*PRA(K)/DUM112(K)
         endif
         QVT(K)=-PRE(K)
         QCT(K)=-PRC(K)-PRA(K)
         QRT(K)=PRC(K)+PRA(K)+PRE(K)
         IF(xb%t(I,J,K).GT.TO)THEN
            DUM411(K)=DUM31(K)
         ELSE
            DUM411(K)=XLS
         ENDIF
         PRD(K)=cp*(1.+0.887*xb%q(I,J,K))
         TTT(K)=-DUM411(K)*QVT(K)/PRD(K)

         DUM113(K)=xb%q(I,J,K)+DT(K)*QVT(K)
         IF(DUM113(K) > 1.E-12 ) THEN
            SCR42(K)=DUM113(K)
         ELSE
            SCR42(K)=1.E-12
         ENDIF
         DUM211(K)=xb%qcw(I,J,K)+QCT(K)*DT(k)
         IF(DUM211(K) > 0.) THEN
            SCR31(K)=DUM211(K)
         ELSE
            SCR31(K)=0.
         ENDIF
         SCR71(K)=xb%t(I,J,K)+TTT(K)*DT(k)
      ENDDO

      CALL da_condens_adj(DT,SCR31,SCR42,SCR71,DUM31,PRD,       &
                          QVT,QCT,QRT,TTT,                      &
                          xb%p(I,J,:),xb%t(I,J,:),xb%q(I,J,:),  &
                          xb%qcw(I,J,:),xb%qrn(I,J,:),          &
                          SCR319,SCR429,SCR719,DUM319,PRD9,     &
                          QVT9,QCT9,QRT9,TTT9,                  &
                          xa%p(I,J,:),xa%t(I,J,:),xa%q(I,J,:),  &
                          xa%qcw(I,J,:),xa%qrn(I,J,:),kts,kte)

      DO K=kts, kte

      if (DT(k) <= 0.) cycle

!  Readjust

         xa%t(I,J,K)=xa%t(I,J,K)+SCR719(K)
         TTT9(K)=TTT9(K)+DT(k)*SCR719(K)
         DUM2119(K)=0.
         IF(DUM211(K) > 0.) THEN
            DUM2119(K)=SCR319(K)
         ENDIF
         xa%qcw(I,J,K)=xa%qcw(I,J,K)+DUM2119(K)
         QCT9(K)=QCT9(K)+DT(K)*DUM2119(K)
         DUM1139(K)=0.
         IF(DUM113(K) > 1.E-12 ) THEN
            DUM1139(K)=SCR429(K)
         ENDIF
         xa%q(I,J,K)=xa%q(I,J,K)+DUM1139(K)
         QVT9(K)=QVT9(K)+DT(K)*DUM1139(K)
         PRD9(K)=PRD9(K)+DUM411(K)*QVT(K)/(PRD(K)*PRD(K))*TTT9(K)
         QVT9(K)=QVT9(K)-DUM411(K)/PRD(K)*TTT9(K)
         DUM4119(K)=-QVT(K)/PRD(K)*TTT9(K)
         xa%q(I,J,K)=xa%q(I,J,K)+cp*0.887*PRD9(K)

         IF(xb%t(I,J,K).GT.TO)THEN
            DUM319(K)=DUM319(K)+DUM4119(K)
         ENDIF
         PRC9(K)=QRT9(K)
         PRA9(K)=QRT9(K)
         PRE9(K)=QRT9(K)
         PRC9(K)=PRC9(K)-QCT9(K)
         PRA9(K)=PRA9(K)-QCT9(K)
         PRE9(K)=PRE9(K)-QVT9(K)

         SCR29(K)=0.
         DUM1129(K)=0.
         if (DUM112(K) > SCR2(k)) then
            DUM1129(K)=-SCR2(K)*PRA2(K)/(DUM112(K)*DUM112(K))*PRA9(K)
            SCR29(K)=PRA2(K)/DUM112(K)*PRA9(K)
            PRA9(K)=PRA9(K)*SCR2(K)/DUM112(K)
            DUM1129(K)=DUM1129(K)-SCR2(K)*PRC2(K)/(DUM112(K)*DUM112(K))*PRC9(K)
            SCR29(K)=SCR29(K)+PRC2(K)/DUM112(K)*PRC9(K)
            PRC9(K)=PRC9(K)*SCR2(K)/DUM112(K)
         endif
         PRC9(K)=PRC9(K)+DT(K)*DUM1129(K)
         PRA9(K)=PRA9(K)+DT(K)*DUM1129(K)

      ENDDO
       
      CALL da_evapo_adj(DT,SCR3,SCR5,SCR7,DUM111,DUM41,xb%q(I,J,:),  &
                 PREC1,PREC2,PRE,  &
                 SCR39,SCR59,SCR79,DUM1119,DUM419,xa%q(I,J,:),PRE9,kts,kte)

      CALL da_accre_adj(DT,SCR2,SCR3,SCR7,PRAC,BVT3,PRA,  &
                  SCR29,SCR79,PRA9,kts,kte)

      CALL da_autoc_adj(DT,SCR2,PRC,SCR29,PRC9,kts,kte)

!  Preparation

      DO K=kts,kte

         if (DT(k) <= 0.) cycle

         PRC19(K)=.3333/(PRC1(K)**.6667*PRE1(K)**.5)*DUM1119(K)
         PRE19(K)=-0.5*PRC1(K)**0.3333/PRE1(K)**1.5*DUM1119(K)
         DUM119(K)=-PRE1(K)/(DUM11(K)*DUM11(K))*PRC19(K)
         PRE19(K)=PRE19(K)+PRC19(K)/DUM11(K)
         DUM219(K)=PRE19(K)/SCR6(K)
         SCR69(K)=-DUM21(K)/(SCR6(K)*SCR6(K))*PRE19(K)
         TMP(K)=SCR41(K)*gas_constant_v*xb%t(I,J,K)*xb%t(I,J,K)
         SCR69(K)=SCR69(K)+PRA1(K)*PRA1(K)/TMP(K)*DUM419(K)
         PRA19(K)=2.*SCR6(K)*PRA1(K)/TMP(K)*DUM419(K)
         xa%t(I,J,K)=xa%t(I,J,K)-SCR6(K)*PRA1(K)*PRA1(K)*2.*   &
                     SCR41(K)*gas_constant_v*xb%t(I,J,K)/TMP(K)**2*DUM419(K)
         SCR419(K)=-SCR6(K)*PRA1(K)*PRA1(K)*gas_constant_v*xb%t(I,J,K)**2  &
                     /TMP(K)**2*DUM419(K)
         DUM119(K)=DUM119(K)-DUM419(K)/(DUM11(K)*DUM11(K)*SCR5(K))
         SCR59(K)=SCR59(K)-DUM419(K)/(DUM11(K)*SCR5(K)*SCR5(K))
         IF(xb%t(I,J,K) > TO)THEN
            DUM319(K)=DUM319(K)+PRA19(K)
         ENDIF
         DUM219(K)=DUM219(K)+1.414E3*SCR419(K)
         xa%t(I,J,K)=xa%t(I,J,K)-XLV1*DUM319(K)
         xa%t(I,J,K)=xa%t(I,J,K)+ &
                   (1.496E-6*1.5*xb%t(I,J,K)**0.5/(xb%t(I,J,K)+120.)   &
                   -1.496E-6*xb%t(I,J,K)**1.5/(xb%t(I,J,K)+120.)**2) * &
                   DUM219(K)
         xa%t(I,J,K)=xa%t(I,J,K)+8.794E-5*1.81*xb%t(I,J,K)**0.81/   &
                     xb%p(I,J,K)*DUM119(K)
         xa%p(I,J,K)=xa%p(I,J,K)-8.794E-5*xb%t(I,J,K)**1.81/   &
                     xb%p(I,J,K)**2*DUM119(K)

         IF( xb%t(I,J,K) > TO ) THEN
            TMP(K)=0.25*PPI*0.001/(SCR6(K)*SCR3(K)*PPI*0.001)**0.75
            SCR69(K)=SCR69(K)+TMP(K)*SCR3(K)*SCR79(K)
            SCR39(K)=SCR39(K)+TMP(K)*SCR6(K)*SCR79(K)
         ELSE
            TMP(K)=0.25*PPIS*0.001/(SCR6(K)*SCR3(K)*PPIS*0.001)**0.75
            SCR69(K)=SCR69(K)+TMP(K)*SCR3(K)*SCR79(K)
            SCR39(K)=SCR39(K)+TMP(K)*SCR6(K)*SCR79(K)
         ENDIF
         xa%p(I,J,K)=xa%p(I,J,K)+SCR69(K)/(gas_constant*xb%t(I,J,K))
         xa%t(I,J,K)=xa%t(I,J,K)-xb%p(I,J,K)/  &
                     (gas_constant*xb%t(I,J,K)**2)*SCR69(K)
         xa%q(I,J,K)=xa%q(I,J,K)+SCR59(K)/SCR4(K)
         SCR49(K)=-xb%q(I,J,K)/SCR4(K)**2*SCR59(K)
         
         IF(xb%qrn(I,J,K) > 1.E-25) THEN
            xa%qrn(I,J,K)=xa%qrn(I,J,K)+SCR39(K)
         ENDIF
         IF(xb%qcw(I,J,K) > 0.) THEN
            xa%qcw(I,J,K)=xa%qcw(I,J,K)+SCR29(K)
         ENDIF

      ENDDO

      DO K=kts,kte

         if (DT(k) <= 0.) cycle

         xa%q(I,J,K)=xa%q(I,J,K)+SCR49(K)/QVSS(K)
         QVSS9(K)=-xb%q(I,J,K)/QVSS(K)**2*SCR49(K)
         TMP(K)=622./((xb%p(I,J,K)-EES(K))**2)
         EES9(K)=TMP(K)*xb%p(I,J,K)*QVSS9(K)
         xa%p(I,J,K)=xa%p(I,J,K)-TMP(K)*EES(K)*QVSS9(K)
         IF( xb%t(I,J,K) > TO )THEN
            xa%t(I,J,K)=xa%t(I,J,K)+EES(K)*SVP2*(SVPT0-SVP3)/   &
                        ((xb%t(I,J,K)-SVP3)*(xb%t(I,J,K)-SVP3))*EES9(K)
         ELSE
            xa%t(I,J,K)=xa%t(I,J,K)+EES(K)*6.15E3/(xb%t(I,J,K)*   &
                        xb%t(I,J,K))*EES9(K)
         ENDIF

      ENDDO

      DO K=kts,kte

         xa % qt (i,j,k) = xa % qt (i,j,k) + xa % q(i,j,k)
         xa % qcw(i,j,k) = xa % qcw(i,j,k) - xa % q(i,j,k)
         xa % qrn(i,j,k) = xa % qrn(i,j,k) - xa % q(i,j,k)

      ENDDO

   ENDDO
   ENDDO

   DO k=kts,kte
   DO j=jts,jte
   DO i=its,ite

      xa % qrn (i,j,k) = xa % qrn (i,j,k) + QRN_OLD(i,j,k)
      xa % qcw (i,j,k) = xa % qcw (i,j,k) + QCW_OLD(i,j,k)
      xa % q (i,j,k) = xa % q (i,j,k) + Q_OLD(i,j,k)
      xa % t (i,j,k) = xa % t (i,j,k) + T_OLD(i,j,k)

   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE DA_Moist_Phys_Adj
