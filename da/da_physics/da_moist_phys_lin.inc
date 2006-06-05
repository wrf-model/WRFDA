SUBROUTINE DA_Moist_Phys_Lin( xb, xa, xp,  &
                ids,ide, jds,jde, kds,kde,  &
                ims,ime, jms,jme, kms,kme,  &
                its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Partition of the hydrometeors via the moist explicit scheme.
!           A warm rain process is used in this subroutine.
!           This is the tangent linear code of the scheme.
!
!  METHOD: The warm rain process is according to Hsie and Anthes (1984)
!          and Dudhia (1989)
!
!  ASSUMPTIONS: 1) Model level stored top down.
!
!  HISTORY: 08/22/2003 - Creation of F90 version. Qingnong Xiao
!  HISTORY: July 2004 - Creation in WRF FRAME of F90 version.  Jianfeng Gu
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
   real, dimension(kms:kme) :: PRA1,PRC1,PRE1

   real, dimension(kms:kme) :: SCR29,SCR39,SCR49,SCR59,SCR69,SCR79,SCR89
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

     T_OLD(i,j,k) = xa % t (i,j,k)
     Q_OLD(i,j,k) = xa % q (i,j,k)
     QCW_OLD(i,j,k) = xa % qcw (i,j,k)
     QRN_OLD(i,j,k) = xa % qrn (i,j,k)

   ENDDO
   ENDDO
   ENDDO

   DO j=jts,jte
   DO i=its,ite

!  Preparation

      DO K=kts,kte
        xa % q(i,j,k) =xa % qt(i,j,k) - xa % qcw(i,j,k) - xa %qrn(i,j,k)
      ENDDO

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
            EES9(K)=EES(K)*SVP2*(SVPT0-SVP3)/((xb%t(I,J,K)-SVP3)*  &
                    (xb%t(I,J,K)-SVP3))*xa%t(I,J,K)
         ELSE
            EES(K)=.611*EXP(22.514-6.15E3/xb%t(I,J,K))
            EES9(K)=EES(K)*6.15E3/(xb%t(I,J,K)*xb%t(I,J,K))*xa%t(I,J,K)
         ENDIF

         TMP(K)=622./((xb%p(I,J,K)-EES(K))**2)
         QVSS9(K)=TMP(K)*xb%p(I,J,K)*EES9(K)  &
               -TMP(K)*EES(K)*xa%p(I,J,K)
         QVSS(K)=622.*EES(K)/(xb%p(I,J,K)-EES(K))


         SCR49(K)=xa%q(I,J,K)/QVSS(K)-xb%q(I,J,K)/QVSS(K)**2*QVSS9(K)
         SCR4(K)=xb%q(I,J,K)/QVSS(K)
      ENDDO

      DO K=kts,kte

         if (DT(k) <= 0.) cycle

         IF(xb%qcw(I,J,K) > 0.) THEN
            SCR29(K)=xa%qcw(I,J,K)
            SCR2(K)=xb%qcw(I,J,K)
         ELSE
            SCR29(K)=0.
            SCR2(K)=0.
         ENDIF
         IF(xb%qrn(I,J,K) > 1.E-25) THEN
            SCR39(K)=xa%qrn(I,J,K)
            SCR3(K)=xb%qrn(I,J,K)
         ELSE
            SCR39(K)=0.
            SCR3(K)=1.E-25
         ENDIF
         SCR59(K)=xa%q(I,J,K)/SCR4(K)-xb%q(I,J,K)/SCR4(K)**2*SCR49(K)
         SCR5(K)=xb%q(I,J,K)/SCR4(K)

         SCR69(K)=xa%p(I,J,K)/(gas_constant*xb%t(I,J,K))-xb%p(I,J,K)/  &
                  (gas_constant*xb%t(I,J,K)**2)*xa%t(I,J,K)
         SCR6(K)=xb%p(I,J,K)/(gas_constant*xb%t(I,J,K))


         IF( xb%t(I,J,K) > TO ) THEN
            TMP(K)=0.25*PPI*0.001/(SCR6(K)*SCR3(K)*PPI*0.001)**0.75
            SCR79(K)=TMP(K)*SCR3(K)*SCR69(K)+TMP(K)*SCR6(K)*SCR39(K)
            SCR7(K)=(SCR6(K)*SCR3(K)*PPI*0.001)**0.25
         ELSE
            TMP(K)=0.25*PPIS*0.001/(SCR6(K)*SCR3(K)*PPIS*0.001)**0.75
            SCR79(K)=TMP(K)*SCR3(K)*SCR69(K)+TMP(K)*SCR6(K)*SCR39(K)
            SCR7(K)=(SCR6(K)*SCR3(K)*PPIS*0.001)**0.25
         ENDIF

         DUM119(K)=8.794E-5*1.81*xb%t(I,J,K)**0.81/xb%p(I,J,K)     &
                   *xa%t(I,J,K)-8.794E-5*xb%t(I,J,K)**1.81/        &
                   xb%p(I,J,K)**2*xa%p(I,j,K)
         DUM11(K)=8.794E-5*xb%t(I,J,K)**1.81/xb%p(I,J,K)


         DUM219(K)=(1.496E-6*1.5*xb%t(I,J,K)**0.5/(xb%t(I,J,K)+120.)   &
                   -1.496E-6*xb%t(I,J,K)**1.5/(xb%t(I,J,K)+120.)**2)   &
                   *xa%t(I,J,K)
         DUM21(K)=1.496E-6*xb%t(I,J,K)**1.5/(xb%t(I,J,K)+120.)
         DUM319(K)=-XLV1*xa%t(I,J,K) 
         DUM31(K)=3.1484E6-XLV1*xb%t(I,J,K)
         SCR419(K)=1.414E3*DUM219(K)
         SCR41(K)=1.414E3*DUM21(K)
         IF(xb%t(I,J,K) > TO)THEN
            PRA19(K)=DUM319(K)
            PRA1(K)=DUM31(K)
         ELSE
            PRA19(K)=0.
            PRA1(K)=XLS
         ENDIF
         TMP(K)=SCR41(K)*gas_constant_v*xb%t(I,J,K)*xb%t(I,J,K)
         DUM419(K)=PRA1(K)*PRA1(K)/TMP(K)*SCR69(K)  &
                  +2.*SCR6(K)*PRA1(K)/TMP(K)*PRA19(K)  &
                  -SCR6(K)*PRA1(K)*PRA1(K)*2.*SCR41(K)*  &
                     gas_constant_v*xb%t(I,J,K)  &
                     /TMP(K)**2*xa%t(I,J,K)  &
                  -SCR6(K)*PRA1(K)*PRA1(K)*gas_constant_v*xb%t(I,J,K)**2  &
                     /TMP(K)**2*SCR419(K)  &
                  -DUM119(K)/(DUM11(K)*DUM11(K)*SCR5(K))  &
                  -SCR59(K)/(DUM11(K)*SCR5(K)*SCR5(K))
         DUM41(K)=SCR6(K)*PRA1(K)*PRA1(K)/(SCR41(K)*gas_constant_v*  &
                  xb%t(I,J,K)*xb%t(I,J,K))+1./(SCR5(K)*DUM11(K))
         PRE19(K)=DUM219(K)/SCR6(K)-DUM21(K)/(SCR6(K)*SCR6(K))*SCR69(K)
         PRE1(K)=DUM21(K)/SCR6(K)
         PRC19(K)=PRE19(K)/DUM11(K)-PRE1(K)/(DUM11(K)*DUM11(K))*DUM119(K)
         PRC1(K)=PRE1(K)/DUM11(K)
         DUM1119(K)=.3333/(PRC1(K)**.6667*PRE1(K)**.5)*PRC19(K)  &
                   -0.5*PRC1(K)**0.3333/PRE1(K)**1.5*PRE19(K)
         DUM111(K)=PRC1(K)**.3333/PRE1(K)**.5
      ENDDO

      CALL da_autoc_lin(DT,SCR2,PRC,SCR29,PRC9,kts,kte)

      CALL da_accre_lin(DT,SCR2,SCR3,SCR7,PRAC,BVT3,PRA,  &
                  SCR29,SCR79,PRA9,kts,kte)

      CALL da_evapo_lin(DT,SCR3,SCR5,SCR7,DUM111,DUM41,xb%q(I,J,:),  &
                 PREC1,PREC2,PRE,  &
                 SCR39,SCR59,SCR79,DUM1119,DUM419,xa%q(I,J,:),PRE9,kts,kte)

      DO K=kts, kte

      if (DT(k) <= 0.) cycle

!  Readjust

         DUM1129(K)=(PRC9(k)+PRA9(k))*DT(k)
         DUM112(K)=(PRC(k)+PRA(k))*DT(k)
         if (DUM112(K) > SCR2(k)) then
            PRC9(K)=SCR29(K)*PRC(K)/DUM112(K)  &
                   +PRC9(K)*SCR2(K)/DUM112(K)  &
                   -SCR2(K)*PRC(K)/(DUM112(K)*DUM112(K))*DUM1129(K)
            PRC(k)=SCR2(K)*PRC(K)/DUM112(K)
            PRA9(K)=SCR29(K)*PRA(K)/DUM112(K)  &
                   +PRA9(K)*SCR2(K)/DUM112(K)  &
                   -SCR2(K)*PRA(K)/(DUM112(K)*DUM112(K))*DUM1129(K)
            PRA(k)=SCR2(K)*PRA(K)/DUM112(K)
         endif
         QVT9(K)=-PRE9(K)
         QVT(K)=-PRE(K)
         QCT9(K)=-PRC9(K)-PRA9(K)
         QCT(K)=-PRC(K)-PRA(K)
         QRT9(K)=PRC9(K)+PRA9(K)+PRE9(K)
         QRT(K)=PRC(K)+PRA(K)+PRE(K)
         IF(xb%t(I,J,K).GT.TO)THEN
            DUM4119(K)=DUM319(K)
            DUM411(K)=DUM31(K)
         ELSE
            DUM4119(K)=0.
            DUM411(K)=XLS
         ENDIF
         PRD9(K)=cp*0.887*xa%q(I,J,K)
         PRD(K)=cp*(1.+0.887*xb%q(I,J,K))
         TTT9(K)=-DUM4119(K)*QVT(K)/PRD(K)  &
                -QVT9(K)*DUM411(K)/PRD(K)  &
                +DUM411(K)*QVT(K)/(PRD(K)*PRD(K))*PRD9(K)
         TTT(K)=-DUM411(K)*QVT(K)/PRD(K)

         DUM1139(K)=xa%q(I,J,K)+DT(K)*QVT9(K)
         DUM113(K)=xb%q(I,J,K)+DT(K)*QVT(K)
         IF(DUM113(K) > 1.E-12 ) THEN
            SCR429(K)=DUM1139(K)
            SCR42(K)=DUM113(K)
         ELSE
            SCR429(K)=0.
            SCR42(K)=1.E-12
         ENDIF
         DUM2119(K)=xa%qcw(I,J,K)+QCT9(K)*DT(k)
         DUM211(K)=xb%qcw(I,J,K)+QCT(K)*DT(k)
         IF(DUM211(K) > 0.) THEN
            SCR319(K)=DUM2119(K)
            SCR31(K)=DUM211(K)
         ELSE
            SCR319(K)=0.
            SCR31(K)=0.
         ENDIF
         SCR719(K)=xa%t(I,J,K)+TTT9(K)*DT(k)
         SCR71(K)=xb%t(I,J,K)+TTT(K)*DT(k)
      ENDDO

      CALL da_condens_lin(DT,SCR31,SCR42,SCR71,DUM31,PRD,         &
                          QVT,QCT,QRT,TTT,                        &
                          xb%p(I,J,:),xb%t(I,J,:),xb%q(I,J,:),    &
                          xb%qcw(I,J,:),xb%qrn(I,J,:),            &
                          SCR319,SCR429,SCR719,DUM319,PRD9,       &
                          QVT9,QCT9,QRT9,TTT9,                    &
                          xa%p(I,J,:),xa%t(I,J,:),xa%q(I,J,:),    &
                          xa%qcw(I,J,:),xa%qrn(I,J,:),kts,kte)
       

   ENDDO
   ENDDO

   DO k=kds,kde
      DO j=jts,jte
      DO i=its,ite
         T_NEW(i,j,k) = xa % t (i,j,k) - T_OLD(i,j,k)
         Q_NEW(i,j,k) = xa % q (i,j,k) - Q_OLD(i,j,k)
         QCW_NEW(i,j,k) = xa % qcw (i,j,k) - QCW_OLD(i,j,k)
         QRN_NEW(i,j,k) = xa % qrn (i,j,k) - QRN_OLD(i,j,k)
      ENDDO
      ENDDO
   ENDDO

   call da_filter(t_new, xp)
   call da_filter(q_new, xp)
   call da_filter(qcw_new, xp)
   call da_filter(qrn_new, xp)

   DO k=kds,kde
      DO j=jts,jte
      DO i=its,ite
         xa % t (i,j,k) = T_NEW(i,j,k) + T_OLD(i,j,k)
         xa % q (i,j,k) = Q_NEW(i,j,k) + Q_OLD(i,j,k)
         xa % qcw (i,j,k) = QCW_NEW(i,j,k) + QCW_OLD(i,j,k)
         xa % qrn (i,j,k) = QRN_NEW(i,j,k) + QRN_OLD(i,j,k)
      ENDDO
      ENDDO
   ENDDO

END SUBROUTINE DA_Moist_Phys_Lin
