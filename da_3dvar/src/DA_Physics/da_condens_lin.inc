SUBROUTINE da_condens_lin(DT,SCR31,SCR42,SCR71,DUM31,PRD,  &
                   QVT,QCT,QRT,TTT,P_B,T_B,QV_B,QCW_B,QRN_B,  &
                   SCR319,SCR429,SCR719,DUM319,PRD9,  &
                   QVT9,QCT9,QRT9,TTT9,P_A,T_A,QV_A,QCW_A,QRN_A,kts,kte)

!  Condensation

   implicit none

   integer                   :: kts, kte, k
!  real                      :: SVP1,SVP2,SVP3,SVPT0,TO,gas_constant_v,XLS
   real, dimension(kts:kte)  :: DT,SCR31,SCR42,SCR71,PRC5,PRD,DUM31
   real, dimension(kts:kte)  :: P_B,T_B,QV_B,QCW_B,QRN_B
   real, dimension(kts:kte)  :: SCR319,SCR429,SCR719,PRC59,PRD9,DUM319
   real, dimension(kts:kte)  :: P_A,T_A,QV_A,QCW_A,QRN_A

   real, dimension(kts:kte)  :: DUM114,DUM115,DUM212,DUM213,  &
                                SCR61,SCR8,QVT,QCT,QRT,TTT
   real, dimension(kts:kte)  :: DUM1149,DUM1159,DUM2129,DUM2139,SCR619,SCR89
   real, dimension(kts:kte)  :: QVT9,QCT9,QRT9,TTT9
   real, dimension(kts:kte)  :: TMP

   DO K=kts, kte

   if (DT(k) <= 0.) cycle

      DUM114(K)=1.E3*SVP1*EXP(SVP2*(SCR71(K)-SVPT0)/(SCR71(K)-SVP3))
      DUM1149(k)=DUM114(K)*SVP2*(SVPT0-SVP3)/(SCR71(K)-SVP3)**2*SCR719(K)

      IF(SCR71(K) > TO) THEN
         DUM2129(K)=2.*DUM31(K)/(gas_constant_v*PRD(K))*DUM319(K)  &
                  -DUM31(K)*DUM31(K)/(gas_constant_v*PRD(K)*PRD(K))*PRD9(K)
         DUM212(K)=DUM31(K)*DUM31(K)/(gas_constant_v*PRD(K))
      ELSE
         DUM2129(K)=XLS*DUM319(K)/(gas_constant_v*PRD(K))  &
                  -XLS*DUM31(K)/(gas_constant_v*PRD(K)*PRD(K))*PRD9(K)
         DUM212(K)=XLS*DUM31(K)/(gas_constant_v*PRD(K))
      ENDIF
      TMP(K)=.622/(P_B(K)-DUM114(K))**2
      PRC59(K)=TMP(K)*P_B(K)*DUM1149(K)-TMP(K)*DUM114(K)*P_A(K)
      PRC5(K)=.622*DUM114(K)/(P_B(K)-DUM114(K))

      IF(SCR42(K) < PRC5(K) .AND. SCR71(K) < TO) THEN
         SCR619(K)=0.
         SCR61(K)=0.
      ELSE
         TMP(K)=1./(1.+DUM212(K)*PRC5(K)/(SCR71(K)*SCR71(K)))
         SCR89(K)=TMP(K)*SCR429(K)  &
                 -TMP(K)*(1.+(SCR42(K)-PRC5(K))*DUM212(K)/  &
                     (SCR71(K)*SCR71(K))*TMP(K))*PRC59(K)  &
                 -TMP(K)*TMP(K)*(SCR42(K)-PRC5(K))*PRC5(K)/  &
                     (SCR71(K)*SCR71(K))*DUM2129(K)  &
!error                -TMP(K)*TMP(K)*2.*DUM212(K)*PRC5(K)  &
!error                    *(SCR42(K)-PRC5(K))/(SCR71(K)*SCR71(K))*SCR719(K)
                 +TMP(K)*TMP(K)*2.*DUM212(K)*PRC5(K)  &
                      *(SCR42(K)-PRC5(K))/(SCR71(K)*SCR71(K)*SCR71(K))*SCR719(K)
         SCR8(K)=(SCR42(K)-PRC5(K))/(1.+DUM212(K)*PRC5(K)/  &
                 (SCR71(K)*SCR71(K)))

         DUM1159(K)=SCR319(K)+SCR89(K)
         DUM115(K)=SCR31(K)+SCR8(K)
         IF(DUM115(K) >= 0.)THEN
            SCR619(K)=SCR89(K)/DT(k)
            SCR61(K)=SCR8(K)/DT(k)
         ELSE
            SCR619(K)=-SCR319(K)/DT(k)
            SCR61(K)=-SCR31(K)/DT(k)
         ENDIF
      ENDIF
      QV_A(K)=QV_A(K)+(QVT9(K)-SCR619(K))*DT(K)
      IF(QV_B(K) < 1.E-25) QV_A(K)=0.
      QCW_A(K)=QCW_A(K)+(QCT9(K)+SCR619(K))*DT(K)
      IF(QCW_B(K) < 1.E-25) QCW_A(K)=0.
      IF(SCR71(K) > TO)THEN
         DUM2139(K)=DUM319(K)/PRD(K)-DUM31(K)/(PRD(K)*PRD(K))*PRD9(K)
         DUM213(K)=DUM31(K)/PRD(K)
      ELSE
         DUM2139(K)=-XLS/(PRD(K)*PRD(K))*PRD9(K)
         DUM213(K)=XLS/PRD(K)
      ENDIF

      QRN_A(K)=QRN_A(K)+DT(K)*QRT9(K)
      IF(QRN_B(K) < 1.E-25) QRN_A(K)=0.
      T_A(K)=T_A(K)+DT(K)*(TTT9(K)+SCR619(K)*DUM213(K)+SCR61(K)*DUM2139(K))

      ENDDO

END SUBROUTINE da_condens_lin
