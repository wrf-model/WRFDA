SUBROUTINE da_condens_adj(DT,SCR31,SCR42,SCR71,DUM31,PRD,  &
                   QVT,QCT,QRT,TTT,P_B,T_B,QV_B,QCW_B,QRN_B,  &
                   SCR319,SCR429,SCR719,DUM319,PRD9,  &
                   QVT9,QCT9,QRT9,TTT9,P_A,T_A,QV_A,QCW_A,QRN_A,kts,kte)

!  Condensation

   implicit none

   integer                   :: kts, kte, k
   real, dimension(kts:kte)  :: DT,SCR31,SCR42,SCR71,PRD,DUM31
   real, dimension(kts:kte)  :: P_B,T_B,QV_B,QCW_B,QRN_B
   real, dimension(kts:kte)  :: SCR319,SCR429,SCR719,PRD9,DUM319
   real, dimension(kts:kte)  :: P_A,T_A,QV_A,QCW_A,QRN_A

   real, dimension(kts:kte)  :: DUM114,DUM115,DUM212,DUM213,PRC5,  &
                                SCR61,SCR8,QVT,QCT,QRT,TTT
   real, dimension(kts:kte)  :: DUM1149,DUM1159,DUM2129,DUM2139,PRC59,  &
                                SCR619,SCR89
   real, dimension(kts:kte)  :: QVT9,QCT9,QRT9,TTT9
   real, dimension(kts:kte)  :: TMP

!  Initilization

   DO K=kts,kte
      DUM2129(K) = 0.
      SCR89 (K) = 0.
      PRC59 (K) = 0.
   ENDDO

   DO K=kts, kte

   if (DT(k) <= 0.) cycle

      DUM114(K)=1.E3*SVP1*EXP(SVP2*(SCR71(K)-SVPT0)/(SCR71(K)-SVP3))

      IF(SCR71(K) > TO) THEN
         DUM212(K)=DUM31(K)*DUM31(K)/(gas_constant_v*PRD(K))
      ELSE
         DUM212(K)=XLS*DUM31(K)/(gas_constant_v*PRD(K))
      ENDIF
      PRC5(K)=.622*DUM114(K)/(P_B(K)-DUM114(K))

      IF(SCR42(K) < PRC5(K) .AND. SCR71(K) < TO) THEN
         SCR61(K)=0.
      ELSE
         SCR8(K)=(SCR42(K)-PRC5(K))/(1.+DUM212(K)*PRC5(K)/  &
                 (SCR71(K)*SCR71(K)))

         DUM115(K)=SCR31(K)+SCR8(K)
         IF(DUM115(K) >= 0.)THEN
            SCR61(K)=SCR8(K)/DT(k)
         ELSE
            SCR61(K)=-SCR31(K)/DT(k)
         ENDIF
      ENDIF
      IF(SCR71(K) > TO)THEN
         DUM213(K)=DUM31(K)/PRD(K)
      ELSE
         DUM213(K)=XLS/PRD(K)
      ENDIF

      TTT9(K)=DT(K)*T_A(K)
      SCR619(K)=DT(K)*DUM213(K)*T_A(K)
      DUM2139(K)=DT(K)*SCR61(K)*T_A(K)
      IF(QRN_B(K) < 1.E-25) QRN_A(K)=0.
      QRT9(K)=DT(K)*QRN_A(K)

      DUM319(K)=0.
      IF(SCR71(K) > TO)THEN
         DUM319(K)=DUM2139(K)/PRD(K)
         PRD9(K)=-DUM31(K)/(PRD(K)*PRD(K))*DUM2139(K)
      ELSE
         PRD9(K)=-XLS/(PRD(K)*PRD(K))*DUM2139(K)
      ENDIF
      IF(QCW_B(K) < 1.E-25) QCW_A(K)=0.
      QCT9(K)=DT(K)*QCW_A(K)
      SCR619(K)=SCR619(K)+DT(K)*QCW_A(K)
      IF(QV_B(K) < 1.E-25) QV_A(K)=0.
      QVT9(K)=DT(K)*QV_A(K)
      SCR619(K)=SCR619(K)-DT(K)*QV_A(K)

      SCR319(K)=0.
      SCR429(K)=0.
      SCR719(K)=0.
      IF(SCR42(K) >= PRC5(K) .OR. SCR71(K) >= TO) THEN
         IF(DUM115(K) >= 0.)THEN
            SCR89(K)=SCR89(K)+SCR619(K)/DT(k)
         ELSE
            SCR319(K)=-SCR619(K)/DT(k)
         ENDIF

         TMP(K)=1./(1.+DUM212(K)*PRC5(K)/(SCR71(K)*SCR71(K)))
         SCR719(K)=TMP(K)*TMP(K)*2.*DUM212(K)*PRC5(K)  &
                   *(SCR42(K)-PRC5(K))/(SCR71(K)*SCR71(K)*SCR71(K))*SCR89(K)
         DUM2129(K)=DUM2129(K)-TMP(K)*TMP(K)*(SCR42(K)-PRC5(K))*PRC5(K)/  &
                     (SCR71(K)*SCR71(K))*SCR89(K)
         PRC59(K)=PRC59(K)-TMP(K)*(1.+(SCR42(K)-PRC5(K))*DUM212(K)/  &
                     (SCR71(K)*SCR71(K))*TMP(K))*SCR89(K)
         SCR429(K)=TMP(K)*SCR89(K)
      ENDIF

      TMP(K)=.622/(P_B(K)-DUM114(K))**2
      DUM1149(K)=TMP(K)*P_B(K)*PRC59(K)
      P_A(K)=P_A(K)-TMP(K)*DUM114(K)*PRC59(K)
      IF(SCR71(K) > TO) THEN
         PRD9(K)=PRD9(K)-DUM31(K)*DUM31(K)/   &
                 (gas_constant_v*PRD(K)*PRD(K))*DUM2129(K)
         DUM319(K)=DUM319(K)+2.*DUM31(K)/(gas_constant_v*PRD(K))*DUM2129(K)
      ELSE
         PRD9(K)=PRD9(K)-XLS*DUM31(K)/(gas_constant_v*PRD(K)*PRD(K))*DUM2129(K)
         DUM319(K)=DUM319(K)+XLS/(gas_constant_v*PRD(K))*DUM2129(K)
      ENDIF
      DUM114(K)=1.E3*SVP1*EXP(SVP2*(SCR71(K)-SVPT0)/(SCR71(K)-SVP3))
      SCR719(K)=SCR719(K)+DUM114(K)*SVP2*(SVPT0-SVP3)/  &
                (SCR71(K)-SVP3)**2*DUM1149(K)

   ENDDO

END SUBROUTINE da_condens_adj
