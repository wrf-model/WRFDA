function da_e_qv_from_rh (RH, T_K, RH0, T_K0, P_PA0) RESULT (QV_KG)

   !--------------------------------------------------------------------------
   !
   !                       function F_QV_FROM_RH
   !                     **************************
   !
   !  PURPOSE: 
   !  -------
   !     TANGENT LINEAR CODE FOR
   !     COMPUTinG MIXinG RATIO FROM RELATIVE HUMIDITY, TEMPERATURE AND PRESSURE
   !
   !     THE ERROR DERIVATION SHOULD useD THE TANGENT LINEAR CODE, NOT THE
   !     ORIGinAL NON-LINEAR CODE.
   !
   !   METHOD:
   !   ------
   !      LINEAR OR LOGARITHMIC VERTICAL inTERPOLATION
   !      out OF BOUND LOCATIONS ARE EXTRAPOLATED
   !
   !   inPUT:
   !   -----
   !      RH:    RELAITVE HUMIDITY in %
   !      P_PA:  PRESSURE          in Pa
   !      T_K:   TEMPERATURE       in K
   !
   !   outPUT:
   !   ------
   !      QV_KG: MIXinG RATIO in kg/kg
   !
   !   COMMON:           NO
   !   -------
   !   EXTERNAL:         NO                   
   !   --------
   !
   !   REFERENCES:
   !   -----------
   !    R. R. ROGERS AND M. K. YAU, 1989: A SHORT COURSE in CLOUD PHYSICS,
   !                                      3ND EDITION, PERGAMON PRESS, PAGE 14-19.
   !
   !   VERifICATION SET:
   !   -----------------
   !    T_K  = 268.15 K,  
   !    TD_K = 262.55 K
   !    RH   = 65 %, 
   !    P_PA = 80000  Pa, 
   !    QV   = 2.11E-03 kg/kg,
   !---------------------------------------------------------------------------

   implicit none

   real T_K , RH , QV_KG
   real P_PA0, T_K0, RH0
   real ES , QS
   real P_MB0, ES0, QS0, QV_KG0

   ! P in mb

   P_MB0 = P_PA0 / 100.

   ! VAPOR PRESSURE in mb

   ES  = 6.112 * 17.67 * 243.5 * T_K /                       &
                 ((T_K0-273.15+243.5)*(T_K0-273.15+243.5)) * &
                 EXP (17.67*(T_K0-273.15)/(T_K0-273.15+243.5))
   ES0 = 6.112 * EXP (17.67*(T_K0-273.15)/(T_K0-273.15+243.5)) 

   ! SATURATION MIXinG RATIO in kg/kg

   QS  = 0.622 * (P_MB0 * ES ) /  &
                ((P_MB0 - ES0) * (P_MB0 - ES0))
   QS0 = 0.622 * ES0 /(P_MB0-ES0)            

   ! MIXinG RATIO in kg/kg

   QV_KG  = 0.01 * (RH0 * QS + RH * QS0)
   QV_KG0 = 0.01 * RH0 * QS0

   ! Mixing ratio must be positive

   if (QV_KG0 < 0.) QV_KG = 0.

end function da_e_qv_from_rh


