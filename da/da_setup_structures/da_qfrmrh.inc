SUBROUTINE da_qfrmrh ( P, T, RH, Q )

   implicit none

   real, intent(IN)    :: P, T, RH
   real, intent(OUT)   :: Q

   real   :: A, B, E, QS

      A=17.26
      B=35.86
      IF(T <= 263.) A=21.87
      IF(T <= 263.) B= 7.66
      E=6.11*EXP(A*(T-273.16)/(T-B))
      QS=0.622*E/(P-0.378*E)
      Q=QS*RH/100.

END SUBROUTINE da_qfrmrh
