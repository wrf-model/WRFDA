subroutine da_qfrmrh ( P, T, RH, Q )

   implicit none

   real, intent(in)    :: P, T, RH
   real, intent(out)   :: Q

   real   :: A, B, E, QS

   A=17.26
   B=35.86
   if (T <= 263.) A=21.87
   if (T <= 263.) B= 7.66
   E=6.11*EXP(A*(T-273.16)/(T-B))
   QS=0.622*E/(P-0.378*E)
   Q=QS*RH/100.

end subroutine da_qfrmrh


