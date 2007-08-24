subroutine da_predictor_crtm(pred,npred,nlevels,temp,hum,T_skin,pm,pf)

   implicit none

   ! temp - model level temperatures (K)
   ! hum  - model level moistures    (g/kg)
   ! T-skin - model skin temperature (K)
   ! nlevels - number of model levels (0:model top)
   ! pm   - model level pressure (hPa)
   ! pf   - full level pressure  (hPa)

   ! pred(1) - 1000-300 thickness 43 1005.43-521.46 thickness
   ! pred(2) - 200-50 thickness   43  194.36-56.73  thickness
   ! pred(3) - T_skin
   ! pred(4) - total column precipitable water
   ! pred(5) - 10 -2 thickness    43  10.37-2.61 thickness
   ! pred(6) - 50 -10thickness    43  45.29-10.37 thickness

   real, parameter :: Kth = gas_constant/gravity
   real, parameter :: Kpc = 100.0/gravity

   integer, intent(in)  :: npred,nlevels
   real, intent(in)     :: temp(nlevels), hum(nlevels), T_skin
   real, intent(in)     :: pm(nlevels), pf(0:nlevels)
   real, intent(out)    :: pred(npred)

   real :: tv(nlevels), qm(nlevels)

   !logical, SAVE :: FIRST = .true.

   !real :: PLEVS(JPRTLEV) =  (/ 0.10,    0.29,    0.69,   1.42,    2.61, &
   !                             4.41,    6.95,   10.37,  14.81,   20.40, &
   !                             27.26,   35.51,   45.29,  56.73,   69.97, &
   !                             85.18,  102.05,  122.04, 143.84,  167.95, &
   !                            194.36,  222.94,  253.71, 286.60,  321.50, &
   !                            358.28,  396.81,  436.95, 478.54,  521.46, &
   !                            565.54,  610.60,  656.43, 702.73,  749.12, &
   !                            795.09,  839.95,  882.80, 922.46,  957.44, &
   !                            985.88, 1005.43, 1013.25 /)

   real :: DLP(nlevels), DP(nlevels)

   qm=hum*0.001  ! g/kg to kg/kg

   ! If first time compute log P(i)/P(i-1) and P(i)-P(i-1)
   !if (FIRST) then
     DLP(1:nlevels) = LOG(pf(1:nlevels)) - LOG(pf(0:nlevels-1))
      DP(1:nlevels) =     pf(1:nlevels)  -     pf(0:nlevels-1)
     !FIRST = .false.
   !end if

   ! 1.0 Convert all temperatures to virtual temperatures
   ! ----------------------------------------------------
   tv =  temp/(1.0 - 0.6*qm)

   ! 2.0 Construct averages for NESDIS thick layers
   ! ----------------------------------------------

   pred(1) = Kth*sum( tv(12:nlevels)*DLP(12:nlevels) )
   pred(2) = Kth*sum( tv(1:11) *DLP(1:11) )
   pred(3) = T_skin
   pred(4) = Kpc*sum( qm(1:nlevels)*DP(1:nlevels) )

   !if (npred == 6) then
   ! pred(5) = Kth*sum((tv(5:8)  + tv(4:7))*DLP(4:7))
   ! pred(6) = Kth*sum((tv(8:13)  + tv(7:12))*DLP(7:12))
   !end if

end subroutine da_predictor_crtm


