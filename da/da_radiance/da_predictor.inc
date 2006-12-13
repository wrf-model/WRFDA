subroutine da_predictor(pred,npred,temp,hum,t_skin)

   implicit none

   ! temp - 43 level temperatures
   ! hum  - 43 level moistures
   ! t-skin - model skin temperature

   ! pred(1) - 1000-300 thickness 43 1005.43-521.46 thickness
   ! pred(2) - 200-50 thickness   43  194.36-56.73  thickness
   ! pred(3) - T_skin
   ! pred(4) - total column precipitable water
   ! pred(5) - 10 -2 thickness    43  10.37-2.61 thickness
   ! pred(6) - 50 -10thickness    43  45.29-10.37 thickness

   integer, parameter :: jprtlev = 43

   real, parameter :: kth = 287.0597*0.5/9.806650
   real, parameter :: kpc = 100.0*0.5/9.806650

   integer, intent(in)  :: npred
   real, intent(in)     :: temp(jprtlev), hum(jprtlev), t_skin
   real, intent(out)    :: pred(npred)

   real :: tv(jprtlev)

   logical, save :: first = .true.

   real :: plevs(jprtlev) =  (/ 0.10,    0.29,    0.69,   1.42,    2.61, &
                                 4.41,    6.95,   10.37,  14.81,   20.40, &
                                 27.26,   35.51,   45.29,  56.73,   69.97, &
                                 85.18,  102.05,  122.04, 143.84,  167.95, &
                                194.36,  222.94,  253.71, 286.60,  321.50, &
                                358.28,  396.81,  436.95, 478.54,  521.46, &
                                565.54,  610.60,  656.43, 702.73,  749.12, &
                                795.09,  839.95,  882.80, 922.46,  957.44, &
                                985.88, 1005.43, 1013.25 /)

   real, save :: dlp(jprtlev-1), dp(jprtlev-1)

   integer :: ist, ilay, i

   ! If first time compute log P(i)/P(i-1) and P(i)-P(i-1)
   if (first) then
      dlp(1:jprtlev-1) = log(plevs(2:jprtlev)) - log(plevs(1:jprtlev-1))
      dp(1:jprtlev-1) = plevs(2:jprtlev) - plevs(1:jprtlev-1)
      first = .false.
   end if

   ! 1.0 Convert all temperatures to virtual temperatures
   tv =  temp/(1.0 - 0.6*hum)

   ! 2.0 Construct averages for NESDIS thick layers

   pred(1) = kth*sum((tv(26:42) + tv(25:41))*dlp(25:41))
   pred(2) = kth*sum((tv(14:21) + tv(13:20))*dlp(13:20))
   pred(3) = t_skin
   pred(4) = kpc*sum((hum(2:43) + hum(1:42))*dp(1:42))

   if (npred == 6) then
      pred(5) = kth*sum((tv(5:8)  + tv(4:7))*dlp(4:7))
      pred(6) = kth*sum((tv(8:13)  + tv(7:12))*dlp(7:12))
   end if

end subroutine da_predictor
