
!============================================================================
SUBROUTINE DA_Transform_XToTb( xb )
!----------------------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------------------
   type (xb_type), intent(inout)   :: xb       ! first guess state.

!  LOCAL VARS

   integer            :: i,j,k,ks,ke 

   real               :: psfc,ta,gamma,sst,htpw,speed,alw,zcld,tpw,dum1,zrhom
!----------------------------------------------------------------------------

   ks = xb%kts
   ke = xb%kte

   do j=xb%jts,xb%jte
   do i=xb%its,xb%ite


! surface pressure (mb) (940 -1030)

      psfc          = 0.01*xb%psfc(i,j)

! sea surface temperature (k) (273 - 303) (doesnot change) 

      sst           = xb%tgrn(i,j)

! effective surface air temperature (263 - 303)

      ta          = xb%tgrn(i,j) + &
                    (xb%t(i,j,ks)-xb%tgrn(i,j))*log(2./0.0001)/ &
                    log(xb%h(i,j,ks)/0.0001)

! gamma is an emperical formula and zcld is given for now

      gamma      = (ta-270)*0.023 + 5.03  ! effective lapse rate   (km) (4.0 - 6.5)

      zcld       = 1                           ! effective cloud height (km)
                                               ! = 1 if no cloud infomation
! total precipitable water in cm
! total precipitable water in (kg/m**2) (0 - 70)

      tpw          = xb%tpw(i,j)*10.
      speed        = xb%speed(i,j)
 
!!Column liquid water (kg/m**2)   (0 - 0.5) (no data now. So, do it later.)

      alw          = 0.

! Column height weighted moisture density on the grid locally 

      zrhom = 0.0
      do k=ks,ke
         zrhom=zrhom+(xb%hf(i,j,k+1)-xb%hf(i,j,k))*xb%h(i,j,k)*xb%q(i,j,k)*xb%rho(i,j,k)
      END DO

! Column moisture density on the grid locally

      htpw          = zrhom/tpw/1000.

      call tb(1,53.,psfc,ta,gamma,sst,tpw,htpw, &
              speed,alw,zcld,xb%tb19v(i,j),xb%tb19h(i,j))
      call tb(2,53.,psfc,ta,gamma,sst,tpw,htpw, &
              speed,alw,zcld,xb%tb22v(i,j),dum1)
      call tb(3,53.,psfc,ta,gamma,sst,tpw,htpw, &
              speed,alw,zcld,xb%tb37v(i,j),xb%tb37h(i,j))
      call tb(4,53.,psfc,ta,gamma,sst,tpw,htpw, &
              speed,alw,zcld,xb%tb85v(i,j),xb%tb85h(i,j))

   end do
   end do

END SUBROUTINE DA_Transform_XToTb

!--------------------------------------------------------------------------
!Input :   IFREQ = (1,2,3, or 4 ) for (19.35, 22.235, 37, or 85.5 GHz)
!          THETA = incidence angle (deg.)
!                                                 (approximate  range)
!          P0    = surface pressure (mb)                    (940 -1030)
!          WV    = precipitable water (kg/m**2)               (0 - 70)
!          HWV   = water vapor density scale height (km)    (0.5 - 3.0)
!          TA, GAMMA = "effective" surface air temperature 
!                        and lapse rate (K ; km)
!               T(z) = Ta - gamma*z              (263 - 303 ; 4.0 - 6.5)
!
!          SST = sea surface temperature (K)     (273 - 303)
!          U  = wind speed (m/sec)               (0 - 30)
!          ALW = column liquid water (kg/m**2)   (0 - 0.5)
!              note: ALW > 0.5 usually indicates precipitation
!
!          ZCLD = effective cloud height (km)    
!              (Ta - GAMMA*ZCLD)  should be warmer than 243 K in order
!               to be reasonably realistic.
!
!Output:   TBV, TBH = vertically and horizontally polarized brightness
!               temperature (K) at frequency given by IFREQ  
!
!----------------------- tbmod_ssmi.f ---  cut here ----------------------
! VERSION:  July 1997
! This has been carefully calibrated against cloud-free pixels over the 
! globe for the month of Nov 87.   It's not perfect, but don't change it 
! unless you come up with 
! a better calibration data set.  One remaining task it to add splines for
! foam between 5 and 12 m/sec
!
!  Reference:
!
!       Petty, 1990: "On the Response of the Special Sensor Microwave/Imager
!         to the Marine Environment - Implications for Atmospheric Parameter
!         Retrievals"  Ph.D. Dissertation, University of Washington, 291 pp.
!         (See below for J.Atmos.Ocean.Tech. articles in press or pending)
!
!   coded in a quick-and-dirty fashion, and without any warranty, by:
!   Grant W. Petty
!   Earth and Atmospheric Sciences Dept.
!   Purdue University
!   West Lafayette, IN  47907
!   USA
!
!   Tel. No. (317) 494-2544
!   Internet address : gpetty@rain.atms.purdue.edu
!
!----------------------------------------------------------------------
      subroutine tb(ifreq,theta,p0,ta,gamma,sst,wv,hwv,u,alw,zcld, &
         tbv,tbh)
      implicit none
      integer ifreq

      real   , intent(IN   ) :: theta,p0,ta,gamma,sst,wv,hwv,u,alw,zcld
      real   , intent(  OUT) :: tbv,tbh
      real freq(4),ebiasv(4),ebiash(4),cf1(4),cf2(4),cg(4)

      real :: f,costhet,gx2,sigma,remv,remh,tbup,tbdn,tauatm
      real :: effangv,effangh,tbdnv,dum,foam,emissv,emissh
      real :: refv,refh,semv,semh,scatv,scath,tbdnh

      data freq/19.35,22.235,37.0,85.5/

! empirical bias corrections for surface emissivity

!
      data ebiasv/0.0095, 0.005,-0.014, -0.0010/
      data ebiash/0.004,   0.0,-0.023, 0.043/


      data cf1/0.0015,0.004,0.0027,0.005/
      data cg/4.50e-3, 5.200e-3, 5.5e-3, 7.2e-3 /

      data cf2/0.0023,0.000,0.0002,-0.006/

! 'foam' emissivity
      real :: fem
      data fem /1.0/
!
      f = freq(ifreq)
      costhet = cos(theta*0.017453)

! effective surface slope variance

      gx2 = cg(ifreq)*u

! get upwelling atmospheric brightness temperature

      call tbatmos(ifreq,theta,p0,wv,hwv,ta,gamma,alw,zcld, &
                                   tbup,tbdn,tauatm)

! convert transmittance to optical depth

      sigma = -alog(tauatm)*costhet

! get rough surface emissivity

      call roughem(ifreq,gx2,sst,theta,remv,remh)

! get effective zenith angles for scattered radiation at surface

      call effang(ifreq,theta,gx2,sigma,effangv,effangh)

! get effective sky brightness temperatures for scattered radiation

      call tbatmos(ifreq,effangv,p0,wv,hwv,ta,gamma,alw,zcld, &
                   dum,tbdnv,dum)

      call tbatmos(ifreq,effangh,p0,wv,hwv,ta,gamma,alw,zcld, &
                   dum,tbdnh,dum)
! compute 'foam' coverage
      foam = cf1(ifreq)*u

      if (u .gt. 5.0) then
        foam = foam + cf2(ifreq)*(u-5.0)
      endif

! compute surface emissivities and reflectivity
      emissv = foam*fem + (1.0 - foam)*(remv + ebiasv(ifreq))
      emissh = foam*fem + (1.0 - foam)*(remh + ebiash(ifreq))
      refv = 1.0 - emissv
      refh = 1.0 - emissh
! compute surface emission term
      semv = sst*emissv
      semh = sst*emissh
! compute surface scattering term
      scatv = refv*tbdnv
      scath = refh*tbdnh
! combine to get space-observed brightness temperature
      tbv = tbup + tauatm*(semv + scatv)
      tbh = tbup + tauatm*(semh + scath)


      end subroutine tb

!==============================================================
!
!  SYNOPSIS OF SUBROUTINES TB, TBATMOS
!
!===============================================================
!  SUBROUTINE TBATMOS(IFREQ,THETA,P0,WV,HWV,TA,GAMMA,LW,ZCLD,TBUP,
!                    TBDN,TAUATM)
!
!  This routine calculates the upwelling and downwelling microwave 
!  atmospheric brightness temperatures and transmittance at SSM/I
!  frequencies.  It is a generalization of "TBCLEAR" and includes a 
!  cloud layer of known height and total liquid water content.
!------------------------------------------
!  Input :   IFREQ = (1,2,3, or 4 ) for (19.35, 22.235, 37, or 85.5 GHz)
!            THETA = incidence angle (deg.)
!
!                                                       ( approximate range )
!            P0    = surface pressure (mb)                      (940 -1030)
!            WV    = precipitable water (kg/m**2)                 (0 - 70)
!            HWV   = water vapor density scale height (km)      (0.5 - 3.0)
!            TA, GAMMA = "effective" surface air temperature 
!                               and lapse rate (K ; km)
!                     T(z) = Ta - gamma*z              (263 - 303 ; 4.0 - 6.5)
!
!            LW, ZCLD  = total cloud liquid water content (kg/m**2) and height (km)
!
!  Output :  TBUP, TBDN = atmospheric component of upwelling and downwelling
!                         brightness temperature (K)
!            TAUATM    = total atmospheric transmittance at specified incidence angle.
!
!  Subroutines called : EFFHT, SIGMA_V
!
!
!===================================================================
!
!   NOTES
!
!===================================================================
!
!
!    1) This model is described in detail by Petty (1990).  
!    Frequency dependent
!    coefficients of the model are based on radiative transfer
!    integrations of 16,893 radiosonde profiles from 29 ships, islands and
!    coastal stations, representing all major climatic regimes and all
!    seasons.  Absorption coefficients used in these integrations were
!    obtained from an adaptation of the Millimeter-wave Propagation Model
!    (MPM) of Liebe (1985) with updated line parameters (Liebe 1988,
!    personal communication).  Empirical corrections to total water
!    absorption values have been added, based on comparisons with
!    radiosondes.
!    
!    2) The frequency-dependent component of the model is only 
!    intended to give meaningful results for
!    combinations of input parameters which are meteorologically and
!    physically reasonable under coastal or maritime conditions near sea
!    level.  The user must ensure that input values are not only
!    individually reasonable, but also are mutually consistent.  See
!    Petty (1990) for details.
!    
!    3) Computed brightness temperatures from this model are generally
!    valid only for (gaseous optical depth)/cos(theta) of order unity or
!    less.  This is usually of concern only for theta > 75 deg. in a moist
!    atmosphere at 22 and 85 GHz.  An ad hoc correction has been added to
!    TBCLEAR which greatly improves the calculated downwelling brightness
!    temperature for near-grazing angles, so that its contribution to the
!    diffuse reflected component from the sea surface may be correctly
!    estimated.  No such correction is applied to upwelling brightness
!    temperatures, since there is no contribution to SSM/I observations
!    from angles other than at the nadir angle.  The correction is also
!    not yet available for downwelling brightness temperatures from
!    TBATMOS. 
!                  
!
!    The author welcomes feedback from other workers concerning the
!    accuracy and overall performance of the routines presented here.
!    
!    
!    References:
!    
!       Liebe, H.J., 1985 :  An updated model for millimeter
!         wave propagation in moist air.  Radio Science, 20, pp. 1069-1089
!    
!       Liebe, H.J., 1988, personal communication: updated absorption line
!         parameters. 
!    
!       Petty, G.W. 1990:  On the Response of the SSM/I to the 
!         Marine Environment --- Implications for Atmospheric Parameter
!         Retrievals.  Ph.D. Dissertation, Dept. of Atmospheric Sciences,
!         University of Washington
!
!       Petty, G.W., and K.B. Katsaros, 1992: The response
!        of the Special Sensor Microwave/Imager to the
!        marine environment. Part I: An analytic model for
!        the atmospheric component of observed brightness
!        temperatures.  J. Atmos. Ocean. Tech., 9, 746--761
!       
!       Petty, G.W., and K.B. Katsaros, 1993: The response
!        of the Special Sensor Microwave/Imager to the
!        marine environment. Part II: A parameterization of
!        roughness effects on sea surface emission and
!        reflection.  Submitted to J. Atmos. Ocean. Tech.
!       
!    
!===============================================================
!
!  fortran source code listings for TBATMOS,
!                       SIGMA_V, EFFHT
!
!===============================================================
!  SUBROUTINE TBATMOS(IFREQ,THETA,P0,WV,HWV,TA,GAMMA,LW,ZCLD,TBUP,
!                    TBDN,TAUATM)
!
!  This routine calculates the upwelling and downwelling microwave 
!  atmospheric brightness temperatures and transmittance at an SSM/I     
!  frequency.  It is a generalization of "TBCLEAR" and includes a 
!  cloud layer of known height and total liquid water content.
!
!  Input :   IFREQ = (1,2,3, or 4 ) for (19.35, 22.235, 37, or 85.5 GHz)
!            THETA = incidence angle (deg.)
!
!                                                       ( approximate range )
!            P0    = surface pressure (mb)                      (940 -1030)
!            WV    = precipitable water (kg/m**2)                 (0 - 70)
!            HWV   = water vapor density scale height (km)      (0.5 - 3.0)
!            TA, GAMMA = "effective" surface air temperature 
!                               and lapse rate (K ; km)
!                     T(z) = Ta - gamma*z              (263 - 303 ; 4.0 - 6.5)
!
!            LW, ZCLD  = total cloud liquid water content (kg/m**2) and height (km)
!
!  Output :  TBUP, TBDN = atmospheric component of upwelling and downwelling
!                         brightness temperature (K)
!            TAUATM    = total atmospheric transmittance at specified incidence angle.
!
!  Subroutines called : EFFHT, SIGMA_V
!------------------------------------------------------------
      subroutine tbatmos(ifreq,theta,p0,wv,hwv,ta,gamma,lw,zcld, &
          tbup,tbdn,tauatm)
      integer ifreq
      real theta,p0,wv,hwv,ta,gamma,lw,zcld,tbup,tbdn,tauatm
      real mu,hdn,hup,hdninf,hupinf
!
      real b1(4),b2(4),b3(4)
      real c(4),d1(4),d2(4),d3(4),zeta(4),kw0(4),kw1(4),kw2(4),kw3(4)
      real tau,tau1,tau2,taucld
      real tcld,tc,em,em1
      real sigv,sigo,sig,sig1,sigcld
      real teff1dn,teff1up,teffdn,teffup
      real tbcld,tbclrdn,tbclrup,tb1dn,tb1up,tb2dn,tb2up
      real otbar,tc2,tc3,hv,ho,alph
!
      data b1/-.46847e-1,-.57752e-1,-.18885,.10990/
      data b2/.26640e-4,.31662e-4,.9832e-4,.60531e-4/
      data b3/.87560e+1,.10961e+2,.36678e+2,-.37578e+2/
      data c/ .9207,   1.208,     .8253,     .8203/
      data zeta/4.2,4.2,4.2,2.9/
      data d1/-.35908e+1,-.38921e+1,-.43072e+1,-.17020e+0/
      data d2/ .29797e-1, .31054e-1, .32801e-1, .13610e-1/
      data d3/-.23174e-1,-.23543e-1,-.24101e-1,-.15776e+0/
      data kw0/ .786e-1, .103,    .267,    .988/
      data kw1/-.230e-2,-.296e-2,-.673e-2,-.107e-1/
      data kw2/ .448e-4, .557e-4, .975e-4,-.535e-4/
      data kw3/-.464e-6,-.558e-6,-.724e-6, .115e-5/
! mu = secant(theta)
      mu = 1.0/cos(theta*0.0174533)
! get water vapor optical depth
!=====
      CALL cal_sigma_v(ifreq,p0,wv,hwv,ta,gamma,sigv)
! otbar = one over "mean" temperature
      otbar = 1.0/(ta - gamma*zeta(ifreq))
! sigo = dry air optical depth
      sigo = b1(ifreq) + b2(ifreq)*p0  + b3(ifreq)*otbar
! cloud parameters
      tcld   = ta - gamma*zcld
      tc = tcld - 273.15
      tc2 = tc*tc
      tc3 = tc2*tc
      sigcld = (kw0(ifreq) + tc*kw1(ifreq) + tc2*kw2(ifreq) +  &
           tc3*kw3(ifreq))*lw
      taucld = exp(-mu*sigcld)
      tbcld = (1.0 - taucld)*tcld
! hv, ho = effective absorber scale heights for vapor, dry air
      hv = c(ifreq)*hwv
      ho = d1(ifreq) + d2(ifreq)*ta + d3(ifreq)*gamma
! get effective emission heights for layer 1 and total atmosphere
      call effht(ho,hv,sigo,sigv,mu,zcld,hdn,hup, &
         hdninf,hupinf)
! atmospheric transmittances in layer one and two, and combined
      sig = sigo + sigv
      sig1 = sigo*(1.0-exp(-zcld/ho)) + sigv*(1.0-exp(-zcld/hv))
      tau  = exp(-mu*sig)
      tau1 = exp(-mu*sig1)
      tau2 = tau/tau1
! atmospheric "emissivity"
      em1  = 1.0 - tau1
      em   = 1.0 - tau
! downwelling and upwelling brightness temperature for each layer
      teff1dn = ta - gamma*hdn
      teff1up = ta - gamma*hup
      teffdn = ta - gamma*hdninf
      teffup = ta - gamma*hupinf
      tbclrdn = teffdn*em
      tbclrup = teffup*em
!
      tb1dn = em1*teff1dn
      tb1up = em1*teff1up
      tb2dn = (tbclrdn - tb1dn)/tau1
      tb2up =  tbclrup - tau2*tb1up
! total downwelling and upwelling brightness temperature and transmittance
      tbdn  = tb1dn + tau1*(tbcld + taucld*tb2dn)
      tbup  = tb2up + tau2*(tbcld + taucld*tb1up)
      tauatm = tau*taucld
!
! the following lines apply an ad hoc correction to improve fit 
! at large angles and/or high gaseous opacities 
!  (downwelling brightness temperatures only)

      alph = (0.636619*atan(mu*sig))**2
      tbdn = (1.0-alph)*tbdn + em*alph*ta
!
      end subroutine tbatmos
!========================================================
!
! FUNCTION SIGMA_V(IFREQ,P0,WV,HWV,TA,GAMMA)
!
! This function returns the contribution of water vapor to total
! atmospheric optical depth at SSM/I frequencies.  
! -------------------------------------------
!  Input :   IFREQ = (1,2,3, or 4 ) for (19.35, 22.235, 37, or 85.5 GHz)
!
!                                                       ( approximate range )
!            P0    = surface pressure (mb)                      (940 -1030)
!            WV    = precipitable water (kg/m**2)                 (0 - 70)
!            HWV   = water vapor density scale height (km)      (0.5 - 3.0)
!            TA, GAMMA = "effective" surface air temperature 
!                               and lapse rate (K ; km)
!                     T(z) = Ta - gamma*z              (263 - 303 ; 4.0 - 6.5)
!
! Return value:  SIGMA_V = total water vapor optical depth (Nepers)
!
! Note:  The following code has been written so as to minimize
! redundant computations during successive calls.  A time savings of
! 50% or more is achieved when several input parameters are unchanged
! from one call to the next, as is often the case in retrieval
! applications.  Without this optimization, less than about 30 lines
! of code would be required to perform the same calculation.
!
!--------------------------------------------- 
!========================================================
!
! FUNCTION SIGMA_V(IFREQ,P0,WV,HWV,TA,GAMMA)
!
! This function returns the contribution of water vapor to total
! atmospheric optical depth at SSM/I frequencies.  
! -------------------------------------------
!  Input :   IFREQ = (1,2,3, or 4 ) for (19.35, 22.235, 37, or 85.5 GHz)
!
!                                                       ( approximate range )
!            P0    = surface pressure (mb)                      (940 -1030)
!            WV    = precipitable water (kg/m**2)                 (0 - 70)
!            HWV   = water vapor density scale height (km)      (0.5 - 3.0)
!            TA, GAMMA = "effective" surface air temperature 
!                               and lapse rate (K ; km)
!                     T(z) = Ta - gamma*z              (263 - 303 ; 4.0 - 6.5)
!
! Return value:  SIGMA_V = total water vapor optical depth (Nepers)
!
!
      subroutine cal_sigma_v(ifreq,p0,wv,hwv,ta,gamma,sigma_v)
      integer ifreq
      real sigma_v,p0,wv,hwv,ta,gamma
!
      real wvc, wvcor(4)
      real sigv
!
      real voh1,otbar1,pbar1
      real term21,term31,term41,term51,term61
      real a11,a21,a31,a41,a51,a61
!
      real voh2,otbar2,pbar2
      real term22,term32,term42,term52,term62
      real a12,a22,a32,a42,a52,a62
!
      real voh3,otbar3,pbar3
      real term23,term33,term43,term53,term63
      real a13,a23,a33,a43,a53,a63
!
      real voh4,otbar4,pbar4
      real term24,term34,term44,term54,term64
      real a14,a24,a34,a44,a54,a64
!     
      real const1,const2,const3,const4
      real h1,h2,h3,h4
!
      data const1,const2,const3,const4/0.6,2.8,0.2,0.2/
      data h1,h2,h3,h4/5.0,4.9,6.8,6.4/
!
      data a11,a21,a31,a41,a51,a61/-.13747e-2,-.43061e-4, .14618e+1,  &
        .25101e-3, .14635e-1,-.18588e+3/
      data a12,a22,a32,a42,a52,a62/ .22176e-1,-.32367e-4,-.10840e-4,  &
        -.63578e-1, .16988e-7,-.29824e+2/
      data a13,a23,a33,a43,a53,a63/-.10566e-2,-.12906e-3, .56975e+0,  &
         .10828e-8,-.17551e-7, .48601e-1/
      data a14,a24,a34,a44,a54,a64/-.60808e-2,-.70936e-3, .28721e+1,  &
         .42636e-8,-.82910e-7, .26166e+0/
!
!      data wvcor/1.01,0.95,1.06,0.92/
      data wvcor/1.02,0.98,1.02,0.88/
! use modified water vapor value to correct for errors in theoretical absorption
!
      wvc = wv*wvcor(ifreq)
!
      if (ifreq.eq.1) then
        pbar1 = p0/(1.0 + hwv/h1)
        voh1   = wv/hwv
        term21 = a21*voh1
        otbar1 = 1.0/(ta - const1*gamma*hwv)
        term31 = a31*otbar1
        term61 = a61*otbar1*otbar1
        term41 = a41*pbar1*otbar1
        term51 = a51*voh1*otbar1
        sigv = a11 + term21 + term31 + term41 + term51 + term61
      else if (ifreq.eq.2) then
        pbar2 = p0/(1.0 + hwv/h2)
        term22 = a22*pbar2
        term52 = a52*pbar2*pbar2
        voh2   = wv/hwv
        term32 = a32*voh2
        otbar2 = 1.0/(ta - const2*gamma*hwv)
        term42 = a42*otbar2
        term62 = a62*otbar2*otbar2
        sigv = a12 + term22 + term32 + term42 + term52 + term62
      else if (ifreq.eq.3) then
        pbar3 = p0/(1.0 + hwv/h3)
        term43 = a43*pbar3*pbar3
        voh3   = wv/hwv
        term23 = a23*voh3
        otbar3 = 1.0/(ta - const3*gamma*hwv)
        term33 = a33*otbar3
        term53 = a53*pbar3*voh3
        term63 = a63*otbar3*voh3
        sigv = a13 + term23 + term33 + term43 + term53 + term63
      else if (ifreq.eq.4) then
        pbar4 = p0/(1.0 + hwv/h4)
        term44 = a44*pbar4*pbar4
        voh4   = wv/hwv
        term24 = a24*voh4
        otbar4 = 1.0/(ta - const4*gamma*hwv)
        term34 = a34*otbar4
        term54 = a54*pbar4*voh4
        term64 = a64*otbar4*voh4
        sigv = a14 + term24 + term34 + term44 + term54 + term64
      else
        sigv = 0.0
      endif
      sigma_v = sigv*wvc

      end subroutine cal_sigma_v
!
!******************************************
!
! SUBROUTINE EFFHT(HO,HV,SIGO,SIGV,MU,ZCLD,HDN,HUP,HDNINF,HUPINF)
!
! This subroutine returns "effective emitting heights" for an atmosphere
! with gaseous absorption approximated by two exponentially-decaying
! profiles with distinct scale heights.  For applications at
! SSM/I frequencies (i.e., 19, 22, 37, 86 GHz), these absorption profiles 
! correspond to water vapor and dry air absorption, respectively.  
!
! Input parameters: HO, HV  --  scale heights of absorption corresponding to 
!                               dry air and water vapor (km):
!
!    SIGO, SIGV --  total optical depth due to each constituent
!                      (the present model is valid only for (sigo+sigv < 1.0) )
! 
!         MU    --  secant(theta), where theta is the path angle from vertical
!
!        ZCLD   --  upper limit (km) of the atmospheric layer for which
!                   hdn, hup are to be calculated.  Layer extends down to the
!                   surface.  These parameters permit the separate calculation
!                   brightness temperature contributions from a lower and
!                   upper atmospheric layer, separated by a cloud layer at
!                   height zcld.
!
! Output parameters:  
!
!  HDN, HUP     --  "effective emitting height" of an atmospheric layer
!            bounded below by the surface and above at height zcld.
!            "Effective emitting temperature" for upward and downward
!            microwave radiation emitted by that layer is then given by
!            (Ta - gamma*hup) and (Ta - gamma*hdn), respectively, where
!            Ta and gamma are effective surface temperature (deg K)
!            and lapse rate (K / km), respectively.  Brightness temperatures
!            due to emission from this layer are then (1 - tau)(Ta - gamma*hup)
!            and (1 - tau)(Ta - gamma*hdn), respectively, where tau is
!            the slant path transmittance of the layer at angle theta
!
! HDINF,HUPINF  -- same as hdn and hup, but for the entire
!                  atmosphere  (i.e., zcld set to infinity)
!
!
!      This version of EFFHT executes 83 floating point multiplications,
!      11 floating point divisions, and 2 calls to exp() .
!
!----------------------------------------------
      subroutine effht(ho,hv,sigo,sigv,mu,zcld,hdn,hup,hdninf,hupinf)
!
      real ho,hv,sigo,sigv,mu,zcld,gint,zgint,hint,zhint, &
           ginf,zginf,hinf,zhinf,hdn,hup,hdninf,hupinf
!
      real   hoinv,hvinv,chio,chiv,ezho,ezhv,alpha,alph2,alph3
      real   beta,beta2,beta3,mu2,mualph,cplus,cmin,dplus,dmin
      real   chiov,chivv,chioo,chioov,chiovv,chiooo,chivvv
      real   h11,h21,h12
      real   sigoo,sigov,sigvv,sigooo,sigoov,sigovv,sigvvv
      real   ezhoo,ezhov,ezhvv,ezhooo,ezhoov,ezhovv,ezhvvv
      real   s,sprim,t,tprim,u,uprim,term1,term2,term3
      real   halfmu,halfmu2,sixthmu2,etnthmu2,quartmu
!
!
      hoinv = 1.0d0/ho
      hvinv = 1.0d0/hv
      chio = zcld*hoinv
      chiv = zcld*hvinv
      ezho = sigo*exp(-chio)
      ezhv = sigv*exp(-chiv)
      alpha = sigo + sigv
      alph2 = alpha*alpha
      alph3 = alpha*alph2
      beta =  ezho + ezhv
      beta2 = beta*beta
      beta3 = beta*beta2
!
      mu2 = mu*mu
      halfmu = 0.5d0*mu
      sixthmu2 = mu2/6.0d0
      etnthmu2 = mu2/18.0d0
      quartmu = 0.25d0*mu
      halfmu2 = 0.5d0*mu2
      mualph = mu*alpha
      cplus = 1.0d0 + mualph
      cmin  = 1.0d0 - mualph
      dplus = halfmu2*alph2
      dmin  = dplus
      dplus = cplus + dplus
      dmin  = cmin  + dmin
!
      h11 = hoinv + hvinv
      h21 = 1.0d0/(h11 + hvinv)
      h12 = 1.0d0/(h11 + hoinv)
      h11 = 1.0d0/h11
      chiov = 1.0d0 + chio + chiv
      chioo = 1.0d0 + chio + chio
      chivv = 1.0d0 + chiv + chiv
      chioov = chioo + chiv
      chiovv = chio + chivv
      chiooo = chioo + chio
      chivvv = chivv + chiv
      chio = 1.0d0 + chio
      chiv = 1.0d0 + chiv
      sigov = sigo*sigv
      sigoo = sigo*sigo
      sigvv = sigv*sigv
      sigooo = sigoo*sigo
      sigoov = sigoo*sigv
      sigovv = sigo*sigvv
      sigvvv = sigvv*sigv
      ezhoo = ezho*ezho
      ezhov = ezho*ezhv
      ezhvv = ezhv*ezhv
      ezhovv = ezho*ezhvv
      ezhoov = ezhoo*ezhv
      ezhooo = ezhoo*ezho
      ezhvvv = ezhvv*ezhv
      s     = sigo*ho   +    sigv*hv
      sprim = ezho*ho*chio + ezhv*hv*chiv
      t     = sigoo*ho    +    4.0d0*sigov*h11     +   sigvv*hv
      tprim = ezhoo*ho*chioo + 4.0d0*ezhov*h11*chiov + ezhvv*hv*chivv
      u     = sigooo*ho+9.0d0*(sigovv*h21+sigoov*h12)+sigvvv*hv
      uprim = ezhvvv*hv*chivvv +  &
           9.0d0*(ezhovv*h21*chiovv + ezhoov*h12*chioov) + &
           ezhooo*ho*chiooo
!
      term1 = s - sprim
      term2 = quartmu*(t - tprim)
      term3 = etnthmu2*(u - uprim)
      zgint = dmin*term1 +  cmin*term2 + term3
      zhint = -dplus*term1 + cplus*term2 - term3
      term2 = quartmu*t
      term3 = etnthmu2*u
      zginf = dmin*s +  cmin*term2 + term3
      zhinf = -dplus*s + cplus*term2 - term3
      term1 = alpha - beta
      term2 = halfmu*(alph2 - beta2)
      term3 = sixthmu2*(alph3 - beta3)
      gint  = dmin*term1 +  cmin*term2 + term3
      hint  = -dplus*term1 + cplus*term2 - term3
      term2 = halfmu*alph2
      term3 = sixthmu2*alph3
      ginf  = dmin*alpha +  cmin*term2 + term3
      hinf  = -dplus*alpha + cplus*term2 - term3
      hdn   = zgint/gint
      hup   = zhint/hint
      hdninf = zginf/ginf
      hupinf = zhinf/hinf

      end subroutine effht
!
!==============================================
!
      subroutine effang(ifreq,theta,gx2,sigma,effangv,effangh)
!
! Calculated the effective zenith angle of reflected microwave radiation
! at SSM/I frequencies for vertical and horizontal polarization
!
      real theta, gx2,sigma,effangv,effangh
      integer ifreq
      real gg,ggg,xx,xd,z1,z2,z3,z4,z5,z6,alnthv
      real alnthh,angv,angh,y,alnsig,dth
!
      real c19v,c19h,c22v,c22h,c37v,c37h,c85v,c85h
      real s19v(6),s19h(6),s22v(6),s22h(6), &
           s37v(6),s37h(6),s85v(6),s85h(6)
!
      data c19v,c19h,c22v,c22h,c37v,c37h,c85v,c85h &
        /-.5108,.5306,-.5108,.5306,-.6931,.1823,-.9163,.3000/
      data s19v /.225E+2,.698E+2,-.238E+2,-.648E+1,.402E+0,.262E+1/
      data s19h /.743E+1,.507E+2,-.206E+2,-.191E+1,.648E-1,.291E+1/
      data s22v /.249E+2,.701E+2,-.240E+2,-.714E+1,.405E+0,.256E+1/
      data s22h /.498E+1,.442E+2,-.190E+2,-.129E+1,.803E-2,.277E+1/
      data s37v /.215E+2,.573E+2,-.211E+2,-.670E+1,.443E+0,.253E+1/
      data s37h /.869E+1,.571E+2,-.257E+2,-.302E+1,.237E+0,.386E+1/
      data s85v /.116E+2,.263E+2,-.101E+2,-.358E+1,.270E+0,.175E+1/
      data s85h /.736E+1,.568E+2,-.254E+2,-.248E+1,.196E+0,.387E+1/
!
      if (gx2 .le. 0.0 .or. sigma .le. 0.0) then
        effangv = theta
        effangh = theta
        return
      endif
      alnsig = alog(sigma)
      gg = gx2*gx2
      ggg = gg*gx2
      if (ifreq .eq. 1) then 
         xd = alnsig - c19v
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthv = s19v(1)*z1 + s19v(2)*z2 +s19v(3)*z3 + &
           s19v(4)*z4 + s19v(5)*z5 + s19v(6)*z6
         alnthv = alnthv + 3.611
!
         xd = alnsig - c19h
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthh = s19h(1)*z1 + s19h(2)*z2 +s19h(3)*z3 + &
           s19h(4)*z4 + s19h(5)*z5 + s19h(6)*z6
         alnthh = alnthh + 3.611
      else if (ifreq .eq. 2) then 
         xd = alnsig - c22v
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthv = s22v(1)*z1 + s22v(2)*z2 +s22v(3)*z3 + &
           s22v(4)*z4 + s22v(5)*z5 + s22v(6)*z6
         alnthv = alnthv + 3.611
!
         xd = alnsig - c22h
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthh = s22h(1)*z1 + s22h(2)*z2 +s22h(3)*z3 + &
           s22h(4)*z4 + s22h(5)*z5 + s22h(6)*z6
         alnthh = alnthh + 3.611
      else if (ifreq .eq. 3) then 
         xd = alnsig - c37v
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthv = s37v(1)*z1 + s37v(2)*z2 +s37v(3)*z3 + &
           s37v(4)*z4 + s37v(5)*z5 + s37v(6)*z6
         alnthv = alnthv + 3.611
!
         xd = alnsig - c37h
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthh = s37h(1)*z1 + s37h(2)*z2 +s37h(3)*z3 + &
           s37h(4)*z4 + s37h(5)*z5 + s37h(6)*z6
         alnthh = alnthh + 3.611
      else if (ifreq .eq. 4) then 
         xd = alnsig - c85v
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthv = s85v(1)*z1 + s85v(2)*z2 +s85v(3)*z3 + &
           s85v(4)*z4 + s85v(5)*z5 + s85v(6)*z6
         alnthv = alnthv + 3.611
!
         xd = alnsig - c85h
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthh = s85h(1)*z1 + s85h(2)*z2 +s85h(3)*z3 + &
           s85h(4)*z4 + s85h(5)*z5 + s85h(6)*z6
         alnthh = alnthh + 3.611
      endif
      angv = 90.0 - exp(alnthv)
      angh = 90.0 - exp(alnthh)
      y  = 1.0 - 28.0*gx2
      if (y .lt. 0.0) y = 0.0
      dth = (theta - 53.0)*y
      effangv = angv + dth
      effangh = angh + dth

      end subroutine effang
!================================================================
      subroutine roughem(ifreq,gx2,tk,theta,remv,remh)
!
! Calculates rough-surface emissivity of ocean surface at SSM/I
! frequencies.
!
      integer :: ifreq
      real, intent(in ) :: gx2,tk,theta
      real, intent(out) :: remv,remh
      real tp,dtheta,g,x1,x2,x3,x4,semv,semh
      real ssw

      real a19v(4),a22v(4),a37v(4),a85v(4)
      real a19h(4),a22h(4),a37h(4),a85h(4)
      real f(4)
!
      data a19v/  -0.111E+01,   0.713E+00,  -0.624E-01,   0.212E-01 /
      data a19h/   0.812E+00,  -0.215E+00,   0.255E-01,   0.305E-02 /
      data a22v/  -0.134E+01,   0.911E+00,  -0.893E-01,   0.463E-01 /
      data a22h/   0.958E+00,  -0.350E+00,   0.566E-01,  -0.262E-01 /
      data a37v/  -0.162E+01,   0.110E+01,  -0.730E-01,   0.298E-01 /
      data a37h/   0.947E+00,  -0.320E+00,   0.624E-01,  -0.300E-01 /
      data a85v/  -0.145E+01,   0.808E+00,  -0.147E-01,  -0.252E-01 /
      data a85h/   0.717E+00,  -0.702E-01,   0.617E-01,  -0.243E-01 /
!
      data f/ 19.35, 22.235, 37.0, 85.5 /
!
      tp = tk/273.0
      dtheta = theta-53.0
      g =  0.5*gx2
      x1 = g
      x2 = tp*g
      x3 = dtheta*g
      x4 = tp*x3
!
      if (ifreq .eq. 1) then
         remv = x1*a19v(1) + x2*a19v(2) + x3*a19v(3) + x4*a19v(4)
         remh = x1*a19h(1) + x2*a19h(2) + x3*a19h(3) + x4*a19h(4)
      else if (ifreq .eq. 2) then
         remv = x1*a22v(1) + x2*a22v(2) + x3*a22v(3) + x4*a22v(4)
         remh = x1*a22h(1) + x2*a22h(2) + x3*a22h(3) + x4*a22h(4)
      else if (ifreq .eq. 3) then
         remv = x1*a37v(1) + x2*a37v(2) + x3*a37v(3) + x4*a37v(4)
         remh = x1*a37h(1) + x2*a37h(2) + x3*a37h(3) + x4*a37h(4)
      else if (ifreq .eq. 4) then
         remv = x1*a85v(1) + x2*a85v(2) + x3*a85v(3) + x4*a85v(4)
         remh = x1*a85h(1) + x2*a85h(2) + x3*a85h(3) + x4*a85h(4)
      endif
      ssw=36.5
      call spemiss(f(ifreq),tk,theta,ssw,semv,semh)
      remv = remv + semv
      remh = remh + semh
  
      end subroutine roughem
      
!*******************
!
      subroutine epsalt(f,t,ssw,epsr,epsi)
!     returns the complex dielectric constant of sea water, using the
!     model of Klein and Swift (1977)
!
!     Input   f = frequency (GHz)
!             t = temperature (C)
!             ssw = salinity (permil) (if ssw < 0, ssw = 32.54)
!     Output  epsr,epsi  = real and imaginary parts of dielectric constant

      real, intent(in    ) :: f,t
      real, intent(inout ) :: ssw
      real, intent(out   ) :: epsr,epsi
      real :: pi
      real :: ssw2,ssw3,t2,t3,es,a,tau,b,delt
      real :: beta,sig,delt2,om

      complex cdum1,cdum2,cdum3
      parameter (pi = 3.14159265)
!
      if (ssw .lt. 0.0) ssw = 32.54
      ssw2 = ssw*ssw
      ssw3 = ssw2*ssw
      t2 = t*t
      t3 = t2*t
      es = 87.134 - 1.949e-1*t - 1.276e-2*t2 + 2.491e-4*t3
      a = 1.0 + 1.613e-5*ssw*t - 3.656e-3*ssw + 3.21e-5*ssw2 - &
         4.232e-7*ssw3
      es = es*a
!
      tau = 1.768e-11 - 6.086e-13*t + 1.104e-14*t2 - 8.111e-17*t3
      b = 1.0 + 2.282e-5*ssw*t - 7.638e-4*ssw - 7.760e-6*ssw2 + &
         1.105e-8*ssw3
      tau = tau*b
!
      sig = ssw*(0.182521 - 1.46192e-3*ssw + 2.09324e-5*ssw2 - &
         1.28205e-7*ssw3)
      delt = 25.0 - t
      delt2 = delt*delt
      beta = 2.033e-2 + 1.266e-4*delt + 2.464e-6*delt2 - &
         ssw*(1.849e-5 - 2.551e-7*delt + 2.551e-8*delt2)
      sig = sig*exp(-beta*delt)
!
      om = 2.0e9*pi*f
      cdum1 = cmplx(0.0,om*tau)
      cdum2 = cmplx(0.0,sig/(om*8.854e-12))

      cdum3 = 4.9 + (es-4.9)/(1.0 + cdum1) - cdum2
      epsr = real(cdum3)
      epsi = -aimag(cdum3)

      end subroutine epsalt
!
!*************************************
      subroutine spemiss(f,tk,theta,ssw,ev,eh)
!     returns the specular emissivity of sea water for given freq. (GHz), 
!     temperature T (K), incidence angle theta (degrees), salinity (permil)
!     
!     Returned values verified against data in Klein and Swift (1977) and
!     against Table 3.8 in Olson (1987, Ph.D. Thesis)
!
      real,intent(in   ) :: f,tk,theta
      real,intent(out  ) :: ev,eh
      real   epsr,epsi,ssw

      real   tc,costh,sinth,rthet,tmp1r,tmp1i
      complex   etav,etah,eps,cterm1v,cterm1h,cterm2,cterm3v,cterm3h
!

      tc = tk - 273.15
      call epsalt(f,tc,ssw,epsr,epsi)
      eps = cmplx(epsr,epsi)
      etav = eps
      etah = (1.0,0.0)
      rthet = theta*0.017453292
      costh = cos(rthet)
      sinth = sin(rthet)
      sinth = sinth*sinth
      cterm1v = etav*costh
      cterm1h = etah*costh
      eps = eps - sinth
      cterm2 = csqrt(eps)
      cterm3v = (cterm1v - cterm2)/(cterm1v + cterm2)
      cterm3h = (cterm1h - cterm2)/(cterm1h + cterm2)
      tmp1r   =  real(cterm3v)
      tmp1i   = -aimag(cterm3v)
!     ev = 1.0 - cabs(cterm3v)**2
      ev =  1.0 - (tmp1r*tmp1r+tmp1i*tmp1i)

      eh = 1.0 - cabs(cterm3h)**2

      end subroutine spemiss
!



