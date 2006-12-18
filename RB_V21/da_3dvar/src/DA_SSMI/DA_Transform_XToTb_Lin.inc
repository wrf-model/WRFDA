
!============================================================================
SUBROUTINE DA_Transform_XToTb_Lin( xa, xb )
!----------------------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------------------
   type (xb_type), intent(in)   :: xb       ! first guess state.
   type (x_type),  intent(inout):: xa       ! gridded analysis increment.

!  LOCAL VARS

   integer                      :: n        ! loop counter.
   INTEGER                      :: ks,ke    ! Range of 3rd dimension of arrays.
   integer                      :: i,j,k
   real                         :: dum1, dum2, zrhom, TGL_zrhom

   real                         :: psfc,ta,gamma,sst,htpw,speed,alw,zcld,tpw
   real                         :: TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,TGL_tpw
   real                         :: TGL_htpw,TGL_speed,TGL_alw,TGL_zcld         

!----------------------------------------------------------------------------

   ks = xb%kts
   ke = xb%kte

!  call DA_Transform_XToTPW( xa, xb)

   do j=xb%jts,xb%jte
   do i=xb%its,xb%ite
    
! surface pressure (mb) (940 -1030)

      psfc     =  0.01*xb%psfc(i,j)
      TGL_psfc =  0.01*xa%psfc(i,j)

! sea surface temperature (k) (273 - 303) (doesnot change) 

      sst      = xb%tgrn(i,j)
!     TGL_sst  = xa%tgrn(1,1)
      TGL_sst  = 0.

! effective surface air temperature (263 - 303)

      ta          = xb%tgrn(i,j) + &
                    (xb%t(i,j,ks)-xb%tgrn(i,j))*log(2./0.0001)/ &
                    log(xb%h(i,j,ks)/0.0001)

      TGL_ta      = (xa%t(i,j,ks)-0.)*log(2./0.0001)/ &
                    log(xb%h(i,j,ks)/0.0001)

! gamma is an emperical formula and zcld is given for now

      gamma = (ta-270)*0.023 + 5.03  ! effective lapse rate   (km) (4.0 - 6.5)
      zcld       = 1                           ! effective cloud height (km)
                                                 ! = 1 if no cloud infomation
      TGL_gamma = TGL_ta*0.023
      TGL_zcld  = 0.
     

! total precipitable water in (kg/m**2) (0 - 70)

      tpw     = xb%tpw(i,j)*10.
      TGL_tpw = xa%tpw(i,j)*10.

! speed, surface wind speed (m/sec)    (0 - 30) , take 10 m wind

      speed     = xb%speed(i,j)
      TGL_speed = xa%speed(i,j)

! Column liquid water (kg/m**2)   (0 - 0.5) (no data now. So, do it later.)

      alw     = 0.
      TGL_alw = 0.

! Column height weighted moisture density on the grid locally 

      zrhom = 0.0
      do k=ks,ke
         zrhom=zrhom+(xb%hf(i,j,k+1)-xb%hf(i,j,k))*xb%h(i,j,k)*xb%q(i,j,k)*xb%rho(i,j,k)
      END DO

      TGL_zrhom = 0.
      DO k = ks,ke
         TGL_zrhom = (xb%hf(i,j,k+1)-xb%hf(i,j,k))*xb%h(i,j,k)*(xb%q(i,j,k)*xa%rho(i,j,k) + &
                                            xa%q(i,j,k)*xb%rho(i,j,k)   &
                                           ) + TGL_zrhom
      END DO

!     call DA_Transform_XToZRhoQ(xb, i, j, zh, zf, zrhom)
!     call DA_Transform_XToZRhoQ_Lin(xb, xa, i, j, zh, zf, TGL_zrhom)

! Column moisture density on the grid locally

      htpw     = zrhom/tpw/1000.
      TGL_htpw = TGL_zrhom/tpw/1000. &
               - TGL_tpw/tpw*htpw

      call TGL_tb(1,53.,psfc,ta,gamma,sst,tpw,htpw,speed,alw,zcld,  &
                  xb%tb19v(i,j),xb%tb19h(i,j),                      &
                  TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,                &
                  TGL_tpw,TGL_htpw,TGL_speed,TGL_alw,               &
                  TGL_zcld,xa%tb19v(i,j),xa%tb19h(i,j)              )

      call TGL_tb(2,53.,psfc,ta,gamma,sst,tpw,htpw,speed,alw,zcld,  &
                  xb%tb22v(i,j),dum1,                               &
                  TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,                &
                  TGL_tpw,TGL_htpw,TGL_speed,TGL_alw,               &
                  TGL_zcld,xa%tb22v(i,j),dum2                       )

      call TGL_tb(3,53.,psfc,ta,gamma,sst,tpw,htpw,speed,alw,zcld,  &
                  xb%tb37v(i,j),xb%tb37h(i,j),                      &
                  TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,                &
                  TGL_tpw,TGL_htpw,TGL_speed,TGL_alw,               &
                  TGL_zcld,xa%tb37v(i,j),xa%tb37h(i,j)              )

      call TGL_tb(4,53.,psfc,ta,gamma,sst,tpw,htpw,speed,alw,zcld,  &
                  xb%tb85v(i,j),xb%tb85h(i,j),                      &
                  TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,                &
                  TGL_tpw,TGL_htpw,TGL_speed,TGL_alw,               &
                  TGL_zcld,xa%tb85v(i,j),xa%tb85h(i,j)              )
   enddo
   enddo

END SUBROUTINE DA_Transform_XToTb_Lin

!============================================================================
SUBROUTINE TGL_tb(ifreq,theta,p0,ta,gamma,sst,wv,hwv,u,alw,zcld,            &
                  tbv,tbh,                                                  &
                  TGL_p0,TGL_ta,TGL_gamma,TGL_sst,TGL_wv,                   &
                  TGL_hwv,TGL_u,TGL_alw,TGL_zcld,TGL_tbv,TGL_tbh            )
! ---------------------------------------------------------------------------
  IMPLICIT NONE
! ---------------------------------------------------------------------------
!
! Input  : TGL_p0,  TGL_ta,   TGL_gamma, TGL_sst, TGL_wv, TGL_hwv, TGL_u
!          TGL_alw, TGL_zcld
! Output : TGL_tbv, TGL_tbh,  tbv,  tbh
! ---------------------------------------------------------------------------
      integer, intent(IN   ) :: ifreq
      real   , intent(IN   ) :: theta,p0,ta,gamma,sst,wv,hwv,u,alw,zcld
      real   , intent(IN   ) :: TGL_p0,TGL_ta,TGL_gamma,TGL_sst,TGL_wv,     &
                                TGL_hwv,TGL_u,TGL_alw,TGL_zcld
      real   , intent(IN   ) :: tbv,tbh
      real   , intent(  OUT) :: TGL_tbv,TGL_tbh 

      real :: freq(4),ebiasv(4),ebiash(4),cf1(4),cf2(4),cg(4)

      real :: f,costhet,gx2,tbup,tbdn,tauatm,sigma,remv,remh
      real :: effangv,effangh,tbdnv,dum,foam,emissv,emissh 
      real :: refv,refh,semv,semh,scatv,scath,tbdnh
      real :: TGL_gx2,TGL_tbup,TGL_tbdn,TGL_tauatm,TGL_sigma
      real :: TGL_tremv,TGL_remh,TGL_effangv,TGL_effangh
      real :: TGL_tbdnh,TGL_dum,TGL_foam,TGL_emissv
      real :: TGL_emissh,TGL_refv,TGL_refh,TGL_semv
      real :: TGL_semh,TGL_scatv,TGL_scath,TGL_remv,TGL_tbdnv
      real :: TGL_theta

      real :: fem

      data freq/19.35,22.235,37.0,85.5/

! empirical bias corrections for surface emissivity

!
      data ebiasv/0.0095, 0.005,-0.014, -0.0010/
      data ebiash/0.004,   0.0,-0.023, 0.043/


      data cf1/0.0015,0.004,0.0027,0.005/
      data cg/4.50e-3, 5.200e-3, 5.5e-3, 7.2e-3 /

      data cf2/0.0023,0.000,0.0002,-0.006/

! 'foam' emissivity
      data fem /1.0/
!
      f = freq(ifreq)
      costhet = cos(theta*0.017453)

! effective surface slope variance

          gx2 = cg(ifreq)*    u
      TGL_gx2 = cg(ifreq)*TGL_u

! get upwelling atmospheric brightness temperature

      TGL_theta=0.
      call TGL_tbatmos(ifreq,theta,p0,wv,hwv,ta,gamma,alw,zcld, &
                       tbup,tbdn,tauatm,                        &
                       TGL_theta,TGL_p0,TGL_wv,TGL_hwv,TGL_ta,TGL_gamma,  &
                       TGL_alw,TGL_zcld,TGL_tbup,TGL_tbdn,      &
                       TGL_tauatm                               )

! convert transmittance to optical depth

          sigma = -alog(tauatm)*costhet
      TGL_sigma = -costhet*TGL_tauatm/tauatm

! get rough surface emissivity

      call TGL_roughem(ifreq,gx2,sst,theta,remv,remh,         &
                       TGL_gx2,TGL_sst,TGL_remv,TGL_remh      )

! get effective zenith angles for scattered radiation at surface

      call TGL_effang(ifreq,theta,gx2,sigma,effangv,effangh,    &
                      TGL_gx2,TGL_sigma,TGL_effangv,TGL_effangh )

! get effective sky brightness temperatures for scattered radiation

      call TGL_tbatmos(ifreq,effangv,p0,wv,hwv,ta,gamma,alw,    &
                       zcld,dum,tbdnv,dum,                      &
                       TGL_effangv,TGL_p0,TGL_wv,TGL_hwv,       &
                       TGL_ta,TGL_gamma,TGL_alw,TGL_zcld,       & 
                       TGL_dum,TGL_tbdnv,TGL_dum                )

      call TGL_tbatmos(ifreq,effangh,p0,wv,hwv,ta,gamma,alw,    &
                       zcld,dum,tbdnh,dum,                      &
                       TGL_effangh,TGL_p0,TGL_wv,TGL_hwv,       &
                       TGL_ta,TGL_gamma,TGL_alw,TGL_zcld,       &
                       TGL_dum,TGL_tbdnh,TGL_dum                )

! compute 'foam' coverage

          foam = cf1(ifreq)*    u
      TGL_foam = cf1(ifreq)*TGL_u

      if (u .gt. 5.0) then
        TGL_foam = TGL_foam + cf2(ifreq)*TGL_u
            foam =     foam + cf2(ifreq)*(   u-5.0)
      endif

! compute surface emissivities and reflectivity

          emissv =     foam*fem + (1.0 - foam)*(remv + ebiasv(ifreq))
      TGL_emissv = TGL_foam*fem - TGL_foam*(remv + ebiasv(ifreq)) &
                                + (1.0 - foam)*TGL_remv
          emissh =     foam*fem + (1.0 - foam)*(remh + ebiash(ifreq))
      TGL_emissh = TGL_foam*fem - TGL_foam*(remh + ebiash(ifreq)) &
                                + (1.0 - foam)*TGL_remh
          refv =   1.0 - emissv
      TGL_refv = - TGL_emissv
          refh =   1.0 - emissh
      TGL_refh = - TGL_emissh

! compute surface emission term

          semv = sst*emissv
      TGL_semv = TGL_sst*emissv + sst*TGL_emissv
          semh = sst*emissh
      TGL_semh = TGL_sst*emissh + sst*TGL_emissh

! compute surface scattering term

          scatv = refv*tbdnv
      TGL_scatv = TGL_refv*tbdnv + refv*TGL_tbdnv
          scath = refh*tbdnh
      TGL_scath = TGL_refh*tbdnh + refh*TGL_tbdnh

! combine to get space-observed brightness temperature

!         tbv =     tbup + tauatm*(semv + scatv)
      TGL_tbv = TGL_tbup + TGL_tauatm*(semv + scatv)   &
                         + tauatm*(TGL_semv + TGL_scatv)
!         tbh =     tbup + tauatm*(semh + scath)
      TGL_tbh = TGL_tbup + TGL_tauatm*(semh + scath)   &
                         + tauatm*(TGL_semh + TGL_scath)

 end SUBROUTINE TGL_tb

!============================================================================
SUBROUTINE TGL_cal_sigma_v(ifreq,p0,wv,hwv,ta,gamma,sigma_v,                &
                           TGL_p0,TGL_wv,TGL_hwv,TGL_ta,TGL_gamma,TGL_sigma_v)
! ---------------------------------------------------------------------------
IMPLICIT NONE
! ---------------------------------------------------------------------------
!
!Input             : TGL_p0, TGL_wv, TGL_hwv, TGL_ta, TGL_gamma
!Output            : TGL_sigma_v
!Output base field : sigma_v
! ---------------------------------------------------------------------------


      integer ifreq
      real, intent(IN   ):: p0,wv,hwv,ta,gamma  ! base field
      real, intent(IN   ):: TGL_p0,TGL_wv,TGL_hwv,TGL_ta,TGL_gamma
      real, intent(OUT  ):: TGL_sigma_v,sigma_v
!
      real wvc, wvcor(4)
      real TGL_wvc
!
      real voh1,otbar1,pbar1
      real term21,term31,term41,term51,term61
      real a11,a21,a31,a41,a51,a61
      real TGL_voh1,TGL_otbar1,TGL_pbar1
      real TGL_term21,TGL_term31,TGL_term41,TGL_term51,TGL_term61
!
      real voh2,otbar2,pbar2
      real term22,term32,term42,term52,term62
      real a12,a22,a32,a42,a52,a62
      real TGL_voh2,TGL_otbar2,TGL_pbar2
      real TGL_term22,TGL_term32,TGL_term42,TGL_term52,TGL_term62
!
      real voh3,otbar3,pbar3
      real term23,term33,term43,term53,term63
      real a13,a23,a33,a43,a53,a63
      real TGL_voh3,TGL_otbar3,TGL_pbar3
      real TGL_term23,TGL_term33,TGL_term43,TGL_term53,TGL_term63
!
      real voh4,otbar4,pbar4
      real term24,term34,term44,term54,term64
      real a14,a24,a34,a44,a54,a64
      real TGL_voh4,TGL_otbar4,TGL_pbar4
      real TGL_term24,TGL_term34,TGL_term44,TGL_term54,TGL_term64
!
      real const1,const2,const3,const4
      real h1,h2,h3,h4

      real sigv, TGL_sigv
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
!     data wvcor/1.01,0.95,1.06,0.92/
      data wvcor/1.02,0.98,1.02,0.88/
! use modified water vapor value to correct for errors in theoretical absorption
!

          wvc =     wv*wvcor(ifreq)
      TGL_wvc = TGL_wv*wvcor(ifreq)

      if (ifreq.eq.1) then

             pbar1 = p0/(1.0 + hwv/h1)
         TGL_pbar1 = TGL_p0/(1.0 + hwv/h1)-pbar1*TGL_hwv/(h1*(1.0 + hwv/h1))
              voh1 = wv/hwv
          TGL_voh1 = TGL_wv/hwv-voh1*TGL_hwv/hwv
            term21 = a21*voh1
        TGL_term21 = a21*TGL_voh1
            otbar1 =  1.0/(ta - const1*gamma*hwv)
        TGL_otbar1 = -otbar1*otbar1*(TGL_ta-const1*gamma*TGL_hwv &
                                           -const1*TGL_gamma*hwv )
            term31 = a31*otbar1
        TGL_term31 = a31*TGL_otbar1
            term61 = a61*otbar1*otbar1
        TGL_term61 = 2.*a61*otbar1*TGL_otbar1
            term41 = a41*pbar1*otbar1
        TGL_term41 = a41*(TGL_pbar1*otbar1+pbar1*TGL_otbar1)
            term51 = a51*voh1*otbar1
        TGL_term51 = a51*(TGL_voh1*otbar1+voh1*TGL_otbar1)
              sigv = a11 + term21 + term31 + term41 + term51 + term61
          TGL_sigv = TGL_term21+TGL_term31+TGL_term41+TGL_term51+TGL_term61

      else if (ifreq.eq.2) then

            pbar2  = p0/(1.0 + hwv/h2)
        TGL_pbar2  = TGL_p0/(1.0 + hwv/h2)-pbar2*TGL_hwv/h2/(1.0 + hwv/h2)
            term22 = a22*pbar2
        TGL_term22 = a22*TGL_pbar2
            term52 = a52*pbar2*pbar2
        TGL_term52 = 2.*a52*pbar2*TGL_pbar2
              voh2 = wv/hwv
          TGL_voh2 = TGL_wv/hwv-voh2*TGL_hwv/hwv
            term32 = a32*voh2
        TGL_term32 = a32*TGL_voh2
            otbar2 = 1.0/(ta - const2*gamma*hwv)
        TGL_otbar2 = -otbar2*otbar2*(TGL_ta-const2*gamma*TGL_hwv &
                                           -const2*TGL_gamma*hwv )
            term42 = a42*otbar2
        TGL_term42 = a42*TGL_otbar2
            term62 = a62*otbar2*otbar2
        TGL_term62 = 2.*a62*otbar2*TGL_otbar2
              sigv = a12 + term22 + term32 + term42 + term52 + term62
          TGL_sigv = TGL_term22 + TGL_term32 + TGL_term42 + TGL_term52 + TGL_term62

      else if (ifreq.eq.3) then

             pbar3 = p0/(1.0 + hwv/h3)
         TGL_pbar3 = TGL_p0/(1.0 + hwv/h3)-pbar3*TGL_hwv/h3/(1.0 + hwv/h3)
            term43 = a43*pbar3*pbar3
        TGL_term43 = 2.*a43*pbar3*TGL_pbar3
              voh3 = wv/hwv
          TGL_voh3 = TGL_wv/hwv-voh3*TGL_hwv/hwv
            term23 = a23*voh3
        TGL_term23 = a23*TGL_voh3
            otbar3 = 1.0/(ta - const3*gamma*hwv)
        TGL_otbar3 = -otbar3*otbar3*(TGL_ta-const3*gamma*TGL_hwv &
                                           -const3*TGL_gamma*hwv )
            term33 = a33*otbar3
        TGL_term33 = a33*TGL_otbar3
            term53 = a53*pbar3*voh3
        TGL_term53 = a53*(TGL_pbar3*voh3+pbar3*TGL_voh3)
            term63 = a63*otbar3*voh3
        TGL_term63 = a63*(TGL_otbar3*voh3+otbar3*TGL_voh3)
              sigv = a13 + term23 + term33 + term43 + term53 + term63
          TGL_sigv = TGL_term23 + TGL_term33 + TGL_term43 + TGL_term53 + TGL_term63

      else if (ifreq.eq.4) then

             pbar4 = p0/(1.0 + hwv/h4)
         TGL_pbar4 = TGL_p0/(1.0 + hwv/h4)-pbar4*TGL_hwv/h4/(1.0 + hwv/h4)
            term44 = a44*pbar4*pbar4
        TGL_term44 = 2.*a44*pbar4*TGL_pbar4
              voh4 = wv/hwv
          TGL_voh4 = TGL_wv/hwv-voh4*TGL_hwv/hwv
            term24 = a24*voh4
        TGL_term24 = a24*TGL_voh4
            otbar4 = 1.0/(ta - const4*gamma*hwv)
        TGL_otbar4 = -otbar4*otbar4*(TGL_ta-const4*gamma*TGL_hwv &
                                           -const4*TGL_gamma*hwv )
            term34 = a34*otbar4
        TGL_term34 = a34*TGL_otbar4
            term54 = a54*pbar4*voh4
        TGL_term54 = a54*(TGL_pbar4*voh4+pbar4*TGL_voh4)
            term64 = a64*otbar4*voh4
        TGL_term64 = a64*(TGL_otbar4*voh4+otbar4*TGL_voh4)
              sigv = a14 + term24 + term34 + term44 + term54 + term64
          TGL_sigv = TGL_term24 + TGL_term34 + TGL_term44 + TGL_term54 + TGL_term64

      else
              sigv = 0.0
          TGL_sigv = 0.0

      endif

           sigma_v = sigv*wvc
       TGL_sigma_v = TGL_sigv*wvc+sigv*TGL_wvc

END SUBROUTINE TGL_cal_sigma_v

!====================================================================
SUBROUTINE TGL_effht(ho,hv,sigo,sigv,mu,zcld,hdn,hup,hdninf,hupinf, &
                     TGL_ho,TGL_hv,TGL_sigo,TGL_sigv,TGL_mu,        &
                     TGL_zcld,TGL_hdn,TGL_hup,TGL_hdninf,TGL_hupinf )
!--------------------------------------------------------------------
IMPLICIT NONE
!--------------------------------------------------------------------
!
!Input  : TGL_ho, TGL_hv, TGL_sigo, TGL_sigv, TGL_mu, TGL_zcld
!Output : TGL_hdn, hdn, TGL_hup, hup, 
!         TGL_hdninf, hdninf, TGL_hupinf, hupinf
!--------------------------------------------------------------------

      real,   intent(IN   ) :: ho,hv,sigo,sigv,mu,zcld
      real,   intent(IN   ) :: TGL_ho,TGL_hv,TGL_sigo,TGL_sigv,TGL_zcld, &
                               TGL_mu
      real,   intent(  OUT) :: hdn,hup,hdninf,hupinf
      real,   intent(  OUT) :: TGL_hdn,TGL_hup,TGL_hdninf,TGL_hupinf

      real :: gint,zgint,hint,zhint
      real :: ginf,zginf,hinf,zhinf
      real :: TGL_gint,TGL_zgint,TGL_hint,TGL_zhint
      real :: TGL_ginf,TGL_zginf,TGL_hinf,TGL_zhinf
      real :: TGL_mu2,TGL_halfmu,TGL_sixthmu2,TGL_etnthmu2
      real :: TGL_quartmu,TGL_halfmu2
!
      real :: hoinv,hvinv,chio,chiv,ezho,ezhv,alpha,alph2,alph3
      real :: beta,beta2,beta3,mu2,mualph,cplus,cmin,dplus,dmin
      real :: chiov,chivv,chioo,chioov,chiovv,chiooo,chivvv
      real :: h11,h21,h12,newh11
      real :: sigoo,sigov,sigvv,sigooo,sigoov,sigovv,sigvvv
      real :: ezhoo,ezhov,ezhvv,ezhooo,ezhoov,ezhovv,ezhvvv
      real :: s,sprim,t,tprim,u,uprim,term1,term2,term3
      real :: halfmu,halfmu2,sixthmu2,etnthmu2,quartmu

      real :: TGL_hoinv,TGL_hvinv,TGL_chio,TGL_chiv,TGL_ezho
      real :: TGL_ezhv,TGL_alpha,TGL_alph2,TGL_alph3
      real :: TGL_beta,TGL_beta2,TGL_beta3,TGL_mualph
      real :: TGL_cplus,TGL_cmin,TGL_dplus,TGL_dmin
      real :: TGL_chiov,TGL_chivv,TGL_chioo,TGL_chioov
      real :: TGL_chiovv,TGL_chiooo,TGL_chivvv
      real :: TGL_h11,TGL_h21,TGL_h12,TGL_newh11
      real :: TGL_sigoo,TGL_sigov,TGL_sigvv,TGL_sigooo
      real :: TGL_sigoov,TGL_sigovv,TGL_sigvvv
      real :: TGL_ezhoo,TGL_ezhov,TGL_ezhvv,TGL_ezhooo
      real :: TGL_ezhoov,TGL_ezhovv,TGL_ezhvvv
      real :: TGL_s,TGL_sprim,TGL_t,TGL_tprim
      real :: TGL_u,TGL_uprim,TGL_term1,TGL_term2,TGL_term3
!
          hoinv =  1.0d0/ho
      TGL_hoinv = -1.0d0*hoinv*hoinv*TGL_ho

          hvinv =  1.0d0/hv
      TGL_hvinv = -1.0d0*hvinv*hvinv*TGL_hv

           chio = zcld*hoinv
       TGL_chio = TGL_zcld*hoinv + zcld*TGL_hoinv

           chiv = zcld*hvinv
       TGL_chiv = TGL_zcld*hvinv + zcld*TGL_hvinv

           ezho = sigo*exp(-chio)
       TGL_ezho = TGL_sigo*exp(-chio)-TGL_chio*ezho

           ezhv = sigv*exp(-chiv)
       TGL_ezhv = TGL_sigv*exp(-chiv)-TGL_chiv*ezhv

          alpha = sigo + sigv
      TGL_alpha = TGL_sigo + TGL_sigv

          alph2 = alpha*alpha
      TGL_alph2 = 2.*alpha*TGL_alpha

          alph3 = alpha*alph2
      TGL_alph3 = TGL_alpha*alph2+alpha*TGL_alph2

           beta = ezho + ezhv
       TGL_beta = TGL_ezho + TGL_ezhv

          beta2 = beta*beta
      TGL_beta2 = 2.*beta*TGL_beta

          beta3 = beta*beta2
      TGL_beta3 = TGL_beta*beta2+beta*TGL_beta2
!
          mu2        = mu*mu
      TGL_mu2        = 2.*mu*TGL_mu
          halfmu     = 0.5d0*    mu
      TGL_halfmu     = 0.5d0*TGL_mu
          sixthmu2   =     mu2/6.0d0
      TGL_sixthmu2   = TGL_mu2/6.0d0
          etnthmu2   =     mu2/18.0d0
      TGL_etnthmu2   = TGL_mu2/18.0d0
          quartmu    = 0.25d0*    mu
      TGL_quartmu    = 0.25d0*TGL_mu
          halfmu2    = 0.5d0*    mu2
      TGL_halfmu2    = 0.5d0*TGL_mu2

          mualph = mu*alpha
      TGL_mualph = TGL_mu*alpha + mu*TGL_alpha

          cplus  = 1.0d0 +     mualph
      TGL_cplus  =         TGL_mualph

          cmin   = 1.0d0 -     mualph
      TGL_cmin   =       - TGL_mualph

          dplus  = halfmu2*alph2
      TGL_dplus  = TGL_halfmu2*alph2 + halfmu2*TGL_alph2

          dmin   =     dplus
      TGL_dmin   = TGL_dplus

      TGL_dplus  = TGL_cplus + TGL_dplus
          dplus  =     cplus +     dplus

      TGL_dmin   = TGL_cmin  + TGL_dmin
          dmin   =     cmin  +     dmin
!

          h11    =     hoinv +     hvinv
      TGL_h11    = TGL_hoinv + TGL_hvinv

          h21    =  1.0d0/(h11 + hvinv)
      TGL_h21    = -1.0d0*h21*h21*(TGL_h11+TGL_hvinv)

          h12    =  1.0d0/(h11 + hoinv)
      TGL_h12    = -1.0d0*h12*h12*(TGL_h11 + TGL_hoinv)

          newh11 =  1.0d0/h11
      TGL_newh11 = -1.0d0*newh11*newh11*TGL_h11

          chiov  = 1.0d0 +     chio +     chiv
      TGL_chiov  =         TGL_chio + TGL_chiv

          chioo  = 1.0d0 +     chio +     chio
      TGL_chioo  =         TGL_chio + TGL_chio

          chivv  = 1.0d0 +     chiv +     chiv
      TGL_chivv  =         TGL_chiv + TGL_chiv

          chioov =     chioo +     chiv
      TGL_chioov = TGL_chioo + TGL_chiv

          chiovv =     chio  +     chivv
      TGL_chiovv = TGL_chio  + TGL_chivv

          chiooo =     chioo +     chio
      TGL_chiooo = TGL_chioo + TGL_chio

          chivvv =     chivv +     chiv
      TGL_chivvv = TGL_chivv + TGL_chiv

      TGL_chio   =         TGL_chio
          chio   = 1.0d0 +     chio

      TGL_chiv   =         TGL_chiv
          chiv   = 1.0d0 +     chiv

          sigov  = sigo*sigv
      TGL_sigov  = TGL_sigo*sigv + sigo*TGL_sigv

          sigoo  = sigo*sigo
      TGL_sigoo  = 2.*sigo*TGL_sigo

          sigvv  = sigv*sigv
      TGL_sigvv  = 2.*sigv*TGL_sigv

          sigooo = sigoo*sigo
      TGL_sigooo = TGL_sigoo*sigo + sigoo*TGL_sigo

          sigoov = sigoo*sigv
      TGL_sigoov = TGL_sigoo*sigv + sigoo*TGL_sigv

          sigovv = sigo*sigvv
      TGL_sigovv = TGL_sigo*sigvv + sigo*TGL_sigvv

          sigvvv = sigvv*sigv
      TGL_sigvvv = TGL_sigvv*sigv + sigvv*TGL_sigv

          ezhoo  = ezho*ezho
      TGL_ezhoo  = 2.*ezho*TGL_ezho

          ezhov  = ezho*ezhv
      TGL_ezhov  = TGL_ezho*ezhv + ezho*TGL_ezhv

          ezhvv  = ezhv*ezhv
      TGL_ezhvv  = 2.*ezhv*TGL_ezhv

          ezhovv = ezho*ezhvv
      TGL_ezhovv = TGL_ezho*ezhvv + ezho*TGL_ezhvv

          ezhoov = ezhoo*ezhv
      TGL_ezhoov = TGL_ezhoo*ezhv + ezhoo*TGL_ezhv

          ezhooo = ezhoo*ezho
      TGL_ezhooo = TGL_ezhoo*ezho + ezhoo*TGL_ezho

          ezhvvv = ezhvv*ezhv
      TGL_ezhvvv = TGL_ezhvv*ezhv + ezhvv*TGL_ezhv

          s      = sigo*ho + sigv*hv
      TGL_s      = TGL_sigo*ho + sigo*TGL_ho + TGL_sigv*hv + sigv*TGL_hv

          sprim  = ezho*ho*chio + ezhv*hv*chiv
      TGL_sprim  = TGL_ezho*ho*chio + ezho*TGL_ho*chio + ezho*ho*TGL_chio + &
                   TGL_ezhv*hv*chiv + ezhv*TGL_hv*chiv + ezhv*hv*TGL_chiv

          t      = sigoo*ho + 4.0d0*sigov*newh11 + sigvv*hv
      TGL_t      = TGL_sigoo*ho + sigoo*TGL_ho + &
                   4.0d0*(TGL_sigov*newh11 + sigov*TGL_newh11) + &
                   TGL_sigvv*hv + sigvv*TGL_hv

          tprim  = ezhoo*ho*chioo + 4.0d0*ezhov*newh11*chiov + ezhvv*hv*chivv
      TGL_tprim  = TGL_ezhoo*ho*chioo +ezhoo*TGL_ho*chioo + ezhoo*ho*TGL_chioo + &
                   4.0d0*(TGL_ezhov*newh11*chiov +    &
                          ezhov*TGL_newh11*chiov +    &
                          ezhov*newh11*TGL_chiov  ) + &
                   TGL_ezhvv*hv*chivv + ezhvv*TGL_hv*chivv + ezhvv*hv*TGL_chivv

          u      = sigooo*ho + 9.0d0*(sigovv*h21+sigoov*h12) + sigvvv*hv
      TGL_u      = TGL_sigooo*ho + sigooo*TGL_ho + &
                   9.0d0*(TGL_sigovv*h21 + sigovv*TGL_h21 +    &
                          TGL_sigoov*h12 + sigoov*TGL_h12  ) + &
                   TGL_sigvvv*hv + sigvvv*TGL_hv

          uprim  = ezhvvv*hv*chivvv +  &
                   9.0d0*(ezhovv*h21*chiovv + ezhoov*h12*chioov) + &
                   ezhooo*ho*chiooo
      TGL_uprim  = TGL_ezhvvv*hv*chivvv +ezhvvv*TGL_hv*chivvv+ ezhvvv*hv*TGL_chivvv+  &
                    9.0d0*(TGL_ezhovv*h21*chiovv +     &
                           ezhovv*TGL_h21*chiovv +     &
                           ezhovv*h21*TGL_chiovv +     &
                           TGL_ezhoov*h12*chioov +     &
                           ezhoov*TGL_h12*chioov +     &
                           ezhoov*h12*TGL_chioov   ) + &
                    TGL_ezhooo*ho*chiooo + ezhooo*TGL_ho*chiooo + ezhooo*ho*TGL_chiooo
!
          term1  =     s -     sprim
      TGL_term1  = TGL_s - TGL_sprim

          term2  = quartmu*(t - tprim)
      TGL_term2  = TGL_quartmu*(t - tprim) + quartmu*(TGL_t - TGL_tprim) 

          term3  = etnthmu2*(    u -     uprim)
      TGL_term3  = TGL_etnthmu2*(u - uprim) + etnthmu2*(TGL_u - TGL_uprim)

          zgint  = dmin*term1 +  cmin*term2 + term3
      TGL_zgint  = TGL_dmin*term1 + dmin*TGL_term1 + &
                   TGL_cmin*term2 + cmin*TGL_term2 + TGL_term3

          zhint  = -dplus*term1 + cplus*term2 - term3
      TGL_zhint  = -TGL_dplus*term1 - dplus*TGL_term1 + &
                    TGL_cplus*term2 + cplus*TGL_term2 - TGL_term3

          term2  = quartmu * t
      TGL_term2  = TGL_quartmu*t + quartmu*TGL_t

          term3  = etnthmu2*u
      TGL_term3  = TGL_etnthmu2*u + etnthmu2*TGL_u

          zginf  = dmin*s +  cmin*term2 + term3
      TGL_zginf  = TGL_dmin*s + dmin*TGL_s +  &
                   TGL_cmin*term2 + cmin*TGL_term2 + TGL_term3

          zhinf  = -dplus*s + cplus*term2 - term3
      TGL_zhinf  = -TGL_dplus*s - dplus*TGL_s + &
                    TGL_cplus*term2 + cplus*TGL_term2 - TGL_term3

          term1  =     alpha -     beta
      TGL_term1  = TGL_alpha - TGL_beta

          term2  = halfmu*(    alph2 -     beta2)
      TGL_term2  = TGL_halfmu*(alph2 - beta2) + halfmu*(TGL_alph2 - TGL_beta2)

          term3  = sixthmu2*(    alph3 -     beta3)
      TGL_term3  = TGL_sixthmu2*(alph3 - beta3) + sixthmu2*(TGL_alph3 - TGL_beta3)

          gint   = dmin*term1 +  cmin*term2 + term3
      TGL_gint   = TGL_dmin*term1 + dmin*TGL_term1 + &
                   TGL_cmin*term2 + cmin*TGL_term2 + TGL_term3

          hint   = -dplus*term1 + cplus*term2 - term3
      TGL_hint   = -TGL_dplus*term1 - dplus*TGL_term1 + &
                    TGL_cplus*term2 + cplus*TGL_term2 - TGL_term3

          term2  = halfmu*alph2
      TGL_term2  = TGL_halfmu*alph2 + halfmu*TGL_alph2

          term3  = sixthmu2*alph3
      TGL_term3  = TGL_sixthmu2*alph3 + sixthmu2*TGL_alph3

          ginf   = dmin*alpha +  cmin*term2 + term3
      TGL_ginf   = TGL_dmin*alpha + dmin*TGL_alpha +  &
                   TGL_cmin*term2 + cmin*TGL_term2 + TGL_term3

          hinf   = -dplus*alpha + cplus*term2 - term3
      TGL_hinf   = -TGL_dplus*alpha - dplus*TGL_alpha + &
                    TGL_cplus*term2 + cplus*TGL_term2 - TGL_term3

          hdn    = zgint/gint
      TGL_hdn    = TGL_zgint/gint - hdn * TGL_gint/gint

          hup    = zhint/hint
      TGL_hup    = TGL_zhint/hint - hup*TGL_hint/hint

          hdninf = zginf/ginf
      TGL_hdninf = TGL_zginf/ginf - hdninf*TGL_ginf/ginf

          hupinf = zhinf/hinf
      TGL_hupinf = TGL_zhinf/hinf - hupinf*TGL_hinf/hinf

END SUBROUTINE TGL_effht

!============================================================================
SUBROUTINE TGL_epsalt(f,t,ssw,epsr,epsi,                                    &
                      TGL_t, TGL_epsr, TGL_epsi                             )
! ---------------------------------------------------------------------------
IMPLICIT NONE
! ---------------------------------------------------------------------------
!
!Input  : TGL_t      (ssw is treated as a constant now)
!Output : TGL_epsr, TGL_epsi, epsr, epsi
! ---------------------------------------------------------------------------

!     returns the complex dielectric constant of sea water, using the
!     model of Klein and Swift (1977)
!
!     Input   f = frequency (GHz)
!             t = temperature (C)
!             ssw = salinity (permil) (if ssw < 0, ssw = 32.54)
!     Output  epsr,epsi  = real and imaginary parts of dielectric constant
!
      real, intent(IN   ) :: f, t, TGL_t
      real, intent(INOUT) :: ssw
      real, intent(  OUT) :: TGL_epsr, TGL_epsi, epsr, epsi

      complex :: cdum1,cdum2,cdum3
      complex :: TGL_cdum1,TGL_cdum2,TGL_cdum3
      real :: pi
      parameter (pi = 3.14159265)
      real :: ssw2,ssw3,t2,t3,es,a,esnew,tau,b,sig,taunew
      real :: delt,delt2,beta,signew,om,d1,d2
      real :: TGL_t2,TGL_t3,TGL_es,TGL_a,TGL_esnew,TGL_tau,TGL_b,TGL_taunew
      real :: TGL_delt,TGL_delt2,TGL_beta,TGL_signew
      real :: TGL_d1,TGL_d2
!
      if (ssw .lt. 0.0) ssw = 32.54
      ssw2       = ssw*ssw
      ssw3       = ssw2*ssw
          t2     = t*t
      TGL_t2     = 2.*t*TGL_t
          t3     = t2*t
      TGL_t3     = TGL_t2*t + t2*TGL_t
          es     = 87.134 - 1.949e-1*t     - 1.276e-2*t2     + 2.491e-4*t3
      TGL_es     =        - 1.949e-1*TGL_t - 1.276e-2*TGL_t2 + 2.491e-4*TGL_t3

          a      = 1.0 + 1.613e-5*ssw*t - 3.656e-3*ssw + 3.21e-5*ssw2 - &
                   4.232e-7*ssw3
      TGL_a      = 1.613e-5*ssw*TGL_t 
          esnew  = es*a
      TGL_esnew  = TGL_es*a + es*TGL_a
!
          tau    = 1.768e-11 - 6.086e-13*t     + 1.104e-14*t2     - 8.111e-17*t3
      TGL_tau    =           - 6.086e-13*TGL_t + 1.104e-14*TGL_t2 - 8.111e-17*TGL_t3
          b      = 1.0 + 2.282e-5*ssw*t - 7.638e-4*ssw - 7.760e-6*ssw2 + &
                   1.105e-8*ssw3
      TGL_b      = 2.282e-5*ssw*TGL_t 
          taunew = tau*b
      TGL_taunew = TGL_tau*b + tau*TGL_b
!
      sig        = ssw*(0.182521 - 1.46192e-3*ssw + 2.09324e-5*ssw2 - &
                   1.28205e-7*ssw3)
          delt   = 25.0 - t
      TGL_delt   =      - TGL_t
          delt2  = delt*delt
      TGL_delt2  = 2.*delt*TGL_delt
          beta   =   2.033e-2 + 1.266e-4*delt      + 2.464e-6*delt2       &
                   - ssw*(1.849e-5 - 2.551e-7*delt + 2.551e-8*delt2)
      TGL_beta   =              1.266e-4*TGL_delt  + 2.464e-6*TGL_delt2   &
                   - ssw*(-2.551e-7*TGL_delt + 2.551e-8*TGL_delt2)
      signew     =   sig*exp(-beta*delt)
      TGL_signew = - signew*(TGL_beta*delt+beta*TGL_delt)
!
      om         = 2.0e9*pi*f
          cdum1  = cmplx(0.0,om*taunew)
      TGL_cdum1  = cmplx(0.0,om*TGL_taunew)
          cdum2  = cmplx(0.0,signew/(om*8.854e-12))
      TGL_cdum2  = cmplx(0.0,TGL_signew/(om*8.854e-12))

      cdum3      = 4.9 + (esnew-4.9)/(1.0 + cdum1) - cdum2
      TGL_cdum3  =  TGL_esnew/(1.0 + cdum1)  &
                  - TGL_cdum1*(esnew-4.9)/((1.0 + cdum1)*(1.0 + cdum1))  &
                  - TGL_cdum2
      epsr       = real(cdum3)
      TGL_epsr   = real(TGL_cdum3)
      epsi       = -aimag(cdum3)
      TGL_epsi   = -aimag(TGL_cdum3)

END SUBROUTINE TGL_epsalt

!---------------------------------------------------------------------------
SUBROUTINE TGL_spemiss(f,tk,theta,ssw,ev,eh,                               &
                       TGL_tk,TGL_ev,TGL_eh                                )
!---------------------------------------------------------------------------
IMPLICIT NONE
!---------------------------------------------------------------------------
!
!Input  : TGL_tk
!Output : TGL_ev, TGL_eh, ev, eh
!---------------------------------------------------------------------------
!     returns the specular emissivity of sea water for given freq. (GHz), 
!     temperature T (K), incidence angle theta (degrees), salinity (permil)
!     
!     Returned values verified against data in Klein and Swift (1977) and
!     against Table 3.8 in Olson (1987, Ph.D. Thesis)
!
      real, intent(IN   ) :: f, tk, theta, TGL_tk
      real, intent(INOUT) :: ssw
      real, intent(  OUT) :: TGL_ev, TGL_eh, ev, eh

      real   epsr,epsi,TGL_epsr,TGL_epsi
!
      real      tc,costh,sinth,rthet
      complex   etav,etah,eps,cterm1v,cterm1h,cterm2,cterm3v,cterm3h,epsnew
      complex   TGL_etav,TGL_eps,TGL_cterm1v,TGL_cterm2,TGL_cterm3v
      complex   TGL_cterm3h,TGL_epsnew
      complex   uniti
      real      tmp1r,tmp1i,tmp2r,tmp2i,tmp0r,tmp0i,rnorm
      real      TGL_tc,TGL_tmp0r,TGL_tmp0i,TGL_rnorm,TGL_tmp1r
      real      TGL_tmp1i,TGL_tmp2r,TGL_tmp2i
!

      tc          =      tk - 273.15
      TGL_tc      =  TGL_tk

      CALL TGL_epsalt(f,tc,ssw,epsr,epsi,         &
                      TGL_tc, TGL_epsr, TGL_epsi  )

          eps     =  cmplx(epsr,epsi)
      TGL_eps     =  cmplx(TGL_epsr,TGL_epsi)
          etav    =  eps
      TGL_etav    =  TGL_eps
      etah        =  (1.0,0.0)
      rthet       =  theta*0.017453292
      costh       =  cos(rthet)
      sinth       =  sin(rthet)
      sinth       =  sinth*sinth
          cterm1v =  etav*costh
      TGL_cterm1v =  TGL_etav*costh
      cterm1h     =  etah*costh
          epsnew  =      eps - sinth
      TGL_epsnew  =  TGL_eps
      cterm2      =  csqrt(epsnew)

! calculate TGL_cterm2

      if (cabs(epsnew) .gt. 0.) then
         TGL_cterm2      =  TGL_epsnew*0.5/cterm2
      else
         TGL_cterm2      =  0.
      endif

! Wei's Comment
!     It is not a standard fortran if statement here.

!     if(0) then
!               tmp0r   =  real(epsnew)
!           TGL_tmp0r   =  real(TGL_epsnew)
!               tmp0i   = -aimag(epsnew)
!           TGL_tmp0i   = -aimag(TGL_epsnew)
!               rnorm   =  sqrt(tmp0r*tmp0r+tmp0i*tmp0i)
!               uniti   =  (0,1)
!           if(rnorm .gt. 0.) then
!             if (abs(tmp0i) .gt. 0.) then
!                TGL_rnorm =  (tmp0r*TGL_tmp0r + tmp0i*TGL_tmp0i)/rnorm
!                TGL_cterm2=  cterm2*0.5*(TGL_rnorm/rnorm  &
!                                      -uniti*(TGL_tmp0r*rnorm-TGL_rnorm*tmp0r)/(rnorm*tmp0i))
!             else
!                TGL_rnorm =  TGL_tmp0r
!                TGL_cterm2=  TGL_tmp0r*0.5/sqrt(tmp0r)
!             endif
!           else 
!             TGL_rnorm =  0.
!             TGL_cterm2=  0.
!           endif
!     endif

! End Wei's Comment

          cterm3v =  (cterm1v - cterm2)/(cterm1v + cterm2)
      TGL_cterm3v =  (TGL_cterm1v - TGL_cterm2)/(cterm1v + cterm2) &
                    -cterm3v*(TGL_cterm1v + TGL_cterm2)/(cterm1v + cterm2)
          cterm3h =  (cterm1h - cterm2)/(cterm1h + cterm2)
      TGL_cterm3h = -TGL_cterm2/(cterm1h + cterm2) &
                    -cterm3h*TGL_cterm2/(cterm1h + cterm2)
          tmp1r   =  real(cterm3v)
      TGL_tmp1r   =  real(TGL_cterm3v)
          tmp1i   = -aimag(cterm3v)
      TGL_tmp1i   = -aimag(TGL_cterm3v)
!         ev      =  1.0 - cabs(cterm3v)**2
          ev      =  1.0 - (tmp1r*tmp1r+tmp1i*tmp1i)
      TGL_ev      = -2.*tmp1r*TGL_tmp1r - 2.*tmp1i*TGL_tmp1i

          tmp2r   =  real(cterm3h)
      TGL_tmp2r   =  real(TGL_cterm3h)
          tmp2i   = -aimag(cterm3h)
      TGL_tmp2i   = -aimag(TGL_cterm3h)
!         eh      =  1.0 - cabs(cterm3h)**2
          eh      =  1.0 - (tmp2r*tmp2r+tmp2i*tmp2i)
      TGL_eh      = -2.*tmp2r*TGL_tmp2r - 2.*tmp2i*TGL_tmp2i
!
END SUBROUTINE TGL_spemiss

!==================================================================
 SUBROUTINE TGL_tbatmos(ifreq,theta,p0,wv,hwv,ta,gamma,lw,zcld,   &
                        tbup,tbdn,tauatm,                         &
                        TGL_theta,TGL_p0,TGL_wv,TGL_hwv,TGL_ta,TGL_gamma,   &
                        TGL_lw,TGL_zcld,TGL_tbup,TGL_tbdn,        &
                        TGL_tauatm                                )
! -----------------------------------------------------------------
  IMPLICIT NONE
! -----------------------------------------------------------------
!
! Input  : TGL_p0,TGL_wv,TGL_hwv,TGL_ta,TGL_gamma,TGL_lw,TGL_zcld 
!          TGL_theta (somtime theta is a variable)
! Output : TGL_tbup,TGL_tbdn,TGL_tauatm,tbup,tbdn,tauatm
! -----------------------------------------------------------------
      integer,intent(IN   ) :: ifreq
      real   ,intent(IN   ) :: theta,p0,wv,hwv,ta,gamma,lw,zcld
      real   ,intent(IN   ) :: TGL_p0,TGL_wv,TGL_hwv,TGL_ta,       &
                               TGL_gamma,TGL_lw,TGL_zcld,TGL_theta
      real   ,intent(  OUT) :: tbup,tbdn,tauatm,TGL_tbup,TGL_tbdn, &
                               TGL_tauatm

      real :: mu,hdn,hup,hdninf,hupinf,TGL_mu
!
      real b1(4),b2(4),b3(4)
      real c(4),d1(4),d2(4),d3(4),zeta(4),kw0(4),kw1(4),kw2(4),kw3(4)
      real tau,tau1,tau2,taucld
      real tcld,tc,em,em1
      real sigv,sigo,sig,sig1,sigcld
      real teff1dn,teff1up,teffdn,teffup
      real tbcld,tbclrdn,tbclrup,tb1dn,tb1up,tb2dn,tb2up
      real :: otbar,tc2,tc3,hv,ho,alph
      real :: TGL_sigv,TGL_otbar,TGL_sigo,TGL_tcld,TGL_tc,TGL_tc2,TGL_tc3
      real :: TGL_sigcld,TGL_taucld,TGL_tbcld,TGL_hv,TGL_ho
      real :: TGL_hdn,TGL_hup,TGL_hdninf,TGL_sig,TGL_sig1,TGL_tau,TGL_tau1
      real :: TGL_tau2,TGL_em1,TGL_teff1dn,TGL_hupinf,TGL_em,TGL_teff1up
      real :: TGL_teffdn,TGL_teffup,TGL_tbclrdn,TGL_tbclrup,TGL_tb1dn,TGL_tb1up
      real :: TGL_tb2dn,TGL_tb2up,TGL_alph

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
! somtime theta is a variable

      mu     = 1.0/cos(theta*0.0174533)
      TGL_mu = mu*mu*0.0174533*TGL_theta*sin(theta*0.0174533)
! get water vapor optical depth
!=====

      CALL TGL_cal_sigma_v(ifreq,p0,wv,hwv,ta,gamma,sigv,   &
                           TGL_p0,TGL_wv,TGL_hwv,TGL_ta,    &
                           TGL_gamma,TGL_sigv               )

! otbar = one over "mean" temperature

          otbar =   1.0/(ta - gamma*zeta(ifreq))
      TGL_otbar = - otbar*otbar*(TGL_ta - TGL_gamma*zeta(ifreq))

! sigo = dry air optical depth

          sigo = b1(ifreq) + b2(ifreq)*    p0  + b3(ifreq)*    otbar
      TGL_sigo =             b2(ifreq)*TGL_p0  + b3(ifreq)*TGL_otbar

! cloud parameters

          tcld   =     ta - gamma*zcld
      TGL_tcld   = TGL_ta - TGL_gamma*zcld - gamma*TGL_zcld
            tc   =     tcld - 273.15
        TGL_tc   = TGL_tcld
           tc2   = tc*tc
       TGL_tc2   = 2.*tc*TGL_tc
           tc3   = tc2*tc
       TGL_tc3   = TGL_tc2*tc + tc2*TGL_tc
          sigcld =  ( kw0(ifreq) + tc*kw1(ifreq) + tc2*kw2(ifreq) +  &
                      tc3*kw3(ifreq) )*lw
      TGL_sigcld =  ( TGL_tc *kw1(ifreq) + TGL_tc2*kw2(ifreq) +  &
                      TGL_tc3*kw3(ifreq) )*lw &
                  + ( kw0(ifreq) + tc*kw1(ifreq) + tc2*kw2(ifreq) +  &
                      tc3*kw3(ifreq) )*TGL_lw

          taucld =   exp(-mu*sigcld)
      TGL_taucld = - (TGL_mu*sigcld + mu*TGL_sigcld)*taucld
           tbcld =   (1.0 - taucld)*tcld
       TGL_tbcld = - TGL_taucld*tcld + (1.0 - taucld)*TGL_tcld

! hv, ho = effective absorber scale heights for vapor, dry air

          hv = c(ifreq)*    hwv
      TGL_hv = c(ifreq)*TGL_hwv
          ho = d1(ifreq) + d2(ifreq)*    ta + d3(ifreq)*    gamma
      TGL_ho =             d2(ifreq)*TGL_ta + d3(ifreq)*TGL_gamma

! get effective emission heights for layer 1 and total atmosphere

      call TGL_effht(ho,hv,sigo,sigv,mu,zcld,hdn,hup,        &
                     hdninf,hupinf,                          &
                     TGL_ho,TGL_hv,TGL_sigo,TGL_sigv,TGL_mu, &
                     TGL_zcld,TGL_hdn,TGL_hup,TGL_hdninf,    &
                     TGL_hupinf                              )

! atmospheric transmittances in layer one and two, and combined

           sig =     sigo +     sigv

       TGL_sig = TGL_sigo + TGL_sigv
          sig1 = sigo*(1.0-exp(-zcld/ho)) + sigv*(1.0-exp(-zcld/hv))
      TGL_sig1 =   TGL_sigo*(1.0-exp(-zcld/ho))  &
                 + TGL_sigv*(1.0-exp(-zcld/hv))  &
                 + sigo*(TGL_zcld/ho - zcld*TGL_ho/(ho*ho))*exp(-zcld/ho) &
                 + sigv*(TGL_zcld/hv - zcld*TGL_hv/(hv*hv))*exp(-zcld/hv)
          tau  =  exp(-mu*sig)
      TGL_tau  = -(TGL_mu*sig + mu*TGL_sig) * tau
          tau1 =  exp(-mu*sig1)
      TGL_tau1 = -(mu*TGL_sig1 + TGL_mu*sig1) *tau1
          tau2 =  tau/tau1
      TGL_tau2 =  TGL_tau/tau1 - tau2*TGL_tau1/tau1

! atmospheric "emissivity"

          em1  =   1.0 - tau1
      TGL_em1  = - TGL_tau1
          em   =   1.0 - tau
      TGL_em   = - TGL_tau

! downwelling and upwelling brightness temperature for each layer

          teff1dn =     ta - gamma*hdn
      TGL_teff1dn = TGL_ta - TGL_gamma*hdn - gamma*TGL_hdn
          teff1up =     ta - gamma*hup
      TGL_teff1up = TGL_ta - TGL_gamma*hup - gamma*TGL_hup
           teffdn =     ta - gamma*hdninf
       TGL_teffdn = TGL_ta - TGL_gamma*hdninf - gamma*TGL_hdninf
           teffup =     ta - gamma*hupinf
       TGL_teffup = TGL_ta - TGL_gamma*hupinf - gamma*TGL_hupinf
          tbclrdn = teffdn*em
      TGL_tbclrdn = TGL_teffdn*em + teffdn*TGL_em
          tbclrup = teffup*em
      TGL_tbclrup = TGL_teffup*em + teffup*TGL_em
!
           tb1dn = em1*teff1dn
       TGL_tb1dn = TGL_em1*teff1dn + em1*TGL_teff1dn
           tb1up = em1*teff1up
       TGL_tb1up = TGL_em1*teff1up + em1*TGL_teff1up
           tb2dn = (tbclrdn - tb1dn)/tau1
       TGL_tb2dn = (TGL_tbclrdn - TGL_tb1dn)/tau1 - tb2dn*TGL_tau1/tau1
           tb2up =      tbclrup - tau2*tb1up
       TGL_tb2up =  TGL_tbclrup - TGL_tau2*tb1up - tau2*TGL_tb1up

! total downwelling and upwelling brightness temperature and transmittance

          tbdn  =     tb1dn + tau1*(tbcld + taucld*tb2dn)
      TGL_tbdn  = TGL_tb1dn + TGL_tau1*(tbcld + taucld*tb2dn) &
                            + tau1*(TGL_tbcld + TGL_taucld*tb2dn + taucld*TGL_tb2dn)
          tbup  =     tb2up + tau2*(tbcld + taucld*tb1up)
      TGL_tbup  = TGL_tb2up + TGL_tau2*(tbcld + taucld*tb1up) &
                            + tau2*(TGL_tbcld + TGL_taucld*tb1up + taucld*TGL_tb1up)
          tauatm = tau*taucld
      TGL_tauatm = TGL_tau*taucld + tau*TGL_taucld
!
! the following lines apply an ad hoc correction to improve fit 
! at large angles and/or high gaseous opacities 
!  (downwelling brightness temperatures only)

      alph = (0.636619*atan(mu*sig))**2
      if (abs(sig) .gt. 0.) then
         TGL_alph = 2.*0.636619*0.636619* &
                    (TGL_mu*sig + mu*TGL_sig)*atan(mu*sig)/(1.+mu*mu*sig*sig)
      else
         TGL_alph = 0.
      endif
      TGL_tbdn = - TGL_alph*tbdn + (1.0-alph)*TGL_tbdn &
                 + TGL_em*alph*ta + em*TGL_alph*ta + em*alph*TGL_ta 
      tbdn = (1.0-alph)*tbdn + em*alph*ta
!
END SUBROUTINE TGL_tbatmos

!=================================================================
SUBROUTINE TGL_effang(ifreq,theta,gx2,sigma,effangv,effangh,     &
                      TGL_gx2,TGL_sigma,TGL_effangv,TGL_effangh  )
! ----------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------
! 
! Input  :: TGL_gx2, TGL_sigma
! Output :: TGL_effangv,TGL_effangh,effangv,effangh
! ----------------------------------------------------------------
!
!
! Calculated the effective zenith angle of reflected microwave radiation
! at SSM/I frequencies for vertical and horizontal polarization
!
      integer, intent(IN   ) :: ifreq
      real   , intent(IN   ) :: theta,gx2,sigma,TGL_gx2, TGL_sigma
      real   , intent(  OUT) :: TGL_effangv,TGL_effangh,effangv,effangh
!
      real c19v,c19h,c22v,c22h,c37v,c37h,c85v,c85h
      real s19v(6),s19h(6),s22v(6),s22h(6), &
           s37v(6),s37h(6),s85v(6),s85h(6)
!
      real :: alnsig,gg,ggg,xd,xx
      real :: z1,z2,z3,z4,z5,z6,alnthv
      real :: y,dth,angh,angv,alnthh
      real :: TGL_alnsig,TGL_gg,TGL_ggg,TGL_xd
      real :: TGL_z1,TGL_z2,TGL_z3,TGL_z4,TGL_z5,TGL_z6,TGL_alnthv
      real :: TGL_y,TGL_dth,TGL_angh,TGL_angv,TGL_xx,TGL_alnthh

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
        TGL_effangv = 0.
            effangh = theta
        TGL_effangh = 0.
        return
      endif
      alnsig = alog(sigma)
      if (abs(sigma) .gt. 0.) then
         TGL_alnsig = TGL_sigma/sigma
      else
         TGL_alnsig = 0.
      endif
          gg  = gx2*gx2
      TGL_gg  = 2.*gx2*TGL_gx2
          ggg = gg*gx2
      TGL_ggg = TGL_gg*gx2 + gg*TGL_gx2

      if (ifreq .eq. 1) then 

             xd =      alnsig - c19v
         TGL_xd =  TGL_alnsig
             xx =  xd*xd
         TGL_xx =  2.*xd*TGL_xd
             z1 =  xx*ggg
         TGL_z1 =  TGL_xx*ggg + xx*TGL_ggg
             z2 =  xd*ggg
         TGL_z2 =  TGL_xd*ggg + xd*TGL_ggg
             z3 =  xd*gg
         TGL_z3 =  TGL_xd*gg + xd*TGL_gg
             z4 =  xx*gg
         TGL_z4 =  TGL_xx*gg + xx*TGL_gg
             z5 =  xx*gx2
         TGL_z5 =  TGL_xx*gx2 + xx*TGL_gx2
             z6 =  xd*gx2
         TGL_z6 =  TGL_xd*gx2 + xd*TGL_gx2
         alnthv =  s19v(1)*z1 + s19v(2)*z2 + s19v(3)*z3 + &
                   s19v(4)*z4 + s19v(5)*z5 + s19v(6)*z6
         TGL_alnthv = s19v(1)*TGL_z1 + s19v(2)*TGL_z2 + s19v(3)*TGL_z3 + &
                      s19v(4)*TGL_z4 + s19v(5)*TGL_z5 + s19v(6)*TGL_z6
         alnthv     =     alnthv + 3.611

             xd =      alnsig - c19h
         TGL_xd =  TGL_alnsig
             xx =  xd*xd
         TGL_xx =  2.*xd*TGL_xd
             z1 =  xx*ggg
         TGL_z1 =  TGL_xx*ggg + xx*TGL_ggg
             z2 =  xd*ggg
         TGL_z2 =  TGL_xd*ggg + xd*TGL_ggg
             z3 =  xd*gg
         TGL_z3 =  TGL_xd*gg + xd*TGL_gg
             z4 =  xx*gg
         TGL_z4 =  TGL_xx*gg + xx*TGL_gg
             z5 =  xx*gx2
         TGL_z5 =  TGL_xx*gx2 + xx*TGL_gx2
             z6 =  xd*gx2
         TGL_z6 =  TGL_xd*gx2 + xd*TGL_gx2
         
         alnthh =  s19h(1)*z1 + s19h(2)*z2 + s19h(3)*z3 + &
                   s19h(4)*z4 + s19h(5)*z5 + s19h(6)*z6
         TGL_alnthh = s19h(1)*TGL_z1 + s19h(2)*TGL_z2 + s19h(3)*TGL_z3 + &
                      s19h(4)*TGL_z4 + s19h(5)*TGL_z5 + s19h(6)*TGL_z6

         alnthh     =     alnthh + 3.611

      else if (ifreq .eq. 2) then 
             xd =      alnsig - c22v
         TGL_xd =  TGL_alnsig
             xx =  xd*xd
         TGL_xx =  2.*xd*TGL_xd
             z1 =  xx*ggg
         TGL_z1 =  TGL_xx*ggg + xx*TGL_ggg 
             z2 =  xd*ggg
         TGL_z2 =  TGL_xd*ggg + xd*TGL_ggg
             z3 =  xd*gg
         TGL_z3 =  TGL_xd*gg + xd*TGL_gg
             z4 =  xx*gg
         TGL_z4 =  TGL_xx*gg + xx*TGL_gg
             z5 =  xx*gx2
         TGL_z5 =  TGL_xx*gx2 + xx*TGL_gx2
             z6 =  xd*gx2
         TGL_z6 =  TGL_xd*gx2 + xd*TGL_gx2
         alnthv =  s22v(1)*z1 + s22v(2)*z2 + s22v(3)*z3 + &
                   s22v(4)*z4 + s22v(5)*z5 + s22v(6)*z6
         TGL_alnthv = s22v(1)*TGL_z1 + s22v(2)*TGL_z2 + s22v(3)*TGL_z3 + &
                      s22v(4)*TGL_z4 + s22v(5)*TGL_z5 + s22v(6)*TGL_z6
         alnthv     =     alnthv + 3.611
!        TGL_alnthv = TGL_alnthv
!
             xd =      alnsig - c22h
         TGL_xd =  TGL_alnsig
             xx =  xd*xd
         TGL_xx =  2.*xd*TGL_xd
             z1 =  xx*ggg
         TGL_z1 =  TGL_xx*ggg + xx*TGL_ggg
             z2 =  xd*ggg
         TGL_z2 =  TGL_xd*ggg + xd*TGL_ggg
             z3 =  xd*gg
         TGL_z3 =  TGL_xd*gg + xd*TGL_gg
             z4 =  xx*gg
         TGL_z4 =  TGL_xx*gg + xx*TGL_gg
             z5 =  xx*gx2
         TGL_z5 =  TGL_xx*gx2 + xx*TGL_gx2
             z6 =  xd*gx2
         TGL_z6 =  TGL_xd*gx2 + xd*TGL_gx2
         alnthh =  s22h(1)*z1 + s22h(2)*z2 + s22h(3)*z3 + &
                   s22h(4)*z4 + s22h(5)*z5 + s22h(6)*z6
         TGL_alnthh = s22h(1)*TGL_z1 + s22h(2)*TGL_z2 + s22h(3)*TGL_z3 + &
                      s22h(4)*TGL_z4 + s22h(5)*TGL_z5 + s22h(6)*TGL_z6
         alnthh     =     alnthh + 3.611
!        TGL_alnthh = TGL_alnthh
      else if (ifreq .eq. 3) then 
             xd =      alnsig - c37v
         TGL_xd =  TGL_alnsig
             xx =  xd*xd
         TGL_xx =  2.*xd*TGL_xd
             z1 =  xx*ggg
         TGL_z1 =  TGL_xx*ggg + xx*TGL_ggg
             z2 =  xd*ggg
         TGL_z2 =  TGL_xd*ggg + xd*TGL_ggg
             z3 =  xd*gg
         TGL_z3 =  TGL_xd*gg  + xd*TGL_gg
             z4 =  xx*gg
         TGL_z4 =  TGL_xx*gg  + xx*TGL_gg
             z5 =  xx*gx2
         TGL_z5 =  TGL_xx*gx2 + xx*TGL_gx2
             z6 =  xd*gx2
         TGL_z6 =  TGL_xd*gx2 + xd*TGL_gx2
         alnthv =  s37v(1)*z1 + s37v(2)*z2 + s37v(3)*z3 + &
                   s37v(4)*z4 + s37v(5)*z5 + s37v(6)*z6
         TGL_alnthv = s37v(1)*TGL_z1 + s37v(2)*TGL_z2 + s37v(3)*TGL_z3 + &
                      s37v(4)*TGL_z4 + s37v(5)*TGL_z5 + s37v(6)*TGL_z6
         alnthv     =     alnthv + 3.611
!        TGL_alnthv = TGL_alnthv 
!
             xd =      alnsig - c37h
         TGL_xd =  TGL_alnsig
             xx =  xd*xd
         TGL_xx =  2.*xd*TGL_xd
             z1 =  xx*ggg
         TGL_z1 =  TGL_xx*ggg +  xx*TGL_ggg
             z2 =  xd*ggg
         TGL_z2 =  TGL_xd*ggg +  xd*TGL_ggg
             z3 =  xd*gg
         TGL_z3 =  TGL_xd*gg  +  xd*TGL_gg
             z4 =  xx*gg
         TGL_z4 =  TGL_xx*gg  +  xx*TGL_gg
             z5 =  xx*gx2
         TGL_z5 =  TGL_xx*gx2 +  xx*TGL_gx2
             z6 =  xd*gx2
         TGL_z6 =  TGL_xd*gx2 +  xd*TGL_gx2
         alnthh =  s37h(1)*z1 + s37h(2)*z2 + s37h(3)*z3 + &
                   s37h(4)*z4 + s37h(5)*z5 + s37h(6)*z6
         TGL_alnthh = s37h(1)*TGL_z1 + s37h(2)*TGL_z2 + s37h(3)*TGL_z3 + &
                      s37h(4)*TGL_z4 + s37h(5)*TGL_z5 + s37h(6)*TGL_z6
         alnthh     =     alnthh + 3.611
!        TGL_alnthh = TGL_alnthh
      else if (ifreq .eq. 4) then 
             xd =      alnsig - c85v
         TGL_xd =  TGL_alnsig
             xx =  xd*xd
         TGL_xx =  2.*xd*TGL_xd 
             z1 =  xx*ggg
         TGL_z1 =  TGL_xx*ggg + xx*TGL_ggg
             z2 =  xd*ggg
         TGL_z2 =  TGL_xd*ggg + xd*TGL_ggg
             z3 =  xd*gg
         TGL_z3 =  TGL_xd*gg  + xd*TGL_gg
             z4 =  xx*gg
         TGL_z4 =  TGL_xx*gg  + xx*TGL_gg
             z5 =  xx*gx2
         TGL_z5 =  TGL_xx*gx2 + xx*TGL_gx2
             z6 =  xd*gx2
         TGL_z6 =  TGL_xd*gx2 + xd*TGL_gx2
         alnthv =  s85v(1)*z1 + s85v(2)*z2 + s85v(3)*z3 + &
                   s85v(4)*z4 + s85v(5)*z5 + s85v(6)*z6
         TGL_alnthv = s85v(1)*TGL_z1 + s85v(2)*TGL_z2 + s85v(3)*TGL_z3 + &
                      s85v(4)*TGL_z4 + s85v(5)*TGL_z5 + s85v(6)*TGL_z6
         alnthv     =     alnthv + 3.611
!        TGL_alnthv = TGL_alnthv 
!
             xd =      alnsig - c85h
         TGL_xd =  TGL_alnsig
             xx =  xd*xd
         TGL_xx =  2.*xd*TGL_xd
             z1 =  xx*ggg
         TGL_z1 =  TGL_xx*ggg + xx*TGL_ggg
             z2 =  xd*ggg
         TGL_z2 =  TGL_xd*ggg + xd*TGL_ggg
             z3 =  xd*gg
         TGL_z3 =  TGL_xd*gg  + xd*TGL_gg
             z4 =  xx*gg
         TGL_z4 =  TGL_xx*gg  + xx*TGL_gg
             z5 =  xx*gx2
         TGL_z5 =  TGL_xx*gx2 + xx*TGL_gx2
             z6 =  xd*gx2
         TGL_z6 =  TGL_xd*gx2 + xd*TGL_gx2
         alnthh =  s85h(1)*z1 + s85h(2)*z2 + s85h(3)*z3 + &
                   s85h(4)*z4 + s85h(5)*z5 + s85h(6)*z6
         TGL_alnthh = s85h(1)*TGL_z1 + s85h(2)*TGL_z2 + s85h(3)*TGL_z3 + &
                      s85h(4)*TGL_z4 + s85h(5)*TGL_z5 + s85h(6)*TGL_z6
         alnthh     =     alnthh + 3.611
!        TGL_alnthh = TGL_alnthh
      endif
          angv =   90.0 - exp(alnthv)
      TGL_angv = - TGL_alnthv*exp(alnthv)
          angh =   90.0 - exp(alnthh)
      TGL_angh = - TGL_alnthh*exp(alnthh)
          y    =   1.0 - 28.0*gx2
      TGL_y    = - 28.0*TGL_gx2
      if (y .lt. 0.0) then
             y = 0.0
         TGL_y = 0.0
      endif
          dth     = (theta - 53.0)*y
      TGL_dth     = (theta - 53.0)*TGL_y
          effangv =     angv +     dth
      TGL_effangv = TGL_angv + TGL_dth
          effangh =     angh +     dth
      TGL_effangh = TGL_angh + TGL_dth

END SUBROUTINE TGL_effang

!=================================================================
SUBROUTINE TGL_roughem(ifreq,gx2,tk,theta,remv,remh,             &
                       TGL_gx2,TGL_tk,TGL_remv,TGL_remh          )
! ----------------------------------------------------------------
IMPLICIT NONE
! ----------------------------------------------------------------
!
! Input  :: TGL_tk, TGL_gx2
! Output :: TGL_remv,TGL_remh, remv,remh
!
! Calculates rough-surface emissivity of ocean surface at SSM/I
! frequencies.
!
      integer, intent(IN   ) :: ifreq
      real   , intent(IN   ) :: tk, theta, gx2, TGL_tk,TGL_gx2
      real   , intent(  OUT) :: TGL_remv,TGL_remh, remv,remh
      real a19v(4),a22v(4),a37v(4),a85v(4)
      real a19h(4),a22h(4),a37h(4),a85h(4)
      real f(4)
      real semv,semh,TGL_semv,TGL_semh,ssw
      real :: tp,g,x1,x2,x3,x4,dtheta
      real :: TGL_tp,TGL_g,TGL_x1,TGL_x2,TGL_x3,TGL_x4
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
          tp =     tk/273.0
      TGL_tp = TGL_tk/273.0
      dtheta = theta-53.0
          g =  0.5*    gx2
      TGL_g =  0.5*TGL_gx2
          x1 =     g
      TGL_x1 = TGL_g
          x2 = tp*g
      TGL_x2 = TGL_tp*g + tp*TGL_g
          x3 = dtheta*    g
      TGL_x3 = dtheta*TGL_g
          x4 = tp*x3
      TGL_x4 = TGL_tp*x3 + tp*TGL_x3
!
      if (ifreq .eq. 1) then
             remv =     x1*a19v(1) +     x2*a19v(2) +     x3*a19v(3) +     x4*a19v(4)
         TGL_remv = TGL_x1*a19v(1) + TGL_x2*a19v(2) + TGL_x3*a19v(3) + TGL_x4*a19v(4)
             remh =     x1*a19h(1) +     x2*a19h(2) +     x3*a19h(3) +     x4*a19h(4)
         TGL_remh = TGL_x1*a19h(1) + TGL_x2*a19h(2) + TGL_x3*a19h(3) + TGL_x4*a19h(4)
      else if (ifreq .eq. 2) then
             remv =     x1*a22v(1) +     x2*a22v(2) +     x3*a22v(3) +     x4*a22v(4)
         TGL_remv = TGL_x1*a22v(1) + TGL_x2*a22v(2) + TGL_x3*a22v(3) + TGL_x4*a22v(4)
             remh =     x1*a22h(1) +     x2*a22h(2) +     x3*a22h(3) +     x4*a22h(4)
         TGL_remh = TGL_x1*a22h(1) + TGL_x2*a22h(2) + TGL_x3*a22h(3) + TGL_x4*a22h(4)
      else if (ifreq .eq. 3) then
             remv =     x1*a37v(1) +     x2*a37v(2) +     x3*a37v(3) +     x4*a37v(4)
         TGL_remv = TGL_x1*a37v(1) + TGL_x2*a37v(2) + TGL_x3*a37v(3) + TGL_x4*a37v(4)
             remh =     x1*a37h(1) +     x2*a37h(2) +     x3*a37h(3) +     x4*a37h(4)
         TGL_remh = TGL_x1*a37h(1) + TGL_x2*a37h(2) + TGL_x3*a37h(3) + TGL_x4*a37h(4)
      else if (ifreq .eq. 4) then
             remv =     x1*a85v(1) +     x2*a85v(2) +     x3*a85v(3) +     x4*a85v(4)
         TGL_remv = TGL_x1*a85v(1) + TGL_x2*a85v(2) + TGL_x3*a85v(3) + TGL_x4*a85v(4)
             remh =     x1*a85h(1) +     x2*a85h(2) +     x3*a85h(3) +     x4*a85h(4)
         TGL_remh = TGL_x1*a85h(1) + TGL_x2*a85h(2) + TGL_x3*a85h(3) + TGL_x4*a85h(4)
      endif

      ssw=36.5
      CALL TGL_spemiss(f(ifreq),tk,theta,ssw,semv,semh,  &
                       TGL_tk,TGL_semv,TGL_semh           )
      TGL_remv = TGL_remv + TGL_semv
          remv =     remv +     semv
      TGL_remh = TGL_remh + TGL_semh
          remh =     remh +     semh
      
END SUBROUTINE TGL_roughem
