
!============================================================================
SUBROUTINE DA_Transform_XToTb_Adj (xa, xb)
!----------------------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------------------

   type (xb_type), intent(in)    :: xb          ! first guess state.
   type (x_type) , intent(inout) :: xa          ! grad_x(jo)

!  LOCAL VARS

   integer                       :: n                ! loop counter.
   integer                       :: i,j,k
   integer                       :: is, js, ie, je, ks, ke

   real                          :: dx, dy, dxm, dym, zhmkz
   real                          :: dum1, dum2, zrhom, ADJ_zrhom

   real                          :: psfc,ta,gamma,sst,htpw,speed,alw,zcld,tpw
   real                          :: ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_tpw
   real                          :: ADJ_htpw,ADJ_speed,ADJ_alw,ADJ_zcld         
!----------------------------------------------------------------------------

   psfc=0.;ta=0.;gamma=0.;sst=0.;htpw=0.;speed=0.;alw=0.;zcld=0.;tpw=0
   dx=0.; dy=0.; dxm=0.; dym=0.; zhmkz=0.
   dum1=0.; dum2=0.; zrhom=0.; ADJ_zrhom=0.

   is = xb%its
   js = xb%jts
   ks = xb%kts

   ie = xb%ite
   je = xb%jte
   ke = xb%kte
   if ( Testing_3DVAR ) then
      is = xb%its-1
      js = xb%jts-1

      ie = xb%ite+1
      je = xb%jte+1

      if ( is < xb%ids ) is = xb%ids
      if ( js < xb%jds ) js = xb%jds

      if ( ie > xb%ide ) ie = xb%ide
      if ( je > xb%jde ) je = xb%jde
   endif
!   print*,'Testing_3DVAR=',Testing_3DVAR,is,js,ie,je

! Mean fields

   do j=js, je
   do i=is, ie

      psfc  = 0.01*xb%psfc(i,j)
!     sst   = xb%tgrn(i,j)
      ta    = xb%tgrn(i,j) + &
              (xb%t(i,j,ks)-xb%tgrn(i,j))*log(2./0.0001)/ &
              log(xb%h(i,j,ks)/0.0001)

      gamma = (ta-270)*0.023 + 5.03  ! effective lapse rate   (km) (4.0 - 6.5)
      zcld  = 1                      ! effective cloud height (km)

      tpw   = xb%tpw(i,j)*10.
!     speed = xb%speed(i,j)

      alw   = 0.

      zrhom = 0.0
      do k=ks,ke
         zrhom=zrhom+(xb%hf(i,j,k+1)-xb%hf(i,j,k))*xb%h(i,j,k)*xb%q(i,j,k)*xb%rho(i,j,k)
      END DO

!     call DA_Transform_XToZRhoQ(xb, i, j, zh, zf, zrhom)

      htpw    = zrhom/tpw/1000.

      dum1=0.
      dum2=0.

      ADJ_gamma    = 0.0
      ADJ_speed    = 0.0
      ADJ_psfc     = 0.0
      ADJ_zcld     = 0.0
      ADJ_htpw     = 0.0
      ADJ_sst      = 0.0
      ADJ_alw      = 0.0
      ADJ_tpw      = 0.0
      ADJ_ta       = 0.0
      ADJ_zrhom    = 0.

!        print *, '0. i, j, xa%tb19v(i,j), xa%tb19h(i,j)=', &
!                     i, j, xa%tb19v(i,j), xa%tb19h(i,j)

!     print *, 'psfc,ta,gamma,xb%tgrn(i,j),tpw,      &
!                 htpw,xb%speed(i,j),alw,zcld,               &
!                 xb%tb19v(i,j),xb%tb19h(i,j),               &
!                 ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
!                 ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
!                 ADJ_zcld,xa%tb19v(i,j),xa%tb19h(i,j) = ', &
!              psfc,ta,gamma,xb%tgrn(i,j),tpw,      &
!                 htpw,xb%speed(i,j),alw,zcld,               &
!                 xb%tb19v(i,j),xb%tb19h(i,j),               &
!                 ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
!                 ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
!                 ADJ_zcld,xa%tb19v(i,j),xa%tb19h(i,j)

      call ADJ_tb(1,53.,psfc,ta,gamma,xb%tgrn(i,j),tpw,      &
                  htpw,xb%speed(i,j),alw,zcld,               &
                  xb%tb19v(i,j),xb%tb19h(i,j),               &
                  ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
                  ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
                  ADJ_zcld,xa%tb19v(i,j),xa%tb19h(i,j)     )

!     print *, 'psfc,ta,gamma,xb%tgrn(i,j),tpw,      &
!                 htpw,xb%speed(i,j),alw,zcld,               &
!                 xb%tb19v(i,j),xb%tb19h(i,j),               &
!                 ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
!                 ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
!                 ADJ_zcld,xa%tb19v(i,j),xa%tb19h(i,j) = ', &
!              psfc,ta,gamma,xb%tgrn(i,j),tpw,      &
!                 htpw,xb%speed(i,j),alw,zcld,               &
!                 xb%tb19v(i,j),xb%tb19h(i,j),               &
!                 ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
!                 ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
!                 ADJ_zcld,xa%tb19v(i,j),xa%tb19h(i,j)

!     if((ADJ_tpw /= 0.0) .or. (ADJ_htpw /= 0.0)) then
!        print *, '1. i,j,ADJ_tpw,ADJ_htpw=', i,j,ADJ_tpw,ADJ_htpw
!     endif

      call ADJ_tb(2,53.,psfc,ta,gamma,xb%tgrn(i,j),tpw,      &
                  htpw,xb%speed(i,j),alw,zcld,               &
                  xb%tb22v(i,j),dum1,                        &
                  ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
                  ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
                  ADJ_zcld,xa%tb22v(i,j),dum2               )

!     if((ADJ_tpw /= 0.0) .or. (ADJ_htpw /= 0.0)) then
!        print *, '2. i,j,ADJ_tpw,ADJ_htpw=', i,j,ADJ_tpw,ADJ_htpw
!     endif

      call ADJ_tb(3,53.,psfc,ta,gamma,xb%tgrn(i,j),tpw,      &
                  htpw,xb%speed(i,j),alw,zcld,               &
                  xb%tb37v(i,j),xb%tb37h(i,j),               &
                  ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
                  ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
                  ADJ_zcld,xa%tb37v(i,j),xa%tb37h(i,j)     )

!     if((ADJ_tpw /= 0.0) .or. (ADJ_htpw /= 0.0)) then
!        print *, '3. i,j,ADJ_tpw,ADJ_htpw=', i,j,ADJ_tpw,ADJ_htpw
!     endif

      call ADJ_tb(4,53.,psfc,ta,gamma,xb%tgrn(i,j),tpw,      &
                  htpw,xb%speed(i,j),alw,zcld,               &
                  xb%tb85v(i,j),xb%tb85h(i,j),               &
                  ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
                  ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
                  ADJ_zcld,xa%tb85v(i,j),xa%tb85h(i,j)     )

!     if((ADJ_tpw /= 0.0) .or. (ADJ_htpw /= 0.0)) then
!        print *, '4. i,j,ADJ_tpw,ADJ_htpw=', i,j,ADJ_tpw,ADJ_htpw
!     endif

      ADJ_zrhom    = ADJ_htpw/tpw/1000.
      ADJ_tpw      = ADJ_tpw - ADJ_htpw/tpw*htpw

      DO k = ks,ke
         xa%rho(i,j,k) = (xb%hf(i,j,k+1)-xb%hf(i,j,k))*xb%h(i,j,k)*xb%q(i,j,k)*ADJ_zrhom + xa%rho(i,j,k)
         xa%q(i,j,k)   = (xb%hf(i,j,k+1)-xb%hf(i,j,k))*xb%h(i,j,k)*ADJ_zrhom*xb%rho(i,j,k) + xa%q(i,j,k)
      END DO

!     call DA_Transform_XToZRhoQ_Adj(xb,xa,i,j,zh,zf,ADJ_zrhom)

      ADJ_alw = 0.

      xa%speed(i,j)=xa%speed(i,j) + ADJ_speed

      xa%tpw(i,j) = xa%tpw(i,j) + ADJ_tpw*10.

      ADJ_zcld= 0
      ADJ_ta  = ADJ_ta + ADJ_gamma*0.023

      xa%t(i,j,ks) = xa%t(i,j,ks) + ADJ_ta* &
                                    log(2./0.0001)/log(xb%h(i,j,ks)/0.0001)
      ADJ_sst = 0.

      xa%psfc(i,j) = xa%psfc(i,j) + ADJ_psfc*0.01 
   enddo
   enddo

END SUBROUTINE DA_Transform_XToTb_Adj

!============================================================================
SUBROUTINE ADJ_tb(ifreq,theta,p0,ta,gamma,sst,wv,hwv,u,alw,zcld,            &
                  tbv,tbh,                                                  &
                  ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,                   &
                  ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld,ADJ_tbv,ADJ_tbh            )
! ---------------------------------------------------------------------------
  IMPLICIT NONE
! ---------------------------------------------------------------------------
!
! Output : ADJ_p0,  ADJ_ta,   ADJ_gamma, ADJ_sst, ADJ_wv, ADJ_hwv, ADJ_u
!          ADJ_alw, ADJ_zcld
! Input  : ADJ_tbv, ADJ_tbh,  tbv,  tbh
! ---------------------------------------------------------------------------
      integer, intent(IN   ) :: ifreq
      real   , intent(IN   ) :: theta,p0,ta,gamma,sst,wv,hwv,u,alw,zcld
      real   , intent(INOUT) :: ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,     &
                                ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld
      real   , intent(IN   ) :: ADJ_tbv,ADJ_tbh 
      real   , intent(IN   ) :: tbv,tbh

      real :: freq(4),ebiasv(4),ebiash(4),cf1(4),cf2(4),cg(4)

      real :: f,costhet,gx2,tbup,tbdn,tauatm,sigma,remv,remh
      real :: effangv,effangh,tbdnv,foam,foam_save,emissv,emissh 
      real :: dum
      real :: refv,refh,semv,semh,scatv,scath,tbdnh
      real :: ADJ_gx2,ADJ_tbup,ADJ_tbdn,ADJ_tauatm,ADJ_sigma
      real :: ADJ_tremv,ADJ_remh,ADJ_effangv,ADJ_effangh
      real :: ADJ_tbdnh,ADJ_dum,ADJ_foam,ADJ_emissv
      real :: ADJ_emissh,ADJ_refv,ADJ_refh,ADJ_semv
      real :: ADJ_semh,ADJ_scatv,ADJ_scath,ADJ_remv,ADJ_tbdnv
      real :: ADJ_theta

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

      f=0.;costhet=0.;gx2=0.;tbup=0.;tbdn=0.;tauatm=0.
      sigma=0.;remv=0.;remh=0.;effangv=0.;effangh=0.
      tbdnv=0.;foam=0.;foam_save=0.;emissv=0.;emissh=0.
      dum=0.;refv=0.;refh=0.;semv=0.;semh=0.;scatv=0.
      scath=0.;tbdnh=0.;ADJ_gx2=0.;ADJ_tbup=0.;ADJ_tbdn=0.
      ADJ_tauatm=0.;ADJ_sigma=0.;ADJ_tremv=0.;ADJ_remh=0.
      ADJ_effangv=0.;ADJ_effangh=0.;ADJ_tbdnh=0.;ADJ_dum=0.
      ADJ_foam=0.;ADJ_emissv=0.;ADJ_emissh=0.;ADJ_refv=0.
      ADJ_refh=0.;ADJ_semv=0.;ADJ_semh=0.;ADJ_scatv=0.
      ADJ_scath=0.;ADJ_remv=0.;ADJ_tbdnv=0.
      ADJ_theta =0.
!!!!

!    print*,'ifreq',ifreq,theta,p0,ta,gamma,sst,wv,hwv,u,alw,zcld,          &
!                 tbv,tbh,                                                  &
!                 ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,                   &
!                 ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld,ADJ_tbv,ADJ_tbh            

      f = freq(ifreq)
      costhet = cos(theta*0.017453)

! effective surface slope variance

      gx2 = cg(ifreq)*    u

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

          foam = cf1(ifreq)*    u

      if (u .gt. 5.0) then
            foam_save = foam
            foam =     foam + cf2(ifreq)*(   u-5.0)
      endif

! compute surface emissivities and reflectivity

          emissv =     foam*fem + (1.0 - foam)*(remv + ebiasv(ifreq))
          emissh =     foam*fem + (1.0 - foam)*(remh + ebiash(ifreq))
          refv =   1.0 - emissv
          refh =   1.0 - emissh

! compute surface emission term

          semv = sst*emissv
          semh = sst*emissh

! compute surface scattering term

          scatv = refv*tbdnv
          scath = refh*tbdnh

! combine to get space-observed brightness temperature

!         tbv =     tbup + tauatm*(semv + scatv)
!         tbh =     tbup + tauatm*(semh + scath)


!!! start
!    print*,'ifreq 1',ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,  &
!                 ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld,ADJ_tbv,ADJ_tbh            

!
      ADJ_tbup   = ADJ_tbh                    !!! first
      ADJ_tauatm = ADJ_tbh*(semh + scath)     !!! first
      ADJ_semh   = tauatm*ADJ_tbh             !!! first
      ADJ_scath  = tauatm*ADJ_tbh             !!! first

      ADJ_tbup   = ADJ_tbv                  + ADJ_tbup
      ADJ_tauatm = ADJ_tbv*(semv + scatv)   + ADJ_tauatm
      ADJ_semv   = tauatm*ADJ_tbv             !!! first
      ADJ_scatv  = tauatm*ADJ_tbv             !!! first

      ADJ_refh   = ADJ_scath*tbdnh            !!! first
      ADJ_tbdnh  = refh*ADJ_scath             !!! first
      ADJ_refv   = ADJ_scatv*tbdnv            !!! first
      ADJ_tbdnv  = refv*ADJ_scatv             !!! first
      ADJ_sst    = ADJ_semh*emissh          + ADJ_sst
      ADJ_emissh = sst*ADJ_semh               !!! first
      ADJ_sst    = ADJ_semv*emissv          + ADJ_sst
      ADJ_emissv = sst*ADJ_semv               !!! first

      ADJ_emissh = - ADJ_refh               + ADJ_emissh
      ADJ_emissv = - ADJ_refv               + ADJ_emissv

      ADJ_foam   =   ADJ_emissh*fem                      !!! first
      ADJ_foam   = - ADJ_emissh*(remh + ebiash(ifreq)) + ADJ_foam
      ADJ_remh   =   (1.0 - foam)*ADJ_emissh             !!! first
      ADJ_foam   =   ADJ_emissv*fem                    + ADJ_foam
      ADJ_foam   = - ADJ_emissv*(remv + ebiasv(ifreq)) + ADJ_foam
      ADJ_remv   =   (1.0 - foam)*ADJ_emissv             !!! first

      if (u .gt. 5.0) then
        ADJ_u = cf2(ifreq)*ADJ_foam  + ADJ_u
        foam=foam_save
      endif
      ADJ_u = cf1(ifreq)*ADJ_foam    + ADJ_u
      
      ADJ_dum = 0.
      dum     = 0.
      ADJ_effangh = 0.
      call ADJ_tbatmos(ifreq,effangh,p0,wv,hwv,ta,gamma,alw,    &
                       zcld,dum,tbdnh,dum,                      &
                       ADJ_effangh,ADJ_p0,ADJ_wv,ADJ_hwv,       &
                       ADJ_ta,ADJ_gamma,ADJ_alw,ADJ_zcld,       &
                       ADJ_dum,ADJ_tbdnh,ADJ_dum                )
      dum     = 0.
      ADJ_dum = 0.

!    print*,'ifreq 2',ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,  &
!                 ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld,ADJ_tbv,ADJ_tbh            

      ADJ_effangv = 0.
      call ADJ_tbatmos(ifreq,effangv,p0,wv,hwv,ta,gamma,alw,    &
                       zcld,dum,tbdnv,dum,                      &
                       ADJ_effangv,ADJ_p0,ADJ_wv,ADJ_hwv,       &
                       ADJ_ta,ADJ_gamma,ADJ_alw,ADJ_zcld,       & 
                       ADJ_dum,ADJ_tbdnv,ADJ_dum                )

      ADJ_gx2=0.
      ADJ_sigma=0.
!    print*,'ifreq 3',ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,  &
!                 ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld,ADJ_tbv,ADJ_tbh            

      call ADJ_effang(ifreq,theta,gx2,sigma,effangv,effangh,    &
                      ADJ_gx2,ADJ_sigma,ADJ_effangv,ADJ_effangh )

      call ADJ_roughem(ifreq,gx2,sst,theta,remv,remh,         &
                       ADJ_gx2,ADJ_sst,ADJ_remv,ADJ_remh      )

      ADJ_tauatm = - costhet*ADJ_sigma/tauatm + ADJ_tauatm


!     print*,'ifreq 4',ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,  &
!                 ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld,ADJ_tbv,ADJ_tbh            

      call ADJ_tbatmos(ifreq,theta,p0,wv,hwv,ta,gamma,alw,zcld, &
                       tbup,tbdn,tauatm,                        &
                       ADJ_theta,ADJ_p0,ADJ_wv,ADJ_hwv,ADJ_ta,ADJ_gamma,  &
                       ADJ_alw,ADJ_zcld,ADJ_tbup,ADJ_tbdn,      &
                       ADJ_tauatm                               )

      ADJ_theta=0.   !!!first

      ADJ_u = cg(ifreq)*ADJ_gx2 + ADJ_u

!    print*,'ifreq 5',ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,  &
!                 ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld,ADJ_tbv,ADJ_tbh            
!!! end

 end SUBROUTINE ADJ_tb

!============================================================================
SUBROUTINE ADJ_cal_sigma_v(ifreq,p0,wv,hwv,ta,gamma,sigv,                   &
                           ADJ_p0,ADJ_wv,ADJ_hwv,ADJ_ta,ADJ_gamma,ADJ_sigma_v)
! ---------------------------------------------------------------------------
IMPLICIT NONE
! ---------------------------------------------------------------------------
!
!OUTPUT : ADJ_p0, ADJ_wv, ADJ_hwv, ADJ_ta, ADJ_gamma
!INPUT  : ADJ_sigma_v
! ---------------------------------------------------------------------------


      integer ifreq
      real, intent(IN   ):: p0,wv,hwv,ta,gamma  ! base field
      real, intent(INOUT):: ADJ_p0,ADJ_wv,ADJ_hwv,ADJ_ta,ADJ_gamma
      real, intent(IN   ):: ADJ_sigma_v
!
      real wvc, wvcor(4)
      real ADJ_wvc
!
      real voh1,otbar1,pbar1
      real term21,term31,term41,term51,term61
      real a11,a21,a31,a41,a51,a61
      real ADJ_voh1,ADJ_otbar1,ADJ_pbar1
      real ADJ_term21,ADJ_term31,ADJ_term41,ADJ_term51,ADJ_term61
!
      real voh2,otbar2,pbar2
      real term22,term32,term42,term52,term62
      real a12,a22,a32,a42,a52,a62
      real ADJ_voh2,ADJ_otbar2,ADJ_pbar2
      real ADJ_term22,ADJ_term32,ADJ_term42,ADJ_term52,ADJ_term62
!
      real voh3,otbar3,pbar3
      real term23,term33,term43,term53,term63
      real a13,a23,a33,a43,a53,a63
      real ADJ_voh3,ADJ_otbar3,ADJ_pbar3
      real ADJ_term23,ADJ_term33,ADJ_term43,ADJ_term53,ADJ_term63
!
      real voh4,otbar4,pbar4
      real term24,term34,term44,term54,term64
      real a14,a24,a34,a44,a54,a64
      real ADJ_voh4,ADJ_otbar4,ADJ_pbar4
      real ADJ_term24,ADJ_term34,ADJ_term44,ADJ_term54,ADJ_term64
!
      real const1,const2,const3,const4
      real h1,h2,h3,h4

      real sigv, ADJ_sigv
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
      wvc=0.;ADJ_wvc=0.;voh1=0.;otbar1=0.;pbar1=0.
      term21=0.;term31=0.;term41=0.;term51=0.;term61=0.
      ADJ_voh1=0.;ADJ_otbar1=0.;ADJ_pbar1=0.;ADJ_term21=0.
      ADJ_term31=0.;ADJ_term41=0.;ADJ_term51=0.;ADJ_term61=0.
!
      voh2=0.;otbar2=0.;pbar2=0.
      term22=0.;term32=0.;term42=0.;term52=0.;term62=0.
      ADJ_voh2=0.;ADJ_otbar2=0.;ADJ_pbar2=0.
      ADJ_term22=0.;ADJ_term32=0.;ADJ_term42=0.
      ADJ_term52=0.;ADJ_term62=0.
!
      voh3=0.;otbar3=0.;pbar3=0.
      term23=0.;term33=0.;term43=0.;term53=0.;term63=0.
      ADJ_voh3=0.;ADJ_otbar3=0.;ADJ_pbar3=0.
      ADJ_term23=0.;ADJ_term33=0.;ADJ_term43=0.
      ADJ_term53=0.;ADJ_term63=0.
!
      voh4=0.;otbar4=0.;pbar4=0.
      term24=0.;term34=0.;term44=0.;term54=0.;term64=0.
      ADJ_voh4=0.;ADJ_otbar4=0.;ADJ_pbar4=0.
      ADJ_term24=0.;ADJ_term34=0.;ADJ_term44=0.
      ADJ_term54=0.;ADJ_term64=0.
!
      sigv=0.;ADJ_sigv=0.
!--------------------------------------
!
      wvc =     wv*wvcor(ifreq)

      if (ifreq.eq.1) then

             pbar1 = p0/(1.0 + hwv/h1)
              voh1 = wv/hwv
            term21 = a21*voh1
            otbar1 =  1.0/(ta - const1*gamma*hwv)
            term31 = a31*otbar1
            term61 = a61*otbar1*otbar1
            term41 = a41*pbar1*otbar1
            term51 = a51*voh1*otbar1
              sigv = a11 + term21 + term31 + term41 + term51 + term61

      else if (ifreq.eq.2) then

            pbar2  = p0/(1.0 + hwv/h2)
            term22 = a22*pbar2
            term52 = a52*pbar2*pbar2
              voh2 = wv/hwv
            term32 = a32*voh2
            otbar2 = 1.0/(ta - const2*gamma*hwv)
            term42 = a42*otbar2
            term62 = a62*otbar2*otbar2
              sigv = a12 + term22 + term32 + term42 + term52 + term62

      else if (ifreq.eq.3) then

             pbar3 = p0/(1.0 + hwv/h3)
            term43 = a43*pbar3*pbar3
              voh3 = wv/hwv
            term23 = a23*voh3
            otbar3 = 1.0/(ta - const3*gamma*hwv)
            term33 = a33*otbar3
            term53 = a53*pbar3*voh3
            term63 = a63*otbar3*voh3
              sigv = a13 + term23 + term33 + term43 + term53 + term63

      else if (ifreq.eq.4) then

             pbar4 = p0/(1.0 + hwv/h4)
            term44 = a44*pbar4*pbar4
              voh4 = wv/hwv
            term24 = a24*voh4
            otbar4 = 1.0/(ta - const4*gamma*hwv)
            term34 = a34*otbar4
            term54 = a54*pbar4*voh4
            term64 = a64*otbar4*voh4
              sigv = a14 + term24 + term34 + term44 + term54 + term64

      else
              sigv = 0.0

      endif

!
      ADJ_sigv    = ADJ_sigma_v*wvc
      ADJ_wvc     = sigv*ADJ_sigma_v

      if (ifreq.eq.1) then

          ADJ_term21 = ADJ_sigv 
          ADJ_term31 = ADJ_sigv
          ADJ_term41 = ADJ_sigv
          ADJ_term51 = ADJ_sigv
          ADJ_term61 = ADJ_sigv

          ADJ_voh1   = a51*ADJ_term51*otbar1
          ADJ_otbar1 = a51*voh1*ADJ_term51

        ADJ_pbar1  = a41*ADJ_term41*otbar1
        ADJ_otbar1 = a41*pbar1*ADJ_term41 + ADJ_otbar1
        ADJ_otbar1 = 2.*a61*otbar1*ADJ_term61 + ADJ_otbar1

        ADJ_otbar1 = a31*ADJ_term31 + ADJ_otbar1

           ADJ_ta  = -otbar1*otbar1*ADJ_otbar1  + ADJ_ta
          ADJ_hwv  = otbar1*otbar1*const1*gamma*ADJ_otbar1  + ADJ_hwv
         ADJ_gamma = otbar1*otbar1*const1*ADJ_otbar1*hwv  + ADJ_gamma      

          ADJ_voh1 = a21*ADJ_term21 + ADJ_voh1

          ADJ_wv   = ADJ_voh1/hwv  + ADJ_wv
          ADJ_hwv  = -voh1*ADJ_voh1/hwv + ADJ_hwv

         ADJ_p0    = ADJ_pbar1/(1.0 + hwv/h1)  + ADJ_p0
         ADJ_hwv   = -pbar1*ADJ_pbar1/(h1*(1.0 + hwv/h1)) + ADJ_hwv

      else if (ifreq.eq.2) then

          ADJ_term22 = ADJ_sigv 
          ADJ_term32 = ADJ_sigv
          ADJ_term42 = ADJ_sigv
          ADJ_term52 = ADJ_sigv
          ADJ_term62 = ADJ_sigv

        ADJ_otbar2 = 2.*a62*otbar2*ADJ_term62

        ADJ_otbar2 = a42*ADJ_term42 + ADJ_otbar2

        ADJ_ta     = -otbar2*otbar2*ADJ_otbar2  + ADJ_ta
        ADJ_hwv    =  otbar2*otbar2*const2*gamma*ADJ_otbar2 + ADJ_hwv
        ADJ_gamma  =  otbar2*otbar2*const2*ADJ_otbar2*hwv + ADJ_gamma

        ADJ_voh2   = a32*ADJ_term32

          ADJ_wv   = ADJ_voh2/hwv + ADJ_wv
          ADJ_hwv  = -voh2*ADJ_voh2/hwv + ADJ_hwv

        ADJ_pbar2  = 2.*a52*pbar2*ADJ_term52

        ADJ_pbar2  = a22*ADJ_term22 + ADJ_pbar2

        ADJ_p0     = ADJ_pbar2/(1.0 + hwv/h2) + ADJ_p0
        ADJ_hwv    = -pbar2*ADJ_pbar2/h2/(1.0 + hwv/h2) + ADJ_hwv

      else if (ifreq.eq.3) then

          ADJ_term23 = ADJ_sigv
          ADJ_term33 = ADJ_sigv
          ADJ_term43 = ADJ_sigv
          ADJ_term53 = ADJ_sigv
          ADJ_term63 = ADJ_sigv

        ADJ_otbar3 = a63*ADJ_term63*voh3
        ADJ_voh3   = a63*otbar3*ADJ_term63

        ADJ_pbar3  = a53*ADJ_term53*voh3
        ADJ_voh3   = a53*pbar3*ADJ_term53 + ADJ_voh3

        ADJ_otbar3 = a33*ADJ_term33 + ADJ_otbar3

        ADJ_ta     = -otbar3*otbar3*ADJ_otbar3 + ADJ_ta
        ADJ_hwv    =  otbar3*otbar3*const3*gamma*ADJ_otbar3 + ADJ_hwv
        ADJ_gamma  =  otbar3*otbar3*const3*ADJ_otbar3*hwv + ADJ_gamma

        ADJ_voh3   = a23*ADJ_term23 + ADJ_voh3

          ADJ_wv   = ADJ_voh3/hwv  + ADJ_wv
          ADJ_hwv  =-voh3*ADJ_voh3/hwv + ADJ_hwv

         ADJ_pbar3 = 2.*a43*pbar3*ADJ_term43 + ADJ_pbar3

         ADJ_p0    = ADJ_pbar3/(1.0 + hwv/h3) + ADJ_p0
         ADJ_hwv   = -pbar3*ADJ_pbar3/h3/(1.0 + hwv/h3) + ADJ_hwv

      else if (ifreq.eq.4) then

          ADJ_term24 = ADJ_sigv
          ADJ_term34 = ADJ_sigv
          ADJ_term44 = ADJ_sigv
          ADJ_term54 = ADJ_sigv
          ADJ_term64 = ADJ_sigv

          ADJ_otbar4 = a64*ADJ_term64*voh4
          ADJ_voh4   = a64*otbar4*ADJ_term64 

          ADJ_pbar4  = a54*ADJ_term54*voh4
          ADJ_voh4   = a54*pbar4*ADJ_term54 + ADJ_voh4

          ADJ_otbar4 = a34*ADJ_term34 + ADJ_otbar4

            ADJ_ta = -otbar4*otbar4*ADJ_otbar4  + ADJ_ta
           ADJ_hwv =  otbar4*otbar4*const4*gamma*ADJ_otbar4 + ADJ_hwv
         ADJ_gamma =  otbar4*otbar4*const4*ADJ_otbar4*hwv + ADJ_gamma

          ADJ_voh4 = a24*ADJ_term24 + ADJ_voh4

          ADJ_wv   = ADJ_voh4/hwv + ADJ_wv
          ADJ_hwv  = -voh4*ADJ_voh4/hwv + ADJ_hwv

        ADJ_pbar4  = 2.*a44*pbar4*ADJ_term44 + ADJ_pbar4

         ADJ_p0    = ADJ_pbar4/(1.0 + hwv/h4) + ADJ_p0
         ADJ_hwv   = -pbar4*ADJ_pbar4/h4/(1.0 + hwv/h4) + ADJ_hwv
!
      endif

      ADJ_wv  = ADJ_wvc*wvcor(ifreq) + ADJ_wv

END SUBROUTINE ADJ_cal_sigma_v

!====================================================================
SUBROUTINE ADJ_effht(ho,hv,sigo,sigv,mu,zcld,hdn,hup,hdninf,hupinf, &
                     ADJ_ho,ADJ_hv,ADJ_sigo,ADJ_sigv,ADJ_mu,        &
                     ADJ_zcld,ADJ_hdn,ADJ_hup,ADJ_hdninf,ADJ_hupinf )
!--------------------------------------------------------------------
IMPLICIT NONE
!--------------------------------------------------------------------
!
!Output  : ADJ_ho, ADJ_hv, ADJ_sigo, ADJ_sigv, ADJ_zcld, ADJ_mu
!Input   : ADJ_hdn, ADJ_hup, ADJ_hdninf, ADJ_hupinf
!--------------------------------------------------------------------

      real,   intent(IN   ) :: ho,hv,sigo,sigv,mu,zcld
      real,   intent(INOUT) :: ADJ_ho,ADJ_hv,ADJ_sigo,ADJ_sigv,ADJ_zcld, &
                               ADJ_mu
      real,   intent(INOUT) :: hdn,hup,hdninf,hupinf
      real,   intent(   IN) :: ADJ_hdn,ADJ_hup,ADJ_hdninf,ADJ_hupinf

      real :: gint,zgint,hint,zhint
      real :: ginf,zginf,hinf,zhinf
      real :: ADJ_gint,ADJ_zgint,ADJ_hint,ADJ_zhint
      real :: ADJ_ginf,ADJ_zginf,ADJ_hinf,ADJ_zhinf
      real :: ADJ_mu2,ADJ_halfmu,ADJ_sixthmu2,ADJ_etnthmu2
      real :: ADJ_quartmu,ADJ_halfmu2
!
      real :: hoinv,hvinv,chio,chiv,ezho,ezhv,alpha,alph2,alph3
      real :: chio_save,chiv_save,dplus_save,dmin_save
      real :: beta,beta2,beta3,mu2,mualph,cplus,cmin,dplus,dmin
      real :: chiov,chivv,chioo,chioov,chiovv,chiooo,chivvv
      real :: h11,h21,h12,newh11
      real :: sigoo,sigov,sigvv,sigooo,sigoov,sigovv,sigvvv
      real :: ezhoo,ezhov,ezhvv,ezhooo,ezhoov,ezhovv,ezhvvv
      real :: s,sprim,t,tprim,u,uprim,term1,term2,term3
      real :: halfmu,halfmu2,sixthmu2,etnthmu2,quartmu

      real :: ADJ_hoinv,ADJ_hvinv,ADJ_chio,ADJ_chiv,ADJ_ezho
      real :: ADJ_ezhv,ADJ_alpha,ADJ_alph2,ADJ_alph3
      real :: ADJ_beta,ADJ_beta2,ADJ_beta3,ADJ_mualph
      real :: ADJ_cplus,ADJ_cmin,ADJ_dplus,ADJ_dmin
      real :: ADJ_chiov,ADJ_chivv,ADJ_chioo,ADJ_chioov
      real :: ADJ_chiovv,ADJ_chiooo,ADJ_chivvv
      real :: ADJ_h11,ADJ_h21,ADJ_h12,ADJ_newh11
      real :: ADJ_sigoo,ADJ_sigov,ADJ_sigvv,ADJ_sigooo
      real :: ADJ_sigoov,ADJ_sigovv,ADJ_sigvvv
      real :: ADJ_ezhoo,ADJ_ezhov,ADJ_ezhvv,ADJ_ezhooo
      real :: ADJ_ezhoov,ADJ_ezhovv,ADJ_ezhvvv
      real :: ADJ_s,ADJ_sprim,ADJ_t,ADJ_tprim
      real :: ADJ_u,ADJ_uprim,ADJ_term1,ADJ_term2,ADJ_term3

! initial zero
      gint=0.;zgint=0.;hint=0.;zhint=0.
      ginf=0.;zginf=0.;hinf=0.;zhinf=0.
      hoinv=0.;hvinv=0.;chio=0.;chiv=0.
      ezho=0.;ezhv=0.;alpha=0.;alph2=0.;alph3=0.
      chio_save=0.;chiv_save=0.;dplus_save=0.;dmin_save=0.
      beta=0.;beta2=0.;beta3=0.;mu2=0.;mualph=0.;cplus=0.
      cmin=0.;dplus=0.;dmin=0.
      chiov=0.;chivv=0.;chioo=0.;chioov=0.;chiovv=0.;chiooo=0.
      chivvv=0.;h11=0.;h21=0.;h12=0.;newh11=0.
      sigoo=0.;sigov=0.;sigvv=0.;sigooo=0.;sigoov=0.;sigovv=0.;sigvvv=0.
      ezhoo=0.;ezhov=0.;ezhvv=0.;ezhooo=0.;ezhoov=0.;ezhovv=0.;ezhvvv=0.
      s=0.;sprim=0.;t=0.;tprim=0.;u=0.;uprim=0.;term1=0.;term2=0.;term3=0.
      halfmu=0.;halfmu2=0.;sixthmu2=0.;etnthmu2=0.;quartmu=0.

!     ADJ_ho=0.;   ADJ_hv=0.;     ADJ_sigo=0.
!     ADJ_sigv=0.; ADJ_zcld=0.;   ADJ_mu=0.

      ADJ_gint=0.; ADJ_zgint=0.;  ADJ_hint=0.;     ADJ_zhint=0.
      ADJ_ginf=0.; ADJ_zginf=0.;  ADJ_hinf=0.;     ADJ_zhinf=0.
      ADJ_mu2=0.;  ADJ_halfmu=0.; ADJ_sixthmu2=0.; ADJ_etnthmu2=0.
      ADJ_quartmu=0.; ADJ_halfmu2=0.
      ADJ_hoinv=0.; ADJ_hvinv=0.;  ADJ_chio=0.;  ADJ_chiv=0.; ADJ_ezho=0.
      ADJ_ezhv=0.;  ADJ_alpha=0.;  ADJ_alph2=0.; ADJ_alph3=0.
      ADJ_beta=0.;  ADJ_beta2=0.;  ADJ_beta3=0.; ADJ_mualph=0.
      ADJ_cplus=0.; ADJ_cmin=0.;   ADJ_dplus=0.; ADJ_dmin=0.
      ADJ_chiov=0.; ADJ_chivv=0.;  ADJ_chioo=0.; ADJ_chioov=0.
      ADJ_chiovv=0.;ADJ_chiooo=0.; ADJ_chivvv=0.
      ADJ_h11=0.;   ADJ_h21=0.;    ADJ_h12=0.;   ADJ_newh11=0.
      ADJ_sigoo=0.; ADJ_sigov=0.;  ADJ_sigvv=0.; ADJ_sigooo=0.
      ADJ_sigoov=0.;ADJ_sigovv=0.; ADJ_sigvvv=0.
      ADJ_ezhoo=0.; ADJ_ezhov=0.;  ADJ_ezhvv=0.; ADJ_ezhooo=0.
      ADJ_ezhoov=0.;ADJ_ezhovv=0.; ADJ_ezhvvv=0.
      ADJ_s=0.;     ADJ_sprim=0.;  ADJ_t=0.;     ADJ_tprim=0.
      ADJ_u=0.;     ADJ_uprim=0.;  ADJ_term1=0.; ADJ_term2=0.; ADJ_term3=0.
!
! base fields

          hoinv =  1.0d0/ho
          hvinv =  1.0d0/hv
           chio = zcld*hoinv
           chiv = zcld*hvinv
           ezho = sigo*exp(-chio)
           ezhv = sigv*exp(-chiv)
          alpha = sigo + sigv
          alph2 = alpha*alpha
          alph3 = alpha*alph2
           beta = ezho + ezhv
          beta2 = beta*beta
          beta3 = beta*beta2
!
          mu2        = mu*mu
          halfmu     = 0.5d0*    mu
          sixthmu2   =     mu2/6.0d0
          etnthmu2   =     mu2/18.0d0
          quartmu    = 0.25d0*    mu
          halfmu2    = 0.5d0*    mu2

          mualph = mu*alpha
          cplus  = 1.0d0 +     mualph
          cmin   = 1.0d0 -     mualph
          dplus  = halfmu2*alph2
          dmin   =     dplus
!    
          dplus_save = dplus
          dplus  =     cplus +     dplus
!    
          dmin_save  = dmin
          dmin   =     cmin  +     dmin
!
          h11    =     hoinv +     hvinv
          h21    =  1.0d0/(h11 + hvinv)
          h12    =  1.0d0/(h11 + hoinv)
          newh11 =  1.0d0/h11
!       
          chiov  = 1.0d0 +     chio +     chiv
          chioo  = 1.0d0 +     chio +     chio
          chivv  = 1.0d0 +     chiv +     chiv
          chioov =     chioo +     chiv
          chiovv =     chio  +     chivv
          chiooo =     chioo +     chio
          chivvv =     chivv +     chiv
          chio_save = chio
          chio   = 1.0d0 +     chio
          chiv_save = chiv
          chiv   = 1.0d0 +     chiv
          sigov  = sigo*sigv
          sigoo  = sigo*sigo
          sigvv  = sigv*sigv
          sigooo = sigoo*sigo
          sigoov = sigoo*sigv
          sigovv = sigo*sigvv
          sigvvv = sigvv*sigv
          ezhoo  = ezho*ezho
          ezhov  = ezho*ezhv
          ezhvv  = ezhv*ezhv
          ezhovv = ezho*ezhvv
          ezhoov = ezhoo*ezhv
          ezhooo = ezhoo*ezho
          ezhvvv = ezhvv*ezhv
          s      = sigo*ho + sigv*hv

          sprim  = ezho*ho*chio + ezhv*hv*chiv
          t      = sigoo*ho + 4.0d0*sigov*newh11 + sigvv*hv

          tprim  = ezhoo*ho*chioo + 4.0d0*ezhov*newh11*chiov + ezhvv*hv*chivv
          u      = sigooo*ho + 9.0d0*(sigovv*h21+sigoov*h12) + sigvvv*hv

          uprim  = ezhvvv*hv*chivvv +  &
                   9.0d0*(ezhovv*h21*chiovv + ezhoov*h12*chioov) + &
                   ezhooo*ho*chiooo
!
          term1  =     s -     sprim
          term2  = quartmu*(t - tprim)
          term3  = etnthmu2*(    u -     uprim)
          zgint  = dmin*term1 +  cmin*term2 + term3
          zhint  = -dplus*term1 + cplus*term2 - term3
          term2  = quartmu * t
          term3  = etnthmu2*u
          zginf  = dmin*s +  cmin*term2 + term3
          zhinf  = -dplus*s + cplus*term2 - term3

          term1  =     alpha -     beta
          term2  = halfmu*(    alph2 -     beta2)
          term3  = sixthmu2*(    alph3 -     beta3)
          gint   = dmin*term1 +  cmin*term2 + term3
          hint   = -dplus*term1 + cplus*term2 - term3
          term2  = halfmu*alph2
          term3  = sixthmu2*alph3
          ginf   = dmin*alpha +  cmin*term2 + term3
          hinf   = -dplus*alpha + cplus*term2 - term3
          hdn    = zgint/gint
          hup    = zhint/hint
          hdninf = zginf/ginf
          hupinf = zhinf/hinf

!! start !!

      ADJ_zhinf  =   ADJ_hupinf/hinf        + ADJ_zhinf
      ADJ_hinf   = - hupinf*ADJ_hupinf/hinf + ADJ_hinf

      ADJ_zginf  =   ADJ_hdninf/ginf        + ADJ_zginf
      ADJ_ginf   = - hdninf*ADJ_hdninf/ginf + ADJ_ginf

      ADJ_zhint  =   ADJ_hup/hint     + ADJ_zhint
      ADJ_hint   = - hup*ADJ_hup/hint + ADJ_hint

      ADJ_zgint  =   ADJ_hdn/gint       + ADJ_zgint
      ADJ_gint   = - hdn * ADJ_hdn/gint + ADJ_gint

       ADJ_dplus  = - ADJ_hinf*alpha  + ADJ_dplus 
       ADJ_alpha = - dplus*ADJ_hinf   + ADJ_alpha
       ADJ_cplus =   ADJ_hinf*term2   + ADJ_cplus
       ADJ_term2 =   cplus*ADJ_hinf 
       ADJ_term3 = - ADJ_hinf         

      ADJ_dmin   =   ADJ_ginf*alpha + ADJ_dmin
      ADJ_alpha  =   dmin*ADJ_ginf  + ADJ_alpha
      ADJ_cmin   =   ADJ_ginf*term2 + ADJ_cmin
      ADJ_term2  =   cmin*ADJ_ginf  + ADJ_term2
      ADJ_term3  =   ADJ_ginf       + ADJ_term3

      ADJ_sixthmu2 = ADJ_term3*alph3    + ADJ_sixthmu2
      ADJ_alph3    = sixthmu2*ADJ_term3 + ADJ_alph3

      ADJ_halfmu =  ADJ_term2*alph2  + ADJ_halfmu
      ADJ_alph2  =  halfmu*ADJ_term2 + ADJ_alph2
!
! new term2,3

      term2 = halfmu*(alph2 - beta2)
      term3 = sixthmu2*(alph3 - beta3)

      ADJ_dplus  = - ADJ_hint*term1 + ADJ_dplus
      ADJ_term1  = - dplus*ADJ_hint 
      ADJ_cplus  =   ADJ_hint*term2 + ADJ_cplus
      ADJ_term2  =   cplus*ADJ_hint 
      ADJ_term3  = - ADJ_hint

      ADJ_dmin   = ADJ_gint*term1 + ADJ_dmin
      ADJ_term1  = dmin*ADJ_gint  + ADJ_term1
      ADJ_cmin   = ADJ_gint*term2 + ADJ_cmin
      ADJ_term2  = cmin*ADJ_gint  + ADJ_term2
      ADJ_term3  = ADJ_gint       + ADJ_term3

      ADJ_sixthmu2 =    ADJ_term3*(alph3 - beta3) + ADJ_sixthmu2
      ADJ_alph3    =    sixthmu2*ADJ_term3        + ADJ_alph3
      ADJ_beta3    =  - sixthmu2*ADJ_term3        + ADJ_beta3
       
      ADJ_halfmu =   ADJ_term2*(alph2 - beta2) + ADJ_halfmu
      ADJ_alph2  =   halfmu*ADJ_term2          + ADJ_alph2
      ADJ_beta2  = - halfmu*ADJ_term2          + ADJ_beta2

      ADJ_alpha  =   ADJ_term1 + ADJ_alpha
      ADJ_beta   = - ADJ_term1 + ADJ_beta
!
! new term2,3

      term2 = quartmu*t
      term3 = etnthmu2*u

       ADJ_dplus = - ADJ_zhinf*s     + ADJ_dplus
           ADJ_s = - dplus*ADJ_zhinf + ADJ_s
       ADJ_cplus =   ADJ_zhinf*term2 + ADJ_cplus
       ADJ_term2 =   cplus*ADJ_zhinf
       ADJ_term3 = - ADJ_zhinf 

      ADJ_dmin   = ADJ_zginf*s     + ADJ_dmin
      ADJ_s      = dmin*ADJ_zginf  + ADJ_s
      ADJ_cmin   = ADJ_zginf*term2 + ADJ_cmin
      ADJ_term2  = cmin*ADJ_zginf  + ADJ_term2
      ADJ_term3  = ADJ_zginf       + ADJ_term3
      
      ADJ_etnthmu2 = ADJ_term3*u        + ADJ_etnthmu2
      ADJ_u        = etnthmu2*ADJ_term3 + ADJ_u
      ADJ_quartmu = ADJ_term2*t         + ADJ_quartmu   
      ADJ_t       = quartmu*ADJ_term2   + ADJ_t

!
! new term1,2,3
!
      term1 = s - sprim
      term2 = quartmu*(t - tprim)
      term3 = etnthmu2*(u - uprim)

       ADJ_dplus = - ADJ_zhint*term1 + ADJ_dplus
       ADJ_term1 = - dplus*ADJ_zhint
       ADJ_cplus =   ADJ_zhint*term2 + ADJ_cplus
       ADJ_term2 =   cplus*ADJ_zhint
       ADJ_term3 = - ADJ_zhint

       ADJ_dmin  = ADJ_zgint*term1 + ADJ_dmin
      ADJ_term1  = dmin*ADJ_zgint  + ADJ_term1
       ADJ_cmin  = ADJ_zgint*term2 + ADJ_cmin
      ADJ_term2  = cmin*ADJ_zgint  + ADJ_term2
      ADJ_term3  = ADJ_zgint       + ADJ_term3

      ADJ_etnthmu2 =   ADJ_term3*(u - uprim) + ADJ_etnthmu2
      ADJ_u        =   etnthmu2*ADJ_term3    + ADJ_u
      ADJ_uprim    = - etnthmu2*ADJ_term3    + ADJ_uprim

      ADJ_quartmu =   ADJ_term2*(t - tprim) + ADJ_quartmu
      ADJ_t       =   quartmu*ADJ_term2     + ADJ_t
      ADJ_tprim   = - quartmu*ADJ_term2     + ADJ_tprim 

      ADJ_s      =   ADJ_term1 + ADJ_s
      ADJ_sprim  = - ADJ_term1 + ADJ_sprim

!!!
      ADJ_ezhvvv = ADJ_uprim*hv*chivvv            + ADJ_ezhvvv
      ADJ_hv     = ezhvvv*ADJ_uprim*chivvv        + ADJ_hv
      ADJ_chivvv = ezhvvv*hv*ADJ_uprim            + ADJ_chivvv
      ADJ_ezhovv = 9.0d0*ADJ_uprim*h21*chiovv     + ADJ_ezhovv
      ADJ_h21    = 9.0d0*ezhovv*ADJ_uprim*chiovv  + ADJ_h21
      ADJ_chiovv = 9.0d0*ezhovv*h21*ADJ_uprim     + ADJ_chiovv
      ADJ_ezhoov = 9.0d0*ADJ_uprim*h12*chioov     + ADJ_ezhoov
      ADJ_h12    = 9.0d0*ezhoov*ADJ_uprim*chioov  + ADJ_h12
      ADJ_chioov = 9.0d0*ezhoov*h12*ADJ_uprim     + ADJ_chioov
      ADJ_ezhooo = ADJ_uprim*ho*chiooo            + ADJ_ezhooo
      ADJ_ho     = ezhooo*ADJ_uprim*chiooo        + ADJ_ho
      ADJ_chiooo = ezhooo*ho*ADJ_uprim            + ADJ_chiooo

      ADJ_sigooo = ADJ_u*ho                       + ADJ_sigooo
      ADJ_ho     = sigooo*ADJ_u                   + ADJ_ho 
      ADJ_sigovv = 9.0d0*ADJ_u*h21                + ADJ_sigovv
      ADJ_h21    = 9.0d0*sigovv*ADJ_u             + ADJ_h21
      ADJ_sigoov = 9.0d0*ADJ_u*h12                + ADJ_sigoov
      ADJ_h12    = 9.0d0*sigoov*ADJ_u             + ADJ_h12
      ADJ_sigvvv = ADJ_u*hv                       + ADJ_sigvvv
      ADJ_hv     = sigvvv*ADJ_u                   + ADJ_hv
 
      ADJ_ezhoo  = ADJ_tprim*ho*chioo             + ADJ_ezhoo
      ADJ_ho     = ezhoo*ADJ_tprim*chioo          + ADJ_ho
      ADJ_chioo  = ezhoo*ho*ADJ_tprim             + ADJ_chioo
      ADJ_ezhov  = 4.0d0*ADJ_tprim*newh11*chiov   + ADJ_ezhov
      ADJ_newh11 = 4.0d0*ezhov*ADJ_tprim*chiov    + ADJ_newh11
      ADJ_chiov  = 4.0d0*ezhov*newh11*ADJ_tprim   + ADJ_chiov
      ADJ_ezhvv  = ADJ_tprim*hv*chivv             + ADJ_ezhvv
      ADJ_hv     = ezhvv*ADJ_tprim*chivv          + ADJ_hv
      ADJ_chivv  = ezhvv*hv*ADJ_tprim             + ADJ_chivv

      ADJ_sigoo  = ADJ_t*ho           + ADJ_sigoo
      ADJ_ho     = sigoo*ADJ_t        + ADJ_ho
      ADJ_sigov  = 4.0d0*ADJ_t*newh11 + ADJ_sigov
      ADJ_newh11 = 4.0d0*sigov*ADJ_t  + ADJ_newh11
      ADJ_sigvv  = ADJ_t*hv           + ADJ_sigvv
      ADJ_hv     = sigvv*ADJ_t        + ADJ_hv

      ADJ_ezho   = ADJ_sprim*ho*chio   + ADJ_ezho
      ADJ_ho     = ezho*ADJ_sprim*chio + ADJ_ho
      ADJ_chio   = ezho*ho*ADJ_sprim   + ADJ_chio
      ADJ_ezhv   = ADJ_sprim*hv*chiv   + ADJ_ezhv
      ADJ_hv     = ezhv*ADJ_sprim*chiv + ADJ_hv
      ADJ_chiv   = ezhv*hv*ADJ_sprim   + ADJ_chiv

      ADJ_sigo   = ADJ_s*ho     + ADJ_sigo
      ADJ_ho     = sigo*ADJ_s   + ADJ_ho
      ADJ_sigv   = ADJ_s*hv     + ADJ_sigv
      ADJ_hv     = sigv*ADJ_s   + ADJ_hv

!!
      ADJ_ezhvv  = ADJ_ezhvvv*ezhv   + ADJ_ezhvv
      ADJ_ezhv   = ezhvv*ADJ_ezhvvv  + ADJ_ezhv
      ADJ_ezhoo  = ADJ_ezhooo*ezho   + ADJ_ezhoo
      ADJ_ezho   = ezhoo*ADJ_ezhooo  + ADJ_ezho
      ADJ_ezhoo  = ADJ_ezhoov*ezhv   + ADJ_ezhoo 
      ADJ_ezhv   = ezhoo*ADJ_ezhoov  + ADJ_ezhv
      ADJ_ezho   = ADJ_ezhovv*ezhvv  + ADJ_ezho
      ADJ_ezhvv  = ezho*ADJ_ezhovv   + ADJ_ezhvv
      ADJ_ezhv   = 2.*ezhv*ADJ_ezhvv + ADJ_ezhv
      ADJ_ezho   = ADJ_ezhov*ezhv    + ADJ_ezho
      ADJ_ezhv   = ezho*ADJ_ezhov    + ADJ_ezhv
      ADJ_ezho   = 2.*ezho*ADJ_ezhoo + ADJ_ezho
      ADJ_sigvv  = ADJ_sigvvv*sigv   + ADJ_sigvv
      ADJ_sigv   = sigvv*ADJ_sigvvv  + ADJ_sigv
      ADJ_sigo   = ADJ_sigovv*sigvv  + ADJ_sigo
      ADJ_sigvv  = sigo*ADJ_sigovv   + ADJ_sigvv
      ADJ_sigoo  = ADJ_sigoov*sigv   + ADJ_sigoo
      ADJ_sigv   = sigoo*ADJ_sigoov  + ADJ_sigv
      ADJ_sigoo  = ADJ_sigooo*sigo   + ADJ_sigoo
      ADJ_sigo   = sigoo*ADJ_sigooo  + ADJ_sigo
      ADJ_sigv   = 2.*sigv*ADJ_sigvv + ADJ_sigv
      ADJ_sigo   = 2.*sigo*ADJ_sigoo + ADJ_sigo
      ADJ_sigo   = ADJ_sigov*sigv    + ADJ_sigo
      ADJ_sigv   = sigo*ADJ_sigov    + ADJ_sigv

!
!     ADJ_chiv   =         ADJ_chiv 
!     ADJ_chio   =         ADJ_chio 

! 
! new chio chiv

          chio   = chio_save
          chiv   = chiv_save

      ADJ_chivv  = ADJ_chivvv + ADJ_chivv
      ADJ_chiv   = ADJ_chivvv + ADJ_chiv

      ADJ_chioo  = ADJ_chiooo + ADJ_chioo
      ADJ_chio   = ADJ_chiooo + ADJ_chio

      ADJ_chio   = ADJ_chiovv + ADJ_chio
      ADJ_chivv  = ADJ_chiovv + ADJ_chivv

      ADJ_chioo  = ADJ_chioov + ADJ_chioo
      ADJ_chiv   = ADJ_chioov + ADJ_chiv

      ADJ_chiv   = ADJ_chivv + ADJ_chiv
      ADJ_chiv   = ADJ_chivv + ADJ_chiv

      ADJ_chio   = ADJ_chioo + ADJ_chio
      ADJ_chio   = ADJ_chioo + ADJ_chio

      ADJ_chio   =  ADJ_chiov + ADJ_chio
      ADJ_chiv   =  ADJ_chiov + ADJ_chiv

      ADJ_h11    = -1.0d0*newh11*newh11*ADJ_newh11 + ADJ_h11

      ADJ_h11    = -1.0d0*h12*h12*ADJ_h12 + ADJ_h11
      ADJ_hoinv  = -1.0d0*h12*h12*ADJ_h12 + ADJ_hoinv

      ADJ_h11    = -1.0d0*h21*h21*ADJ_h21 + ADJ_h11
      ADJ_hvinv  = -1.0d0*h21*h21*ADJ_h21 + ADJ_hvinv

      ADJ_hoinv  = ADJ_h11 + ADJ_hoinv
      ADJ_hvinv  = ADJ_h11 + ADJ_hvinv

      ADJ_cmin   = ADJ_dmin  + ADJ_cmin
!     ADJ_dmin   = ADJ_dmin
          dmin   = dmin_save
 
      ADJ_cplus  = ADJ_dplus + ADJ_cplus
!     ADJ_dplus  = ADJ_dplus
      dplus      = dplus_save

      ADJ_dplus  = ADJ_dmin           + ADJ_dplus
      ADJ_halfmu2 = ADJ_dplus*alph2   + ADJ_halfmu2
      ADJ_alph2   = halfmu2*ADJ_dplus + ADJ_alph2
      ADJ_mualph = - ADJ_cmin         + ADJ_mualph
      ADJ_mualph = ADJ_cplus          + ADJ_mualph
      ADJ_mu     = ADJ_mualph*alpha   + ADJ_mu
      ADJ_alpha  = mu*ADJ_mualph      + ADJ_alpha
!
      ADJ_mu2  = 0.5d0*ADJ_halfmu2   + ADJ_mu2
      ADJ_mu   = 0.25d0*ADJ_quartmu  + ADJ_mu
      ADJ_mu2  = ADJ_etnthmu2/18.0d0 + ADJ_mu2
      ADJ_mu2  = ADJ_sixthmu2/6.0d0  + ADJ_mu2
      ADJ_mu   = 0.5d0*ADJ_halfmu    + ADJ_mu
      ADJ_mu   = 2.*mu*ADJ_mu2       + ADJ_mu
!
      ADJ_beta = ADJ_beta3*beta2               + ADJ_beta
      ADJ_beta2 = beta*ADJ_beta3               + ADJ_beta2
      ADJ_beta = 2.*beta*ADJ_beta2             + ADJ_beta
      ADJ_ezho = ADJ_beta                      + ADJ_ezho
      ADJ_ezhv = ADJ_beta                      + ADJ_ezhv
      ADJ_alpha = ADJ_alph3*alph2              + ADJ_alpha
      ADJ_alph2 = alpha*ADJ_alph3              + ADJ_alph2
      ADJ_alpha = 2.*alpha*ADJ_alph2           + ADJ_alpha
      ADJ_sigo = ADJ_alpha                     + ADJ_sigo
      ADJ_sigv = ADJ_alpha                     + ADJ_sigv
      ADJ_sigv =  ADJ_ezhv*exp(-chiv)          + ADJ_sigv
      ADJ_chiv = -ADJ_ezhv*ezhv                + ADJ_chiv
      ADJ_sigo =  ADJ_ezho*exp(-chio)          + ADJ_sigo
      ADJ_chio = -ADJ_ezho*ezho                + ADJ_chio
      ADJ_zcld = ADJ_chiv*hvinv                + ADJ_zcld
      ADJ_hvinv= zcld*ADJ_chiv                 + ADJ_hvinv
      ADJ_zcld  = ADJ_chio*hoinv               + ADJ_zcld
      ADJ_hoinv = zcld*ADJ_chio                + ADJ_hoinv
      ADJ_hv    = -1.0d0*hvinv*hvinv*ADJ_hvinv + ADJ_hv
      ADJ_ho    = -1.0d0*hoinv*hoinv*ADJ_hoinv + ADJ_ho

!! end !!

END SUBROUTINE ADJ_effht

!============================================================================
SUBROUTINE ADJ_epsalt(f,t,ssw,epsr,epsi,                                    &
                      ADJ_t, ADJ_epsr, ADJ_epsi                             )
! ---------------------------------------------------------------------------
IMPLICIT NONE
! ---------------------------------------------------------------------------
!Output  : ADJ_t      (ssw is treated as a constant now)
!Input   : ADJ_epsr, ADJ_epsi, epsr, epsi
! ---------------------------------------------------------------------------
      real, intent(IN   ) :: f, t
      real, intent(INOUT) :: ADJ_t
      real, intent(INOUT) :: ssw
      real, intent(IN   ) :: ADJ_epsr, ADJ_epsi, epsr, epsi

      complex :: cdum1,cdum2,cdum3
      complex :: ADJ_cdum1,ADJ_cdum2,ADJ_cdum3
      real :: pi
      parameter (pi = 3.14159265)
      real :: ssw2,ssw3,t2,t3,es,a,esnew,tau,b,sig,taunew
      real :: delt,delt2,beta,signew,om,d1,d2
      real :: ADJ_t2,ADJ_t3,ADJ_es,ADJ_a,ADJ_esnew,ADJ_tau,ADJ_b,ADJ_taunew
      real :: ADJ_delt,ADJ_delt2,ADJ_beta,ADJ_signew
      real :: ADJ_d1,ADJ_d2

      ssw2=0.;ssw3=0.;t2=0.;t3=0.;es=0.;a=0.;esnew=0.;tau=0.;b=0.
      sig=0.;taunew=0.;delt=0.;delt2=0.;beta=0.;signew=0.;om=0.
      d1=0.;d2=0.;ADJ_t2=0.;ADJ_t3=0.;ADJ_es=0.;ADJ_a=0.;ADJ_esnew=0.
      ADJ_tau=0.;ADJ_b=0.;ADJ_taunew=0.;ADJ_delt=0.;ADJ_delt2=0.
      ADJ_beta=0.;ADJ_signew=0.;ADJ_d1=0.;ADJ_d2=0.
!
      if (ssw .lt. 0.0) ssw = 32.54
        ssw2     = ssw*ssw
        ssw3     = ssw2*ssw
          t2     = t*t
          t3     = t2*t
          es     = 87.134 - 1.949e-1*t - 1.276e-2*t2 + 2.491e-4*t3
          a      = 1.0 + 1.613e-5*ssw*t - 3.656e-3*ssw + 3.21e-5*ssw2 - &
                   4.232e-7*ssw3
          esnew  = es*a
!
          tau    = 1.768e-11 - 6.086e-13*t + 1.104e-14*t2 - 8.111e-17*t3
          b      = 1.0 + 2.282e-5*ssw*t - 7.638e-4*ssw - 7.760e-6*ssw2 + &
                   1.105e-8*ssw3
          taunew = tau*b
!
          sig    = ssw*(0.182521 - 1.46192e-3*ssw + 2.09324e-5*ssw2 - &
                   1.28205e-7*ssw3)
          delt   = 25.0 - t
          delt2  = delt*delt
          beta   =   2.033e-2 + 1.266e-4*delt      + 2.464e-6*delt2       &
                   - ssw*(1.849e-5 - 2.551e-7*delt + 2.551e-8*delt2)
         signew  =   sig*exp(-beta*delt)
!
             om  = 2.0e9*pi*f
          cdum1  = cmplx(0.0,om*taunew)
          cdum2  = cmplx(0.0,signew/(om*8.854e-12))

          cdum3  = 4.9 + (esnew-4.9)/(1.0 + cdum1) - cdum2
    
!         epsr   = real(cdum3)
!         epsi   = -aimag(cdum3)

!!! start

      ADJ_cdum3  = ADJ_epsr + ADJ_epsi *(0.,1.)
      ADJ_esnew  =   ADJ_cdum3/(1.0 + cdum1)
      ADJ_cdum1  = - ADJ_cdum3*(esnew-4.9)/((1.0 + cdum1)*(1.0 + cdum1))
      ADJ_cdum2  = - ADJ_cdum3


      ADJ_signew = -aimag(ADJ_cdum2/(om*8.854e-12)) 

      ADJ_taunew = om*(-aimag(ADJ_cdum1))

      ADJ_beta   = - signew*ADJ_signew*delt
      ADJ_delt   = - signew*beta*ADJ_signew

      ADJ_delt   =   1.266e-4*ADJ_beta     + ADJ_delt
      ADJ_delt2  =   2.464e-6*ADJ_beta
      ADJ_delt   =   ssw*2.551e-7*ADJ_beta + ADJ_delt
      ADJ_delt2  = - ssw*2.551e-8*ADJ_beta + ADJ_delt2

      ADJ_delt   = 2.*delt*ADJ_delt2 + ADJ_delt

      ADJ_t      =  - ADJ_delt + ADJ_t

      ADJ_tau    = ADJ_taunew*b 
      ADJ_b      = tau*ADJ_taunew

      ADJ_t      = 2.282e-5*ssw*ADJ_b  + ADJ_t

      ADJ_t      = - 6.086e-13*ADJ_tau + ADJ_t
      ADJ_t2     =   1.104e-14*ADJ_tau
      ADJ_t3     = - 8.111e-17*ADJ_tau

      ADJ_es     = ADJ_esnew*a
      ADJ_a      = es*ADJ_esnew
      ADJ_t      = 1.613e-5*ssw*ADJ_a  + ADJ_t
      ADJ_t      = - 1.949e-1*ADJ_es   + ADJ_t
      ADJ_t2     = - 1.276e-2*ADJ_es   + ADJ_t2
      ADJ_t3     =   2.491e-4*ADJ_es   + ADJ_t3

      ADJ_t2     = ADJ_t3*t     + ADJ_t2
      ADJ_t      = t2*ADJ_t3    + ADJ_t
      ADJ_t      = 2.*t*ADJ_t2  + ADJ_t

!!! end


END SUBROUTINE ADJ_epsalt

!---------------------------------------------------------------------------
SUBROUTINE ADJ_spemiss(f,tk,theta,ssw,ev,eh,                               &
                       ADJ_tk,ADJ_ev,ADJ_eh                                )
!---------------------------------------------------------------------------
IMPLICIT NONE
!---------------------------------------------------------------------------
!
!Output :: ADJ_tk
!Input  :: ADJ_ev, ADJ_eh, ev, eh
!---------------------------------------------------------------------------
      real, intent(IN   ) :: f, tk, theta, ADJ_ev,ADJ_eh
      real, intent(INOUT) :: ssw
      real, intent(INOUT) :: ADJ_tk

      real   epsr,epsi,ADJ_epsr,ADJ_epsi, ev, eh
!
      real      tc,costh,sinth,rthet
      complex   etav,etah,eps,cterm1v,cterm1h,cterm2,cterm3v,cterm3h,epsnew
      complex   ADJ_etav,ADJ_eps,ADJ_cterm1v,ADJ_cterm2,ADJ_cterm3v
      complex   ADJ_cterm3h,ADJ_epsnew
      complex   uniti
      real      tmp1r,tmp1i,tmp2r,tmp2i,tmp0r,tmp0i,rnorm
      real      ADJ_tc,ADJ_tmp0r,ADJ_tmp0i,ADJ_rnorm,ADJ_tmp1r
      real      ADJ_tmp1i,ADJ_tmp2r,ADJ_tmp2i
!
      epsr=0.;epsi=0.;ADJ_epsr=0.;ADJ_epsi=0.; ev=0.;eh=0.
      tc=0.;costh=0.;sinth=0.;rthet=0.
      tmp1r=0.;tmp1i=0.;tmp2r=0.;tmp2i=0.;tmp0r=0.;tmp0i=0.;rnorm=0.
      ADJ_tc=0.;ADJ_tmp0r=0.;ADJ_tmp0i=0.;ADJ_rnorm=0.;ADJ_tmp1r=0.
      ADJ_tmp1i=0.;ADJ_tmp2r=0.;ADJ_tmp2i=0.


           tc     =      tk - 273.15

      call epsalt(f,tc,ssw,epsr,epsi)

          eps     =  cmplx(epsr,epsi)
          etav    =  eps
          etah    =  (1.0,0.0)
          rthet   =  theta*0.017453292
          costh   =  cos(rthet)
          sinth   =  sin(rthet)
          sinth   =  sinth*sinth
        cterm1v   =  etav*costh
        cterm1h   =  etah*costh
         epsnew   =  eps - sinth
         cterm2   =  csqrt(epsnew)

        cterm3v   =  (cterm1v - cterm2)/(cterm1v + cterm2)
        cterm3h   =  (cterm1h - cterm2)/(cterm1h + cterm2)
          tmp1r   =  real(cterm3v)
          tmp1i   = -aimag(cterm3v)
!            ev   =  1.0 - (tmp1r*tmp1r+tmp1i*tmp1i)

          tmp2r   =  real(cterm3h)
          tmp2i   = -aimag(cterm3h)
!            eh   =  1.0 - (tmp2r*tmp2r+tmp2i*tmp2i)

!!!

      ADJ_tmp2r   = - 2.*tmp2r*ADJ_eh
      ADJ_tmp2i   = - 2.*tmp2i*ADJ_eh

      ADJ_cterm3h =  ADJ_tmp2r + ADJ_tmp2i*(0.,1.)

      ADJ_tmp1r   = - 2.*tmp1r*ADJ_ev
      ADJ_tmp1i   = - 2.*tmp1i*ADJ_ev

      ADJ_cterm3v =  ADJ_tmp1r + ADJ_tmp1i*(0.,1.)

      ADJ_cterm2  = - ADJ_cterm3h/(cterm1h + cterm2)
      ADJ_cterm2  = - cterm3h*ADJ_cterm3h/(cterm1h + cterm2) + ADJ_cterm2

      ADJ_cterm1v =   ADJ_cterm3v/(cterm1v + cterm2)
      ADJ_cterm2  = - ADJ_cterm3v/(cterm1v + cterm2) + ADJ_cterm2
      ADJ_cterm1v = - cterm3v*ADJ_cterm3v/(cterm1v + cterm2) + ADJ_cterm1v
      ADJ_cterm2  = - cterm3v*ADJ_cterm3v/(cterm1v + cterm2) + ADJ_cterm2

      if (cabs(epsnew) .gt. 0.) then
         ADJ_epsnew  = ADJ_cterm2*0.5/cterm2
      else
         ADJ_epsnew  =  0.
      endif

      ADJ_eps     =  ADJ_epsnew

      ADJ_etav    =  ADJ_cterm1v*costh

      ADJ_eps     =  ADJ_etav + ADJ_eps

      ADJ_epsr    =  real(ADJ_eps)
      ADJ_epsi    =  -aimag(ADJ_eps) 
      ADJ_tc      =  0.
      CALL ADJ_epsalt(f,tc,ssw,epsr,epsi,ADJ_tc, ADJ_epsr, ADJ_epsi  )

      ADJ_tk      =  ADJ_tc + ADJ_tk


END SUBROUTINE ADJ_spemiss

!==================================================================
 SUBROUTINE ADJ_tbatmos(ifreq,theta,p0,wv,hwv,ta,gamma,lw,zcld,   &
                        tbup,tbdn,tauatm,                         &
                        ADJ_theta,ADJ_p0,ADJ_wv,ADJ_hwv,ADJ_ta,ADJ_gamma,   &
                        ADJ_lw,ADJ_zcld,ADJ_tbup,ADJ_tbdn,        &
                        ADJ_tauatm                                )
! -----------------------------------------------------------------
  IMPLICIT NONE
! -----------------------------------------------------------------
!
! Output : ADJ_p0,ADJ_wv,ADJ_hwv,ADJ_ta,ADJ_gamma,ADJ_lw,ADJ_zcld 
!          ADJ_theta (somtime theta is a variable)
! Input  : ADJ_tbup,ADJ_tbdn,ADJ_tauatm
! Input mean fields : tbup,tbdn,tauatm
! -----------------------------------------------------------------
      integer,intent(IN   ) :: ifreq
      real   ,intent(IN   ) :: theta,p0,wv,hwv,ta,gamma,lw,zcld
      real   ,intent(INOUT) :: ADJ_p0,ADJ_wv,ADJ_hwv,ADJ_ta,       &
                               ADJ_gamma,ADJ_lw,ADJ_zcld,ADJ_theta
      real   ,intent(INOUT) :: ADJ_tbup,ADJ_tbdn,ADJ_tauatm
      real                  :: tbup,tbdn,tauatm,tbdn_save

      real :: mu,hdn,hup,hdninf,hupinf,ADJ_mu
!
      real :: b1(4),b2(4),b3(4)
      real :: c(4),d1(4),d2(4),d3(4),zeta(4),kw0(4),kw1(4),kw2(4),kw3(4)
      real :: tau,tau1,tau2,taucld
      real :: tcld,tc,em,em1
      real :: sigv,sigo,sig,sig1,sigcld
      real :: teff1dn,teff1up,teffdn,teffup
      real :: tbcld,tbclrdn,tbclrup,tb1dn,tb1up,tb2dn,tb2up
      real :: otbar,tc2,tc3,hv,ho,alph
      real :: ADJ_sigv,ADJ_otbar,ADJ_sigo,ADJ_tcld,ADJ_tc,ADJ_tc2,ADJ_tc3
      real :: ADJ_sigcld,ADJ_taucld,ADJ_tbcld,ADJ_hv,ADJ_ho
      real :: ADJ_hdn,ADJ_hup,ADJ_hdninf,ADJ_sig,ADJ_sig1,ADJ_tau,ADJ_tau1
      real :: ADJ_tau2,ADJ_em1,ADJ_teff1dn,ADJ_hupinf,ADJ_em,ADJ_teff1up
      real :: ADJ_teffdn,ADJ_teffup,ADJ_tbclrdn,ADJ_tbclrup,ADJ_tb1dn,ADJ_tb1up
      real :: ADJ_tb2dn,ADJ_tb2up,ADJ_alph

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

      mu=0.;hdn=0.;hup=0.;hdninf=0.;hupinf=0.;ADJ_mu=0.
      tcld=0.;tc=0.;em=0.;em1=0.
      sigv=0.;sigo=0.;sig=0.;sig1=0.;sigcld=0.
      teff1dn=0.;teff1up=0.;teffdn=0.;teffup=0.
      tbcld=0.;tbclrdn=0.;tbclrup=0.;tb1dn=0.;tb1up=0.;tb2dn=0.;tb2up=0.
      otbar=0.;tc2=0.;tc3=0.;hv=0.;ho=0.;alph=0.
      ADJ_sigv=0.;ADJ_otbar=0.;ADJ_sigo=0.;ADJ_tcld=0.;
      ADJ_tc=0.;ADJ_tc2=0.;ADJ_tc3=0.
      ADJ_sigcld=0.;ADJ_taucld=0.;ADJ_tbcld=0.;ADJ_hv=0.;ADJ_ho=0.
      ADJ_hdn=0.;ADJ_hup=0.;ADJ_hdninf=0.;ADJ_sig=0.;ADJ_sig1=0.
      ADJ_tau=0.;ADJ_tau1=0.
      ADJ_tau2=0.;ADJ_em1=0.;ADJ_teff1dn=0.;ADJ_hupinf=0.;ADJ_em=0.
      ADJ_teff1up=0.;ADJ_teffdn=0;ADJ_teffup=0.;ADJ_tbclrdn=0.
      ADJ_tbclrup=0.;ADJ_tb1dn=0.;ADJ_tb1up=0.
      ADJ_tb2dn=0.;ADJ_tb2up=0.;ADJ_alph=0.
      tau=0.;tau1=0.;tau2=0.;taucld=0.
      tcld=0.;tc=0.;em=0.;em1=0.
      sigv=0.;sigo=0.;sig=0.;sig1=0.;sigcld=0.
      teff1dn=0.;teff1up=0.;teffdn=0.;teffup=0.


! mu = secant(theta)
! somtime theta is a variable

      mu     = 1.0/cos(theta*0.0174533)

! get water vapor optical depth
!=====

      CALL cal_sigma_v(ifreq,p0,wv,hwv,ta,gamma,sigv)

! otbar = one over "mean" temperature

          otbar =   1.0/(ta - gamma*zeta(ifreq))

! sigo = dry air optical depth

          sigo = b1(ifreq) + b2(ifreq)*    p0  + b3(ifreq)*    otbar

! cloud parameters

          tcld   =     ta - gamma*zcld
            tc   =     tcld - 273.15
           tc2   =     tc*tc
           tc3   =     tc2*tc
          sigcld =  ( kw0(ifreq) + tc*kw1(ifreq) + tc2*kw2(ifreq) +  &
                      tc3*kw3(ifreq) )*lw
          taucld =   exp(-mu*sigcld)
           tbcld =   (1.0 - taucld)*tcld

! hv, ho = effective absorber scale heights for vapor, dry air

          hv = c(ifreq)*   hwv
          ho = d1(ifreq) + d2(ifreq)* ta + d3(ifreq)* gamma

! get effective emission heights for layer 1 and total atmosphere


      CALL effht(ho,hv,sigo,sigv,mu,zcld,hdn,hup, &
                 hdninf,hupinf)

! atmospheric transmittances in layer one and two, and combined

           sig =     sigo +     sigv
          sig1 = sigo*(1.0-exp(-zcld/ho)) + sigv*(1.0-exp(-zcld/hv))
          tau  =  exp(-mu*sig)
          tau1 =  exp(-mu*sig1)
          tau2 =  tau/tau1

! atmospheric "emissivity"

          em1  =   1.0 - tau1
          em   =   1.0 - tau

! downwelling and upwelling brightness temperature for each layer

          teff1dn =     ta - gamma*hdn
          teff1up =     ta - gamma*hup
           teffdn =     ta - gamma*hdninf
           teffup =     ta - gamma*hupinf
          tbclrdn = teffdn*em
          tbclrup = teffup*em
!
           tb1dn = em1*teff1dn
           tb1up = em1*teff1up
           tb2dn = (tbclrdn - tb1dn)/tau1
           tb2up =      tbclrup - tau2*tb1up

! total downwelling and upwelling brightness temperature and transmittance

          tbdn  =     tb1dn + tau1*(tbcld + taucld*tb2dn)
          tbup  =     tb2up + tau2*(tbcld + taucld*tb1up)
          tauatm = tau*taucld
!
! the following lines apply an ad hoc correction to improve fit 
! at large angles and/or high gaseous opacities 
!  (downwelling brightness temperatures only)

      alph = (0.636619*atan(mu*sig))**2
      tbdn_save = tbdn
      tbdn = (1.0-alph)*tbdn + em*alph*ta
!

!!! start

      tbdn = tbdn_save

      ADJ_alph = - ADJ_tbdn*tbdn
!     print*,'debug',ADJ_tbdn,alph,ta,tbdn
      ADJ_em   = ADJ_tbdn*alph*ta
      ADJ_alph = em*ADJ_tbdn*ta + ADJ_alph
      ADJ_ta   = em*alph*ADJ_tbdn + ADJ_ta
      ADJ_tbdn = (1.0-alph)*ADJ_tbdn 

      if (abs(sig) .gt. 0.) then
         ADJ_mu  = 2.*0.636619*0.636619*ADJ_alph*sig*atan(mu*sig)/(1.+mu*mu*sig*sig)
         ADJ_sig = 2.*0.636619*0.636619*mu*ADJ_alph*atan(mu*sig)/(1.+mu*mu*sig*sig)
      else
         ADJ_mu  = 0.
         ADJ_sig = 0.
      endif

      ADJ_tau    = ADJ_tauatm*taucld
      ADJ_taucld = tau*ADJ_tauatm
      ADJ_tb2up  = ADJ_tbup
      ADJ_tau2   = ADJ_tbup*(tbcld + taucld*tb1up)
      ADJ_tbcld  = tau2*ADJ_tbup
      ADJ_taucld = tau2*ADJ_tbup*tb1up  + ADJ_taucld
      ADJ_tb1up  = tau2*taucld*ADJ_tbup
      ADJ_tb1dn = ADJ_tbdn
      ADJ_tau1  = ADJ_tbdn*(tbcld + taucld*tb2dn)
      ADJ_tbcld = tau1*ADJ_tbdn   + ADJ_tbcld
      ADJ_taucld = tau1*ADJ_tbdn*tb2dn  + ADJ_taucld
      ADJ_tb2dn  = tau1*taucld*ADJ_tbdn

       ADJ_tbclrup =   ADJ_tb2up
       ADJ_tau2    = - ADJ_tb2up*tb1up + ADJ_tau2
       ADJ_tb1up   = - tau2*ADJ_tb2up  + ADJ_tb1up

       ADJ_tbclrdn =   ADJ_tb2dn/tau1
       ADJ_tb1dn   = - ADJ_tb2dn/tau1  + ADJ_tb1dn
       ADJ_tau1    = - tb2dn*ADJ_tb2dn/tau1 + ADJ_tau1

       ADJ_em1     = ADJ_tb1up*teff1up
       ADJ_teff1up = em1*ADJ_tb1up

       ADJ_em1     = ADJ_tb1dn*teff1dn + ADJ_em1
       ADJ_teff1dn = em1*ADJ_tb1dn

       ADJ_teffup  = ADJ_tbclrup*em
       ADJ_em      = teffup*ADJ_tbclrup + ADJ_em

       ADJ_teffdn  = ADJ_tbclrdn*em
       ADJ_em      = teffdn*ADJ_tbclrdn + ADJ_em

       ADJ_ta      =   ADJ_teffup + ADJ_ta
       ADJ_gamma   = - ADJ_teffup*hupinf + ADJ_gamma
       ADJ_hupinf  = - gamma*ADJ_teffup

       ADJ_ta      =   ADJ_teffdn + ADJ_ta
       ADJ_gamma   = - ADJ_teffdn*hdninf + ADJ_gamma
       ADJ_hdninf  = - gamma*ADJ_teffdn

       ADJ_ta    =   ADJ_teff1up + ADJ_ta
       ADJ_gamma = - ADJ_teff1up*hup + ADJ_gamma
       ADJ_hup   = - gamma*ADJ_teff1up

       ADJ_ta    =   ADJ_teff1dn + ADJ_ta
       ADJ_gamma = - ADJ_teff1dn*hdn + ADJ_gamma
       ADJ_hdn   = - gamma*ADJ_teff1dn

       ADJ_tau  = - ADJ_em + ADJ_tau

       ADJ_tau1 = - ADJ_em1 + ADJ_tau1

       ADJ_tau  =   ADJ_tau2/tau1 + ADJ_tau
       ADJ_tau1 = - tau2*ADJ_tau2/tau1 + ADJ_tau1

       ADJ_sig1 = - mu*ADJ_tau1*tau1
       ADJ_mu   = - ADJ_tau1*sig1*tau1 + ADJ_mu
    
       ADJ_mu   = - ADJ_tau*sig*tau + ADJ_mu
       ADJ_sig  = - mu*ADJ_tau*tau + ADJ_sig
 
       ADJ_sigo = ADJ_sig1*(1.0-exp(-zcld/ho))
       ADJ_sigv = ADJ_sig1*(1.0-exp(-zcld/hv)) 
       ADJ_zcld = sigo*ADJ_sig1/ho*exp(-zcld/ho) + ADJ_zcld
       ADJ_ho   = - sigo*zcld*ADJ_sig1/(ho*ho)*exp(-zcld/ho)
       ADJ_zcld = sigv*ADJ_sig1/hv*exp(-zcld/hv) + ADJ_zcld
       ADJ_hv   = - sigv*zcld*ADJ_sig1/(hv*hv)*exp(-zcld/hv)

       ADJ_sigo = ADJ_sig + ADJ_sigo
       ADJ_sigv = ADJ_sig + ADJ_sigv

       CALL ADJ_effht(ho,hv,sigo,sigv,mu,zcld,hdn,hup,        &
                      hdninf,hupinf,                          &
                      ADJ_ho,ADJ_hv,ADJ_sigo,ADJ_sigv,ADJ_mu, &
                      ADJ_zcld,ADJ_hdn,ADJ_hup,ADJ_hdninf,    &
                      ADJ_hupinf                              )

       ADJ_ta    = d2(ifreq)*ADJ_ho + ADJ_ta
       ADJ_gamma = d3(ifreq)*ADJ_ho + ADJ_gamma
       ADJ_hwv   = c(ifreq)*ADJ_hv  + ADJ_hwv

       ADJ_taucld = - ADJ_tbcld*tcld + ADJ_taucld
       ADJ_tcld = (1.0 - taucld)*ADJ_tbcld

       ADJ_mu     = - ADJ_taucld*sigcld*taucld + ADJ_mu
       ADJ_sigcld = - mu*ADJ_taucld*taucld

       ADJ_tc  = ADJ_sigcld*kw1(ifreq)*lw
       ADJ_tc2 = ADJ_sigcld*kw2(ifreq)*lw
       ADJ_tc3 = ADJ_sigcld*kw3(ifreq)*lw
       ADJ_lw  = (kw0(ifreq)+tc*kw1(ifreq)+tc2*kw2(ifreq)+tc3*kw3(ifreq)) &
                 *ADJ_sigcld + ADJ_lw

       ADJ_tc2  = ADJ_tc3*tc + ADJ_tc2
       ADJ_tc   = tc2*ADJ_tc3 + ADJ_tc
       ADJ_tc   = 2.*tc*ADJ_tc2 + ADJ_tc

       ADJ_tcld = ADJ_tc + ADJ_tcld

       ADJ_ta    =   ADJ_tcld + ADJ_ta
       ADJ_gamma = - ADJ_tcld*zcld + ADJ_gamma
       ADJ_zcld  = - gamma*ADJ_tcld + ADJ_zcld

       ADJ_p0    = b2(ifreq)*ADJ_sigo + ADJ_p0
       ADJ_otbar = b3(ifreq)*ADJ_sigo

       ADJ_ta    = - otbar*otbar*ADJ_otbar + ADJ_ta
       ADJ_gamma =   otbar*otbar*ADJ_otbar*zeta(ifreq) + ADJ_gamma

       CALL ADJ_cal_sigma_v(ifreq,p0,wv,hwv,ta,gamma,sigv,   &
                            ADJ_p0,ADJ_wv,ADJ_hwv,ADJ_ta,    &
                            ADJ_gamma,ADJ_sigv               )

       ADJ_theta = mu*mu*0.0174533*ADJ_mu*sin(theta*0.0174533) + ADJ_theta

!
END SUBROUTINE ADJ_tbatmos

!=================================================================
SUBROUTINE ADJ_effang(ifreq,theta,gx2,sigma,effangv,effangh,     &
                      ADJ_gx2,ADJ_sigma,ADJ_effangv,ADJ_effangh  )
! ----------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------
! 
! Output :: ADJ_gx2, ADJ_sigma
! Input  :: ADJ_effangv,ADJ_effangh,effangv,effangh
! ----------------------------------------------------------------
!
! Calculated the effective zenith angle of reflected microwave radiation
! at SSM/I frequencies for vertical and horizontal polarization
!
      integer, intent(IN   ) :: ifreq
      real   , intent(IN   ) :: theta,gx2,sigma
      real   , intent(INOUT) :: ADJ_gx2,ADJ_sigma
      real   , intent(INOUT) :: ADJ_effangv,ADJ_effangh
      real                   :: effangv,effangh
!
      real c19v,c19h,c22v,c22h,c37v,c37h,c85v,c85h
      real s19v(6),s19h(6),s22v(6),s22h(6), &
           s37v(6),s37h(6),s85v(6),s85h(6)
!
      real :: alnsig,gg,ggg,xd,xx,xd_save,xx_save
      real :: z1,z2,z3,z4,z5,z6
      real :: y,dth,angh,angv,alnthv,alnthh,alnthv_save,alnthh_save
      real :: ADJ_alnsig,ADJ_gg,ADJ_ggg,ADJ_xd
      real :: ADJ_z1,ADJ_z2,ADJ_z3,ADJ_z4,ADJ_z5,ADJ_z6,ADJ_alnthv
      real :: ADJ_y,ADJ_dth,ADJ_angh,ADJ_angv,ADJ_xx,ADJ_alnthh

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

      alnsig=0.;gg=0.;ggg=0.;xd=0.;xx=0.;xd_save=0.;xx_save=0.
      z1=0.;z2=0.;z3=0.;z4=0.;z5=0.;z6=0.;y=0.;dth=0.;angh=0.
      angv=0.;alnthv=0.;alnthh=0.;alnthv_save=0.;alnthh_save=0.
      ADJ_alnsig=0.;ADJ_gg=0.;ADJ_ggg=0.;ADJ_xd=0.
      ADJ_z1=0.;ADJ_z2=0.;ADJ_z3=0.;ADJ_z4=0.;ADJ_z5=0.
      ADJ_z6=0.;ADJ_alnthv=0.;ADJ_y=0.;ADJ_dth=0.;ADJ_angh=0.
      ADJ_angv=0.;ADJ_xx=0.;ADJ_alnthh=0.
!

      if (gx2 .le. 0.0 .or. sigma .le. 0.0) then
            effangv = theta
            effangh = theta
        return
      endif
      alnsig = alog(sigma)
      gg  = gx2*gx2
      ggg = gg*gx2

      if (ifreq .eq. 1) then 
             xd =      alnsig - c19v
             xx =  xd*xd
             z1 =  xx*ggg
             z2 =  xd*ggg
             z3 =  xd*gg
             z4 =  xx*gg
             z5 =  xx*gx2
             z6 =  xd*gx2
         alnthv =  s19v(1)*z1 + s19v(2)*z2 + s19v(3)*z3 + &
                   s19v(4)*z4 + s19v(5)*z5 + s19v(6)*z6
         alnthv_save = alnthv
         alnthv =  alnthv + 3.611
!
             xd_save = xd
             xx_save = xx

             xd =  alnsig - c19h
             xx =  xd*xd
             z1 =  xx*ggg
             z2 =  xd*ggg
             z3 =  xd*gg
             z4 =  xx*gg
             z5 =  xx*gx2
             z6 =  xd*gx2

         alnthh =  s19h(1)*z1 + s19h(2)*z2 + s19h(3)*z3 + &
                   s19h(4)*z4 + s19h(5)*z5 + s19h(6)*z6
         alnthh_save = alnthh
         alnthh =  alnthh + 3.611
!
      else if (ifreq .eq. 2) then 
             xd =      alnsig - c22v
             xx =  xd*xd
             z1 =  xx*ggg
             z2 =  xd*ggg
             z3 =  xd*gg
             z4 =  xx*gg
             z5 =  xx*gx2
             z6 =  xd*gx2
         alnthv =  s22v(1)*z1 + s22v(2)*z2 + s22v(3)*z3 + &
                   s22v(4)*z4 + s22v(5)*z5 + s22v(6)*z6

         alnthv_save = alnthv
         alnthv =  alnthv + 3.611
 
             xd_save = xd
             xx_save = xx

             xd =      alnsig - c22h
             xx =  xd*xd
             z1 =  xx*ggg
             z2 =  xd*ggg
             z3 =  xd*gg
             z4 =  xx*gg
             z5 =  xx*gx2
             z6 =  xd*gx2
         alnthh =  s22h(1)*z1 + s22h(2)*z2 + s22h(3)*z3 + &
                   s22h(4)*z4 + s22h(5)*z5 + s22h(6)*z6

         alnthh_save = alnthh
         alnthh =  alnthh + 3.611

      else if (ifreq .eq. 3) then 
             xd =      alnsig - c37v
             xx =  xd*xd
             z1 =  xx*ggg
             z2 =  xd*ggg
             z3 =  xd*gg
             z4 =  xx*gg
             z5 =  xx*gx2
             z6 =  xd*gx2
         alnthv =  s37v(1)*z1 + s37v(2)*z2 + s37v(3)*z3 + &
                   s37v(4)*z4 + s37v(5)*z5 + s37v(6)*z6

         alnthv_save = alnthv
         alnthv =  alnthv + 3.611
 
             xd_save = xd
             xx_save = xx

             xd =      alnsig - c37h
             xx =  xd*xd
             z1 =  xx*ggg
             z2 =  xd*ggg
             z3 =  xd*gg
             z4 =  xx*gg
             z5 =  xx*gx2
             z6 =  xd*gx2
         alnthh =  s37h(1)*z1 + s37h(2)*z2 + s37h(3)*z3 + &
                   s37h(4)*z4 + s37h(5)*z5 + s37h(6)*z6
 
         alnthh_save = alnthh
         alnthh =  alnthh + 3.611

      else if (ifreq .eq. 4) then 
             xd =      alnsig - c85v
             xx =  xd*xd
             z1 =  xx*ggg
             z2 =  xd*ggg
             z3 =  xd*gg
             z4 =  xx*gg
             z5 =  xx*gx2
             z6 =  xd*gx2
         alnthv =  s85v(1)*z1 + s85v(2)*z2 + s85v(3)*z3 + &
                   s85v(4)*z4 + s85v(5)*z5 + s85v(6)*z6

         alnthv_save = alnthv
         alnthv =  alnthv + 3.611
!
             xd_save = xd
             xx_save = xx

             xd =      alnsig - c85h
             xx =  xd*xd
             z1 =  xx*ggg
             z2 =  xd*ggg
             z3 =  xd*gg
             z4 =  xx*gg
             z5 =  xx*gx2
             z6 =  xd*gx2
         alnthh =  s85h(1)*z1 + s85h(2)*z2 + s85h(3)*z3 + &
                   s85h(4)*z4 + s85h(5)*z5 + s85h(6)*z6

         alnthh_save = alnthh
         alnthh =  alnthh + 3.611

      endif
      angv =   90.0 - exp(alnthv)
      angh =   90.0 - exp(alnthh)
          y    =   1.0 - 28.0*gx2
      if (y .lt. 0.0) then
          y = 0.0
      endif

      dth     = (theta - 53.0)*y
      effangv = angv + dth
      effangh = angh + dth

!!!! start

      if (gx2 .le. 0.0 .or. sigma .le. 0.0) then
        ADJ_effangv = 0.
        ADJ_effangh = 0.
        return
      endif

      ADJ_angh  = ADJ_effangh
      ADJ_dth   = ADJ_effangh

      ADJ_angv  = ADJ_effangv
      ADJ_dth   = ADJ_effangv  + ADJ_dth

      ADJ_y     = (theta - 53.0)*ADJ_dth  

      if (y .lt. 0.0) then
         ADJ_y = 0.0
      endif

      ADJ_gx2  = - 28.0*ADJ_y + ADJ_gx2

      ADJ_alnthh = - ADJ_angh*exp(alnthh)
      ADJ_alnthv = - ADJ_angv*exp(alnthv)

      if (ifreq .eq. 1) then 

         alnthh = alnthh_save

         ADJ_z1  = s19h(1)*ADJ_alnthh
         ADJ_z2  = s19h(2)*ADJ_alnthh
         ADJ_z3  = s19h(3)*ADJ_alnthh
         ADJ_z4  = s19h(4)*ADJ_alnthh
         ADJ_z5  = s19h(5)*ADJ_alnthh
         ADJ_z6  = s19h(6)*ADJ_alnthh

         ADJ_xd  = ADJ_z6*gx2
         ADJ_gx2 = xd*ADJ_z6  + ADJ_gx2
         ADJ_xx  = ADJ_z5*gx2
         ADJ_gx2 = xx*ADJ_z5  + ADJ_gx2
         ADJ_xx  = ADJ_z4*gg  + ADJ_xx
         ADJ_gg  = xx*ADJ_z4
         ADJ_xd  = ADJ_z3*gg  + ADJ_xd
         ADJ_gg  = xd*ADJ_z3  + ADJ_gg
         ADJ_xd  = ADJ_z2*ggg + ADJ_xd
         ADJ_ggg = xd*ADJ_z2
         ADJ_xx  = ADJ_z1*ggg + ADJ_xx
         ADJ_ggg = xx*ADJ_z1  + ADJ_ggg

         ADJ_xd  = 2.*xd*ADJ_xx + ADJ_xd
         ADJ_alnsig = ADJ_xd

!! vertical
             xd =  xd_save
             xx =  xx_save

         alnthv = alnthv_save

         ADJ_z1   = s19v(1)*ADJ_alnthv  
         ADJ_z2   = s19v(2)*ADJ_alnthv 
         ADJ_z3   = s19v(3)*ADJ_alnthv 
         ADJ_z4   = s19v(4)*ADJ_alnthv
         ADJ_z5   = s19v(5)*ADJ_alnthv
         ADJ_z6   = s19v(6)*ADJ_alnthv

         ADJ_xd  = ADJ_z6*gx2 !+ ADJ_xd
         ADJ_gx2 = xd*ADJ_z6   + ADJ_gx2
         ADJ_xx  = ADJ_z5*gx2 !+ ADJ_xx
         ADJ_gx2 = xx*ADJ_z5   + ADJ_gx2
         ADJ_xx  = ADJ_z4*gg   + ADJ_xx
         ADJ_gg  = xx*ADJ_z4  + ADJ_gg
         ADJ_xd  = ADJ_z3*gg  + ADJ_xd
         ADJ_gg  = xd*ADJ_z3  + ADJ_gg
         ADJ_xd  = ADJ_z2*ggg + ADJ_xd
         ADJ_ggg = xd*ADJ_z2  + ADJ_ggg
         ADJ_xx  = ADJ_z1*ggg + ADJ_xx
         ADJ_ggg = xx*ADJ_z1  + ADJ_ggg

         ADJ_xd  = 2.*xd*ADJ_xx + ADJ_xd

         ADJ_alnsig = ADJ_xd + ADJ_alnsig

      else if (ifreq .eq. 2) then 

         ADJ_z1 = s22h(1)*ADJ_alnthh
         ADJ_z2 = s22h(2)*ADJ_alnthh
         ADJ_z3 = s22h(3)*ADJ_alnthh
         ADJ_z4 = s22h(4)*ADJ_alnthh
         ADJ_z5 = s22h(5)*ADJ_alnthh
         ADJ_z6 = s22h(6)*ADJ_alnthh
 
         ADJ_xd  = ADJ_z6*gx2
         ADJ_gx2 = xd*ADJ_z6  + ADJ_gx2
         ADJ_xx  = ADJ_z5*gx2
         ADJ_gx2 = xx*ADJ_z5  + ADJ_gx2
         ADJ_xx  = ADJ_z4*gg  + ADJ_xx
         ADJ_gg  = xx*ADJ_z4  + ADJ_gg
         ADJ_xd  = ADJ_z3*gg  + ADJ_xd
         ADJ_gg  = xd*ADJ_z3  + ADJ_gg
         ADJ_xd  = ADJ_z2*ggg + ADJ_xd
         ADJ_ggg = xd*ADJ_z2  + ADJ_ggg
         ADJ_xx  = ADJ_z1*ggg + ADJ_xx 
         ADJ_ggg = xx*ADJ_z1  + ADJ_ggg

         ADJ_xd  =  2.*xd*ADJ_xx + ADJ_xd
         ADJ_alnsig = ADJ_xd

!! vertical
             xd =  xd_save
             xx =  xx_save

         alnthv = alnthv_save

         ADJ_z1  = s22v(1)*ADJ_alnthv
         ADJ_z2  = s22v(2)*ADJ_alnthv
         ADJ_z3  = s22v(3)*ADJ_alnthv
         ADJ_z4  = s22v(4)*ADJ_alnthv
         ADJ_z5  = s22v(5)*ADJ_alnthv
         ADJ_z6  = s22v(6)*ADJ_alnthv

         ADJ_xd  = ADJ_z6*gx2
         ADJ_gx2 = xd*ADJ_z6  + ADJ_gx2
         ADJ_xx  = ADJ_z5*gx2 
         ADJ_gx2 = xx*ADJ_z5  + ADJ_gx2
         ADJ_xx  = ADJ_z4*gg  + ADJ_xx
         ADJ_gg  = xx*ADJ_z4  + ADJ_gg
         ADJ_xd  = ADJ_z3*gg  + ADJ_xd
         ADJ_gg  = xd*ADJ_z3  + ADJ_gg
         ADJ_xd  = ADJ_z2*ggg + ADJ_xd
         ADJ_ggg = xd*ADJ_z2  + ADJ_ggg
         ADJ_xx  = ADJ_z1*ggg + ADJ_xx
         ADJ_ggg = xx*ADJ_z1  + ADJ_ggg
         ADJ_xd  =  2.*xd*ADJ_xx + ADJ_xd
         ADJ_alnsig = ADJ_xd     + ADJ_alnsig

      else if (ifreq .eq. 3) then 

         ADJ_z1  = s37h(1)*ADJ_alnthh
         ADJ_z2  = s37h(2)*ADJ_alnthh
         ADJ_z3  = s37h(3)*ADJ_alnthh
         ADJ_z4  = s37h(4)*ADJ_alnthh
         ADJ_z5  = s37h(5)*ADJ_alnthh
         ADJ_z6  = s37h(6)*ADJ_alnthh

         ADJ_xd  = ADJ_z6*gx2
         ADJ_gx2 = xd*ADJ_z6  + ADJ_gx2
         ADJ_xx  = ADJ_z5*gx2 
         ADJ_gx2 = xx*ADJ_z5  + ADJ_gx2
         ADJ_xx  = ADJ_z4*gg  + ADJ_xx
         ADJ_gg  = xx*ADJ_z4  + ADJ_gg
         ADJ_xd  = ADJ_z3*gg  + ADJ_xd
         ADJ_gg  = xd*ADJ_z3  + ADJ_gg
         ADJ_xd  = ADJ_z2*ggg + ADJ_xd
         ADJ_ggg = xd*ADJ_z2  + ADJ_ggg
         ADJ_xx  = ADJ_z1*ggg + ADJ_xx
         ADJ_ggg = xx*ADJ_z1  + ADJ_ggg
         ADJ_xd  = 2.*xd*ADJ_xx + ADJ_xd
         ADJ_alnsig = ADJ_xd

!! vertical
             xd =  xd_save
             xx =  xx_save

         alnthv = alnthv_save

         ADJ_z1  = s37v(1)*ADJ_alnthv
         ADJ_z2  = s37v(2)*ADJ_alnthv
         ADJ_z3  = s37v(3)*ADJ_alnthv
         ADJ_z4  = s37v(4)*ADJ_alnthv
         ADJ_z5  = s37v(5)*ADJ_alnthv
         ADJ_z6  = s37v(6)*ADJ_alnthv
  
         ADJ_xd  = ADJ_z6*gx2
         ADJ_gx2 = xd*ADJ_z6  + ADJ_gx2
         ADJ_xx  = ADJ_z5*gx2
         ADJ_gx2 = xx*ADJ_z5  + ADJ_gx2
         ADJ_xx  = ADJ_z4*gg  + ADJ_xx
         ADJ_gg  = xx*ADJ_z4  + ADJ_gg
         ADJ_xd  =ADJ_z3*gg   + ADJ_xd
         ADJ_gg  = xd*ADJ_z3  + ADJ_gg
         ADJ_xd  = ADJ_z2*ggg + ADJ_xd 
         ADJ_ggg = xd*ADJ_z2  + ADJ_ggg
         ADJ_xx  = ADJ_z1*ggg + ADJ_xx
         ADJ_ggg = xx*ADJ_z1  + ADJ_ggg
         ADJ_xd  = 2.*xd*ADJ_xx + ADJ_xd
         ADJ_alnsig = ADJ_xd  + ADJ_alnsig

      else if (ifreq .eq. 4) then 

         ADJ_z1  = s85h(1)*ADJ_alnthh
         ADJ_z2  = s85h(2)*ADJ_alnthh
         ADJ_z3  = s85h(3)*ADJ_alnthh
         ADJ_z4  = s85h(4)*ADJ_alnthh
         ADJ_z5  = s85h(5)*ADJ_alnthh
         ADJ_z6  = s85h(6)*ADJ_alnthh

         ADJ_xd  = ADJ_z6*gx2
         ADJ_gx2 = xd*ADJ_z6  + ADJ_gx2
         ADJ_xx  = ADJ_z5*gx2
         ADJ_gx2 = xx*ADJ_z5  + ADJ_gx2
         ADJ_xx  = ADJ_z4*gg  + ADJ_xx
         ADJ_gg  = xx*ADJ_z4  + ADJ_gg
         ADJ_xd  = ADJ_z3*gg  + ADJ_xd
         ADJ_gg  = xd*ADJ_z3  + ADJ_gg
         ADJ_xd  = ADJ_z2*ggg + ADJ_xd
         ADJ_ggg = xd*ADJ_z2  + ADJ_ggg
         ADJ_xx  = ADJ_z1*ggg + ADJ_xx
         ADJ_ggg = xx*ADJ_z1  + ADJ_ggg
         ADJ_xd  = 2.*xd*ADJ_xx + ADJ_xd
         ADJ_alnsig = ADJ_xd

!! vertical
             xd =  xd_save
             xx =  xx_save

         alnthv = alnthv_save

         ADJ_z1  = s85v(1)*ADJ_alnthv
         ADJ_z2  = s85v(2)*ADJ_alnthv
         ADJ_z3  = s85v(3)*ADJ_alnthv
         ADJ_z4  = s85v(4)*ADJ_alnthv
         ADJ_z5  = s85v(5)*ADJ_alnthv
         ADJ_z6  = s85v(6)*ADJ_alnthv

         ADJ_xd  = ADJ_z6*gx2
         ADJ_gx2 = xd*ADJ_z6  + ADJ_gx2
         ADJ_xx  = ADJ_z5*gx2
         ADJ_gx2 = xx*ADJ_z5  + ADJ_gx2
         ADJ_xx  = ADJ_z4*gg  + ADJ_xx
         ADJ_gg  = xx*ADJ_z4  + ADJ_gg
         ADJ_xd  = ADJ_z3*gg  + ADJ_xd
         ADJ_gg  = xd*ADJ_z3  + ADJ_gg
         ADJ_xd  = ADJ_z2*ggg + ADJ_xd
         ADJ_ggg = xd*ADJ_z2  + ADJ_ggg
         ADJ_xx  = ADJ_z1*ggg + ADJ_xx
         ADJ_ggg = xx*ADJ_z1  + ADJ_ggg
         ADJ_xd  = 2.*xd*ADJ_xx  + ADJ_xd
         ADJ_alnsig = ADJ_xd  + ADJ_alnsig

      endif

      ADJ_gg  = ADJ_ggg*gx2   + ADJ_gg
      ADJ_gx2 = gg*ADJ_ggg    + ADJ_gx2
      ADJ_gx2 = 2.*gx2*ADJ_gg + ADJ_gx2

      if (abs(sigma) .gt. 0.) then
         ADJ_sigma = ADJ_alnsig/sigma + ADJ_sigma
!     else
!        ADJ_sigma = 0.
      endif


END SUBROUTINE ADJ_effang

!=================================================================
SUBROUTINE ADJ_roughem(ifreq,gx2,tk,theta,remv,remh,             &
                       ADJ_gx2,ADJ_tk,ADJ_remv,ADJ_remh          )
! ----------------------------------------------------------------
IMPLICIT NONE
! ----------------------------------------------------------------
!
! Input  :: ADJ_tk, ADJ_gx2
! Output :: ADJ_remv,ADJ_remh, remv,remh
!
! Calculates rough-surface emissivity of ocean surface at SSM/I
! frequencies.
!
      integer, intent(IN   ) :: ifreq
      real   , intent(IN   ) :: tk, theta, gx2
      real   , intent(IN   ) :: ADJ_remv,ADJ_remh
      real   , intent(INOUT) :: ADJ_tk,ADJ_gx2

      real                   :: remv,remh,ssw
      real a19v(4),a22v(4),a37v(4),a85v(4)
      real a19h(4),a22h(4),a37h(4),a85h(4)
      real f(4)
      real semv,semh,ADJ_semv,ADJ_semh,remv_save,remh_save
      real :: tp,g,x1,x2,x3,x4,dtheta
      real :: ADJ_tp,ADJ_g,ADJ_x1,ADJ_x2,ADJ_x3,ADJ_x4
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
      semv=0.;semh=0.;ADJ_semv=0.;ADJ_semh=0.;remv_save=0.;remh_save=0.
      tp=0.;g=0.;x1=0.;x2=0.;x3=0.;x4=0.;dtheta=0.
      ADJ_tp=0.;ADJ_g=0.;ADJ_x1=0.;ADJ_x2=0.;ADJ_x3=0.;ADJ_x4=0.

          tp =  tk/273.0
      dtheta =  theta-53.0
           g =  0.5* gx2
          x1 =  g
          x2 =  tp*g
          x3 =  dtheta* g
          x4 =  tp*x3
!
      if (ifreq .eq. 1) then
             remv =     x1*a19v(1) +     x2*a19v(2) +     x3*a19v(3) +     x4*a19v(4)
             remh =     x1*a19h(1) +     x2*a19h(2) +     x3*a19h(3) +     x4*a19h(4)
      else if (ifreq .eq. 2) then
             remv =     x1*a22v(1) +     x2*a22v(2) +     x3*a22v(3) +     x4*a22v(4)
             remh =     x1*a22h(1) +     x2*a22h(2) +     x3*a22h(3) +     x4*a22h(4)
      else if (ifreq .eq. 3) then
             remv =     x1*a37v(1) +     x2*a37v(2) +     x3*a37v(3) +     x4*a37v(4)
             remh =     x1*a37h(1) +     x2*a37h(2) +     x3*a37h(3) +     x4*a37h(4)
      else if (ifreq .eq. 4) then
             remv =     x1*a85v(1) +     x2*a85v(2) +     x3*a85v(3) +     x4*a85v(4)
             remh =     x1*a85h(1) +     x2*a85h(2) +     x3*a85h(3) +     x4*a85h(4)
      endif

      ssw=36.5
      CALL spemiss(f(ifreq),tk,theta,ssw,semv,semh)

      remv_save = remv
      remh_save = remh

!!!start

      ADJ_semh = ADJ_remh
     
      ADJ_semv = ADJ_remv
!!
      remv = remv_save
      remh = remh_save

      CALL ADJ_spemiss(f(ifreq),tk,theta,ssw,semv,semh,  &
                       ADJ_tk,ADJ_semv,ADJ_semh           )

 
      if (ifreq .eq. 1) then

         ADJ_x1   = ADJ_remh*a19h(1)
         ADJ_x2   = ADJ_remh*a19h(2)
         ADJ_x3   = ADJ_remh*a19h(3)
         ADJ_x4   = ADJ_remh*a19h(4)
 
         ADJ_x1   = ADJ_remv*a19v(1) + ADJ_x1
         ADJ_x2   = ADJ_remv*a19v(2) + ADJ_x2
         ADJ_x3   = ADJ_remv*a19v(3) + ADJ_x3
         ADJ_x4   = ADJ_remv*a19v(4) + ADJ_x4

      else if (ifreq .eq. 2) then

         ADJ_x1   = ADJ_remh*a22h(1) 
         ADJ_x2   = ADJ_remh*a22h(2)
         ADJ_x3   = ADJ_remh*a22h(3)
         ADJ_x4   = ADJ_remh*a22h(4)

         ADJ_x1   = ADJ_remv*a22v(1) + ADJ_x1
         ADJ_x2   = ADJ_remv*a22v(2) + ADJ_x2
         ADJ_x3   = ADJ_remv*a22v(3) + ADJ_x3
         ADJ_x4   = ADJ_remv*a22v(4) + ADJ_x4
 

      else if (ifreq .eq. 3) then

         ADJ_x1   = ADJ_remh*a37h(1)
         ADJ_x2   = ADJ_remh*a37h(2)
         ADJ_x3   = ADJ_remh*a37h(3)
         ADJ_x4   = ADJ_remh*a37h(4)         

         ADJ_x1   = ADJ_remv*a37v(1) + ADJ_x1
         ADJ_x2   = ADJ_remv*a37v(2) + ADJ_x2
         ADJ_x3   = ADJ_remv*a37v(3) + ADJ_x3
         ADJ_x4   = ADJ_remv*a37v(4) + ADJ_x4

      else if (ifreq .eq. 4) then

         ADJ_x1   = ADJ_remh*a85h(1) 
         ADJ_x2   = ADJ_remh*a85h(2) 
         ADJ_x3   = ADJ_remh*a85h(3)
         ADJ_x4   = ADJ_remh*a85h(4) 
 
         ADJ_x1   = ADJ_remv*a85v(1) + ADJ_x1
         ADJ_x2   = ADJ_remv*a85v(2) + ADJ_x2
         ADJ_x3   = ADJ_remv*a85v(3) + ADJ_x3
         ADJ_x4   = ADJ_remv*a85v(4) + ADJ_x4
   
      endif

      ADJ_tp = ADJ_x4*x3
      ADJ_x3 = tp*ADJ_x4     + ADJ_x3
      ADJ_g  = dtheta*ADJ_x3
      ADJ_tp = ADJ_x2*g      + ADJ_tp
      ADJ_g  = tp*ADJ_x2     + ADJ_g
      ADJ_g  = ADJ_x1        + ADJ_g
      ADJ_gx2= 0.5*ADJ_g     + ADJ_gx2
      ADJ_tk = ADJ_tp/273.0  + ADJ_tk
 
END SUBROUTINE ADJ_roughem
