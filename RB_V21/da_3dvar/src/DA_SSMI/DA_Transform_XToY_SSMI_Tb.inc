!============================================================================
SUBROUTINE DA_Transform_XToY_SSMI_Tb( xb, oi, xa, xp, y )
!----------------------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------------------
   type (xb_type), intent(in)   :: xb       ! first guess state.
   type (ob_type), intent(in)   :: oi       ! obs. increment vector (o-b).
   type (x_type),  intent(inout):: xa       ! gridded analysis increment.
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   type (y_type), intent(inout) :: y        ! y = h (xa)

!  LOCAL VARS

   integer                      :: n        ! loop counter.
   integer                      :: i,j
   real                         :: dx, dy, dxm, dym

!  real, dimension(mix,mjy)     :: TGL_dotspeed
!  real, dimension(mix,mjy)     :: psfc,ta,gamma,sst,htpw,speed,alw,zcld,tpw
!  real, dimension(mix,mjy)     :: TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,TGL_tpw
!  real, dimension(mix,mjy)     :: TGL_htpw,TGL_speed,TGL_alw,TGL_zcld         

!  real, dimension(mix,mjy)     :: tb19v,tb19h, &
!                                  tb22v,       &
!                                  tb37v,tb37h, &
!                                  tb85v,tb85h

!  real, dimension(mix,mjy)     :: TGL_tb19v,TGL_tb19h, &
!                                  TGL_tb22v,           &
!                                  TGL_tb37v,TGL_tb37h, &
!                                  TGL_tb85v,TGL_tb85h

!  real, dimension(mkz)         :: zh
!  real, dimension(mkz+1)       :: zf
!----------------------------------------------------------------------------

!!Wind at 1st level at dot points

!!speed, surface wind speed (m/sec)    (0 - 30) , take 10 m wind

!!total precipitable water in cm

!  call DA_Transform_XToTPW( xa, xb)

!  do j=1,mjy-1
!  do i=1,mix-1
!     zf(1) = xb%ztop
!     do k=1,mkz
!        zh(k)=xb%h(i,j,k)
!        zf(k+1)=xb%hf(i,j,k)
!     enddo

!!surface pressure (mb) (940 -1030)

!     psfc(i,j)     = (xb%p(i,j,mkz)+xb%Psac(i,j)*(1.0-xb % sigmah(mkz)))*0.01
!     TGL_psfc(i,j) =  xa%p  (i,j,mkz)*0.01

! sea surface temperature (k) (273 - 303) (doesnot change) 

!     sst(i,j)      = xb%tgrn(i,j)
!!    TGL_sst(i,j)  = xa%tgrn(1,1)
!     TGL_sst(i,j)  = 0.

! effective surface air temperature (263 - 303)

!     ta(i,j)     = xb%tgrn(i,j) + &
!                   (xb%t(i,j,mkz)-xb%tgrn(i,j))*log(2./0.0001)/ &
!                   log(zh(mkz)/0.0001)

!!    TGL_ta(i,j) = xa%tgrn(1,1) + &
!!                  (xa%t(i,j,mkz)-xa%tgrn(1,1))*log(2./0.0001)/ &
!!                  log(zh(mkz)/0.0001)

!     TGL_ta(i,j) = (xa%t(i,j,mkz)-0.)*log(2./0.0001)/ &
!                   log(zh(mkz)/0.0001)

!!gamma is an emperical formula and zcld is given for now

!     gamma(i,j) = (ta(i,j)-270)*0.023 + 5.03  ! effective lapse rate   (km) (4.0 - 6.5)
!     zcld(i,j)  = 1                           ! effective cloud height (km)
                                                 ! = 1 if no cloud infomation
!     TGL_gamma(i,j) = TGL_ta(i,j)*0.023
!     TGL_zcld(i,j)  = 0.
     
!!total precipitable water in (kg/m**2) (0 - 70)

!     tpw(i,j)     = xb%tpw(i,j)*10.
!     TGL_tpw(i,j) = xa%tpw(i,j)*10.

!!Column liquid water (kg/m**2)   (0 - 0.5) (no data now. So, do it later.)

!     alw(i,j)     = 0.
!     TGL_alw(i,j) = 0.

!!Column height weighted moisture density on the grid locally 

!     call DA_Transform_XToZRhoQ(xb, i, j, zh, zf, zrhom)
!     call DA_Transform_XToZRhoQ_Lin(xb, xa, i, j, zh, zf, TGL_zrhom)

!!Column moisture density on the grid locally

!     htpw(i,j)     = zrhom/tpw(i,j)/1000.
!     TGL_htpw(i,j) = TGL_zrhom/tpw(i,j)/1000. &
!                   - TGL_tpw(i,j)/tpw(i,j)*htpw(i,j)

!  enddo
!  enddo

!  do j=1,mjy-1
!  do i=1,mix-1

!     call TGL_tb(1,53.,psfc(i,j),ta(i,j),gamma(i,j),sst(i,j),tpw(i,j),  &
!                 htpw(i,j),speed(i,j),alw(i,j),zcld(i,j),               &
!                 xb%tb19v(i,j),xb%tb19h(i,j),                           &
!                 TGL_psfc(i,j),TGL_ta(i,j),TGL_gamma(i,j),TGL_sst(i,j), &
!                 TGL_tpw(i,j),TGL_htpw(i,j),TGL_speed(i,j),TGL_alw(i,j),&
!                 TGL_zcld(i,j),TGL_tb19v(i,j),TGL_tb19h(i,j)            )

!     call TGL_tb(2,53.,psfc(i,j),ta(i,j),gamma(i,j),sst(i,j),tpw(i,j),  &
!                 htpw(i,j),speed(i,j),alw(i,j),zcld(i,j),               &
!                 xb%tb22v(i,j),dum1,                                    &
!                 TGL_psfc(i,j),TGL_ta(i,j),TGL_gamma(i,j),TGL_sst(i,j), &
!                 TGL_tpw(i,j),TGL_htpw(i,j),TGL_speed(i,j),TGL_alw(i,j),&
!                 TGL_zcld(i,j),TGL_tb22v(i,j),dum2                      )

!     call TGL_tb(3,53.,psfc(i,j),ta(i,j),gamma(i,j),sst(i,j),tpw(i,j),  &
!                 htpw(i,j),speed(i,j),alw(i,j),zcld(i,j),               &
!                 xb%tb37v(i,j),xb%tb37h(i,j),                           &
!                 TGL_psfc(i,j),TGL_ta(i,j),TGL_gamma(i,j),TGL_sst(i,j), &
!                 TGL_tpw(i,j),TGL_htpw(i,j),TGL_speed(i,j),TGL_alw(i,j),&
!                 TGL_zcld(i,j),TGL_tb37v(i,j),TGL_tb37h(i,j)            )

!     call TGL_tb(4,53.,psfc(i,j),ta(i,j),gamma(i,j),sst(i,j),tpw(i,j),  &
!                 htpw(i,j),speed(i,j),alw(i,j),zcld(i,j),               &
!                 xb%tb85v(i,j),xb%tb85h(i,j),                           &
!                 TGL_psfc(i,j),TGL_ta(i,j),TGL_gamma(i,j),TGL_sst(i,j), &
!                 TGL_tpw(i,j),TGL_htpw(i,j),TGL_speed(i,j),TGL_alw(i,j),&
!                 TGL_zcld(i,j),TGL_tb85v(i,j),TGL_tb85h(i,j)            )
!  enddo
!  enddo

   do n=1, oi % num_ssmi_tb

     if(oi%ssmi_tb(n)%loc%proc_domain_with_halo) then

         i = oi%ssmi_tb(n)%loc%i
         j = oi%ssmi_tb(n)%loc%j
         dx = oi%ssmi_tb(n)%loc%dx
         dy = oi%ssmi_tb(n)%loc%dy
         dxm = oi%ssmi_tb(n)%loc%dxm
         dym = oi%ssmi_tb(n)%loc%dym

!--------Interpolating from 4 grid points to obs location

#ifndef DEREF_KLUDGE
!     if (xb%landmask(i,j  ) .eq. 0 .and. xb%landmask(i+1,j  ) .eq. 0 .and. &
!         xb%landmask(i,j+1) .eq. 0 .and. xb%landmask(i+1,j+1) .eq. 0       &
!         ) then ! only calculate over water

         call Interp_lin_2D(xa%tb19v, xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_tb(n)%tb19v)
         call Interp_lin_2D(xa%tb19h, xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_tb(n)%tb19h)
         call Interp_lin_2D(xa%tb22v, xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_tb(n)%tb22v)
         call Interp_lin_2D(xa%tb37v, xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_tb(n)%tb37v)
         call Interp_lin_2D(xa%tb37h, xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_tb(n)%tb37h)
         call Interp_lin_2D(xa%tb85v, xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_tb(n)%tb85v)
         call Interp_lin_2D(xa%tb85h, xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_tb(n)%tb85h)
!     endif
#else
!     if (xb%landmask(i,j  ) .eq. 0 .and. xb%landmask(i+1,j  ) .eq. 0 .and. &
!         xb%landmask(i,j+1) .eq. 0 .and. xb%landmask(i+1,j+1) .eq. 0       &
!         ) then ! only calculate over water

         call Interp_lin_2D(xa%tb19v(xb%ims,xb%jms), xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_tb(n)%tb19v)
         call Interp_lin_2D(xa%tb19h(xb%ims,xb%jms), xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_tb(n)%tb19h)
         call Interp_lin_2D(xa%tb22v(xb%ims,xb%jms), xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_tb(n)%tb22v)
         call Interp_lin_2D(xa%tb37v(xb%ims,xb%jms), xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_tb(n)%tb37v)
         call Interp_lin_2D(xa%tb37h(xb%ims,xb%jms), xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_tb(n)%tb37h)
         call Interp_lin_2D(xa%tb85v(xb%ims,xb%jms), xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_tb(n)%tb85v)
         call Interp_lin_2D(xa%tb85h(xb%ims,xb%jms), xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_tb(n)%tb85h)
!     endif
#endif
      endif
   end do
    
!        y%ssmi_tb(n)%tb19v = hor_interp(dxm,dx,dym,dy, &
!                                        TGL_tb19v(i,j  ),TGL_tb19v(i+1,j  ), &
!                                        TGL_tb19v(i,j+1),TGL_tb19v(i+1,j+1)  )

!        y%ssmi_tb(n)%tb19h = hor_interp(dxm,dx,dym,dy, &
!                                        TGL_tb19h(i,j  ),TGL_tb19h(i+1,j  ), &
!                                        TGL_tb19h(i,j+1),TGL_tb19h(i+1,j+1)  )

!        y%ssmi_tb(n)%tb22v = hor_interp(dxm,dx,dym,dy, &
!                                        TGL_tb22v(i,j  ),TGL_tb22v(i+1,j  ), &
!                                        TGL_tb22v(i,j+1),TGL_tb22v(i+1,j+1)  )

!        y%ssmi_tb(n)%tb37v = hor_interp(dxm,dx,dym,dy, &
!                                        TGL_tb37v(i,j  ),TGL_tb37v(i+1,j  ), &
!                                        TGL_tb37v(i,j+1),TGL_tb37v(i+1,j+1)  )

!        y%ssmi_tb(n)%tb37h = hor_interp(dxm,dx,dym,dy, &
!                                        TGL_tb37h(i,j  ),TGL_tb37h(i+1,j  ), &
!                                        TGL_tb37h(i,j+1),TGL_tb37h(i+1,j+1)  )

!        y%ssmi_tb(n)%tb85v = hor_interp(dxm,dx,dym,dy, &
!                                        TGL_tb85v(i,j  ),TGL_tb85v(i+1,j  ), &
!                                        TGL_tb85v(i,j+1),TGL_tb85v(i+1,j+1)  )

!        y%ssmi_tb(n)%tb85h = hor_interp(dxm,dx,dym,dy, &
!                                        TGL_tb85h(i,j  ),TGL_tb85h(i+1,j  ), &
!                                        TGL_tb85h(i,j+1),TGL_tb85h(i+1,j+1)  )
!     endif
!    endif

!  end do

END SUBROUTINE DA_Transform_XToY_SSMI_Tb

