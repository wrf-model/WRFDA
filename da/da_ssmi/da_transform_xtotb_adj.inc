subroutine da_transform_xtotb_adj (xa, xb)

   IMPLICIT NONE

   type (xb_type), intent(in)    :: xb          ! first guess state.
   type (x_type) , intent(inout) :: xa          ! grad_x(jo)

   integer                       :: n                ! loop counter.
   integer                       :: i,j,k
   integer                       :: is, js, ie, je, ks, ke

   real                          :: dx, dy, dxm, dym, zhmkz
   real                          :: dum1, dum2, zrhom, ADJ_zrhom

   real                          :: psfc,ta,gamma,sst,htpw,speed,alw,zcld,tpw
   real                          :: ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_tpw
   real                          :: ADJ_htpw,ADJ_speed,ADJ_alw,ADJ_zcld         

   psfc=0.;ta=0.;gamma=0.;sst=0.;htpw=0.;speed=0.;alw=0.;zcld=0.;tpw=0
   dx=0.; dy=0.; dxm=0.; dym=0.; zhmkz=0.
   dum1=0.; dum2=0.; zrhom=0.; ADJ_zrhom=0.

   is = xb%its
   js = xb%jts
   ks = xb%kts

   ie = xb%ite
   je = xb%jte
   ke = xb%kte
   if ( Testing_WRFVAR ) then
      is = xb%its-1
      js = xb%jts-1

      ie = xb%ite+1
      je = xb%jte+1

      if ( is < xb%ids ) is = xb%ids
      if ( js < xb%jds ) js = xb%jds

      if ( ie > xb%ide ) ie = xb%ide
      if ( je > xb%jde ) je = xb%jde
   endif

   ! Mean fields

   do j=js, je
      do i=is, ie

         psfc  = 0.01*xb%psfc(i,j)
         ! sst   = xb%tgrn(i,j)
         ta    = xb%tgrn(i,j) + &
                 (xb%t(i,j,ks)-xb%tgrn(i,j))*log(2./0.0001)/ &
                 log(xb%h(i,j,ks)/0.0001)

         gamma = (ta-270)*0.023 + 5.03  ! effective lapse rate   (km) (4.0-6.5)
         zcld  = 1                      ! effective cloud height (km)

         tpw   = xb%tpw(i,j)*10.
         ! speed = xb%speed(i,j)

         alw   = 0.

         zrhom = 0.0
         do k=ks,ke
            zrhom=zrhom+(xb%hf(i,j,k+1)-xb%hf(i,j,k))*xb%h(i,j,k)*xb%q(i,j,k)* &
               xb%rho(i,j,k)
         END DO

         ! call da_transform_xtozrhoq(xb, i, j, zh, zf, zrhom)

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

         call da_adj_tb(1,53.,psfc,ta,gamma,xb%tgrn(i,j),tpw,      &
                     htpw,xb%speed(i,j),alw,zcld,               &
                     xb%tb19v(i,j),xb%tb19h(i,j),               &
                     ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
                     ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
                     ADJ_zcld,xa%tb19v(i,j),xa%tb19h(i,j)     )

         call da_adj_tb(2,53.,psfc,ta,gamma,xb%tgrn(i,j),tpw,      &
                     htpw,xb%speed(i,j),alw,zcld,               &
                     xb%tb22v(i,j),dum1,                        &
                     ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
                     ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
                     ADJ_zcld,xa%tb22v(i,j),dum2               )

         call da_adj_tb(3,53.,psfc,ta,gamma,xb%tgrn(i,j),tpw,      &
                     htpw,xb%speed(i,j),alw,zcld,               &
                     xb%tb37v(i,j),xb%tb37h(i,j),               &
                     ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
                     ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
                     ADJ_zcld,xa%tb37v(i,j),xa%tb37h(i,j)     )

         call da_adj_tb(4,53.,psfc,ta,gamma,xb%tgrn(i,j),tpw,      &
                     htpw,xb%speed(i,j),alw,zcld,               &
                     xb%tb85v(i,j),xb%tb85h(i,j),               &
                     ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
                     ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
                     ADJ_zcld,xa%tb85v(i,j),xa%tb85h(i,j)     )

         ADJ_zrhom    = ADJ_htpw/tpw/1000.
         ADJ_tpw      = ADJ_tpw - ADJ_htpw/tpw*htpw

         DO k = ks,ke
            xa%rho(i,j,k) = (xb%hf(i,j,k+1)-xb%hf(i,j,k))*xb%h(i,j,k)* &
               xb%q(i,j,k)*ADJ_zrhom + xa%rho(i,j,k)
            xa%q(i,j,k)   = (xb%hf(i,j,k+1)-xb%hf(i,j,k))*xb%h(i,j,k)* &
               ADJ_zrhom*xb%rho(i,j,k) + xa%q(i,j,k)
         END DO

         ! call da_transform_xtozrhoq_adj(xb,xa,i,j,zh,zf,ADJ_zrhom)

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

end subroutine da_transform_xtotb_adj


