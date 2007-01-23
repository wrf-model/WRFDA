subroutine da_transform_xtotb_lin(xa, xb)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   !-------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)   :: xb       ! first guess state.
   type (x_type),  intent(inout):: xa       ! gridded analysis increment.

   integer                      :: ks,ke    ! Range of 3rd dimension of arrays.
   integer                      :: i,j,k
   real                         :: dum1, dum2, zrhom, TGL_zrhom

   real                         :: psfc,ta,gamma,sst,htpw,speed,alw,zcld,tpw
   real                         :: TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,TGL_tpw
   real                         :: TGL_htpw,TGL_speed,TGL_alw,TGL_zcld         

   ks = xb%kts
   ke = xb%kte

   ! WHY
   ! call da_transform_xtotpw(xa, xb)

   do j=xb%jts,xb%jte
      do i=xb%its,xb%ite
         ! surface pressure (mb) (940 -1030)

         psfc     =  0.01*xb%psfc(i,j)
         TGL_psfc =  0.01*xa%psfc(i,j)

         ! sea surface temperature (k) (273 - 303) (doesnot change) 

         sst      = xb%tgrn(i,j)
         ! TGL_sst  = xa%tgrn(1,1)
         TGL_sst  = 0.0

         ! effective surface air temperature (263 - 303)

         ta          = xb%tgrn(i,j) + &
                    (xb%t(i,j,ks)-xb%tgrn(i,j))*log(2.0/0.0001)/ &
                    log(xb%h(i,j,ks)/0.0001)

         TGL_ta      = (xa%t(i,j,ks)-0.)*log(2.0/0.0001)/ &
                    log(xb%h(i,j,ks)/0.0001)

         ! gamma is an emperical formula and zcld is given for now

         gamma = (ta-270)*0.023 + 5.03  ! effective lapse rate (km) (4.0 - 6.5)
         zcld       = 1                           ! effective cloud height (km)
                                                 ! = 1 if no cloud infomation
         TGL_gamma = TGL_ta*0.023
         TGL_zcld  = 0.0
     

         ! total precipitable water in (kg/m**2) (0 - 70)

         tpw     = xb%tpw(i,j)*10.0
         TGL_tpw = xa%tpw(i,j)*10.0

         ! speed, surface wind speed (m/sec)    (0 - 30) , take 10 m wind

         speed     = xb%speed(i,j)
         TGL_speed = xa%speed(i,j)

         ! Column liquid water (kg/m**2)   (0 - 0.5) (no data now. So, do it later.)

         alw     = 0.0
         TGL_alw = 0.0

         ! Column height weighted moisture density on the grid locally 

         zrhom = 0.0
         do k=ks,ke
            zrhom=zrhom+(xb%hf(i,j,k+1)-xb%hf(i,j,k))*xb%h(i,j,k)* &
               xb%q(i,j,k)*xb%rho(i,j,k)
         end do

         TGL_zrhom = 0.0
         do k = ks,ke
            TGL_zrhom = (xb%hf(i,j,k+1)-xb%hf(i,j,k))*xb%h(i,j,k)* &
               (xb%q(i,j,k)*xa%rho(i,j,k) + &
               xa%q(i,j,k)*xb%rho(i,j,k)) + TGL_zrhom
      end do

      ! WHY
      ! call da_transform_xtozrhoq(xb, i, j, zh, zf, zrhom)
      ! call da_transform_xtozrhoq_lin(xb, xa, i, j, zh, zf, TGL_zrhom)

      ! Column moisture density on the grid locally

      htpw     = zrhom/tpw/1000.
      TGL_htpw = TGL_zrhom/tpw/1000. &
               - TGL_tpw/tpw*htpw

      dum1 = 0.0

      call da_tb_tl(1,53.,psfc,ta,gamma,sst,tpw,htpw,speed,alw,zcld,  &
                  xb%tb19v(i,j),xb%tb19h(i,j),                      &
                  TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,                &
                  TGL_tpw,TGL_htpw,TGL_speed,TGL_alw,               &
                  TGL_zcld,xa%tb19v(i,j),xa%tb19h(i,j)             )

      call da_tb_tl(2,53.,psfc,ta,gamma,sst,tpw,htpw,speed,alw,zcld,  &
                  xb%tb22v(i,j),dum1,                               &
                  TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,                &
                  TGL_tpw,TGL_htpw,TGL_speed,TGL_alw,               &
                  TGL_zcld,xa%tb22v(i,j),dum2                      )

      call da_tb_tl(3,53.,psfc,ta,gamma,sst,tpw,htpw,speed,alw,zcld,  &
                  xb%tb37v(i,j),xb%tb37h(i,j),                      &
                  TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,                &
                  TGL_tpw,TGL_htpw,TGL_speed,TGL_alw,               &
                  TGL_zcld,xa%tb37v(i,j),xa%tb37h(i,j)             )

         call da_tb_tl(4,53.,psfc,ta,gamma,sst,tpw,htpw,speed,alw,zcld,  &
                  xb%tb85v(i,j),xb%tb85h(i,j),                      &
                  TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,                &
                  TGL_tpw,TGL_htpw,TGL_speed,TGL_alw,               &
                  TGL_zcld,xa%tb85v(i,j),xa%tb85h(i,j)             )
      end do
   end do

end subroutine da_transform_xtotb_lin


