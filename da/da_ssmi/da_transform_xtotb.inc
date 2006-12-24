subroutine da_transform_xtotb(xb)

   !----------------------------------------------------------------------
   ! Purpose: TBD
   !----------------------------------------------------------------------

   implicit none

   type (xb_type), intent(inout)   :: xb       ! first guess state.

   integer            :: i,j,k,ks,ke 

   real               :: psfc,ta,gamma,sst,htpw,speed,alw,zcld,tpw,dum1,zrhom

   if (trace_use) call da_trace_entry("da_transform_xtotb")

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

         gamma   = (ta-270)*0.023 + 5.03  ! effective lapse rate(km) (4.0-6.5)

         zcld    = 1                           ! effective cloud height (km)
                                               ! = 1 if no cloud infomation
         ! total precipitable water in cm
         ! total precipitable water in (kg/m**2) (0 - 70)

         tpw          = xb%tpw(i,j)*10.
         speed        = xb%speed(i,j)

         ! Column liquid water (kg/m**2)  (0-0.5) (no data now. So do it later)

         alw          = 0.

         ! Column height weighted moisture density on the grid locally 

         zrhom = 0.0
         do k=ks,ke
            zrhom=zrhom+(xb%hf(i,j,k+1)-xb%hf(i,j,k))*xb%h(i,j,k)*xb%q(i,j,k)* &
               xb%rho(i,j,k)
         end do

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

   if (trace_use) call da_trace_entry("da_transform_xtotb")

end subroutine da_transform_xtotb


