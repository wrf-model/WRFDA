subroutine da_add_pbl_and_sfc_info( xp, xb, xbx )

   !------------------------------------------------------------------------------
   !  PURPOSE: Updates PBL and Surface parameters information for background field 
   !------------------------------------------------------------------------------

   implicit none
   
   type (xpose_type), intent(in)      :: xp           ! Domain decomposition vars.
   type (xb_type), intent(inout)      :: xb           ! First guess state
   type (xbx_type),intent(in)         :: xbx          ! Header & non-gridded vars.

   integer                            :: is,ie   ! Range of 1st dimension of arrays.
   integer                            :: js,je   ! Range of 2nd dimension of arrays.
   integer                            :: ks,ke   ! Range of 3rd dimension of arrays.
   integer                            :: i,j,k        ! Loop counter

   real                               :: ta,gamma,zcld
   real                               :: tpw,alw,zrhom,htpw
   real                               :: dum1

   if (trace_use) call da_trace_entry("da_add_pbl_and_sfc_info")

   ! Set xb array range indices for processor subdomain.
   is = xp%its
   ie = xp%ite
   js = xp%jts
   je = xp%jte
   ks = xp%kts
   ke = xp%kte

   !---------------------------------------------------------------------------  
   ! [1.0] Calculate density:
   !---------------------------------------------------------------------------

   xb%rho(is:ie,js:je,ks:ke) = xb%p(is:ie,js:je,ks:ke) /  &
                                   ( gas_constant * xb%t(is:ie,js:je,ks:ke) )

   !---------------------------------------------------------------------------
   ! [2.0] Calculate 1/grid box areas:
   !---------------------------------------------------------------------------

   xb%grid_box_area(is:ie,js:je) = xb%ds * xb%ds / &
                ( xb%map_factor(is:ie,js:je) * xb%map_factor(is:ie,js:je) )

   !---------------------------------------------------------------------------
   ! [3.0] Calculate saturation vapour pressure and relative humidity:
   !---------------------------------------------------------------------------

   do j = js,je
      do k = ks, ke
         do i = is,ie
            call da_tpq_to_rh( xb%t(i,j,k), xb%p(i,j,k), xb%q(i,j,k), &
                               xb%es(i,j,k), xb%qs(i,j,k), xb%rh(i,j,k) )
         end do
      end do
   end do

   !------------------------------------------------------------------------------
   ! [3.0]  Calculate dew point temperature:
   !------------------------------------------------------------------------------

   call da_trh_to_td ( xb%rh, xb%t, xb%td, xp )

   !------------------------------------------------------------------------------
   ! [4.0] Calculate  Sea level pressure and total precipitable water
   !------------------------------------------------------------------------------

   call da_wrf_tpq_2_slp ( xb )

   ! do j = js,je
   !    do i = is,ie
   !       call da_tpq_to_slp( xb%t(i,j,:), xb%q(i,j,:), xb%p(i,j,:), xb%terr(i,j), &
   !                        xb%psfc(i,j), xb%slp(i,j), xp )
   !    end do
   ! end do

   call da_integrat_dz(xb)

   !---------------------------------------------------------------------------
   ! [5.0] Calculate Roughness length 
   !---------------------------------------------------------------------------

   call da_roughness_from_lanu(19, xbx%mminlu, ANALYSIS_DATE(1:19), &
                            xp, xb%lanu, xb%rough)

   !---------------------------------------------------------------------------
   ! [6.0]  Surface Wind speed SH Chen
   !---------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         call da_transform_xtoseasfcwind( xb%u(i,j,ks),xb%v(i,j,ks), &
                                       xb%speed(i,j),xb%h(i,j,ks)  )
      end do
   end do

   !---------------------------------------------------------------------------
   ! [7.0]  Brightness temperature SH Chen
   !---------------------------------------------------------------------------


   if(Use_SsmiTbObs) then
      zcld  = 1                           ! effective cloud height (km)
                                          ! = 1 if no cloud infomation
      alw   = 0.
      do j=js,je
         do i=is,ie
            ta    = xb%tgrn(i,j) + &
                    (xb%t(i,j,ks)-xb%tgrn(i,j))*log(2./0.0001)/ &
                    log(xb%h(i,j,ks)/0.0001)

            gamma = (ta-270)*0.023 + 5.03  ! effective lapse rate   (km) (4.0 - 6.5)
            tpw   = xb%tpw(i,j) * 10.

            zrhom = 0.0
            do k=ks,ke
               zrhom=zrhom+(xb%hf(i,j,k+1)-xb%hf(i,j,k))*xb%h(i,j,k)*xb%q(i,j,k)*xb%rho(i,j,k)
            end do

            !  call da_transform_xtozrhoq(xb, i, j, xb%h(i,j,:), xb%hf(i,j,:), zrhom)

            htpw  = zrhom/tpw/1000.

            call tb(1,53.,0.01*xb%psfc(i,j),ta,gamma,xb%tgrn(i,j),tpw,htpw, &
                    xb%speed(i,j),alw,zcld, xb%tb19v(i,j),xb%tb19h(i,j))
            call tb(2,53.,0.01*xb%psfc(i,j),ta,gamma,xb%tgrn(i,j),tpw, &
                    htpw,xb%speed(i,j),alw,zcld, xb%tb22v(i,j),dum1)
            call tb(3,53.,0.01*xb%psfc(i,j),ta,gamma,xb%tgrn(i,j),tpw, &
                    htpw,xb%speed(i,j),alw,zcld, xb%tb37v(i,j),xb%tb37h(i,j))
            call tb(4,53.,0.01*xb%psfc(i,j),ta,gamma,xb%tgrn(i,j),tpw, &
               htpw,xb%speed(i,j),alw,zcld, xb%tb85v(i,j),xb%tb85h(i,j))
         end do
      end do
   end if

   !---------------------------------------------------------------------------
   ! [7.0]  Calculate surface variable(wind (10-m) , moistureR & temperature (2-m)
   !---------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         call da_sfc_wtq(xb%psfc(i,j), xb%tgrn(i,j), &
                      xb%p(i,j,1), xb%t(i,j,1), xb%q(i,j,1), &
                      xb%u(i,j,1), xb%v(i,j,1), &
                      xb%p(i,j,2), xb%t(i,j,2), xb%q(i,j,2), &
                      xb%h(i,j,1), xb%rough(i,j),xb%landmask(i,j), &
                      xb%u10(i,j), xb%v10(i,j), xb%t2(i,j), xb%q2(i,j), &
                      xb%regime(i,j))
      end do
   end do

   if (trace_use) call da_trace_exit("da_add_pbl_and_sfc_info")

end subroutine da_add_pbl_and_sfc_info


