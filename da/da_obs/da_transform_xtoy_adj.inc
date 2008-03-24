subroutine da_transform_xtoy_adj(grid, iv, jo_grad_y, jo_grad_x)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------
   
   implicit none
   
   type (domain),  intent(inout) :: grid
   type (iv_type), intent(inout) :: iv          ! obs. inc vector (o-b).
   type (y_type),  intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type),  intent(inout) :: jo_grad_x   ! grad_x(jo)
   real, allocatable,dimension(:,:,:,:) :: ltmp, gtmp
   integer :: i, j, k, n

   if (trace_use) call da_trace_entry("da_transform_xtoy_adj")
  
   !--------------------------------------------------------------------------
   ! [1.0] observation operator y = H(x):
   !--------------------------------------------------------------------------
  
   if (iv%info(sound)%nlocal > 0) then
      call da_transform_xtoy_sound_adj     (iv, jo_grad_y, jo_grad_x) 
      call da_transform_xtoy_sonde_sfc_adj (grid, iv, jo_grad_y, jo_grad_x) 
   end if

   if (iv%info(synop)%nlocal    > 0) call da_transform_xtoy_synop_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(geoamv)%nlocal   > 0) call da_transform_xtoy_geoamv_adj   (iv, jo_grad_y, jo_grad_x)
   if (iv%info(polaramv)%nlocal > 0) call da_transform_xtoy_polaramv_adj (iv, jo_grad_y, jo_grad_x)   
   if (iv%info(airep)%nlocal    > 0) call da_transform_xtoy_airep_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv%info(metar)%nlocal    > 0) call da_transform_xtoy_metar_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(ships)%nlocal    > 0) call da_transform_xtoy_ships_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(gpspw)%nlocal    > 0) call da_transform_xtoy_gpspw_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(ssmi_tb)%nlocal  > 0) call da_transform_xtoy_ssmi_tb_adj  (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(ssmi_rv)%nlocal  > 0) call da_transform_xtoy_ssmi_rv_adj  (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(pilot)%nlocal    > 0) call da_transform_xtoy_pilot_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv%info(satem)%nlocal    > 0) call da_transform_xtoy_satem_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(ssmt1)%nlocal    > 0) call da_transform_xtoy_ssmt1_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv%info(ssmt2)%nlocal    > 0) call da_transform_xtoy_ssmt2_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv%info(qscat)%nlocal    > 0) call da_transform_xtoy_qscat_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv%info(profiler)%nlocal > 0) call da_transform_xtoy_profiler_adj (iv, jo_grad_y, jo_grad_x)
   if (iv%info(buoy)%nlocal     > 0) call da_transform_xtoy_buoy_adj     (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(gpsref)%nlocal   > 0) call da_transform_xtoy_gpsref_adj   (iv, jo_grad_y, jo_grad_x)
   if (iv%info(radar)%nlocal    > 0) call da_transform_xtoy_radar_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(bogus)%nlocal    > 0) call da_transform_xtoy_bogus_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(airsr)%nlocal    > 0) call da_transform_xtoy_airsr_adj    (iv, jo_grad_y, jo_grad_x)

   if (num_pseudo            > 0) call da_transform_xtoy_pseudo_adj   (iv, jo_grad_y, jo_grad_x)

#if defined(CRTM) || defined(RTTOV)
   if (use_rad) then
      if (rtm_option == rtm_option_rttov) then
         call da_transform_xtoy_rttov_adj (iv, jo_grad_y, jo_grad_x)
      elseif (rtm_option == rtm_option_crtm) then
#ifdef CRTM
         !if (use_crtm_kmatrix) then
         !   call da_transform_xtoy_crtmk_adj (iv, jo_grad_y, jo_grad_x)
         !else if (use_crtm_kmatrix_fast) then
         !   call da_transform_xtoy_crtmk_f_adj (iv, jo_grad_y, jo_grad_x)
         !else
         call da_transform_xtoy_crtm_adj (iv, jo_grad_y, jo_grad_x)
#ifdef DM_PARALLEL
         if( thinning ) then
            if (crtm_cloud) then
               allocate ( ltmp( ids:ide, jds:jde, kts:kte, 8 ) )
               allocate ( gtmp( ids:ide, jds:jde, kts:kte, 8 ) )
               n = 8*(ide-ids+1)*(jde-jds+1)*(kte-kts+1)    
            else
               allocate ( ltmp( ids:ide, jds:jde, kts:kte, 3 ) )
               allocate ( gtmp( ids:ide, jds:jde, kts:kte, 3 ) )
               n = 3*(ide-ids+1)*(jde-jds+1)*(kte-kts+1)    
            endif
            ltmp = 0.0 ; gtmp = 0.0
            do k = kts, kte
               do j = max(jds,jms),  min(jde,jme)
                  do i = max(ids,ims),  min(ide,ime)
                     ltmp(i,j,k,1) = jo_grad_x%t(i,j,k)
                     ltmp(i,j,k,2) = jo_grad_x%q(i,j,k)
                  enddo
               enddo
            enddo
            do j = max(jds,jms),  min(jde,jme)
               do i = max(ids,ims),  min(ide,ime)
                  ltmp(i,j,1,3) = jo_grad_x%psfc(i,j)
               enddo
            enddo
            if (crtm_cloud) then
               do k = kts, kte
                  do j = max(jds,jms),  min(jde,jme)
                     do i = max(ids,ims),  min(ide,ime)
                        ltmp(i,j,k,4) = jo_grad_x%qcw(i,j,k)
                        ltmp(i,j,k,5) = jo_grad_x%qci(i,j,k)
                        ltmp(i,j,k,6) = jo_grad_x%qrn(i,j,k)
                        ltmp(i,j,k,7) = jo_grad_x%qsn(i,j,k)
                        ltmp(i,j,k,8) = jo_grad_x%qgr(i,j,k)
                     enddo
                  enddo
               enddo
            endif
            call mpi_reduce(ltmp, gtmp, n, true_mpi_real, mpi_sum, root, comm, ierr)
            call wrf_dm_bcast_real (gtmp, n )
            do k = kts, kte
               do j = max(jds,jms),  min(jde,jme)
                  do i = max(ids,ims),  min(ide,ime)
                     jo_grad_x%t(i,j,k) = gtmp(i,j,k,1)
                     jo_grad_x%q(i,j,k) = gtmp(i,j,k,2)
                  enddo
               enddo
            enddo
            do j = max(jds,jms),  min(jde,jme)
               do i = max(ids,ims),  min(ide,ime)
                  jo_grad_x%psfc(i,j) = gtmp(i,j,1,3)
               enddo
            enddo
            if (crtm_cloud) then
               do k = kts, kte
                  do j = max(jds,jms),  min(jde,jme)
                     do i = max(ids,ims),  min(ide,ime)
                        jo_grad_x%qcw(i,j,k) = gtmp(i,j,k,4)
                        jo_grad_x%qci(i,j,k) = gtmp(i,j,k,5)
                        jo_grad_x%qrn(i,j,k) = gtmp(i,j,k,6)
                        jo_grad_x%qsn(i,j,k) = gtmp(i,j,k,7)
                        jo_grad_x%qgr(i,j,k) = gtmp(i,j,k,8)
                     enddo
                  enddo
               enddo
            endif
            deallocate (ltmp)
            deallocate (gtmp)
         endif
#endif
         !end if
         !end if
         !end if
#endif
      else
         call da_warning(__FILE__,__LINE__,(/"Unknown radiative transfer model"/))
      end if
   end if
#endif

   if (trace_use) call da_trace_exit("da_transform_xtoy_adj")

end subroutine da_transform_xtoy_adj


