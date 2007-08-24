subroutine da_get_innov_vector_crtm ( it, xb, xp, ob, iv )

   !---------------------------------------------------------------------------
   !  PURPOSE: Calculate innovation vector for radiance data.
   !
   !  METHOD:  d = y - H(x)
   !       1. interpolate xb to obs location
   !       2. call foreward RTM to get simulated bright temperature 
   !       3. obs BT - simulated BT
   !---------------------------------------------------------------------------

   implicit none
   
   integer,           intent(in)    :: it       ! External iteration.
   type (xb_type),    intent(in)    :: xb       ! first guess state.
   type (xpose_type), intent(in)    :: xp       ! Domain decomposition vars.
   type (y_type),     intent(inout) :: ob       ! Observation structure.
   type (iv_type),    intent(inout) :: iv       ! O-B structure.

#ifdef CRTM

   integer :: n, icld  ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: num_levs ! Number of obs levels.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   integer :: alloc_status(40)

   real, allocatable :: model_u10(:)
   real, allocatable :: model_v10(:)
   real, allocatable :: model_psfc(:)
   real, allocatable :: model_ts(:)
   real, allocatable :: model_elv(:)
   real, allocatable :: model_smois(:)
   real, allocatable :: model_tslb(:)
   real, allocatable :: model_snowh(:)
   real, allocatable :: model_vegfra(:)
   real, allocatable :: model_qcw(:,:)
   real :: model_ptop
   real    :: model_isltyp, model_ivgtyp
   integer :: model_isflg

   real    :: v_p(kms:kme)  ! Model value p at ob hor. location.

   ! real    :: model_tm(kms:kme)

   integer :: inst, nchanl

   ! variables for computing clwp
   real    :: clw(kms:kme), dpf(kms:kme)
   real    :: clwp

   ! CRTM local variables and types
   integer :: wmo_sensor_id,Error_Status, Allocate_Status

   type (CRTM_RTSolution_type), allocatable :: RTSolution(:)
   type (CRTM_Atmosphere_type)   :: Atmosphere
   type (CRTM_Surface_type)      :: Surface
   type (CRTM_GeometryInfo_type) :: GeometryInfo

   ! WHY? use argument it
   if (it==0) then; write(unit=stdout,fmt='(A)') "WHY? have argument it to"//__FILE__; end if

   alloc_status (:) = 0

   if (trace_use) call da_trace_entry("da_get_innov_vector_crtm")

   ! CRTM allocation

   ! Atmosphere structure
   Atmosphere%n_Layers=(kte-kts)+1   ! number of vertical levels
   Atmosphere%n_Absorbers=2
   Atmosphere%n_Clouds=0
   Atmosphere%n_Aerosols=0
   if (crtm_cloud) Atmosphere%n_Clouds=6
  
   Error_Status = CRTM_Allocate_Atmosphere( Atmosphere%n_Layers, &
                                            Atmosphere%n_Absorbers, &
                                            Atmosphere%n_Clouds, &
                                            Atmosphere%n_Aerosols, &
                                            Atmosphere)
   if (Error_Status /= 0) then 
       call da_error(__FILE__,__LINE__, &
         (/"Error in allocating CRTM Atmosphere Structure"/))
   end if

   Atmosphere%Absorber_ID(1)=H2O_ID
   Atmosphere%Absorber_ID(2)=O3_ID

   if (crtm_cloud) then
      Atmosphere%Cloud(1)%Type=WATER_CLOUD
      Atmosphere%Cloud(2)%Type=ICE_CLOUD
      Atmosphere%Cloud(3)%Type=RAIN_CLOUD
      Atmosphere%Cloud(4)%Type=SNOW_CLOUD
      Atmosphere%Cloud(5)%Type=GRAUPEL_CLOUD
      Atmosphere%Cloud(6)%Type=HAIL_CLOUD
   end if

   !------------------------------------------------------
   ! [1.0] calculate the background bright temperature
   !-------------------------------------------------------
   do inst = 1, iv%num_inst                 ! loop for sensor
      ! if ( iv%instid(inst)%num_rad < 1 ) cycle
      if (iv%plocal_rad(iv%time,inst) <= iv%plocal_rad(iv%time-1,inst) ) cycle
      num_levs  = kte-kts+1 
      ! CRTM channel information structure
      Error_Status = CRTM_Set_ChannelInfo(Sensor_Descriptor(inst),ChannelInfo)
      if (Error_Status /= 0) then
         call da_error(__FILE__,__LINE__, (/"Error in calling CRTM_Set_ChannelInfo"/))
      end if
      nchanl    = ChannelInfo%n_channels

      ! Allocate forward model solution RTSolution array to number of channels
      allocate (RTSolution(ChannelInfo%n_Channels), STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, (/"Error in allocating RTSolution"/))
      end if
             
      ! CRTM Surface Structure
      if (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsua') then
         wmo_sensor_id=WMO_AMSUA
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsub') then
         wmo_sensor_id=WMO_AMSUB
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsre') then
         wmo_sensor_id=WMO_AMSRE
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='ssmi') then
         wmo_sensor_id=WMO_SSMI
      else
         wmo_sensor_id=INVALID_WMO_SENSOR_ID
      end if

      Error_Status = CRTM_Allocate_Surface( nchanl,     &  ! Input
                                   Surface)  ! Output
      if (Error_Status /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Error in allocating CRTM Surface Structure Structure"/))
      end if

      allocate (model_ts    (iv%plocal_rad(iv%time-1,inst)+1: iv%plocal_rad(iv%time,inst)))
      allocate (model_u10   (iv%plocal_rad(iv%time-1,inst)+1: iv%plocal_rad(iv%time,inst)))
      allocate (model_v10   (iv%plocal_rad(iv%time-1,inst)+1: iv%plocal_rad(iv%time,inst)))
      allocate (model_psfc  (iv%plocal_rad(iv%time-1,inst)+1: iv%plocal_rad(iv%time,inst)))
      allocate (model_elv   (iv%plocal_rad(iv%time-1,inst)+1: iv%plocal_rad(iv%time,inst)))
      allocate (model_smois (iv%plocal_rad(iv%time-1,inst)+1: iv%plocal_rad(iv%time,inst)))
      allocate (model_tslb  (iv%plocal_rad(iv%time-1,inst)+1: iv%plocal_rad(iv%time,inst)))
      allocate (model_snowh (iv%plocal_rad(iv%time-1,inst)+1: iv%plocal_rad(iv%time,inst)))
      allocate (model_vegfra(iv%plocal_rad(iv%time-1,inst)+1: iv%plocal_rad(iv%time,inst)))
      allocate (model_qcw   (kms:kme,iv%plocal_rad(iv%time-1,inst)+1: iv%plocal_rad(iv%time,inst)))

      do n= iv%plocal_rad(iv%time-1,inst)+1, iv%plocal_rad(iv%time,inst)
         ! if ( n > iv%instid(inst)%num_rad ) exit
         model_ts(n)   = 0.0
         model_u10(n)  = 0.0
         model_v10(n)  = 0.0
         model_psfc(n) = 0.0

         ! [1.1] Get horizontal interpolation weights:

         i = iv%instid(inst)%new_loc%i(1,n)
         j = iv%instid(inst)%new_loc%j(1,n)
         dx = iv%instid(inst)%new_loc%dx(1,n)
         dy = iv%instid(inst)%new_loc%dy(1,n)
         dxm = iv%instid(inst)%new_loc%dxm(1,n)
         dym = iv%instid(inst)%new_loc%dym(1,n)
         ! horizontal interpolate xb pressure to ob position for every xb layer
         ! get CRTM pressure Layers 
         do k=kts,kte ! from bottem to top
            v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
            v_p(k) = 0.01*v_p(k)  ! convert Pa to hPa
            Atmosphere%Pressure(kte-k+1)=v_p(k) ! from top to bottom
         end do

         ! [1.2] Interpolate horizontally to ob:
         do k=kts,kte ! from bottem to top
            call da_interp_lin_2d_newer (xb%t(:,:,k), iv%instid(inst)%new_loc,k,n,n,Atmosphere%Temperature(kte-k+1:kte-k+1))
            call da_interp_lin_2d_newer (xb%q(:,:,k), iv%instid(inst)%new_loc,k,n,n,Atmosphere%Absorber(kte-k+1:kte-k+1,1)) 
            Atmosphere%Absorber(kte-k+1,1) = 1000.0*Atmosphere%Absorber(kte-k+1,1) ! in g/kg
            ! NOTE: WRF high-level q values seems too big, replaced by constants
            if (v_p(k) < 75.0) Atmosphere%Absorber(kte-k+1,1) = 0.001

            call da_interp_lin_2d_newer (xb%qcw(:,:,k), iv%instid(inst)%new_loc,k,n,n, model_qcw(kte-k+1,n:n))
            
            if (crtm_cloud) then
               Atmosphere%Cloud(1)%Effective_Radius(kte-k+1)=10.0
               Atmosphere%Cloud(1)%Water_Content(kte-k+1) = model_qcw(kte-k+1,n)

               Atmosphere%Cloud(2)%Effective_Radius(kte-k+1)=200.0
               call da_interp_lin_2d_newer (xb%qci(:,:,k), iv%instid(inst)%new_loc,k,n,n, &
                  Atmosphere%Cloud(2)%Water_Content(kte-k+1:kte-k+1) )

               Atmosphere%Cloud(3)%Effective_Radius(kte-k+1)=200.0
               call da_interp_lin_2d_newer (xb%qrn(:,:,k), iv%instid(inst)%new_loc,k,n,n, &
                  Atmosphere%Cloud(3)%Water_Content(kte-k+1:kte-k+1) )

               Atmosphere%Cloud(4)%Effective_Radius(kte-k+1)=200.0
               call da_interp_lin_2d_newer (xb%qsn(:,:,k), iv%instid(inst)%new_loc, k,n,n, &
                  Atmosphere%Cloud(4)%Water_Content(kte-k+1:kte-k+1) )

               Atmosphere%Cloud(5)%Effective_Radius(kte-k+1)=200.0
               call da_interp_lin_2d_newer (xb%qgr(:,:,k), iv%instid(inst)%new_loc, k,n,n, &
                  Atmosphere%Cloud(5)%Water_Content(kte-k+1:kte-k+1) )

               Atmosphere%Cloud(6)%Effective_Radius(kte-k+1)=200.0
               Atmosphere%Cloud(6)%Water_Content(kte-k+1) = 0.0
            end if
         end do

         ! determine surface type of obs location
         !-----------------------------------------
         call da_detsurtyp ( xb%snow, xb%xice, xb%landmask,  &
            xb%ivgtyp, xb%isltyp, &
            ims, ime, jms, jme, &
            i, j, dx, dy, dxm, dym, &
            model_isflg,model_ivgtyp, model_isltyp, &
            Surface%Water_Coverage, Surface%Ice_Coverage, &
            Surface%Land_Coverage, Surface%Snow_Coverage )

         call da_interp_lin_2d_newer (xb % u10,  iv%instid(inst)%new_loc, 1, n, n, model_u10(n:n))
         call da_interp_lin_2d_newer (xb % v10,  iv%instid(inst)%new_loc, 1, n, n, model_v10(n:n))
         call da_interp_lin_2d_newer (xb % psfc, iv%instid(inst)%new_loc, 1, n, n, model_psfc(n:n))

         model_psfc(n) = 0.01*model_psfc(n)           ! convert to hPa
         model_ptop = 0.01*xb%ptop

         ! get CRTM levels (0.005hPa at top) /model full level
         Atmosphere%Level_Pressure(0)=model_ptop   ! to sigma level 51-->sigmaf=0
         Atmosphere%Level_Pressure(Atmosphere%n_Layers)=model_psfc(n) ! to sigma level 1->sigmaf=1
         do k=kts+1,kte
            Atmosphere%Level_Pressure(kte-k+1)= xb%sigmaf(k)*(model_psfc(n)-model_ptop)+model_ptop
         end do
 
         ! convert cloud content unit from kg/kg to kg/m^2        
         if (crtm_cloud) then
            do k=kts,kte
               do icld=1,Atmosphere%n_Clouds
                  Atmosphere%Cloud(icld)%Water_Content(k)= Atmosphere%Cloud(icld)%Water_Content(k)* &
                     (Atmosphere%Level_Pressure(k)- Atmosphere%Level_Pressure(k-1))*100.0/gravity 
               end do
            end do
         end if

         if ( model_isflg == 0 ) then   ! over sea using SST
            call da_interp_lin_2d_newer (xb % tgrn, iv%instid(inst)%new_loc, 1, n, n, model_ts(n:n))
         else
            call da_interp_lin_2d_newer (xb % tsk,  iv%instid(inst)%new_loc, 1, n, n, model_ts(n:n))
         end if

         call da_interp_lin_2d_newer (xb % terr, iv%instid(inst)%new_loc, 1, n, n, model_elv(n:n))

         ! variables for emissivity calculations
         !---------------------------------------- 
         call da_interp_lin_2d_newer (xb % smois,  iv%instid(inst)%new_loc, 1, n, n, model_smois(n:n) )
         call da_interp_lin_2d_newer (xb % tslb,   iv%instid(inst)%new_loc, 1, n, n, model_tslb(n:n) )
         call da_interp_lin_2d_newer (xb % snowh,  iv%instid(inst)%new_loc, 1, n, n, model_snowh(n:n) )
         call da_interp_lin_2d_newer (xb % vegfra, iv%instid(inst)%new_loc, 1, n, n, model_vegfra(n:n) )

         ! model_snowh(n) = model_snowh(n)*100.0   ! convert from m to mm
         model_vegfra(n) = 0.01*model_vegfra(n)  ! convert range to 0~1

         ! ADD for computing cloud liquid water path (mm) from guess
         clwp = 0.0
         do k = kts,kte ! from top to bottom
            dpf(k) = 100.0*(atmosphere%level_pressure(k) - atmosphere%level_pressure(k-1))
            clw(k) = model_qcw(k,n)*dpf(k)/gravity ! kg/m2 or mm
            if (atmosphere%pressure(k)<100.0) clw(k) = 0.0
            clwp  = clwp + clw(k)
         end do

         ! CRTM GeometryInfo Structure
         GeometryInfo%Sensor_Zenith_Angle=iv%instid(inst)%satzen(n)
         GeometryInfo%Source_Zenith_Angle=iv%instid(inst)%solzen(n)
         ! GeometryInfo%Satellite_Height=830.0
         ! GeometryInfo%Sensor_Scan_Angle=
         ! GeometryInfo%Sensor_Zenith_Angle=
         ! GeometryInfo%Sensor_Scan_Angle=
         ! GeometryInfo%Source_Zenith_Angle=

         ! CRTM Surface parameter data

         if (Surface%Land_Coverage > 0.0) then
            Surface%Land_Type=GRASS_SOIL           ! land type (User guide appendix 3)
            Surface%Land_Temperature=model_ts(n)      ! K
            Surface%Soil_Moisture_Content= model_smois(n) !0.05    ! volumetric water content (g/cm**3)
            ! Surface%Canopy_Water_Content=0.05      ! gravimetric water content
            Surface%Vegetation_Fraction=model_vegfra(n)
            Surface%Soil_Temperature=model_tslb(n)
         end if
         if (Surface%Water_Coverage > 0.0) then
            ! Surface%Water_Type=SEA_WATER          ! (Currently NOT used)
            Surface%Water_Temperature=model_ts(n)     ! K
            Surface%Wind_Speed=sqrt(model_u10(n)**2+model_v10(n)**2)  ! m/sec
            ! surface%Wind_Direction=0.0            ! NOT used
            Surface%Salinity=33.                   ! ppmv
         end if
         if (Surface%Snow_Coverage > 0.0) then
            Surface%Snow_Type=NEW_SNOW             ! User guide appendix 3
            Surface%Snow_Temperature=model_ts(n)      ! K
            Surface%Snow_Depth=model_snowh(n)         ! mm
            ! Surface%Snow_Density=0.2               ! g/cm**3
            ! Surface%Snow_Grain_Size=2.0            ! mm
         end if
         if (Surface%Ice_Coverage > 0.0) then
            ! Surface%Ice_Type=FRESH_ICE             ! NO Table offered, single example is FRESH_ICE
            Surface%Ice_Temperature=model_ts(n)       ! K
            Surface%Ice_Thickness=10.0              ! mm
            ! Surface%Ice_Density=0.9                ! g/cm**3
            ! Surface%Ice_Roughness=0.0               ! NO Table offered, single example is ZERO
         end if
         if (nchanl > 0) then
            Surface%SensorData%n_channels = nchanl
            Surface%SensorData%Sensor_ID  = wmo_sensor_id 
            Surface%SensorData%Tb(1:nchanl) = ob%instid(inst)%tb(1:nchanl,n)
         end if

         ! [1.3] Call RTM foreward model
         call da_crtm_direct(nchanl, Atmosphere,   &
            Surface,      &
            GeometryInfo, &
            ChannelInfo,  &
            RTSolution)

         !Error_Status = CRTM_Forward (Atmosphere,   &
         !   Surface,      &
         !   GeometryInfo, &
         !   ChannelInfo,  &
         !   RTSolution)
         !if (n<=10) then
         !write(6,'(f10.3)') Atmosphere%level_pressure(0)
         !do k=1,Atmosphere%n_layers
         !  write(6,'(4f10.3)') Atmosphere%Level_pressure(k),Atmosphere%pressure(k), &
         !                      Atmosphere%temperature(k),Atmosphere%absorber(k,1)
         !enddo
         !  write(6,'(15f8.3)') RTSolution(1:nchanl)%Brightness_Temperature
         !  write(6,'(15f8.3)') RTSolution(1:nchanl)%surface_emissivity
         !end if

         !if ( Error_Status /= 0 ) then
         !     call da_error(__FILE__,__LINE__, &
         !        (/"Error in calling CRTM_Forward"/))
         !endif

         !----------------------------------------------------------------
         ! [2.0] calculate components of innovation vector:
         !----------------------------------------------------------------
         do k = 1, nchanl
            if ( iv%instid(inst)%tb_inv(k,n) > missing_r ) then 
               iv%instid(inst)%tb_xb(k,n)  = rtsolution(k)%Brightness_Temperature
               iv%instid(inst)%tb_inv(k,n) = &
               ob%instid(inst)%tb(k,n) - rtsolution(k)%Brightness_Temperature
            else
               iv%instid(inst)%tb_xb(k,n)    = rtsolution(k)%Brightness_Temperature
               iv%instid(inst)%tb_inv(k,n)   = missing_r
            end if
            iv%instid(inst)%emiss(k,n) = rtsolution(k)%surface_emissivity
         end do

         !----------------------------------------------------------------
         ! [3.0] store base state to innovation structure
         !----------------------------------------------------------------
         ! full level pressures
         iv%instid(inst)%pf(0,n)  = atmosphere%level_pressure(0)
         do k=1,atmosphere%n_layers
            iv%instid(inst)%pm(k,n)  = atmosphere%pressure(k)
            iv%instid(inst)%pf(k,n)  = atmosphere%level_pressure(k)
            iv%instid(inst)%tm(k,n)  = atmosphere%temperature(k)
            iv%instid(inst)%qm(k,n)  = atmosphere%absorber(k,1)
            if (crtm_cloud) then
               iv%instid(inst)%qcw(k,n) = atmosphere%cloud(1)%water_content(k)
               iv%instid(inst)%qci(k,n) = atmosphere%cloud(2)%water_content(k)
               iv%instid(inst)%qrn(k,n) = atmosphere%cloud(3)%water_content(k)
               iv%instid(inst)%qsn(k,n) = atmosphere%cloud(4)%water_content(k)
               iv%instid(inst)%qgr(k,n) = atmosphere%cloud(5)%water_content(k)
            end if
         end do
         iv%instid(inst)%u10(n)       = model_u10(n)
         iv%instid(inst)%v10(n)       = model_v10(n)
         iv%instid(inst)%t2m(n)       = 0.01*missing_r !model_t2m
         iv%instid(inst)%mr2m(n)      = 0.01*missing_r !model_mr2m
         iv%instid(inst)%ps(n)        = model_psfc(n)
         iv%instid(inst)%ts(n)        = model_ts(n)
         iv%instid(inst)%smois(n)     = model_smois(n)
         iv%instid(inst)%tslb(n)      = model_tslb(n)
         iv%instid(inst)%snowh(n)     = model_snowh(n)
         iv%instid(inst)%isflg(n)     = model_isflg
         iv%instid(inst)%elevation(n) = model_elv(n)
         iv%instid(inst)%soiltyp(n)   = model_isltyp
         iv%instid(inst)%vegtyp(n)    = model_ivgtyp
         iv%instid(inst)%vegfra(n)    = model_vegfra(n)
         iv%instid(inst)%clwp(n)      = clwp
         iv%instid(inst)%water_coverage(n) = surface%water_coverage
         iv%instid(inst)%land_coverage(n)  = surface%land_coverage
         iv%instid(inst)%ice_coverage(n)   = surface%ice_coverage                              
         iv%instid(inst)%snow_coverage(n)  = surface%snow_coverage
      end do       !  end loop for pixels

      deallocate( rtsolution, STAT = Allocate_Status )
      if (Allocate_Status /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Error in deallocating RTSolution"/))
      end if

      Error_Status = CRTM_Destroy_Surface(Surface)
      if (Error_Status /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Error in deallocating CRTM Surface Structure"/))
      end if

      deallocate (model_ts)
      deallocate (model_u10)
      deallocate (model_v10)
      deallocate (model_psfc)
      deallocate (model_elv)
      deallocate (model_smois)
      deallocate (model_tslb)
      deallocate (model_snowh)
      deallocate (model_vegfra)
      deallocate (model_qcw)
   end do        ! end loop for sensor

   !------------------------------------------
   ! 4.0 perfoming bias correction files
   !------------------------------------------

   if (biascorr) then
      do inst = 1, iv%num_inst
         write(unit=stdout,fmt='(A,A)') 'Performing bias correction for ', &
            trim(iv%instid(inst)%rttovid_string)
         call da_biascorr(inst,ob,iv)
      end do
      write(unit=stdout,fmt='(A)') " "
   end if

   !------------------------------------------------------------------------
   ! [5.0] Perform QC check
   !------------------------------------------------------------------------
   if (qc_rad) then
      call da_qc_crtm(ob, iv)
   end if
   !------------------------------------------
   ! 6.0 prepare bias statistics files
   !------------------------------------------

   if (biasprep) then
      do inst = 1, iv%num_inst
         write(unit=stdout,fmt='(a,a)') 'Preparing bias statistics files for ', &
            trim(iv%instid(inst)%rttovid_string)
         call da_biasprep(inst,ob,iv)
      end do
      write(unit=stdout,fmt='(A)') " "
   end if

   Error_Status = CRTM_Destroy_Atmosphere (Atmosphere)
   if (Error_Status /= 0) then
       call da_error(__FILE__,__LINE__, &
         (/"Error in deallocating CRTM Atmosphere Structure"/))
   end if   

   if (trace_use) call da_trace_exit("da_get_innov_vector_crtm")
#else
   call da_error(__FILE__,__LINE__, &
      (/"Must compile with $CRTM option for radiances"/))
#endif
 
end subroutine da_get_innov_vector_crtm

