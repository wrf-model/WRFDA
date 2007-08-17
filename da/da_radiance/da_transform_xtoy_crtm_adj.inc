subroutine da_transform_xtoy_crtm_adj ( iv, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! PURPOSE: transform gradient from obs space to model grid space.
   !
   ! METHOD:  jo_grad_x = H^T jo_grad_y =  - H^T R^-1 ( d - H delta_x )
   !           1. input gradient in obs space and reference state of RTTOV
   !           2. call adjoint of RTM
   !           3. adjoint of interpolation from model grid to obs loc
   !
   !  HISTORY: 11/19/2006 - Creation            Zhiquan Liu
   !
   !---------------------------------------------------------------------------

   implicit none

   type (x_type), intent(inout)   :: jo_grad_x ! 
   type (y_type),  intent(in)     :: jo_grad_y ! H' delta_x
   type (ob_type), intent(in)     :: iv        ! O-B structure.

#ifdef CRTM

   integer                        :: k  ! Index dimension.
   integer                        :: num_rad  ! Number of radiance obs
   integer                        :: inst, nchanl, n
   real                           :: q_ad

   ! CRTM local varaibles and types
   integer :: wmo_sensor_id, Error_Status, Allocate_Status
   type (CRTM_RTSolution_type ), allocatable :: RTSolution(:),RTSolution_AD(:)
   type (CRTM_Atmosphere_type )   :: Atmosphere, Atmosphere_AD
   type (CRTM_Surface_type )      :: Surface, Surface_AD
   type (CRTM_GeometryInfo_type ) :: GeometryInfo
   !type(CRTM_Options_type )      :: Options

   type (model_loc_type) :: loc

!---------------------------------------------------------

   if ( iv%num_inst < 1 ) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_crtm_adj")

!----------------------------------------------------------------------------
! CRTM allocation
!
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
    if ( Error_Status /= 0 ) then
       call da_error(__FILE__,__LINE__, &
         (/"Error in allocatting CRTM Atmosphere Structure"/))
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

!-------------------------------------------------------------------------------

   do inst = 1, iv%num_inst                 ! loop for sensor
      if ( iv%instid(inst)%num_rad < 1 ) cycle
      num_rad = iv%instid(inst)%num_rad

  ! CRTM channel information structure
      Error_Status = CRTM_Set_ChannelInfo(Sensor_Descriptor(inst),ChannelInfo)
      if ( Error_Status /= 0) then
         call da_error(__FILE__,__LINE__, &
          (/"Error in calling CRTM_Set_ChannelInfo"/))
      end if
      nchanl    = ChannelInfo%n_channels
 
  ! Allocate forward model solution RTSolution array to number of channels
      allocate( RTSolution( ChannelInfo%n_Channels )   , &
                RTSolution_AD( ChannelInfo%n_Channels ), &
               STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting RTSolution"/))
      END IF
 
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
      if ( Error_Status /= 0 ) then
        call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting CRTM Surface Structure"/))
      end if
 
  ! CRTM Options structure
      !Options%n_channels = nchanl
      !Error_Status = CRTM_Allocate_Options( nchanl,     &  ! Input
      !                             Options)  ! Output
      !if ( Error_Status /= 0 ) then
      !  call da_error(__FILE__,__LINE__, &
      !    (/"Error in allocatting CRTM Options Structure"/))
      !endif

      do n=1,num_rad

      ! [1.0] Extract base state Atmosphere variables
            atmosphere%level_pressure(0) = iv%instid(inst)%pf(0,n)
           do k=1,atmosphere%n_layers
            atmosphere%pressure(k) = iv%instid(inst)%pm(k,n)
            atmosphere%level_pressure(k) = iv%instid(inst)%pf(k,n)
            atmosphere%temperature(k) = iv%instid(inst)%tm(k,n)
            atmosphere%absorber(k,1) = iv%instid(inst)%qm(k,n)
           if (crtm_cloud) then
            atmosphere%cloud(1)%water_content(k)=iv%instid(inst)%qcw(k,n)
            atmosphere%cloud(2)%water_content(k)=iv%instid(inst)%qci(k,n)
            atmosphere%cloud(3)%water_content(k)=iv%instid(inst)%qrn(k,n)
            atmosphere%cloud(4)%water_content(k)=iv%instid(inst)%qsn(k,n)
            atmosphere%cloud(5)%water_content(k)=iv%instid(inst)%qgr(k,n)
            atmosphere%cloud(6)%water_content(k)=0.0
            atmosphere%cloud(1)%effective_radius(k)=10.0
            atmosphere%cloud(2)%effective_radius(k)=200.0
            atmosphere%cloud(3)%effective_radius(k)=200.0
            atmosphere%cloud(4)%effective_radius(k)=200.0
            atmosphere%cloud(5)%effective_radius(k)=200.0
            atmosphere%cloud(6)%effective_radius(k)=200.0
           end if
           end do

      ! [1.1] User-supplied emissivity
          ! Options%emissivity_switch = 1
          ! Options%emissivity(1:Options%n_channels) = &
          !     iv%instid(inst)%emiss(1:Options%n_channels,n)

      ! [1.1] CRTM Surface parameter data
     Surface%Land_Coverage=iv%instid(inst)%land_coverage(n)
     Surface%Water_Coverage=iv%instid(inst)%water_coverage(n)
     Surface%Snow_Coverage=iv%instid(inst)%snow_coverage(n)
     Surface%Ice_Coverage=iv%instid(inst)%ice_coverage(n)

     if (Surface%Land_Coverage > 0.0) then
       Surface%Land_Type=GRASS_SOIL           ! land type (User guide appendix 3)
       Surface%Land_Temperature=iv%instid(inst)%ts(n)      ! K
       Surface%Soil_Moisture_Content=iv%instid(inst)%smois(n)  !0.05    ! volumetric water content (g/cm**3)
       !Surface%Canopy_Water_Content=0.05      ! gravimetric water content
       Surface%Vegetation_Fraction=iv%instid(inst)%vegtyp(n)
       Surface%Soil_Temperature=iv%instid(inst)%tslb(n)
     end if
     if (Surface%Water_Coverage > 0.0) then
       !Surface%Water_Type=SEA_WATER          ! (Currently NOT used)
       Surface%Water_Temperature=iv%instid(inst)%ts(n)     ! K
       Surface%Wind_Speed=sqrt((iv%instid(inst)%u10(n))**2+ &
                               (iv%instid(inst)%v10(n))**2)  ! m/sec
       !surface%Wind_Direction=0.0            ! NOT used
       Surface%Salinity=33.                   ! ppmv
     end if
     if (Surface%Snow_Coverage > 0.0) then
       Surface%Snow_Type=NEW_SNOW             ! User guide appendix 3
       Surface%Snow_Temperature=iv%instid(inst)%ts(n)      ! K
       Surface%Snow_Depth=iv%instid(inst)%snowh(n)         ! mm
       !Surface%Snow_Density=0.2               ! g/cm**3
       !Surface%Snow_Grain_Size=2.0            ! mm
     end if
     if (Surface%Ice_Coverage > 0.0) then
       !Surface%Ice_Type=FRESH_ICE             ! NO Table offered, single example is FRESH_ICE
       Surface%Ice_Temperature=iv%instid(inst)%ts(n)       ! K
       Surface%Ice_Thickness=10.0              ! mm
       !Surface%Ice_Density=0.9                ! g/cm**3
       !Surface%Ice_Roughness=0.0               ! NO Table offered, single example is ZERO
     end if
     Surface%SensorData%n_channels = nchanl
     Surface%SensorData%Sensor_ID  = wmo_sensor_id
     Surface%SensorData%Tb(1:nchanl) = iv%instid(inst)%tb_inv(1:nchanl,n) + &
                                          iv%instid(inst)%tb_xb(1:nchanl,n)
     
      ! -- Copy the adjoint atmosphere structure
      Error_Status = CRTM_Assign_Atmosphere( Atmosphere, Atmosphere_AD )

      if ( Error_Status /= 0 ) then
        call da_error(__FILE__,__LINE__, &
          (/"Error copying Atmosphere_AD structure"/))
      END IF

      ! -- Copy the adjoint surface structure
      Error_Status = CRTM_Assign_Surface( Surface, Surface_AD )

      if ( Error_Status /= 0 ) then
        call da_error(__FILE__,__LINE__, &
          (/"Error copying Surface_AD structure"/))
      END IF

    ! -- Zero the Adjoint outputs 
    ! Important: adjoint variables must be initialized
       call CRTM_Zero_Atmosphere( Atmosphere_AD )
       call CRTM_Zero_Surface( Surface_AD )

      ! [1.2] CRTM GeometryInfo Structure
        GeometryInfo%Sensor_Zenith_Angle=iv%instid(inst)%satzen(n)
        GeometryInfo%Source_Zenith_Angle=iv%instid(inst)%solzen(n)
   !     GeometryInfo%Satellite_Height=830.0
   !     GeometryInfo%Sensor_Scan_Angle=
   !     GeometryInfo%Sensor_Zenith_Angle=
   !     GeometryInfo%Sensor_Scan_Angle=
   !     GeometryInfo%Source_Zenith_Angle=


      ! [1.3] assign tb = R^-1 Re :

         RTSolution_AD(:)%brightness_temperature = jo_grad_y%instid(inst)%tb(:,n)
         RTSolution_AD(:)%radiance = 0.0 ! must assign zero, since each call of AD model will return non-zero value
       !if (n <= 10) write(6,'(15f8.3)') RTSolution_AD(:)%brightness_temperature

      ! [1.4] Call CRTM_AD model

         call da_crtm_ad (nchanl,Atmosphere,   &
                            Surface,      &
                            RTSolution_AD,&
                            GeometryInfo, &
                            ChannelInfo,  &
                            Atmosphere_AD,&
                            Surface_AD,   &
                            RTSolution) !,   &
                            !Options = Options)

         !Error_Status = CRTM_Adjoint(Atmosphere,   &
         !                   Surface,      &
         !                   RTSolution_AD,&
         !                   GeometryInfo, &
         !                   ChannelInfo,  &
         !                   Atmosphere_AD,&
         !                   Surface_AD,   &
         !                   RTSolution) !,   &
         !                   !Options = Options)
         !if ( Error_Status /= 0 ) then
         !     call da_error(__FILE__,__LINE__, &
         !        (/"Error in calling CRTM_Adjoint"/))
         !endif

      ! [1.5] Scale transformation and fill zero for no-control variable

         Atmosphere_AD%Level_Pressure(atmosphere%n_layers) = &
                 Atmosphere_AD%Level_Pressure(atmosphere%n_layers)*0.01 
         Atmosphere_AD%Absorber(:,1) = 1000.0*Atmosphere_AD%Absorber(:,1) ! in g/kg

            !-----------------------------------------------------
            ! [1.6] Get horizontal interpolation weights:
            !-----------------------------------------------------

            loc%i = iv%instid(inst)%loc_i(n)
            loc%j = iv%instid(inst)%loc_j(n)
            loc%dx = iv%instid(inst)%loc_dx(n)
            loc%dy = iv%instid(inst)%loc_dy(n)
            loc%dxm = iv%instid(inst)%loc_dxm(n)
            loc%dym = iv%instid(inst)%loc_dym(n)

            ! [1.7] Adjoint of Interpolate horizontally from ob to grid:

            do k=kts,kte ! from bottem to top
               q_ad = Atmosphere_AD%Absorber(kte-k+1,1)
               if (atmosphere%pressure(kte-k+1) < 75.0) q_ad = 0.0
               call da_interp_lin_2d_adj( jo_grad_x%t(:,:,k), loc, &
                  Atmosphere_AD%Temperature(kte-k+1))

               call da_interp_lin_2d_adj( jo_grad_x%q(:,:,k), loc, q_ad )
            end do

               call da_interp_lin_2d_adj(jo_grad_x% psfc, loc,  &
                  Atmosphere_AD%Level_Pressure(atmosphere%n_layers) )

               !if (n <=10 ) then
               !   write(6,'(15f8.2)') rtsolution(:)%brightness_temperature
               !   write(6,'(e15.5)') jo_grad_x% psfc(i,j)
               !  do k=kts,kte
               !   write(6,'(2e15.5)') jo_grad_x%t(i,j,k), jo_grad_x%q(i,j,k)
               !  end do
               !endif

            Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_AD )
            if ( Error_Status /= 0 ) then
              call da_error(__FILE__,__LINE__, &
                 (/"Error in deallocatting CRTM Atmosphere_AD Structure"/))
            end if

            Error_Status = CRTM_Destroy_Surface(Surface_AD)
            if ( Error_Status /= 0 ) then
              call da_error(__FILE__,__LINE__, &
                 (/"Error in deallocatting CRTM Surface_AD Structure"/))
            end if

         end do       !  end loop for pixels

      !-------------------------------------------------------------------
      ! [2.0] Deallocating CRTM structures
      !-------------------------------------------------------------------
         deallocate( rtsolution, rtsolution_ad, STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
              (/"Error in deallocatting RTSolution"/))
         END IF
                                                                                                                          
         Error_Status = CRTM_Destroy_Surface(Surface)
         if ( Error_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Surface Structure"/))
         end if
                                                                                                                          
   end do        ! end loop for sensor

      !-------------------------------------------------------------------
      ! [3.0] Deallocating CRTM Atmosphere structures
      !-------------------------------------------------------------------
    Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )
    if ( Error_Status /= 0 ) then
       call da_error(__FILE__,__LINE__, &
         (/"Error in deallocatting CRTM Atmosphere Structure"/))
    end if
       
   if (trace_use) call da_trace_exit("da_transform_xtoy_crtm_adj")
#else
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#endif
 
end subroutine da_transform_xtoy_crtm_adj

