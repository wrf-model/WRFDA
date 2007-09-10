subroutine da_transform_xtoy_crtm_adj ( iv, xp, jo_grad_y, jo_grad_x )

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

   IMPLICIT NONE

   TYPE (x_type), INTENT(INOUT)   :: jo_grad_x ! 
   TYPE (xpose_type), INTENT(IN)  :: xp        ! Domain decomposition vars.
   TYPE (y_type),  INTENT(IN)     :: jo_grad_y ! H' delta_x
   TYPE (ob_type), INTENT(IN)     :: iv        ! O-B structure.

   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_rad  ! Number of radiance obs
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
   INTEGER                        :: inst, nchanl, n
   REAL                           :: q_ad

#if defined(CRTM)
   ! CRTM local varaibles and types
   INTEGER :: wmo_sensor_id, Error_Status, Allocate_Status
   TYPE( CRTM_RTSolution_type ), ALLOCATABLE :: RTSolution(:,:),RTSolution_AD(:,:)
   TYPE( CRTM_Atmosphere_type )   :: Atmosphere(1), Atmosphere_AD(1)
   TYPE( CRTM_Surface_type )      :: Surface(1), Surface_AD(1)
   TYPE( CRTM_GeometryInfo_type ) :: GeometryInfo(1)
   !TYPE( CRTM_Options_type )      :: Options
#endif
!---------------------------------------------------------

#if !defined(CRTM)
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#else
   IF ( iv%num_inst < 1 ) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_crtm_adj")

!----------------------------------------------------------------------------
! CRTM allocation
!
! Atmosphere structure
    Atmosphere(1)%n_Layers=(xp%kte-xp%kts)+1   ! number of vertical levels
    Atmosphere(1)%n_Absorbers=2
    Atmosphere(1)%n_Clouds=0
    Atmosphere(1)%n_Aerosols=0
    if (crtm_cloud) Atmosphere(1)%n_Clouds=6
 
    Error_Status = CRTM_Allocate_Atmosphere( Atmosphere(1)%n_Layers, &
                                             Atmosphere(1)%n_Absorbers, &
                                             Atmosphere(1)%n_Clouds, &
                                             Atmosphere(1)%n_Aerosols, &
                                             Atmosphere)
    if ( Error_Status /= 0 ) THEN
       call da_error(__FILE__,__LINE__, &
         (/"Error in allocatting CRTM Atmosphere Structure"/))
    endif
    Atmosphere(1)%Absorber_ID(1)=H2O_ID
    Atmosphere(1)%Absorber_ID(2)=O3_ID
 
    if (crtm_cloud) then
       Atmosphere(1)%Cloud(1)%Type=WATER_CLOUD
       Atmosphere(1)%Cloud(2)%Type=ICE_CLOUD
       Atmosphere(1)%Cloud(3)%Type=RAIN_CLOUD
       Atmosphere(1)%Cloud(4)%Type=SNOW_CLOUD
       Atmosphere(1)%Cloud(5)%Type=GRAUPEL_CLOUD
       Atmosphere(1)%Cloud(6)%Type=HAIL_CLOUD
    end if

!-------------------------------------------------------------------------------

   do inst = 1, iv%num_inst                 ! loop for sensor
      if ( iv%instid(inst)%num_rad < 1 ) cycle
      num_rad = iv%instid(inst)%num_rad

  ! CRTM channel information structure
      ! Error_Status = CRTM_Set_ChannelInfo(Sensor_Descriptor(inst),ChannelInfo)
      ! if ( Error_Status /= 0) then
      !    call da_error(__FILE__,__LINE__, &
      !     (/"Error in calling CRTM_Set_ChannelInfo"/))
      ! endif
      nchanl    = ChannelInfo(inst)%n_channels
 
  ! Allocate forward model solution RTSolution array to number of channels
      ALLOCATE( RTSolution( ChannelInfo(inst)%n_Channels, 1 )   , &
                RTSolution_AD( ChannelInfo(inst)%n_Channels, 1 ), &
               STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
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
      endif
 
      Error_Status = CRTM_Allocate_Surface( nchanl,     &  ! Input
                                   Surface)  ! Output
      if ( Error_Status /= 0 ) THEN
        call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting CRTM Surface Structure"/))
      endif
 
  ! CRTM Options structure
      !Options%n_channels = nchanl
      !Error_Status = CRTM_Allocate_Options( nchanl,     &  ! Input
      !                             Options)  ! Output
      !if ( Error_Status /= 0 ) THEN
      !  call da_error(__FILE__,__LINE__, &
      !    (/"Error in allocatting CRTM Options Structure"/))
      !endif

      do n=1,num_rad

      ! [1.0] Extract base state Atmosphere variables
            atmosphere(1)%level_pressure(0) = iv%instid(inst)%pf(0,n)
           do k=1,atmosphere(1)%n_layers
            atmosphere(1)%pressure(k) = iv%instid(inst)%pm(k,n)
            atmosphere(1)%level_pressure(k) = iv%instid(inst)%pf(k,n)
            atmosphere(1)%temperature(k) = iv%instid(inst)%tm(k,n)
            atmosphere(1)%absorber(k,1) = iv%instid(inst)%qm(k,n)
           if (crtm_cloud) then
            atmosphere(1)%cloud(1)%water_content(k)=iv%instid(inst)%qcw(k,n)
            atmosphere(1)%cloud(2)%water_content(k)=iv%instid(inst)%qci(k,n)
            atmosphere(1)%cloud(3)%water_content(k)=iv%instid(inst)%qrn(k,n)
            atmosphere(1)%cloud(4)%water_content(k)=iv%instid(inst)%qsn(k,n)
            atmosphere(1)%cloud(5)%water_content(k)=iv%instid(inst)%qgr(k,n)
            atmosphere(1)%cloud(6)%water_content(k)=0.
            atmosphere(1)%cloud(1)%effective_radius(k)=10.
            atmosphere(1)%cloud(2)%effective_radius(k)=200.
            atmosphere(1)%cloud(3)%effective_radius(k)=200.
            atmosphere(1)%cloud(4)%effective_radius(k)=200.
            atmosphere(1)%cloud(5)%effective_radius(k)=200.
            atmosphere(1)%cloud(6)%effective_radius(k)=200.
           end if
           enddo

      ! [1.1] User-supplied emissivity
          ! Options%emissivity_switch = 1
          ! Options%emissivity(1:Options%n_channels) = &
          !     iv%instid(inst)%emiss(1:Options%n_channels,n)

      ! [1.1] CRTM Surface parameter data
     Surface(1)%Land_Coverage=iv%instid(inst)%land_coverage(n)
     Surface(1)%Water_Coverage=iv%instid(inst)%water_coverage(n)
     Surface(1)%Snow_Coverage=iv%instid(inst)%snow_coverage(n)
     Surface(1)%Ice_Coverage=iv%instid(inst)%ice_coverage(n)

     if (Surface(1)%Land_Coverage > 0.) then
       Surface(1)%Land_Type=GRASS_SOIL           ! land type (User guide appendix 3)
       Surface(1)%Land_Temperature=iv%instid(inst)%ts(n)      ! K
       Surface(1)%Soil_Moisture_Content=iv%instid(inst)%smois(n)  !0.05    ! volumetric water content (g/cm**3)
       !Surface(1)%Canopy_Water_Content=0.05      ! gravimetric water content
       Surface(1)%Vegetation_Fraction=iv%instid(inst)%vegtyp(n)
       Surface(1)%Soil_Temperature=iv%instid(inst)%tslb(n)
     endif
     if (Surface(1)%Water_Coverage > 0.) then
       !Surface(1)%Water_Type=SEA_WATER          ! (Currently NOT used)
       Surface(1)%Water_Temperature=iv%instid(inst)%ts(n)     ! K
       Surface(1)%Wind_Speed=sqrt((iv%instid(inst)%u10(n))**2+ &
                               (iv%instid(inst)%v10(n))**2)  ! m/sec
       !surface(1)%Wind_Direction=0.0            ! NOT used
       Surface(1)%Salinity=33.                   ! ppmv
     endif
     if (Surface(1)%Snow_Coverage > 0.) then
       Surface(1)%Snow_Type=NEW_SNOW             ! User guide appendix 3
       Surface(1)%Snow_Temperature=iv%instid(inst)%ts(n)      ! K
       Surface(1)%Snow_Depth=iv%instid(inst)%snowh(n)         ! mm
       !Surface(1)%Snow_Density=0.2               ! g/cm**3
       !Surface(1)%Snow_Grain_Size=2.0            ! mm
     endif
     if (Surface(1)%Ice_Coverage > 0.) then
       !Surface(1)%Ice_Type=FRESH_ICE             ! NO Table offered, single example is FRESH_ICE
       Surface(1)%Ice_Temperature=iv%instid(inst)%ts(n)       ! K
       Surface(1)%Ice_Thickness=10.              ! mm
       !Surface(1)%Ice_Density=0.9                ! g/cm**3
       !Surface(1)%Ice_Roughness=0.               ! NO Table offered, single example is ZERO
     endif
     Surface(1)%SensorData%n_channels = nchanl
     Surface(1)%SensorData%Sensor_ID  = wmo_sensor_id
     Surface(1)%SensorData%Tb(1:nchanl) = iv%instid(inst)%tb_inv(1:nchanl,n) + &
                                          iv%instid(inst)%tb_xb(1:nchanl,n)
     
      ! -- Copy the adjoint atmosphere structure
      Error_Status = CRTM_Assign_Atmosphere( Atmosphere, Atmosphere_AD )

      IF ( Error_Status /= 0 ) THEN
        call da_error(__FILE__,__LINE__, &
          (/"Error copying Atmosphere_AD structure"/))
      END IF

      ! -- Copy the adjoint surface structure
      Error_Status = CRTM_Assign_Surface( Surface, Surface_AD )

      IF ( Error_Status /= 0 ) THEN
        call da_error(__FILE__,__LINE__, &
          (/"Error copying Surface_AD structure"/))
      END IF

    ! -- Zero the Adjoint outputs 
    ! Important: adjoint variables must be initialized
       CALL CRTM_Zero_Atmosphere( Atmosphere_AD )
       CALL CRTM_Zero_Surface( Surface_AD )

      ! [1.2] CRTM GeometryInfo Structure
        GeometryInfo(1)%Sensor_Zenith_Angle=iv%instid(inst)%satzen(n)
        GeometryInfo(1)%Source_Zenith_Angle=iv%instid(inst)%solzen(n)
   !     GeometryInfo(1)%Satellite_Height=830.
   !     GeometryInfo(1)%Sensor_Scan_Angle=
   !     GeometryInfo(1)%Sensor_Zenith_Angle=
   !     GeometryInfo(1)%Sensor_Scan_Angle=
   !     GeometryInfo(1)%Source_Zenith_Angle=


      ! [1.3] assign tb = R^-1 Re :

         RTSolution_AD(:,1)%brightness_temperature = jo_grad_y%instid(inst)%tb(:,n)
         RTSolution_AD(:,1)%radiance = 0. ! must assign zero, since each call of AD model will return non-zero value
       !if (n <= 10) write(6,'(15f8.3)') RTSolution_AD(:,1)%brightness_temperature

      ! [1.4] Call CRTM_AD model

         call da_crtm_ad (1, nchanl, 1, Atmosphere,   &
                            Surface,      &
                            RTSolution_AD,&
                            GeometryInfo, &
                            ChannelInfo(inst:inst),  &
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
         !if ( Error_Status /= 0 ) THEN
         !     call da_error(__FILE__,__LINE__, &
         !        (/"Error in calling CRTM_Adjoint"/))
         !endif

      ! [1.5] Scale transformation and fill zero for no-control variable

         Atmosphere_AD(1)%Level_Pressure(atmosphere(1)%n_layers) = &
                 Atmosphere_AD(1)%Level_Pressure(atmosphere(1)%n_layers)*0.01 
         Atmosphere_AD(1)%Absorber(:,1) = 1000.*Atmosphere_AD(1)%Absorber(:,1) ! in g/kg

            !-----------------------------------------------------
            ! [1.6] Get horizontal interpolation weights:
            !-----------------------------------------------------

            i = iv%instid(inst)%loc_i(n)
            j = iv%instid(inst)%loc_j(n)
            dx = iv%instid(inst)%loc_dx(n)
            dy = iv%instid(inst)%loc_dy(n)
            dxm = iv%instid(inst)%loc_dxm(n)
            dym = iv%instid(inst)%loc_dym(n)

            ! [1.7] Adjoint of Interpolate horizontally from ob to grid:

            do k=xp%kts,xp%kte ! from bottem to top
               q_ad = Atmosphere_AD(1)%Absorber(xp%kte-k+1,1)
               if (atmosphere(1)%pressure(xp%kte-k+1) < 75.) q_ad = 0.
               call da_interp_lin_2d_adj( jo_grad_x%t(:,:,k), xp%ims, xp%ime, xp%jms, &
                                          xp%jme, i, j, dx, dy, dxm, dym, &
                                          Atmosphere_AD(1)%Temperature(xp%kte-k+1))

               call da_interp_lin_2d_adj( jo_grad_x%q(:,:,k), xp%ims, xp%ime, xp%jms, & 
                                           xp%jme, i, j, dx, dy, dxm, dym, &
                                           q_ad )
            enddo

               call da_interp_lin_2d_adj(jo_grad_x% psfc, xp%ims, xp%ime, xp%jms, &
                                         xp%jme, i, j, dx, dy, dxm, dym,  &
                                         Atmosphere_AD(1)%Level_Pressure(atmosphere(1)%n_layers) )

               !if (n <=10 ) then
               !   write(6,'(15f8.2)') rtsolution(:)%brightness_temperature
               !   write(6,'(e15.5)') jo_grad_x% psfc(i,j)
               !  do k=xp%kts,xp%kte
               !   write(6,'(2e15.5)') jo_grad_x%t(i,j,k), jo_grad_x%q(i,j,k)
               !  enddo
               !endif

            Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_AD )
            if ( Error_Status /= 0 ) THEN
              call da_error(__FILE__,__LINE__, &
                 (/"Error in deallocatting CRTM Atmosphere_AD Structure"/))
            endif

            Error_Status = CRTM_Destroy_Surface(Surface_AD)
            if ( Error_Status /= 0 ) THEN
              call da_error(__FILE__,__LINE__, &
                 (/"Error in deallocatting CRTM Surface_AD Structure"/))
            endif

         end do       !  end loop for pixels

      !-------------------------------------------------------------------
      ! [2.0] Deallocating CRTM structures
      !-------------------------------------------------------------------
         deallocate( rtsolution, rtsolution_ad, STAT = Allocate_Status )
         IF ( Allocate_Status /= 0 ) THEN
            call da_error(__FILE__,__LINE__, &
              (/"Error in deallocatting RTSolution"/))
         END IF
                                                                                                                          
         Error_Status = CRTM_Destroy_Surface(Surface)
         if ( Error_Status /= 0 ) THEN
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Surface Structure"/))
         endif
                                                                                                                          
   end do        ! end loop for sensor

      !-------------------------------------------------------------------
      ! [3.0] Deallocating CRTM Atmosphere structures
      !-------------------------------------------------------------------
    Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )
    if ( Error_Status /= 0 ) THEN
       call da_error(__FILE__,__LINE__, &
         (/"Error in deallocatting CRTM Atmosphere Structure"/))
    endif
       
   if (trace_use) call da_trace_exit("da_transform_xtoy_crtm_adj")
#endif
 
end subroutine da_transform_xtoy_crtm_adj

