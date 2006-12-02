subroutine da_transform_xtoy_rad_crtm ( xa, iv, xp, y )

   !---------------------------------------------------------------------------
   !  PURPOSE: transform from analysis increment to 
   !                          pertubation radiance.
   !
   !  METHOD:  delta_y = H delta_x
   !           1. input reference state of CRTM_TL
   !           2. interpolate analysis increment to obs location
   !           3. Call CRTM_TL
   !
   !  HISTORY: 11/16/2006 - Creation            Zhiquan Liu
   !
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (x_type), INTENT(IN)      :: xa       ! delta_x
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (y_type),  INTENT(INOUT)  :: y        ! H' delta_x
   TYPE (ob_type), INTENT(IN)     :: iv       ! O-B structure.

   INTEGER                        :: i, j, k  ! Index dimension.
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.

   INTEGER            :: inst, num_rad, nchanl, n

   ! CRTM local varaibles and types
   INTEGER :: wmo_sensor_id, nchan_emis, Error_Status, Allocate_Status
   TYPE( CRTM_RTSolution_type ), ALLOCATABLE :: RTSolution(:),RTSolution_TL(:)
   TYPE (CRTM_ChannelInfo_type)   :: ChannelInfo
   TYPE( CRTM_Atmosphere_type )   :: Atmosphere, Atmosphere_TL
   TYPE( CRTM_Surface_type )      :: Surface, Surface_TL
   TYPE( CRTM_GeometryInfo_type ) :: GeometryInfo
   !TYPE( CRTM_Options_type )      :: Options
!---------------------------------------------------------

#if !defined(CRTM)
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#else

   IF ( iv%num_inst < 1 ) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_rad_crtm")

!----------------------------------------------------------------------------
! CRTM allocation
!
! Atmosphere structure
    Atmosphere%n_Layers=(xp%kte-xp%kts)+1   ! number of vertical levels
    Atmosphere%n_Absorbers=2
    Atmosphere%n_Clouds=0
    Atmosphere%n_Aerosols=0
    if (crtm_cloud) Atmosphere%n_Clouds=6                       

    Error_Status = CRTM_Allocate_Atmosphere( Atmosphere%n_Layers, &
                                             Atmosphere%n_Absorbers, &
                                             Atmosphere%n_Clouds, &
                                             Atmosphere%n_Aerosols, &
                                             Atmosphere)
    if ( Error_Status /= 0 ) THEN
       call da_error(__FILE__,__LINE__, &
         (/"Error in allocatting CRTM Atmosphere Structure"/))
    endif

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
      num_rad   = iv%instid(inst)%num_rad

  ! CRTM channel information structure
      Error_Status = CRTM_Set_ChannelInfo(Sensor_Descriptor(inst),ChannelInfo)
      if ( Error_Status /= 0) then
         call da_error(__FILE__,__LINE__, &
          (/"Error in calling CRTM_Set_ChannelInfo"/))
      endif
      nchanl    = ChannelInfo%n_channels
                                        
  ! Allocate forward model solution RTSolution array to number of channels
      ALLOCATE( RTSolution( ChannelInfo%n_Channels ), &
                RTSolution_TL( ChannelInfo%n_Channels ), &
               STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
         call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting RTSolution"/))
      END IF

  ! CRTM Surface Structure
      if (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsua') then
         nchan_emis=4
         wmo_sensor_id=WMO_AMSUA
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsub') then
         nchan_emis=2
         wmo_sensor_id=WMO_AMSUB
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsre') then
         nchan_emis=12
         wmo_sensor_id=WMO_AMSRE
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='ssmi') then
         nchan_emis=7
         wmo_sensor_id=WMO_SSMI
      else
         nchan_emis=0
         wmo_sensor_id=INVALID_WMO_SENSOR_ID
      endif

      Error_Status = CRTM_Allocate_Surface( nchan_emis,     &  ! Input
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

      do n= 1, num_rad           ! loop for pixel
            
      ! [1.1] Get horizontal interpolation weights:

            i = iv%instid(inst)%rad(n)%loc%i
            j = iv%instid(inst)%rad(n)%loc%j
            dx = iv%instid(inst)%rad(n)%loc%dx
            dy = iv%instid(inst)%rad(n)%loc%dy
            dxm = iv%instid(inst)%rad(n)%loc%dxm
            dym = iv%instid(inst)%rad(n)%loc%dym

      ! [1.3] Extract base state Atmosphere variables 
            atmosphere%level_pressure(0) = iv%instid(inst)%rad(n)%t(0)
           do k=1,atmosphere%n_layers
            atmosphere%pressure(k) = iv%instid(inst)%rad(n)%pm(k)
            atmosphere%level_pressure(k) = iv%instid(inst)%rad(n)%t(k)
            atmosphere%temperature(k) = iv%instid(inst)%rad(n)%tm(k)
            atmosphere%absorber(k,1) = iv%instid(inst)%rad(n)%qm(k)
           if (crtm_cloud) then
            atmosphere%cloud(1)%water_content(k)=iv%instid(inst)%rad(n)%qcw(k)
            atmosphere%cloud(2)%water_content(k)=iv%instid(inst)%rad(n)%qci(k)
            atmosphere%cloud(3)%water_content(k)=iv%instid(inst)%rad(n)%qrn(k)
            atmosphere%cloud(4)%water_content(k)=iv%instid(inst)%rad(n)%qsn(k)
            atmosphere%cloud(5)%water_content(k)=iv%instid(inst)%rad(n)%qgr(k)
            atmosphere%cloud(6)%water_content(k)=0.
            atmosphere%cloud(1)%effective_radius(k)=10.
            atmosphere%cloud(2)%effective_radius(k)=200.
            atmosphere%cloud(3)%effective_radius(k)=200.
            atmosphere%cloud(4)%effective_radius(k)=200.
            atmosphere%cloud(5)%effective_radius(k)=200.
            atmosphere%cloud(6)%effective_radius(k)=200.
           end if
           enddo

      ! [1.4] User-supplied emissivity
           !Options%emissivity_switch = 1
           !Options%emissivity(1:Options%n_channels) = &
           !    iv%instid(inst)%rad(n)%emiss(1:Options%n_channels)

      ! [1.4] CRTM Surface parameter data
     Surface%Land_Coverage=iv%instid(inst)%rad(n)%land_coverage 
     Surface%Water_Coverage=iv%instid(inst)%rad(n)%water_coverage 
     Surface%Snow_Coverage=iv%instid(inst)%rad(n)%snow_coverage
     Surface%Ice_Coverage=iv%instid(inst)%rad(n)%ice_coverage

     if (Surface%Land_Coverage > 0.) then
       Surface%Land_Type=GRASS_SOIL           ! land type (User guide appendix 3)
       Surface%Land_Temperature=iv%instid(inst)%rad(n)%ts      ! K
       Surface%Soil_Moisture_Content=iv%instid(inst)%rad(n)%smois  !0.05    ! volumetric water content (g/cm**3)
       !Surface%Canopy_Water_Content=0.05      ! gravimetric water content
       Surface%Vegetation_Fraction=iv%instid(inst)%rad(n)%vegtyp
       Surface%Soil_Temperature=iv%instid(inst)%rad(n)%tslb
     endif
     if (Surface%Water_Coverage > 0.) then
       !Surface%Water_Type=SEA_WATER          ! (Currently NOT used)
       Surface%Water_Temperature=iv%instid(inst)%rad(n)%ts     ! K
       Surface%Wind_Speed=sqrt((iv%instid(inst)%rad(n)%u10)**2+ &
                               (iv%instid(inst)%rad(n)%v10)**2)  ! m/sec
       !surface%Wind_Direction=0.0            ! NOT used
       Surface%Salinity=33.                   ! ppmv
     endif
     if (Surface%Snow_Coverage > 0.) then
       Surface%Snow_Type=NEW_SNOW             ! User guide appendix 3
       Surface%Snow_Temperature=iv%instid(inst)%rad(n)%ts      ! K
       Surface%Snow_Depth=iv%instid(inst)%rad(n)%snowh         ! mm
       !Surface%Snow_Density=0.2               ! g/cm**3
       !Surface%Snow_Grain_Size=2.0            ! mm
     endif
     if (Surface%Ice_Coverage > 0.) then
       !Surface%Ice_Type=FRESH_ICE             ! NO Table offered, single example is FRESH_ICE
       Surface%Ice_Temperature=iv%instid(inst)%rad(n)%ts       ! K
       Surface%Ice_Thickness=10.              ! mm
       !Surface%Ice_Density=0.9                ! g/cm**3
       !Surface%Ice_Roughness=0.               ! NO Table offered, single example is ZERO
     endif
     Surface%SensorData%n_channels = nchan_emis
     Surface%SensorData%Sensor_ID  = wmo_sensor_id
     Surface%SensorData%Tb(1:nchan_emis) = iv%instid(inst)%rad(n)%tb(1:nchan_emis)%inv + &
                                           iv%instid(inst)%rad(n)%tb_xb(1:nchan_emis)

      ! -- Copy the TL atmosphere structure
      Error_Status = CRTM_Assign_Atmosphere( Atmosphere, Atmosphere_TL )

      IF ( Error_Status /= 0 ) THEN
        call da_error(__FILE__,__LINE__, &
          (/"Error copying Atmosphere_TL structure"/))
      END IF

      ! -- Copy the TL surface structure
      Error_Status = CRTM_Assign_Surface( Surface, Surface_TL )

      IF ( Error_Status /= 0 ) THEN
        call da_error(__FILE__,__LINE__, &
          (/"Error copying Surface_TL structure"/))
      END IF

    ! -- Zero the TL outputs
    ! Important: adjoint variables must be initialized
    CALL CRTM_Zero_Atmosphere( Atmosphere_TL )
    CALL CRTM_Zero_Surface( Surface_TL )

      ! [1.2] Interpolate horizontally Atmoshere_TL variables to ob:
            do k=xp%kts,xp%kte ! from bottem to top
              call da_interp_lin_2d( xa%t(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                     i, j, dx, dy, dxm, dym, &  ! temperature (in K)
                                     Atmosphere_TL%Temperature(xp%kte-k+1) )
              call da_interp_lin_2d( xa%q(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                     i, j, dx, dy, dxm, dym, &  ! specific humidity (in kg/kg)
                                     Atmosphere_TL%Absorber(xp%kte-k+1,1) )
              if (atmosphere%pressure(xp%kte-k+1) < 75.) Atmosphere_TL%Absorber(xp%kte-k+1,1) = 0.
            end do
              call da_interp_lin_2d( xa%psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                                     i, j, dx, dy, dxm, dym, &  ! in Pa
                                     Atmosphere_TL%Level_Pressure(Atmosphere_TL%n_Layers) )

              Atmosphere_TL%Level_Pressure(Atmosphere%n_Layers)= &
                         0.01*Atmosphere_TL%Level_Pressure(Atmosphere%n_Layers)
              Atmosphere_TL%Absorber(:,1) = 1000.*Atmosphere_TL%Absorber(:,1) ! in g/kg

      ! [1.5] CRTM GeometryInfo Structure
        GeometryInfo%Sensor_Zenith_Angle=iv%instid(inst)%rad(n)%satzen
        GeometryInfo%Source_Zenith_Angle=iv%instid(inst)%rad(n)%solzen
   !     GeometryInfo%Satellite_Height=830.
   !     GeometryInfo%Sensor_Scan_Angle=
   !     GeometryInfo%Sensor_Zenith_Angle=
   !     GeometryInfo%Sensor_Scan_Angle=
   !     GeometryInfo%Source_Zenith_Angle=

      ! [1.6] Call CRTM_TL model

         call da_crtm_tl (nchanl, Atmosphere,   &
                            Surface,      &
                            Atmosphere_TL,&
                            Surface_TL,   &
                            GeometryInfo, &
                            ChannelInfo,  &
                            RTSolution,   &
                            RTSolution_TL)
      
         !Error_Status = CRTM_Tangent_Linear(Atmosphere,   &
         !                   Surface,      &
         !                   Atmosphere_TL,&
         !                   Surface_TL,   &
         !                   GeometryInfo, &
         !                   ChannelInfo,  &
         !                   RTSolution,   &
         !                   RTSolution_TL)  !,&
         !                   !Options = Options)
         !if ( Error_Status /= 0 ) THEN
         !     call da_error(__FILE__,__LINE__, &
         !        (/"Error in calling CRTM_Tangent_Linear"/))
         !endif

      !-------------------------------------------------------------------
      ! [1.7] assign Hdx :
      !-------------------------------------------------------------------
         y%instid(inst)%rad(n)%tb(:) = RTSolution_TL(:)%brightness_temperature
         !if (n <= 10 ) write(6,'(15f8.3)') RTSolution_TL(:)%brightness_temperature

         Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_TL )
         if ( Error_Status /= 0 ) THEN
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Atmosphere_TL Structure"/))
         endif

         Error_Status = CRTM_Destroy_Surface(Surface_TL)
         if ( Error_Status /= 0 ) THEN
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Surface_TL Structure"/))
         endif

      end do  ! end loop for pixels                                                                                                                          
      !-------------------------------------------------------------------
      ! [2.0] Deallocating CRTM structures
      !-------------------------------------------------------------------
         deallocate( rtsolution, rtsolution_tl, STAT = Allocate_Status )
         IF ( Allocate_Status /= 0 ) THEN
            call da_error(__FILE__,__LINE__, &
              (/"Error in deallocatting RTSolution"/))
         END IF
             
         Error_Status = CRTM_Destroy_Surface(Surface)
         if ( Error_Status /= 0 ) THEN
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Surface Structure"/))
         endif

   end do     ! end loop for sensor

      !-------------------------------------------------------------------
      ! [3.0] Deallocating CRTM Atmosphere structures
      !-------------------------------------------------------------------
    Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )
    if ( Error_Status /= 0 ) THEN
       call da_error(__FILE__,__LINE__, &
         (/"Error in deallocatting CRTM Atmosphere Structure"/))
    endif

   if (trace_use) call da_trace_exit("da_transform_xtoy_rad_crtm")
#endif

end subroutine da_transform_xtoy_rad_crtm

