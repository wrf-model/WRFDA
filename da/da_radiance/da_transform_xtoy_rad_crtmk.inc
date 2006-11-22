subroutine da_transform_xtoy_rad_crtmk ( xa, iv, xp, y )

   !---------------------------------------------------------------------------
   !  PURPOSE: transform from analysis increment to
   !                          pertubation radiance.
   !           K-Matrix vesion of da_transform_xtoy_rad_crtm
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

   INTEGER                        :: l, i, j, k  ! Index dimension.
   INTEGER                        :: num_rad  ! Number of radiance obs
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
   INTEGER                        :: inst, nchanl, n
   REAL, pointer                  :: q_tl(:),t_tl(:)
   REAL                           :: ps_tl

   ! CRTM local varaibles and types
   INTEGER :: wmo_sensor_id, nchan_emis, Error_Status, Allocate_Status
   TYPE( CRTM_RTSolution_type ), ALLOCATABLE :: RTSolution(:),RTSolution_K(:)
   TYPE (CRTM_ChannelInfo_type)   :: ChannelInfo
   TYPE( CRTM_Atmosphere_type )   :: Atmosphere
   TYPE( CRTM_Surface_type )      :: Surface
   TYPE( CRTM_Atmosphere_type ), ALLOCATABLE ::  Atmosphere_K(:)
   TYPE( CRTM_Surface_type ),    ALLOCATABLE ::  Surface_K(:)
   TYPE( CRTM_GeometryInfo_type ) :: GeometryInfo
   !TYPE( CRTM_Options_type )      :: Options
!---------------------------------------------------------

#if !defined(CRTM)
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#else
   IF ( iv%num_inst < 1 ) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_rad_crtmk")

!-------------------------------------------------------------------------------

   do inst = 1, iv%num_inst                 ! loop for sensor
      if ( iv%instid(inst)%num_rad < 1 ) cycle
      num_rad = iv%instid(inst)%num_rad

  ! CRTM channel information structure
      Error_Status = CRTM_Set_ChannelInfo(Sensor_Descriptor(inst),ChannelInfo)
      if ( Error_Status /= 0) then
         call da_error(__FILE__,__LINE__, &
          (/"Error in calling CRTM_Set_ChannelInfo"/))
      endif
      nchanl    = ChannelInfo%n_channels
 
  ! Allocate forward model solution RTSolution array to number of channels
      ALLOCATE( RTSolution( ChannelInfo%n_Channels ), &
                RTSolution_K( ChannelInfo%n_Channels ), &
                Atmosphere_K( ChannelInfo%n_Channels ), &
                Surface_K( ChannelInfo%n_Channels ), &
                t_tl     ( xp%kte-xp%kts+1 ), &
                q_tl     ( xp%kte-xp%kts+1 ), &
                STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
         call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting Atmosphere_K"/))                                                                                   
      END IF

!----------------------------------------------------------------------------
! CRTM Atmosphere structure
    Atmosphere%n_Layers=(xp%kte-xp%kts)+1   ! number of vertical levels
    Atmosphere%n_Absorbers=2
    Atmosphere%n_Clouds=0
    Atmosphere%n_Aerosols=0

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
 
      do n=1,num_rad

      ! [1.0] Extract base state Atmosphere variables
            atmosphere%level_pressure(0) = iv%instid(inst)%rad(n)%t(0)
           do k=1,atmosphere%n_layers
            atmosphere%pressure(k) = iv%instid(inst)%rad(n)%pm(k)
            atmosphere%level_pressure(k) = iv%instid(inst)%rad(n)%t(k)
            atmosphere%temperature(k) = iv%instid(inst)%rad(n)%tm(k)
            atmosphere%absorber(k,1) = iv%instid(inst)%rad(n)%qm(k)
           enddo

      ! [1.1] Extract base Surface parameter data
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
     
     do l = 1, ChannelInfo%n_Channels
      ! -- Copy the adjoint atmosphere structure
      Error_Status = CRTM_Assign_Atmosphere( Atmosphere, Atmosphere_K(l) )

      IF ( Error_Status /= 0 ) THEN
        call da_error(__FILE__,__LINE__, &
          (/"Error copying Atmosphere_K structure"/))
      END IF

      ! -- Copy the adjoint surface structure
      Error_Status = CRTM_Assign_Surface( Surface, Surface_K(l) )

      IF ( Error_Status /= 0 ) THEN
        call da_error(__FILE__,__LINE__, &
          (/"Error copying Surface_K structure"/))
      END IF
     end do

    ! -- Zero the Adjoint outputs 
    ! Important: adjoint variables must be initialized
    CALL CRTM_Zero_Atmosphere( Atmosphere_K(:) )
    CALL CRTM_Zero_Surface( Surface_K(:) )

      ! [1.2] Interpolate horizontally Atmoshere_TL variables to ob:
            !-----------------------------------------------------
            ! [1.6] Get horizontal interpolation weights:
            !-----------------------------------------------------

            i = iv%instid(inst)%rad(n)%loc%i
            j = iv%instid(inst)%rad(n)%loc%j
            dx = iv%instid(inst)%rad(n)%loc%dx
            dy = iv%instid(inst)%rad(n)%loc%dy
            dxm = iv%instid(inst)%rad(n)%loc%dxm
            dym = iv%instid(inst)%rad(n)%loc%dym

            do k=xp%kts,xp%kte ! from bottem to top
              call da_interp_lin_2d( xa%t(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                     i, j, dx, dy, dxm, dym, &  ! temperature (in K)
                                     t_tl(xp%kte-k+1) )
              call da_interp_lin_2d( xa%q(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                     i, j, dx, dy, dxm, dym, &  ! specific humidity (in kg/kg)
                                     q_tl(xp%kte-k+1) )
              if ( atmosphere%pressure(xp%kte-k+1) < 75. ) q_tl(xp%kte-k+1) = 0.
            end do
              call da_interp_lin_2d( xa%psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                                     i, j, dx, dy, dxm, dym, &  ! in Pa
                                     ps_tl )
              ps_tl = ps_tl*0.01
              q_tl(:)  = q_tl(:)*1000. ! in g/kg

      ! [1.2] CRTM GeometryInfo Structure
        GeometryInfo%Sensor_Zenith_Angle=iv%instid(inst)%rad(n)%satzen
        GeometryInfo%Source_Zenith_Angle=iv%instid(inst)%rad(n)%solzen

      ! [1.3] assign tb = R^-1 Re :

         RTSolution_K(:)%brightness_temperature = 1.
         RTSolution_K(:)%radiance = 0.

      ! [1.4] Call CRTM_K model
      ! put CRTM_K_Matrix into da_crtm_k for tracing

         call da_crtm_k(nchanl, Atmosphere,   &
                            Surface,      &
                            RTSolution_K,&
                            GeometryInfo, &
                            ChannelInfo,  &
                            Atmosphere_K,&
                            Surface_K,   &
                            RTSolution) !,   &
                            !Options = Options)

         !Error_Status = CRTM_K_Matrix(Atmosphere,   &
         !                   Surface,      &
         !                   RTSolution_K,&
         !                   GeometryInfo, &
         !                   ChannelInfo,  &
         !                   Atmosphere_K,&
         !                   Surface_K,   &
         !                   RTSolution) !,   &
         !                   !Options = Options)
         !if ( Error_Status /= 0 ) THEN
         !     call da_error(__FILE__,__LINE__, &
         !        (/"Error in calling CRTM_Adjoint"/))
         !endif

      ! [1.5] Scale transformation and fill zero for no-control variable
          y%instid(inst)%rad(n)%tb(:) = 0.
         do l=1, ChannelInfo%n_Channels
          y%instid(inst)%rad(n)%tb(l) = y%instid(inst)%rad(n)%tb(l) +  &
                    Atmosphere_K(l)%Level_Pressure(atmosphere%n_layers)*ps_tl
         do k=1,atmosphere%n_layers
          y%instid(inst)%rad(n)%tb(l) = y%instid(inst)%rad(n)%tb(l) & 
                               + Atmosphere_K(l)%Temperature(k)*t_tl(k)  &
                               + Atmosphere_K(l)%Absorber(k,1)*q_tl(k)
         end do
         end do
          
         !if (n <=10 ) write(6,'(15f8.3)') y%instid(inst)%rad(n)%tb(:)

         end do       !  end loop for pixels

      !-------------------------------------------------------------------
      ! [2.0] Deallocating CRTM structures
      !-------------------------------------------------------------------
         deallocate( RTSolution, RTSolution_K,  &
                     STAT = Allocate_Status )

         Error_Status = CRTM_Destroy_Surface(Surface)
         if ( Error_Status /= 0 ) THEN
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Surface Structure"/))
         endif
                                                                                                                          
         Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )
         if ( Error_Status /= 0 ) THEN
         call da_error(__FILE__,__LINE__, &
            (/"Error in deallocatting CRTM Atmosphere Structure"/))
         endif

         Error_Status = CRTM_Destroy_Surface(Surface_K)
         if ( Error_Status /= 0 ) THEN
               call da_error(__FILE__,__LINE__, &
                  (/"Error in deallocatting CRTM Surface_K Structure"/))
         endif

         Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_K )
         if ( Error_Status /= 0 ) THEN
               call da_error(__FILE__,__LINE__, &
                  (/"Error in deallocatting CRTM Atmosphere_K Structure"/))
         endif

         deallocate( Atmosphere_K, Surface_K, t_tl, q_tl, &
                     STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) THEN
              call da_error(__FILE__,__LINE__, &
                  (/"Error in deallocatting CRTM Surface Structure"/))
         endif

   end do        ! end loop for sensor

   if (trace_use) call da_trace_exit("da_transform_xtoy_rad_crtmk")
#endif
 
end subroutine da_transform_xtoy_rad_crtmk

