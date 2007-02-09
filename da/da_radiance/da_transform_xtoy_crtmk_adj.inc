subroutine da_transform_xtoy_crtmk_adj ( iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! PURPOSE: transform gradient from obs space to model grid space.
   !
   ! METHOD:  jo_grad_x = H^T jo_grad_y =  - H^T R^-1 ( d - H delta_x )
   !           1. input gradient in obs space and reference state of RTTOV
   !           2. call adjoint of RTM
   !           3. adjoint of interpolation from model grid to obs loc
   !
   !  HISTORY: 11/16/2006 - Creation            Zhiquan Liu
   !
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (x_type), INTENT(INOUT)   :: jo_grad_x ! 
   TYPE (xpose_type), INTENT(IN)  :: xp        ! Domain decomposition vars.
   TYPE (y_type),  INTENT(IN)     :: jo_grad_y ! H' delta_x
   TYPE (ob_type), INTENT(IN)     :: iv        ! O-B structure.

   INTEGER                        :: l, i, j, k  ! Index dimension.
   INTEGER                        :: num_rad  ! Number of radiance obs
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
   INTEGER                        :: inst, nchanl, n
   REAL, allocatable              :: q_k(:),t_k(:)
   REAL                           :: ps_k

#if defined(CRTM)
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
#endif
!---------------------------------------------------------

#if !defined(CRTM)
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#else
   IF ( iv%num_inst < 1 ) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_crtmk_adj")

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
      ALLOCATE (RTSolution(ChannelInfo%n_Channels))
      ALLOCATE (RTSolution_K(ChannelInfo%n_Channels))
      ALLOCATE (Atmosphere_K(ChannelInfo%n_Channels))
      ALLOCATE (Surface_K(ChannelInfo%n_Channels))
      ALLOCATE (t_k(xp%kte-xp%kts+1))
      ALLOCATE (q_k(xp%kte-xp%kts+1))

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
            atmosphere%level_pressure(0) = iv%instid(inst)%pf(0,n)
           do k=1,atmosphere%n_layers
            atmosphere%pressure(k) = iv%instid(inst)%pm(k,n)
            atmosphere%level_pressure(k) = iv%instid(inst)%pf(k,n)
            atmosphere%temperature(k) = iv%instid(inst)%tm(k,n)
            atmosphere%absorber(k,1) = iv%instid(inst)%qm(k,n)
           enddo

      ! [1.1] Extract base Surface parameter data
     Surface%Land_Coverage=iv%instid(inst)%land_coverage(n)
     Surface%Water_Coverage=iv%instid(inst)%water_coverage(n)
     Surface%Snow_Coverage=iv%instid(inst)%snow_coverage(n)
     Surface%Ice_Coverage=iv%instid(inst)%ice_coverage(n)

     if (Surface%Land_Coverage > 0.) then
       Surface%Land_Type=GRASS_SOIL           ! land type (User guide appendix 3)
       Surface%Land_Temperature=iv%instid(inst)%ts(n)      ! K
       Surface%Soil_Moisture_Content=iv%instid(inst)%smois(n)  !0.05    ! volumetric water content (g/cm**3)
       !Surface%Canopy_Water_Content=0.05      ! gravimetric water content
       Surface%Vegetation_Fraction=iv%instid(inst)%vegtyp(n)
       Surface%Soil_Temperature=iv%instid(inst)%tslb(n)
     endif
     if (Surface%Water_Coverage > 0.) then
       !Surface%Water_Type=SEA_WATER          ! (Currently NOT used)
       Surface%Water_Temperature=iv%instid(inst)%ts(n)     ! K
       Surface%Wind_Speed=sqrt((iv%instid(inst)%u10(n))**2+ &
                               (iv%instid(inst)%v10(n))**2)  ! m/sec
       !surface%Wind_Direction=0.0            ! NOT used
       Surface%Salinity=33.                   ! ppmv
     endif
     if (Surface%Snow_Coverage > 0.) then
       Surface%Snow_Type=NEW_SNOW             ! User guide appendix 3
       Surface%Snow_Temperature=iv%instid(inst)%ts(n)      ! K
       Surface%Snow_Depth=iv%instid(inst)%snowh(n)         ! mm
       !Surface%Snow_Density=0.2               ! g/cm**3
       !Surface%Snow_Grain_Size=2.0            ! mm
     endif
     if (Surface%Ice_Coverage > 0.) then
       !Surface%Ice_Type=FRESH_ICE             ! NO Table offered, single example is FRESH_ICE
       Surface%Ice_Temperature=iv%instid(inst)%ts(n)       ! K
       Surface%Ice_Thickness=10.              ! mm
       !Surface%Ice_Density=0.9                ! g/cm**3
       !Surface%Ice_Roughness=0.               ! NO Table offered, single example is ZERO
     endif
     Surface%SensorData%n_channels = nchan_emis
     Surface%SensorData%Sensor_ID  = wmo_sensor_id
     Surface%SensorData%Tb(1:nchan_emis) = iv%instid(inst)%tb_inv(1:nchan_emis,n) + &
                                          iv%instid(inst)%tb_xb(1:nchan_emis,n)
     
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

      ! [1.2] CRTM GeometryInfo Structure
        GeometryInfo%Sensor_Zenith_Angle=iv%instid(inst)%satzen(n)
        GeometryInfo%Source_Zenith_Angle=iv%instid(inst)%solzen(n)

      ! [1.3] assign tb = R^-1 Re :

         RTSolution_K(:)%brightness_temperature = jo_grad_y%instid(inst)%tb(:,n) 
         RTSolution_K(:)%radiance = 0.

      ! [1.4] Call CRTM_K model

         Error_Status = CRTM_K_Matrix(Atmosphere,   &
                            Surface,      &
                            RTSolution_K,&
                            GeometryInfo, &
                            ChannelInfo,  &
                            Atmosphere_K,&
                            Surface_K,   &
                            RTSolution) !,   &
                            !Options = Options)
         if ( Error_Status /= 0 ) THEN
              call da_error(__FILE__,__LINE__, &
                 (/"Error in calling CRTM_Adjoint"/))
         endif

      ! [1.5] Scale transformation and fill zero for no-control variable
          ps_k = 0.
         do l=1, ChannelInfo%n_Channels
          ps_k = ps_k + Atmosphere_K(l)%Level_Pressure(atmosphere%n_layers)                    
         end do
          ps_k = ps_k*0.01

         do k=1,atmosphere%n_layers
          t_k(k) = 0.
          q_k(k) = 0.
         do l=1, ChannelInfo%n_Channels
          t_k(k)  = t_k(k) + Atmosphere_K(l)%Temperature(k)
          q_k(k)  = q_k(k) + Atmosphere_K(l)%Absorber(k,1)
         end do
          q_k(k) = 1000.*q_k(k)
         end do
          
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
               if (atmosphere%pressure(xp%kte-k+1) < 75.) q_k(xp%kte-k+1) = 0.
               call da_interp_lin_2d_adj( jo_grad_x%t(:,:,k), xp%ims, xp%ime, xp%jms, &
                                          xp%jme, i, j, dx, dy, dxm, dym, &
                                          t_k(xp%kte-k+1))

               call da_interp_lin_2d_adj( jo_grad_x%q(:,:,k), xp%ims, xp%ime, xp%jms, & 
                                           xp%jme, i, j, dx, dy, dxm, dym, &
                                           q_k(xp%kte-k+1) )
            enddo

               call da_interp_lin_2d_adj(jo_grad_x% psfc, xp%ims, xp%ime, xp%jms, &
                                         xp%jme, i, j, dx, dy, dxm, dym,  &
                                         ps_k )

               !if (n <=10 ) then
               !   write(6,'(15f8.2)') rtsolution(:)%brightness_temperature
               !   write(6,'(e15.5)') jo_grad_x% psfc(i,j)
               !  do k=xp%kts,xp%kte
               !   write(6,'(2e15.5)') jo_grad_x%t(i,j,k), jo_grad_x%q(i,j,k)
               !  enddo
               !endif

         end do       !  end loop for pixels

      !-------------------------------------------------------------------
      ! [2.0] Deallocating CRTM structures
      !-------------------------------------------------------------------
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

         deallocate( RTSolution, RTSolution_K, Atmosphere_K, Surface_K, t_k, q_k, &
                     STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) THEN
              call da_error(__FILE__,__LINE__, &
                  (/"Error in deallocatting CRTM Surface Structure"/))
         endif

   end do        ! end loop for sensor

   if (trace_use) call da_trace_exit("da_transform_xtoy_crtmk_adj")
#endif
 
end subroutine da_transform_xtoy_crtmk_adj

