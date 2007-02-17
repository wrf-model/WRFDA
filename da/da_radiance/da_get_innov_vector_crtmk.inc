subroutine da_get_innov_vector_crtmk ( it, xb, xp, ob, iv )

   !---------------------------------------------------------------------------
   !  PURPOSE: Calculate innovation vector for radiance data.
   !
   !  METHOD:  d = y - H(x)
   !       1. interpolate xb to obs location
   !       2. call foreward RTM to get simulated bright temperature 
   !       3. obs BT - simulated BT
   !
   !  HISTORY: 11/14/2005 - Creation            Zhiquan Liu
   !---------------------------------------------------------------------------

   IMPLICIT NONE
   
   INTEGER, INTENT(IN)            :: it       ! External iteration.
   TYPE (xb_type), INTENT(IN)     :: xb       ! first guess state.
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (y_type),  INTENT(INOUT)  :: ob       ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)  :: iv       ! O-B structure.

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k,l  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.

   REAL                             :: model_u10, model_v10
   REAL                             :: model_psfc, model_ptop
   REAL                             :: model_ts, model_elv
   REAL                             :: model_smois, model_tslb, model_snowh
   REAL                             :: model_isltyp, model_ivgtyp, model_vegfra
   INTEGER                          :: model_isflg

   REAL, DIMENSION(xp%kms:xp%kme)   :: v_p    ! Model value p at ob hor. location.
   
   ! REAL, DIMENSION(xp%kms:xp%kme)   :: model_tm

   REAL, DIMENSION(xp%kms:xp%kme)   :: model_qcw

   ! REAL, DIMENSION(xp%kms:xp%kme) ::  model_qci, model_qsn, &
   !                                    model_qgr, model_qm

   INTEGER            :: inst, nchanl
   INTEGER            :: nchan_emis

   ! variables for computing clwp
   REAL, dimension(xp%kms:xp%kme)     :: vtm, clw, dlnpf
   REAL                               :: clwp

#if defined(CRTM)
   ! CRTM local varaibles and types
   INTEGER :: wmo_sensor_id,Error_Status, Allocate_Status
   TYPE( CRTM_RTSolution_type ), ALLOCATABLE :: RTSolution(:), RTSolution_K(:)
   TYPE (CRTM_ChannelInfo_type)   :: ChannelInfo
   TYPE( CRTM_Atmosphere_type )   :: Atmosphere
   TYPE( CRTM_Surface_type )      :: Surface
   TYPE( CRTM_Atmosphere_type ), ALLOCATABLE ::  Atmosphere_K(:)
   TYPE( CRTM_Surface_type ),    ALLOCATABLE ::  Surface_K(:)
   TYPE( CRTM_GeometryInfo_type ) :: GeometryInfo
#endif
!---------------------------------------------------------

   !JRB use argument it
   if (it==0) then; print *,"why have argument it to "//__FILE__; end if

#if !defined(CRTM)
   IF ( iv%num_inst < 1 ) return

    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#else

   if (trace_use) call da_trace_entry("da_get_innov_vector_crtmk")

   WRITE(UNIT=stdout,FMT='(A)') ' Computing Radiance Innovation and Jacobians using CRTM_K------- '

!----------------------------------------------------------------------------
! CRTM allocation
!
! Atmosphere structure
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

   !------------------------------------------------------
   ! [1.0] calculate the background bright temperature
   !-------------------------------------------------------

   do inst = 1, iv%num_inst                 ! loop for sensor
      if ( iv%instid(inst)%num_rad < 1 ) cycle
      num_levs  = xp%kte-xp%kts+1 

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
          (/"Error in allocatting CRTM Surface Structure Structure"/))
      endif

      if (iv%ob_numb(iv%current_ob_time)%radiance(inst) <= &
          iv%ob_numb(iv%current_ob_time-1)%radiance(inst) ) cycle

      ! do n= 1, iv%instid(inst)%num_rad           ! loop for pixel
      do n= iv%ob_numb(iv%current_ob_time-1)%radiance(inst)+1, &
         iv%ob_numb(iv%current_ob_time)%radiance(inst)
         ! if ( n > iv%instid(inst)%num_rad ) exit

         model_ts   = 0.0
         model_u10  = 0.0
         model_v10  = 0.0
         model_psfc = 0.0


         ! [1.1] Get horizontal interpolation weights:

         i = iv%instid(inst)%loc_i(n)
         j = iv%instid(inst)%loc_j(n)
         dx = iv%instid(inst)%loc_dx(n)
         dy = iv%instid(inst)%loc_dy(n)
         dxm = iv%instid(inst)%loc_dxm(n)
         dym = iv%instid(inst)%loc_dym(n)

         ! horizontal interpolate xb pressure to ob position for every xb layer
         ! get CRTM pressure Layers 
         do k=xp%kts,xp%kte ! from bottem to top
            v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
            v_p(k) = 0.01*v_p(k)  ! convert Pa to hPa
            Atmosphere%Pressure(xp%kte-k+1)=v_p(k) ! from top to bottom
         enddo

         ! [1.2] Interpolate horizontally to ob:
         do k=xp%kts,xp%kte ! from bottem to top
             call da_interp_lin_2d( xb%t(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                    i, j, dx, dy, dxm, dym, &      ! temperature (in K)
                                    Atmosphere%Temperature(xp%kte-k+1) )
             call da_interp_lin_2d( xb%q(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                    i, j, dx, dy, dxm, dym, &      ! specific humidity (in kg/kg)
                                    Atmosphere%Absorber(xp%kte-k+1,1) ) 
             Atmosphere%Absorber(xp%kte-k+1,1) = 1000.*Atmosphere%Absorber(xp%kte-k+1,1) ! in g/kg
           ! NOTE: WRF high-level q values seems too big, replaced by constants
             if ( v_p(k) < 75. ) Atmosphere%Absorber(xp%kte-k+1,1) = 0.001
         enddo
 

         ! determine surface type of obs location
         !-----------------------------------------
         call da_detsurtyp ( xb%snow, xb%xice, xb%landmask,  &
                            xb%ivgtyp, xb%isltyp, &
                            xp%ims, xp%ime, xp%jms, xp%jme, &
                            i, j, dx, dy, dxm, dym, &
                            model_isflg,model_ivgtyp, model_isltyp, &
                            Surface%Water_Coverage, Surface%Ice_Coverage, &
                            Surface%Land_Coverage, Surface%Snow_Coverage )

         call da_interp_lin_2d( xb % u10, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_u10 )
         call da_interp_lin_2d( xb % v10, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_v10 )
         call da_interp_lin_2d( xb % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_psfc )

         model_psfc = 0.01*model_psfc           ! convert to hPa
         model_ptop = 0.01*xb%ptop

         ! get CRTM levels (0.005hPa at top) /model full level
         Atmosphere%Level_Pressure(0)=model_ptop   ! to sigma level 51-->sigmaf=0
         Atmosphere%Level_Pressure(Atmosphere%n_Layers)=model_psfc ! to sigma level 1->sigmaf=1
         do k=xp%kts+1,xp%kte
            Atmosphere%Level_Pressure(xp%kte-k+1)= &
              xb%sigmaf(k)*(model_psfc-model_ptop)+model_ptop
         enddo
         
         if ( model_isflg == 0 ) then   ! over sea using SST
             call da_interp_lin_2d( xb % tgrn, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_ts )
         else
             call da_interp_lin_2d( xb % tsk, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_ts )
         end if

         call da_interp_lin_2d( xb % terr, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_elv )

         ! variables for emissivity calculations
         !---------------------------------------- 
         call da_interp_lin_2d( xb % smois, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_smois )
         call da_interp_lin_2d( xb % tslb, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_tslb )
         call da_interp_lin_2d( xb % snowh, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_snowh )
         call da_interp_lin_2d( xb % vegfra, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_vegfra )

         ! model_snowh = model_snowh*100.0   ! convert from m to mm
         model_vegfra = 0.01*model_vegfra  ! convert range to 0~1

         ! variables for calculation of cloud affected radiance
         !-------------------------------------------------------
         do k=xp%kts,xp%kte
            call da_interp_lin_2d( xb%qcw(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, &      ! cloud water
                                model_qcw(xp%kte-k+1) )
         !   call da_interp_lin_2d( xb%qrn(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
         !                       i, j, dx, dy, dxm, dym, &      ! rain
         !                       model_qrn(k) )
         !   call da_interp_lin_2d( xb%qci(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
         !                       i, j, dx, dy, dxm, dym, &      ! cloud ice
         !                       model_qci(k) )
         !   call da_interp_lin_2d( xb%qsn(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
         !                       i, j, dx, dy, dxm, dym, &      ! snow
         !                       model_qsn(k) )
         !   call da_interp_lin_2d( xb%qgr(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
         !                       i, j, dx, dy, dxm, dym, &      ! granpul
         !                       model_qgr(k) )
         enddo

         do k=xp%kts,xp%kte
         ! ADD for computing cloud liquid water path
          vtm (k) = (1.+0.608*0.001*atmosphere%absorber(k,1))*atmosphere%temperature(k)  ! virtual T
         ! convert kg/kg to g/m3
            ! clw (k) = model_qcw(k)*v_p(k)*100.*0.0289644/8.31451/model_tm(k)
          clw (k) = 0.34836*model_qcw(k)*atmosphere%pressure(k)/atmosphere%temperature(k)
          if ( atmosphere%pressure(k)<100. ) clw (k) = 0.
         enddo

         ! ADD for computing cloud liquid water path (mm) from guess
         clwp = 0.
         do k = xp%kts,xp%kte ! from top to bottom
            dlnpf(k) = log(atmosphere%level_pressure(k)) - log(atmosphere%level_pressure(k-1))
            clw  (k) = 29.27095*clw(k)*vtm(k)*dlnpf(k)
            clwp  = clwp + clw(k)
         end do
         clwp = 0.001*clwp   ! kg/m2 = mm

  ! CRTM GeometryInfo Structure
        GeometryInfo%Sensor_Zenith_Angle=iv%instid(inst)%satzen(n)
        GeometryInfo%Source_Zenith_Angle=iv%instid(inst)%solzen(n)
!     GeometryInfo%Satellite_Height=830.
!     GeometryInfo%Sensor_Scan_Angle=
!     GeometryInfo%Sensor_Zenith_Angle=
!     GeometryInfo%Sensor_Scan_Angle=
!     GeometryInfo%Source_Zenith_Angle=

  ! CRTM Surface parameter data

     if (Surface%Land_Coverage > 0.) then
       Surface%Land_Type=GRASS_SOIL           ! land type (User guide appendix 3)
       Surface%Land_Temperature=model_ts      ! K
       Surface%Soil_Moisture_Content= model_smois !0.05    ! volumetric water content (g/cm**3)
       !Surface%Canopy_Water_Content=0.05      ! gravimetric water content
       Surface%Vegetation_Fraction=model_vegfra
       Surface%Soil_Temperature=model_tslb
     endif
     if (Surface%Water_Coverage > 0.) then
       !Surface%Water_Type=SEA_WATER          ! (Currently NOT used)
       Surface%Water_Temperature=model_ts     ! K
       Surface%Wind_Speed=sqrt(model_u10**2+model_v10**2)  ! m/sec
       !surface%Wind_Direction=0.0            ! NOT used
       Surface%Salinity=33.                   ! ppmv
     endif
     if (Surface%Snow_Coverage > 0.) then
       Surface%Snow_Type=NEW_SNOW             ! User guide appendix 3
       Surface%Snow_Temperature=model_ts      ! K
       Surface%Snow_Depth=model_snowh         ! mm
       !Surface%Snow_Density=0.2               ! g/cm**3
       !Surface%Snow_Grain_Size=2.0            ! mm
     endif
     if (Surface%Ice_Coverage > 0.) then
       !Surface%Ice_Type=FRESH_ICE             ! NO Table offered, single example is FRESH_ICE
       Surface%Ice_Temperature=model_ts       ! K
       Surface%Ice_Thickness=10.              ! mm
       !Surface%Ice_Density=0.9                ! g/cm**3
       !Surface%Ice_Roughness=0.               ! NO Table offered, single example is ZERO
     endif
     Surface%SensorData%n_channels = nchan_emis
     Surface%SensorData%Sensor_ID  = wmo_sensor_id 
     Surface%SensorData%Tb(1:nchan_emis) = ob%instid(inst)%tb(1:nchan_emis,n)

! CRTM surface/atmosphere K initialization
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


      ! [1.3] assign tb = R^-1 Re :

         RTSolution_K(:)%brightness_temperature = 1.
         RTSolution_K(:)%radiance = 0.

         ! [1.3] Call RTM K-Matrix model

         call da_crtm_k(nchanl, Atmosphere,   &
                            Surface,      &
                            RTSolution_K,&
                            GeometryInfo, &
                            ChannelInfo,  &
                            Atmosphere_K,&
                            Surface_K,   &
                            RTSolution) !,   &
                            !Options = Options)

         !----------------------------------------------------------------
         ! [2.0] calculate components of innovation vector:
         !----------------------------------------------------------------

         do k = 1, nchanl
            if ( iv%instid(inst)%tb_inv(k,n) /= missing_r ) then 
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
         ! temperarily using %t to save full level pressures
            iv%instid(inst)%t(0,n)   = atmosphere%level_pressure(0)
         do k=1,atmosphere%n_layers
            iv%instid(inst)%pm(k,n)  = atmosphere%pressure(k)
            iv%instid(inst)%t(k,n)   = atmosphere%level_pressure(k)
            iv%instid(inst)%tm(k,n)  = atmosphere%temperature(k)
            iv%instid(inst)%qm(k,n)  = atmosphere%absorber(k,1)
           do l=1,nchanl
            iv%instid(inst)%t_jacobian(l,k,n) = atmosphere_k(l)%temperature(k)
            iv%instid(inst)%q_jacobian(l,k,n) = atmosphere_k(l)%absorber(k,1)
            iv%instid(inst)%ps_jacobian(l,n)  = atmosphere_k(l)%level_pressure(atmosphere%n_layers)
           end do
         !   iv%instid(inst)%qrn(k,n) = model_qrn(k)
         !   iv%instid(inst)%qcw(k,n) = model_qcw(k)
         !   iv%instid(inst)%qci(k,n) = model_qci(k)
         !   iv%instid(inst)%qsn(k,n) = model_qsn(k)
         !   iv%instid(inst)%qgr(k,m) = model_qgr(k)
         enddo

         iv%instid(inst)%u10(n)  = model_u10
         iv%instid(inst)%v10(n)  = model_v10
         iv%instid(inst)%t2m(n)  = 0.01*missing_r !model_t2m
         iv%instid(inst)%mr2m(n) = 0.01*missing_r !model_mr2m
         iv%instid(inst)%ps(n)   = model_psfc
         iv%instid(inst)%ts(n)   = model_ts
         iv%instid(inst)%smois(n)  = model_smois
         iv%instid(inst)%tslb(n)   = model_tslb
         iv%instid(inst)%snowh(n)  = model_snowh
         iv%instid(inst)%isflg(n)  = model_isflg
         iv%instid(inst)%elevation(n) = model_elv
         iv%instid(inst)%soiltyp(n)  = model_isltyp
         iv%instid(inst)%vegtyp(n)   = model_ivgtyp
         iv%instid(inst)%vegfra(n)  = model_vegfra
         iv%instid(inst)%clwp(n)    = clwp
         iv%instid(inst)%water_coverage(n)=surface%water_coverage
         iv%instid(inst)%land_coverage(n)=surface%land_coverage
         iv%instid(inst)%ice_coverage(n)=surface%ice_coverage                              
         iv%instid(inst)%snow_coverage(n)=surface%snow_coverage

         !----------------------------------------------------------------
         ! [4.0] Apply Bias Correction 
         !----------------------------------------------------------------

         if (biascorr) then
            call da_biascorr_rad(inst,ob,iv)
         end if
      end do       !  end loop for pixels

      deallocate( rtsolution, rtsolution_k, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
         call da_error(__FILE__,__LINE__, &
          (/"Error in deallocatting RTSolution"/))
      END IF

      Error_Status = CRTM_Destroy_Surface(Surface)
      if ( Error_Status /= 0 ) THEN
        call da_error(__FILE__,__LINE__, &
          (/"Error in deallocatting CRTM Surface Structure"/))
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

         deallocate( Atmosphere_K, Surface_K, &
                     STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) THEN
              call da_error(__FILE__,__LINE__, &
                  (/"Error in deallocatting CRTM Surface Structure"/))
         endif

   end do        ! end loop for sensor

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
         WRITE(UNIT=stdout,FMT='(A,A)') 'Preparing bias statistics files for ', &
                            trim(iv%instid(inst)%rttovid_string)
         call da_biasprep(inst,ob,iv)
      end do
   end if

    Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )
    if ( Error_Status /= 0 ) THEN
       call da_error(__FILE__,__LINE__, &
         (/"Error in deallocatting CRTM Atmosphere Structure"/))
    endif   

   if (trace_use) call da_trace_exit("da_get_innov_vector_crtmk")
#endif
 
end subroutine da_get_innov_vector_crtmk

