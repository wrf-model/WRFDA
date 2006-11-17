subroutine da_get_innov_vector_rad_crtm ( it, xb, xp, ob, iv )

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
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
   INTEGER                        :: alloc_status(40)

   REAL, DIMENSION(1:max_ob_levels) :: model_t  ! Model value t at ob location.
   REAL, DIMENSION(1:max_ob_levels) :: model_q  ! Model value q(kg/kg) at ob location.
   REAL, DIMENSION(1:max_ob_levels) :: model_mr ! Model value mr(ppmv) at ob location.
   REAL                             :: model_t2m
   REAL                             :: model_q2m, model_mr2m
   REAL                             :: model_u10, model_v10
   REAL                             :: model_psfc, model_ptop
   REAL                             :: model_ts, model_elv
   REAL                             :: model_smois, model_tslb, model_snowh
   REAL                             :: model_isltyp, model_ivgtyp, model_vegfra
   INTEGER                          :: model_isflg

   REAL, DIMENSION(xp%kms:xp%kme)   :: v_p, model_tm, &     ! Model value p at ob hor. location.
                                       model_qrn, model_qcw, &
                                       model_qci, model_qsn, &
                                       model_qgr, model_qm

   INTEGER            :: inst, nchanl
   INTEGER            :: nchan_emis
   integer            :: sensor_type

   ! variables for computing clwp
   REAL, dimension(xp%kms:xp%kme)     :: vtm, clw, dlnpf
   REAL, dimension(xp%kms:xp%kme+1)   :: pf
   REAL                               :: clwp

   ! CRTM local varaibles and types
   INTEGER :: wmo_sensor_id,Error_Status, Allocate_Status
   TYPE( CRTM_RTSolution_type ), ALLOCATABLE :: RTSolution(:)
   TYPE (CRTM_ChannelInfo_type)   :: ChannelInfo
   TYPE( CRTM_Atmosphere_type )   :: Atmosphere
   TYPE( CRTM_Surface_type )      :: Surface
   TYPE( CRTM_GeometryInfo_type ) :: GeometryInfo
!---------------------------------------------------------

   alloc_status (:) = 0

   IF ( iv%num_inst < 1 ) return

#if !defined(CRTM)
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#else

   if (trace_use) call da_trace_entry("da_get_innov_vector_rad_crtm")

   WRITE(UNIT=stdout,FMT='(A)') ' Computing Radiance Innovation------- '

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

         Allocate ( iv%instid(inst)%rad(n)%tb_xb(nchanl), stat = alloc_status(7) )
         Allocate ( iv%instid(inst)%rad(n)%emiss(nchanl), stat = alloc_status(8) )
         allocate (iv%instid(inst)%rad(n)%tm(xp%kts:xp%kte), stat= alloc_status(9) )
         allocate (iv%instid(inst)%rad(n)%qm(xp%kts:xp%kte), stat= alloc_status(16) )
         !allocate (iv%instid(inst)%rad(n)%qrn(xp%kms:xp%kme), stat= alloc_status(10) )
         allocate (iv%instid(inst)%rad(n)%qcw(xp%kms:xp%kme), stat= alloc_status(11) )
         !allocate (iv%instid(inst)%rad(n)%qci(xp%kms:xp%kme), stat= alloc_status(12) )
         !allocate (iv%instid(inst)%rad(n)%qsn(xp%kms:xp%kme), stat= alloc_status(13) )
         !allocate (iv%instid(inst)%rad(n)%qgr(xp%kms:xp%kme), stat= alloc_status(14) )
         allocate (iv%instid(inst)%rad(n)%pm(xp%kts:xp%kte), stat= alloc_status(15) )
         allocate (iv%instid(inst)%rad(n)%t(0:xp%kte), stat= alloc_status(16) )
 

         if ( any(alloc_status /= 0) ) then
            call da_error(__FILE__,__LINE__, &
               (/"Cannot Allocate mem iv%tm,qm,pm,pf"/))
         end if

         model_ts   = 0.0
         model_u10  = 0.0
         model_v10  = 0.0
         model_psfc = 0.0


         ! [1.1] Get horizontal interpolation weights:

         i = iv%instid(inst)%rad(n)%loc%i
         j = iv%instid(inst)%rad(n)%loc%j
         dx = iv%instid(inst)%rad(n)%loc%dx
         dy = iv%instid(inst)%rad(n)%loc%dy
         dxm = iv%instid(inst)%rad(n)%loc%dxm
         dym = iv%instid(inst)%rad(n)%loc%dym

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
             call da_interp_lin_2D( xb%t(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                    i, j, dx, dy, dxm, dym, &      ! temperature (in K)
                                    Atmosphere%Temperature(xp%kte-k+1) )
             call da_interp_lin_2D( xb%q(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                    i, j, dx, dy, dxm, dym, &      ! specific humidity (in kg/kg)
                                    Atmosphere%Absorber(xp%kte-k+1,1) ) 
             Atmosphere%Absorber(xp%kte-k+1,1) = 1000.*Atmosphere%Absorber(xp%kte-k+1,1) ! in g/kg
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
         !call da_interp_lin_2d( xb % t2, xp%ims, xp%ime, xp%jms, xp%jme, &
         !                    i, j, dx, dy, dxm, dym, &
         !                    model_t2m )
         !call da_interp_lin_2d( xb % q2, xp%ims, xp%ime, xp%jms, xp%jme, &
         !                    i, j, dx, dy, dxm, dym, &
         !                    model_q2m )
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
         !   call da_interp_lin_2d( xb%qrn(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
         !                       i, j, dx, dy, dxm, dym, &      ! rain
         !                       model_qrn(k) )
            call da_interp_lin_2d( xb%qcw(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, &      ! cloud water
                                model_qcw(xp%kte-xp%kts+1) )
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
            dlnpf(k) = log(atmosphere%level_pressure(k+1)) - log(atmosphere%level_pressure(k))
            clw  (k) = 29.27095*clw(k)*vtm(k)*dlnpf(k)
            clwp  = clwp + clw(k)
         end do
         clwp = 0.001*clwp   ! kg/m2 = mm

  ! CRTM GeometryInfo Structure
        GeometryInfo%Sensor_Zenith_Angle=iv%instid(inst)%rad(n)%satzen
        GeometryInfo%Source_Zenith_Angle=iv%instid(inst)%rad(n)%solzen
!     GeometryInfo%Satellite_Height=830.
!     GeometryInfo%Sensor_Scan_Angle=
!     GeometryInfo%Sensor_Zenith_Angle=
!     GeometryInfo%Sensor_Scan_Angle=
!     GeometryInfo%Source_Zenith_Angle=

  ! CRTM Surface parameter data
  ! tv comment, 10/06: values entered are DEFAULTs from Table 9 in CRTM User Guide
     !Surface%Land_Coverage=1.               ! fraction
     !Surface%Water_Coverage=0.              ! fraction
     !Surface%Snow_Coverage=0.               ! fraction
     !Surface%Ice_Coverage=0.                ! fraction

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
     Surface%SensorData%Tb(1:nchan_emis) = ob%instid(inst)%rad(n)%tb(1:nchan_emis)

         ! [1.3] Call RTM foreward model

         Error_Status = CRTM_Forward (Atmosphere,   &
                            Surface,      &
                            GeometryInfo, &
                            ChannelInfo,  &
                            RTSolution)

         !write(6,'(f10.3)') Atmosphere%pressure(0)
         !do k=1,Atmosphere%n_layers
         !  write(6,'(4f10.3)') Atmosphere%pressure(k),Atmosphere%level_pressure(k), &
         !                      Atmosphere%temperature(k),Atmosphere%absorber(k,1)
         !enddo
         !  write(6,'(15f8.3)') RTSolution(1:nchanl)%Brightness_Temperature
         !  write(6,'(15f8.3)') RTSolution(1:nchanl)%surface_emissivity

         !IF ( Error_Status /= SUCCESS) THEN
         !   CALL Display_Message( 'da_get_innov_vector_rad_crtm', &
         !                  'Error initializing CRTM', &
         !                   Error_Status)
         !   STOP
         !END IF

         if ( Error_Status /= 0 ) THEN
              call da_error(__FILE__,__LINE__, &
                 (/"Error in calling CRTM_Forward"/))
         endif

         !----------------------------------------------------------------
         ! [2.0] calculate components of innovation vector:
         !----------------------------------------------------------------

         do k = 1, nchanl
            if ( iv%instid(inst)%rad(n)%tb(k)%inv /= missing_r ) then 
               iv%instid(inst)%rad(n)%tb_xb(k)  = rtsolution(k)%Brightness_Temperature
               iv%instid(inst)%rad(n)%tb(k)%inv = &
               ob%instid(inst)%rad(n)%tb(k) - rtsolution(k)%Brightness_Temperature
            else
               iv%instid(inst)%rad(n)%tb_xb(k)    = rtsolution(k)%Brightness_Temperature
               iv%instid(inst)%rad(n)%tb(k)%inv   = missing_r
            end if
               iv%instid(inst)%rad(n)%emiss(k) = rtsolution(k)%surface_emissivity
         end do

         !----------------------------------------------------------------
         ! [3.0] store base state to innovation structure
         !----------------------------------------------------------------
         ! temperarily using %t to save full level pressures
            iv%instid(inst)%rad(n)%t(0)   = atmosphere%level_pressure(0)
         do k=1,atmosphere%n_layers
            iv%instid(inst)%rad(n)%pm(k)  = atmosphere%pressure(k)
            iv%instid(inst)%rad(n)%t(k)   = atmosphere%level_pressure(k)
            iv%instid(inst)%rad(n)%tm(k)  = atmosphere%temperature(k)
            iv%instid(inst)%rad(n)%qm(k)  = atmosphere%absorber(k,1)
         !   iv%instid(inst)%rad(n)%qrn(k) = model_qrn(k)
         !   iv%instid(inst)%rad(n)%qcw(k) = model_qcw(k)
         !   iv%instid(inst)%rad(n)%qci(k) = model_qci(k)
         !   iv%instid(inst)%rad(n)%qsn(k) = model_qsn(k)
         !   iv%instid(inst)%rad(n)%qgr(k) = model_qgr(k)
         enddo

         iv%instid(inst)%rad(n)%u10  = model_u10
         iv%instid(inst)%rad(n)%v10  = model_v10
         iv%instid(inst)%rad(n)%t2m  = 0.01*missing_r !model_t2m
         iv%instid(inst)%rad(n)%mr2m = 0.01*missing_r !model_mr2m
         iv%instid(inst)%rad(n)%ps   = model_psfc
         iv%instid(inst)%rad(n)%ts   = model_ts
         iv%instid(inst)%rad(n)%smois  = model_smois
         iv%instid(inst)%rad(n)%tslb   = model_tslb
         iv%instid(inst)%rad(n)%snowh  = model_snowh
         iv%instid(inst)%rad(n)%isflg  = model_isflg
         iv%instid(inst)%rad(n)%elevation = model_elv
         iv%instid(inst)%rad(n)%soiltyp  = model_isltyp
         iv%instid(inst)%rad(n)%vegtyp   = model_ivgtyp
         iv%instid(inst)%rad(n)%vegfra  = model_vegfra
         iv%instid(inst)%rad(n)%clwp    = clwp
         iv%instid(inst)%rad(n)%water_coverage=surface%water_coverage
         iv%instid(inst)%rad(n)%land_coverage=surface%land_coverage
         iv%instid(inst)%rad(n)%ice_coverage=surface%ice_coverage                              
         iv%instid(inst)%rad(n)%snow_coverage=surface%snow_coverage

         !----------------------------------------------------------------
         ! [4.0] Apply Bias Correction 
         !----------------------------------------------------------------

         if (biascorr) then
            call da_biascorr_rad(inst,nchanl,  &
                              ob%instid(inst)%rad(n)%tb, &
                              iv%instid(inst)%rad(n) )
         end if
      end do       !  end loop for pixels

      deallocate( rtsolution, STAT = Allocate_Status )
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

   !------------------------------------------------------------------------
   ! [5.0] Perform QC check
   !------------------------------------------------------------------------
 
   if (qc_rad) then
      call da_qc_rad(ob, iv)
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

   if (trace_use) call da_trace_exit("da_get_innov_vector_rad_crtm")
#endif
 
end subroutine da_get_innov_vector_rad_crtm

