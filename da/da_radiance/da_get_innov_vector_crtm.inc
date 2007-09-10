subroutine da_get_innov_vector_crtm ( it, xb, xp, ob, iv )

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

   INTEGER                        :: n, icld  ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
   INTEGER                        :: alloc_status(40)

   REAL                             :: model_u10, model_v10
   REAL                             :: model_psfc, model_ptop
   REAL                             :: model_ts, model_elv
   REAL                             :: model_smois, model_tslb, model_snowh
   REAL                             :: model_isltyp, model_ivgtyp, model_vegfra
   INTEGER                          :: model_isflg

   REAL, DIMENSION(xp%kms:xp%kme)   :: v_p, &     ! Model value p at ob hor. location.
                                       model_qcw

   ! REAL, DIMENSION(xp%kms:xp%kme)   :: model_tm

   INTEGER            :: inst, nchanl

   ! variables for computing clwp
   REAL, dimension(xp%kms:xp%kme)     :: clw, dpf
   REAL                               :: clwp

#if defined(CRTM)
   ! CRTM local varaibles and types
   INTEGER :: wmo_sensor_id,Error_Status, Allocate_Status
   TYPE( CRTM_RTSolution_type ), ALLOCATABLE :: RTSolution(:,:)
   TYPE( CRTM_Atmosphere_type )   :: Atmosphere(1)
   TYPE( CRTM_Surface_type )      :: Surface(1)
   TYPE( CRTM_GeometryInfo_type ) :: GeometryInfo(1)
#endif
!---------------------------------------------------------

   !JRB use argument it
   if (it==0) then; print *,"why have argument it to"//__FILE__; end if

   alloc_status (:) = 0

   IF ( iv%num_inst < 1 ) return

#if !defined(CRTM)
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#else

   if (trace_use) call da_trace_entry("da_get_innov_vector_crtm")

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

   !------------------------------------------------------
   ! [1.0] calculate the background bright temperature
   !-------------------------------------------------------
   do inst = 1, iv%num_inst                 ! loop for sensor
!      if ( iv%instid(inst)%num_rad < 1 ) cycle
      if (iv%ob_numb(iv%current_ob_time)%radiance(inst) <= &
          iv%ob_numb(iv%current_ob_time-1)%radiance(inst) ) cycle
      write(UNIT=stdout,FMT='(A,A)') ' Computing Innovation : ',iv%instid(inst)%rttovid_string
      num_levs  = xp%kte-xp%kts+1 
  ! CRTM channel information structure
      ! Error_Status = CRTM_Set_ChannelInfo(Sensor_Descriptor(inst),ChannelInfo)
      ! if ( Error_Status /= 0) then
      !    call da_error(__FILE__,__LINE__, &
      !    (/"Error in calling CRTM_Set_ChannelInfo"/))
      ! endif
      nchanl    = ChannelInfo(inst)%n_channels

  ! Allocate forward model solution RTSolution array to number of channels
      ALLOCATE( RTSolution( ChannelInfo(inst)%n_Channels, 1 ), &
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
          (/"Error in allocatting CRTM Surface Structure Structure"/))
      endif
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
            Atmosphere(1)%Pressure(xp%kte-k+1)=v_p(k) ! from top to bottom
         enddo

         ! [1.2] Interpolate horizontally to ob:
         do k=xp%kts,xp%kte ! from bottem to top
             call da_interp_lin_2d( xb%t(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                    i, j, dx, dy, dxm, dym, &      ! temperature (in K)
                                    Atmosphere(1)%Temperature(xp%kte-k+1) )
             call da_interp_lin_2d( xb%q(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                    i, j, dx, dy, dxm, dym, &  ! mixture ratio (in kg/kg)
                                    Atmosphere(1)%Absorber(xp%kte-k+1,1) ) 
             Atmosphere(1)%Absorber(xp%kte-k+1,1) = 1000.*Atmosphere(1)%Absorber(xp%kte-k+1,1) ! in g/kg
           ! NOTE: WRF high-level q values seems too big, replaced by constants
             if ( v_p(k) < 75. ) Atmosphere(1)%Absorber(xp%kte-k+1,1) = 0.001

            call da_interp_lin_2d( xb%qcw(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, &   ! cloud water
                                model_qcw(xp%kte-k+1) )
            
           if (crtm_cloud) then
            Atmosphere(1)%Cloud(1)%Effective_Radius(xp%kte-k+1)=10.
            Atmosphere(1)%Cloud(1)%Water_Content(xp%kte-k+1) = &
                  model_qcw(xp%kte-k+1)

            Atmosphere(1)%Cloud(2)%Effective_Radius(xp%kte-k+1)=200.
            call da_interp_lin_2d( xb%qci(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, &  ! cloud ice
                                Atmosphere(1)%Cloud(2)%Water_Content(xp%kte-k+1) )

            Atmosphere(1)%Cloud(3)%Effective_Radius(xp%kte-k+1)=200.
            call da_interp_lin_2d( xb%qrn(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, &  ! rain
                                Atmosphere(1)%Cloud(3)%Water_Content(xp%kte-k+1) )

            Atmosphere(1)%Cloud(4)%Effective_Radius(xp%kte-k+1)=200.
            call da_interp_lin_2d( xb%qsn(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, &  ! snow
                                Atmosphere(1)%Cloud(4)%Water_Content(xp%kte-k+1) )

            Atmosphere(1)%Cloud(5)%Effective_Radius(xp%kte-k+1)=200.
            call da_interp_lin_2d( xb%qgr(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, &  ! grauple
                                Atmosphere(1)%Cloud(5)%Water_Content(xp%kte-k+1) )
 
            Atmosphere(1)%Cloud(6)%Effective_Radius(xp%kte-k+1)=200.
            Atmosphere(1)%Cloud(6)%Water_Content(xp%kte-k+1) = 0.

            end if
         enddo

         ! determine surface type of obs location
         !-----------------------------------------
         call da_detsurtyp ( xb%snow, xb%xice, xb%landmask,  &
                            xb%ivgtyp, xb%isltyp, &
                            xp%ims, xp%ime, xp%jms, xp%jme, &
                            i, j, dx, dy, dxm, dym, &
                            model_isflg,model_ivgtyp, model_isltyp, &
                            Surface(1)%Water_Coverage, Surface(1)%Ice_Coverage, &
                            Surface(1)%Land_Coverage, Surface(1)%Snow_Coverage )

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
         Atmosphere(1)%Level_Pressure(0)=model_ptop   ! to sigma level 51-->sigmaf=0
         Atmosphere(1)%Level_Pressure(Atmosphere(1)%n_Layers)=model_psfc ! to sigma level 1->sigmaf=1
         do k=xp%kts+1,xp%kte
            Atmosphere(1)%Level_Pressure(xp%kte-k+1)= &
              xb%sigmaf(k)*(model_psfc-model_ptop)+model_ptop
         enddo
 
         ! convert cloud content unit from kg/kg to kg/m^2        
         if (crtm_cloud) then
          do k=xp%kts,xp%kte
          do icld=1,Atmosphere(1)%n_Clouds
            Atmosphere(1)%Cloud(icld)%Water_Content(k)= Atmosphere(1)%Cloud(icld)%Water_Content(k)* &
            (Atmosphere(1)%Level_Pressure(k)- Atmosphere(1)%Level_Pressure(k-1))*100./gravity 
          enddo
          enddo
         end if

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

         ! ADD for computing cloud liquid water path (mm) from guess
         clwp = 0.
         do k = xp%kts,xp%kte ! from top to bottom
            dpf(k) = 100.*(atmosphere(1)%level_pressure(k) - atmosphere(1)%level_pressure(k-1))
            clw  (k) = model_qcw(k)*dpf(k)/gravity ! kg/m2 or mm
            if ( atmosphere(1)%pressure(k)<100. ) clw (k) = 0.
            clwp  = clwp + clw(k)
         end do

  ! CRTM GeometryInfo Structure
        GeometryInfo(1)%Sensor_Zenith_Angle=iv%instid(inst)%satzen(n)
        GeometryInfo(1)%Source_Zenith_Angle=iv%instid(inst)%solzen(n)
!     GeometryInfo(1)%Satellite_Height=830.
!     GeometryInfo(1)%Sensor_Scan_Angle=
!     GeometryInfo(1)%Sensor_Zenith_Angle=
!     GeometryInfo(1)%Sensor_Scan_Angle=
!     GeometryInfo(1)%Source_Zenith_Angle=

  ! CRTM Surface parameter data

     if (Surface(1)%Land_Coverage > 0.) then
       Surface(1)%Land_Type=GRASS_SOIL           ! land type (User guide appendix 3)
       Surface(1)%Land_Temperature=model_ts      ! K
       Surface(1)%Soil_Moisture_Content= model_smois !0.05    ! volumetric water content (g/cm**3)
       !Surface(1)%Canopy_Water_Content=0.05      ! gravimetric water content
       Surface(1)%Vegetation_Fraction=model_vegfra
       Surface(1)%Soil_Temperature=model_tslb
     endif
     if (Surface(1)%Water_Coverage > 0.) then
       !Surface(1)%Water_Type=SEA_WATER          ! (Currently NOT used)
       Surface(1)%Water_Temperature=model_ts     ! K
       Surface(1)%Wind_Speed=sqrt(model_u10**2+model_v10**2)  ! m/sec
       !surface(1)%Wind_Direction=0.0            ! NOT used
       Surface(1)%Salinity=33.                   ! ppmv
     endif
     if (Surface(1)%Snow_Coverage > 0.) then
       Surface(1)%Snow_Type=NEW_SNOW             ! User guide appendix 3
       Surface(1)%Snow_Temperature=model_ts      ! K
       Surface(1)%Snow_Depth=model_snowh         ! mm
       !Surface(1)%Snow_Density=0.2               ! g/cm**3
       !Surface(1)%Snow_Grain_Size=2.0            ! mm
     endif
     if (Surface(1)%Ice_Coverage > 0.) then
       !Surface(1)%Ice_Type=FRESH_ICE             ! NO Table offered, single example is FRESH_ICE
       Surface(1)%Ice_Temperature=model_ts       ! K
       Surface(1)%Ice_Thickness=10.              ! mm
       !Surface(1)%Ice_Density=0.9                ! g/cm**3
       !Surface(1)%Ice_Roughness=0.               ! NO Table offered, single example is ZERO
     endif
     if (nchanl > 0) then
       Surface(1)%SensorData%n_channels = nchanl
       Surface(1)%SensorData%Sensor_ID  = wmo_sensor_id 
       Surface(1)%SensorData%Tb(1:nchanl) = ob%instid(inst)%tb(1:nchanl,n)
     end if

         ! [1.3] Call RTM foreward model
          call da_crtm_direct(1, nchanl, 1, Atmosphere,   &
                            Surface,      &
                            GeometryInfo, &
                            ChannelInfo(inst:inst),  &
                            RTSolution)

         !Error_Status = CRTM_Forward (Atmosphere,   &
         !                   Surface,      &
         !                   GeometryInfo, &
         !                   ChannelInfo,  &
         !                   RTSolution)
         !if (n<=10) then
         !write(6,'(f10.3)') Atmosphere%level_pressure(0)
         !do k=1,Atmosphere%n_layers
         !  write(6,'(4f10.3)') Atmosphere%Level_pressure(k),Atmosphere%pressure(k), &
         !                      Atmosphere%temperature(k),Atmosphere%absorber(k,1)
         !enddo
         !  write(6,'(15f8.3)') RTSolution(1:nchanl)%Brightness_Temperature
         !  write(6,'(15f8.3)') RTSolution(1:nchanl)%surface_emissivity
         !end if

         !if ( Error_Status /= 0 ) THEN
         !     call da_error(__FILE__,__LINE__, &
         !        (/"Error in calling CRTM_Forward"/))
         !endif

         !----------------------------------------------------------------
         ! [2.0] calculate components of innovation vector:
         !----------------------------------------------------------------
         do k = 1, nchanl
            if ( iv%instid(inst)%tb_inv(k,n) > missing_r ) then 
               iv%instid(inst)%tb_xb(k,n)  = rtsolution(k,1)%Brightness_Temperature
               iv%instid(inst)%tb_inv(k,n) = &
               ob%instid(inst)%tb(k,n) - rtsolution(k,1)%Brightness_Temperature
            else
               iv%instid(inst)%tb_xb(k,n)    = rtsolution(k,1)%Brightness_Temperature
               iv%instid(inst)%tb_inv(k,n)   = missing_r
            end if
               iv%instid(inst)%emiss(k,n) = rtsolution(k,1)%surface_emissivity
         end do

         !----------------------------------------------------------------
         ! [3.0] store base state to innovation structure
         !----------------------------------------------------------------
         ! full level pressures
            iv%instid(inst)%pf(0,n)  = atmosphere(1)%level_pressure(0)
         do k=1,atmosphere(1)%n_layers
            iv%instid(inst)%pm(k,n)  = atmosphere(1)%pressure(k)
            iv%instid(inst)%pf(k,n)  = atmosphere(1)%level_pressure(k)
            iv%instid(inst)%tm(k,n)  = atmosphere(1)%temperature(k)
            iv%instid(inst)%qm(k,n)  = atmosphere(1)%absorber(k,1)
          if (crtm_cloud) then
            iv%instid(inst)%qcw(k,n) = atmosphere(1)%cloud(1)%water_content(k)
            iv%instid(inst)%qci(k,n) = atmosphere(1)%cloud(2)%water_content(k)
            iv%instid(inst)%qrn(k,n) = atmosphere(1)%cloud(3)%water_content(k)
            iv%instid(inst)%qsn(k,n) = atmosphere(1)%cloud(4)%water_content(k)
            iv%instid(inst)%qgr(k,n) = atmosphere(1)%cloud(5)%water_content(k)
          end if
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
         iv%instid(inst)%water_coverage(n)=surface(1)%water_coverage
         iv%instid(inst)%land_coverage(n)=surface(1)%land_coverage
         iv%instid(inst)%ice_coverage(n)=surface(1)%ice_coverage                              
         iv%instid(inst)%snow_coverage(n)=surface(1)%snow_coverage

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

   !------------------------------------------
   ! 4.0 perfoming bias correction files
   !------------------------------------------

   if (biascorr) then
      do inst = 1, iv%num_inst
         WRITE(UNIT=stdout,FMT='(A,A)') 'Performing bias correction for ', &
                            trim(iv%instid(inst)%rttovid_string)
         call da_biascorr(inst,ob,iv)
      end do
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

   if (trace_use) call da_trace_exit("da_get_innov_vector_crtm")
#endif
 
end subroutine da_get_innov_vector_crtm

