subroutine da_get_innov_vector_rad ( it, xb, xp, ob, iv )

   !---------------------------------------------------------------------------
   !  PURPOSE: Calculate innovation vector for radiance data.
   !
   !  METHOD:  d = y - H(x)
   !       1. interpolate xb to obs location
   !       2. call forward RTM to get simulated bright temperature 
   !       3. obs BT - simulated BT
   !---------------------------------------------------------------------------

   IMPLICIT NONE
   
!#include "rttov_q2v.interface"

   INTEGER, INTENT(IN)            :: it       ! External iteration.
   TYPE (xb_type), INTENT(IN)     :: xb       ! first guess state.
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (y_type),  INTENT(INOUT)  :: ob       ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)  :: iv       ! O-B structure.

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: nlevels ! Number of obs levels.
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
   INTEGER                        :: alloc_status(40)

   REAL, DIMENSION(1:max_ob_levels) :: model_t  ! Model value t at ob location.
   REAL, DIMENSION(1:max_ob_levels) :: model_q  ! Model value q(kg/kg) at ob location.
   REAL, DIMENSION(1:max_ob_levels) :: model_mr ! Model value mr(ppmv) at ob location.
   REAL                             :: model_t2m
   REAL                             :: model_q2m, model_mr2m
   REAL                             :: model_u10(1)
   REAL                             :: model_v10(1)
   REAL                             :: model_psfc
   REAL                             :: model_ts(1), model_elv
   REAL                             :: model_smois(1), model_tslb(1), model_snowh(1)
   REAL                             :: model_isltyp(1), model_ivgtyp(1), model_vegfra(1)
   INTEGER                          :: model_isflg(1)

   REAL, DIMENSION(xp%kms:xp%kme)   :: v_p, model_tm, &     ! Model value p at ob hor. location.
                                       model_qrn, model_qcw, &
                                       model_qci, model_qsn, &
                                       model_qgr, model_qm

   INTEGER            :: inst, num_rad, nchan, iprof
   INTEGER            :: nfrequencies, nchannels, nbtout
   REAL,    pointer   :: polar(:)
   REAL,    pointer   :: pres(:)
   real,    pointer   :: tb(:,:)
   logical            :: amsua, amsub, mhs, ssmi

   integer            :: ip,kn,knchpf,sensor_type
   integer, pointer   :: kprof(:), kchan(:),kochan(:),knchan(:),indexn(:,:)
   real   , pointer   :: pemsv(:), pemsh(:), pems5(:), emissivity(:)
   logical, pointer   :: calcemis(:)
   real   , pointer   :: zasat(:), zlsat(:)

   type(con_vars_type),  pointer  :: con_vars(:)
   type(aux_vars_type),  pointer  :: aux_vars(:)

   ! variables for computing clwp
   REAL, dimension(xp%kms:xp%kme)     :: vtm, clw, dlnpf
   REAL, dimension(xp%kms:xp%kme+1)   :: pf
   REAL                               :: clwp

   alloc_status (:) = 0

   IF ( iv%num_inst < 1 ) return

#if !defined(RTTOV)
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $RTTOV option for radiances"/))
#else

   if (trace_use) call da_trace_entry("da_get_innov_vector_rad")

   WRITE(UNIT=stdout,FMT='(A)') ' Computing Radiance Innovation------- '

   !------------------------------------------------------
   ! [1.0] calculate the background bright temperature
   !-------------------------------------------------------

   num_rad = 1
   do inst = 1, iv%num_inst                 ! loop for sensor
      if ( iv%instid(inst)%num_rad < 1 ) cycle
      nlevels  = iv%instid(inst)%nlevels
      nchan    = iv%instid(inst)%nchan

      allocate (pres(1:nlevels), stat= alloc_status(2) )
      allocate (tb(num_rad,nchan), stat= alloc_status(3) )
      allocate (con_vars(num_rad), stat= alloc_status(4) )
      allocate (aux_vars(num_rad), stat= alloc_status(5) )
      allocate (zasat(num_rad), stat= alloc_status(6) )
      allocate (zlsat(num_rad), stat= alloc_status(7) )
      allocate (polar(nchan), stat= alloc_status(8) )
      if ( any(alloc_status /= 0) ) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot allocate mem pres tb con_vars aux_vars"/))
      end if

      pres(1:nlevels) = coefs(inst) % ref_prfl_p(1:nlevels)

      if (iv%ob_numb(iv%current_ob_time)%radiance(inst) <= &
          iv%ob_numb(iv%current_ob_time-1)%radiance(inst) ) cycle

      ! do n= 1, iv%instid(inst)%num_rad           ! loop for pixel
      do n= iv%ob_numb(iv%current_ob_time-1)%radiance(inst)+1, &
         iv%ob_numb(iv%current_ob_time)%radiance(inst)
         ! if ( n > iv%instid(inst)%num_rad ) exit

         allocate (iv%instid(inst)%rad(n)%zk(1:nlevels), stat= alloc_status(6) )
         allocate (iv%instid(inst)%rad(n)%t(1:nlevels), stat= alloc_status(7) )
         allocate (iv%instid(inst)%rad(n)%mr(1:nlevels), stat= alloc_status(8) )
         allocate (iv%instid(inst)%rad(n)%tm(xp%kms:xp%kme), stat= alloc_status(9) )
         allocate (iv%instid(inst)%rad(n)%qm(xp%kms:xp%kme), stat= alloc_status(16) )
         allocate (iv%instid(inst)%rad(n)%qrn(xp%kms:xp%kme), stat= alloc_status(10) )
         allocate (iv%instid(inst)%rad(n)%qcw(xp%kms:xp%kme), stat= alloc_status(11) )
         allocate (iv%instid(inst)%rad(n)%qci(xp%kms:xp%kme), stat= alloc_status(12) )
         allocate (iv%instid(inst)%rad(n)%qsn(xp%kms:xp%kme), stat= alloc_status(13) )
         allocate (iv%instid(inst)%rad(n)%qgr(xp%kms:xp%kme), stat= alloc_status(14) )
         allocate (iv%instid(inst)%rad(n)%pm(xp%kms:xp%kme), stat= alloc_status(15) )

         if ( any(alloc_status /= 0) ) then
            call da_error(__FILE__,__LINE__, &
               (/"Cannot allocate mem iv%zk, t, mr"/))
         end if

         model_t(:) = 0.0
         model_q(:) = 0.0
         model_mr(:)= 0.0
         model_t2m  = 0.0
         model_q2m  = 0.0
         model_mr2m = 0.0
         model_ts   = 0.0
         model_u10(1)  = 0.0
         model_v10(1)  = 0.0
         model_psfc = 0.0


         ! [1.1] Get horizontal interpolation weights:

         i = iv%instid(inst)%rad(n)%loc%i
         j = iv%instid(inst)%rad(n)%loc%j
         dx = iv%instid(inst)%rad(n)%loc%dx
         dy = iv%instid(inst)%rad(n)%loc%dy
         dxm = iv%instid(inst)%rad(n)%loc%dxm
         dym = iv%instid(inst)%rad(n)%loc%dym

         ! horizontal interpolate xb pressure to ob position for every xb levels
         do k=xp%kts,xp%kte
            v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
            v_p(k) = 0.01*v_p(k)  ! convert to mb
         enddo

         do k=1, nlevels
            iv%instid(inst)%rad(n)%zk(k)=missing_r
            IF ( pres(k) >= v_p (xp%kte) .and. pres(k) <= v_p (xp%kts) ) THEN
               call da_to_zk(pres(k), v_p, xp, v_interp_p, iv%instid(inst)%rad(n)%zk(k))
            END IF
         enddo
         if (interp_new) then
            iv%instid(inst)%zk(:,n) = iv%instid(inst)%rad(n)%zk(:)
         end if


         ! [1.2] Interpolate horizontally to ob:

         call da_interp_lin_3d( xb%t, xp, i, j, dx, dy, dxm, dym, &      ! temperature
                           model_t, max_ob_levels, iv%instid(inst)%rad(n)%zk, nlevels )
         call da_interp_lin_3d( xb%q, xp, i, j, dx, dy, dxm, dym, &      ! specific humidity
                           model_q, max_ob_levels, iv%instid(inst)%rad(n)%zk, nlevels )

         do k=1, nlevels
            IF ( iv%instid(inst)%rad(n)%zk(k) <= 0.0 ) THEN
               model_t(k) = coefs(inst) % ref_prfl_t(k,gas_id_watervapour)  ! outside model level
               model_mr(k) = coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
            ELSE
               model_mr(k) = model_q(k) * q2ppmv
               ! call rttov_q2v (&
               !    gas_unit_specconc,  &! in
               !    model_q(k),         &! in
               !    gas_id_watervapour, &! in
               !    model_q(k),         &! in
               !    model_mr(k)     )    ! inout
            END IF
            if (pres(k) < 100.) model_mr(k) = coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
            iv%instid(inst)%rad(n)%t(k)  = model_t(k)
            iv%instid(inst)%rad(n)%mr(k) = model_mr(k)
         end do

         ! determine surface type of obs location
         !-----------------------------------------
         call da_detsurtyp( xb%snow, xb%xice, xb%landmask,  &
                            xb%ivgtyp, xb%isltyp, &
                            xp%ims, xp%ime, xp%jms, xp%jme, &
                            i, j, dx, dy, dxm, dym, &
                            model_isflg(1),model_ivgtyp(1), model_isltyp(1) )


         call da_interp_lin_2d( xb % u10, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_u10(1) )
         call da_interp_lin_2d( xb % v10, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_v10(1) )
         call da_interp_lin_2d( xb % t2, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_t2m )
         call da_interp_lin_2d( xb % q2, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_q2m )
         call da_interp_lin_2d( xb % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_psfc )

         if ( model_isflg(1) == 0 ) then   ! over sea using SST
             call da_interp_lin_2d( xb % tgrn, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_ts(1) )
         else
             call da_interp_lin_2d( xb % tsk, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_ts(1) )
         end if

         call da_interp_lin_2d( xb % terr, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_elv )

         ! variables for emissivity calculations
         !---------------------------------------- 
         call da_interp_lin_2d( xb % smois, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_smois(1) )
         call da_interp_lin_2d( xb % tslb, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_tslb(1) )
         call da_interp_lin_2d( xb % snowh, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_snowh(1) )
         call da_interp_lin_2d( xb % vegfra, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_vegfra(1) )

         ! model_snowh(1) = model_snowh(1)*100.0   ! convert from m to mm
         model_vegfra(1) = 0.01*model_vegfra(1)  ! convert range to 0~1

         ! variables for calculation of cloud affected radiance
         !-------------------------------------------------------
         do k=xp%kts,xp%kte
            call da_interp_lin_2d( xb%t(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, &      ! temperature at model level
                                model_tm(k) )
            call da_interp_lin_2d( xb%q(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, &      ! temperature at model level
                                model_qm(k) )
            call da_interp_lin_2d( xb%qrn(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, &      ! rain
                                model_qrn(k) )
            call da_interp_lin_2d( xb%qcw(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, &      ! cloud water
                                model_qcw(k) )
            call da_interp_lin_2d( xb%qci(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, &      ! cloud ice
                                model_qci(k) )
            call da_interp_lin_2d( xb%qsn(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, &      ! snow
                                model_qsn(k) )
            call da_interp_lin_2d( xb%qgr(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, &      ! granpul
                                model_qgr(k) )

            iv%instid(inst)%rad(n)%pm(k)  = v_p(k)
            iv%instid(inst)%rad(n)%tm(k)  = model_tm(k)
            iv%instid(inst)%rad(n)%qm(k)  = model_qm(k)
            iv%instid(inst)%rad(n)%qrn(k) = model_qrn(k)
            iv%instid(inst)%rad(n)%qcw(k) = model_qcw(k)
            iv%instid(inst)%rad(n)%qci(k) = model_qci(k)
            iv%instid(inst)%rad(n)%qsn(k) = model_qsn(k)
            iv%instid(inst)%rad(n)%qgr(k) = model_qgr(k)

            ! ADD for computing cloud liquid water path

            vtm (k) = (1.+0.608*model_qm(k))*model_tm(k)  ! virtual T
            ! convert kg/kg to g/m3
            ! clw (k) = model_qcw(k)*v_p(k)*100.*0.0289644/8.31451/model_tm(k)
            clw (k) = 0.34836*model_qcw(k)*v_p(k)/model_tm(k)
            if ( v_p(k)<100. ) clw (k) = 0.

         enddo

         model_psfc = 0.01*model_psfc           ! convert to hPa 
         model_mr2m = model_q2m * q2ppmv

         ! ADD for computing cloud liquid water path (mm) from guess

         pf(xp%kts) = 100.*model_psfc
         do k = 2,xp%kte
            ! pf(k) = 100.*(v_p(k-1)+v_p(k))*0.5  ! full level pressure
            pf(k) = 50.*(v_p(k-1)+v_p(k))
         end do
         ! pf(xp%kte+1)= 100.*v_p(xp%kte)*0.5
         pf(xp%kte+1)= 50.*v_p(xp%kte)

         clwp = 0.
         do k = xp%kts,xp%kte
            dlnpf(k) = log(pf(k)) - log(pf(k+1))
            ! clw  (k) = clw(k)*vtm(k)*287.05/9.80665*dlnpf(k)
            clw  (k) = 29.27095*clw(k)*vtm(k)*dlnpf(k)
            clwp  = clwp + clw(k)
         end do
         ! clwp = 0.001*clwp   ! kg/m2 = mm

         ! call rttov_q2v (&
         !    gas_unit_specconc,  &! in
         !    model_q2m,          &! in
         !    gas_id_watervapour, &! in
         !    model_q2m,          &! in
         !    model_mr2m      )    ! inout

         iv%instid(inst)%rad(n)%u10  = model_u10(1)
         iv%instid(inst)%rad(n)%v10  = model_v10(1)
         iv%instid(inst)%rad(n)%t2m  = model_t2m
         iv%instid(inst)%rad(n)%mr2m = model_mr2m
         iv%instid(inst)%rad(n)%ps   = model_psfc
         iv%instid(inst)%rad(n)%ts   = model_ts(1)         
         iv%instid(inst)%rad(n)%smois  = model_smois(1)
         iv%instid(inst)%rad(n)%tslb   = model_tslb(1)
         iv%instid(inst)%rad(n)%snowh  = model_snowh(1)
         iv%instid(inst)%rad(n)%isflg  = model_isflg(1)
         iv%instid(inst)%rad(n)%elevation = model_elv
         iv%instid(inst)%rad(n)%soiltyp  = model_isltyp(1)
         iv%instid(inst)%rad(n)%vegtyp   = model_ivgtyp(1)
         iv%instid(inst)%rad(n)%vegfra  = model_vegfra(1)
         iv%instid(inst)%rad(n)%clwp    = clwp

         ! Compute surface emissivity for observation
         !-------------------------------------------

         knchpf = num_rad*nchan

         Allocate ( knchan (num_rad), stat = alloc_status(1) )
         Allocate ( kprof  (knchpf), stat = alloc_status(2) )
         Allocate ( kchan  (knchpf), stat = alloc_status(3) )
         Allocate ( kochan (knchpf), stat = alloc_status(4) )
         Allocate ( indexn (1:nchan,1:num_rad), stat = alloc_status(5) )
         Allocate ( pemsv  (knchpf), stat = alloc_status(7) )
         Allocate ( pemsh  (knchpf), stat = alloc_status(9) )
         Allocate ( pems5  (knchpf), stat = alloc_status(11) )
         Allocate ( calcemis(knchpf), stat = alloc_status(8) )
         Allocate ( iv%instid(inst)%rad(n)%tb_xb(nchan), stat = alloc_status(10) )
         ! Allocate ( iv%instid(inst)%rad(n)%calcemis(knchpf), stat = alloc_status(10) )

         if ( any(alloc_status /= 0) ) then
            call da_error(__FILE__,__LINE__, &
               (/"Cannot allocate mem before emiss"/))
         end if

         sensor_type  = coefs(inst)%id_sensor - 1  ! 0:IR 1:MW

         do ip=1,num_rad
            knchan(ip) = nchan
            do kn=1,nchan 
               kprof((ip-1)*nchan+kn)  = ip
               kchan((ip-1)*nchan+kn)  = kn
               kochan((ip-1)*nchan+kn) = kn
               indexn(kn,ip) = (ip-1)*nchan+kn 
            end do
         end do
         amsua = trim( inst_name(rtminit_sensor(inst))) == 'amsua'
         amsub = trim( inst_name(rtminit_sensor(inst))) == 'amsub'
         ! mhs = trim( inst_name(rtminit_sensor(inst))) == 'mhs'
         ssmi  = trim( inst_name(rtminit_sensor(inst))) == 'ssmi'

         zasat(1:num_rad) = iv%instid(inst)%rad(n)%satzen * deg2rad
         zlsat(1:num_rad) = iv%instid(inst)%rad(n)%satazi * deg2rad

#ifdef RTTOV
         call rttov_setupchan(num_rad, nchan, coefs(inst), &   ! in
            nfrequencies, nchannels, nbtout )      ! out
#endif
         Allocate ( emissivity(nchannels))
         Allocate ( iv%instid(inst)%rad(n)%emiss(nchannels))

         pemsv(:)    = 0.0
         pemsh(:)    = 0.0
         if (amsua) then
            polar(1:4) = 0.0  ! polarizition: 0:vertical; 1:horizontal
            polar(5:6) = 1.0
            polar(7)   = 0.0
            polar(8:14)= 1.0
            polar(15)  = 0.0
         end if
         if (amsub) then
            polar(1:5) = 0.0
         end if

         if (coefs(inst)%id_sensor == 2) then ! microwave
            call gsi_emiss(inst,knchpf,kprof,kchan,knchan,indexn, &
                 zasat,zlsat, polar, &
                 model_isflg(1),pemsv,pemsh,pems5, model_ts(1), &
                 model_isltyp(1),model_tslb(1), &
                 model_smois(1),model_ivgtyp(1),model_vegfra(1),model_snowh(1), &
                 sensor_type,num_rad,nchan, &
                 iv%instid(inst)%rad(n)%tb(:)%inv,amsua,amsub,ssmi, &
                 model_u10(1),model_v10(1))
         end if

         do iprof = 1, num_rad
            con_vars(iprof) % nlevels = nlevels 
            allocate (con_vars(iprof) % t(nlevels))
            allocate (con_vars(iprof) % q(nlevels))
            con_vars(iprof) % t(1:nlevels) = model_t  (1:nlevels)
            con_vars(iprof) % q(1:nlevels) = model_mr (1:nlevels)
            con_vars(iprof) % ps  = model_psfc

            aux_vars(iprof) % t2m = model_t2m
            aux_vars(iprof) % q2m = model_mr2m
            aux_vars(iprof) % u10 = model_u10(iprof)
            aux_vars(iprof) % v10 = model_v10(iprof)

            aux_vars(iprof) % surftype = iv%instid(inst)%rad(n)%landsea_mask 
            aux_vars(iprof) % surft   = model_ts(iprof)

            aux_vars(iprof) % satzen  = iv%instid(inst)%rad(n)%satzen
            aux_vars(iprof) % satazi  = iv%instid(inst)%rad(n)%satazi
         end do

         ! [1.3] Call RTM forward model

         ! print *, n, iv%instid(inst)%rad(n)%info%lat,iv%instid(inst)%rad(n)%info%lon

         call da_rttov_direct (inst, model_isflg(1), nchan, num_rad, &
                          nfrequencies, nchannels, nbtout, &
                          con_vars, aux_vars, &
                          tb, calcemis, pemsv, pemsh, emissivity )

         do iprof = 1, num_rad
            deallocate (con_vars(iprof) % t)
            deallocate (con_vars(iprof) % q)
         end do

         !----------------------------------------------------------------
         ! [2.0] calculate components of innovation vector:
         !----------------------------------------------------------------

         do k = 1, nchan
            if ( iv%instid(inst)%rad(n)%tb(k)%inv /= missing_r ) then 
               iv%instid(inst)%rad(n)%tb_xb(k)  = tb(num_rad,k)
               iv%instid(inst)%rad(n)%tb(k)%inv = &
               ob%instid(inst)%rad(n)%tb(k) - tb(num_rad,k)
            else
               iv%instid(inst)%rad(n)%tb_xb(k)    = tb(num_rad,k)
               iv%instid(inst)%rad(n)%tb(k)%inv   = missing_r
            end if
         end do

         if (coefs(inst)%id_sensor == 2) then ! microwave
            iv%instid(inst)%rad(n)%emiss(1:nchan) = pems5(1:nchan)
         else
            iv%instid(inst)%rad(n)%emiss(1:nchan) = emissivity(1:nchan)
         end if

         !----------------------------------------------------------------
         ! [3.0] Apply Bias Correction 
         !----------------------------------------------------------------

         if (lbiascorr) then
            call da_biascorr_rad(inst,nchan,  &
                              ob%instid(inst)%rad(n)%tb, &
                              iv%instid(inst)%rad(n) )
         end if
      end do       !  end loop for pixels

      deallocate (pres)
      deallocate (tb)
      deallocate (con_vars)
      deallocate (aux_vars)
      deallocate (zasat)
      deallocate (zlsat)
      deallocate (polar)
      deallocate (knchan)
      deallocate (kprof)
      deallocate (kchan)
      deallocate (kochan)
      deallocate (indexn)
      deallocate (pemsv)
      deallocate (pemsh) 
      deallocate (pems5)
   end do        ! end loop for sensor

   !------------------------------------------------------------------------
   ! [4.0] Perform QC check
   !------------------------------------------------------------------------
 
   if (lqc_rad) then
      call da_qc_rad(ob, iv )
   end if

   if (interp_new) then
      do inst = 1, iv%num_inst
         if ( iv%instid(inst)%num_rad < 1 ) cycle
         call da_togrid_new (iv%instid(inst)%zk, xp%kts, xp%kte,  &
            iv%instid(inst)%loc_k, &
            iv%instid(inst)%loc_dz, &
            iv%instid(inst)%loc_dzm)
      end do
   end if

   !------------------------------------------
   ! 5.0 prepare bias statistics files
   !------------------------------------------

   if (lbiasprep) then
      do inst = 1, iv%num_inst
         WRITE(UNIT=stdout,FMT='(A,A)') 'Preparing bias statistics files for ', &
                            trim(iv%instid(inst)%rttovid_string)
         call da_biasprep(inst,ob,iv)
      end do
   end if

   if (trace_use) call da_trace_exit("da_get_innov_vector_rad")
#endif
 
end subroutine da_get_innov_vector_rad

