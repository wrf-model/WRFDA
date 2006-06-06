 subroutine da_get_innov_vector_rad ( it, xb, xp, ob, iv )
!------------------------------------------------------------------------------
!  PURPOSE: Calculate innovation vector for radiance data.
!
!  METHOD:  d = y - H(x)
!       1. interpolate xb to obs location
!       2. call foreward RTM to get simulated bright temperature 
!       3. obs BT - simulated BT
!
!  HISTORY: 27/07/2005 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
#ifdef DM_PARALLEL
   INCLUDE 'mpif.h'
#endif

!#include "rttov_q2v.interface"

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

   INTEGER            :: inst, nprofiles, nchanl, iprof, ios
   INTEGER            :: nfrequencies, nchannels, nbtout
   REAL,    pointer   :: zk(:), polar(:)
   REAL,    pointer   :: pres(:)
   character(len=30)  :: filename
   real,    pointer   :: tb(:,:)
   logical            :: amsua, amsub, mhs, ssmi
   integer            :: myproc, ierr, ndomain

   integer            :: ip,kn,knchpf,sensor_type
   integer, pointer   :: kprof(:), kchan(:),kochan(:),knchan(:),indexn(:,:)
   real   , pointer   :: pemsv(:), pemsh(:), pems5(:), emissivity(:)
   logical, pointer   :: calcemis(:)
   real   , pointer   :: zasat(:), zlsat(:)
!
   type(con_vars_type),  pointer  :: con_vars(:)
   type(aux_vars_type),  pointer  :: aux_vars(:)

! variables for computing clwp
   REAL, dimension(xp%kms:xp%kme)     :: vtm, clw, dlnpf
   REAL, dimension(xp%kms:xp%kme+1)   :: pf
   REAL                               :: clwp


#ifdef DM_PARALLEL
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
#endif

   alloc_status (:) = 0

   IF ( iv%num_inst < 1 ) return

   WRITE(UNIT=stdout,FMT='(A)') ' Computing Radiance Innovation------- '

!------------------------------------------------------
!   [1.0] calculate the background bright temperature
!-------------------------------------------------------
    nprofiles = 1
 do inst = 1, iv%num_inst                 ! loop for sensor
     if ( iv%instid(inst)%num_rad < 1 ) cycle
     num_levs  = iv%instid(inst)%nlevels
     nchanl    = iv%instid(inst)%nchan
  
     allocate (zk(1:num_levs), stat= alloc_status(1) )
     allocate (pres(1:num_levs), stat= alloc_status(2) )
     allocate (tb(nprofiles,nchanl), stat= alloc_status(3) )
     allocate (con_vars(nprofiles), stat= alloc_status(4) )
     allocate (aux_vars(nprofiles), stat= alloc_status(5) )
     allocate (zasat(nprofiles), stat= alloc_status(6) )
     allocate (zlsat(nprofiles), stat= alloc_status(7) )
     allocate (polar(nchanl), stat= alloc_status(8) )
     if ( any(alloc_status /= 0) ) then
       CALL wrf_error_fatal3(__FILE__,__LINE__, &
         "Cannot Allocate mem zk, pres tb con_vars aux_vars")
     end if

     pres(1:num_levs) = coefs(inst) % ref_prfl_p(1:num_levs)

   do n= 1, iv%instid(inst)%num_rad           ! loop for pixel
!      if ( n > iv%instid(inst)%num_rad ) exit
      if(iv%instid(inst)%rad(n)%loc%proc_domain_with_halo) then

         allocate (iv%instid(inst)%rad(n)%zk(1:num_levs), stat= alloc_status(6) )
         allocate (iv%instid(inst)%rad(n)%t(1:num_levs), stat= alloc_status(7) )
         allocate (iv%instid(inst)%rad(n)%mr(1:num_levs), stat= alloc_status(8) )
         allocate (iv%instid(inst)%rad(n)%tm(xp%kms:xp%kme), stat= alloc_status(9) )
         allocate (iv%instid(inst)%rad(n)%qm(xp%kms:xp%kme), stat= alloc_status(16) )
         allocate (iv%instid(inst)%rad(n)%qrn(xp%kms:xp%kme), stat= alloc_status(10) )
         allocate (iv%instid(inst)%rad(n)%qcw(xp%kms:xp%kme), stat= alloc_status(11) )
         allocate (iv%instid(inst)%rad(n)%qci(xp%kms:xp%kme), stat= alloc_status(12) )
         allocate (iv%instid(inst)%rad(n)%qsn(xp%kms:xp%kme), stat= alloc_status(13) )
         allocate (iv%instid(inst)%rad(n)%qgr(xp%kms:xp%kme), stat= alloc_status(14) )
         allocate (iv%instid(inst)%rad(n)%pm(xp%kms:xp%kme), stat= alloc_status(15) )

         if ( any(alloc_status /= 0) ) then
           CALL wrf_error_fatal3(__FILE__,__LINE__, &
             "Cannot Allocate mem iv%zk, t, mr")
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


!        [1.1] Get horizontal interpolation weights:

           i = iv%instid(inst)%rad(n)%loc%i
           j = iv%instid(inst)%rad(n)%loc%j
          dx = iv%instid(inst)%rad(n)%loc%dx
          dy = iv%instid(inst)%rad(n)%loc%dy
         dxm = iv%instid(inst)%rad(n)%loc%dxm
         dym = iv%instid(inst)%rad(n)%loc%dym

!     horizontal interpolate xb pressure to ob position for every xb levels
         do k=xp%kts,xp%kte
            v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                   + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
            v_p(k) = 0.01*v_p(k)  ! convert to mb
         enddo

         do k=1, num_levs
            zk(k)=missing_r
            IF ( pres(k) >= v_p (xp%kte) .and. pres(k) <= v_p (xp%kts) ) THEN
               call da_to_zk(pres(k), v_p, xp, v_interp_p, zk(k))
            END IF
            iv%instid(inst)%rad(n)%zk(k) = zk(k)
         enddo

!        [1.2] Interpolate horizontally to ob:

         call da_interp_lin_3d( xb%t, xp, i, j, dx, dy, dxm, dym, &      ! temperature
                              model_t, max_ob_levels, zk, num_levs )
         call da_interp_lin_3d( xb%q, xp, i, j, dx, dy, dxm, dym, &      ! specific humidity
                              model_q, max_ob_levels, zk, num_levs )

         do k=1, num_levs

            IF ( zk(k) <= 0.0 ) THEN
               model_t(k) = coefs(inst) % ref_prfl_t(k,gas_id_watervapour)  ! outside model level
               model_mr(k) = coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
            ELSE
!            model_mr(k) = model_q(k) * 1.60771704e+6 ! convert q(kg/kg) to ppmv
             model_mr(k) = model_q(k) * q2ppmv
!               call rttov_q2v (&
!                 & gas_unit_specconc,  &! in
!                 & model_q(k),         &! in
!                 & gas_id_watervapour, &! in
!                 & model_q(k),         &! in
!                 & model_mr(k)     )    ! inout
            END IF
            if (pres(k) < 100.) model_mr(k) = coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
            iv%instid(inst)%rad(n)%t(k)  = model_t(k)
            iv%instid(inst)%rad(n)%mr(k) = model_mr(k)

         end do

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
         call da_interp_lin_2d( xb % tsk, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_ts(1) )
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

!         model_snowh(1) = model_snowh(1)*100.0   ! convert from m to mm
         model_vegfra(1) = 0.01*model_vegfra(1)  ! convert range to 0~1

!
! determine surface type of obs location
!-----------------------------------------
         call da_detsurtyp( xb%snow, xb%xice, xb%landmask,  &
                            xb%ivgtyp, xb%isltyp, &
                            xp%ims, xp%ime, xp%jms, xp%jme, &
                            i, j, dx, dy, dxm, dym, &
                            model_isflg(1),model_ivgtyp(1), model_isltyp(1) )


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
!        clw (k) = model_qcw(k)*v_p(k)*100.*0.0289644/8.31451/model_tm(k) ! convert kg/kg to g/m3
        clw (k) = 0.34836*model_qcw(k)*v_p(k)/model_tm(k)
        if ( v_p(k)<100. ) clw (k) = 0.

       enddo

          model_psfc = 0.01*model_psfc           ! convert to hPa 
!          model_mr2m = model_q2m * 1.60771704e+6  ! convert q(kg/kg) to ppmv
          model_mr2m = model_q2m * q2ppmv

!   ADD for computing cloud liquid water path (mm) from guess
!
    pf(xp%kts) = 100.*model_psfc
    do k = 2,xp%kte
!      pf(k) = 100.*(v_p(k-1)+v_p(k))*0.5  ! full level pressure
      pf(k) = 50.*(v_p(k-1)+v_p(k))
    end do
!    pf(xp%kte+1)= 100.*v_p(xp%kte)*0.5
    pf(xp%kte+1)= 50.*v_p(xp%kte)

      clwp = 0.
    do k = xp%kts,xp%kte
      dlnpf(k) = log(pf(k)) - log(pf(k+1))
!      clw  (k) = clw(k)*vtm(k)*287.05/9.80665*dlnpf(k)
      clw  (k) = 29.27095*clw(k)*vtm(k)*dlnpf(k)
      clwp  = clwp + clw(k)
    end do
    !  clwp = 0.001*clwp   ! kg/m2 = mm

!               call rttov_q2v (&
!                 & gas_unit_specconc,  &! in
!                 & model_q2m,          &! in
!                 & gas_id_watervapour, &! in
!                 & model_q2m,          &! in
!                 & model_mr2m      )    ! inout

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

!     Compute surface emissivity for observation
!-----------------------------------------------------

  knchpf = nprofiles*nchanl

  Allocate ( knchan (nprofiles), stat = alloc_status(1) )
  Allocate ( kprof  (knchpf), stat = alloc_status(2) )
  Allocate ( kchan  (knchpf), stat = alloc_status(3) )
  Allocate ( kochan (knchpf), stat = alloc_status(4) )
  Allocate ( indexn (1:nchanl,1:nprofiles), stat = alloc_status(5) )
  Allocate ( pemsv  (knchpf), stat = alloc_status(7) )
  Allocate ( pemsh  (knchpf), stat = alloc_status(9) )
  Allocate ( pems5  (knchpf), stat = alloc_status(11) )
  Allocate ( calcemis(knchpf), stat = alloc_status(8) )
  Allocate ( iv%instid(inst)%rad(n)%tb_xb(nchanl), stat = alloc_status(10) )
!  Allocate ( iv%instid(inst)%rad(n)%calcemis(knchpf), stat = alloc_status(10) )
  
         if ( any(alloc_status /= 0) ) then
           CALL wrf_error_fatal3(__FILE__,__LINE__, &
             "Cannot allocate mem before emiss")
         end if

  sensor_type  = coefs(inst)%id_sensor - 1  ! 0:IR 1:MW

   do ip=1,nprofiles
    knchan(ip) = nchanl
   do kn=1,nchanl 
      kprof((ip-1)*nchanl+kn)  = ip
      kchan((ip-1)*nchanl+kn)  = kn
      kochan((ip-1)*nchanl+kn) = kn
      indexn(kn,ip) = (ip-1)*nchanl+kn 
   end do
   end do
      amsua = trim( inst_name(rtminit_sensor(inst))) == 'amsua'
      amsub = trim( inst_name(rtminit_sensor(inst))) == 'amsub'
!      mhs   = trim( inst_name(rtminit_sensor(inst))) == 'mhs'
      ssmi  = trim( inst_name(rtminit_sensor(inst))) == 'ssmi'
   
!---------------------------------------------------------------------     
      zasat(1:nprofiles) = iv%instid(inst)%rad(n)%satzen * deg2rad
      zlsat(1:nprofiles) = iv%instid(inst)%rad(n)%satazi * deg2rad

#ifdef RTTOV
      call rttov_setupchan(nprofiles, nchanl, coefs(inst), &   ! in
                     nfrequencies, nchannels, nbtout )      ! out
#endif
      allocate(emissivity(nchannels))
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
           sensor_type,nprofiles,nchanl, &
           iv%instid(inst)%rad(n)%tb(:)%inv,amsua,amsub,ssmi, &
           model_u10(1),model_v10(1))
   end if

        do iprof = 1, nprofiles

              con_vars(iprof) % nlevels = num_levs 
              allocate (con_vars(iprof) % t(num_levs))
              allocate (con_vars(iprof) % q(num_levs))
              con_vars(iprof) % t(1:num_levs) = model_t  (1:num_levs)
              con_vars(iprof) % q(1:num_levs) = model_mr (1:num_levs)
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

      if (.not. use_kma1dvar) then

!        [1.3] Call RTM foreward model

         !print *, n, iv%instid(inst)%rad(n)%info%lat,iv%instid(inst)%rad(n)%info%lon

         call da_rtm_direct (inst, model_isflg(1), nchanl, nprofiles, &
                             nfrequencies, nchannels, nbtout, &
                             con_vars, aux_vars, &
                             tb, calcemis, pemsv, pemsh, emissivity )

!------------------------------------------------------------------------
!        [2.0] calculate components of innovation vector:
!------------------------------------------------------------------------

         do k = 1, nchanl
          if ( iv%instid(inst)%rad(n)%tb(k)%inv > missing_r ) then 
            iv%instid(inst)%rad(n)%tb_xb(k)  = tb(nprofiles,k)
            iv%instid(inst)%rad(n)%tb(k)%inv = &
               ob%instid(inst)%rad(n)%tb(k) - tb(nprofiles,k)
          else
            iv%instid(inst)%rad(n)%tb_xb(k)    = tb(nprofiles,k)
            iv%instid(inst)%rad(n)%tb(k)%inv   = missing_r
          end if
         end do
               
!------------------------------------------------------------------------
!        [3.0] Apply Bias Correction 
!------------------------------------------------------------------------
        if (lbiascorr) then
            call da_biascorr_rad(inst,nchanl,  &
                                 ob%instid(inst)%rad(n)%tb, &
                                 iv%instid(inst)%rad(n) )
        end if

!------------------------------------------------------------------------
!        [4.0] Perform QC check
!------------------------------------------------------------------------
            call da_qc_rad(inst,nchanl,  &
                           ob%instid(inst)%rad(n)%tb, &
                           iv%instid(inst)%rad(n) )

       end if   ! end if .not. use_kma1dvar

          if (coefs(inst)%id_sensor == 2) then ! microwave
             iv%instid(inst)%rad(n)%emiss(1:nchanl) = pems5(1:nchanl)
          else
             iv%instid(inst)%rad(n)%emiss(1:nchanl) = emissivity(1:nchanl)
          end if

          do iprof = 1, nprofiles
             deallocate (con_vars(iprof) % t)
             deallocate (con_vars(iprof) % q)
          end do

       end if   !  end endif for within_halo

   end do       !  end loop for pixels

     deallocate (zk)
     deallocate (pres)
     deallocate (tb)
     deallocate (con_vars)
     deallocate (aux_vars)
     deallocate (polar)
     deallocate (pemsv)
     deallocate (pemsh) 
     deallocate (pems5)

  end do        ! end loop for sensor

!  6.0  print iv output
!------------------------------
 if ( lwrite_iv ) then
#ifdef DM_PARALLEL
 call MPI_COMM_RANK( MPI_COMM_WORLD, myproc, ierr )
#endif
 do n = 1, iv%num_inst
  if (iv%instid(n)%num_rad < 1) cycle

!! count number of obs within the loc%proc_domain
!!---------------------------------------------
  ndomain = 0
  do i =1,iv%instid(n)%num_rad

  if(iv%instid(n)%rad(i)%loc%proc_domain) then
    ndomain = ndomain + 1
  end if
  end do
#ifdef DM_PARALLEL
  write(UNIT=stdout,FMT='(a,i3,a,i7)') 'number of obs in proc # ', myproc,' : ', ndomain
#else
  write(UNIT=stdout,FMT='(a,i7)') 'number of obs in proc # ', ndomain
#endif

#ifdef DM_PARALLEL
  write(UNIT=filename,FMT='(a,i2.2)') trim(iv%instid(n)%rttovid_string)//'.inv_', myproc
#else
  write(UNIT=filename,FMT='(a)') trim(iv%instid(n)%rttovid_string)//'.inv'
#endif
  open(UNIT=innov_vector_unit,file=trim(filename),form='formatted',iostat=ios)
  if( ios /= 0 ) Then
    CALL wrf_error_fatal3(__FILE__,__LINE__,&
      "Cannot open output file "//TRIM(filename))
  Endif
    write(UNIT=innov_vector_unit,FMT='(a,a,i7,a,i5,a)') &
      trim(iv%instid(n)%rttovid_string), &
                        ' number-of-pixels : ', ndomain, &
                        ' channel-number-of-each-pixel : ', iv%instid(n)%nchan, &
                        ' index-of-channels : '
    write(UNIT=innov_vector_unit,FMT='(10i5)') iv%instid(n)%ichan
    write(UNIT=innov_vector_unit,FMT=*) ' pixel-info : i date scanpos landsea_mask  elv lat lon  satzen satazi '
    write(UNIT=innov_vector_unit,FMT=*) ' xb-surf-info : i t2m mr2m(ppmv) u10 v10 ps ts smois tslb snowh isflg &
                    & soiltyp vegtyp vegfra elev clwp'
   ndomain = 0
  do i =1,iv%instid(n)%num_rad
   if(iv%instid(n)%rad(i)%loc%proc_domain) then
    ndomain=ndomain+1
    write(UNIT=innov_vector_unit,FMT='(a,i7,2x,a,2i3,f6.0,4f8.2)') &
      'INFO : ', ndomain, &
                             iv%instid(n)%rad(i)%info%date_char, &
                             iv%instid(n)%rad(i)%scanpos,   &
                             iv%instid(n)%rad(i)%landsea_mask, &
                             iv%instid(n)%rad(i)%info%elv,  &
                             iv%instid(n)%rad(i)%info%lat,  &
                             iv%instid(n)%rad(i)%info%lon, &
                             iv%instid(n)%rad(i)%satzen,    &
                             iv%instid(n)%rad(i)%satazi
 if (iv%instid(n)%rad(i)%isflg==0) then
    write(UNIT=innov_vector_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') &
      ' SEA : ', i, &
                             iv%instid(n)%rad(i)%t2m, &
                             iv%instid(n)%rad(i)%mr2m,   &
                             iv%instid(n)%rad(i)%u10, &
                             iv%instid(n)%rad(i)%v10,  &
                             iv%instid(n)%rad(i)%ps,  &
                             iv%instid(n)%rad(i)%ts,  &
                             iv%instid(n)%rad(i)%smois,  &
                             iv%instid(n)%rad(i)%tslb,  &
                             iv%instid(n)%rad(i)%snowh, &
                             iv%instid(n)%rad(i)%isflg, &
                             nint(iv%instid(n)%rad(i)%soiltyp), &
                             nint(iv%instid(n)%rad(i)%vegtyp), &
                             iv%instid(n)%rad(i)%vegfra, &
                             iv%instid(n)%rad(i)%elevation, &
                             iv%instid(n)%rad(i)%clwp
 else if (iv%instid(n)%rad(i)%isflg==1) then
   write(UNIT=innov_vector_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') &
     ' ICE : ', i, &
                             iv%instid(n)%rad(i)%t2m, &
                             iv%instid(n)%rad(i)%mr2m,   &
                             iv%instid(n)%rad(i)%u10, &
                             iv%instid(n)%rad(i)%v10,  &
                             iv%instid(n)%rad(i)%ps,  &
                             iv%instid(n)%rad(i)%ts,  &
                             iv%instid(n)%rad(i)%smois,  &
                             iv%instid(n)%rad(i)%tslb,  &
                             iv%instid(n)%rad(i)%snowh, &
                             iv%instid(n)%rad(i)%isflg, &
                             nint(iv%instid(n)%rad(i)%soiltyp), &
                             nint(iv%instid(n)%rad(i)%vegtyp), &
                             iv%instid(n)%rad(i)%vegfra, &
                             iv%instid(n)%rad(i)%elevation, &
                             iv%instid(n)%rad(i)%clwp
 else if (iv%instid(n)%rad(i)%isflg==2) then
   write(UNIT=innov_vector_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') &
     'LAND : ', i, &
                             iv%instid(n)%rad(i)%t2m, &
                             iv%instid(n)%rad(i)%mr2m,   &
                             iv%instid(n)%rad(i)%u10, &
                             iv%instid(n)%rad(i)%v10,  &
                             iv%instid(n)%rad(i)%ps,  &
                             iv%instid(n)%rad(i)%ts,  &
                             iv%instid(n)%rad(i)%smois,  &
                             iv%instid(n)%rad(i)%tslb,  &
                             iv%instid(n)%rad(i)%snowh, &
                             iv%instid(n)%rad(i)%isflg, &
                             nint(iv%instid(n)%rad(i)%soiltyp), &
                             nint(iv%instid(n)%rad(i)%vegtyp), &
                             iv%instid(n)%rad(i)%vegfra, &
                             iv%instid(n)%rad(i)%elevation, &
                             iv%instid(n)%rad(i)%clwp
 else if (iv%instid(n)%rad(i)%isflg==3) then
   write(UNIT=innov_vector_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') &
     'SNOW : ', i, &
                             iv%instid(n)%rad(i)%t2m, &
                             iv%instid(n)%rad(i)%mr2m,   &
                             iv%instid(n)%rad(i)%u10, &
                             iv%instid(n)%rad(i)%v10,  &
                             iv%instid(n)%rad(i)%ps,  &
                             iv%instid(n)%rad(i)%ts,  &
                             iv%instid(n)%rad(i)%smois,  &
                             iv%instid(n)%rad(i)%tslb,  &
                             iv%instid(n)%rad(i)%snowh, &
                             iv%instid(n)%rad(i)%isflg, &
                             nint(iv%instid(n)%rad(i)%soiltyp), &
                             nint(iv%instid(n)%rad(i)%vegtyp), &
                             iv%instid(n)%rad(i)%vegfra, &
                             iv%instid(n)%rad(i)%elevation, &
                             iv%instid(n)%rad(i)%clwp
 else if (iv%instid(n)%rad(i)%isflg==4) then
   write(UNIT=innov_vector_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') &
     'MSEA : ', i, &
                             iv%instid(n)%rad(i)%t2m, &
                             iv%instid(n)%rad(i)%mr2m,   &
                             iv%instid(n)%rad(i)%u10, &
                             iv%instid(n)%rad(i)%v10,  &
                             iv%instid(n)%rad(i)%ps,  &
                             iv%instid(n)%rad(i)%ts,  &
                             iv%instid(n)%rad(i)%smois,  &
                             iv%instid(n)%rad(i)%tslb,  &
                             iv%instid(n)%rad(i)%snowh, &
                             iv%instid(n)%rad(i)%isflg, &
                             nint(iv%instid(n)%rad(i)%soiltyp), &
                             nint(iv%instid(n)%rad(i)%vegtyp), &
                             iv%instid(n)%rad(i)%vegfra, &
                             iv%instid(n)%rad(i)%elevation, &
                             iv%instid(n)%rad(i)%clwp
 else if (iv%instid(n)%rad(i)%isflg==5) then
   write(UNIT=innov_vector_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') &
     'MICE : ', i, &
                             iv%instid(n)%rad(i)%t2m, &
                             iv%instid(n)%rad(i)%mr2m,   &
                             iv%instid(n)%rad(i)%u10, &
                             iv%instid(n)%rad(i)%v10,  &
                             iv%instid(n)%rad(i)%ps,  &
                             iv%instid(n)%rad(i)%ts,  &
                             iv%instid(n)%rad(i)%smois,  &
                             iv%instid(n)%rad(i)%tslb,  &
                             iv%instid(n)%rad(i)%snowh, &
                             iv%instid(n)%rad(i)%isflg, &
                             nint(iv%instid(n)%rad(i)%soiltyp), &
                             nint(iv%instid(n)%rad(i)%vegtyp), &
                             iv%instid(n)%rad(i)%vegfra, &
                             iv%instid(n)%rad(i)%elevation, &
                             iv%instid(n)%rad(i)%clwp
 else if (iv%instid(n)%rad(i)%isflg==6) then
   write(UNIT=innov_vector_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') &
     'MLND : ', i, &
                             iv%instid(n)%rad(i)%t2m, &
                             iv%instid(n)%rad(i)%mr2m,   &
                             iv%instid(n)%rad(i)%u10, &
                             iv%instid(n)%rad(i)%v10,  &
                             iv%instid(n)%rad(i)%ps,  &
                             iv%instid(n)%rad(i)%ts,  &
                             iv%instid(n)%rad(i)%smois,  &
                             iv%instid(n)%rad(i)%tslb,  &
                             iv%instid(n)%rad(i)%snowh, &
                             iv%instid(n)%rad(i)%isflg, &
                             nint(iv%instid(n)%rad(i)%soiltyp), &
                             nint(iv%instid(n)%rad(i)%vegtyp), &
                             iv%instid(n)%rad(i)%vegfra, &
                             iv%instid(n)%rad(i)%elevation, &
                             iv%instid(n)%rad(i)%clwp
 else if (iv%instid(n)%rad(i)%isflg==7) then
   write(UNIT=innov_vector_unit, FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') &
     'MSNO : ', i, &
                             iv%instid(n)%rad(i)%t2m, &
                             iv%instid(n)%rad(i)%mr2m,   &
                             iv%instid(n)%rad(i)%u10, &
                             iv%instid(n)%rad(i)%v10,  &
                             iv%instid(n)%rad(i)%ps,  &
                             iv%instid(n)%rad(i)%ts,  &
                             iv%instid(n)%rad(i)%smois,  &
                             iv%instid(n)%rad(i)%tslb,  &
                             iv%instid(n)%rad(i)%snowh, &
                             iv%instid(n)%rad(i)%isflg, &
                             nint(iv%instid(n)%rad(i)%soiltyp), &
                             nint(iv%instid(n)%rad(i)%vegtyp), &
                             iv%instid(n)%rad(i)%vegfra, &
                             iv%instid(n)%rad(i)%elevation, &
                             iv%instid(n)%rad(i)%clwp
 end if

!    write(UNIT=innov_vector_unit,FMT='(a,19f11.2)')
!      'OBS  : ', ob%instid(n)%rad(i)%tb(:)
!    write(UNIT=innov_vector_unit,FMT='(a,19f11.2)')
!      'INV  : ', iv%instid(n)%rad(i)%tb(:)%inv
!    write(UNIT=innov_vector_unit,FMT='(a,19f11.2)')
!      'EMS  : ', iv%instid(n)%rad(i)%emiss(1:iv%instid(n)%nchan)
    write(UNIT=innov_vector_unit,FMT='(a)') 'OBS  : '
    write(UNIT=innov_vector_unit,FMT='(10f11.2)') &
      ob%instid(n)%rad(i)%tb(:)
    write(UNIT=innov_vector_unit,FMT='(a)') 'INV  : '
    write(UNIT=innov_vector_unit,FMT='(10f11.2)') &
      iv%instid(n)%rad(i)%tb(:)%inv
    write(UNIT=innov_vector_unit,FMT='(a)') 'EMS  : '
    write(UNIT=innov_vector_unit,FMT='(10f11.2)') &
      iv%instid(n)%rad(i)%emiss(1:iv%instid(n)%nchan)

!    write(UNIT=innov_vector_unit,FMT='(a,19f11.2)') &
!      'ERR  : ', iv%instid(n)%rad(i)%tb(:)%error
!    write(UNIT=innov_vector_unit,FMT='(a,19i11)') &
!      'QC   : ', iv%instid(n)%rad(i)%tb(:)%qc


  if (lwrite_profile) then
    write(UNIT=innov_vector_unit,FMT=*) &
      'RTM_level pres(mb) T(k) Q(ppmv) WRF_level pres(mb) T(k) q(g/kg) clw(g/kg) rain(g/kg)'
    do k=xp%kts,xp%kte
     if (k <= num_levs) then
      write(UNIT=innov_vector_unit,FMT='(i3,f10.2,f8.2,e11.4,i3,f10.2,f8.2,3e11.4)') &
            k, &                             ! RTTOV levels
            coefs(n) % ref_prfl_p(k) , &
            iv%instid(n)%rad(i)%t(k) , &
            iv%instid(n)%rad(i)%mr(k), &
            k,  &                            ! WRF model levels
            iv%instid(n)%rad(i)%pm(k) , &
            iv%instid(n)%rad(i)%tm(k) , &
            iv%instid(n)%rad(i)%qm(k)*1000 , &    
            iv%instid(n)%rad(i)%qcw(k)*1000., &
            iv%instid(n)%rad(i)%qrn(k)*1000.

     else
      write(UNIT=innov_vector_unit,FMT='(32x,i3,f10.2,f8.2,3e11.4)') k, &
            iv%instid(n)%rad(i)%pm(k) , &
            iv%instid(n)%rad(i)%tm(k) , &
            iv%instid(n)%rad(i)%qm(k)*1000 , &
            iv%instid(n)%rad(i)%qcw(k)*1000., &
            iv%instid(n)%rad(i)%qrn(k)*1000.
!            iv%instid(n)%rad(i)%qci(k)*1000., &
!            iv%instid(n)%rad(i)%qsn(k)*1000., &
!            iv%instid(n)%rad(i)%qgr(k)*1000.
     end if  
    end do   ! end loop profile
    end if   ! end if write_profile

   end if    ! end if proc_domain
  end do     ! end do pixels
  close(UNIT=innov_vector_unit)
 end do    !! end do instruments
end if     !! end if lwrite_iv

 return
 
END SUBROUTINE da_get_innov_vector_rad

