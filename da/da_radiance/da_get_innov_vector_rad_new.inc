subroutine da_get_innov_vector_rad_new ( it, xb, xp, ob, iv )

   !---------------------------------------------------------------------------
   !  PURPOSE: Calculate innovation vector for radiance data.
   !
   !  METHOD:  d = y - H(x)
   !       1. interpolate xb to obs location
   !       2. call forward RTM to get simulated bright temperature 
   !       3. obs BT - simulated BT
   !---------------------------------------------------------------------------

   implicit none
   
!#include "rttov_q2v.interface"

   integer, intent(in)            :: it       ! External iteration.
   type (xb_type), intent(in)     :: xb       ! first guess state.
   type (xpose_type), intent(in)  :: xp       ! Domain decomposition vars.
   type (y_type),  intent(inout)  :: ob       ! Observation structure.
   type (ob_type), intent(inout)  :: iv       ! O-B structure.

   integer                        :: n        ! Loop counter.
   integer                        :: k  ! Index dimension.
   integer                        :: nlevels ! Number of obs levels.
   integer                        :: alloc_status(40)

   real, allocatable :: v_p(:,:)

   integer            :: inst, num_rad, nchan
   integer            :: nfrequencies, nchannels, nbtout
   real,    pointer   :: polar(:)
   real,    pointer   :: pres(:)
   logical            :: amsua, amsub, mhs, ssmi

   integer            :: ip,kn,knchpf,sensor_type,n1,n2,n1n2
   integer, pointer   :: kprof(:), kchan(:),kochan(:),knchan(:),indexn(:,:)
   real   , pointer   :: pemsv(:), pemsh(:), pems5(:), emissivity(:)
   logical, pointer   :: calcemis(:)
   real   , pointer   :: zasat(:), zlsat(:)

   type(con_vars_type),  pointer  :: con_vars(:)
   type(aux_vars_type),  pointer  :: aux_vars(:)

   ! variables for computing clwp
   real, allocatable :: vtm(:,:), dlnpf(:,:), clw(:,:), pf(:,:)

   alloc_status (:) = 0

   IF ( iv%num_inst < 1 ) return

#if !defined(RTTOV)
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $RTTOV option for radiances"/))
#else

   if (trace_use) call da_trace_entry("da_get_innov_vector_rad_new")

   write(unit=stdout,fmt='(A)') ' Computing Radiance Innovation------- '

   !------------------------------------------------------
   ! [1.0] calculate the background bright temperature
   !-------------------------------------------------------

   do inst = 1, iv%num_inst                 ! loop for sensor
      if ( iv%instid(inst)%num_rad < 1 ) cycle
      nlevels  = iv%instid(inst)%nlevels
      nchan    = iv%instid(inst)%nchan
      num_rad  = iv%instid(inst)%num_rad

      amsua = trim( inst_name(rtminit_sensor(inst))) == 'amsua'
      amsub = trim( inst_name(rtminit_sensor(inst))) == 'amsub'
      ! mhs = trim( inst_name(rtminit_sensor(inst))) == 'mhs'
      ssmi  = trim( inst_name(rtminit_sensor(inst))) == 'ssmi'
      sensor_type  = coefs(inst)%id_sensor - 1  ! 0:IR 1:MW

      if (iv%ob_numb(iv%current_ob_time)%radiance(inst) <= &
          iv%ob_numb(iv%current_ob_time-1)%radiance(inst) ) cycle
      n1 = iv%ob_numb(iv%current_ob_time-1)%radiance(inst)+1
      n2 = iv%ob_numb(iv%current_ob_time)%radiance(inst)

      n1n2=n2-n1+1

!      if (.not. interp_new) then
if (.true.) then
         allocate (pres(1:nlevels))
         allocate (con_vars(n1:n2))
         allocate (aux_vars(n1:n2))
         allocate (zasat(n1:n2))
         allocate (zlsat(n1:n2))
         allocate (polar(nchan))
      end if

      knchpf = n1n2*nchan

      allocate (knchan (n1:n2))
      allocate (kprof  (knchpf))
      allocate (kchan  (knchpf))
      allocate (kochan (knchpf))
      allocate (indexn (1:nchan,n1:n2))
      allocate (pemsv  (knchpf))
      allocate (pemsh  (knchpf))
      allocate (pems5  (knchpf))
      allocate (calcemis(knchpf))

      pres(1:nlevels) = coefs(inst) % ref_prfl_p(1:nlevels)

      allocate(v_p(xp%kms:xp%kme,n1:n2))
      allocate(vtm(xp%kms:xp%kme,n1:n2))
      allocate(clw(xp%kms:xp%kme,n1:n2))
      allocate(dlnpf(xp%kms:xp%kme,n1:n2))
      allocate(pf(xp%kms:xp%kme+1,n1:n2))

      ! horizontal interpolate xb pressure to ob position for every xb levels
      do n=n1,n2
         do k=xp%kts,xp%kte
            ! convert to mb
            v_p(k,n) = 0.01*(iv%instid(inst)%loc_dym(n)*( &
               iv%instid(inst)%loc_dxm(n)*xb%p(iv%instid(inst)%loc_i(n),  iv%instid(inst)%loc_j(n),k) + &
               iv%instid(inst)%loc_dx(n) *xb%p(iv%instid(inst)%loc_i(n)+1,iv%instid(inst)%loc_j(n),k)) + &
               iv%instid(inst)%loc_dy(n) *( &
               iv%instid(inst)%loc_dxm(n)*xb%p(iv%instid(inst)%loc_i(n),  iv%instid(inst)%loc_j(n)+1,k) + &
               iv%instid(inst)%loc_dx(n) *xb%p(iv%instid(inst)%loc_i(n)+1,iv%instid(inst)%loc_j(n)+1,k)))
         end do
      end do
      call da_to_zk_new(pres, v_p(:,n1:n2), xp, v_interp_p, &
         iv%instid(inst)%zk(:,n1:n2), num_rad,nlevels )

do n=n1,n2
write (0,*) "zk",iv%instid(inst)%zk(:,n)
end do
      call da_togrid_new (iv%instid(inst)%zk(:,n1:n2), xp%kts, xp%kte,  &
         iv%instid(inst)%loc_k(:,n1:n2), &
         iv%instid(inst)%loc_dz(:,n1:n2), &
         iv%instid(inst)%loc_dzm(:,n1:n2))

      ! [1.2] Interpolate horizontally to ob:

      call da_interp_lin_3d_new( xb%t, xp, &
         iv%instid(inst)%loc_i(n1:n2), &
         iv%instid(inst)%loc_j(n1:n2), &
         iv%instid(inst)%loc_k(:,n1:n2), &
         iv%instid(inst)%loc_dx(n1:n2), &
         iv%instid(inst)%loc_dy(n1:n2), &
         iv%instid(inst)%loc_dz(:,n1:n2), &
         iv%instid(inst)%loc_dxm(n1:n2), &
         iv%instid(inst)%loc_dym(n1:n2), &
         iv%instid(inst)%loc_dzm(:,n1:n2), &
         iv%instid(inst)%t(:,n1:n2), nlevels, n1n2 )

      call da_interp_lin_3d_new( xb%q, xp, &
         iv%instid(inst)%loc_i(n1:n2), &
         iv%instid(inst)%loc_j(n1:n2), &
         iv%instid(inst)%loc_k(:,n1:n2), &
         iv%instid(inst)%loc_dx(n1:n2), &
         iv%instid(inst)%loc_dy(n1:n2), &
         iv%instid(inst)%loc_dz(:,n1:n2), &
         iv%instid(inst)%loc_dxm(n1:n2), &
         iv%instid(inst)%loc_dym(n1:n2), &
         iv%instid(inst)%loc_dzm(:,n1:n2), &
         iv%instid(inst)%q(:,n1:n2), nlevels, n1n2 )

      do n= n1,n2
         do k=1, nlevels
            if (iv%instid(inst)%zk(k,n) <= 0.0) then
               iv%instid(inst)%t(k,n) = coefs(inst) % ref_prfl_t(k,gas_id_watervapour)  ! outside model level
               iv%instid(inst)%mr(k,n) = coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
            else
               iv%instid(inst)%mr(k,n) = iv%instid(inst)%q(k,n) * q2ppmv
               ! call rttov_q2v (&
               !    gas_unit_specconc,  &! in
               !    iv%instid(inst)%q(k,n),         &! in
               !    gas_id_watervapour, &! in
               !    iv%instid(inst)%q(k,n),         &! in
               !    iv%instid(inst)%mr(k,n)     )    ! inout
            end if
            if (pres(k) < 100.0) iv%instid(inst)%mr(k,n) = coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
         end do

         ! determine surface type of obs location
         !-----------------------------------------
         call da_detsurtyp( xb%snow, xb%xice, xb%landmask,  &
            xb%ivgtyp, xb%isltyp, &
            xp%ims, xp%ime, xp%jms, xp%jme, &
            iv%instid(inst)%loc_i(n), iv%instid(inst)%loc_j(n), &
            iv%instid(inst)%loc_dx(n), iv%instid(inst)%loc_dy(n), &
            iv%instid(inst)%loc_dxm(n), iv%instid(inst)%loc_dym(n), &
            iv%instid(inst)%isflg(n),iv%instid(inst)%ivgtyp(n), iv%instid(inst)%isltyp(n))
      end do

      call da_interp_lin_2d_new( xb % u10, xp%ims, xp%ime, xp%jms, xp%jme, &
         iv%instid(inst)%loc_i(n1:n2), &
         iv%instid(inst)%loc_j(n1:n2), &
         iv%instid(inst)%loc_dx(n1:n2), &
         iv%instid(inst)%loc_dy(n1:n2), &
         iv%instid(inst)%loc_dxm(n1:n2), &
         iv%instid(inst)%loc_dym(n1:n2), &
         iv%instid(inst)%u10(n1:n2),n1n2 )
      call da_interp_lin_2d_new( xb % v10, xp%ims, xp%ime, xp%jms, xp%jme, &
         iv%instid(inst)%loc_i(n1:n2), &
         iv%instid(inst)%loc_j(n1:n2), &
         iv%instid(inst)%loc_dx(n1:n2), &
         iv%instid(inst)%loc_dy(n1:n2), &
         iv%instid(inst)%loc_dxm(n1:n2), &
         iv%instid(inst)%loc_dym(n1:n2), &
         iv%instid(inst)%v10(n1:n2),n1n2 )
      call da_interp_lin_2d_new( xb % t2, xp%ims, xp%ime, xp%jms, xp%jme, &
         iv%instid(inst)%loc_i(n1:n2), &
         iv%instid(inst)%loc_j(n1:n2), &
         iv%instid(inst)%loc_dx(n1:n2), &
         iv%instid(inst)%loc_dy(n1:n2), &
         iv%instid(inst)%loc_dxm(n1:n2), &
         iv%instid(inst)%loc_dym(n1:n2), &
         iv%instid(inst)%t2m(n1:n2) ,n1n2)
      call da_interp_lin_2d_new( xb % q2, xp%ims, xp%ime, xp%jms, xp%jme, &
         iv%instid(inst)%loc_i(n1:n2), &
         iv%instid(inst)%loc_j(n1:n2), &
         iv%instid(inst)%loc_dx(n1:n2), &
         iv%instid(inst)%loc_dy(n1:n2), &
         iv%instid(inst)%loc_dxm(n1:n2), &
         iv%instid(inst)%loc_dym(n1:n2), &
         iv%instid(inst)%q2m(n1:n2),n1n2 )
      call da_interp_lin_2d_new( xb % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
         iv%instid(inst)%loc_i(n1:n2), &
         iv%instid(inst)%loc_j(n1:n2), &
         iv%instid(inst)%loc_dx(n1:n2), &
         iv%instid(inst)%loc_dy(n1:n2), &
         iv%instid(inst)%loc_dxm(n1:n2), &
         iv%instid(inst)%loc_dym(n1:n2), &
         iv%instid(inst)%psfc(n1:n2),n1n2 )

      do n=n1,n2
         if (iv%instid(inst)%isflg(n) == 0) then   ! over sea using SST
            call da_interp_lin_2d( xb % tgrn, &
               xp%ims, xp%ime, xp%jms, xp%jme, &
               iv%instid(inst)%loc_i(n), &
               iv%instid(inst)%loc_j(n), &
               iv%instid(inst)%loc_dx(n), &
               iv%instid(inst)%loc_dy(n), &
               iv%instid(inst)%loc_dxm(n), &
               iv%instid(inst)%loc_dym(n), &
               iv%instid(inst)%ts(n))
         else
            call da_interp_lin_2d( xb % tsk, &
               xp%ims, xp%ime, xp%jms, xp%jme, &
               iv%instid(inst)%loc_i(n), &
               iv%instid(inst)%loc_j(n), &
               iv%instid(inst)%loc_dx(n), &
               iv%instid(inst)%loc_dy(n), &
               iv%instid(inst)%loc_dxm(n), &
               iv%instid(inst)%loc_dym(n), &
               iv%instid(inst)%ts(n))
         end if
      end do

      call da_interp_lin_2d_new(xb % terr, &
         xp%ims, xp%ime, xp%jms, xp%jme, &
         iv%instid(inst)%loc_i(n1:n2), &
         iv%instid(inst)%loc_j(n1:n2), &
         iv%instid(inst)%loc_dx(n1:n2), &
         iv%instid(inst)%loc_dy(n1:n2), &
         iv%instid(inst)%loc_dxm(n1:n2), &
         iv%instid(inst)%loc_dym(n1:n2), &
         iv%instid(inst)%elv(n1:n2),n1n2 )

      ! variables for emissivity calculations
      !---------------------------------------- 
      call da_interp_lin_2d_new( xb % smois, xp%ims, xp%ime, xp%jms, xp%jme, &
         iv%instid(inst)%loc_i(n1:n2), &
         iv%instid(inst)%loc_j(n1:n2), &
         iv%instid(inst)%loc_dx(n1:n2), &
         iv%instid(inst)%loc_dy(n1:n2), &
         iv%instid(inst)%loc_dxm(n1:n2), &
         iv%instid(inst)%loc_dym(n1:n2), &
         iv%instid(inst)%smois(n1:n2),n1n2)

      call da_interp_lin_2d_new( xb % tslb, xp%ims, xp%ime, xp%jms, xp%jme, &
         iv%instid(inst)%loc_i(n1:n2), &
         iv%instid(inst)%loc_j(n1:n2), &
         iv%instid(inst)%loc_dx(n1:n2), &
         iv%instid(inst)%loc_dy(n1:n2), &
         iv%instid(inst)%loc_dxm(n1:n2), &
         iv%instid(inst)%loc_dym(n1:n2), &
         iv%instid(inst)%tslb(n1:n2),n1n2)

      call da_interp_lin_2d_new( xb % snowh, xp%ims, xp%ime, xp%jms, xp%jme, &
         iv%instid(inst)%loc_i(n1:n2), &
         iv%instid(inst)%loc_j(n1:n2), &
         iv%instid(inst)%loc_dx(n1:n2), &
         iv%instid(inst)%loc_dy(n1:n2), &
         iv%instid(inst)%loc_dxm(n1:n2), &
         iv%instid(inst)%loc_dym(n1:n2), &
         iv%instid(inst)%snowh(n1:n2),n1n2)

      call da_interp_lin_2d_new( xb % vegfra, xp%ims, xp%ime, xp%jms, xp%jme, &
         iv%instid(inst)%loc_i(n1:n2), &
         iv%instid(inst)%loc_j(n1:n2), &
         iv%instid(inst)%loc_dx(n1:n2), &
         iv%instid(inst)%loc_dy(n1:n2), &
         iv%instid(inst)%loc_dxm(n1:n2), &
         iv%instid(inst)%loc_dym(n1:n2), &
         iv%instid(inst)%vegfra(n1:n2),n1n2)

      ! iv%instid(inst)%snowh(n1:n2) = iv%instid(inst)%snowh(n1:n2)*100.0   ! convert from m to mm
      iv%instid(inst)%vegfra(n1:n2) = 0.01*iv%instid(inst)%vegfra(n1:n2)  ! convert range to 0~1

      ! variables for calculation of cloud affected radiance
      !-------------------------------------------------------
      do k=xp%kts,xp%kte
         call da_interp_lin_2d_new( xb%t(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
            iv%instid(inst)%loc_i(n1:n2), &
            iv%instid(inst)%loc_j(n1:n2), &
            iv%instid(inst)%loc_dx(n1:n2), &
            iv%instid(inst)%loc_dy(n1:n2), &
            iv%instid(inst)%loc_dxm(n1:n2), &
            iv%instid(inst)%loc_dym(n1:n2), &
            iv%instid(inst)%tm(k,n1:n2),n1n2)

         call da_interp_lin_2d_new( xb%q(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
            iv%instid(inst)%loc_i(n1:n2), &
            iv%instid(inst)%loc_j(n1:n2), &
            iv%instid(inst)%loc_dx(n1:n2), &
            iv%instid(inst)%loc_dy(n1:n2), &
            iv%instid(inst)%loc_dxm(n1:n2), &
            iv%instid(inst)%loc_dym(n1:n2), &
            iv%instid(inst)%qm(k,n1:n2),n1n2)

         call da_interp_lin_2d_new( xb%qrn(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
            iv%instid(inst)%loc_i(n1:n2), &
            iv%instid(inst)%loc_j(n1:n2), &
            iv%instid(inst)%loc_dx(n1:n2), &
            iv%instid(inst)%loc_dy(n1:n2), &
            iv%instid(inst)%loc_dxm(n1:n2), &
            iv%instid(inst)%loc_dym(n1:n2), &
            iv%instid(inst)%qrn(k,n1:n2),n1n2)

         call da_interp_lin_2d_new( xb%qcw(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
            iv%instid(inst)%loc_i(n1:n2), &
            iv%instid(inst)%loc_j(n1:n2), &
            iv%instid(inst)%loc_dx(n1:n2), &
            iv%instid(inst)%loc_dy(n1:n2), &
            iv%instid(inst)%loc_dxm(n1:n2), &
            iv%instid(inst)%loc_dym(n1:n2), &
            iv%instid(inst)%qcw(k,n1:n2),n1n2)

         call da_interp_lin_2d_new( xb%qci(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
            iv%instid(inst)%loc_i(n1:n2), &
            iv%instid(inst)%loc_j(n1:n2), &
            iv%instid(inst)%loc_dx(n1:n2), &
            iv%instid(inst)%loc_dy(n1:n2), &
            iv%instid(inst)%loc_dxm(n1:n2), &
            iv%instid(inst)%loc_dym(n1:n2), &
            iv%instid(inst)%qci(k,n1:n2),n1n2)

         call da_interp_lin_2d_new( xb%qsn(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
            iv%instid(inst)%loc_i(n1:n2), &
            iv%instid(inst)%loc_j(n1:n2), &
            iv%instid(inst)%loc_dx(n1:n2), &
            iv%instid(inst)%loc_dy(n1:n2), &
            iv%instid(inst)%loc_dxm(n1:n2), &
            iv%instid(inst)%loc_dym(n1:n2), &
            iv%instid(inst)%qsn(k,n1:n2),n1n2)

         call da_interp_lin_2d_new( xb%qgr(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
            iv%instid(inst)%loc_i(n1:n2), &
            iv%instid(inst)%loc_j(n1:n2), &
            iv%instid(inst)%loc_dx(n1:n2), &
            iv%instid(inst)%loc_dy(n1:n2), &
            iv%instid(inst)%loc_dxm(n1:n2), &
            iv%instid(inst)%loc_dym(n1:n2), &
            iv%instid(inst)%qgr(k,n1:n2),n1n2)

         ! ADD for computing cloud liquid water path
         ! virtual T
         vtm (k,n1:n2) = (1.+0.608*iv%instid(inst)%qm(k,n1:n2))*iv%instid(inst)%tm(k,n1:n2)
         ! convert kg/kg to g/m3
         ! clw (k,n1:n2) = iv%instid(inst)%qcw(k,n1:n2)*v_p(k,n1:n2)*100.*0.0289644/8.31451/iv%instid(inst)%tm(k,n1:n2)
         clw(k,n1:n2) = 0.34836*iv%instid(inst)%qcw(k,n1:n2)*v_p(k,n1:n2)/iv%instid(inst)%tm(k,n1:n2)
         where (v_p(k,n1:n2)<100.0) clw (k,n1:n2) = 0.0

         iv%instid(inst)%psfc(n1:n2) = 0.01*iv%instid(inst)%psfc(n1:n2)           ! convert to hPa 
         iv%instid(inst)%mr2m(n1:n2) = iv%instid(inst)%q2m(n1:n2) * q2ppmv

      end do

      ! ADD for computing cloud liquid water path (mm) from guess

      pf(xp%kts,n1:n2) = 100.0*iv%instid(inst)%psfc(n1:n2)
      do k = 2,xp%kte
         ! pf(k,n1:n2) = 100.0*(v_p(k-1,n1:n2)+v_p(k,n1:n2))*0.5  ! full level pressure
         pf(k,n1:n2) = 50.0*(v_p(k-1,n1:n2)+v_p(k,n1:n2))
      end do
      ! pf(xp%kte+1,n1:n2)= 100.0*v_p(xp%kte,n1:n2)*0.5
      pf(xp%kte+1,n1:n2)= 50.0*v_p(xp%kte,n1:n2)

      iv%instid(inst)%clwp(n1:n2) = 0.0
      do k = xp%kts,xp%kte
         dlnpf(k,n1:n2) = log(pf(k,n1:n2)) - log(pf(k+1,n1:n2))
         ! clw  (k,n1:n2) = clw(k,n1:n2)*vtm(k,n1:n2)*287.05/9.80665*dlnpf(k,n1:n2)
         clw(k,n1:n2) = 29.27095*clw(k,n1:n2)*vtm(k,n1:n2)*dlnpf(k,n1:n2)
         iv%instid(inst)%clwp(n1:n2)  =iv%instid(inst)%clwp(n1:n2) + clw(k,n1:n2)
      end do
      ! iv%instid(inst)%clwp = 0.001*iv%instid(inst)%clwp   ! kg/m2 = mm

      ! call rttov_q2v (&
      !    gas_unit_specconc,  &! in
      !    iv%instid(inst)%q2m(n1:n2),          &! in
      !    gas_id_watervapour, &! in
      !    iv%instid(inst)%q2m(n1:n2),          &! in
      !    iv%instid(inst)%mr2m(n1:n2)    ! inout

      ! Compute surface emissivity for observation
      !-------------------------------------------


      do n=n1,n2
         knchan(n) = nchan
         do kn=1,nchan 
            kprof((n-1)*nchan+kn)  = n
            kchan((n-1)*nchan+kn)  = kn
            kochan((n-1)*nchan+kn) = kn
            indexn(kn,n) = (n-1)*nchan+kn 
         end do
      end do

      zasat(n1:n2) = iv%instid(inst)%satzen(n1:n2) * deg2rad
      zlsat(n1:n2) = iv%instid(inst)%satazi(n1:n2) * deg2rad

#ifdef RTTOV
      call rttov_setupchan(n1n2, nchan, coefs(inst), &   ! in
         nfrequencies, nchannels, nbtout )      ! out
#endif
      allocate (emissivity(nchannels))

      pemsv(:)    = 0.0
      pemsh(:)    = 0.0

      if (coefs(inst)%id_sensor == 2) then ! microwave
         if (amsua) then
            polar(1:4) = 0.0  ! polarisation: 0:vertical; 1:horizontal
            polar(5:6) = 1.0
            polar(7)   = 0.0
            polar(8:14)= 1.0
            polar(15)  = 0.0
         end if
         if (amsub) then
            polar(1:5) = 0.0
         end if
         call gsi_emiss(inst,knchpf,kprof,kchan,knchan,indexn, &
              iv%instid(inst)%satzen(n1:n2) * deg2rad, &
              iv%instid(inst)%satazi(n1:n2) * deg2rad, polar, &
              iv%instid(inst)%isflg(n1:n2),pemsv,pemsh,pems5, &
              iv%instid(inst)%ts(n1:n2), &
              iv%instid(inst)%isltyp(n1:n2), &
              iv%instid(inst)%tslb(n1:n2), &
              iv%instid(inst)%smois(n1:n2), &
              iv%instid(inst)%ivgtyp(n1:n2), &
              iv%instid(inst)%vegfra(n1:n2), &
              iv%instid(inst)%snowh(n1:n2), &
              sensor_type,n1n2,nchan, &
              iv%instid(inst)%tb_inv(:,n1:n2),amsua,amsub,ssmi, &
              iv%instid(inst)%u10(n1:n2), &
              iv%instid(inst)%v10(n1:n2))
      end if

      do n = n1,n2
         con_vars(n) % nlevels = nlevels 
         allocate (con_vars(n) % t(nlevels))
         allocate (con_vars(n) % q(nlevels))

         con_vars(n) % t(1:nlevels) = iv%instid(inst)%t(1:nlevels,n)
         con_vars(n) % q(1:nlevels) = iv%instid(inst)%mr(1:nlevels,n)
         con_vars(n) % ps  = iv%instid(inst)%psfc(n)

         aux_vars(n) % t2m      = iv%instid(inst)%t2m(n)
         aux_vars(n) % q2m      = iv%instid(inst)%mr2m(n)
         aux_vars(n) % u10      = iv%instid(inst)%u10(n)
         aux_vars(n) % v10      = iv%instid(inst)%v10(n)
         aux_vars(n) % surftype = iv%instid(inst)%landsea_mask(n) 
         aux_vars(n) % surft    = iv%instid(inst)%ts(n)
         aux_vars(n) % satzen   = iv%instid(inst)%satzen(n)
         aux_vars(n) % satazi   = iv%instid(inst)%satazi(n)
      end do

      ! [1.3] Call RTM forward model

      ! da_rttov_direct nominally an array version, but doesn't handle arrays
      ! of surface flags properly
      do n = n1,n2
         call da_rttov_direct (inst, iv%instid(inst)%isflg(n), nchan, 1, &
            nfrequencies, nchannels, nbtout, &
            con_vars(n:n), aux_vars(n:n), &
            iv%instid(inst)%tb(:,n), calcemis, pemsv, pemsh, emissivity )
         if (coefs(inst)%id_sensor == 2) then ! microwave
            iv%instid(inst)%emiss(1:nchan,n) = pems5(1:nchan)
         else
            iv%instid(inst)%emiss(1:nchan,n) = emissivity(1:nchan)
         end if
      end do

      iv%instid(inst)%tb_xb(:,n1:n2) = iv%instid(inst)%tb(:,n1:n2)

      do n=n1,n2
         deallocate (con_vars(n) % t)
         deallocate (con_vars(n) % q)
      end do

      !----------------------------------------------------------------
      ! [2.0] calculate components of innovation vector:
      !----------------------------------------------------------------

      where (iv%instid(inst)%tb_inv(1:nchan,n1:n2) /= missing_r)
         iv%instid(inst)%tb_inv(1:nchan,n1:n2) = &
            ob%instid(inst)%tb(1:nchan,n1:n2) - iv%instid(inst)%tb(1:nchan,n1:n2)
      elsewhere
         iv%instid(inst)%tb_inv(1:nchan,n1:n2)   = missing_r
      endwhere


      !----------------------------------------------------------------
      ! [3.0] Apply Bias Correction 
      !----------------------------------------------------------------

      if (biascorr) then
         do n=n1,n2
            call da_biascorr_rad_new(inst,ob,iv)
         end do
      end if

      deallocate(v_p)
      deallocate(vtm)
      deallocate(clw)
      deallocate(dlnpf)
      deallocate(pf)
   end do        ! end loop for sensor

   !------------------------------------------------------------------------
   ! [4.0] Perform QC check
   !------------------------------------------------------------------------
 
   if (qc_rad) then
      call da_qc_rad(ob, iv )
   end if

   do inst = 1, iv%num_inst
      if ( iv%instid(inst)%num_rad < 1 ) cycle
      call da_togrid_new (iv%instid(inst)%zk(:,n1:n2), xp%kts, xp%kte,  &
         iv%instid(inst)%loc_k(:,n1:n2), &
         iv%instid(inst)%loc_dz(:,n1:n2), &
         iv%instid(inst)%loc_dzm(:,n1:n2))
   end do

   !------------------------------------------
   ! 5.0 prepare bias statistics files
   !------------------------------------------

   if (biasprep) then
      do inst = 1, iv%num_inst
         write(unit=stdout,fmt='(A,A)') 'Preparing bias statistics files for ', &
            trim(iv%instid(inst)%rttovid_string)
         call da_biasprep(inst,ob,iv)
      end do
   end if

   if (trace_use) call da_trace_exit("da_get_innov_vector_rad_new")
#endif
 
end subroutine da_get_innov_vector_rad_new

