subroutine da_transform_xtoy_rad ( xa, iv, xp, y )

   !---------------------------------------------------------------------------
   !  PURPOSE: transform from analysis increment to 
   !                          pertubation radiance.
   !
   !  METHOD:  delta_y = H delta_x
   !           1. input reference state of RTTOV_TL
   !           2. interpolate analysis increment to obs location
   !           3. Call RTTOV_TL
   !
   !  HISTORY: 27/07/2005 - Creation            Zhiquan Liu
   !
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   !#include "rttov_q2v.interface"

   TYPE (x_type), INTENT(IN)      :: xa       ! delta_x
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (y_type),  INTENT(INOUT)  :: y        ! H' delta_x
   TYPE (ob_type), INTENT(IN)     :: iv       ! O-B structure.

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
   REAL                             :: model_u10
   REAL                             :: model_v10
   REAL                             :: model_psfc
   REAL                             :: model_ts

   REAL, DIMENSION(xp%kms:xp%kme)   :: v_p      ! Model value p at ob hor. location.

   INTEGER            :: inst, nprofiles, nchanl, unit, iprof, ios, iscan
   REAL,    pointer   :: zk(:)
   REAL,    pointer   :: pres(:)
   character(len=30)  :: filename
   real,    pointer   :: tb(:,:)

   type(con_vars_type),  pointer  :: con_vars(:), con_vars_tl(:)
   type(aux_vars_type),  pointer  :: aux_vars(:), aux_vars_tl(:)

   ! evenly distrbuted versions
   type(con_vars_type),  pointer  :: d_con_vars(:), d_con_vars_tl(:)
   type(aux_vars_type),  pointer  :: d_aux_vars(:), d_aux_vars_tl(:)
   real,    pointer   :: d_tb(:,:)
   integer :: d_nprofiles, lprofiles,e,s,len,nl,ierr
   real, allocatable :: temp_t(:,:),temp_q(:,:), temp_t_tl(:,:),temp_q_tl(:,:),rtemp(:,:)
   integer, allocatable :: itemp(:,:)
   INTEGER :: status(mpi_status_size) ! MPI status.

#if !defined(RTTOV)
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $RTTOV option for radiances"/))
#else

   IF ( iv%num_inst < 1 ) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_rad")

   alloc_status(:) = 0

   do inst = 1, iv%num_inst                 ! loop for sensor
      if ( iv%instid(inst)%num_rad < 1 ) cycle
      nprofiles = iv%instid(inst)%num_rad
      num_levs  = iv%instid(inst)%nlevels
      nchanl    = iv%instid(inst)%nchan

      allocate (zk(1:num_levs), stat= alloc_status(1) )
      allocate (pres(1:num_levs), stat= alloc_status(2) )
      allocate (tb(nprofiles,nchanl), stat= alloc_status(3) )
      allocate (con_vars(nprofiles), stat= alloc_status(4) )
      allocate (con_vars_tl(nprofiles), stat= alloc_status(5) )
      allocate (aux_vars(nprofiles), stat= alloc_status(6) )
      allocate (aux_vars_tl(nprofiles), stat= alloc_status(7) )
      if ( any(alloc_status /= 0) ) then
         call da_error(__FILE__,__LINE__, &
            (/"Canot allocate memory to con_vars"/))
      end if

      pres(1:num_levs) = coefs(inst) % ref_prfl_p(1:num_levs)


      if (interp_new) then
         call da_interp_lin_3d_new( xa%t, xp, &
           iv%instid(inst)%loc_i, &
           iv%instid(inst)%loc_j, &
           iv%instid(inst)%loc_k, &
           iv%instid(inst)%loc_dx, &
           iv%instid(inst)%loc_dy, &
           iv%instid(inst)%loc_dz, &
           iv%instid(inst)%loc_dxm, &
           iv%instid(inst)%loc_dym, &
           iv%instid(inst)%loc_dzm, &
           model_t, max_ob_levels, num_levs, nprofiles)
write(0,*) "SUM(model_t)",SUM(model_t)

         call da_interp_lin_3d_new( xa%q, xp, &
           iv%instid(inst)%loc_i, &
           iv%instid(inst)%loc_j, &
           iv%instid(inst)%loc_k, &
           iv%instid(inst)%loc_dx, &
           iv%instid(inst)%loc_dy, &
           iv%instid(inst)%loc_dz, &
           iv%instid(inst)%loc_dxm, &
           iv%instid(inst)%loc_dym, &
           iv%instid(inst)%loc_dzm, &
           model_q, max_ob_levels, num_levs, nprofiles )
write(0,*) "SUM(model_q)",SUM(model_q)

         call da_interp_lin_2d_new( xa % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
            iv%instid(inst)%loc_i, iv%instid(inst)%loc_j, &
            iv%instid(inst)%loc_dx, iv%instid(inst)%loc_dy, &
            iv%instid(inst)%loc_dxm, iv%instid(inst)%loc_dym, &
            model_psfc,nprofiles )
write(0,*) "model_psfc",model_psfc
      end if

      do iprof= 1, nprofiles           ! loop for pixel
         model_t(:) = 0.0
         model_q(:) = 0.0
         model_mr(:)= 0.0
         model_t2m  = 0.0
         model_q2m  = 0.0
         model_mr2m  = 0.0
         model_ts   = 0.0
         model_u10  = 0.0
         model_v10  = 0.0
         model_psfc = 0.0

         zk(1:num_levs)  =  iv%instid(inst)%rad(iprof)%zk(1:num_levs)

         if (.NOT. interp_new) then

            ! [1.1] Get horizontal interpolation weights:

            i = iv%instid(inst)%rad(iprof)%loc%i
            j = iv%instid(inst)%rad(iprof)%loc%j
            dx = iv%instid(inst)%rad(iprof)%loc%dx
            dy = iv%instid(inst)%rad(iprof)%loc%dy
            dxm = iv%instid(inst)%rad(iprof)%loc%dxm
            dym = iv%instid(inst)%rad(iprof)%loc%dym

            ! [1.2] Interpolate horizontally to ob:

            call da_interp_lin_3d( xa%t, xp, i, j, dx, dy, dxm, dym, &
                              model_t, max_ob_levels, zk, num_levs )
write(0,*) "SUM(model_t)",SUM(model_t)
            call da_interp_lin_3d( xa%q, xp, i, j, dx, dy, dxm, dym, &
                              model_q, max_ob_levels, zk, num_levs )
write(0,*) "SUM(model_q)",SUM(model_q)
         end if

         do k=1, num_levs
            IF ( zk(k) <= 0.0 ) THEN
               model_t(k)  = 0.  !coefs(inst) % ref_prfl_t(k,gas_id_watervapour)
               model_mr(k) = 0.  !coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
            ELSE
               ! model_mr(k) = model_q(k) * 1.60771704e+6 ! convert q(kg/kg) to ppmv 
               model_mr(k) = model_q(k) * q2ppmv
               ! call rttov_q2v ( &
               !    gas_unit_specconc,  &! in
               !    model_q(k),         &! in
               !    gas_id_watervapour, &! in
               !    model_q(k),         &! in
               !    model_mr(k)     )    ! inout
            END IF
         end do

         if (.not. interp_new) then
            call da_interp_lin_2d( xa % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_psfc )
write(0,*) "model_psfc",model_psfc
         end if
!if (iprof ==1) then
!   write (0,*) "xa % psfc",xa % psfc
!   write (0,*) "xp%ims, xp%ime, xp%jms, xp%jme",xp%ims, xp%ime, xp%jms, xp%jme
!   write (0,*) "i, j, dx, dy, dxm, dym",i, j, dx, dy, dxm, dym
!   write (0,*) "model_psfc",model_psfc
!end if

         model_psfc = 0.01*model_psfc  ! convert to hPa 

         con_vars(iprof) % nlevels = num_levs 
         con_vars_tl(iprof) % nlevels = num_levs

         allocate (con_vars(iprof) % t(num_levs))
         allocate (con_vars(iprof) % q(num_levs))
         con_vars(iprof) % t(:) = iv%instid(inst)%rad(iprof)%t (:)
         con_vars(iprof) % q(:) = iv%instid(inst)%rad(iprof)%mr(:)
         con_vars(iprof) % ps   = iv%instid(inst)%rad(iprof)%ps 

         allocate (con_vars_tl(iprof) % t(num_levs))
         allocate (con_vars_tl(iprof) % q(num_levs))

         con_vars_tl(iprof) % t(1:num_levs) = model_t  (1:num_levs)
         con_vars_tl(iprof) % q(1:num_levs) = model_mr (1:num_levs)
         con_vars_tl(iprof) % ps            = model_psfc

         aux_vars(iprof) % t2m = iv%instid(inst)%rad(iprof)%t2m
         aux_vars(iprof) % q2m = iv%instid(inst)%rad(iprof)%mr2m
         aux_vars(iprof) % u10 = iv%instid(inst)%rad(iprof)%u10
         aux_vars(iprof) % v10 = iv%instid(inst)%rad(iprof)%v10
         aux_vars(iprof) % surftype = iv%instid(inst)%rad(iprof)%landsea_mask
         aux_vars(iprof) % surft    = iv%instid(inst)%rad(iprof)%ts
         ! aux_vars(iprof) % fastem(:) = 0.
         aux_vars(iprof) % satzen  = iv%instid(inst)%rad(iprof)%satzen
         aux_vars(iprof) % satazi  = iv%instid(inst)%rad(iprof)%satazi

         aux_vars_tl(iprof) % t2m = model_t2m
         aux_vars_tl(iprof) % q2m = model_mr2m
         aux_vars_tl(iprof) % u10 = model_u10
         aux_vars_tl(iprof) % v10 = model_v10
         aux_vars_tl(iprof) % surftype = iv%instid(inst)%rad(iprof)%landsea_mask 
         aux_vars_tl(iprof) % surft   = model_ts
         ! aux_vars_tl(iprof) % fastem(:) = 0.
         aux_vars_tl(iprof) % satzen  = iv%instid(inst)%rad(iprof)%satzen
         aux_vars_tl(iprof) % satazi  = iv%instid(inst)%rad(iprof)%satazi
      end do



      ! [1.3] Call RTM TL model
      if (tovs_redistribute) then
#ifdef DM_PARALLEL
         d_nprofiles=num_tovs_after(inst,myproc+1)

         allocate (d_con_vars(d_nprofiles))
         allocate (d_con_vars_tl(d_nprofiles))
         allocate (d_aux_vars(d_nprofiles))
         allocate (d_aux_vars_tl(d_nprofiles))
         allocate (d_tb(d_nprofiles,nchanl))
         d_con_vars(:) % nlevels = num_levs

         do iprof = 1, d_nprofiles
            allocate (d_con_vars(iprof) % t(num_levs))
            allocate (d_con_vars(iprof) % q(num_levs))

            allocate (d_con_vars_tl(iprof) % t(num_levs))
            allocate (d_con_vars_tl(iprof) % q(num_levs))
         end do

         ! Fill up with data that stays local
         lprofiles=MIN(nprofiles,d_nprofiles)

         d_con_vars(1:lprofiles) % nlevels  = con_vars(1:lprofiles) % nlevels
         d_con_vars_tl(1:lprofiles) % nlevels  = con_vars_tl(1:lprofiles) % nlevels

         do iprof = 1, lprofiles
            d_con_vars(iprof) % t(:)    = con_vars(iprof) % t(:) 
            d_con_vars(iprof) % q(:)    = con_vars(iprof) % q(:)
            d_con_vars_tl(iprof) % t(:) = con_vars_tl(iprof) % t(:) 
            d_con_vars_tl(iprof) % q(:) = con_vars_tl(iprof) % q(:)
            ! d_aux_vars(iprof) % fastem(:) = 0.
         end do

         d_con_vars(1:lprofiles) % ps     = con_vars(1:lprofiles) % ps
         d_con_vars_tl(1:lprofiles) % ps  = con_vars_tl(1:lprofiles) % ps

         d_aux_vars(1:lprofiles) % t2m         = aux_vars(1:lprofiles) % t2m
         d_aux_vars(1:lprofiles) % q2m         = aux_vars(1:lprofiles) % q2m
         d_aux_vars(1:lprofiles) % u10         = aux_vars(1:lprofiles) % u10
         d_aux_vars(1:lprofiles) % v10         = aux_vars(1:lprofiles) % v10
         d_aux_vars(1:lprofiles) % surftype    = aux_vars(1:lprofiles) % surftype
         d_aux_vars(1:lprofiles) % surft       = aux_vars(1:lprofiles) % surft
         ! d_aux_vars(1:lprofiles) % fastem(:) = aux_vars(1:lprofiles) % fastem(:)
         d_aux_vars(1:lprofiles) % satzen      = aux_vars(1:lprofiles) % satzen
         d_aux_vars(1:lprofiles) % satazi      = aux_vars(1:lprofiles) % satazi

         d_aux_vars_tl(1:lprofiles) % t2m      = aux_vars_tl(1:lprofiles) % t2m
         d_aux_vars_tl(1:lprofiles) % q2m      = aux_vars_tl(1:lprofiles) % q2m
         d_aux_vars_tl(1:lprofiles) % u10      = aux_vars_tl(1:lprofiles) % u10
         d_aux_vars_tl(1:lprofiles) % v10      = aux_vars_tl(1:lprofiles) % v10
         d_aux_vars_tl(1:lprofiles) % surftype = aux_vars_tl(1:lprofiles) % surftype
         d_aux_vars_tl(1:lprofiles) % surft    = aux_vars_tl(1:lprofiles) % surft
         ! d_aux_vars_tl(1:lprofiles) % fastem(:)    = aux_vars_tl(1:lprofiles) % fastem(:)
         d_aux_vars_tl(1:lprofiles) % satzen   = aux_vars_tl(1:lprofiles) % satzen
         d_aux_vars_tl(1:lprofiles) % satazi   = aux_vars_tl(1:lprofiles) % satazi

         ! Get data from elsewhere

         do i=1,tovs_copy_count(inst)
            if (tovs_send_pe(inst,i)==myproc) then
               s=tovs_send_start(inst,i)
               len=tovs_send_count(inst,i)
               e=s+len-1
               nl=len*num_levs

               allocate(temp_t(num_levs,len))
               do j=1,len
                  temp_t(:,j)=con_vars(s+j-1) % t(:)
               end do
               call MPI_SEND( temp_t,nl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*7+1, MPI_COMM_WORLD, IERR )

               allocate(temp_t_tl(num_levs,len))
               do j=1,len
                  temp_t_tl(:,j)=con_vars_tl(s+j-1) % t(:)
               end do
               call MPI_SEND( temp_t_tl,nl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*7+2, MPI_COMM_WORLD, IERR )

               allocate(temp_q(num_levs,len))
               do j=1,len
                  temp_q(:,j)=con_vars(s+j-1) % q(:)
               end do
               call MPI_SEND( temp_q,nl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*7+3, MPI_COMM_WORLD, IERR )

               allocate(temp_q_tl(num_levs,len))
               do j=1,len
                  temp_q_tl(:,j)=con_vars_tl(s+j-1) % q(:)
               end do
               call MPI_SEND( temp_q_tl,nl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*7+4, MPI_COMM_WORLD, IERR )

               allocate (rtemp(len,16))
               rtemp(:,1)= con_vars(s:e) % ps
               rtemp(:,2)= con_vars_tl(s:e) % ps
               rtemp(:,3)= aux_vars(s:e) % t2m
               rtemp(:,4)= aux_vars(s:e) % q2m
               rtemp(:,5)= aux_vars(s:e) % u10
               rtemp(:,6)= aux_vars(s:e) % v10
               rtemp(:,7)= aux_vars(s:e) % surft
               rtemp(:,8)= aux_vars(s:e) % satzen
               rtemp(:,9)= aux_vars(s:e) % satazi
               rtemp(:,10)= aux_vars_tl(s:e) % t2m
               rtemp(:,11)= aux_vars_tl(s:e) % q2m
               rtemp(:,12)= aux_vars_tl(s:e) % u10
               rtemp(:,13)= aux_vars_tl(s:e) % v10
               rtemp(:,14)= aux_vars_tl(s:e) % surft
               rtemp(:,15)= aux_vars_tl(s:e) % satzen
               rtemp(:,16)= aux_vars_tl(s:e) % satazi
               call MPI_SEND(rtemp,len*16, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*7+5, MPI_COMM_WORLD, IERR )

               allocate (itemp(len,2))
               itemp(:,1)= con_vars(s:e) % nlevels  ! aux_vars_tl version identical
               itemp(:,2)= aux_vars(s:e) % surftype ! aux_vars_tl version identical
               call MPI_SEND(itemp,len*2, MPI_INTEGER, tovs_recv_pe(inst,i), i*7+6, MPI_COMM_WORLD, IERR )

               deallocate (temp_t)
               deallocate (temp_q)
               deallocate (temp_t_tl)
               deallocate (temp_q_tl)
               deallocate (rtemp)
               deallocate (itemp)
            end if
            if (tovs_recv_pe(inst,i)==myproc) then
               s=tovs_recv_start(inst,i)
               len=tovs_send_count(inst,i)
               e=s+len-1
               nl=len*num_levs

               allocate(temp_t(num_levs,len))
               call MPI_RECV(temp_t,nl, TRUE_MPI_REAL, tovs_send_pe(inst,i), &
                  i*7+1, MPI_COMM_WORLD, status, IERR )
               do j=1,len
                  d_con_vars(s+j-1) % t(:)=temp_t(:,j)
               end do

               allocate(temp_t_tl(num_levs,len))
               call MPI_RECV(temp_t_tl,nl, TRUE_MPI_REAL, tovs_send_pe(inst,i), &
                  i*7+2, MPI_COMM_WORLD, status, IERR )
               do j=1,len
                  d_con_vars_tl(s+j-1) % t(:)=temp_t_tl(:,j)
               end do

               allocate(temp_q(num_levs,len))
               call MPI_RECV(temp_q,nl, TRUE_MPI_REAL, tovs_send_pe(inst,i), &
                  i*7+3, MPI_COMM_WORLD, status, IERR )
               do j=1,len
                  d_con_vars(s+j-1) % q(:)=temp_q(:,j)
               end do

               allocate(temp_q_tl(num_levs,len))
               call MPI_RECV(temp_q_tl,nl, TRUE_MPI_REAL, tovs_send_pe(inst,i), &
                  i*7+4, MPI_COMM_WORLD, status, IERR )
               do j=1,len
                  d_con_vars_tl(s+j-1) % q(:)=temp_q_tl(:,j)
               end do

               allocate (rtemp(len,16))
               call MPI_RECV(rtemp,len*16, TRUE_MPI_REAL, tovs_send_pe(inst,i), &
                  i*7+5, MPI_COMM_WORLD, status, IERR )
               d_con_vars(s:e) % ps          = rtemp(:,1)
               d_con_vars_tl(s:e) % ps       = rtemp(:,2)
               d_aux_vars(s:e) % t2m         = rtemp(:,3)
               d_aux_vars(s:e) % q2m         = rtemp(:,4)
               d_aux_vars(s:e) % u10         = rtemp(:,5)
               d_aux_vars(s:e) % v10         = rtemp(:,6)
               d_aux_vars(s:e) % surft       = rtemp(:,7)
               d_aux_vars(s:e) % satzen      = rtemp(:,8)
               d_aux_vars(s:e) % satazi      = rtemp(:,9)
               d_aux_vars_tl(s:e) % t2m      = rtemp(:,10)
               d_aux_vars_tl(s:e) % q2m      = rtemp(:,11)
               d_aux_vars_tl(s:e) % u10      = rtemp(:,12)
               d_aux_vars_tl(s:e) % v10      = rtemp(:,13)
               d_aux_vars_tl(s:e) % surft    = rtemp(:,14)
               d_aux_vars_tl(s:e) % satzen   = rtemp(:,15)
               d_aux_vars_tl(s:e) % satazi   = rtemp(:,16)

               allocate (itemp(len,2))
               call MPI_RECV(itemp,len*2, MPI_INTEGER, tovs_send_pe(inst,i), &
                  i*7+6, MPI_COMM_WORLD, status, IERR )
               d_con_vars(s:e) % nlevels     = itemp(:,1)
               d_aux_vars(s:e) % surftype    = itemp(:,2)

               d_con_vars_tl(s:e) % nlevels  = d_con_vars(s:e) % nlevels
               d_aux_vars_tl(s:e) % surftype = d_aux_vars(s:e) % surftype

               deallocate (temp_t)
               deallocate (temp_q)
               deallocate (temp_t_tl)
               deallocate (temp_q_tl)
               deallocate (itemp)
               deallocate (rtemp)
            end if
         end do

if (.false.) then
write (0,*) "d_con_vars(1)%nlevels",d_con_vars(1)%nlevels
write (0,*) "d_aux_vars(1) % surftype",d_aux_vars(1) % surftype
write (0,*) "d_con_vars_tl(1) % ps",d_con_vars_tl(1)%ps
write (0,*) "d_con_vars(1) % ps",d_con_vars(1)%ps
write (0,*) "d_aux_vars(1) % t2m",d_aux_vars(1)%t2m
write (0,*) "d_aux_vars(1) % q2m",d_aux_vars(1)%q2m
write (0,*) "d_aux_vars(1) % u10",d_aux_vars(1)%u10
write (0,*) "d_aux_vars(1) % v10",d_aux_vars(1)%v10
write (0,*) "d_aux_vars(1) % surft",d_aux_vars(1)%surft
write (0,*) "d_aux_vars(1) % satzen",d_aux_vars(1)%satzen
write (0,*) "d_aux_vars(1) % satazi",d_aux_vars(1)%satazi
write (0,*) "d_aux_vars_tl(1) % t2m",d_aux_vars_tl(1)%t2m
write (0,*) "d_aux_vars_tl(1) % q2m",d_aux_vars_tl(1)%q2m
write (0,*) "d_aux_vars_tl(1) % u10",d_aux_vars_tl(1)%u10
write (0,*) "d_aux_vars_tl(1) % v10",d_aux_vars_tl(1)%v10
write (0,*) "d_aux_vars_tl(1) % surft",d_aux_vars_tl(1)%surft
write (0,*) "d_aux_vars_tl(1) % satzen",d_aux_vars_tl(1)%satzen
write (0,*) "d_aux_vars_tl(1) % satazi",d_aux_vars_tl(1)%satazi

write (0,*) "d_con_vars(d_nprofiles)%nlevels",d_con_vars(d_nprofiles)%nlevels
write (0,*) "d_aux_vars(d_nprofiles) % surftype",d_aux_vars(d_nprofiles) % surftype
write (0,*) "d_con_vars_tl(d_nprofiles) % ps",d_con_vars_tl(d_nprofiles)%ps
write (0,*) "d_con_vars(d_nprofiles) % ps",d_con_vars(d_nprofiles)%ps
write (0,*) "d_aux_vars(d_nprofiles) % t2m",d_aux_vars(d_nprofiles)%t2m
write (0,*) "d_aux_vars(d_nprofiles) % q2m",d_aux_vars(d_nprofiles)%q2m
write (0,*) "d_aux_vars(d_nprofiles) % u10",d_aux_vars(d_nprofiles)%u10
write (0,*) "d_aux_vars(d_nprofiles) % v10",d_aux_vars(d_nprofiles)%v10
write (0,*) "d_aux_vars(d_nprofiles) % surft",d_aux_vars(d_nprofiles)%surft
write (0,*) "d_aux_vars(d_nprofiles) % satzen",d_aux_vars(d_nprofiles)%satzen
write (0,*) "d_aux_vars(d_nprofiles) % satazi",d_aux_vars(d_nprofiles)%satazi
write (0,*) "d_aux_vars_tl(d_nprofiles) % t2m",d_aux_vars_tl(d_nprofiles)%t2m
write (0,*) "d_aux_vars_tl(d_nprofiles) % q2m",d_aux_vars_tl(d_nprofiles)%q2m
write (0,*) "d_aux_vars_tl(d_nprofiles) % u10",d_aux_vars_tl(d_nprofiles)%u10
write (0,*) "d_aux_vars_tl(d_nprofiles) % v10",d_aux_vars_tl(d_nprofiles)%v10
write (0,*) "d_aux_vars_tl(d_nprofiles) % surft",d_aux_vars_tl(d_nprofiles)%surft
write (0,*) "d_aux_vars_tl(d_nprofiles) % satzen",d_aux_vars_tl(d_nprofiles)%satzen
write (0,*) "d_aux_vars_tl(d_nprofiles) % satazi",d_aux_vars_tl(d_nprofiles)%satazi
endif

         call da_rttov_tl (inst, nchanl, d_nprofiles, d_con_vars, &
                           d_aux_vars, d_con_vars_tl, d_aux_vars_tl, d_tb )

         ! Transfer data back

         ! Return the local data
         tb(1:lprofiles,:) = d_tb(1:lprofiles,:)

         ! Return the data to other processors. Note the meaning of send_pe and recv_pe is
         ! swapped here

         nl=num_levs
         do i=1,tovs_copy_count(inst)
            if (tovs_recv_pe(inst,i)==myproc) then
               s=tovs_recv_start(inst,i)
               len=tovs_send_count(inst,i)
               e=s+len-1
               nl=len*num_levs
               call MPI_SEND(d_tb(s:e,:),len*nchanl, TRUE_MPI_REAL, tovs_send_pe(inst,i), i*7+7, MPI_COMM_WORLD, IERR )
            end if
            if (tovs_send_pe(inst,i)==myproc) then
               s=tovs_send_start(inst,i)
               len=tovs_send_count(inst,i)
               e=s+len-1
               nl=len*num_levs
               call MPI_RECV(tb(s:e,:),len*nchanl, TRUE_MPI_REAL, &
                  tovs_recv_pe(inst,i), i*7+7, MPI_COMM_WORLD, status, IERR )
            end if
         end do
         do iprof=1,d_nprofiles
            deallocate (d_con_vars(iprof) % t)
            deallocate (d_con_vars(iprof) % q)
            deallocate (d_con_vars_tl(iprof) % t)
            deallocate (d_con_vars_tl(iprof) % q)
         end do
#endif
      else
         call da_rttov_tl (inst, nchanl, nprofiles, con_vars, aux_vars, &
                                  con_vars_tl, aux_vars_tl, tb )
      end if

write (0,*) "tb(1,:)",tb(1,:)
write (0,*) "tb(nprofiles,:)",tb(nprofiles,:)

      do iprof = 1, nprofiles
         deallocate (con_vars(iprof) % t)
         deallocate (con_vars(iprof) % q)
         deallocate (con_vars_tl(iprof) % t)
         deallocate (con_vars_tl(iprof) % q)
         y%instid(inst)%rad(iprof)%tb(:) = tb(iprof,:)
      end do
      !-------------------------------------------------------------------
      ! [2.0] assign Hdx :
      !-------------------------------------------------------------------
      iscan = iv%instid(inst)%rad(iprof)%scanpos

      deallocate (zk)
      deallocate (pres)
      deallocate (tb)
      deallocate (con_vars)
      deallocate (aux_vars)
      deallocate (con_vars_tl)
      deallocate (aux_vars_tl)
      if (tovs_redistribute) then
         deallocate (d_tb)
         deallocate (d_con_vars)
         deallocate (d_aux_vars)
         deallocate (d_aux_vars_tl)
         deallocate (d_con_vars_tl)
      end if

   end do        ! end loop for sensor

   if (trace_use) call da_trace_exit("da_transform_xtoy_rad")
#endif

end subroutine da_transform_xtoy_rad

