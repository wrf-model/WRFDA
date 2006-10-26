subroutine da_transform_xtoy_rad_adj ( iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! PURPOSE: transform gradient from obs space to model grid space.
   !
   ! METHOD:  jo_grad_x = H^T jo_grad_y =  - H^T R^-1 ( d - H delta_x )
   !           1. input gradient in obs space and reference state of RTTOV
   !           2. call adjoint of RTM
   !           3. adjoint of interpolation from model grid to obs loc
   !---------------------------------------------------------------------------

use da_interpolation, only : da_interp_lin_3d_adj_new

   IMPLICIT NONE

!#include "rttov_v2q.interface"

   TYPE (x_type), INTENT(INOUT)   :: jo_grad_x ! 
   TYPE (xpose_type), INTENT(IN)  :: xp        ! Domain decomposition vars.
   TYPE (y_type),  INTENT(IN)     :: jo_grad_y ! H' delta_x
   TYPE (ob_type), INTENT(IN)     :: iv        ! O-B structure.

   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
   INTEGER                        :: alloc_status(40)
   INTEGER :: status(mpi_status_size) ! MPI status.

   
   REAL, pointer                  :: model_mr(:)
   
   REAL, DIMENSION(xp%kms:xp%kme)   :: v_p      ! Model value p at ob hor. location.

   INTEGER            :: inst, nprofiles, nchanl, unit, iprof, ios
   REAL,    pointer   :: zk(:)
   REAL,    pointer   :: pres(:)
   character(len=30)  :: filename
   real,    pointer   :: tb(:,:)

   type(con_vars_type),  pointer  :: con_vars(:), con_vars_ad(:)
   type(aux_vars_type),  pointer  :: aux_vars(:)

   ! evenly distrbuted versions
   type(con_vars_type),  pointer  :: d_con_vars(:), d_con_vars_ad(:)
   type(aux_vars_type),  pointer  :: d_aux_vars(:)
   real,    pointer   :: d_tb(:,:)
   integer :: d_nprofiles, lprofiles,e,s,len,nl,ierr
   real, allocatable :: temp_t(:,:),temp_q(:,:), temp_ps(:), rtemp(:,:)
   integer, allocatable :: itemp(:,:)

#if !defined(RTTOV)
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $RTTOV option for radiances"/))
#else
   IF ( iv%num_inst < 1 ) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_rad_adj")

   alloc_status (:) = 0

   do inst = 1, iv%num_inst                 ! loop for sensor
      if ( iv%instid(inst)%num_rad < 1 ) cycle
      nprofiles = iv%instid(inst)%num_rad
      num_levs  = iv%instid(inst)%nlevels
      nchanl    = iv%instid(inst)%nchan

      allocate (zk(1:num_levs), stat= alloc_status(1))
      allocate (pres(1:num_levs), stat= alloc_status(2))
      allocate (model_mr(1:num_levs), stat= alloc_status(3))
      allocate (tb(nprofiles,nchanl), stat= alloc_status(4))
      allocate (con_vars(nprofiles), stat= alloc_status(5))
      allocate (con_vars_ad(nprofiles), stat= alloc_status(6))
      allocate (aux_vars(nprofiles), stat= alloc_status(7))

      if ( any(alloc_status /= 0) ) then
         call da_error(__FILE__,__LINE__, &
            (/"Canot allocate memory to con_vars"/))
      end if

      pres(1:num_levs) = coefs(inst) % ref_prfl_p(1:num_levs)

      !---------------------------------------------------------------
      ! [1.0] assign tb = R^-1 Re :
      !---------------------------------------------------------------

      do iprof=1,nprofiles
         do k = 1, nchanl
            tb(iprof,k) = jo_grad_y%instid(inst)%rad(iprof)%tb(k) 
         end do
      end do

      !---------------------------------------------
      ! [2.0] get input of da_rttov_ad
      !---------------------------------------------

      do iprof = 1, nprofiles
         con_vars(iprof) % nlevels = num_levs 

         allocate (con_vars(iprof) % t(num_levs))
         allocate (con_vars(iprof) % q(num_levs))

         allocate (con_vars_ad(iprof) % t(num_levs))
         allocate (con_vars_ad(iprof) % q(num_levs))

         con_vars(iprof) % t(:) = iv%instid(inst)%rad(iprof)%t (:)
         con_vars(iprof) % q(:) = iv%instid(inst)%rad(iprof)%mr(:)
         con_vars(iprof) % ps   = iv%instid(inst)%rad(iprof)%ps 

         aux_vars(iprof) % t2m = iv%instid(inst)%rad(iprof)%t2m
         aux_vars(iprof) % q2m = iv%instid(inst)%rad(iprof)%mr2m
         aux_vars(iprof) % u10 = iv%instid(inst)%rad(iprof)%u10
         aux_vars(iprof) % v10 = iv%instid(inst)%rad(iprof)%v10

         aux_vars(iprof) % surftype = iv%instid(inst)%rad(iprof)%landsea_mask
         aux_vars(iprof) % surft    = iv%instid(inst)%rad(iprof)%ts
         ! aux_vars(iprof) % fastem(:) = 0.

         aux_vars(iprof) % satzen  = iv%instid(inst)%rad(iprof)%satzen
         aux_vars(iprof) % satazi  = iv%instid(inst)%rad(iprof)%satazi
      end do

      !-------------------------------------------------
      ! [2.9] Distribute evenly across processors
      !-------------------------------------------------

!write (0,*) "tb(1,:)",tb(1,:)
!write (0,*) "con_vars(1)%nlevels",con_vars(1)%nlevels
!write (0,*) "aux_vars(1) % surftype",aux_vars(1) % surftype
!write (0,*) "con_vars(1) % ps",con_vars(1)%ps
!write (0,*) "aux_vars(1) % t2m",aux_vars(1)%t2m
!write (0,*) "aux_vars(1) % q2m",aux_vars(1)%q2m
!write (0,*) "aux_vars(1) % u10",aux_vars(1)%u10
!write (0,*) "aux_vars(1) % v10",aux_vars(1)%v10
!write (0,*) "aux_vars(1) % surft",aux_vars(1)%surft
!write (0,*) "aux_vars(1) % satzen",aux_vars(1)%satzen
!write (0,*) "aux_vars(1) % satazi",aux_vars(1)%satazi

      if (tovs_redistribute) then
! if (.false.) then
#ifdef DM_PARALLEL
         d_nprofiles=num_tovs_after(inst,myproc+1)

         allocate (d_con_vars(d_nprofiles))
         allocate (d_con_vars_ad(d_nprofiles))
         allocate (d_aux_vars(d_nprofiles))
         allocate (d_tb(d_nprofiles,nchanl))
         d_con_vars(:) % nlevels = num_levs

         do iprof = 1, d_nprofiles
            allocate (d_con_vars(iprof) % t(num_levs))
            allocate (d_con_vars(iprof) % q(num_levs))

            allocate (d_con_vars_ad(iprof) % t(num_levs))
            allocate (d_con_vars_ad(iprof) % q(num_levs))
         end do

         ! Fill up with data that stays local
         lprofiles=MIN(nprofiles,d_nprofiles)
         do iprof = 1, lprofiles
            d_con_vars(iprof) % t(:) = con_vars(iprof) % t(:) 
            d_con_vars(iprof) % q(:) = con_vars(iprof) % q(:)
            ! d_aux_vars(iprof) % fastem(:) = 0.
         end do

         d_con_vars(1:lprofiles) % nlevels  = con_vars(1:lprofiles) % nlevels
         d_con_vars(1:lprofiles) % ps       = con_vars(1:lprofiles) % ps
         d_aux_vars(1:lprofiles) % t2m      = aux_vars(1:lprofiles) % t2m
         d_aux_vars(1:lprofiles) % q2m      = aux_vars(1:lprofiles) % q2m
         d_aux_vars(1:lprofiles) % u10      = aux_vars(1:lprofiles) % u10
         d_aux_vars(1:lprofiles) % v10      = aux_vars(1:lprofiles) % v10
         d_aux_vars(1:lprofiles) % surftype = aux_vars(1:lprofiles) % surftype
         d_aux_vars(1:lprofiles) % surft    = aux_vars(1:lprofiles) % surft
         d_aux_vars(1:lprofiles) % satzen   = aux_vars(1:lprofiles) % satzen
         d_aux_vars(1:lprofiles) % satazi   = aux_vars(1:lprofiles) % satazi

         d_tb(1:lprofiles,:) = tb(1:lprofiles,:)

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
               call MPI_SEND( temp_t,nl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*8+1, MPI_COMM_WORLD, IERR )

               allocate(temp_q(num_levs,len))
               do j=1,len
                  temp_q(:,j)=con_vars(s+j-1) % q(:)
               end do
               call MPI_SEND( temp_q,nl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*8+2, MPI_COMM_WORLD, IERR )
  
               call MPI_SEND( tb(s:e,:),len*nchanl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*8+3, MPI_COMM_WORLD, IERR )

               allocate (rtemp(len,8))
               rtemp(:,1)= con_vars(s:e) % ps
               rtemp(:,2)= aux_vars(s:e) % t2m
               rtemp(:,3)= aux_vars(s:e) % q2m
               rtemp(:,4)= aux_vars(s:e) % u10
               rtemp(:,5)= aux_vars(s:e) % v10
               rtemp(:,6)= aux_vars(s:e) % surft
               rtemp(:,7)= aux_vars(s:e) % satzen
               rtemp(:,8)= aux_vars(s:e) % satazi
               call MPI_SEND(rtemp,len*8, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*8+4, MPI_COMM_WORLD, IERR )

               allocate (itemp(len,2))
               itemp(:,1)= con_vars(s:e) % nlevels
               itemp(:,2)= aux_vars(s:e) % surftype
               call MPI_SEND(itemp,len*2, MPI_INTEGER, tovs_recv_pe(inst,i), i*8+5, MPI_COMM_WORLD, IERR )

               deallocate (temp_t)
               deallocate (temp_q)
               deallocate (itemp)
               deallocate (rtemp)
            end if
            if (tovs_recv_pe(inst,i)==myproc) then
               s=tovs_recv_start(inst,i)
               len=tovs_send_count(inst,i)
               e=s+len-1
               nl=len*num_levs
               allocate(temp_t(num_levs,len))
               call MPI_RECV(temp_t,nl, TRUE_MPI_REAL, tovs_send_pe(inst,i), &
                  i*8+1, MPI_COMM_WORLD, status, IERR )
               do j=1,len
                  d_con_vars(s+j-1) % t(:)=temp_t(:,j)
               end do

               allocate(temp_q(num_levs,len))
               call MPI_RECV(temp_q,nl, TRUE_MPI_REAL, tovs_send_pe(inst,i), &
                  i*8+2, MPI_COMM_WORLD, status, IERR )
               do j=1,len
                  d_con_vars(s+j-1) % q(:)=temp_q(:,j)
               end do

               call MPI_RECV(d_tb(s:e,:),len*nchanl, TRUE_MPI_REAL, &
                  tovs_send_pe(inst,i), i*8+3, MPI_COMM_WORLD, status, IERR )

               allocate (rtemp(len,8))
               call MPI_RECV(rtemp,len*8, TRUE_MPI_REAL, &
                  tovs_send_pe(inst,i), i*8+4, MPI_COMM_WORLD, status, IERR )
               d_con_vars(s:e) % ps       = rtemp(:,1)
               d_aux_vars(s:e) % t2m      = rtemp(:,2)
               d_aux_vars(s:e) % q2m      = rtemp(:,3)
               d_aux_vars(s:e) % u10      = rtemp(:,4)
               d_aux_vars(s:e) % v10      = rtemp(:,5)
               d_aux_vars(s:e) % surft    = rtemp(:,6)
               d_aux_vars(s:e) % satzen   = rtemp(:,7)
               d_aux_vars(s:e) % satazi   = rtemp(:,8)

               allocate (itemp(len,2))
               call MPI_RECV(itemp,len*2, MPI_INTEGER, &
                  tovs_send_pe(inst,i), i*8+5, MPI_COMM_WORLD, status, IERR )
               d_con_vars(s:e) % nlevels  = itemp(:,1)
               d_aux_vars(s:e) % surftype = itemp(:,2)

               deallocate (temp_t)
               deallocate (temp_q)
               deallocate (rtemp)
               deallocate (itemp)
            end if
            call mpi_barrier(MPI_COMM_WORLD, IERR)
         end do
if (.false.) then
write (0,*) "d_tb(1,:)",d_tb(1,:)
write (0,*) "d_con_vars(1)%nlevels",d_con_vars(1)%nlevels
write (0,*) "d_aux_vars(1) % surftype",d_aux_vars(1) % surftype
write (0,*) "d_con_vars(1) % ps",d_con_vars(1)%ps
write (0,*) "d_aux_vars(1) % t2m",d_aux_vars(1)%t2m
write (0,*) "d_aux_vars(1) % q2m",d_aux_vars(1)%q2m
write (0,*) "d_aux_vars(1) % u10",d_aux_vars(1)%u10
write (0,*) "d_aux_vars(1) % v10",d_aux_vars(1)%v10
write (0,*) "d_aux_vars(1) % surft",d_aux_vars(1)%surft
write (0,*) "d_aux_vars(1) % satzen",d_aux_vars(1)%satzen
write (0,*) "d_aux_vars(1) % satazi",d_aux_vars(1)%satazi

write (0,*) "d_tb(d_nprofiles,:)",d_tb(d_nprofiles,:)
write (0,*) "d_con_vars(d_nprofiles)%nlevels",d_con_vars(d_nprofiles)%nlevels
write (0,*) "d_aux_vars(d_nprofiles) % surftype",d_aux_vars(d_nprofiles) % surftype
write (0,*) "d_con_vars(d_nprofiles) % ps",d_con_vars(d_nprofiles)%ps
write (0,*) "d_aux_vars(d_nprofiles) % t2m",d_aux_vars(d_nprofiles)%t2m
write (0,*) "d_aux_vars(d_nprofiles) % q2m",d_aux_vars(d_nprofiles)%q2m
write (0,*) "d_aux_vars(d_nprofiles) % u10",d_aux_vars(d_nprofiles)%u10
write (0,*) "d_aux_vars(d_nprofiles) % v10",d_aux_vars(d_nprofiles)%v10
write (0,*) "d_aux_vars(d_nprofiles) % surft",d_aux_vars(d_nprofiles)%surft
write (0,*) "d_aux_vars(d_nprofiles) % satzen",d_aux_vars(d_nprofiles)%satzen
write (0,*) "d_aux_vars(d_nprofiles) % satazi",d_aux_vars(d_nprofiles)%satazi
endif

         call da_rttov_ad (inst, nchanl, d_nprofiles, d_con_vars, &
                           d_aux_vars, d_con_vars_ad, d_tb )

         ! Transfer data back

         ! Return the local data
         do iprof = 1, lprofiles
            con_vars_ad(iprof) % t(:) = d_con_vars_ad(iprof) % t(:) 
            con_vars_ad(iprof) % q(:) = d_con_vars_ad(iprof) % q(:)
         end do

         con_vars_ad(1:lprofiles) % ps = d_con_vars_ad(1:lprofiles) % ps

         ! Return the data to other processors. Note the meaning of send_pe and recv_pe is
         ! swapped here

         nl=num_levs
         do i=1,tovs_copy_count(inst)
            if (tovs_recv_pe(inst,i)==myproc) then
               s=tovs_recv_start(inst,i)
               len=tovs_send_count(inst,i)
               e=s+len-1
               nl=len*num_levs
               allocate(temp_t(num_levs,len))
               do j=1,len
                  temp_t(:,j)=d_con_vars_ad(s+j-1) % t(:)
               end do
               call MPI_SEND( temp_t,nl, TRUE_MPI_REAL, tovs_send_pe(inst,i), i*8+6, MPI_COMM_WORLD, IERR )

               allocate(temp_q(num_levs,len))
               do j=1,len
                  temp_q(:,j)=d_con_vars_ad(s+j-1) % q(:)
               end do
               call MPI_SEND( temp_q,nl, TRUE_MPI_REAL, tovs_send_pe(inst,i), i*8+7, MPI_COMM_WORLD, IERR )

               allocate(rtemp(len,1))
               rtemp(:,1) = d_con_vars_ad(s:e) % ps
               call MPI_SEND(rtemp,len, TRUE_MPI_REAL, tovs_send_pe(inst,i), i*8+8, MPI_COMM_WORLD, IERR )

               deallocate (temp_t)
               deallocate (temp_q)
               deallocate (rtemp)
            end if
            if (tovs_send_pe(inst,i)==myproc) then
               s=tovs_send_start(inst,i)
               len=tovs_send_count(inst,i)
               e=s+len-1
               nl=len*num_levs
               allocate(temp_t(num_levs,len))
               call MPI_RECV(temp_t,nl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), &
               i*8+6, MPI_COMM_WORLD, status, IERR )
               do j=1,len
                  con_vars_ad(s+j-1) % t(:)=temp_t(:,j)
               end do

               allocate(temp_q(num_levs,len))
               call MPI_RECV(temp_q,nl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), &
                  i*8+7, MPI_COMM_WORLD, status, IERR )
               do j=1,len
                  con_vars_ad(s+j-1) % q(:)=temp_q(:,j)
               end do

               allocate(rtemp(len,1))
               call MPI_RECV(rtemp,len, TRUE_MPI_REAL, tovs_recv_pe(inst,i), &
                  i*8+8, MPI_COMM_WORLD, status, IERR )
               con_vars_ad(s:e) % ps=rtemp(:,1)
               deallocate (temp_t)
               deallocate (temp_q)
               deallocate (rtemp)
            end if
            call mpi_barrier(MPI_COMM_WORLD, IERR)
         end do
         do iprof=1,d_nprofiles
            deallocate (d_con_vars(iprof) % t)
            deallocate (d_con_vars(iprof) % q)
            deallocate (d_con_vars_ad(iprof) % t)
            deallocate (d_con_vars_ad(iprof) % q)
         end do
#endif
      else
         call da_rttov_ad (inst, nchanl, nprofiles, con_vars, &
                           aux_vars, con_vars_ad, tb )
      end if
if (.false.) then
write (0,*) "con_vars_ad(1) % t(:)",con_vars_ad(1) % t(:)
write (0,*) "con_vars_ad(1) % q(:)",con_vars_ad(1) % q(:)
write (0,*) "con_vars_ad(1) % ps",con_vars_ad(1) % ps
write (0,*) "con_vars_ad(nprofiles) % t(:)",con_vars_ad(nprofiles) % t(:)
write (0,*) "con_vars_ad(nprofiles) % q(:)",con_vars_ad(nprofiles) % q(:)
write (0,*) "con_vars_ad(nprofiles) % ps",con_vars_ad(nprofiles) % ps
!do iprof=1,nprofiles
!  write (0,*) iprof,"con_vars_ad(iprof) % t(1)",con_vars_ad(iprof) % t(1)
!  write (0,*) iprof,"con_vars_ad(iprof) % q(1)",con_vars_ad(iprof) % q(1)
!  write (0,*) iprof,"con_vars_ad(iprof) % t(num_levs)",con_vars_ad(iprof) % t(num_levs)
!  write (0,*) iprof,"con_vars_ad(iprof) % q(num_levs)",con_vars_ad(iprof) % q(num_levs)
!  write (0,*) iprof,"con_vars_ad(iprof) % ps",con_vars_ad(iprof) % ps
!end do
endif

      ! adjoint of convert to hPa

      con_vars_ad(:)% ps = con_vars_ad(:)%ps * 0.01 

      do iprof=1,nprofiles
         do k=1, num_levs
            zk(k)  =  iv%instid(inst)%rad(iprof)%zk(k) 
         enddo

         ! 4.2 scale transform 

         do k=1, num_levs
            model_mr(k) = con_vars_ad(iprof) % q(k)

            IF ( zk(k) <= 0.0 ) THEN
               con_vars_ad(iprof)%t(k)  = 0.0  !coefs(inst) % ref_prfl_t(k,gas_id_watervapour)
               con_vars_ad(iprof)%q(k)  = 0.0  !coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
            ELSE
               ! adjoint of q(kg/kg) to ppmv

               con_vars_ad(iprof)%q(k) = model_mr(k) * q2ppmv
               ! call rttov_v2q (&
               !    gas_unit_ppmv,      &! in
               !    model_mr(k),        &! in
               !    gas_id_watervapour, &! in
               !    model_mr(k),        &! in
               !    con_vars_ad(iprof)%q(k)     )    ! inout
            END IF
         end do
      end do

write (0,*) iprof,"sod con_vars_ad(iprof)% t",con_vars_ad(iprof)% t
      if (interp_new) then
         allocate(temp_t(num_levs,nprofiles))
         do iprof=1,nprofiles
            temp_t(:,iprof) = con_vars_ad(iprof)% t(:)
         end do
write (0,*) "temp_t(:,1)",temp_t(:,1)
write (0,*) "temp_t(:,2)",temp_t(:,2)
         call da_interp_lin_3d_adj_new( jo_grad_x%t, xp, &
            iv%instid(inst)%loc_i, &
            iv%instid(inst)%loc_j, &
            iv%instid(inst)%loc_k, &
            iv%instid(inst)%loc_dx, &
            iv%instid(inst)%loc_dy, &
            iv%instid(inst)%loc_dz, &
            iv%instid(inst)%loc_dxm, &
            iv%instid(inst)%loc_dym, &
            iv%instid(inst)%loc_dzm, &
            temp_t, max_ob_levels, num_levs,nprofiles )
write (0,*) "SUM(jo_grad_x%t)",SUM(jo_grad_x%t)
         deallocate(temp_t)
         allocate(temp_q(num_levs,nprofiles))
         do iprof=1,nprofiles
            temp_q(:,iprof) = con_vars_ad(iprof)% q(:)
         end do
         call da_interp_lin_3d_adj_new( jo_grad_x%q, xp, &
            iv%instid(inst)%loc_i, &
            iv%instid(inst)%loc_j, &
            iv%instid(inst)%loc_k, &
            iv%instid(inst)%loc_dx, &
            iv%instid(inst)%loc_dy, &
            iv%instid(inst)%loc_dz, &
            iv%instid(inst)%loc_dxm, &
            iv%instid(inst)%loc_dym, &
            iv%instid(inst)%loc_dzm, &
            temp_q, max_ob_levels, num_levs,nprofiles )
write (0,*) "SUM(jo_grad_x%q)",SUM(jo_grad_x%q)
         deallocate(temp_q)

         allocate(temp_ps(nprofiles))
         do iprof=1,nprofiles
            temp_ps(iprof) = con_vars_ad(iprof)% ps
         end do
write (0,*) "temp_ps", temp_ps
         call da_interp_lin_2d_adj_new(jo_grad_x% psfc, xp%ims, xp%ime, xp%jms, &
            xp%jme, &
            iv%instid(inst)%loc_i, &
            iv%instid(inst)%loc_j, &
            iv%instid(inst)%loc_dx, &
            iv%instid(inst)%loc_dy, &
            iv%instid(inst)%loc_dxm, &
            iv%instid(inst)%loc_dym, &
            temp_ps,nprofiles)
write (0,*) "SUM(jo_grad_x%psfc)",SUM(jo_grad_x%psfc)
         deallocate(temp_ps)
      else

         do iprof=1,nprofiles
            !-----------------------------------------------------
            ! [4.1] Get horizontal interpolation weights:
            !-----------------------------------------------------

            i = iv%instid(inst)%rad(iprof)%loc%i
            j = iv%instid(inst)%rad(iprof)%loc%j
            dx = iv%instid(inst)%rad(iprof)%loc%dx
            dy = iv%instid(inst)%rad(iprof)%loc%dy
            dxm = iv%instid(inst)%rad(iprof)%loc%dxm
            dym = iv%instid(inst)%rad(iprof)%loc%dym
         do k=1, num_levs
            zk(k)  =  iv%instid(inst)%rad(iprof)%zk(k) 
         enddo

write (0,*) "iprof",iprof
write (0,*) "i,j,dx,dy,dxm,dym",i,j,dx,dy,dxm,dym
write (0,*) "zk",zk

            ! [4.3] Interpolate horizontally from ob to grid:
write (0,*) iprof,"con_vars_ad(iprof)% t",con_vars_ad(iprof)% t
            call da_interp_lin_3d_adj( jo_grad_x%t, xp, i, j, dx, dy, dxm, dym, &
               con_vars_ad(iprof)% t, num_levs, zk, num_levs )
!               con_vars_ad(iprof)% t, max_ob_levels, zk, num_levs )
write (0,*) "SUM(jo_grad_x%t)",SUM(jo_grad_x%t)
            call da_interp_lin_3d_adj( jo_grad_x%q, xp, i, j, dx, dy, dxm, dym, &
               con_vars_ad(iprof)% q, num_levs, zk, num_levs )
!               con_vars_ad(iprof)% q, max_ob_levels, zk, num_levs )
write (0,*) "SUM(jo_grad_x%q)",SUM(jo_grad_x%q)

write(0,*) iprof, "con_vars_ad(iprof)% ps",con_vars_ad(iprof)% ps
            call da_interp_lin_2d_adj(jo_grad_x% psfc, xp%ims, xp%ime, xp%jms, &
               xp%jme, i, j, dx, dy, dxm, dym, con_vars_ad(iprof)% ps)
write (0,*) "SUM(jo_grad_x%psfc)",SUM(jo_grad_x%psfc)
            deallocate (con_vars(iprof) % t)
            deallocate (con_vars(iprof) % q)
            deallocate (con_vars_ad(iprof) % t)
            deallocate (con_vars_ad(iprof) % q)
         end do       !  end loop for pixels
      end if

      deallocate (zk)
      deallocate (pres)
      deallocate (model_mr)
      deallocate (tb)
      deallocate (con_vars)
      deallocate (aux_vars)
      deallocate (con_vars_ad)
      if (tovs_redistribute) then
!if (.false.) then
         deallocate (d_tb)
         deallocate (d_con_vars)
         deallocate (d_aux_vars)
         deallocate (d_con_vars_ad)
      end if
   end do        ! end loop for sensor

   if (trace_use) call da_trace_exit("da_transform_xtoy_rad_adj")
#endif
 
end subroutine da_transform_xtoy_rad_adj

