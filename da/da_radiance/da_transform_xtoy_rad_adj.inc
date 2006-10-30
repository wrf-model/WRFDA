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
   INTEGER                        :: nlevels ! Number of obs levels.
   INTEGER                        :: num_rad  ! Number of radiance obs
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
   INTEGER                        :: alloc_status(40)
   INTEGER :: status(mpi_status_size) ! MPI status.

   
   REAL, pointer                  :: model_mr(:)
   
   REAL, DIMENSION(xp%kms:xp%kme)   :: v_p      ! Model value p at ob hor. location.

   INTEGER            :: inst, nchan, unit, n, ios
   REAL,    pointer   :: pres(:)
   character(len=30)  :: filename
   real,    pointer   :: tb(:,:)

   type(con_vars_type),  pointer  :: con_vars(:), con_vars_ad(:)
   type(aux_vars_type),  pointer  :: aux_vars(:)

   ! evenly distrbuted versions
   type(con_vars_type),  pointer  :: d_con_vars(:), d_con_vars_ad(:)
   type(aux_vars_type),  pointer  :: d_aux_vars(:)
   real,    pointer   :: d_tb(:,:)
   integer :: d_num_rad, l_num_rad,e,s,len,nl,ierr
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
      num_rad = iv%instid(inst)%num_rad
      nlevels  = iv%instid(inst)%nlevels
      nchan    = iv%instid(inst)%nchan

      allocate (pres(1:nlevels), stat= alloc_status(2))
      allocate (model_mr(1:nlevels), stat= alloc_status(3))
      allocate (tb(num_rad,nchan), stat= alloc_status(4))
      allocate (con_vars(num_rad), stat= alloc_status(5))
      allocate (con_vars_ad(num_rad), stat= alloc_status(6))
      allocate (aux_vars(num_rad), stat= alloc_status(7))

      if ( any(alloc_status /= 0) ) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot allocate memory to con_vars"/))
      end if

      tb(:,:) = 0.0

      pres(1:nlevels) = coefs(inst) % ref_prfl_p(1:nlevels)

      !---------------------------------------------------------------
      ! [1.0] assign tb = R^-1 Re :
      !---------------------------------------------------------------

      do n=1,num_rad
         do k = 1, nchan
            tb(n,k) = jo_grad_y%instid(inst)%rad(n)%tb(k) 
         end do
      end do

      !---------------------------------------------
      ! [2.0] get input of da_rttov_ad
      !---------------------------------------------

      do n = 1, num_rad
         con_vars(n) % nlevels = nlevels 

         allocate (con_vars(n)    % t(nlevels))
         allocate (con_vars(n)    % q(nlevels))
         allocate (con_vars_ad(n) % t(nlevels))
         allocate (con_vars_ad(n) % q(nlevels))

         con_vars(n)    % t(:) = 0.0
         con_vars(n)    % q(:) = 0.0
         con_vars_ad(n) % t(:) = 0.0
         con_vars_ad(n) % q(:) = 0.0

         con_vars(n) % t(:) = iv%instid(inst)%rad(n)%t (:)
         con_vars(n) % q(:) = iv%instid(inst)%rad(n)%mr(:)
         con_vars(n) % ps   = iv%instid(inst)%rad(n)%ps 

         aux_vars(n) % t2m = iv%instid(inst)%rad(n)%t2m
         aux_vars(n) % q2m = iv%instid(inst)%rad(n)%mr2m
         aux_vars(n) % u10 = iv%instid(inst)%rad(n)%u10
         aux_vars(n) % v10 = iv%instid(inst)%rad(n)%v10

         aux_vars(n) % surftype = iv%instid(inst)%rad(n)%landsea_mask
         aux_vars(n) % surft    = iv%instid(inst)%rad(n)%ts
         ! aux_vars(n) % fastem(:) = 0.

         aux_vars(n) % satzen  = iv%instid(inst)%rad(n)%satzen
         aux_vars(n) % satazi  = iv%instid(inst)%rad(n)%satazi
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
#ifdef DM_PARALLEL
         d_num_rad=num_tovs_after(inst,myproc+1)

         allocate (d_con_vars(d_num_rad))
         allocate (d_con_vars_ad(d_num_rad))
         allocate (d_aux_vars(d_num_rad))
         allocate (d_tb(d_num_rad,nchan))
         d_con_vars(:) % nlevels = nlevels

         do n = 1, d_num_rad
            allocate (d_con_vars(n) % t(nlevels))
            allocate (d_con_vars(n) % q(nlevels))
            allocate (d_con_vars_ad(n) % t(nlevels))
            allocate (d_con_vars_ad(n) % q(nlevels))
         end do

         ! Fill up with data that stays local
         l_num_rad=MIN(num_rad,d_num_rad)
         do n = 1, l_num_rad
            d_con_vars(n) % t(:) = con_vars(n) % t(:) 
            d_con_vars(n) % q(:) = con_vars(n) % q(:)
            ! d_aux_vars(n) % fastem(:) = 0.
         end do

         d_con_vars(1:l_num_rad) % nlevels  = con_vars(1:l_num_rad) % nlevels
         d_con_vars(1:l_num_rad) % ps       = con_vars(1:l_num_rad) % ps
         d_aux_vars(1:l_num_rad) % t2m      = aux_vars(1:l_num_rad) % t2m
         d_aux_vars(1:l_num_rad) % q2m      = aux_vars(1:l_num_rad) % q2m
         d_aux_vars(1:l_num_rad) % u10      = aux_vars(1:l_num_rad) % u10
         d_aux_vars(1:l_num_rad) % v10      = aux_vars(1:l_num_rad) % v10
         d_aux_vars(1:l_num_rad) % surftype = aux_vars(1:l_num_rad) % surftype
         d_aux_vars(1:l_num_rad) % surft    = aux_vars(1:l_num_rad) % surft
         d_aux_vars(1:l_num_rad) % satzen   = aux_vars(1:l_num_rad) % satzen
         d_aux_vars(1:l_num_rad) % satazi   = aux_vars(1:l_num_rad) % satazi

         d_tb(1:l_num_rad,:) = tb(1:l_num_rad,:)

         ! Get data from elsewhere

         do i=1,tovs_copy_count(inst)
            if (tovs_send_pe(inst,i)==myproc) then
               s=tovs_send_start(inst,i)
               len=tovs_send_count(inst,i)
               e=s+len-1
               nl=len*nlevels
               allocate(temp_t(nlevels,len))
               do j=1,len
                  temp_t(:,j)=con_vars(s+j-1) % t(:)
               end do
               call MPI_SEND( temp_t,nl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*8+1, MPI_COMM_WORLD, IERR )

               allocate(temp_q(nlevels,len))
               do j=1,len
                  temp_q(:,j)=con_vars(s+j-1) % q(:)
               end do
               call MPI_SEND( temp_q,nl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*8+2, MPI_COMM_WORLD, IERR )
  
               call MPI_SEND( tb(s:e,:),len*nchan, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*8+3, MPI_COMM_WORLD, IERR )

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
               nl=len*nlevels
               allocate(temp_t(nlevels,len))
               call MPI_RECV(temp_t,nl, TRUE_MPI_REAL, tovs_send_pe(inst,i), &
                  i*8+1, MPI_COMM_WORLD, status, IERR )
               do j=1,len
                  d_con_vars(s+j-1) % t(:)=temp_t(:,j)
               end do

               allocate(temp_q(nlevels,len))
               call MPI_RECV(temp_q,nl, TRUE_MPI_REAL, tovs_send_pe(inst,i), &
                  i*8+2, MPI_COMM_WORLD, status, IERR )
               do j=1,len
                  d_con_vars(s+j-1) % q(:)=temp_q(:,j)
               end do

               call MPI_RECV(d_tb(s:e,:),len*nchan, TRUE_MPI_REAL, &
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

!write (0,*) "d_tb(1,:)",d_tb(1,:)
!write (0,*) "d_con_vars(1)%nlevels",d_con_vars(1)%nlevels
!write (0,*) "d_aux_vars(1) % surftype",d_aux_vars(1) % surftype
!write (0,*) "d_con_vars(1) % ps",d_con_vars(1)%ps
!write (0,*) "d_aux_vars(1) % t2m",d_aux_vars(1)%t2m
!write (0,*) "d_aux_vars(1) % q2m",d_aux_vars(1)%q2m
!write (0,*) "d_aux_vars(1) % u10",d_aux_vars(1)%u10
!write (0,*) "d_aux_vars(1) % v10",d_aux_vars(1)%v10
!write (0,*) "d_aux_vars(1) % surft",d_aux_vars(1)%surft
!write (0,*) "d_aux_vars(1) % satzen",d_aux_vars(1)%satzen
!write (0,*) "d_aux_vars(1) % satazi",d_aux_vars(1)%satazi

!write (0,*) "d_tb(d_num_rad,:)",d_tb(d_num_rad,:)
!write (0,*) "d_con_vars(d_num_rad)%nlevels",d_con_vars(d_num_rad)%nlevels
!write (0,*) "d_aux_vars(d_num_rad) % surftype",d_aux_vars(d_num_rad) % surftype
!write (0,*) "d_con_vars(d_num_rad) % ps",d_con_vars(d_num_rad)%ps
!write (0,*) "d_aux_vars(d_num_rad) % t2m",d_aux_vars(d_num_rad)%t2m
!write (0,*) "d_aux_vars(d_num_rad) % q2m",d_aux_vars(d_num_rad)%q2m
!write (0,*) "d_aux_vars(d_num_rad) % u10",d_aux_vars(d_num_rad)%u10
!write (0,*) "d_aux_vars(d_num_rad) % v10",d_aux_vars(d_num_rad)%v10
!write (0,*) "d_aux_vars(d_num_rad) % surft",d_aux_vars(d_num_rad)%surft
!write (0,*) "d_aux_vars(d_num_rad) % satzen",d_aux_vars(d_num_rad)%satzen
!write (0,*) "d_aux_vars(d_num_rad) % satazi",d_aux_vars(d_num_rad)%satazi

         call da_rttov_ad (inst, nchan, d_num_rad, d_con_vars, &
                           d_aux_vars, d_con_vars_ad, d_tb )

         ! Transfer data back

         ! Return the local data
         do n = 1, l_num_rad
            con_vars_ad(n) % t(:) = d_con_vars_ad(n) % t(:) 
            con_vars_ad(n) % q(:) = d_con_vars_ad(n) % q(:)
         end do

         con_vars_ad(1:l_num_rad) % ps = d_con_vars_ad(1:l_num_rad) % ps

         ! Return the data to other processors. Note the meaning of send_pe and recv_pe is
         ! swapped here

         nl=nlevels
         do i=1,tovs_copy_count(inst)
            if (tovs_recv_pe(inst,i)==myproc) then
               s=tovs_recv_start(inst,i)
               len=tovs_send_count(inst,i)
               e=s+len-1
               nl=len*nlevels
               allocate(temp_t(nlevels,len))
               do j=1,len
                  temp_t(:,j)=d_con_vars_ad(s+j-1) % t(:)
               end do
               call MPI_SEND( temp_t,nl, TRUE_MPI_REAL, tovs_send_pe(inst,i), i*8+6, MPI_COMM_WORLD, IERR )

               allocate(temp_q(nlevels,len))
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
               nl=len*nlevels
               allocate(temp_t(nlevels,len))
               call MPI_RECV(temp_t,nl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), &
               i*8+6, MPI_COMM_WORLD, status, IERR )
               do j=1,len
                  con_vars_ad(s+j-1) % t(:)=temp_t(:,j)
               end do

               allocate(temp_q(nlevels,len))
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
         do n=1,d_num_rad
            deallocate (d_con_vars(n) % t)
            deallocate (d_con_vars(n) % q)
            deallocate (d_con_vars_ad(n) % t)
            deallocate (d_con_vars_ad(n) % q)
         end do
#endif
      else
!do n=1,num_rad
!   write (0,*) n,"before rttov_ad con_vars_ad(n)% t",con_vars_ad(n)% t
!end do
!write (0,*) "sum(tb)",sum(tb)
!write (0,*) "inst,nchan,num_rad",inst,nchan,num_rad
!write (0,*) "con_vars(:) % ps",con_vars(:) % ps
!write (0,*) "aux_vars(:) % t2m",aux_vars(:) % t2m
!write (0,*) "aux_vars(:) % q2m",aux_vars(:) % q2m
!write (0,*) "aux_vars(:) % u10",aux_vars(:) % u10
!write (0,*) "aux_vars(:) % v10",aux_vars(:) % v10
!write (0,*) "aux_vars(:) % surft",aux_vars(:) % surft
!write (0,*) "aux_vars(:) % satzen",aux_vars(:) % satzen
!write (0,*) "aux_vars(:) % satazi",aux_vars(:) % satazi
         call da_rttov_ad (inst, nchan, num_rad, con_vars, &
                           aux_vars, con_vars_ad, tb )
      end if
!write (0,*) "con_vars_ad(1) % t(:)",con_vars_ad(1) % t(:)
!write (0,*) "con_vars_ad(1) % q(:)",con_vars_ad(1) % q(:)
!write (0,*) "con_vars_ad(1) % ps",con_vars_ad(1) % ps
!write (0,*) "con_vars_ad(num_rad) % t(:)",con_vars_ad(num_rad) % t(:)
!write (0,*) "con_vars_ad(num_rad) % q(:)",con_vars_ad(num_rad) % q(:)
!write (0,*) "con_vars_ad(num_rad) % ps",con_vars_ad(num_rad) % ps
!do n=1,num_rad
!  write (0,*) n,"con_vars_ad(n) % t(1)",con_vars_ad(n) % t(1)
!  write (0,*) n,"con_vars_ad(n) % q(1)",con_vars_ad(n) % q(1)
!  write (0,*) n,"con_vars_ad(n) % t(nlevels)",con_vars_ad(n) % t(nlevels)
!  write (0,*) n,"con_vars_ad(n) % q(nlevels)",con_vars_ad(n) % q(nlevels)
!  write (0,*) n,"con_vars_ad(n) % ps",con_vars_ad(n) % ps
!end do

      ! adjoint of convert to hPa

      con_vars_ad(:)% ps = con_vars_ad(:)%ps * 0.01 

      do n=1,num_rad

         ! 4.2 scale transform 

         do k=1, nlevels
            model_mr(k) = con_vars_ad(n) % q(k)

            IF ( iv%instid(inst)%rad(n)%zk(k) <= 0.0 ) THEN
               con_vars_ad(n)%t(k)  = 0.0  !coefs(inst) % ref_prfl_t(k,gas_id_watervapour)
               con_vars_ad(n)%q(k)  = 0.0  !coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
            ELSE
               ! adjoint of q(kg/kg) to ppmv

               con_vars_ad(n)%q(k) = model_mr(k) * q2ppmv
               ! call rttov_v2q (&
               !    gas_unit_ppmv,      &! in
               !    model_mr(k),        &! in
               !    gas_id_watervapour, &! in
               !    model_mr(k),        &! in
               !    con_vars_ad(n)%q(k)     )    ! inout
            END IF
         end do
      end do

      if (interp_new) then

         allocate(temp_t(nlevels,num_rad))
         do n=1,num_rad
            temp_t(:,n) = con_vars_ad(n)% t(:)
         end do
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
            temp_t, nlevels,num_rad )
         deallocate(temp_t)

         allocate(temp_q(nlevels,num_rad))
         do n=1,num_rad
            temp_q(:,n) = con_vars_ad(n)% q(:)
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
            temp_q, nlevels,num_rad )
         deallocate(temp_q)

         allocate(temp_ps(num_rad))
         do n=1,num_rad
            temp_ps(n) = con_vars_ad(n)% ps
         end do
         call da_interp_lin_2d_adj_new(jo_grad_x% psfc, xp%ims, xp%ime, xp%jms, &
            xp%jme, &
            iv%instid(inst)%loc_i, &
            iv%instid(inst)%loc_j, &
            iv%instid(inst)%loc_dx, &
            iv%instid(inst)%loc_dy, &
            iv%instid(inst)%loc_dxm, &
            iv%instid(inst)%loc_dym, &
            temp_ps,num_rad)
         deallocate(temp_ps)
      else
         do n=1,num_rad
            !-----------------------------------------------------
            ! [4.1] Get horizontal interpolation weights:
            !-----------------------------------------------------

            i = iv%instid(inst)%rad(n)%loc%i
            j = iv%instid(inst)%rad(n)%loc%j
            dx = iv%instid(inst)%rad(n)%loc%dx
            dy = iv%instid(inst)%rad(n)%loc%dy
            dxm = iv%instid(inst)%rad(n)%loc%dxm
            dym = iv%instid(inst)%rad(n)%loc%dym

            ! [4.3] Interpolate horizontally from ob to grid:
            call da_interp_lin_3d_adj( jo_grad_x%t, xp, i, j, dx, dy, dxm, dym, &
               con_vars_ad(n)% t,  max_ob_levels, iv%instid(inst)%rad(n)%zk, nlevels )

            call da_interp_lin_3d_adj( jo_grad_x%q, xp, i, j, dx, dy, dxm, dym, &
               con_vars_ad(n)% q, max_ob_levels, iv%instid(inst)%rad(n)%zk, nlevels )

            call da_interp_lin_2d_adj(jo_grad_x% psfc, xp%ims, xp%ime, xp%jms, &
               xp%jme, i, j, dx, dy, dxm, dym, con_vars_ad(n)% ps)

            deallocate (con_vars(n) % t)
            deallocate (con_vars(n) % q)
            deallocate (con_vars_ad(n) % t)
            deallocate (con_vars_ad(n) % q)
         end do       !  end loop for pixels
      end if

!do n=1,num_rad
!write (0,*) n,"SUM(con_vars_ad(n)% t)",SUM(con_vars_ad(n)% t)
!write (0,*) n,"SUM(con_vars_ad(n)% q)",SUM(con_vars_ad(n)% q)
!write (0,*) n,"con_vars_ad(n)% ps",con_vars_ad(n)% ps
!end do

      deallocate (pres)
      deallocate (model_mr)
      deallocate (tb)
      deallocate (con_vars)
      deallocate (aux_vars)
      deallocate (con_vars_ad)
      if (tovs_redistribute) then
         deallocate (d_tb)
         deallocate (d_con_vars)
         deallocate (d_aux_vars)
         deallocate (d_con_vars_ad)
      end if
   end do        ! end loop for sensor

   if (trace_use) call da_trace_exit("da_transform_xtoy_rad_adj")
#endif
 
end subroutine da_transform_xtoy_rad_adj

