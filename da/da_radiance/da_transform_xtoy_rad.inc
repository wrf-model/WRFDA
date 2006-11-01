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
   INTEGER                        :: nlevels  ! Number of obs levels.
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
   INTEGER                        :: alloc_status(40)

   REAL, allocatable                :: model_t(:,:)  ! Model value t at ob location.
   REAL, allocatable                :: model_q(:,:)  ! Model value q(kg/kg) at ob location.
   REAL, allocatable                :: model_mr(:,:) ! Model value mr(ppmv) at ob location.
   REAL, allocatable                :: model_psfc(:)

   REAL, DIMENSION(xp%kms:xp%kme)   :: v_p      ! Model value p at ob hor. location.

   INTEGER            :: inst, num_rad, nchan, unit, n, ios, iscan
   REAL,    pointer   :: pres(:)
   character(len=30)  :: filename
   real,    pointer   :: tb(:,:)

   type(con_vars_type),  pointer  :: con_vars(:), con_vars_tl(:)
   type(aux_vars_type),  pointer  :: aux_vars(:), aux_vars_tl(:)

   ! evenly distrbuted versions
   type(con_vars_type),  pointer  :: d_con_vars(:), d_con_vars_tl(:)
   type(aux_vars_type),  pointer  :: d_aux_vars(:), d_aux_vars_tl(:)
   real,    pointer   :: d_tb(:,:)
   integer :: d_num_rad, l_num_rad,e,s,len,nl,ierr
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
      num_rad   = iv%instid(inst)%num_rad
      nlevels   = iv%instid(inst)%nlevels
      nchan     = iv%instid(inst)%nchan

      allocate (pres(1:nlevels), stat= alloc_status(2) )
      allocate (tb(num_rad,nchan), stat= alloc_status(3) )
      allocate (con_vars(num_rad), stat= alloc_status(4) )
      allocate (con_vars_tl(num_rad), stat= alloc_status(5) )
      allocate (aux_vars(num_rad), stat= alloc_status(6) )
      allocate (aux_vars_tl(num_rad), stat= alloc_status(7) )
      if ( any(alloc_status /= 0) ) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot allocate memory to con_vars"/))
      end if

      pres(1:nlevels) = coefs(inst) % ref_prfl_p(1:nlevels)

      allocate(model_t(nlevels,num_rad))
      allocate(model_q(nlevels,num_rad))
      allocate(model_mr(nlevels,num_rad))
      allocate(model_psfc(num_rad))
      model_t(:,:)=0.0
      model_q(:,:)=0.0
      model_mr(:,:)=0.0
      model_psfc(:)=0.0

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
           model_t, nlevels, num_rad)

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
           model_q, nlevels, num_rad )

         call da_interp_lin_2d_new( xa % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
            iv%instid(inst)%loc_i, iv%instid(inst)%loc_j, &
            iv%instid(inst)%loc_dx, iv%instid(inst)%loc_dy, &
            iv%instid(inst)%loc_dxm, iv%instid(inst)%loc_dym, &
            model_psfc,num_rad )
      else
         do n= 1, num_rad           ! loop for pixel
            
            ! [1.1] Get horizontal interpolation weights:

            i = iv%instid(inst)%rad(n)%loc%i
            j = iv%instid(inst)%rad(n)%loc%j
            dx = iv%instid(inst)%rad(n)%loc%dx
            dy = iv%instid(inst)%rad(n)%loc%dy
            dxm = iv%instid(inst)%rad(n)%loc%dxm
            dym = iv%instid(inst)%rad(n)%loc%dym

            ! [1.2] Interpolate horizontally to ob:

            call da_interp_lin_3d( xa%t, xp, i, j, dx, dy, dxm, dym, &
                              model_t(:,n), nlevels, iv%instid(inst)%rad(n)%zk, nlevels )
            call da_interp_lin_3d( xa%q, xp, i, j, dx, dy, dxm, dym, &
                              model_q(:,n), nlevels, iv%instid(inst)%rad(n)%zk, nlevels )
            call da_interp_lin_2d( xa % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_psfc(n) )
         end do
      end if
!write (0,*) "model_t(:,:)",model_t(:,:)
!write (0,*) "model_q(:,:)",model_q(:,:)
!write (0,*) "model_psfc(:)",model_psfc(:)

      model_psfc(:) = 0.01*model_psfc(:)  ! convert to hPa 
      con_vars(:) % nlevels = nlevels 
      con_vars_tl(:) % nlevels = nlevels
      aux_vars_tl(:) % t2m = 0.0
      aux_vars_tl(:) % q2m = 0.0
      aux_vars_tl(:) % u10 = 0.0
      aux_vars_tl(:) % v10 = 0.0
      ! aux_vars(:) % fastem(:) = 0.
      aux_vars_tl(:) % surft   = 0.0
      ! aux_vars_tl(:) % fastem(:) = 0.0

      do n=1,num_rad
         do k=1, nlevels
            IF ( iv%instid(inst)%rad(n)%zk(k) <= 0.0 ) THEN
               model_t(k,n)  = 0.0  !coefs(inst) % ref_prfl_t(k,gas_id_watervapour)
               model_mr(k,n) = 0.0  !coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
            ELSE
               ! model_mr(k,n) = model_q(k,n) * 1.60771704e+6 ! convert q(kg/kg) to ppmv 
               model_mr(k,n) = model_q(k,n) * q2ppmv
               ! call rttov_q2v ( &
               !    gas_unit_specconc,  &! in
               !    model_q(k,n),         &! in
               !    gas_id_watervapour, &! in
               !    model_q(k,n),         &! in
               !    model_mr(k,n)     )    ! inout
            END IF
         end do

         allocate (con_vars(n) % t(nlevels))
         allocate (con_vars(n) % q(nlevels))
         con_vars(n) % t(:) = iv%instid(inst)%rad(n)%t (:)
         con_vars(n) % q(:) = iv%instid(inst)%rad(n)%mr(:)
         con_vars(n) % ps   = iv%instid(inst)%rad(n)%ps 

         allocate (con_vars_tl(n) % t(nlevels))
         allocate (con_vars_tl(n) % q(nlevels))

         con_vars_tl(n) % t(:) = model_t  (:,n)
         con_vars_tl(n) % q(:) = model_mr (:,n)
         con_vars_tl(n) % ps   = model_psfc(n)

         aux_vars(n) % t2m = iv%instid(inst)%rad(n)%t2m
         aux_vars(n) % q2m = iv%instid(inst)%rad(n)%mr2m
         aux_vars(n) % u10 = iv%instid(inst)%rad(n)%u10
         aux_vars(n) % v10 = iv%instid(inst)%rad(n)%v10
         aux_vars(n) % surftype = iv%instid(inst)%rad(n)%landsea_mask
         aux_vars(n) % surft    = iv%instid(inst)%rad(n)%ts
         aux_vars(n) % satzen  = iv%instid(inst)%rad(n)%satzen
         aux_vars(n) % satazi  = iv%instid(inst)%rad(n)%satazi

         aux_vars_tl(n) % surftype = iv%instid(inst)%rad(n)%landsea_mask 
         aux_vars_tl(n) % satzen  = iv%instid(inst)%rad(n)%satzen
         aux_vars_tl(n) % satazi  = iv%instid(inst)%rad(n)%satazi
      end do

      deallocate(model_t)
      deallocate(model_q)
      deallocate(model_mr)
      deallocate(model_psfc)

      ! [1.3] Call RTM TL model
      if (tovs_redistribute) then
#ifdef DM_PARALLEL
         d_num_rad=num_tovs_after(inst,myproc+1)

         allocate (d_con_vars(d_num_rad))
         allocate (d_con_vars_tl(d_num_rad))
         allocate (d_aux_vars(d_num_rad))
         allocate (d_aux_vars_tl(d_num_rad))
         allocate (d_tb(d_num_rad,nchan))
         d_con_vars(:) % nlevels = nlevels

         do n = 1, d_num_rad
            allocate (d_con_vars(n) % t(nlevels))
            allocate (d_con_vars(n) % q(nlevels))

            allocate (d_con_vars_tl(n) % t(nlevels))
            allocate (d_con_vars_tl(n) % q(nlevels))
         end do

         ! Fill up with data that stays local
         l_num_rad=MIN(num_rad,d_num_rad)

         d_con_vars(1:l_num_rad) % nlevels  = con_vars(1:l_num_rad) % nlevels
         d_con_vars_tl(1:l_num_rad) % nlevels  = con_vars_tl(1:l_num_rad) % nlevels

         do n = 1, l_num_rad
            d_con_vars(n) % t(:)    = con_vars(n) % t(:) 
            d_con_vars(n) % q(:)    = con_vars(n) % q(:)
            d_con_vars_tl(n) % t(:) = con_vars_tl(n) % t(:) 
            d_con_vars_tl(n) % q(:) = con_vars_tl(n) % q(:)
            ! d_aux_vars(n) % fastem(:) = 0.
         end do

         d_con_vars(1:l_num_rad) % ps     = con_vars(1:l_num_rad) % ps
         d_con_vars_tl(1:l_num_rad) % ps  = con_vars_tl(1:l_num_rad) % ps

         d_aux_vars(1:l_num_rad) % t2m         = aux_vars(1:l_num_rad) % t2m
         d_aux_vars(1:l_num_rad) % q2m         = aux_vars(1:l_num_rad) % q2m
         d_aux_vars(1:l_num_rad) % u10         = aux_vars(1:l_num_rad) % u10
         d_aux_vars(1:l_num_rad) % v10         = aux_vars(1:l_num_rad) % v10
         d_aux_vars(1:l_num_rad) % surftype    = aux_vars(1:l_num_rad) % surftype
         d_aux_vars(1:l_num_rad) % surft       = aux_vars(1:l_num_rad) % surft
         ! d_aux_vars(1:l_num_rad) % fastem(:) = aux_vars(1:l_num_rad) % fastem(:)
         d_aux_vars(1:l_num_rad) % satzen      = aux_vars(1:l_num_rad) % satzen
         d_aux_vars(1:l_num_rad) % satazi      = aux_vars(1:l_num_rad) % satazi

         d_aux_vars_tl(1:l_num_rad) % t2m      = aux_vars_tl(1:l_num_rad) % t2m
         d_aux_vars_tl(1:l_num_rad) % q2m      = aux_vars_tl(1:l_num_rad) % q2m
         d_aux_vars_tl(1:l_num_rad) % u10      = aux_vars_tl(1:l_num_rad) % u10
         d_aux_vars_tl(1:l_num_rad) % v10      = aux_vars_tl(1:l_num_rad) % v10
         d_aux_vars_tl(1:l_num_rad) % surftype = aux_vars_tl(1:l_num_rad) % surftype
         d_aux_vars_tl(1:l_num_rad) % surft    = aux_vars_tl(1:l_num_rad) % surft
         ! d_aux_vars_tl(1:l_numrad) % fastem(:)    = aux_vars_tl(1:l_num_rad) % fastem(:)
         d_aux_vars_tl(1:l_num_rad) % satzen   = aux_vars_tl(1:l_num_rad) % satzen
         d_aux_vars_tl(1:l_num_rad) % satazi   = aux_vars_tl(1:l_num_rad) % satazi

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
               call MPI_SEND( temp_t,nl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*7+1, MPI_COMM_WORLD, IERR )

               allocate(temp_t_tl(nlevels,len))
               do j=1,len
                  temp_t_tl(:,j)=con_vars_tl(s+j-1) % t(:)
               end do
               call MPI_SEND( temp_t_tl,nl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*7+2, MPI_COMM_WORLD, IERR )

               allocate(temp_q(nlevels,len))
               do j=1,len
                  temp_q(:,j)=con_vars(s+j-1) % q(:)
               end do
               call MPI_SEND( temp_q,nl, TRUE_MPI_REAL, tovs_recv_pe(inst,i), i*7+3, MPI_COMM_WORLD, IERR )

               allocate(temp_q_tl(nlevels,len))
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
               nl=len*nlevels

               allocate(temp_t(nlevels,len))
               call MPI_RECV(temp_t,nl, TRUE_MPI_REAL, tovs_send_pe(inst,i), &
                  i*7+1, MPI_COMM_WORLD, status, IERR )
               do j=1,len
                  d_con_vars(s+j-1) % t(:)=temp_t(:,j)
               end do

               allocate(temp_t_tl(nlevels,len))
               call MPI_RECV(temp_t_tl,nl, TRUE_MPI_REAL, tovs_send_pe(inst,i), &
                  i*7+2, MPI_COMM_WORLD, status, IERR )
               do j=1,len
                  d_con_vars_tl(s+j-1) % t(:)=temp_t_tl(:,j)
               end do

               allocate(temp_q(nlevels,len))
               call MPI_RECV(temp_q,nl, TRUE_MPI_REAL, tovs_send_pe(inst,i), &
                  i*7+3, MPI_COMM_WORLD, status, IERR )
               do j=1,len
                  d_con_vars(s+j-1) % q(:)=temp_q(:,j)
               end do

               allocate(temp_q_tl(nlevels,len))
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


         call da_rttov_tl (inst, nchan, d_num_rad, d_con_vars, &
                           d_aux_vars, d_con_vars_tl, d_aux_vars_tl, d_tb )

         ! Transfer data back

         ! Return the local data
         tb(1:l_num_rad,:) = d_tb(1:l_num_rad,:)

         ! Return the data to other processors. Note the meaning of send_pe and recv_pe is
         ! swapped here

         nl=nlevels
         do i=1,tovs_copy_count(inst)
            if (tovs_recv_pe(inst,i)==myproc) then
               s=tovs_recv_start(inst,i)
               len=tovs_send_count(inst,i)
               e=s+len-1
               nl=len*nlevels
               call MPI_SEND(d_tb(s:e,:),len*nchan, TRUE_MPI_REAL, tovs_send_pe(inst,i), i*7+7, MPI_COMM_WORLD, IERR )
            end if
            if (tovs_send_pe(inst,i)==myproc) then
               s=tovs_send_start(inst,i)
               len=tovs_send_count(inst,i)
               e=s+len-1
               nl=len*nlevels
               call MPI_RECV(tb(s:e,:),len*nchan, TRUE_MPI_REAL, &
                  tovs_recv_pe(inst,i), i*7+7, MPI_COMM_WORLD, status, IERR )
            end if
         end do
         do n=1,d_num_rad
            deallocate (d_con_vars(n) % t)
            deallocate (d_con_vars(n) % q)
            deallocate (d_con_vars_tl(n) % t)
            deallocate (d_con_vars_tl(n) % q)
         end do
#endif
      else
         call da_rttov_tl (inst, nchan, num_rad, con_vars, aux_vars, &
                                  con_vars_tl, aux_vars_tl, tb )
      end if

      do n = 1, num_rad
         deallocate (con_vars(n) % t)
         deallocate (con_vars(n) % q)
         deallocate (con_vars_tl(n) % t)
         deallocate (con_vars_tl(n) % q)
         y%instid(inst)%rad(n)%tb(:) = tb(n,:)
      end do
      !-------------------------------------------------------------------
      ! [2.0] assign Hdx :
      !-------------------------------------------------------------------
      iscan = iv%instid(inst)%rad(n)%scanpos

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

