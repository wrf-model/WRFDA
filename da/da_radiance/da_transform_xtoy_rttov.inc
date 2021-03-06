subroutine da_transform_xtoy_rttov (grid, iv, y )

   !---------------------------------------------------------------------------
   !  Purpose: transform from analysis increment to 
   !                          pertubation radiance.
   !
   !  METHOD:  delta_y = H delta_x
   !           1. input reference state of RTTOV_TL
   !           2. interpolate analysis increment to obs location
   !           3. Call RTTOV_TL
   !---------------------------------------------------------------------------

   implicit none

   !#include "rttov_q2v.interface"

   type (domain),     intent(in)     :: grid
   type (y_type),     intent(inout)  :: y        ! H' delta_x
   type (iv_type),    intent(in)     :: iv       ! O-B structure.

#ifdef RTTOV

   integer                        :: i,j,k,c  ! Index dimension.
   integer                        :: nlevels  ! Number of obs levels.

   real, allocatable :: model_t(:,:)  ! Model value t at ob location.
   real, allocatable :: model_q(:,:)  ! Model value q(kg/kg) at ob location.
   real, allocatable :: model_mr(:,:) ! Model value mr(ppmv) at ob location.
   real, allocatable :: model_psfc(:)

   integer            :: num_rad, nchan, n

   type(con_vars_type),  allocatable :: con_vars(:), con_vars_tl(:)
   type(aux_vars_type),  allocatable :: aux_vars(:), aux_vars_tl(:)

   ! evenly distributed versions
   type(con_vars_type),  allocatable :: d_con_vars(:), d_con_vars_tl(:)
   type(aux_vars_type),  allocatable :: d_aux_vars(:), d_aux_vars_tl(:)
   real, allocatable :: d_tb(:,:)

   integer :: d_num_rad, l_num_rad,e,s,len,nl
   real, allocatable :: temp_t(:,:),temp_q(:,:)
   real, allocatable :: temp_t_tl(:,:),temp_q_tl(:,:),rtemp(:,:)
   real, allocatable :: temp_tb(:,:)
   integer, allocatable :: itemp(:,:)

#ifdef DM_PARALLEL
   integer :: status(mpi_status_size) ! MPI status.
#endif

   if (iv%num_inst < 1) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_rttov")

   do i= 1, iv%num_inst                 ! loop for sensor

      if (iv%instid(i)%num_rad_glo < 1) cycle

      num_rad   = iv%instid(i)%num_rad
      nlevels   = iv%instid(i)%nlevels
      nchan     = iv%instid(i)%nchan

      if (num_rad > 0) then
         allocate (con_vars(num_rad))
         allocate (con_vars_tl(num_rad))
         allocate (aux_vars(num_rad))
         allocate (aux_vars_tl(num_rad))

         allocate(model_t(nlevels,num_rad))
         allocate(model_q(nlevels,num_rad))
         allocate(model_mr(nlevels,num_rad))
         allocate(model_psfc(num_rad))
         model_t(:,:)=0.0
         model_q(:,:)=0.0
         model_mr(:,:)=0.0
         model_psfc(:)=0.0

         call da_interp_lin_3d (grid%xa%t, iv%instid(i)%info, model_t)
         call da_interp_lin_3d (grid%xa%q, iv%instid(i)%info, model_q)

         call da_interp_lin_2d (grid%xa%psfc, iv%instid(i)%info, 1, model_psfc)

         model_psfc(:) = 0.01*model_psfc(:)  ! convert to hPa 
         con_vars(:) % nlevels = nlevels 
         con_vars_tl(:) % nlevels = nlevels
         aux_vars_tl(:) % t2m = 0.0
         aux_vars_tl(:) % q2m = 0.0
         aux_vars_tl(:) % u10 = 0.0
         aux_vars_tl(:) % v10 = 0.0
         ! aux_vars(:) % fastem(:) = 0.0
         aux_vars_tl(:) % surft   = 0.0
         ! aux_vars_tl(:) % fastem(:) = 0.0

         do n=1,num_rad
            do k=1, nlevels
               if ( iv%instid(i)%info%zk(k,n) <= 0.0 ) then
                  model_t(k,n)  = 0.0  !coefs(i) % ref_prfl_t(k,gas_id_watervapour)
                  model_mr(k,n) = 0.0  !coefs(i) % ref_prfl_mr(k,gas_id_watervapour)
               else
                  ! model_mr(k,n) = model_q(k,n) * 1.60771704e+6 ! convert q(kg/kg) to ppmv 
                  model_mr(k,n) = model_q(k,n) * q2ppmv
                  ! call rttov_q2v ( &
                  !    gas_unit_specconc,  &! in
                  !    model_q(k,n),         &! in
                  !    gas_id_watervapour, &! in
                  !    model_q(k,n),         &! in
                  !    model_mr(k,n)     )    ! inout
               end if
            end do

            allocate (con_vars(n) % t(nlevels))
            allocate (con_vars(n) % q(nlevels))
            con_vars(n) % t(:) = iv%instid(i)%t (:,n)
            con_vars(n) % q(:) = iv%instid(i)%mr(:,n)
            con_vars(n) % ps   = iv%instid(i)%ps(n) 

            allocate (con_vars_tl(n) % t(nlevels))
            allocate (con_vars_tl(n) % q(nlevels))

            con_vars_tl(n) % t(:) = model_t  (:,n)
            con_vars_tl(n) % q(:) = model_mr (:,n)
            con_vars_tl(n) % ps   = model_psfc(n)

            aux_vars(n) % t2m = iv%instid(i)%t2m(n)
            aux_vars(n) % q2m = iv%instid(i)%mr2m(n)
            aux_vars(n) % u10 = iv%instid(i)%u10(n)
            aux_vars(n) % v10 = iv%instid(i)%v10(n)
            aux_vars(n) % surftype = iv%instid(i)%landsea_mask(n)
            aux_vars(n) % surft    = iv%instid(i)%ts(n)
            aux_vars(n) % satzen  = iv%instid(i)%satzen(n)
            aux_vars(n) % satazi  = iv%instid(i)%satazi(n)

            aux_vars_tl(n) % surftype = iv%instid(i)%landsea_mask(n) 
            aux_vars_tl(n) % satzen  = iv%instid(i)%satzen(n)
            aux_vars_tl(n) % satazi  = iv%instid(i)%satazi(n)
         end do

         deallocate(model_t)
         deallocate(model_q)
         deallocate(model_mr)
         deallocate(model_psfc)
      end if

      ! [1.3] Call RTM TL model

      d_num_rad=num_tovs_after(i,myproc+1)

      if (d_num_rad > 0) then
         allocate (d_con_vars(d_num_rad))
         allocate (d_con_vars_tl(d_num_rad))
         allocate (d_aux_vars(d_num_rad))
         allocate (d_aux_vars_tl(d_num_rad))
         allocate (d_tb(nchan,d_num_rad))

         d_con_vars(:) % nlevels = nlevels

         do n = 1, d_num_rad
            allocate (d_con_vars(n) % t(nlevels))
            allocate (d_con_vars(n) % q(nlevels))

            allocate (d_con_vars_tl(n) % t(nlevels))
            allocate (d_con_vars_tl(n) % q(nlevels))
         end do
      end if

      ! Fill up with data that stays local
      l_num_rad=Min(num_rad,d_num_rad)

      if (l_num_rad > 0) then
         d_con_vars(1:l_num_rad) % nlevels  = con_vars(1:l_num_rad) % nlevels
         d_con_vars_tl(1:l_num_rad) % nlevels  = con_vars_tl(1:l_num_rad) % nlevels

         do n = 1, l_num_rad
            d_con_vars(n) % t(:)    = con_vars(n) % t(:) 
            d_con_vars(n) % q(:)    = con_vars(n) % q(:)
            d_con_vars_tl(n) % t(:) = con_vars_tl(n) % t(:) 
            d_con_vars_tl(n) % q(:) = con_vars_tl(n) % q(:)
            ! d_aux_vars(n) % fastem(:) = 0.0
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
      end if

      ! Get data from elsewhere
#ifdef DM_PARALLEL
      do c=1,tovs_copy_count(i)
         if (tovs_send_pe(i,c)==myproc) then
            s=tovs_send_start(i,c)
            len=tovs_send_count(i,c)
            e=s+len-1
            nl=len*nlevels

            allocate(temp_t(nlevels,len))
            do j=1,len
               temp_t(:,j)=con_vars(s+j-1) % t(:)
            end do
            call mpi_send( temp_t,nl, true_mpi_real, tovs_recv_pe(i,c), &
               c*7+1, comm, ierr)

            allocate(temp_t_tl(nlevels,len))
            do j=1,len
               temp_t_tl(:,j)=con_vars_tl(s+j-1) % t(:)
            end do
            call mpi_send( temp_t_tl,nl, true_mpi_real, tovs_recv_pe(i,c), &
               c*7+2, comm, ierr)

            allocate(temp_q(nlevels,len))
            do j=1,len
               temp_q(:,j)=con_vars(s+j-1) % q(:)
            end do
            call mpi_send (temp_q,nl, true_mpi_real, tovs_recv_pe(i,c), &
               c*7+3, comm, ierr)

            allocate(temp_q_tl(nlevels,len))
            do j=1,len
               temp_q_tl(:,j)=con_vars_tl(s+j-1) % q(:)
            end do
            call mpi_send (temp_q_tl,nl, true_mpi_real, tovs_recv_pe(i,c), &
               c*7+4, comm, ierr)

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
            call mpi_send (rtemp,len*16, true_mpi_real, tovs_recv_pe(i,c), &
               c*7+5, comm, ierr)

            allocate (itemp(len,2))
            itemp(:,1)= con_vars(s:e) % nlevels  ! aux_vars_tl identical
            itemp(:,2)= aux_vars(s:e) % surftype ! aux_vars_tl identical
            call mpi_send (itemp,len*2, mpi_integer, tovs_recv_pe(i,c), &
               c*7+6, comm, ierr)

            deallocate (temp_t)
            deallocate (temp_q)
            deallocate (temp_t_tl)
            deallocate (temp_q_tl)
            deallocate (rtemp)
            deallocate (itemp)
         end if
         if (tovs_recv_pe(i,c)==myproc) then
            s=tovs_recv_start(i,c)
            len=tovs_send_count(i,c)
            e=s+len-1
            nl=len*nlevels

            allocate(temp_t(nlevels,len))
            call mpi_recv (temp_t,nl, true_mpi_real, tovs_send_pe(i,c), &
               c*7+1, comm, status, ierr)
            do j=1,len
               d_con_vars(s+j-1) % t(:)=temp_t(:,j)
            end do

            allocate(temp_t_tl(nlevels,len))
            call mpi_recv (temp_t_tl,nl, true_mpi_real, tovs_send_pe(i,c), &
               c*7+2, comm, status, ierr)
            do j=1,len
               d_con_vars_tl(s+j-1) % t(:)=temp_t_tl(:,j)
            end do

            allocate(temp_q(nlevels,len))
            call mpi_recv (temp_q,nl, true_mpi_real, tovs_send_pe(i,c), &
               c*7+3, comm, status, ierr)
            do j=1,len
               d_con_vars(s+j-1) % q(:)=temp_q(:,j)
            end do

            allocate(temp_q_tl(nlevels,len))
            call mpi_recv (temp_q_tl,nl, true_mpi_real, tovs_send_pe(i,c), &
               c*7+4, comm, status, ierr)
            do j=1,len
               d_con_vars_tl(s+j-1) % q(:)=temp_q_tl(:,j)
            end do

            allocate (rtemp(len,16))
            call mpi_recv (rtemp,len*16, true_mpi_real, tovs_send_pe(i,c), &
               c*7+5, comm, status, ierr)
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
            call mpi_recv (itemp,len*2, mpi_integer, tovs_send_pe(i,c), &
               c*7+6, comm, status, ierr)
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
#endif
      if (d_num_rad > 0) then
         if (tovs_batch) then
            call da_rttov_tl (i, nchan, d_num_rad, d_con_vars, &
               d_aux_vars, d_con_vars_tl, d_aux_vars_tl, d_tb)
         else
            do n=1,d_num_rad
               call da_rttov_tl (i, nchan, 1, d_con_vars(n:n), &
                  d_aux_vars(n:n), d_con_vars_tl(n:n), d_aux_vars_tl(n:n), &
                  d_tb(:,n:n))
            end do
         end if
      end if

      ! Transfer data back

      ! Return the local data
      if (l_num_rad > 0) then
         y%instid(i)%tb(:,1:l_num_rad) = d_tb(:,1:l_num_rad)
      end if

      ! Return the data to other processors. Note the meaning of send_pe and recv_pe is
      ! swapped here

      nl=nlevels
#ifdef DM_PARALLEL
      do c=1,tovs_copy_count(i)
         if (tovs_recv_pe(i,c)==myproc) then
            s=tovs_recv_start(i,c)
            len=tovs_send_count(i,c)
            e=s+len-1
            nl=len*nlevels
            allocate(temp_tb(nchan,len))
            temp_tb(:,:) = d_tb(:,s:e)
            call mpi_send (temp_tb,len*nchan, true_mpi_real, tovs_send_pe(i,c), c*7+7, comm, ierr)
            deallocate(temp_tb)
         end if
         if (tovs_send_pe(i,c)==myproc) then
            s=tovs_send_start(i,c)
            len=tovs_send_count(i,c)
            e=s+len-1
            nl=len*nlevels
            allocate(temp_tb(nchan,len))
            call mpi_recv (temp_tb,len*nchan, true_mpi_real, &
               tovs_recv_pe(i,c), c*7+7, comm, status, ierr)
            y%instid(i)%tb(:,s:e)=temp_tb(:,:)
            deallocate(temp_tb)
         end if
      end do
#endif

      if (d_num_rad > 0) then
         do n=1,d_num_rad
            deallocate (d_con_vars(n) % t)
            deallocate (d_con_vars(n) % q)
            deallocate (d_con_vars_tl(n) % t)
            deallocate (d_con_vars_tl(n) % q)
         end do

         deallocate (d_tb)
         deallocate (d_con_vars)
         deallocate (d_aux_vars)
         deallocate (d_aux_vars_tl)
         deallocate (d_con_vars_tl)
      end if

      !-------------------------------------------------------------------
      ! [2.0] assign Hdx :
      !-------------------------------------------------------------------

      if (num_rad > 0) then
         do n=1,num_rad
            deallocate (con_vars(n) % t)
            deallocate (con_vars(n) % q)
            deallocate (con_vars_tl(n) % t)
            deallocate (con_vars_tl(n) % q)
         end do
         deallocate (con_vars)
         deallocate (aux_vars)
         deallocate (con_vars_tl)
         deallocate (aux_vars_tl)
      end if
   end do        ! end loop for sensor

   if (trace_use) call da_trace_exit("da_transform_xtoy_rttov")
#else
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $RTTOV option for radiances"/))
#endif

end subroutine da_transform_xtoy_rttov


