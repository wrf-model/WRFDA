subroutine da_transform_xtoy_rad_adj ( iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! PURPOSE: transform gradient from obs space to model grid space.
   !
   ! METHOD:  jo_grad_x = H^T jo_grad_y =  - H^T R^-1 ( d - H delta_x )
   !           1. input gradient in obs space and reference state of RTTOV
   !           2. call adjoint of RTM
   !           3. adjoint of interpolation from model grid to obs loc
   !---------------------------------------------------------------------------

   IMPLICIT NONE

!#include "rttov_v2q.interface"
#ifdef DM_PARALLEL
      include "mpif.h"
#endif

   TYPE (x_type), INTENT(INOUT)   :: jo_grad_x ! 
   TYPE (xpose_type), INTENT(IN)  :: xp        ! Domain decomposition vars.
   TYPE (y_type),  INTENT(IN)     :: jo_grad_y ! H' delta_x
   TYPE (ob_type), INTENT(IN)     :: iv        ! O-B structure.

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
   INTEGER                        :: alloc_status(40)

   
   REAL, pointer                  :: model_mr(:)
   
   REAL, DIMENSION(xp%kms:xp%kme)   :: v_p      ! Model value p at ob hor. location.

   INTEGER            :: inst, nprofiles, nchanl, unit, iprof, ios, iscan
   REAL,    pointer   :: zk(:)
   REAL,    pointer   :: pres(:)
   character(len=30)  :: filename
   real,    pointer   :: tb(:,:)

   type(con_vars_type),  pointer  :: con_vars(:), con_vars_ad(:)
   type(aux_vars_type),  pointer  :: aux_vars(:)

   ! evenly distrbuted versions
   type(con_vars_type),  pointer  :: d_con_vars(:), d_con_vars_ad(:)
   type(aux_vars_type),  pointer  :: d_aux_vars(:)
   integer :: d_nprofiles, lprofiles,e,s,l,nl,ierr
   real, allocatable :: temp1(:,:),temp2(:,:)

   IF ( iv%num_inst < 1 ) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_rad_adj")

   alloc_status (:) = 0

   do inst = 1, iv%num_inst                 ! loop for sensor
      if ( iv%instid(inst)%num_rad < 1 ) cycle
      nprofiles = 1
      num_levs  = iv%instid(inst)%nlevels
      nchanl    = iv%instid(inst)%nchan

      allocate (zk(1:num_levs), stat= alloc_status(1) )
      allocate (pres(1:num_levs), stat= alloc_status(2) )
      allocate (model_mr(1:num_levs), stat= alloc_status(7) )
      allocate (tb(nprofiles,nchanl), stat= alloc_status(3) )
      allocate (con_vars(nprofiles), stat= alloc_status(4) )
      allocate (con_vars_ad(nprofiles), stat= alloc_status(5) )
      allocate (aux_vars(nprofiles), stat= alloc_status(6) )

      if ( any(alloc_status /= 0) ) then
         call da_error(__FILE__,__LINE__, &
            (/"Canot allocate memory to con_vars"/))
      end if

      pres(1:num_levs) = coefs(inst) % ref_prfl_p(1:num_levs)

      do n= 1, iv%instid(inst)%num_rad           ! loop for pixel
         if (iv%instid(inst)%rad(n)%loc%proc_domain_with_halo) then
            !---------------------------------------------------------------
            ! [1.0] assign tb = R^-1 Re :
            !---------------------------------------------------------------

            iscan = iv%instid(inst)%rad(n)%scanpos
            do k = 1, nchanl
               if (lbiascorr) then
                  tb(nprofiles,k) = satinfo(inst)%b(iscan+1,k)* &
                     jo_grad_y%instid(inst)%rad(n)%tb(k)
               else
                  tb(nprofiles,k) = jo_grad_y%instid(inst)%rad(n)%tb(k) 
               end if
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

               con_vars(iprof) % t(:) = iv%instid(inst)%rad(n)%t (:)
               con_vars(iprof) % q(:) = iv%instid(inst)%rad(n)%mr(:)
               con_vars(iprof) % ps   = iv%instid(inst)%rad(n)%ps 

               aux_vars(iprof) % t2m = iv%instid(inst)%rad(n)%t2m
               aux_vars(iprof) % q2m = iv%instid(inst)%rad(n)%mr2m
               aux_vars(iprof) % u10 = iv%instid(inst)%rad(n)%u10
               aux_vars(iprof) % v10 = iv%instid(inst)%rad(n)%v10

               aux_vars(iprof) % surftype = iv%instid(inst)%rad(n)%landsea_mask
               aux_vars(iprof) % surft    = iv%instid(inst)%rad(n)%ts
               ! aux_vars(iprof) % fastem(:) = 0.

               aux_vars(iprof) % satzen  = iv%instid(inst)%rad(n)%satzen
               aux_vars(iprof) % satazi  = iv%instid(inst)%rad(n)%satazi
            end do

            !-------------------------------------------------
            ! [2.9] Distribute evenly across processors
            !-------------------------------------------------

            if (tovs_redistribute) then
#ifdef DM_PARALLEL
               d_nprofiles=num_tovs_after(myproc-1)

               allocate (d_con_vars(d_nprofiles))
               allocate (d_aux_vars(d_nprofiles))
               d_con_vars(:) % nlevels = num_levs

               do iprof = 1, d_nprofiles
                  allocate (d_con_vars(iprof) % t(num_levs))
                  allocate (d_con_vars(iprof) % q(num_levs))

                  allocate (d_con_vars_ad(iprof) % t(num_levs))
                  allocate (d_con_vars_ad(iprof) % q(num_levs))
               end do

               ! Fill up with data that stays local
               lprofiles=MIN(nprofiles,d_nprofiles)
               do iprof = 1, nprofiles
                  d_con_vars(iprof) % t(:) = con_vars(iprof) % t(:) 
                  d_con_vars(iprof) % q(:) = con_vars(iprof) % q(:)
                  ! d_aux_vars(iprof) % fastem(:) = 0.
               end do

               d_con_vars(1:lprofiles) % ps       = con_vars(1:lprofiles) % ps
               d_aux_vars(1:lprofiles) % t2m      = aux_vars(1:lprofiles) % t2m
               d_aux_vars(1:lprofiles) % q2m      = aux_vars(1:lprofiles) % q2m
               d_aux_vars(1:lprofiles) % u10      = aux_vars(1:lprofiles) % u10
               d_aux_vars(1:lprofiles) % v10      = aux_vars(1:lprofiles) % v10
               d_aux_vars(1:lprofiles) % surftype = aux_vars(1:lprofiles) % surftype
               d_aux_vars(1:lprofiles) % surft    = aux_vars(1:lprofiles) % surft
               d_aux_vars(1:lprofiles) % satzen   = aux_vars(1:lprofiles) % satzen
               d_aux_vars(1:lprofiles) % satazi   = aux_vars(1:lprofiles) % satazi

               ! Get data from elsewhere

               nl=num_levs
               do i=1,tovs_copy_count
                  if (tovs_send_pe(i)==myproc) then
                     s=tovs_send_start(i)
                     l=tovs_send_count(i)
                     e=s+l-1
                     nl=l*num_levs
                     allocate(temp1(1:l,num_levs))
                     do j=1,l
                        temp1(j,:)=con_vars(s+j-1) % t(:)
                     end do
                     call MPI_SEND( temp1(:,:),nl, MPI_REAL, tovs_recv_pe(i), i*3+1, MPI_COMM_WORLD, IERR )
                     do j=1,l
                        temp1(j,:)=con_vars(s+j-1) % q(:)
                     end do
                     call MPI_SEND( temp1(:,:),nl, MPI_REAL, tovs_recv_pe(i), i*3+2, MPI_COMM_WORLD, IERR )
                     allocate (temp2(9,l))
                     temp2(1,:)= con_vars(s:e) % ps
                     temp2(2,:)= aux_vars(s:e) % t2m
                     temp2(3,:)= aux_vars(s:e) % q2m
                     temp2(4,:)= aux_vars(s:e) % u10
                     temp2(5,:)= aux_vars(s:e) % v10
                     temp2(6,:)= aux_vars(s:e) % surftype
                     temp2(7,:)= aux_vars(s:e) % surft
                     temp2(8,:)= aux_vars(s:e) % satzen
                     temp2(9,:)= aux_vars(s:e) % satazi
                     call MPI_SEND( temp2(:,:),nl, MPI_REAL, tovs_recv_pe(i), i*3+3, MPI_COMM_WORLD, IERR )
                     deallocate (temp1)
                     deallocate (temp2)
                  end if
                  if (tovs_recv_pe(i)==myproc) then
                     s=tovs_recv_start(i)
                     l=tovs_send_count(i)
                     e=s+l-1
                     nl=l*num_levs
                     allocate(temp1(1:l,num_levs))
                     call MPI_RECV(temp1(:,:),nl, MPI_REAL, tovs_send_pe(i), i*3+1, MPI_COMM_WORLD, IERR )
                     do j=1,l
                        con_vars(s+j-1) % t(:)=temp1(j,:)
                     end do
                     call MPI_RECV(temp1(:,:),nl, MPI_REAL, tovs_send_pe(i), i*3+2, MPI_COMM_WORLD, IERR )
                     do j=1,l
                        con_vars(s+j-1) % q(:)=temp1(j,:)
                     end do
                     allocate (temp2(9,l))
                     call MPI_RECV(temp2(:,:),nl, MPI_REAL, tovs_send_pe(i), i*3+3, MPI_COMM_WORLD, IERR )
                     con_vars(s:e) % ps         = temp2(1,:)
                     d_aux_vars(s:e) % t2m      = temp2(2,:)
                     d_aux_vars(s:e) % q2m      = temp2(3,:)
                     d_aux_vars(s:e) % u10      = temp2(4,:)
                     d_aux_vars(s:e) % v10      = temp2(5,:)
                     d_aux_vars(s:e) % surftype = temp2(6,:)
                     d_aux_vars(s:e) % surft    = temp2(7,:)
                     d_aux_vars(s:e) % satzen   = temp2(8,:)
                     d_aux_vars(s:e) % satazi   = temp2(9,:)
                     deallocate (temp1)
                     deallocate (temp2)
                  end if
               end do

               call da_rttov_ad (inst, nchanl, nprofiles, d_con_vars, &
                                 d_aux_vars, con_vars_ad, tb )
#endif
            else
               call da_rttov_ad (inst, nchanl, nprofiles, con_vars, &
                                 aux_vars, con_vars_ad, tb )
            end if

            !-----------------------------------------------------
            ! [4.1] Get horizontal interpolation weights:
            !-----------------------------------------------------

            i = iv%instid(inst)%rad(n)%loc%i
            j = iv%instid(inst)%rad(n)%loc%j
            dx = iv%instid(inst)%rad(n)%loc%dx
            dy = iv%instid(inst)%rad(n)%loc%dy
            dxm = iv%instid(inst)%rad(n)%loc%dxm
            dym = iv%instid(inst)%rad(n)%loc%dym

            do k=1, num_levs
               zk(k)  =  iv%instid(inst)%rad(n)%zk(k) 
            enddo

            ! 4.2 scale transform 
 
            do k=1, num_levs
               model_mr(k) = con_vars_ad(nprofiles) % q(k)
               
               IF ( zk(k) <= 0.0 ) THEN
                  con_vars_ad(nprofiles)%t(k)  = 0.  !coefs(inst) % ref_prfl_t(k,gas_id_watervapour)
                  con_vars_ad(nprofiles)%q(k)  = 0.  !coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
               ELSE

                  ! adjoint of q(kg/kg) to ppmv

                  con_vars_ad(nprofiles)%q(k) = model_mr(k) * q2ppmv
                  ! call rttov_v2q (&
                  !    gas_unit_ppmv,      &! in
                  !    model_mr(k),        &! in
                  !    gas_id_watervapour, &! in
                  !    model_mr(k),        &! in
                  !    con_vars_ad(nprofiles)%q(k)     )    ! inout
               END IF
            end do

            ! adjoint ofconvert to hPa

            con_vars_ad(nprofiles)% ps = con_vars_ad(nprofiles)% ps*0.01 
         
            ! [4.3] Interpolate horizontally from ob to grid:

            call da_interp_lin_3d_adj( jo_grad_x%t, xp, i, j, dx, dy, dxm, dym, &
               con_vars_ad(nprofiles)% t, max_ob_levels, zk, num_levs )
                              
            call da_interp_lin_3d_adj( jo_grad_x%q, xp, i, j, dx, dy, dxm, dym, &
               con_vars_ad(nprofiles)% q, max_ob_levels, zk, num_levs )
                              
            call da_interp_lin_2d_adj(jo_grad_x% psfc, xp%ims, xp%ime, xp%jms, &
               xp%jme, i, j, dx, dy, dxm, dym, con_vars_ad(nprofiles)% ps)


            do iprof = 1, nprofiles
               deallocate (con_vars(iprof) % t)
               deallocate (con_vars(iprof) % q)
               deallocate (con_vars_ad(iprof) % t)
               deallocate (con_vars_ad(iprof) % q)
            end do        
         end if   !  end endif for within_halo
      end do       !  end loop for pixels

      deallocate (zk)
      deallocate (pres)
      deallocate (model_mr)
      deallocate (tb)
      deallocate (con_vars)
      deallocate (aux_vars)
      deallocate (con_vars_ad)

   end do        ! end loop for sensor

   if (trace_use) call da_trace_exit("da_transform_xtoy_rad_adj")
 
end subroutine da_transform_xtoy_rad_adj

