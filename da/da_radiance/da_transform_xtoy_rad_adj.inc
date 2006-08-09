subroutine da_transform_xtoy_rad_adj ( iv, xp, jo_grad_y, jo_grad_x )
!------------------------------------------------------------------------------
!  PURPOSE: transform gradient from obs space to model grid space.
!
!  METHOD:  jo_grad_x = H^T jo_grad_y =  - H^T R^-1 ( d - H delta_x )
!            1. input gradient in obs space and reference state of RTTOV
!            2. call adjoint of RTM
!            3. adjoint of interpolation from model grid to obs loc
!
!  HISTORY: 27/07/2005 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

!#include "rttov_v2q.interface"

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
!
   type(con_vars_type),  pointer  :: con_vars(:), con_vars_ad(:)
   type(aux_vars_type),  pointer  :: aux_vars(:)

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

      if(iv%instid(inst)%rad(n)%loc%proc_domain_with_halo) then

!------------------------------------------------------------------------
!        [1.0] assign tb = R^-1 Re :
!------------------------------------------------------------------------
         iscan = iv%instid(inst)%rad(n)%scanpos
         do k = 1, nchanl
           if (lbiascorr) then
             tb(nprofiles,k) = satinfo(inst)%b(iscan+1,k)* jo_grad_y%instid(inst)%rad(n)%tb(k)
           else
             tb(nprofiles,k) = jo_grad_y%instid(inst)%rad(n)%tb(k) 
           end if
         end do

!---------------------------------------------
!        [2.0] get input of da_rtm_ad
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
!              aux_vars(iprof) % fastem(:) = 0.

              aux_vars(iprof) % satzen  = iv%instid(inst)%rad(n)%satzen
              aux_vars(iprof) % satazi  = iv%instid(inst)%rad(n)%satazi

        end do

!-------------------------------------------------
!         [3.0] Call RTM Adjoint model
!-------------------------------------------------
 
        call da_rtm_ad (inst, nchanl, nprofiles, con_vars, &
                               aux_vars, con_vars_ad, tb )

!----------------------------------------------------------
!        [4.1] Get horizontal interpolation weights:
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
!
!        4.2 scale transform 
!
         do k=1, num_levs

               model_mr(k) = con_vars_ad(nprofiles) % q(k)
               
            IF ( zk(k) <= 0.0 ) THEN
               con_vars_ad(nprofiles)%t(k)  = 0.  !coefs(inst) % ref_prfl_t(k,gas_id_watervapour)
               con_vars_ad(nprofiles)%q(k)  = 0.  !coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
            ELSE
!
!    adjoint of q(kg/kg) to ppmv
!
               con_vars_ad(nprofiles)%q(k) = model_mr(k) * q2ppmv
!               call rttov_v2q (&
!                 & gas_unit_ppmv,      &! in
!                 & model_mr(k),        &! in
!                 & gas_id_watervapour, &! in
!                 & model_mr(k),        &! in
!                 & con_vars_ad(nprofiles)%q(k)     )    ! inout
            END IF

         end do
!
!   adjoint ofconvert to hPa
!
         con_vars_ad(nprofiles)% ps = con_vars_ad(nprofiles)% ps*0.01 
         
!
!        [4.3] Interpolate horizontally from ob to grid:
!
         call da_interp_lin_3d_adj( jo_grad_x%t, xp, i, j, dx, dy, dxm, dym, &
                              con_vars_ad(nprofiles)% t, max_ob_levels, zk, num_levs )
                              
         call da_interp_lin_3d_adj( jo_grad_x%q, xp, i, j, dx, dy, dxm, dym, &
                              con_vars_ad(nprofiles)% q, max_ob_levels, zk, num_levs )
                              
         call da_interp_lin_2d_adj(jo_grad_x% psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, con_vars_ad(nprofiles)% ps)


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

