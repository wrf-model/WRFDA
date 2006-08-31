subroutine da_minimise_cg( grid, config_flags,            &
                           it, cv_size, xb, xbx, be, ep, iv, &
                           j_grad_norm_target, xhat, cv, &
                           xa, vv, vp, xp, re, y, j,  &
                           ids, ide, jds, jde, kds, kde,  &
                           ims, ime, jms, jme, kms, kme,  &
                           its, ite, jts, jte, kts, kte   )

   !-------------------------------------------------------------------------
   !     Purpose:         Main Conjugate Gradient minimisation routine 
   !
   !     Here 
   !       cv   is updated in outer-loop.
   !       xhat is the control variable in inner-loop.
   !-------------------------------------------------------------------------

   implicit none

   integer, intent(in)               :: it    ! external iteration.
   integer, intent(in)               :: cv_size          ! Total cv size
   type (xb_type), intent(in)        :: xb    ! first guess (local).
   type (xbx_type),intent(in)        :: xbx   ! Header & non-gridded vars.
   type (be_type), intent(in)        :: be    ! background error structure.
   type (ep_type), intent(in)        :: ep    ! Ensemble perturbations.
   type (ob_type), intent(inout)     :: iv    ! ob. increment vector.
   real, intent(inout)               :: j_grad_norm_target ! Target norm.
   real, intent(out)                 :: xhat(1:cv_size)  ! control variable (local).
   real, intent(inout)               :: cv(1:cv_size)    ! control variable (local).
   type (x_type), intent(out)        :: xa    ! analysis increments (local).
   type (vp_type), intent(inout)     :: vv    ! Grdipt/EOF CV.
   type (vp_type), intent(inout)     :: vp    ! Grdipt/level CV.
   type (xpose_type), intent(inout)  :: xp    ! Dimensions and xpose buffers. 
   type (y_type), intent(inout)      :: re    ! residual (o-a) structure.
   type (y_type), intent(inout)      :: y     ! y = H(x_inc) structure.

   type (j_type), intent(out)    :: j                         ! cost function
   integer, intent(in)           :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)           :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)           :: its,ite, jts,jte, kts,kte ! tile   dims.

   type(domain) ,   TARGET                   :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   integer                           :: iter            
   real                              :: j_grad(1:cv_size)      ! grad_v j (local-grid)
   real                              :: fhat(1:cv_size)        ! cv copy.
   real                              :: ghat(1:cv_size)        ! cv copy.
   real                              :: phat(1:cv_size)        ! cv copy.
   real                              :: apdotp,step,rrmold,rrmnew,ratio 
   real                              :: ob_grad,rrmnew_norm

   type (y_type)                     :: jo_grad_y        ! Grad_y(Jo)

   integer                           :: ndynopt

   IF (trace_use) call da_trace_entry("da_minimise_cg")

   write (UNIT=stdout,FMT='(a/)') ' ----------------------------------------------------------'
   WRITE(UNIT=stdout,FMT='(A)') ' [5.2] Minimize cost function Using CG Method'
   if (calculate_cg_cost_fn) then
      WRITE(UNIT=stdout,FMT='(A)') &
         '  For this run cost function diagnostics will be written'
   else
      WRITE(UNIT=stdout,FMT='(A)') &
         '  For this run cost function diagnostics will not be written'
   end if
   WRITE(UNIT=stdout,FMT='(A)') &
      ' ----------------------------------------------------------'
   WRITE(UNIT=stdout,FMT=*) ' '

   ! Initialize temporary cv structures:
   xhat = 0.0
   j_grad = 0.0
   fhat = 0.0

   call da_allocate_y(iv, jo_grad_y)

   call da_calculate_j( it, 0, cv_size, be % cv % size_jb, be % cv % size_je, &
                        xb, xbx, be, ep, iv, xhat, cv, &
                        vv, vp, xp, re, y, xa, j, j_grad,         &
                        ids, ide, jds, jde, kds, kde,             &
                        ims, ime, jms, jme, kms, kme,             &
                        its, ite, jts, jte, kts, kte,             &
                        grid, config_flags                        )

   ghat  = j_grad
   phat  = - ghat

   rrmold = da_dot_cv( cv_size, cv_size_domain, ghat, ghat, xp, & 
                       (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /) )

   if (j%total == 0.0) go to 1004

   if (it == 1) j_grad_norm_target = sqrt (rrmold)

   WRITE(UNIT=stdout,FMT='("Starting outer iteration : ",i310)') it
   WRITE(UNIT=stdout,FMT=11)j_grad_norm_target,eps(it)*j_grad_norm_target, &
              j%total, sqrt(rrmold)
11 format(2x,'Original gradient is ',1PD15.8,/,&
          2x,'For this outer iteration Gradient target is ',1PD15.8,/,&
          2x,'Starting Cost function: ' ,1PD15.8,' Gradient= ',1PD15.8)
   WRITE(UNIT=stdout,FMT='(A)') &
      ' ----------------------------------------------------------'
   if (calculate_cg_cost_fn) then
      WRITE(UNIT=stdout,FMT='(2x,/,2x,A)') &
         ' Iter    Cost Function         Gradient             Step'
   else
      WRITE(UNIT=stdout,FMT='(2x,/,2x,A)')' Iter      Gradient             Step'
   endif
   WRITE(UNIT=stdout,FMT=*) ' '

   !-------------------------------------------------------------------------
   ! [2.0] iteratively solve for minimum of cost function:
   !-------------------------------------------------------------------------

   do iter=1, ntmax
      if (rrmold == 0.) go to 1002
      fhat = phat

      call da_transform_vtoy( cv_size, be, ep, fhat, iv, vp, vv, xa, xb, xbx, xp, y,         &
                              ids, ide, jds, jde, kds, kde,                     &
                              ims, ime, jms, jme, kms, kme,                     &
                              its, ite, jts, jte, kts, kte,                     &
                              grid, config_flags                                )

      call da_calculate_grady( iv, y , jo_grad_y)

      call da_transform_vtoy_adj( iter, cv_size, be, ep, fhat, iv, vp, vv, \
                                  xa, xb, xbx, xp, jo_grad_y, &
                                  ids, ide, jds, jde, kds, kde,                     &
                                  ims, ime, jms, jme, kms, kme,                     &
                                  its, ite, jts, jte, kts, kte,                     &
                                  grid, config_flags                                )

      fhat = - fhat + phat
      apdotp = da_dot_cv( cv_size, cv_size_domain, fhat, phat, xp, & 
                       (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /) )

      step = 0.0
      if (apdotp .gt. 0.0) step = rrmold/apdotp
      ghat = ghat + step * fhat
      xhat = xhat + step * phat
      if (calculate_cg_cost_fn) then
         j_grad = 0.
         call da_calculate_j( it, iter, cv_size, be % cv % size_jb, be % cv % size_je, &
                             xb, xbx, be, ep, iv, xhat, cv, &
                             vv, vp, xp, re, y, xa, j, j_grad,         &
                             ids, ide, jds, jde, kds, kde,             &
                             ims, ime, jms, jme, kms, kme,             &
                             its, ite, jts, jte, kts, kte,             &
                             grid, config_flags                        )
          ob_grad = da_dot_cv( cv_size, cv_size_domain, j_grad, j_grad, xp, &
                       (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /) )
         ob_grad = sqrt(ob_grad)
      end if

      rrmnew = da_dot_cv( cv_size, cv_size_domain, ghat, ghat, xp, &
                       (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /) )

      rrmnew_norm = sqrt(rrmnew)

      if (rrmnew_norm  < eps(it) * j_grad_norm_target) go to 1002
      ratio = 0.0
      if(rrmold .gt. 0.0 ) ratio = rrmnew/rrmold

      phat = - ghat       + ratio * phat

      rrmold=rrmnew
      if (calculate_cg_cost_fn) then
         WRITE(UNIT=stdout,FMT=12)iter, j%total, ob_grad, step
      else
         WRITE(UNIT=stdout,FMT=14)iter, rrmnew_norm , step
      endif
12    format(2x,i3,5x,1PD15.8,5x,1PD15.8,5x,1PD15.8)
14    format(2x,i3,5x,1PD15.8,5x,1PD15.8)
   end do

   !-------------------------------------------------------------------------
   ! End of the minimization of cost function
   !-------------------------------------------------------------------------
   iter = iter -1
   go to 1003
1002 continue
   if (calculate_cg_cost_fn) then
      WRITE(UNIT=stdout,FMT=12)iter, j%total, ob_grad, step
   else
     WRITE(UNIT=stdout,FMT=14)iter, rrmnew_norm , step
   endif
1003 continue
   WRITE(UNIT=stdout,FMT='(A)') &
      ' ----------------------------------------------------------'
   WRITE(UNIT=stdout,FMT='(2x,"Inner iteration stopped after ",i4," iterations")') iter

   if (calculate_cg_cost_fn) then 
      rrmnew_norm = ob_grad
   else
      call da_calculate_j( it, iter, cv_size, be % cv % size_jb, be % cv % size_je, &
                           xb, xbx, be, ep, iv, xhat, cv, &
                           vv, vp, xp, re, y, xa, j, j_grad,         &
                           ids, ide, jds, jde, kds, kde,             &
                           ims, ime, jms, jme, kms, kme,             &
                           its, ite, jts, jte, kts, kte,             &
                           grid, config_flags                        )

      rrmnew_norm = da_dot_cv( cv_size, cv_size_domain, j_grad, j_grad, xp, &
                              (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /) )
      rrmnew_norm = sqrt(rrmnew_norm)
    endif

    WRITE(UNIT=stdout,FMT=15)j%total , rrmnew_norm
15  format(2x,' Final cost function : ',1PD15.8,' and Gradient: ',1PD15.8)
    WRITE(UNIT=stdout,FMT='(A)') &
      ' ----------------------------------------------------------'
1004 continue

   cv = cv + xhat

   call da_deallocate_y( jo_grad_y)   

   IF (trace_use) call da_trace_exit("da_minimise_cg")

end subroutine da_minimise_cg

