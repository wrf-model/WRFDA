subroutine DA_Minimise_CG( grid, config_flags,            &
                           it, ob, xb, xbx, be, iv, xhat, &
                           cv, xa, vv, vp, xp, re, y, j,  &
                           ids, ide, jds, jde, kds, kde,  &
                           ims, ime, jms, jme, kms, kme,  &
                           its, ite, jts, jte, kts, kte   )

!-------------------------------------------------------------------------
!  History:
!
!     Purpose:         Main Conjugate Gradient minimisation routine 
!
!     Creation:        02/17/2003                    S. R. H. Rizvi
!
!     Here 
!       cv   is updated in outer-loop.
!       xhat is the control variable in inner-loop.
!
!                                             commented by Y.-R. Guo
!-------------------------------------------------------------------------
      implicit none

      integer, intent(in)               :: it    ! external iteration.
      type (y_type), intent(in)         :: ob    ! Observation structure.
      type (xb_type), intent(in)        :: xb    ! first guess (local).
      type (xbx_type),intent(in)        :: xbx   ! Header & non-gridded vars.
      type (be_type), intent(in)        :: be    ! background error structure.
      type (ob_type), intent(inout)     :: iv    ! ob. increment vector.
      real,intent(out),dimension(cv_size)::xhat  ! Increament control variable
      type (cv_type), intent(inout)     :: cv    ! control variable (local).
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
      real                              :: j_grad_norm ! target gradient norm.
      real, dimension(cv_size)          :: j_grad      ! grad_v j (local-grid)

      real                              :: apdotp,step,rrmold,rrmnew,ratio 
      real                              :: ob_grad,rrmnew_norm

      real, dimension(cv_size)          :: ghat, phat, fhat ! work array
      type (y_type)                     :: jo_grad_y        ! Grad_y(Jo)

      integer                           :: nobwin,ndynopt

      character(len=4)                  :: filnam

      write (6,'(A)') &
        ' ----------------------------------------------------------'
      write(6,*)
      write (6,'(A)') ' [5.2] Minimize cost function Using CG Method'
      if( calculate_cg_cost_function ) then
        write (6,'(A)') &
          '  For this run cost function diagnostics will be written'
      else
        write (6,'(A)') &
          '  For this run cost function diagnostics will not be written'
      end if
      write (6,'(A)') &
        ' ----------------------------------------------------------'
      write(6,*)

      xhat   = 0.0
      j_grad = 0.0

      call da_allocate_y(iv, jo_grad_y)

      call da_calculate_j( it, 0, xb, xbx, be, iv, xhat, cv % array, &
                           vv, vp, xp, re, y, xa, j, j_grad,         &
                           ids, ide, jds, jde, kds, kde,             &
                           ims, ime, jms, jme, kms, kme,             &
                           its, ite, jts, jte, kts, kte,             &
                           grid, config_flags                        )

      ghat  = j_grad
      phat  = - ghat
   
      rrmold = da_dot(ghat, ghat, cv_size)

      if( j%total == 0.0) go to 1004
      if(it == 1) cv % j_grad_norm_target =  sqrt (rrmold)

!-------------------------------------------------------------------------
      write(6,10)it
10    format(2x,'Starting outer iteration : ',i3)
      write(6,11)cv%j_grad_norm_target,eps(it)*cv%j_grad_norm_target, &
                 j%total, sqrt(rrmold)
11    format(2x,'Original gradient is ',1PD15.8,/,&
             2x,'For this outer iteration Gradient target is ',1PD15.8,/,&
             2x,'Starting Cost function: ' ,1PD15.8,' Gradient= ',1PD15.8)
      write (6,'(A)') &
        ' ----------------------------------------------------------'
      if( calculate_cg_cost_function ) then
      write(6,'(2x,/,2x,A)') &
        ' Iter    Cost Function         Gradient             Step'
      else
      write(6,'(2x,/,2x,A)')' Iter      Gradient             Step'
      endif
      write(6,*)

!-------------------------------------------------------------------------
!     [2.0] iteratively solve for minimum of cost function:
!-------------------------------------------------------------------------
!
      do 1000  iter = 1  ,  ntmax
      if( rrmold == 0.) go to 1002
!-------------------------------------------------------------------------
      fhat = phat

             call DA_Transform_VToY( be, fhat, iv, vp, vv, xa, xb, xbx, xp, y,         &
                                     ids, ide, jds, jde, kds, kde,                     &
                                     ims, ime, jms, jme, kms, kme,                     &
                                     its, ite, jts, jte, kts, kte,                     &
                                     grid, config_flags                                )

         call da_calculate_grady( iv, y , jo_grad_y)

         call DA_Transform_VToY_Adj( be, fhat, iv, vp, vv, xa, xb, xbx, xp, jo_grad_y, &
                                     ids, ide, jds, jde, kds, kde,                     &
                                     ims, ime, jms, jme, kms, kme,                     &
                                     its, ite, jts, jte, kts, kte,                     &
                                     grid, config_flags                                )

         fhat = - fhat + phat
         apdotp = da_dot(fhat, phat, cv_size)
         step = 0.0
         if(apdotp .gt. 0.0   ) step = rrmold/apdotp
         ghat = ghat + step * fhat
         xhat = xhat + step * phat
         if( calculate_cg_cost_function ) then
           j_grad = 0.
           call da_calculate_j(it,iter, xb, xbx, be, iv, xhat, cv%array, &
                               vv, vp, xp, re, y, xa, j, j_grad,         &
                               ids, ide, jds, jde, kds, kde,             &
                               ims, ime, jms, jme, kms, kme,             &
                               its, ite, jts, jte, kts, kte,             &
                               grid, config_flags                        )
           ob_grad = sqrt(da_dot(j_grad,j_grad,cv_size))  
         end if

         rrmnew = da_dot (ghat, ghat, cv_size)

         rrmnew_norm = sqrt(rrmnew)

         if (rrmnew_norm  < eps(it)*cv % j_grad_norm_target) go to 1002
         ratio = 0.0
         if(rrmold .gt. 0.0 ) ratio = rrmnew/rrmold

         phat = - ghat       + ratio * phat

         rrmold=rrmnew
         if( calculate_cg_cost_function ) then
           write(6,12)iter, j%total, ob_grad, step
         else
           write(6,14)iter, rrmnew_norm , step
         endif
12       format(2x,i3,5x,1PD15.8,5x,1PD15.8,5x,1PD15.8)
14       format(2x,i3,5x,1PD15.8,5x,1PD15.8)
1000     continue
!-------------------------------------------------------------------------
!        End of the minimization of cost function
!-------------------------------------------------------------------------
       iter = iter -1
       go to 1003
1002   continue
       if( calculate_cg_cost_function ) then
         write(6,12)iter, j%total, ob_grad, step
       else
         write(6,14)iter, rrmnew_norm , step
       endif
1003   continue
       write (6,'(A)') &
         ' ----------------------------------------------------------'
       write(6,13)iter
13     format(2x,'Inner iteration stopped after ',i4,' iterations')
       if( calculate_cg_cost_function ) then 
         rrmnew_norm = ob_grad
       else
         call da_calculate_j( it, iter, xb, xbx,be,iv,xhat, cv % array, &
                              vv, vp, xp, re, y, xa, j, j_grad,         &
                              ids, ide, jds, jde, kds, kde,             &
                              ims, ime, jms, jme, kms, kme,             &
                              its, ite, jts, jte, kts, kte,             &
                              grid, config_flags                        )

         rrmnew_norm = sqrt( da_dot(j_grad,j_grad,cv_size) )
       endif
        
       write(6,15)j%total , rrmnew_norm
15     format(2x,' Final cost function : ',1PD15.8,' and Gradient: ',1PD15.8)
       write (6,'(A)') &
         ' ----------------------------------------------------------'
1004   continue
!
      cv % array = cv % array + xhat

      call da_deallocate_y( jo_grad_y)

      end subroutine DA_Minimise_CG

