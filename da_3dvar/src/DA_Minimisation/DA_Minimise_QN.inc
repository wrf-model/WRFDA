subroutine DA_Minimise_QN( grid, config_flags,               &
                           it, ob, xb, xbx, be, iv, xhat,    &
                           cv, xa, vv, vp, xp, re, y, j,     &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           its, ite, jts, jte, kts, kte      )

!-------------------------------------------------------------------------
!  Purpose: Main subroutine for Quasi Newton minimisation
!-------------------------------------------------------------------------

      implicit none

      integer, intent(in)                 :: it    ! external iteration.
      type (y_type), intent(in)           :: ob    ! Observation structure.
      type (xb_type), intent(in)          :: xb    ! first guess (local).
      type (xbx_type),intent(in)          :: xbx   ! Header & non-gridded vars.
      type (be_type), intent(in)          :: be    ! background error structure.
      type (ob_type), intent(inout)       :: iv    ! ob. increment vector.
      real,intent(out),dimension(cv_size) :: xhat  ! Increament control variable
      type (cv_type), intent(inout)       :: cv    ! control variable (local).
      type (x_type), intent(out)          :: xa    ! analysis increments (local).
      type (vp_type), intent(inout)       :: vv    ! Grdipt/EOF CV.
      type (vp_type), intent(inout)       :: vp    ! Grdipt/level CV.
      type (xpose_type), intent(inout)    :: xp    ! Dimensions and xpose buffers.
      type (y_type), intent(inout)        :: re    ! residual (o-a) structure.
      type (y_type), intent(inout)        :: y     ! y = H(x_inc) structure.

!     LOGICAL, EXTERNAL :: wrf_dm_on_monitor       ! For statistics output.

      type (j_type), intent(out) :: j                         ! cost function
      integer, intent(in)        :: ids,ide, jds,jde, kds,kde ! domain dims.
      integer, intent(in)        :: ims,ime, jms,jme, kms,kme ! memory dims.
      integer, intent(in)        :: its,ite, jts,jte, kts,kte ! tile   dims.

      type(domain) ,   TARGET                   :: grid
      type(grid_config_rec_type), intent(inout) :: config_flags
      
      integer                           :: iflag, info
      integer                           :: ipoint, ifev
      integer                           :: ifun, infun, i
      integer                           :: iter
      integer                           :: iter_start
      integer                           :: iter_inner
      integer                           :: ip
      real                              :: step        ! minimization step size.
      real                              :: j_grad_norm ! target gradient norm.
      real, dimension(cv_size)          :: j_grad      ! grad_v j (local-grid)
      real, dimension(cv_size)          :: j_grad_old  ! grad_v j from previous iter
      real, dimension(cv_size)          :: work        ! work array
      real, dimension(nsave*cv_size)    :: zs, zy      ! work arrays for minimization
      real, dimension(2*nsave+cv_size)  :: zw          ! work arrays for minimization
      integer, dimension(2)             :: iprint
      logical                           :: llfin

      write (6,'(A)') &
        ' --------------------------------------------------------'
      write (6,'(A)') &
        ' [5.2] Minimizing cost function using Quasi Newton Method'
      write (6,'(A)') &
        ' --------------------------------------------------------'
      write(6,*)

      iflag = 0
      llfin = .false.
      ipoint = 0
      iprint(1)=1
      iprint(2)=0
      ifun = 1
      step = 1.0

      infoc = 1
      brackt = .false.
      stage1 = .true.
      
      j_grad = 0.0
      j_grad_old = 0.0
      zs = 0.0
      zy = 0.0
      zw = 0.0

!-----------------------------------------------------------------------------
! ** Note ** The usage of variables, xhat and cv%array:
!
!  xhat      is used to store the increments, w'k, of control variables 
!            in inner-loop.
!
!  cv%array  is used to store the sum, (w'k + wk - wb), of increments 
!            of the control variables from inner-loop and outer-loop.
!
!  Reference:
!    Veerse, F., and J.-N. Thepaut, 1998: Multiple-truncation incremental
!        approach for four-dimensional variational data assimilation.
!        Q.J.R.Meteorol. Soc., 124, 1889-1908.
!
!                                                   Y.-R. Guo  08/15/2003
!------------------------------------------------------------------------------

      xhat = 0.0

      call da_calculate_j(it,  0, xb, xbx, be, iv, xhat, cv % array, &
                           vv, vp, xp, re, y, xa, j, j_grad,         &
                           ids, ide, jds, jde, kds, kde,             &
                           ims, ime, jms, jme, kms, kme,             &
                           its, ite, jts, jte, kts, kte,             &
                           grid, config_flags                        )

      if( j %total == 0.0) go to 1004

      llfin = .false.

      j_grad_norm = sqrt( da_dot( j_grad, j_grad, cv_size ) )

      if (j_grad_norm > 1.0e-20) step = 1.0 / j_grad_norm
      j_grad_old = j_grad
      zs(1:cv_size) = -j_grad(1:cv_size)
!-------------------------------------------------------------------------
!     [1.0] initialise minimization and perform tests:
!-------------------------------------------------------------------------

      if ( it == 1 ) cv % j_grad_norm_target = j_grad_norm
!     print initial cost function and gradient norm:

      call da_va15bd( iprint, 0, ifun, cv_size, nsave, cv % array, j % total, &
                 j_grad, step, eps(it)*cv % j_grad_norm_target, llfin)

!-------------------------------------------------------------------------
!     [2.0] iteratively solve for minimum of cost function:
!-------------------------------------------------------------------------

!      iter_start = (it-1) * ntmax + 1
!      do iter = iter_start, iter_start + ntmax - 1
      do iter = 1, ntmax                            

!         iter_inner = iter - iter_start + 1

!        [3.2] find descent direction:

!        call da_lmdir ( step, iter_inner, ipoint, iflag,  &
        call da_lmdir ( step, iter, ipoint, iflag,  &
                      cv_size, j_grad, j_grad_old, &
                      nsave, zs, zy, zw )

         if (iflag < 0 ) exit              ! minimisation achieved.

!        [3.3] find stepsize down descent direction:

         infun = 0
         info = 0
!         if ( iter_inner /= 1 ) step = 1.0
         if ( iter /= 1 ) step = 1.0
         j_grad_old = j_grad

         do i = 1, maxfev
            ip = ipoint * cv_size
            if ( info == 0 .or. info == -1 ) then
               xhat = xhat - cv % array
               call da_vd05ad(cv_size, cv % array, j % total, j_grad,  &
                              zs(ip+1), &
                              step, ftol, gtol, xtol, stpmin,  &
                              stpmax, maxfev, info, ifev, work)
               xhat = xhat + cv % array
               if ( info == -1 ) then
                  iflag = 1

                  call da_calculate_j( it, iter, xb, xbx, be, iv,xhat, &
                                       cv % array, vv, vp, xp,         &
                                       re, y, xa, j, j_grad,           &
                                       ids, ide, jds, jde, kds, kde,   &
                                       ims, ime, jms, jme, kms, kme,   &
                                       its, ite, jts, jte, kts, kte,   &
                                       grid, config_flags              )
                  infun = infun + 1
               end if
            end if

         end do
         
         if ( info /= 1 ) call da_minimisation_warning( info )

         ifun = ifun + infun

!        [3.4] perform diagnostics and print current step of minimisation:

         call da_va15bd( iprint, iter, ifun, cv_size, nsave, cv % array, &
            j % total, j_grad, step, eps(it)*cv % j_grad_norm_target, llfin)

         if ( llfin ) exit
      end do
      
!------------------------------------------------------------------------------
!  [4.0] calculate assimilation diagnostics:
!------------------------------------------------------------------------------

      if (iter > ntmax) iter = iter-1
1004  continue

end subroutine DA_Minimise_QN

