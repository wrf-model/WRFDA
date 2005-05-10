subroutine da_calculate_j( it, iter, xb, xbx, be, iv, xhat, cv, vv, &
                           vp, xp, re, y, xa, j, j_grad,            &
                           ids, ide, jds, jde, kds, kde,            &
                           ims, ime, jms, jme, kms, kme,            &
                           its, ite, jts, jte, kts, kte,            &
                           grid, config_flags                       )
!------------------------------------------------------------------------------
! PURPOSE: Initialises the Y-array
!
! PARENT_MODULE: DA_DA_Minimisation  
!------------------------------------------------------------------------------

      implicit none

      integer, intent(in)                :: it     ! external iteration #.
      integer, intent(in)                :: iter   ! internal iteration #.
      type (xb_type), intent(in)         :: xb     ! first guess (local).
      type (xbx_type),intent(in)         :: xbx    ! For header & non-grid arrays.
      type (be_type), intent(in)         :: be     ! background error structure.
      type (ob_type), intent(inout)      :: iv     ! innovation vector (o-b).
      real,intent(in),dimension(cv_size) :: xhat   ! Increament control variable
      real,intent(in),dimension(cv_size) :: cv     ! control variable (local).
      type (vp_type), intent(inout)      :: vv     ! Grdipt/EOF CV.
      type (vp_type), intent(inout)      :: vp     ! Grdipt/level CV.
      type (xpose_type), intent(inout)   :: xp     ! Domain decomposition vars.
      type (y_type) , intent(inout)      :: re     ! residual vector (o-a).
      type (y_type) , intent(inout)      :: y      ! y = H(x_inc).
      real,intent(out),dimension(cv_size):: j_grad ! grad(jo) (local grid)
      type (x_type) , intent(inout)      :: xa     ! gridded analy. incs. (local)
      type (j_type) , intent(out)        :: j      ! cost function j

      integer   , intent(in)       :: ids,ide, jds,jde, kds,kde ! domain dims.
      integer   , intent(in)       :: ims,ime, jms,jme, kms,kme ! memory dims.
      integer   , intent(in)       :: its,ite, jts,jte, kts,kte ! tile   dims.

      type(domain) ,   TARGET                   :: grid
      type(grid_config_rec_type), intent(inout) :: config_flags

      real             :: jb                           ! jb
      real             :: jo                           ! jo
      real             :: jo_partial                   ! jo for this processor
      real             :: gnorm_cv, gnorm_jo, gnorm_j
 
      type (y_type)    :: jo_grad_y ! Grad_y(jo)

      integer          :: ierror

      character(len=4) :: filnam

!-------------------------------------------------------------------------
      
      call da_allocate_y( iv, jo_grad_y )

!-------------------------------------------------------------------------
!     [2.0] calculate jo:
!-------------------------------------------------------------------------
      
!     [2.1] transform from control variable to model grid space:

      call da_zero_y (iv, y)
!-----initialising now the residual array
      call da_zero_y (iv, re)
!
      if ( iter > 0 ) &
         call DA_Transform_VToY( be, xhat, iv, vp, vv, xa, xb, xbx, xp, y, &
                                 ids, ide, jds, jde, kds, kde,             &
                                 ims, ime, jms, jme, kms, kme,             &
                                 its, ite, jts, jte, kts, kte,             &
                                 grid, config_flags                        )

!     [2.3] compute residual (o-a) = (o-b) - h x~

      call da_calculate_residual( iv, y, re )

!     [2.4] calculate jo:

      call da_calculate_jo_and_grady( iv, re, jo_partial, j % jo, jo_grad_y)

#ifdef DM_PARALLEL
      call MPI_ALLREDUCE( jo_partial, j % jo % total, 1, TRUE_MPI_REAL, MPI_SUM, &
                          MPI_COMM_WORLD, IERROR )
#else
      j % jo % total = jo_partial
#endif
 

!-------------------------------------------------------------------------
!     [1.0] calculate jb:
!-------------------------------------------------------------------------

     if(minimisation_option == 1) then
        j%jb = 0.5*da_dot(cv,cv,cv_size)
     else if(minimisation_option == 2) then
        j%jb = 0.5*da_dot(cv+xhat,cv+xhat,cv_size)
     endif

!-------------------------------------------------------------------------
!     [3.0] calculate total cost function j = jo + jb:
!-------------------------------------------------------------------------

      j % total = j % jb + j % jo % total

      if(it == 1 .and. iter == 0) then
      write(81,'(a)')'Outer   EPS   Inner      J          Jb      Jo'
      write(81,'(a)')'Iter          Iter                            '
      write(82,'(a)')'Outer   EPS   Inner      G          Gb      Go'
      write(82,'(a)')'Iter          Iter                            '
      endif

      write(81,81) it, EPS(it), iter, j % total, j % jb, j % jo % total
 81   format(2x,i2,1x,f6.3,2x,i4,2x,3f10.3)

!-------------------------------------------------------------------------
!     [2.0] calculate grad_v (jo):
!-------------------------------------------------------------------------

      call DA_Transform_VToY_Adj( be, j_grad, iv, vp, vv, xa, xb, xbx, xp, jo_grad_y, &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  its, ite, jts, jte, kts, kte,     &
                                  grid, config_flags )

      call da_deallocate_y( jo_grad_y )

      if( print_detail >= 100) then
         write(unit=0, fmt='(2a,2(2x,a,i6))') 'file:', __FILE__, 'line:', __LINE__, 'iter=', iter
         write(unit=0, fmt='(a, e24.14)') ' in da_calculate_j: cv.cv = ',sum(cv(1:cv_size) * cv(1:cv_size))
         write(unit=0, fmt='(a, e24.14)') ' in da_calculate_j: xhat.xhat = ',sum(xhat(1:cv_size) * xhat(1:cv_size))
         write(unit=0, fmt='(a, e24.14)') ' in da_calculate_j: j_grad.j_grad = ',sum(j_grad(1:cv_size) * j_grad(1:cv_size))
      endif

!-------------------------------------------------------------------------
!     [3.0] calculate grad_v (j) = grad_v (jb) + grad_v (jo):
!-------------------------------------------------------------------------

      gnorm_jo= da_dot(j_grad,j_grad,cv_size)

      gnorm_jo= sqrt(gnorm_jo)

!     add background gradient to observational gradient

      if(minimisation_option == 1) then
         j_grad =  cv        + j_grad

         gnorm_cv=da_dot(cv, cv, cv_size)
      else if(minimisation_option == 2) then
         j_grad =  cv + xhat + j_grad

         gnorm_cv=da_dot(cv + xhat, cv + xhat, cv_size)
      else
         write(unit=*, fmt='(a)') 'Wrong minimisation_option.'
      endif

      if( print_detail >= 100) then
         write(unit=0, fmt='(2a,2(2x,a,i6))') 'file:', __FILE__, 'line:', __LINE__, 'iter=', iter
         write(unit=0, fmt='(a, e24.14)') ' in da_calculate_j: cv.cv = ',sum(cv(1:cv_size) * cv(1:cv_size))
         write(unit=0, fmt='(a, e24.14)') ' in da_calculate_j: xhat.xhat = ',sum(xhat(1:cv_size) * xhat(1:cv_size))
         write(unit=0, fmt='(a, e24.14)') ' in da_calculate_j: j_grad.j_grad = ',sum(j_grad(1:cv_size) * j_grad(1:cv_size))
      endif

      gnorm_cv= sqrt(gnorm_cv)

      gnorm_j = da_dot(j_grad, j_grad,cv_size)

      gnorm_j = sqrt(gnorm_j )

      write(82,81) it, EPS(it), iter, gnorm_j, gnorm_cv, gnorm_jo

  end subroutine da_calculate_j

