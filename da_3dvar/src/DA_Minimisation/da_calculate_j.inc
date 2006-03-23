subroutine da_calculate_j( it, iter, cv_size, cv_size_jb, cv_size_je, &
                           xb, xbx, be, ep, iv, xhat, cv, vv, &
                           vp, xp, re, y, xa, j, j_grad,            &
                           ids, ide, jds, jde, kds, kde,            &
                           ims, ime, jms, jme, kms, kme,            &
                           its, ite, jts, jte, kts, kte,            &
                           grid, config_flags                       )
!------------------------------------------------------------------------------
! PURPOSE: Initialises the Y-array
!
! PARENT_MODULE: DA_DA_Minimisation  
!
!------------------------------------------------------------------------------

      implicit none

      integer, intent(in)                :: it     ! external iteration #.
      integer, intent(in)                :: iter   ! internal iteration #.
      integer, intent(in)                :: cv_size    ! Total cv size. 
      integer, intent(in)                :: cv_size_jb ! Jb cv size. 
      integer, intent(in)                :: cv_size_je ! Je cv size. 
      type (xb_type), intent(in)         :: xb     ! first guess (local).
      type (xbx_type),intent(in)         :: xbx    ! For header & non-grid arrays.
      type (be_type), intent(in)         :: be     ! background error structure.
      type (ep_type), intent(in)         :: ep     ! Ensemble perturbations.
      type (ob_type), intent(inout)      :: iv     ! innovation vector (o-b).
      real, intent(in)                   :: xhat(1:cv_size) ! control variables.
      real, intent(inout)                :: cv(1:cv_size)   ! control variables.
      type (vp_type), intent(inout)      :: vv     ! Grdipt/EOF CV.
      type (vp_type), intent(inout)      :: vp     ! Grdipt/level CV.
      type (xpose_type), intent(inout)   :: xp     ! Domain decomposition vars.
      type (y_type) , intent(inout)      :: re     ! residual vector (o-a).
      type (y_type) , intent(inout)      :: y      ! y = H(x_inc).
      real, intent(out)                  :: j_grad(1:cv_size) ! control variables.
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
      real             :: gnorm_jb, gnorm_jo, gnorm_j, gnorm_je
 
      type (y_type)    :: jo_grad_y ! Grad_y(jo)

      integer          :: je_start, je_end             ! Start/end indices of Je.
      character(len=4) :: filnam
      real             :: cv_jb(1:cv_size_jb)          ! Jb control variable.
      real             :: xhat_jb(1:cv_size_jb)        ! Jb control variable.
      real             :: cv_je(1:cv_size_je)          ! Je control variable.
      real             :: xhat_je(1:cv_size_je)        ! Je control variable.

      je_start = cv_size_jb + 1
      je_end = cv_size_jb + cv_size_je

!-------------------------------------------------------------------------
      
      call da_allocate_y( iv, jo_grad_y )

!-------------------------------------------------------------------------
!     [2.0] calculate jo:
!-------------------------------------------------------------------------
      
!     [2.1] transform from control variable to model grid space:

      call da_allocate_y (iv, y)
!-----initialising now the residual array
      call da_allocate_y (iv, re)
!
      if ( iter > 0 ) &
         call DA_Transform_VToY( cv_size, be, ep, xhat, iv, vp, vv, xa, xb, xbx, xp, y, &
                                 ids, ide, jds, jde, kds, kde,             &
                                 ims, ime, jms, jme, kms, kme,             &
                                 its, ite, jts, jte, kts, kte,             &
                                 grid, config_flags                        )

!     [2.3] compute residual (o-a) = (o-b) - h x~

      call da_calculate_residual( iv, y, re )

!     [2.4] calculate jo:

      call da_calculate_jo_and_grady( iv, re, jo_partial, j % jo, jo_grad_y)

    IF ( testing_dm_exact ) THEN
      ! jo_partial has been already summed at lower level
      j % jo % total = jo_partial
    ELSE
      j % jo % total = wrf_dm_sum_real( jo_partial )
    ENDIF
 
!-------------------------------------------------------------------------
!     [1.0] calculate jb:
!-------------------------------------------------------------------------

     cv_jb(1:cv_size_jb) = cv (1:cv_size_jb)
     xhat_jb(1:cv_size_jb) = xhat (1:cv_size_jb)


     j % jb = 0.5 * da_dot( cv_jb + xhat_jb, cv_jb + xhat_jb, cv_size_jb )
     j % jb = jb_factor * j % jb

!-------------------------------------------------------------------------
!     [1.0] calculate je:
!-------------------------------------------------------------------------

     j % je = 0.0
     if( be % ne > 0 ) then
        cv_je(1:cv_size_je) = cv(je_start:je_end)
        xhat_je(1:cv_size_je) = xhat(je_start:je_end)
        j % je = 0.5 * da_dot( cv_je + xhat_je, cv_je + xhat_je, cv_size_je )
     endif
     j % je = je_factor * j % je


!-------------------------------------------------------------------------
!     [3.0] calculate total cost function j = jo + jb + je:
!-------------------------------------------------------------------------

      j % total = j % jb + j % jo % total + j % je

      if(it == 1 .and. iter == 0) then
        write(UNIT=cost_unit,FMT='(a)')'Outer   EPS   Inner      J          Jb      Jo      Je'
        write(UNIT=cost_unit,FMT='(a)')'Iter          Iter                            '
        write(UNIT=grad_unit,FMT='(a)')'Outer   EPS   Inner      G          Gb      Go      Ge'
        write(UNIT=grad_unit,FMT='(a)')'Iter          Iter                            '
      endif

      write(UNIT=grad_unit,FMT='(2x,i2,1x,f6.3,2x,i4,2x,4f10.3)') &
        it, EPS(it), iter, j % total, j % jb, j % jo % total, j % je


!-------------------------------------------------------------------------
!     [2.0] calculate grad_v (jo):
!-------------------------------------------------------------------------

      call DA_Transform_VToY_Adj( cv_size, be, ep, j_grad, iv, vp, vv, xa, xb, xbx, xp, jo_grad_y, &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  its, ite, jts, jte, kts, kte,     &
                                  grid, config_flags )

      call da_deallocate_y( jo_grad_y )

      if( print_detail >= 100) then
         write(unit=stderr, fmt='(2a,2(2x,a,i6))') 'file:', __FILE__, 'line:', __LINE__, 'iter=', iter
         write(unit=stderr, fmt='(a, e24.14)') ' in da_calculate_j: cv.cv = ',sum(cv(1:cv_size) * cv(1:cv_size))
         write(unit=stderr, fmt='(a, e24.14)') ' in da_calculate_j: xhat.xhat = ',sum(xhat(1:cv_size) * xhat(1:cv_size))
         write(unit=stderr, fmt='(a, e24.14)') ' in da_calculate_j: j_grad.j_grad = ',sum(j_grad(1:cv_size) * j_grad(1:cv_size))
      endif

!-------------------------------------------------------------------------
!     [3.0] calculate grad_v (j) = grad_v (jb) + grad_v (jo) + grad_v (je):
!-------------------------------------------------------------------------

      gnorm_jo= da_dot(j_grad,j_grad,cv_size)

!     add background/ensemble gradient to observational gradient

      cv(1:cv_size_jb) = jb_factor * cv(1:cv_size_jb)

!     Add Del_v (Jb)
      j_grad(1:cv_size_jb) =  jb_factor * cv(1:cv_size_jb) + &
                              jb_factor * xhat(1:cv_size_jb) + &
                              j_grad(1:cv_size_jb)
!     Add Del_v (Je)
      if ( cv_size_je > 0 ) then
         j_grad(je_start:je_end) = je_factor * cv(je_start:je_end) + &
                                   je_factor * xhat(je_start:je_end) + &
                                   j_grad(je_start:je_end)
      end if
      gnorm_jb=da_dot(cv_jb + xhat_jb , cv_jb + xhat_jb, cv_size_jb)
      gnorm_je=da_dot(cv_je + xhat_je , cv_je + xhat_je, cv_size_je)

      if( print_detail >= 100) then
         write(unit=stderr, fmt='(2a,2(2x,a,i6))') 'file:', __FILE__, 'line:', __LINE__, 'iter=', iter
         write(unit=stderr, fmt='(a, e24.14)') ' in da_calculate_j: cv_jb.cv_jb = ',&
                                          sum(cv_jb(1:cv_size_jb) * cv_jb(1:cv_size_jb))
         write(unit=stderr, fmt='(a, e24.14)') ' in da_calculate_j: cv_je.cv_je = ',&
                                          sum(cv_je(1:cv_size_je) * cv_je(1:cv_size_je))
         write(unit=stderr, fmt='(a, e24.14)') ' in da_calculate_j: xhat_jb.xhat_jb = ', &
                                          sum(xhat_jb(1:cv_size_jb) * xhat_jb(1:cv_size_jb))
         write(unit=stderr, fmt='(a, e24.14)') ' in da_calculate_j: xhat_je.xhat_je = ', &
                                          sum(xhat_je(1:cv_size_je) * xhat_je(1:cv_size_je))
         write(unit=stderr, fmt='(a, e24.14)') ' in da_calculate_j: j_grad.j_grad = ',sum(j_grad(1:cv_size) * &
                                          j_grad(1:cv_size))
      endif

      gnorm_j = da_dot(j_grad, j_grad,cv_size)

      gnorm_jo= sqrt(gnorm_jo)
      gnorm_jb= jb_factor * sqrt(gnorm_jb )
      gnorm_je= je_factor * sqrt(gnorm_je )
      gnorm_j = sqrt(gnorm_j )

      write(grad_unit,FMT='(2x,i2,1x,f6.3,2x,i4,2x,4f10.3)') & 
        it, EPS(it), iter, gnorm_j, gnorm_jb, gnorm_jo, gnorm_je

  end subroutine da_calculate_j

