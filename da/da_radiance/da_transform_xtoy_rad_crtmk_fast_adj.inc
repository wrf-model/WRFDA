subroutine da_transform_xtoy_rad_crtmk_fast_adj ( iv, xp, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! PURPOSE: transform gradient from obs space to model grid space.
   !
   ! METHOD:  jo_grad_x = H^T jo_grad_y =  - H^T R^-1 ( d - H delta_x )
   !           1. input gradient in obs space and reference state of RTTOV
   !           2. call adjoint of RTM
   !           3. adjoint of interpolation from model grid to obs loc
   !
   !  HISTORY: 11/16/2006 - Creation            Zhiquan Liu
   !
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (x_type), INTENT(INOUT)   :: jo_grad_x ! 
   TYPE (xpose_type), INTENT(IN)  :: xp        ! Domain decomposition vars.
   TYPE (y_type),  INTENT(IN)     :: jo_grad_y ! H' delta_x
   TYPE (ob_type), INTENT(IN)     :: iv        ! O-B structure.

   INTEGER                        :: l, i, j, k  ! Index dimension.
   INTEGER                        :: num_rad  ! Number of radiance obs
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
   INTEGER                        :: inst, nchanl, n
   REAL, pointer                  :: q_ad(:),t_ad(:)
   REAL                           :: ps_ad

   ! CRTM local varaibles and types
   INTEGER :: Allocate_Status
!---------------------------------------------------------

#if !defined(CRTM)
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#else
   IF ( iv%num_inst < 1 ) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_rad_crtmk_fast_adj")

!-------------------------------------------------------------------------------

   do inst = 1, iv%num_inst                 ! loop for sensor
      if ( iv%instid(inst)%num_rad < 1 ) cycle
      num_rad = iv%instid(inst)%num_rad

  ! Allocate forward model solution RTSolution array to number of channels
      ALLOCATE( t_ad     ( xp%kte-xp%kts+1 ), &
                q_ad     ( xp%kte-xp%kts+1 ), &
                STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
         call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting t_ad q_ad"/))                                                                                   
      END IF

!----------------------------------------------------------------------------
      do n=1,num_rad

      ! [1.5] Scale transformation and fill zero for no-control variable
          ps_ad = 0.
         do l=1, iv%instid(inst)%nchan
          ps_ad = ps_ad + iv%instid(inst)%ps_jacobian(l,n)*jo_grad_y%instid(inst)%tb(l,n)
         end do
          ps_ad = ps_ad*0.01

         do k=xp%kts,xp%kte
          t_ad(k) = 0.
          q_ad(k) = 0.
         do l=1, iv%instid(inst)%nchan
          t_ad(k)  = t_ad(k) + iv%instid(inst)%t_jacobian(l,k,n)*jo_grad_y%instid(inst)%tb(l,n)
          q_ad(k)  = q_ad(k) + iv%instid(inst)%q_jacobian(l,k,n)*jo_grad_y%instid(inst)%tb(l,n)
         end do
          q_ad(k) = 1000.*q_ad(k)
         end do
          
            !-----------------------------------------------------
            ! [1.6] Get horizontal interpolation weights:
            !-----------------------------------------------------

            i = iv%instid(inst)%loc_i(n)
            j = iv%instid(inst)%loc_j(n)
            dx = iv%instid(inst)%loc_dx(n)
            dy = iv%instid(inst)%loc_dy(n)
            dxm = iv%instid(inst)%loc_dxm(n)
            dym = iv%instid(inst)%loc_dym(n)

            ! [1.7] Adjoint of Interpolate horizontally from ob to grid:

            do k=xp%kts,xp%kte ! from bottem to top
               if (iv%instid(inst)%pm(xp%kte-k+1,n) < 75.) q_ad(xp%kte-k+1) = 0.
               call da_interp_lin_2d_adj( jo_grad_x%t(:,:,k), xp%ims, xp%ime, xp%jms, &
                                          xp%jme, i, j, dx, dy, dxm, dym, &
                                          t_ad(xp%kte-k+1))

               call da_interp_lin_2d_adj( jo_grad_x%q(:,:,k), xp%ims, xp%ime, xp%jms, & 
                                           xp%jme, i, j, dx, dy, dxm, dym, &
                                           q_ad(xp%kte-k+1) )
            enddo

               call da_interp_lin_2d_adj(jo_grad_x% psfc, xp%ims, xp%ime, xp%jms, &
                                         xp%jme, i, j, dx, dy, dxm, dym,  &
                                         ps_ad )

               !if (n <=10 ) then
               !   write(6,'(e15.5)') jo_grad_x% psfc(i,j)
               !  do k=xp%kts,xp%kte
               !   write(6,'(2e15.5)') jo_grad_x%t(i,j,k), jo_grad_x%q(i,j,k)
               !  enddo
               !endif

         end do       !  end loop for pixels

      !-------------------------------------------------------------------
      ! [2.0] Deallocating CRTM structures
      !-------------------------------------------------------------------

         deallocate( t_ad, q_ad, &
                     STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) THEN
              call da_error(__FILE__,__LINE__, &
                  (/"Error in deallocatting t_ad q_ad"/))
         endif

   end do        ! end loop for sensor

   if (trace_use) call da_trace_exit("da_transform_xtoy_rad_crtmk_fast_adj")
#endif
 
end subroutine da_transform_xtoy_rad_crtmk_fast_adj

