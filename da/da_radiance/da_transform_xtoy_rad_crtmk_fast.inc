subroutine da_transform_xtoy_rad_crtmk_fast ( xa, iv, xp, y )

   !---------------------------------------------------------------------------
   !  PURPOSE: transform from analysis increment to
   !                          pertubation radiance.
   !           K-Matrix vesion of da_transform_xtoy_rad_crtm
   !           use stored Jacobian to directly multi (no need re-call K-matrix)
   !
   !  METHOD:  delta_y = H delta_x
   !           1. input reference state of CRTM_TL
   !           2. interpolate analysis increment to obs location
   !           3. Call CRTM_TL
   !
   !  HISTORY: 11/16/2006 - Creation            Zhiquan Liu
   !
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (x_type), INTENT(IN)      :: xa       ! delta_x
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (y_type),  INTENT(INOUT)  :: y        ! H' delta_x
   TYPE (ob_type), INTENT(IN)     :: iv       ! O-B structure.

   INTEGER                        :: l, i, j, k  ! Index dimension.
   INTEGER                        :: num_rad  ! Number of radiance obs
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
   INTEGER                        :: inst, nchanl, n
   REAL, pointer                  :: q_tl(:),t_tl(:)
   REAL                           :: ps_tl

   ! CRTM local varaibles and types
   INTEGER :: Allocate_Status
!---------------------------------------------------------

#if !defined(CRTM)
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#else
   IF ( iv%num_inst < 1 ) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_rad_crtmk_fast")

!-------------------------------------------------------------------------------

   do inst = 1, iv%num_inst                 ! loop for sensor
      if ( iv%instid(inst)%num_rad < 1 ) cycle
      num_rad = iv%instid(inst)%num_rad

      ALLOCATE( t_tl     ( xp%kte-xp%kts+1 ), &
                q_tl     ( xp%kte-xp%kts+1 ), &
                STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
         call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting t_tl q_tl"/))                                                                                   
      END IF

!----------------------------------------------------------------------------
 
      do n=1,num_rad

      ! [1.2] Interpolate horizontally Atmoshere_TL variables to ob:
            !-----------------------------------------------------
            ! [1.6] Get horizontal interpolation weights:
            !-----------------------------------------------------

            i = iv%instid(inst)%rad(n)%loc%i
            j = iv%instid(inst)%rad(n)%loc%j
            dx = iv%instid(inst)%rad(n)%loc%dx
            dy = iv%instid(inst)%rad(n)%loc%dy
            dxm = iv%instid(inst)%rad(n)%loc%dxm
            dym = iv%instid(inst)%rad(n)%loc%dym

            do k=xp%kts,xp%kte ! from bottem to top
              call da_interp_lin_2d( xa%t(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                     i, j, dx, dy, dxm, dym, &  ! temperature (in K)
                                     t_tl(xp%kte-k+1) )
              call da_interp_lin_2d( xa%q(:,:,k), xp%ims, xp%ime, xp%jms, xp%jme, &
                                     i, j, dx, dy, dxm, dym, &  ! specific humidity (in kg/kg)
                                     q_tl(xp%kte-k+1) )
              if ( iv%instid(inst)%rad(n)%pm(xp%kte-k+1) < 75. ) q_tl(xp%kte-k+1) = 0.
            end do
              call da_interp_lin_2d( xa%psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                                     i, j, dx, dy, dxm, dym, &  ! in Pa
                                     ps_tl )
              ps_tl = ps_tl*0.01
              q_tl(:)  = q_tl(:)*1000. ! in g/kg

      ! [1.5] Scale transformation and fill zero for no-control variable
          y%instid(inst)%rad(n)%tb(:) = 0.
         do l=1, iv%instid(inst)%nchan
          y%instid(inst)%rad(n)%tb(l) = y%instid(inst)%rad(n)%tb(l) +  &
                    iv%instid(inst)%rad(n)%ps_jacobian(l)*ps_tl
         do k=xp%kts,xp%kte
          y%instid(inst)%rad(n)%tb(l) = y%instid(inst)%rad(n)%tb(l) & 
                               + iv%instid(inst)%rad(n)%t_jacobian(l,k)*t_tl(k)  &
                               + iv%instid(inst)%rad(n)%q_jacobian(l,k)*q_tl(k)
         end do
         end do
          
         !if (n <=10 ) write(6,'(15f8.3)') y%instid(inst)%rad(n)%tb(:)

         end do       !  end loop for pixels

      !-------------------------------------------------------------------
      ! [2.0] Deallocating CRTM structures
      !-------------------------------------------------------------------

         deallocate( t_tl, q_tl, &
                     STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) THEN
              call da_error(__FILE__,__LINE__, &
                  (/"Error in deallocatting t_tl q_tl"/))
         endif

   end do        ! end loop for sensor

   if (trace_use) call da_trace_exit("da_transform_xtoy_rad_crtmk_fast")
#endif
 
end subroutine da_transform_xtoy_rad_crtmk_fast

