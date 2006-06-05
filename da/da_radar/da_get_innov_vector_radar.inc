SUBROUTINE da_get_innov_vector_Radar ( it, xb, xp, ob, iv )

!------------------------------------------------------------------------------
!  History: radial velocity on April 2004  Jianfeng Gu
!  History: reflectivity    on July  2004  Jianfeng Gu
!  History: update, September 2004  Jianfeng Gu
!  History: multi Radar Obs., 29 September 2004  Jianfeng Gu
!------------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN)            :: it       ! External iteration.
   TYPE (xb_type), INTENT(IN)     :: xb       ! first guess state.
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (y_type),  INTENT(INOUT)  :: ob       ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)  :: iv       ! O-B structure.

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.

   REAL, DIMENSION(max_ob_levels) :: model_p   ! Model value p at ob location.
   REAL, DIMENSION(max_ob_levels) :: model_u   ! Model value u at ob location.
   REAL, DIMENSION(max_ob_levels) :: model_v   ! Model value v at ob location.
   REAL, DIMENSION(max_ob_levels) :: model_w   ! Model value w at ob location.
   REAL, DIMENSION(max_ob_levels) :: model_rho ! Model value rho at ob location.
   REAL, DIMENSION(max_ob_levels) :: model_qrn ! Model value qrn at ob location.

   REAL, DIMENSION(max_ob_levels) :: model_rv ! Model value rv at ob location.
   REAL, DIMENSION(max_ob_levels) :: model_rf ! Model value rf at ob location.

   REAL, DIMENSION(xp%kms:xp%kme) :: v_h      ! Model value h at ob hor. location.

   REAL                           :: model_ps

   REAL                           :: xr,yr,zr,zdiff_bottom
   INTEGER                        :: irv, irvf
   INTEGER                        :: irf, irff


   IF ( iv % num_Radar < 1 ) RETURN

   irv = 0; irvf = 0; irf = 0; irff = 0

   DO n=iv%ob_numb(iv%current_ob_time-1)%radar + 1, iv%ob_numb(iv%current_ob_time)%radar

       if(iv%Radar(n)%loc%proc_domain_with_halo) then

         num_levs = iv % Radar(n) % info % levels

         if ( num_levs < 1 ) cycle

         model_p(:) = 0.0
         model_u(:) = 0.0
         model_v(:) = 0.0
         model_w(:) = 0.0
         model_rho(:) = 0.0
         model_qrn(:) = 0.0
      
! [1.0] Get horizontal interpolation weights:

         i = iv%Radar(n)%loc%i
         j = iv%Radar(n)%loc%j
         dx = iv%Radar(n)%loc%dx
         dy = iv%Radar(n)%loc%dy
         dxm = iv%Radar(n)%loc%dxm
         dym = iv%Radar(n)%loc%dym

         do k=xp%kts,xp%kte
            v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                   + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
         enddo

         do k=1, num_levs

            call to_zk(iv%Radar(n)%height(k), v_h, xp, v_interp_h, iv%Radar(n)%zk(k))

            if ( iv%Radar(n)%zk(k) < 1.0 ) then

               iv%Radar(n)%height_qc(k) = below_model_surface

            else if ( iv%Radar(n)%zk(k) > mkz ) then

               iv%Radar(n)%height_qc(k) = above_model_lid

            endif

         enddo


! [2.0] Interpolate horizontally to ob points:

#ifndef DEREF_KLUDGE

         call Interp_lin_3D(xb % p, xp, i, j, dx, dy, dxm, dym, &
                            model_p, max_ob_levels, iv%Radar(n)%zk, num_levs )

         call Interp_lin_3D(xb % u, xp, i, j, dx, dy, dxm, dym, &
                            model_u, max_ob_levels, iv%Radar(n)%zk, num_levs )

         call Interp_lin_3D(xb % v, xp, i, j, dx, dy, dxm, dym, &
                            model_v, max_ob_levels, iv%Radar(n)%zk, num_levs )

         call Interp_lin_3D(xb % wh, xp, i, j, dx, dy, dxm, dym, &
                            model_w, max_ob_levels, iv%Radar(n)%zk, num_levs )

         call Interp_lin_3D(xb % rho, xp, i, j, dx, dy, dxm, dym, &
                            model_rho, max_ob_levels, iv%Radar(n)%zk, num_levs )

         call Interp_lin_3D(xb % qrn, xp, i, j, dx, dy, dxm, dym, &
                            model_qrn, max_ob_levels, iv%Radar(n)%zk, num_levs )

#else

         call Interp_lin_3D(xb%p(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_p(1), max_ob_levels, iv%Radar(n)%zk(1), num_levs )

         call Interp_lin_3D(xb%u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_u(1), max_ob_levels, iv%Radar(n)%zk(1), num_levs )

         call Interp_lin_3D(xb%v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_v(1), max_ob_levels, iv%Radar(n)%zk(1), num_levs )

         call Interp_lin_3D(xb%wh(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_w(1), max_ob_levels, iv%Radar(n)%zk(1), num_levs )

         call Interp_lin_3D(xb%rho(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_rho(1), max_ob_levels, iv%Radar(n)%zk(1), num_levs )

         call Interp_lin_3D(xb%qrn(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_qrn(1), max_ob_levels, iv%Radar(n)%zk(1), num_levs )

#endif

!        Test 5.E-5 as critical value. It can not be smaller.

         do k=1,num_levs
            model_qrn(k)=amax1(5.E-5,model_qrn(k))
         enddo

         model_ps = dxm * (dym * xb % psac(i,  j) + dy * xb%psac(i+1,  j)) + &
                    dx  * (dym * xb % psac(i,j+1) + dy * xb%psac(i+1,j+1)) + &
                    xb % ptop

         iv%Radar(n)%model_p(1:num_levs) = model_p(1:num_levs)
         iv%Radar(n)%model_rho(1:num_levs) = model_rho(1:num_levs)
         iv%Radar(n)%model_qrn(1:num_levs) = model_qrn(1:num_levs)
         iv%Radar(n)%model_ps = model_ps

! [3.0] Calculate rv, rf at OBS location and initialise components of innovation vector:
         
         if(fg_format == 1) then
            call latlon_to_ij(map_info, &
                              iv%Radar(n)%stn_loc%lat, iv%Radar(n)%stn_loc%lon, &
                              iv%Radar(n)%stn_loc%x,   iv%Radar(n)%stn_loc%y )
         else
            CALL llxy ( iv%Radar(n)%stn_loc%lat, iv%Radar(n)%stn_loc%lon, &
                        iv%Radar(n)%stn_loc%x,   iv%Radar(n)%stn_loc%y )
         endif

         xr = xb%ds * (iv%Radar(n)%loc%x - iv%Radar(n)%stn_loc%x)
         yr = xb%ds * (iv%Radar(n)%loc%y - iv%Radar(n)%stn_loc%y)

         model_rv(:) = 0.0
         model_rf(:) = 0.0

         do k=1, num_levs

          iv % Radar(n) % rv(k) % inv = 0.0
          iv % Radar(n) % rf(k) % inv = 0.0

           IF ( iv % Radar(n) % height_qc(k) /= below_model_surface .and.  &
                iv % Radar(n) % height_qc(k) /= above_model_lid ) THEN

             IF (USE_Radar_rv) THEN
              IF ( ABS(iv % Radar(n) % rv(k) % qc - missing_data) > 1 ) THEN
               IF ( ABS(ob % Radar(n) % rv(k) - missing_r) > 1. .AND. &
                    iv % Radar(n) % rv(k) % qc >= obs_qc_pointer ) THEN

                  zr=iv%Radar(n)%height(k) - iv%Radar(n)%stn_loc%elv

                  call da_radial_velocity(model_rv(k), model_p(k),  &
                       model_u(k), model_v(k), model_w(k),          &
                       model_qrn(k), model_ps, xr, yr, zr)

                    iv % Radar(n) % rv(k) % inv = ob % Radar(n) % rv(k) - &
                                                 model_rv(k)
               END IF
              END IF
             END IF

             IF (USE_Radar_rf) THEN
              IF ( ABS(iv % Radar(n) % rf(k) % qc - missing_data) > 1 ) THEN
               IF ( ABS(ob % Radar(n) % rf(k) - missing_r) > 1. .AND. &
                    iv % Radar(n) % rf(k) % qc >= obs_qc_pointer ) THEN

                  call da_reflectivity( model_rf(k), model_rho(k), model_qrn(k) )

                    iv % Radar(n) % rf(k) % inv = ob % Radar(n) % rf(k) - &
                                                 model_rf(k)
               END IF
              END IF
             END IF

            END IF

         end do

!------------------------------------------------------------------------
! [4.0] Perform optional maximum error check:
!------------------------------------------------------------------------

      IF (check_max_iv )   & 
      CALL da_check_max_iv_Radar(it, iv % Radar(n), irv, irf, irvf, irff )

       endif
            
      END DO

    write(unit = check_max_iv_unit, FMT ='(/,A,i5,A)')   &
          'For outer iteration ', it, ', Total Rejections for Radar follows:'

     IF ( USE_Radar_rv ) THEN
       WRITE ( UNIT = check_max_iv_unit, FMT = '(/,2(A,I6))' ) &
      'Number of failed rv observations:     ',irvf, ' on ',irv
     ENDIF

     IF ( USE_Radar_rf ) THEN
       WRITE ( UNIT = check_max_iv_unit, FMT = '(/,2(A,I6))' ) &
      'Number of failed rf observations:     ',irff, ' on ',irf
     ENDIF


END SUBROUTINE da_get_innov_vector_Radar

