subroutine da_get_innov_vector_buoy( it, xb, xp, ob, iv)

   implicit none

   integer, intent(in)           :: it      ! External iteration.
   type(xb_type), intent(in)    :: xb      ! first guess state.
   type(xpose_type), intent(in) :: xp      ! Domain decomposition vars.
   type(y_type),  intent(inout) :: ob      ! Observation structure.
   type(ob_type), intent(inout) :: iv      ! O-B structure.

   integer                      :: n        ! Loop counter.
   integer                      :: i, j, k  ! Index dimension.
   real                         :: dx, dxm  ! Interpolation weights.
   real                         :: dy, dym  ! Interpolation weights.
   real                         :: model_u  ! Model value u at oblocation.
   real                         :: model_v  ! Model value v at oblocation.
   real                         :: model_t  ! Model value t at oblocation.
   real                         :: model_p  ! Model value p at oblocation.
   real                         :: model_q  ! Model value q at oblocation.

   real, dimension(xp%kms:xp%kme) :: v_h      ! Model value h at ob hor. location.
   real, dimension(xp%kms:xp%kme) :: v_p      ! Model value p at ob hor. location.

   real :: hd, psfcm

   real :: hsm , ho, to, qo

   integer :: itu,ituf,itvv,itvvf,itp,itpf,itt,ittf,itqv,itqvf


   if (iv % num_buoy < 1) return


   itu   = 0; itvv    = 0; itp  = 0; itt  = 0; itqv  = 0;
   ituf  = 0; itvvf   = 0; itpf = 0; ittf = 0; itqvf = 0;

   do n=iv%ob_numb(iv%current_ob_time-1)%buoy + 1, &
      iv%ob_numb(iv%current_ob_time)%buoy
      ! [1.1] Get horizontal interpolation weights:

      i = iv%buoy(n)%loc%i
      j = iv%buoy(n)%loc%j
      dx = iv%buoy(n)%loc%dx
      dy = iv%buoy(n)%loc%dy
      dxm = iv%buoy(n)%loc%dxm
      dym = iv%buoy(n)%loc%dym

      ! Surface correction

      iv%buoy(n)%p%inv = ob%buoy(n)%p
      iv%buoy(n)%t%inv = ob%buoy(n)%t
      iv%buoy(n)%q%inv = ob%buoy(n)%q
      iv%buoy(n)%u%inv = ob%buoy(n)%u
      iv%buoy(n)%v%inv = ob%buoy(n)%v

      if (sfc_assi_options == 1) then
         iv%buoy(n)%zk=missing_r

         if (iv % buoy(n) % h > missing_r) then
            do k=xp%kts,xp%kte
               v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                      + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
            end do

            hd = v_h(xp%kts) - iv % buoy(n) % h

            if (abs(hd) <= Max_StHeight_Diff) then
               if (iv % buoy(n) % h < v_h(xp%kts)) then
                  iv%buoy(n)%zk = 1.0+1.0e-6
                  call da_obs_sfc_correction(iv%buoy(n), xb)

                  ! To keep the original "ob" with no change for multiple 
                  ! outer-loops use:

                  ! ob%buoy(n)%p = iv%buoy(n)%p%inv
                  ! ob%buoy(n)%t = iv%buoy(n)%t%inv
                  ! ob%buoy(n)%q = iv%buoy(n)%q%inv
                  ! ob%buoy(n)%u = iv%buoy(n)%u%inv
                  ! ob%buoy(n)%v = iv%buoy(n)%v%inv
               else
                  call da_to_zk(iv % buoy(n) % h, v_h, xp, v_interp_h, &
                     iv%buoy(n)%zk)
               end if
            else
               iv%buoy(n)%zk = missing_r
            end if
         else if (ob % buoy(n) % p > 1.0) then
            do k=xp%kts,xp%kte
               v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                       + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
            end do

            call da_to_zk(ob % buoy(n) % p, v_p, xp, v_interp_p, &
               iv%buoy(n)%zk)

            if (iv%buoy(n)%zk < 0.0) then
               iv % buoy(n) % p % inv = missing_r
               iv % buoy(n) % p % qc  = missing

               iv%buoy(n)%zk = 1.0+1.0e-6
            end if
         end if

         !------------------------------------------------------------------
         ! [2.0] Initialise components of innovation vector:
         !------------------------------------------------------------------

         if (iv%buoy(n)%zk < 0.0) then
            iv % buoy(n) % u % qc = missing
            iv % buoy(n) % v % qc = missing
            iv % buoy(n) % t % qc = missing
            iv % buoy(n) % q % qc = missing
            iv % buoy(n) % p % qc = missing
         else
            ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
            call da_interp_obs_lin_2d( xb % u, xp, i, j, dx, dy, dxm, dym, &
                                      model_u, iv%buoy(n)%zk)
            call da_interp_obs_lin_2d( xb % v, xp, i, j, dx, dy, dxm, dym, &
                                      model_v, iv%buoy(n)%zk)
            call da_interp_obs_lin_2d( xb % t, xp, i, j, dx, dy, dxm, dym, &
                                      model_t, iv%buoy(n)%zk)
            call da_interp_obs_lin_2d( xb % q, xp, i, j, dx, dy, dxm, dym, &
                                      model_q, iv%buoy(n)%zk)
            call da_interp_obs_lin_2d( xb % p, xp, i, j, dx, dy, dxm, dym, &
                                      model_p, iv%buoy(n)%zk)
#else
            call da_interp_obs_lin_2d( xb % u(xp%ims,xp%jms,xb%kms), xp, &
               i, j, dx, dy, dxm, dym, model_u, iv%buoy(n)%zk)
            call da_interp_obs_lin_2d( xb % v(xp%ims,xp%jms,xb%kms), xp, &
               i, j, dx, dy, dxm, dym, model_v, iv%buoy(n)%zk)
            call da_interp_obs_lin_2d( xb % t(xp%ims,xp%jms,xb%kms), xp, &
               i, j, dx, dy, dxm, dym, model_t, iv%buoy(n)%zk)
            call da_interp_obs_lin_2d( xb % q(xp%ims,xp%jms,xb%kms), xp, &
               i, j, dx, dy, dxm, dym, model_q, iv%buoy(n)%zk)
            call da_interp_obs_lin_2d( xb % p(xp%ims,xp%jms,xb%kms), xp, &
               i, j, dx, dy, dxm, dym, model_p, iv%buoy(n)%zk)
#endif
         end if
      else if (sfc_assi_options == 2) then

         ! Surface data assimilation approca 2
         ! -----------------------------------

         ! 1.2.1 Surface assimilation approach 2(10-m u, v, 2-m t, q, 
         ! and sfc_p)

         call da_interp_lin_2d( xb % u10, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_u)
         call da_interp_lin_2d( xb % v10, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_v)
         call da_interp_lin_2d( xb % t2, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_t)
         call da_interp_lin_2d( xb % q2, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_q)
         call da_interp_lin_2d( xb % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_p)

         if (iv%buoy(n)%p%qc >= 0) then
            ! model surface p, t, q, h at observed site:

            call da_interp_lin_2d( xb % terr, xp%ims, xp%ime, xp%jms,xp%jme,&
                              i, j, dx, dy, dxm, dym, &
                              hsm)

            ho = iv%buoy(n)%h
            to = -888888.0
            qo = -888888.0

            if (iv%buoy(n)%t%qc >= 0 .and. iv%buoy(n)%q%qc >= 0) then
               to = ob%buoy(n)%t
               qo = ob%buoy(n)%q
               call da_sfc_pre(psfcm, model_p, model_t, model_q, &
                  hsm, ho, to, qo)
            else if (iv%buoy(n)%t%qc >= 0 .and. iv%buoy(n)%q%qc < 0) then
               to = ob%buoy(n)%t
               call da_sfc_pre(psfcm, model_p, model_t, model_q, hsm, ho, to)
            else
               call da_sfc_pre(psfcm, model_p, model_t, model_q, hsm, ho)
            end if

            ! Pressure at the observed height:
            model_p = psfcm
         end if
      end if

      !-----------------------------------------------------------------------
      ! [3.0] Fast interpolation: 
      !-----------------------------------------------------------------------

      if (ob % buoy(n) % u > missing_r .AND. &
          iv % buoy(n) % u % qc >= obs_qc_pointer) then
         iv % buoy(n) % u % inv = iv%buoy(n)%u%inv - model_u
      else
         iv % buoy(n) % u % inv = 0.0
      end if

      if (ob % buoy(n) % v > missing_r .AND. &
          iv % buoy(n) % v % qc >= obs_qc_pointer) then
         iv % buoy(n) % v % inv = iv%buoy(n)%v%inv - model_v
      else
         iv % buoy(n) % v % inv = 0.0
      end if

      if (ob % buoy(n) % p > 0.0 .AND. &
          iv % buoy(n) % p % qc >= obs_qc_pointer) then
         iv % buoy(n) % p % inv = iv%buoy(n)%p%inv - model_p
      else
         iv % buoy(n) % p % inv = 0.0
      end if

      if (ob % buoy(n) % t > 0.0 .AND. &
          iv % buoy(n) % t % qc >= obs_qc_pointer) then
         iv % buoy(n) % t % inv = iv%buoy(n)%t%inv - model_t
      else
         iv % buoy(n) % t % inv = 0.0
      end if

      if (ob % buoy(n) % q > 0.0 .AND. &
          iv % buoy(n) % q % qc >= obs_qc_pointer) then
         iv % buoy(n) % q % inv = iv%buoy(n)%q%inv - model_q
      else
         iv % buoy(n) % q % inv = 0.0
      end if

      ! -----------------------------------------------------------------------
      ! [5.0] Perform optional maximum error check:
      !-----------------------------------------------------------------------

      if (check_max_iv) then
         call da_check_max_iv_buoy(it, iv % buoy(n), &
            itu,ituf,itvv,itvvf,itp,itpf,itt,ittf,itqv,itqvf)
      end if
   end do

   if (check_max_iv_print) then
      write(unit= check_max_iv_unit, fmt ='(A,i5,A)')'For outer iteration ',it, &
         ', Total Rejections for Buoy follows:'

      write(unit = check_max_iv_unit, fmt = '(/,10(2(A,I6),/))') &
         'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
         'Number of failed v-wind observations:     ',itvvf,' on ',itvv,   &
         'Number of failed pressure observations:   ',itpf, ' on ',itp,   &
         'Number of failed temperature observations:',ittf, ' on ',itt,   &
         'Number of failed mixing ratio observations:',itqvf,' on ',itqv, &
         'Finally Total Buoy rejections ',ituf+itvvf+itpf+ittf+itqvf,' on ',&
                                            itu +itvv +itp +itt +itqv
   end if

end subroutine da_get_innov_vector_buoy


