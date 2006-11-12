subroutine da_get_innov_vector_ships ( it, xb, xp, ob, iv )

   implicit none

   integer,           intent(in)    :: it      ! External iteration.
   type (xb_type),    intent(in)    :: xb      ! first guess state.
   type (xpose_type), intent(in)    :: xp      ! Domain decomposition vars.
   type (y_type),     intent(inout) :: ob      ! Observation structure.
   type (ob_type),    intent(inout) :: iv      ! O-B structure.

   !-------------------------------------------------------------------------

   integer                      :: n        ! Loop counter.
   integer                      :: i, j, k  ! Index dimension.
   real                         :: dx, dxm  ! Interpolation weights.
   real                         :: dy, dym  ! Interpolation weights.
   real                         :: model_u  ! Model value u at oblocation.
   real                         :: model_v  ! Model value v at oblocation.
   real                         :: model_t  ! Model value t at oblocation.
   real                         :: model_p  ! Model value p at oblocation.
   real                         :: model_q  ! Model value q at oblocation.

   real, dimension(xp%kms:xp%kme) :: v_h   ! Model value h at ob hor. location.
   real, dimension(xp%kms:xp%kme) :: v_p   ! Model value p at ob hor. location.

   real :: hd, psfcm

   real :: hsm , ho, to, qo

   integer :: itu,ituf,itvv,itvvf,itp,itpf,itt,ittf,itqv,itqvf

   !-------------------------------------------------------------------------

   if ( iv % num_ships < 1 ) return

   !-------------------------------------------------------------------------

   itu   = 0; itvv    = 0; itp  = 0; itt  = 0; itqv  = 0;
   ituf  = 0; itvvf   = 0; itpf = 0; ittf = 0; itqvf = 0;

   do n=iv%ob_numb(iv%current_ob_time-1)%ships + 1, &
      iv%ob_numb(iv%current_ob_time)%ships

      ! [1.1] Get horizontal interpolation weights:

      i = iv%ships(n)%loc%i
      j = iv%ships(n)%loc%j
      dx = iv%ships(n)%loc%dx
      dy = iv%ships(n)%loc%dy
      dxm = iv%ships(n)%loc%dxm
      dym = iv%ships(n)%loc%dym

      !-----------------Surface correction

      iv%ships(n)%p%inv = ob%ships(n)%p
      iv%ships(n)%t%inv = ob%ships(n)%t
      iv%ships(n)%q%inv = ob%ships(n)%q
      iv%ships(n)%u%inv = ob%ships(n)%u
      iv%ships(n)%v%inv = ob%ships(n)%v

      if (sfc_assi_options == 1) then
         iv%ships(n)%zk=missing_r

         if ( iv % ships(n) % h > missing_r) then
            do k=xp%kts,xp%kte
              v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                        + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
            end do

            hd = v_h(xp%kts) - iv % ships(n) % h

            if (abs(hd) <= Max_StHeight_Diff) then
               if(iv % ships(n) % h < v_h(xp%kts)) then
                  iv%ships(n)%zk = 1.0+1.0e-6
                  call da_obs_sfc_correction(iv%ships(n), xb)

                  ! To keep the original "ob" with no change for multiple 
                  ! outer-loops use:

                  ! ob%ships(n)%p = iv%ships(n)%p%inv
                  ! ob%ships(n)%t = iv%ships(n)%t%inv
                  ! ob%ships(n)%q = iv%ships(n)%q%inv
                  ! ob%ships(n)%u = iv%ships(n)%u%inv
                  ! ob%ships(n)%v = iv%ships(n)%v%inv
               else
                  call da_to_zk(iv % ships(n) % h, v_h, xp, v_interp_h, &
                     iv%ships(n)%zk)
               end if
            else
               iv%ships(n)%zk = missing_r
            end if
         else if ( ob % ships(n) % p > 1.0 ) then
            do k=xp%kts,xp%kte
              v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                       + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
            end do

            call da_to_zk(ob % ships(n) % p, v_p, xp, v_interp_p, &
               iv%ships(n)%zk)

            if(iv%ships(n)%zk < 0.0) then
               iv % ships(n) % p % inv = missing_r
               iv % ships(n) % p % qc  = missing
               iv%ships(n)%zk = 1.0+1.0e-6
            end if
         end if

         !-------------------------------------------------------------------
         ! [2.0] Initialise components of innovation vector:
         !-------------------------------------------------------------------

         if(iv%ships(n)%zk < 0.0) then
            iv % ships(n) % u % qc = missing
            iv % ships(n) % v % qc = missing
            iv % ships(n) % t % qc = missing
            iv % ships(n) % q % qc = missing
            iv % ships(n) % p % qc = missing
         else
            !------------[1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
            call da_interp_obs_lin_2d( xb % u, xp, i, j, dx, dy, dxm, dym, &
                                    model_u, iv%ships(n)%zk)
            call da_interp_obs_lin_2d( xb % v, xp, i, j, dx, dy, dxm, dym, &
                                    model_v, iv%ships(n)%zk)
            call da_interp_obs_lin_2d( xb % t, xp, i, j, dx, dy, dxm, dym, &
                                    model_t, iv%ships(n)%zk)
            call da_interp_obs_lin_2d( xb % q, xp, i, j, dx, dy, dxm, dym, &
                                    model_q, iv%ships(n)%zk)
            call da_interp_obs_lin_2d( xb % p, xp, i, j, dx, dy, dxm, dym, &
                                    model_p, iv%ships(n)%zk)
#else
            call da_interp_obs_lin_2d( xb % u(xp%ims,xp%jms,xb%kms), xp, &
               i, j, dx, dy, dxm, dym, model_u, iv%ships(n)%zk)
            call da_interp_obs_lin_2d( xb % v(xp%ims,xp%jms,xb%kms), xp, &
               i, j, dx, dy, dxm, dym, model_v, iv%ships(n)%zk)
            call da_interp_obs_lin_2d( xb % t(xp%ims,xp%jms,xb%kms), xp, &
               i, j, dx, dy, dxm, dym, model_t, iv%ships(n)%zk)
            call da_interp_obs_lin_2d( xb % q(xp%ims,xp%jms,xb%kms), xp, &
               i, j, dx, dy, dxm, dym, model_q, iv%ships(n)%zk)
            call da_interp_obs_lin_2d( xb % p(xp%ims,xp%jms,xb%kms), xp, &
               i, j, dx, dy, dxm, dym, model_p, iv%ships(n)%zk)
#endif
         end if
      else if (sfc_assi_options == 2) then

         ! Surface data assimilation approca 2

         ! 1.2.1 Surface assimilation approach 2 (10-m u, v, 2-m t, q, and 
         ! sfc_p)

         call da_interp_lin_2d( xb % u10, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_u )
         call da_interp_lin_2d( xb % v10, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_v )
         call da_interp_lin_2d( xb % t2, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_t )
         call da_interp_lin_2d( xb % q2, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_q )
         call da_interp_lin_2d( xb % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_p )

         if (iv%ships(n)%p%qc >= 0) then

            !.......... model surface p, t, q, h at observed site:

            call da_interp_lin_2d( xb % terr, xp%ims, xp%ime, xp%jms,xp%jme,&
                              i, j, dx, dy, dxm, dym, &
                              hsm )

            ho = iv%ships(n)%h
            to = -888888.0
            qo = -888888.0

            if (iv%ships(n)%t%qc >= 0 .and. iv%ships(n)%q%qc >= 0) then
               to = ob%ships(n)%t
               qo = ob%ships(n)%q
               call da_sfc_pre (psfcm, model_p, model_t, model_q, &
                                 hsm, ho, to, qo)
            else if (iv%ships(n)%t%qc >= 0 .and. iv%ships(n)%q%qc < 0) then
                to = ob%ships(n)%t
                call da_sfc_pre (psfcm, model_p, model_t, model_q, &
                                 hsm, ho, to)
            else
                call da_sfc_pre (psfcm, model_p, model_t, model_q, &
                                 hsm, ho)
            end if

            !.......... Pressure at the observed height:
            model_p = psfcm
         end if
      end if

      !-----------------------------------------------------------------------
      ! [3.0] Fast interpolation:
      !-----------------------------------------------------------------------

      if (ob % ships(n) % u > missing_r .AND. &
         iv % ships(n) % u % qc >= obs_qc_pointer ) then
         iv % ships(n) % u % inv = iv%ships(n)%u%inv - model_u
      else
         iv % ships(n) % u % inv = 0.0
      end if

      if (ob % ships(n) % v > missing_r .AND. &
         iv % ships(n) % v % qc >= obs_qc_pointer ) then
         iv % ships(n) % v % inv = iv%ships(n)%v%inv - model_v
      else
         iv % ships(n) % v % inv = 0.0
      end if

      if (ob % ships(n) % p > 0.0 .AND. &
          iv % ships(n) % p % qc >= obs_qc_pointer ) then
         iv % ships(n) % p % inv = iv%ships(n)%p%inv - model_p
      else
         iv % ships(n) % p % inv = 0.0
      end if

      if (ob % ships(n) % t > 0.0 .AND. &
          iv % ships(n) % t % qc >= obs_qc_pointer ) then
         iv % ships(n) % t % inv = iv%ships(n)%t%inv - model_t
      else
         iv % ships(n) % t % inv = 0.0
      end if

      if (ob % ships(n) % q > 0.0 .AND. &
          iv % ships(n) % q % qc >= obs_qc_pointer ) then
         iv % ships(n) % q % inv = iv%ships(n)%q%inv - model_q
      else
         iv % ships(n) % q % inv = 0.0
      end if

      !---------------------------------------------------------------------
      ! [5.0] Perform optional maximum error check:
      !---------------------------------------------------------------------

      if ( check_max_iv ) then
        call da_check_max_iv_ships(it, iv % ships(n), &
                       itu,ituf,itvv,itvvf,itp,itpf,itt,ittf,itqv,itqvf )
      end if
   end do

   if (check_max_iv_print) then
      write(unit= check_max_iv_unit, fmt ='(A,i5,A)')'For outer iteration ',it, &
         ', Total Rejections for Ships follows:'

      write (unit = check_max_iv_unit, fmt = '(/,10(2(A,I6),/))') &
         'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
         'Number of failed v-wind observations:     ',itvvf,' on ',itvv,   &
         'Number of failed pressure observations:   ',itpf, ' on ',itp,   &
         'Number of failed temperature observations:',ittf, ' on ',itt,   &
         'Number of failed mixing ratio observations:',itqvf,' on ',itqv, &
         'Finally Total Ships rejections ',ituf+itvvf+itpf+ittf+itqvf,' on ',&
                                           itu +itvv +itp +itt +itqv
   end if

end subroutine da_get_innov_vector_ships

