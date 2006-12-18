SUBROUTINE da_get_innov_vector_sonde_sfc ( it, xb, xp, ob, iv )
!-------------------------------------------------------------------------
!
!  History:
!
!
!     Additions:        03/19/2003                    S. R. H. Rizvi
!
!               1. DA_Check_Max_IV is called for each outer iteration
!
!               2. Extra diagnostics added for generating detailed information
!                  about rejected obs in various outer iterations
!
!     Modification:	02/18/2004		Wei Huang
!		Make obs. operator independent of cv-options.
!                       03/26/2004              Y.-R. Guo
!               Keep "ob" as original values for use of the multiple outer-loops.
!
!-------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN)           :: it      ! External iteration.
   TYPE (xb_type), INTENT(IN)    :: xb      ! first guess state.
   TYPE (xpose_type), INTENT(IN) :: xp      ! Domain decomposition vars.
   TYPE (y_type),  INTENT(INOUT) :: ob      ! Observation structure.
   TYPE (ob_type), INTENT(INOUT) :: iv      ! O-B structure.

!-------------------------------------------------------------------------

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j, k  ! Index dimension.
   REAL                         :: dx, dxm  ! Interpolation weights.
   REAL                         :: dy, dym  ! Interpolation weights.
   REAL                         :: model_u  ! Model value u at oblocation.
   REAL                         :: model_v  ! Model value v at oblocation.
   REAL                         :: model_t  ! Model value t at oblocation.
   REAL                         :: model_p  ! Model value p at oblocation.
   REAL                         :: model_q  ! Model value q at oblocation.
   REAL                         :: model_h  ! Model value h at oblocation.

   REAL, DIMENSION(xp%kms:xp%kme) :: v_h      ! Model value h at ob hor. location.
   REAL, DIMENSION(xp%kms:xp%kme) :: v_p      ! Model value p at ob hor. location.

   REAL :: hd, psfcm

   REAL :: tsm , qsm , psm , hsm , ho, to, qo

   INTEGER :: itu,ituf,itvv,itvvf,itp,itpf,itt,ittf,itqv,itqvf

!-------------------------------------------------------------------------

   if ( iv % num_sound < 1 ) return

!-------------------------------------------------------------------------

   itu   = 0; itvv    = 0; itp  = 0; itt  = 0; itqv  = 0;
   ituf  = 0; itvvf   = 0; itpf = 0; ittf = 0; itqvf = 0;

   do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, iv%ob_numb(iv%current_ob_time)%sound

      if(iv%sonde_sfc(n)%loc%proc_domain_with_halo) then  ! domain_with_halo

!     [1.1] Get horizontal interpolation weights:

      i = iv%sonde_sfc(n)%loc%i
      j = iv%sonde_sfc(n)%loc%j
      dx = iv%sonde_sfc(n)%loc%dx
      dy = iv%sonde_sfc(n)%loc%dy
      dxm = iv%sonde_sfc(n)%loc%dxm
      dym = iv%sonde_sfc(n)%loc%dym

!-----------------Surface correction

      iv%sonde_sfc(n)%p%inv = ob%sonde_sfc(n)%p
      iv%sonde_sfc(n)%t%inv = ob%sonde_sfc(n)%t
      iv%sonde_sfc(n)%q%inv = ob%sonde_sfc(n)%q
      iv%sonde_sfc(n)%u%inv = ob%sonde_sfc(n)%u
      iv%sonde_sfc(n)%v%inv = ob%sonde_sfc(n)%v

      if (sfc_assi_options == 1) then
         iv%sonde_sfc(n)%zk=missing_r

         IF ( iv % sonde_sfc(n) % h > missing_r) THEN
            do k=xp%kts,xp%kte
               v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                      + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
            enddo

            hd = v_h(xp%kts) - iv % sonde_sfc(n) % h

            if ( (abs(hd) <= Max_StHeight_Diff) .or. &
                 (ANALYSIS_TYPE == 'VERIFY' .or. ANALYSIS_TYPE == 'verify') ) THEN

               if(iv % sonde_sfc(n) % h < v_h(xp%kts)) then

                  iv%sonde_sfc(n)%zk = 1.0+1.0e-6

                  CALL DA_Obs_Sfc_correction(iv%sonde_sfc(n), xb, sfc_assi_options)

! To keep the original "ob" with no change for multiple outer-loops use:

!                  ob%sonde_sfc(n)%p = iv%sonde_sfc(n)%p%inv
!                  ob%sonde_sfc(n)%t = iv%sonde_sfc(n)%t%inv
!                  ob%sonde_sfc(n)%q = iv%sonde_sfc(n)%q%inv
!                  ob%sonde_sfc(n)%u = iv%sonde_sfc(n)%u%inv
!                  ob%sonde_sfc(n)%v = iv%sonde_sfc(n)%v%inv
               else
                  call to_zk(iv % sonde_sfc(n) % h, v_h, xp, v_interp_h, iv%sonde_sfc(n)%zk)
               endif
            else
               iv%sonde_sfc(n)%zk = missing_r
            endif
         ELSE IF ( ob % sonde_sfc(n) % p > 1.0 ) THEN
            do k=xp%kts,xp%kte
               v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                      + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
            enddo

            call to_zk(ob % sonde_sfc(n) % p, v_p, xp, v_interp_p, iv%sonde_sfc(n)%zk)

            if(iv%sonde_sfc(n)%zk < 0.0 .and. &
              (ANALYSIS_TYPE /= 'VERIFY' .and. ANALYSIS_TYPE /= 'verify') ) then
               iv % sonde_sfc(n) % p % inv = missing_r
               iv % sonde_sfc(n) % p % qc  = missing
 
               iv%sonde_sfc(n)%zk = 1.0+1.0e-6
            endif
         ENDIF

!------------------------------------------------------------------------
!        [2.0] Initialise components of innovation vector:
!------------------------------------------------------------------------

          if(iv%sonde_sfc(n)%zk < 0.0 .and. &
           (ANALYSIS_TYPE /= 'VERIFY' .and. ANALYSIS_TYPE /= 'verify') ) then
            iv % sonde_sfc(n) % u % qc = missing
            iv % sonde_sfc(n) % v % qc = missing
            iv % sonde_sfc(n) % t % qc = missing
            iv % sonde_sfc(n) % q % qc = missing
            iv % sonde_sfc(n) % p % qc = missing
          else
!------------[1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
             call Interp_Obs_lin_2D( xb % u, xp, i, j, dx, dy, dxm, dym, &
                                     model_u, iv%sonde_sfc(n)%zk)
             call Interp_Obs_lin_2D( xb % v, xp, i, j, dx, dy, dxm, dym, &
                                     model_v, iv%sonde_sfc(n)%zk)
             call Interp_Obs_lin_2D( xb % t, xp, i, j, dx, dy, dxm, dym, &
                                     model_t, iv%sonde_sfc(n)%zk)
             call Interp_Obs_lin_2D( xb % q, xp, i, j, dx, dy, dxm, dym, &
                                     model_q, iv%sonde_sfc(n)%zk)
             call Interp_Obs_lin_2D( xb % p, xp, i, j, dx, dy, dxm, dym, &
                                     model_p, iv%sonde_sfc(n)%zk)
#else
             call Interp_Obs_lin_2D( xb % u(xp%ims,xp%jms,xb%kms), xp, i, j, dx, dy, dxm, dym, &
                                     model_u, iv%sonde_sfc(n)%zk)
             call Interp_Obs_lin_2D( xb % v(xp%ims,xp%jms,xb%kms), xp, i, j, dx, dy, dxm, dym, &
                                     model_v, iv%sonde_sfc(n)%zk)
             call Interp_Obs_lin_2D( xb % t(xp%ims,xp%jms,xb%kms), xp, i, j, dx, dy, dxm, dym, &
                                     model_t, iv%sonde_sfc(n)%zk)
             call Interp_Obs_lin_2D( xb % q(xp%ims,xp%jms,xb%kms), xp, i, j, dx, dy, dxm, dym, &
                                     model_q, iv%sonde_sfc(n)%zk)
             call Interp_Obs_lin_2D( xb % p(xp%ims,xp%jms,xb%kms), xp, i, j, dx, dy, dxm, dym, &
                                     model_p, iv%sonde_sfc(n)%zk)
#endif
          endif
      else if (sfc_assi_options == 2) then

!------- Surface data assimilation approca 2
!-------------------------------------------

! Retrieve the surface pressure from the Sea Level Pressure (SLP) first:
         CALL DA_Obs_Sfc_correction(iv%sonde_sfc(n), xb, sfc_assi_options)
         ob%sonde_sfc(n)%p = iv%sonde_sfc(n)%p%inv         

!------- 1.2.1 Surface assimilation approach 2 (10-m u, v, 2-m t, q, and sfc_p)

         call Interp_lin_2D( xb % u10, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_u )
         call Interp_lin_2D( xb % v10, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_v )
         call Interp_lin_2D( xb % t2, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_t )
         call Interp_lin_2D( xb % q2, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_q )
         call Interp_lin_2D( xb % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_p )

! No height, no sfc pressure calculation (YRG 02/16/2006):
         if (iv%sonde_sfc(n)%h == missing_r ) iv%sonde_sfc(n)%p%qc = missing

         if (iv%sonde_sfc(n)%p%qc >= 0) then

!.......... model surface p, t, q, h at observed site:

            call Interp_lin_2D( xb % terr, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             hsm )

            ho = iv%sonde_sfc(n)%h
            to = -888888.0
            qo = -888888.0

            if (iv%sonde_sfc(n)%t%qc >= 0 .and. iv%sonde_sfc(n)%q%qc >= 0) then
               to = ob%sonde_sfc(n)%t
               qo = ob%sonde_sfc(n)%q
               call da_sfc_pre (psfcm, model_p, model_t, model_q, &
                                hsm, ho, to, qo)
            else if (iv%sonde_sfc(n)%t%qc >= 0 .and. iv%sonde_sfc(n)%q%qc < 0) then
               to = ob%sonde_sfc(n)%t
               call da_sfc_pre (psfcm, model_p, model_t, model_q, &
                                hsm, ho, to)
            else
               call da_sfc_pre (psfcm, model_p, model_t, model_q, &
                                hsm, ho)
            endif

!.......... Pressure at the observed height:
            model_p = psfcm
         endif
      endif

!-----------------------------------------------------------------------
!     [3.0] Fast interpolation:
!-----------------------------------------------------------------------

      IF(ob % sonde_sfc(n) % u > missing_r .AND. &
         iv % sonde_sfc(n) % u % qc >= obs_qc_pointer ) THEN
         iv % sonde_sfc(n) % u % inv = iv%sonde_sfc(n)%u%inv - model_u
      ELSE
         iv % sonde_sfc(n) % u % inv = 0.0
      ENDIF

      IF(ob % sonde_sfc(n) % v > missing_r .AND. &
         iv % sonde_sfc(n) % v % qc >= obs_qc_pointer ) THEN
         iv % sonde_sfc(n) % v % inv = iv%sonde_sfc(n)%v%inv - model_v
      ELSE
         iv % sonde_sfc(n) % v % inv = 0.0
      ENDIF

      IF(ob % sonde_sfc(n) % p > 0.0 .AND. &
         iv % sonde_sfc(n) % p % qc >= obs_qc_pointer ) THEN
         iv % sonde_sfc(n) % p % inv = iv%sonde_sfc(n)%p%inv - model_p
      ELSE
         iv % sonde_sfc(n) % p % inv = 0.0
      ENDIF

      IF(ob % sonde_sfc(n) % t > 0.0 .AND. &
         iv % sonde_sfc(n) % t % qc >= obs_qc_pointer ) THEN
         iv % sonde_sfc(n) % t % inv = iv%sonde_sfc(n)%t%inv - model_t
      ELSE
         iv % sonde_sfc(n) % t % inv = 0.0
      ENDIF

      IF(ob % sonde_sfc(n) % q > 0.0 .AND. &
         iv % sonde_sfc(n) % q % qc >= obs_qc_pointer ) THEN
         iv % sonde_sfc(n) % q % inv = iv%sonde_sfc(n)%q%inv - model_q
      ELSE
         iv % sonde_sfc(n) % q % inv = 0.0
      ENDIF

!-----------------------------------------------------------------------
!     [5.0] Perform optional maximum error check:
!-----------------------------------------------------------------------

      IF ( check_max_iv ) &
         CALL da_check_max_iv_sonde_sfc(it, iv % sonde_sfc(n), &
                        itu,ituf,itvv,itvvf,itp,itpf,itt,ittf,itqv,itqvf )

      endif  ! domain_with_halo

   enddo
!
   write(unit= check_max_iv_unit, FMT ='(A,i5,A)')'For outer iteration ',it, &
                   ', Total Rejections for Sonde_Sfc follows:'

   WRITE (UNIT = check_max_iv_unit, FMT = '(/,10(2(A,I6),/))') &
      'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
      'Number of failed v-wind observations:     ',itvvf,' on ',itvv,   &
      'Number of failed pressure observations:   ',itpf, ' on ',itp,   &
      'Number of failed temperature observations:',ittf, ' on ',itt,   &
      'Number of failed mixing ratio observations:',itqvf,' on ',itqv, &
      'Finally Total Sonde_Sfc rejections ',ituf+itvvf+itpf+ittf+itqvf,' on ',&
                                        itu +itvv +itp +itt +itqv

END SUBROUTINE da_get_innov_vector_sonde_sfc

