SUBROUTINE da_transform_xtoy_rad ( xa, iv, xp, y )
!------------------------------------------------------------------------------
!  PURPOSE: transform from analysis increment to 
!                          pertubation radiance.
!
!  METHOD:  delta_y = H delta_x
!           1. input reference state of RTTOV_TL
!           2. interpolate analysis increment to obs location
!           3. Call RTTOV_TL
!
!  HISTORY: 27/07/2005 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

!#include "rttov_q2v.interface"

   TYPE (x_type), INTENT(IN)      :: xa       ! delta_x
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (y_type),  INTENT(INOUT)  :: y        ! H' delta_x
   TYPE (ob_type), INTENT(IN)     :: iv       ! O-B structure.

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
   INTEGER                        :: alloc_status(40)

   REAL, DIMENSION(1:max_ob_levels) :: model_t  ! Model value t at ob location.
   REAL, DIMENSION(1:max_ob_levels) :: model_q  ! Model value q(kg/kg) at ob location.
   REAL, DIMENSION(1:max_ob_levels) :: model_mr ! Model value mr(ppmv) at ob location.
   REAL                             :: model_t2m
   REAL                             :: model_q2m, model_mr2m
   REAL                             :: model_u10
   REAL                             :: model_v10
   REAL                             :: model_psfc
   REAL                             :: model_ts

   REAL, DIMENSION(xp%kms:xp%kme)   :: v_p      ! Model value p at ob hor. location.

   INTEGER            :: inst, nprofiles, nchanl, unit, iprof, ios
   REAL,    pointer   :: zk(:)
   REAL,    pointer   :: pres(:)
   character(len=30)  :: filename
   real,    pointer   :: tb(:,:)
!
   type(con_vars_type),  pointer  :: con_vars(:), con_vars_tl(:)
   type(aux_vars_type),  pointer  :: aux_vars(:), aux_vars_tl(:)

#if !defined(RTTOV)
    CALL wrf_error_fatal3(__FILE__,__LINE__, &
      "Must compile with $RTTOV option for radiances")
#else

!  print *, ' Computing Radiance Increment Hdx ------- '
  alloc_status(:) = 0

  IF ( iv%num_inst < 1 ) return

 do inst = 1, iv%num_inst                 ! loop for sensor
     if ( iv%instid(inst)%num_rad < 1 ) cycle
     nprofiles = 1
     num_levs  = iv%instid(inst)%nlevels
     nchanl    = iv%instid(inst)%nchan

     allocate (zk(1:num_levs), stat= alloc_status(1) )
     allocate (pres(1:num_levs), stat= alloc_status(2) )
     allocate (tb(nprofiles,nchanl), stat= alloc_status(3) )
     allocate (con_vars(nprofiles), stat= alloc_status(4) )
     allocate (con_vars_tl(nprofiles), stat= alloc_status(5) )
     allocate (aux_vars(nprofiles), stat= alloc_status(6) )
     allocate (aux_vars_tl(nprofiles), stat= alloc_status(7) )
     if ( any(alloc_status /= 0) ) then
       CALL wrf_error_fatal3(__FILE__,__LINE__, &
         "Canot allocate memory to con_vars")
     end if

     pres(1:num_levs) = coefs(inst) % ref_prfl_p(1:num_levs)

   do n= 1, iv%instid(inst)%num_rad           ! loop for pixel

      if(iv%instid(inst)%rad(n)%loc%proc_domain_with_halo) then

         model_t(:) = 0.0
         model_q(:) = 0.0
         model_mr(:)= 0.0
         model_t2m  = 0.0
         model_q2m  = 0.0
         model_mr2m  = 0.0
         model_ts   = 0.0
         model_u10  = 0.0
         model_v10  = 0.0
         model_psfc = 0.0

!        [1.1] Get horizontal interpolation weights:

           i = iv%instid(inst)%rad(n)%loc%i
           j = iv%instid(inst)%rad(n)%loc%j
          dx = iv%instid(inst)%rad(n)%loc%dx
          dy = iv%instid(inst)%rad(n)%loc%dy
         dxm = iv%instid(inst)%rad(n)%loc%dxm
         dym = iv%instid(inst)%rad(n)%loc%dym

         do k=1, num_levs
            zk(k)  =  iv%instid(inst)%rad(n)%zk(k) 
         enddo

!        [1.2] Interpolate horizontally to ob:

         call Interp_lin_3D( xa%t, xp, i, j, dx, dy, dxm, dym, &
                              model_t, max_ob_levels, zk, num_levs )
         call Interp_lin_3D( xa%q, xp, i, j, dx, dy, dxm, dym, &
                              model_q, max_ob_levels, zk, num_levs )

         do k=1, num_levs

            IF ( zk(k) <= 0.0 ) THEN
               model_t(k)  = 0.  !coefs(inst) % ref_prfl_t(k,gas_id_watervapour)
               model_mr(k) = 0.  !coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
            ELSE
!            model_mr(k) = model_q(k) * 1.60771704e+6 ! convert q(kg/kg) to ppmv 
            model_mr(k) = model_q(k) * q2ppmv
!               call rttov_q2v (&
!                 & gas_unit_specconc,  &! in
!                 & model_q(k),         &! in
!                 & gas_id_watervapour, &! in
!                 & model_q(k),         &! in
!                 & model_mr(k)     )    ! inout
            END IF

         end do


         call Interp_lin_2D( xa % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                             i, j, dx, dy, dxm, dym, &
                             model_psfc )

         model_psfc = 0.01*model_psfc  ! convert to hPa 

        do iprof = 1, nprofiles

              con_vars(iprof) % nlevels = num_levs 
              con_vars_tl(iprof) % nlevels = num_levs

              allocate (con_vars(iprof) % t(num_levs))
              allocate (con_vars(iprof) % q(num_levs))
              allocate (con_vars_tl(iprof) % t(num_levs))
              allocate (con_vars_tl(iprof) % q(num_levs))

              con_vars_tl(iprof) % t(1:num_levs) = model_t  (1:num_levs)
              con_vars_tl(iprof) % q(1:num_levs) = model_mr (1:num_levs)
              con_vars_tl(iprof) % ps            = model_psfc

              aux_vars_tl(iprof) % t2m = model_t2m
              aux_vars_tl(iprof) % q2m = model_mr2m
              aux_vars_tl(iprof) % u10 = model_u10
              aux_vars_tl(iprof) % v10 = model_v10

              aux_vars_tl(iprof) % surftype = iv%instid(inst)%rad(n)%landsea_mask 
              aux_vars_tl(iprof) % surft   = model_ts
!              aux_vars_tl(iprof) % fastem(:) = 0.

              aux_vars_tl(iprof) % satzen  = iv%instid(inst)%rad(n)%satzen
              aux_vars_tl(iprof) % satazi  = iv%instid(inst)%rad(n)%satazi

              con_vars(iprof) % t(:) = iv%instid(inst)%rad(n)%t (:)
              con_vars(iprof) % q(:) = iv%instid(inst)%rad(n)%mr(:)
              con_vars(iprof) % ps   = iv%instid(inst)%rad(n)%ps 

              aux_vars(iprof) % t2m = iv%instid(inst)%rad(n)%t2m
              aux_vars(iprof) % q2m = iv%instid(inst)%rad(n)%mr2m
              aux_vars(iprof) % u10 = iv%instid(inst)%rad(n)%u10
              aux_vars(iprof) % v10 = iv%instid(inst)%rad(n)%v10

              aux_vars(iprof) % surftype = iv%instid(inst)%rad(n)%landsea_mask
              aux_vars(iprof) % surft    = iv%instid(inst)%rad(n)%ts
!              aux_vars(iprof) % fastem(:) = 0.

              aux_vars(iprof) % satzen  = iv%instid(inst)%rad(n)%satzen
              aux_vars(iprof) % satazi  = iv%instid(inst)%rad(n)%satazi


        end do

!        [1.3] Call RTM TL model

         call da_rtm_tl (inst, nchanl, nprofiles, con_vars, aux_vars, &
                                  con_vars_tl, aux_vars_tl, tb )

       do iprof = 1, nprofiles
         deallocate (con_vars(iprof) % t)
         deallocate (con_vars(iprof) % q)
         deallocate (con_vars_tl(iprof) % t)
         deallocate (con_vars_tl(iprof) % q)
       end do
!------------------------------------------------------------------------
!        [2.0] assign Hdx :
!------------------------------------------------------------------------

         do k = 1, nchanl
            y%instid(inst)%rad(n)%tb(k) = tb(nprofiles,k)
         end do
               
       end if   !  end endif for within_halo

   end do       !  end loop for pixels

     deallocate (zk)
     deallocate (pres)
     deallocate (tb)
     deallocate (con_vars)
     deallocate (aux_vars)
     deallocate (con_vars_tl)
     deallocate (aux_vars_tl)

  end do        ! end loop for sensor
#endif

 return
 
END SUBROUTINE da_transform_xtoy_rad

