SUBROUTINE da_get_innov_vector_gpsref ( it, xb, xp, ob, iv )

! ------------------------------------------------------------------------
!  Called by DA_Obs/da_get_innov_vector.inc
!
!                                          Y.-R. Guo  06/02/2004
! ------------------------------------------------------------------------
   IMPLICIT NONE

   INTEGER, INTENT(IN)             :: it       ! External iteration.
   TYPE (xb_type), INTENT(IN)      :: xb       ! first guess state.
   TYPE (xpose_type), INTENT(IN)   :: xp       ! Domain decomposition vars.
   TYPE (y_type),  INTENT(INOUT)   :: ob       ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)   :: iv       ! O-B structure.

   INTEGER                         :: n        ! Loop counter.
   INTEGER                         :: i, j, k  ! Index dimension.
   INTEGER                         :: num_levs ! Number of obs levels.
   REAL                            :: dx, dxm  ! Interpolation weights.
   REAL                            :: dy, dym  ! Interpolation weights.
   REAL, DIMENSION(1:max_ob_levels) :: model_ref !Model gpsref at ob loc
   REAL, DIMENSION(xp%kms:xp%kme)   :: v_h     ! Model value h at ob
!                                                hor. location.
   INTEGER           :: pref, fref

!
! GPS REF Pseudo OBS test:
!
    if ( pseudo_var(1:3) == 'ref' .and. num_pseudo > 0) then

! Deallocate:
        if (iv%num_gpsref > 0) then
!          print '(a,i6)','iv%num_gpsref=', iv%num_gpsref
          do n = 1, iv%num_gpsref
          deallocate (iv % gpsref(n) %  h )
          deallocate (iv % gpsref(n) %  zk)
          deallocate (iv % gpsref(n) % ref)
          deallocate (iv % gpsref(n) %   p)
          deallocate (iv % gpsref(n) %   t)
          deallocate (iv % gpsref(n) %   q)
          deallocate (ob % gpsref(n) % ref)
          enddo
          DEALLOCATE (iv % gpsref)
          DEALLOCATE (ob % gpsref)
        endif

       use_GpsrefOBS = .true.

! Allocate:
        iv%num_gpsref = num_pseudo
        iv%ob_numb(1)%gpsref = num_pseudo
        iv%num_pseudo = 0

        ALLOCATE (iv % gpsref(1:num_pseudo))
        allocate (iv%gpsref(num_pseudo)%zk (1:1))
        allocate (iv%gpsref(num_pseudo)%ref(1:1))
        allocate (ob%gpsref(1:num_pseudo))
        allocate (ob%gpsref(num_pseudo)%ref(1:1))
        
        write(6,'(a,i2)') '==> GPS REF pseudo OBS test: num_pseudo=',num_pseudo
!       ......................................................................
        
        iv%gpsref(1)%info%levels = 1

        iv % gpsref(1)%loc%x = pseudo_x
        iv % gpsref(1)%loc%y = pseudo_y

        iv%gpsref(1)%loc%i = int(pseudo_x)
        iv%gpsref(1)%loc%j = int(pseudo_y)

        iv%gpsref(1)%loc%dx = pseudo_x-real(iv%gpsref(1)%loc%i)
        iv%gpsref(1)%loc%dy = pseudo_y-real(iv%gpsref(1)%loc%j)
        iv%gpsref(1)%loc%dxm=1.0-iv%gpsref(1)%loc%dx
        iv%gpsref(1)%loc%dym=1.0-iv%gpsref(1)%loc%dy

        iv % gpsref(1) %ref(1) % inv = pseudo_val
        iv % gpsref(1) %ref(1) % error = pseudo_err
        iv % gpsref(1) %ref(1) % qc = 0

! Set halo:
        if((iv%gpsref(1)%loc%i < xp%its-1) .or. (iv%gpsref(1)%loc%i > xp%ite) .or. &
           (iv%gpsref(1)%loc%j < xp%jts-1) .or. (iv%gpsref(1)%loc%j > xp%jte)) then
           iv%gpsref(1)%loc%proc_domain_with_halo = .false.
           iv%gpsref(1)%loc%proc_domain = .false.
        else
           iv%gpsref(1)%loc%proc_domain_with_halo = .true. 
           iv%gpsref(1)%loc%proc_domain = .false. 
    
           if(iv%gpsref(1)%loc%i >= xp%its .and. iv%gpsref(1)%loc%i <= xp%ite .and. & 
              iv%gpsref(1)%loc%j >= xp%jts .and. iv%gpsref(1)%loc%j <= xp%jte) then 
              iv%gpsref(1)%loc%proc_domain = .true. 
           endif 
        endif

        write(6,'(a4,2f15.5)') pseudo_var, pseudo_val, pseudo_err
        write(6,'(3f15.5)')    pseudo_x, pseudo_y, pseudo_z
    endif

! .......................................................................

   IF ( iv%num_gpsref < 1 ) RETURN

   pref = 0 ; fref = 0

   DO n=iv%ob_numb(iv%current_ob_time-1)%gpsref + 1, iv%ob_numb(iv%current_ob_time)%gpsref 

      if(iv%gpsref(n)%loc%proc_domain_with_halo) then

         num_levs = iv%gpsref(n)%info%levels

         if ( num_levs < 1 ) cycle

         model_ref(:) = 0.0

!        Get cross pt. horizontal interpolation weights:

         i = iv%gpsref(n)%loc%i
         j = iv%gpsref(n)%loc%j
         dx = iv%gpsref(n)%loc%dx
         dy = iv%gpsref(n)%loc%dy
         dxm = iv%gpsref(n)%loc%dxm
         dym = iv%gpsref(n)%loc%dym

       if (.not.(pseudo_var(1:3) == 'ref' .and. num_pseudo > 0)) then

!        Get the gpsref%zk from gpsref%h:

         do k=xp%kts,xp%kte
            v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                   + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
         enddo
         do k=1, num_levs
            iv%gpsref(n)%zk(k)=missing_r
            if ( iv%gpsref(n)%h(k) > 0.0 ) &
               call to_zk(iv%gpsref(n)%h(k), v_h, xp, v_interp_h, iv%gpsref(n)%zk(k))
            if(iv%gpsref(n)%zk(k) < 0.0) iv%gpsref(n)%ref(k)%qc = missing
         enddo

       else

         iv % gpsref(1)%zk(1) = pseudo_z

       endif
         
#ifndef DEREF_KLUDGE
         call Interp_lin_3D( xb%ref, xp, i, j, dx, dy, dxm, dym, &
                             model_ref, max_ob_levels, iv%gpsref(n)%zk, num_levs )
#else
         call Interp_lin_3D( xb%ref(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_ref(1), max_ob_levels, iv%gpsref(n)%zk(1), num_levs )
#endif

       if (.not.(pseudo_var(1:3) == 'ref' .and. num_pseudo > 0)) then

         do k = 1, num_levs

            iv%gpsref(n)%ref(k)%inv = 0.0

            if ( ob%gpsref(n)%ref(k) > missing_r .AND. &
                 iv%gpsref(n)%ref(k)%qc >= obs_qc_pointer ) THEN

                 iv%gpsref(n)%ref(k)%inv = ob%gpsref(n)%ref(k) - model_ref(k)
            endif
        
         enddo

       else

         ob % gpsref(1)%ref(1) = model_ref(1) + iv %gpsref(1)%ref(1)%inv 

       endif

      endif

      IF ( check_max_iv ) &
         CALL da_check_max_iv_gpsref(it, iv%gpsref(n), pref, fref )

   END DO

   write(unit = check_max_iv_unit, FMT ='(A,i5,A)') &
        'For outer iteration ',it, &
        ', Total Rejections for GPSRef follows:'
   write(unit = check_max_iv_unit, FMT = '(/,10(2(A,I6),/))') &
        'Number of failed gpsrefes observations:', &
                   fref, ' on ',pref

END SUBROUTINE da_get_innov_vector_gpsref

