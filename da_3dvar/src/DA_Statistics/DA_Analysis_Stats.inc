SUBROUTINE DA_Analysis_Stats ( stats_unit, xp, xa )
   
!---- -------------------------------------------------------------------------
!     PURPOSE: Calculate min, max, mean and RMS of input 1d field.
!
!     METHOD:  Straightforward.
!
!     HISTORY: 09/28/1999 - Creation of F90 version.           Dale Barker
!
!     PARENT MODULE: DA_Statistics
!------------------------------------------------------------------------------

   implicit none

#ifdef DM_PARALLEL
   INCLUDE 'mpif.h'
#endif

   INTEGER,       INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (xpose_type), INTENT(in)   :: xp            ! Dimensions and xpose buffers.
   TYPE (x_type), INTENT (in)      :: xa            ! gridded analysis increments.
   
   INTEGER                         :: i, j, k
   INTEGER                         :: ij, ijk       ! 2D and 3D subdomain sizes.
   INTEGER                         :: ij_g, ijk_g   ! ij, ijk for global domain. 
   integer                         :: is, ie        ! i range of processor subdomain.
   integer                         :: js, je        ! j range of processor subdomain.
   integer                         :: ks, ke, kdim  ! k range 
   integer                         :: ierror        ! MPI return code.

   REAL                            :: rij, rijk                     ! On local domain.
   REAL                            :: um, vm, tm, pm, qm            ! On local domain.
   REAL                            :: rij_g, rijk_g                 ! On global domain.

   TYPE (maxmin_field_type) :: max_u(xp%kts:xp%kte), max_v(xp%kts:xp%kte), &
                               max_t(xp%kts:xp%kte), max_p(xp%kts:xp%kte), &
                               max_q(xp%kts:xp%kte), &
                               min_u(xp%kts:xp%kte), min_v(xp%kts:xp%kte), &
                               min_t(xp%kts:xp%kte), min_p(xp%kts:xp%kte), &
                               min_q(xp%kts:xp%kte)
 
   REAL                        uv(xp%kts:xp%kte), vv(xp%kts:xp%kte), &
                               tv(xp%kts:xp%kte), pv(xp%kts:xp%kte), &
                               qv(xp%kts:xp%kte)

   logical, external        :: wrf_dm_on_monitor

!------------------------------------------------------------------------------

   is = xp%its
   ie = xp%ite
   js = xp%jts
   je = xp%jte
   ks = xp%kts
   ke = xp%kte

   ij  = (ie-is+1) * (je-js+1)
   kdim = ke-ks+1
   ijk = ij*kdim

   ij_g = mix * mjy
   ijk_g = ij_g * mkz
   
   rij  = 1.0/real(ij)
   rijk = 1.0/real(ijk)
   rij_g  = 1.0/real(ij_g)
   rijk_g = 1.0/real(ijk_g)

!------------------------------------------------------------------------------
   
   IF ( wrf_dm_on_monitor() ) THEN
     write(unit=stats_unit, fmt='(/a/)') &
        ' Minimum of gridded analysis increments'

     write(unit=stats_unit, fmt='(6a/)') &
        ' Lvl         ', &
        'u     i    j          ', &
        'v     i    j          ', &
        't     i    j          ', &
        'p     i    j          ', &
        'q     i    j          '
   END IF

   call maxmin_in_field(xa%u(is:ie,js:je,ks:ke), max_u, min_u)
   call proc_maxmin_combine(kdim, max_u, min_u)
   call maxmin_in_field(xa%v(is:ie,js:je,ks:ke), max_v, min_v)
   call proc_maxmin_combine(kdim, max_v, min_v)
   call maxmin_in_field(xa%t(is:ie,js:je,ks:ke), max_t, min_t)
   call proc_maxmin_combine(kdim, max_t, min_t)
   call maxmin_in_field(xa%p(is:ie,js:je,ks:ke), max_p, min_p)
   call proc_maxmin_combine(kdim, max_p, min_p)
   call maxmin_in_field(xa%q(is:ie,js:je,ks:ke), max_q, min_q)
   call proc_maxmin_combine(kdim, max_q, min_q)

   um = 999999.0
   vm = 999999.0
   tm = 999999.0
   pm = 999999.0
   qm = 999999.0

   do k = ks, ke
   
      IF ( wrf_dm_on_monitor() ) THEN
       write(unit=stats_unit, fmt='(i4,4(f12.4,2i5),e12.4,2i5)') k, &
              min_u(k), min_v(k), min_t(k), min_p(k), min_q(k)
      END IF

      um=minval(min_u(:)%value)
      vm=minval(min_v(:)%value)
      tm=minval(min_t(:)%value)
      pm=minval(min_p(:)%value)
      qm=minval(min_q(:)%value)

   end do

   IF ( wrf_dm_on_monitor() ) THEN
     write(unit=stats_unit, fmt='(a,4(f12.4,10x),e12.4)') ' ALL', &
          um, vm, tm, pm, qm
   
!------------------------------------------------------------------------------
  
     write(unit=stats_unit, fmt='(/a/)') ' Maximum of gridded analysis increments'

     write(unit=stats_unit, fmt='(6a/)') &
        ' Lvl         ', &
        'u     i    j          ', &
        'v     i    j          ', &
        't     i    j          ', &
        'p     i    j          ', &
        'q     i    j          '
   END IF

   do k = ks, ke
   
     IF ( wrf_dm_on_monitor() ) THEN
        write(unit=stats_unit, fmt='(i4,4(f12.4,2i5),e12.4,2i5)') k, &
                max_u(k), max_v(k), max_t(k), max_p(k), max_q(k)
     END IF

     um=maxval(max_u(:)%value)
     vm=maxval(max_v(:)%value)
     tm=maxval(max_t(:)%value)
     pm=maxval(max_p(:)%value)
     qm=maxval(max_q(:)%value)

   end do

   IF ( wrf_dm_on_monitor() ) THEN
      write(unit=stats_unit, fmt='(a,4(f12.4,10x),e12.4)') ' ALL', &
           um, vm, tm, pm, qm
   
!------------------------------------------------------------------------------
 
   write(unit=stats_unit, fmt='(/a/)') ' Mean of gridded analysis increments'

   write(unit=stats_unit, fmt='(a/)') &
        ' Lvl        u           v           t           p           q'
   END IF

   um = 0.0
   vm = 0.0
   tm = 0.0
   pm = 0.0
   qm = 0.0

   do k = ks, ke
      uv(k) = sum(xa%u(is:ie,js:je,k))
      vv(k) = sum(xa%v(is:ie,js:je,k))
      tv(k) = sum(xa%t(is:ie,js:je,k))
      pv(k) = sum(xa%p(is:ie,js:je,k))
      qv(k) = sum(xa%q(is:ie,js:je,k))
   end do

   call proc_sum_real ( kdim, uv )
   call proc_sum_real ( kdim, vv )
   call proc_sum_real ( kdim, tv )
   call proc_sum_real ( kdim, pv )
   call proc_sum_real ( kdim, qv )

   IF ( wrf_dm_on_monitor() ) THEN
     do k = ks, ke
        write(unit=stats_unit, fmt='(i4,4f12.4,4e12.4)') k, &
            uv(k)*rij_g, vv(k)*rij_g, tv(k)*rij_g, &
            pv(k)*rij_g, qv(k)*rij_g

        um=um+uv(k)
        vm=vm+vv(k)
        tm=tm+tv(k)
        pm=pm+pv(k)
        qm=qm+qv(k)

     end do
   END IF

   IF ( wrf_dm_on_monitor() ) THEN
     write(unit=stats_unit, fmt='(a,4f12.4,4e12.4)') ' ALL', &
        um*rijk_g, vm*rijk_g, tm*rijk_g, pm*rijk_g, qm*rijk_g

!------------------------------------------------------------------------------
 
     write(unit=stats_unit, fmt='(/a/)') ' RMSE of gridded analysis increments'

     write(unit=stats_unit, fmt='(a/)') &
        ' Lvl        u           v           t           p           q'
   ENDIF

   um = 0.0
   vm = 0.0
   tm = 0.0
   pm = 0.0
   qm = 0.0
   uv = 0.0
   vv = 0.0
   tv = 0.0
   pv = 0.0
   qv = 0.0

   do k = ks, ke
      do j=js,je
      do i=is,ie
         uv(k) = uv(k) + xa%u(i,j,k) * xa%u(i,j,k)
         vv(k) = vv(k) + xa%v(i,j,k) * xa%v(i,j,k)
         tv(k) = tv(k) + xa%t(i,j,k) * xa%t(i,j,k)
         pv(k) = pv(k) + xa%p(i,j,k) * xa%p(i,j,k)
         qv(k) = qv(k) + xa%q(i,j,k) * xa%q(i,j,k)
      enddo
      enddo
   end do

   call proc_sum_real ( kdim, uv )
   call proc_sum_real ( kdim, vv )
   call proc_sum_real ( kdim, tv )
   call proc_sum_real ( kdim, pv )
   call proc_sum_real ( kdim, qv )

   IF ( wrf_dm_on_monitor() ) THEN
     do k = ks, ke
        write(unit=stats_unit, fmt='(i4,4f12.4,4e12.4)') k, &
            sqrt(uv(k)*rij_g), &
            sqrt(vv(k)*rij_g), &
            sqrt(tv(k)*rij_g), &
            sqrt(pv(k)*rij_g), &
            sqrt(qv(k)*rij_g)

        um=um+uv(k)
        vm=vm+vv(k)
        tm=tm+tv(k)
        pm=pm+pv(k)
        qm=qm+qv(k)
     end do
   END IF

   IF ( wrf_dm_on_monitor() ) THEN
     write(unit=stats_unit, fmt='(a,4f12.4,4e12.4)') ' ALL', &
         sqrt(um*rijk_g), sqrt(vm*rijk_g), sqrt(tm*rijk_g), &
         sqrt(pm*rijk_g), sqrt(qm*rijk_g)
   END IF

!------------------------------------------------------------------------------

contains

  subroutine maxmin_in_field(field, max, min)

   REAL                  :: field(is:ie,js:je,ks:ke)

   TYPE (maxmin_field_type) :: max(xp%kts:xp%kte), min(xp%kts:xp%kte)

   do k = ks, ke
     max(k)%value = -1.E20
     min(k)%value =  1.E20
     do j = js, je
     do i = is, ie
       if (field(i,j,k) > max(k)%value) then
          max(k)%value = field(i,j,k)
          max(k)%i     = i
          max(k)%j     = j
       endif
       if (field(i,j,k) < min(k)%value) then
          min(k)%value = field(i,j,k)
          min(k)%i     = i
          min(k)%j     = j
       endif
     end do
     end do
   end do

   end subroutine maxmin_in_field
   
END SUBROUTINE DA_Analysis_Stats
