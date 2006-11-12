subroutine da_get_innov_vector_gpsref ( it, xb, xp, ob, iv )

   !----------------------------------------------------------------------
   ! Called by da_obs/da_get_innov_vector.inc
   !----------------------------------------------------------------------
   implicit none

   integer, intent(in)             :: it       ! External iteration.
   type (xb_type), intent(in)      :: xb       ! first guess state.
   type (xpose_type), intent(in)   :: xp       ! Domain decomposition vars.
   type (y_type),  intent(inout)   :: ob       ! Observation structure.
   type (ob_type), intent(inout)   :: iv       ! O-B structure.

   integer                         :: n        ! Loop counter.
   integer                         :: i, j, k  ! Index dimension.
   integer                         :: num_levs ! Number of obs levels.
   real                            :: dx, dxm  ! Interpolation weights.
   real                            :: dy, dym  ! Interpolation weights.
   real, dimension(1:max_ob_levels) :: model_ref !Model gpsref at ob loc
   real, dimension(xp%kms:xp%kme)   :: v_h     ! Model value h at ob
                                               ! hor. location.
   integer           :: pref, fref

   ! For quality control

   real   , parameter :: h_1 = 7000.0, h_2 = 25000.0
   ! Lidia Cucurull values:
   real   , parameter :: pcnt1 = 0.05, pcnt2 = 0.04, pcnt3 = 0.10
   ! testing values: 
   ! real   , parameter :: pcnt1 = 0.02, pcnt2 = 0.01, pcnt3 = 0.03
   integer, parameter :: qc_below = -31, qc_middle = -32, qc_above = -33

   integer :: nn, n1, ntotal, nqc0, nqc1, nqc2, nqc3
   real    :: percnt
   character (len=40), dimension(5000) :: name_qc
   real              , dimension(5000) :: height_below

   ! GPS REF Pseudo OBS test:

   if ( pseudo_var(1:3) == 'ref' .and. num_pseudo > 0) then

      ! Deallocate:
      if (iv%num_gpsref > 0) then
         ! write(unit=stdout,fmt='(a,i6)') 'iv%num_gpsref=', iv%num_gpsref
         do n = 1, iv%num_gpsref
            deallocate (iv % gpsref(n) %  h )
            deallocate (iv % gpsref(n) %  zk)
            deallocate (iv % gpsref(n) % ref)
            deallocate (iv % gpsref(n) %   p)
            deallocate (iv % gpsref(n) %   t)
            deallocate (iv % gpsref(n) %   q)
            deallocate (ob % gpsref(n) % ref)
         end do
         deallocate (iv % gpsref)
         deallocate (ob % gpsref)
      end if

      use_GpsrefOBS = .true.

      ! Allocate:
      iv%num_gpsref = num_pseudo
      iv%ob_numb(1)%gpsref = num_pseudo
      iv%num_pseudo = 0

      allocate (iv % gpsref(1:num_pseudo))
      allocate (iv%gpsref(num_pseudo)%zk (1:1))
      allocate (iv%gpsref(num_pseudo)%ref(1:1))
      allocate (ob%gpsref(1:num_pseudo))
      allocate (ob%gpsref(num_pseudo)%ref(1:1))

      write(unit=stdout,fmt='(a,i2)') &
         '==> GPS REF pseudo OBS test: num_pseudo=',num_pseudo

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
      if ((iv%gpsref(1)%loc%i < xp%its-1) .or. &
         (iv%gpsref(1)%loc%i > xp%ite)    .or. &
         (iv%gpsref(1)%loc%j < xp%jts-1)  .or. &
         (iv%gpsref(1)%loc%j > xp%jte)) then
!         iv%gpsref(1)%loc%proc_domain_with_halo = .false.
       call da_error(__FILE__,__LINE__,(/"Should never have obs outside halo by now"/))

         iv%gpsref(1)%loc%proc_domain = .false.
      else
!         iv%gpsref(1)%loc%proc_domain_with_halo = .true. 
         iv%gpsref(1)%loc%proc_domain = .false. 

         if (iv%gpsref(1)%loc%i >= xp%its .and. &
             iv%gpsref(1)%loc%i <= xp%ite .and. & 
             iv%gpsref(1)%loc%j >= xp%jts .and. &
             iv%gpsref(1)%loc%j <= xp%jte) then 
            iv%gpsref(1)%loc%proc_domain = .true. 
         end if 
      end if

      write(unit=stdout,fmt='(a4,2f15.5)') pseudo_var, pseudo_val, pseudo_err
      write(unit=stdout,fmt='(3f15.5)')    pseudo_x, pseudo_y, pseudo_z
   end if


   if (iv%num_gpsref < 1) return

   ntotal = 0
   pref = 0 ; fref = 0

   do n=iv%ob_numb(iv%current_ob_time-1)%gpsref + 1, &
      iv%ob_numb(iv%current_ob_time)%gpsref 
      num_levs = iv%gpsref(n)%info%levels

      if ( num_levs < 1 ) cycle

      model_ref(:) = 0.0

      ! Get cross pt. horizontal interpolation weights:

      i = iv%gpsref(n)%loc%i
      j = iv%gpsref(n)%loc%j
      dx = iv%gpsref(n)%loc%dx
      dy = iv%gpsref(n)%loc%dy
      dxm = iv%gpsref(n)%loc%dxm
      dym = iv%gpsref(n)%loc%dym

      if (.not.(pseudo_var(1:3) == 'ref' .and. num_pseudo > 0)) then

         ! Get the gpsref%zk from gpsref%h:

         do k=xp%kts,xp%kte
            v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                   + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
         end do
         do k=1, num_levs
            iv%gpsref(n)%zk(k)=missing_r
            if ( iv%gpsref(n)%h(k) > 0.0 ) &
               call da_to_zk(iv%gpsref(n)%h(k), v_h, xp, v_interp_h, &
                  iv%gpsref(n)%zk(k))
            if (iv%gpsref(n)%zk(k) < 0.0) iv%gpsref(n)%ref(k)%qc = missing
         end do
      else
         iv % gpsref(1)%zk(1) = pseudo_z
      end if

#ifndef DEREF_KLUDGE
      call da_interp_lin_3d( xb%ref, xp, i, j, dx, dy, dxm, dym, &
         model_ref, max_ob_levels, iv%gpsref(n)%zk, num_levs )
#else
      call da_interp_lin_3d( xb%ref(xp%ims,xp%jms,xp%kms), xp, i, j, &
         dx, dy, dxm, dym, &
         model_ref(1), max_ob_levels, iv%gpsref(n)%zk(1), num_levs )
#endif

      if (.not.(pseudo_var(1:3) == 'ref' .and. num_pseudo > 0)) then
         do k = 1, num_levs
            iv%gpsref(n)%ref(k)%inv = 0.0
            if ( ob%gpsref(n)%ref(k) > missing_r .AND. &
                 iv%gpsref(n)%ref(k)%qc >= obs_qc_pointer ) then
               iv%gpsref(n)%ref(k)%inv = ob%gpsref(n)%ref(k) - model_ref(k)
            end if
         end do
      else
         ob % gpsref(1)%ref(1) = model_ref(1) + iv %gpsref(1)%ref(1)%inv 
      end if

      !==========================================================
      ! Quality control code is implemeted by Y.-R. Guo, 07/18/2005:
      !==========================================================

      ! Quality check 1: Gross error (departure from the background) check 
      ! ----------------

      if ( check_max_iv ) &
         call da_check_max_iv_gpsref(it, iv%gpsref(n), pref, fref )


      ! Quality check 2: Error percentage check.
      ! ---------------

      if (.not.(pseudo_var(1:3) == 'ref' .and. num_pseudo > 0)) then

         do k=1, num_levs

            ! incremetal refractivity or the relative error:
            ! abs[(O-B)/{(O+B)/2}]               (Lidia Cucurull 2005)

            ntotal = ntotal + 1
            percnt = 2.0 * abs(iv%gpsref(n)%ref(k)%inv / &
               (ob%gpsref(n)%ref(k) + model_ref(k)))

            if (iv%gpsref(n)%ref(k)%qc >= obs_qc_pointer) then
               if ( iv%gpsref(n)%h(k) < h_1 ) then
                  if (percnt > pcnt1) iv%gpsref(n)%ref(k)%qc = qc_below
               else if ( iv%gpsref(n)%h(k) > h_2) then
                  if (percnt > pcnt3) iv%gpsref(n)%ref(k)%qc = qc_above
               else
                  if (percnt > pcnt2) iv%gpsref(n)%ref(k)%qc = qc_middle
               end if
            end if

            ! write(unit=stdout,fmt='(a,i6,a,i6,a,i3,a,i8,a,f10.1,"  pecnt, inv, m_ro:", 3f11.4)') &
            !    "ntotal=",ntotal,"  n=",n, " k=",k, " qc=",iv%gpsref(n)%ref(k)%qc, &
            !    " h=",iv%gpsref(n)%h(k), percnt, &
            !    iv%gpsref(n)%ref(k)%inv, .5*(ob%gpsref(n)%ref(k) + model_ref(k)) 

         end do
      end if
   end do

   ! Quality check 3: Low levels quality control
   ! ----------------

   if (.not.(pseudo_var(1:3) == 'ref' .and. num_pseudo > 0)) then

      ! Search for the GPS RO's name with the 'qc_below':

      nn = 0
      height_below = 0.0
      name_qc      = '                                       '

      do n=iv%ob_numb(iv%current_ob_time-1)%gpsref + 1, &
         iv%ob_numb(iv%current_ob_time)%gpsref 

         nn = nn + 1
         num_levs = iv%gpsref(n)%info%levels
         do k=1, num_levs
           if (iv%gpsref(n)%ref(k)%qc == qc_below) then
               name_qc(nn) = iv%gpsref(n)%info%name
               height_below(nn) = max(iv%gpsref(n)%h(k),height_below(nn))
           end if
         end do
         if (height_below(nn) == 0.0) nn = nn - 1
      end do

      ! Set the flag qc_below to the levels below percnt < pcnt1::

      ntotal = 0
      nqc0   = 0
      nqc1   = 0
      nqc2   = 0
      nqc3   = 0

      do n=iv%ob_numb(iv%current_ob_time-1)%gpsref + 1, &
         iv%ob_numb(iv%current_ob_time)%gpsref 
         num_levs = iv%gpsref(n)%info%levels

         do n1 = 1,nn
            if (iv%gpsref(n)%info%name == name_qc(n1)) then

               do k=1, num_levs
                  if (iv%gpsref(n)%h(k) < height_below(n1) .and. &
                      iv%gpsref(n)%ref(k)%qc >= 0) then
                     iv%gpsref(n)%ref(k)%qc = qc_below
                  end if
               end do
               exit
            end if
         end do

         do k=1, num_levs
            ntotal = ntotal + 1
            if (iv%gpsref(n)%ref(k)%qc == fails_error_max) nqc0 = nqc0 + 1
            if (iv%gpsref(n)%ref(k)%qc == qc_middle) nqc1 = nqc1 + 1
            if (iv%gpsref(n)%ref(k)%qc == qc_below ) nqc2 = nqc2 + 1
            if (iv%gpsref(n)%ref(k)%qc == qc_above ) nqc3 = nqc3 + 1
            ! write(unit=stdout,fmt='(a,5i6,a,i6,a,i3,a,i8,a,f10.1,a, f11.4)') &
            !    "ntotal, nqc0, nqc1, nqc2, nqc3:",ntotal, nqc0, nqc1, nqc2, nqc3, &
            !    "  n=",n, " k=",k, "  qc=",iv%gpsref(n)%ref(k)%qc,  &
            !    "  h=",iv%gpsref(n)%h(k), "  inv=",iv%gpsref(n)%ref(k)%inv
         end do
      end do
   end if

   write(unit = check_max_iv_unit, fmt ='(A,i5,A)') &
        'For outer iteration ',it, &
        ', Total Rejections for GPSRef follows:'
   write(unit = check_max_iv_unit, fmt = '(/,10(2(A,I6),/))') &
        'Number of failed gpsrefes observations:', &
                   fref, ' on ',pref

end subroutine da_get_innov_vector_gpsref

