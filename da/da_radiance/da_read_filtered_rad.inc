subroutine da_read_filtered_rad( xp, iv )

   !---------------------------------------------------------------------------
   ! PURPOSE: read in QCed radiance data from separated PEs 
   !---------------------------------------------------------------------------

   implicit none

   TYPE (xpose_type), intent(in)     :: xp       ! Domain decomposition vars.
   TYPE (ob_type),    intent(inout)  :: iv       ! O-B structure.

   integer                        :: n        ! Loop counter.
   integer                        :: i, j, k  ! Index dimension.

   integer            :: ios, filtered_rad_unit
   character(len=50)  :: filename
   integer            :: ierr, ndomain
   logical            :: outside

   if (trace_use) call da_trace_entry("da_read_filtered_rad")

   call init_constants_derived

   do n = 1, iv%num_inst

#ifdef DM_PARALLEL
      write(filename, '(a,i2.2)') 'filtered_'//trim(iv%instid(n)%rttovid_string)//'.bin_', myproc
#else
      write(filename, '(a)') 'filtered_'//trim(iv%instid(n)%rttovid_string)//'.bin'
#endif

      if (print_detail_radiance) then
         write(unit=stdout,fmt=*) 'Reading in ', trim(filename) 
      end if

      call da_get_unit(filtered_rad_unit)
      open(unit=filtered_rad_unit,file=trim(filename),form='unformatted',status='old',iostat=ios)
      if (ios /= 0) then
         write(unit=stdout,fmt=*) 'Can not open filtered radiance file ', filename
         iv%instid(n)%num_rad = 0
         cycle
      endif

      read(UNIT=filtered_rad_unit) iv%instid(n)%num_rad
      if (iv%instid(n)%num_rad < 1) cycle
      if (print_detail_radiance) then
         write(unit=stdout,fmt='(a,i3,2x,a,3x,i10)')  ' allocating space for radiance innov structure', &
                               n, iv%instid(n)%rttovid_string, iv%instid(n)%num_rad
      end if
      allocate( iv%instid(n)%rad(1:iv%instid(n)%num_rad), stat = ierr )
      if (ierr /= 0) then
        call da_error(__FILE__,__LINE__, &
           (/"Canot allocate memory to iv%instid(i)%rad"/))
      end if

      do i =1,iv%instid(n)%num_rad
         allocate ( iv%instid(n)%rad(i)%tb(1:iv%instid(n)%nchan) )
         read(unit=filtered_rad_unit) ndomain, &
                  iv%instid(n)%rad(i)%info%date_char, &
                  iv%instid(n)%rad(i)%scanpos       , &
                  iv%instid(n)%rad(i)%ifgat         , &
                  iv%instid(n)%rad(i)%landsea_mask  , &
                  iv%instid(n)%rad(i)%info%elv      , &
                  iv%instid(n)%rad(i)%info%lat      , &
                  iv%instid(n)%rad(i)%info%lon      , &
                  iv%instid(n)%rad(i)%satzen        , &
                  iv%instid(n)%rad(i)%satazi        , &
                  iv%instid(n)%rad(i)%tb(:)%inv     , &
                  iv%instid(n)%rad(i)%tb(:)%error   , &
                  iv%instid(n)%rad(i)%tb(:)%qc

         call da_ll_to_xy (iv%instid(n)%rad(i)%info, &
                            iv%instid(n)%rad(i)%loc,  &
                            xp, outside)
         ! if (outside) cycle

      end do     ! end do pixels
      close(filtered_rad_unit)
      call da_free_unit(filtered_rad_unit)

      iv%total_obs = iv%total_obs + iv%instid(n)%num_rad
      iv%total_rad_pixel   = iv%total_rad_pixel + iv%instid(n)%num_rad
      iv%total_rad_channel = iv%total_rad_channel + iv%instid(n)%num_rad*iv%instid(n)%nchan
   end do      ! end do instruments

   if (trace_use) call da_trace_exit("da_read_filtered_rad")
 
end subroutine da_read_filtered_rad


