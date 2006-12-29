subroutine da_write_filtered_rad(ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: write out QCed radiance data.
   !
   ! Method: write out for separate PEs 
   !---------------------------------------------------------------------------

   implicit none

   type (y_type),     intent(in)  :: ob       ! Observation structure.
   type (ob_type),    intent(in)  :: iv       ! O-B structure.

   integer                        :: n        ! Loop counter.
   integer                        :: i, j, k  ! Index dimension.

   integer            :: ios, filtered_rad_unit
   character(len=50)  :: filename

   if (trace_use) call da_trace_entry("da_write_filtered_rad")

   do i = 1, iv%num_inst
      if (iv%instid(i)%num_rad < 1) cycle

#ifdef DM_PARALLEL
      write(unit=stdout,fmt='(a,i3,a,i7)') 'number of obs in proc # ', myproc,' : ', iv%instid(i)%num_rad
#else
      write(unit=stdout,fmt='(a,i7)') 'number of obs ', iv%instid(i)%num_rad
#endif

#ifdef DM_PARALLEL
      write(unit=filename, fmt='(a,i2.2)') &
         'filtered_'//trim(iv%instid(i)%rttovid_string)//'.bin_', myproc
#else
      write(unit=filename, fmt='(a)') &
         'filtered_'//trim(iv%instid(i)%rttovid_string)//'.bin'
#endif
      
      call da_get_unit(filtered_rad_unit)
      open(unit=filtered_rad_unit,file=trim(filename), &
         form='unformatted',iostat=ios)
      if (ios /= 0) Then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open filtered radiance file"//filename/))
      Endif

      write(unit=filtered_rad_unit) iv%instid(i)%num_rad

      do n =1,iv%instid(i)%num_rad
         write(unit=filtered_rad_unit) n, &
                   iv%instid(i)%info(n)%date_char, &
                   iv%instid(i)%scanpos(n)       , &
                   iv%instid(i)%ifgat(n)         , &
                   iv%instid(i)%landsea_mask(n)  , &
                   iv%instid(i)%info(n)%elv      , &
                   iv%instid(i)%info(n)%lat      , &
                   iv%instid(i)%info(n)%lon      , &
                   iv%instid(i)%satzen(n)        , &
                   iv%instid(i)%satazi(n)        , &
                   ob%instid(i)%tb(:,n)         , &
                   iv%instid(i)%tb_error(:,n)   , &
                   iv%instid(i)%tb_qc(:,n)

      end do     ! end do pixels
      close(unit=filtered_rad_unit)
      call da_free_unit(filtered_rad_unit)
   end do    !! end do instruments

   if (trace_use) call da_trace_exit("da_write_filtered_rad")
 
end subroutine da_write_filtered_rad


