 subroutine da_write_filtered_rad( ob, iv )
!------------------------------------------------------------------------------
!  PURPOSE: write out QCed radiance data.
!
!  METHOD: write out for separated PEs 
!
!  HISTORY: 2006/02/13 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
#ifdef DM_PARALLEL
   INCLUDE 'mpif.h'
#endif

   TYPE (y_type),     INTENT(IN)  :: ob       ! Observation structure.
   TYPE (ob_type),    INTENT(IN)  :: iv       ! O-B structure.

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.

   INTEGER            :: ios
   character(len=50)  :: filename

 do n = 1, iv%num_inst
  if (iv%instid(n)%num_rad < 1) cycle

#ifdef DM_PARALLEL
  write(UNIT=stdout,FMT='(a,i3,a,i7)') 'number of obs in proc # ', myproc,' : ', iv%instid(n)%num_rad
#else
  write(UNIT=stdout,FMT='(a,i7)') 'number of obs ', iv%instid(n)%num_rad
#endif

#ifdef DM_PARALLEL
  write(UNIT=filename, FMT='(a,i2.2)') 'filtered_'//trim(iv%instid(n)%rttovid_string)//'.bin_', myproc
#else
  write(UNIT=filename, FMT='(a)') 'filtered_'//trim(iv%instid(n)%rttovid_string)//'.bin'
#endif

  open(UNIT=filtered_rad_unit,file=trim(filename),form='unformatted',iostat=ios)
  if( ios /= 0 ) Then
     call da_error(__FILE__,__LINE__, &
       (/"Canot open filtered radiance file"//filename/))
  Endif

   write(UNIT=filtered_rad_unit) iv%instid(n)%num_rad

  do i =1,iv%instid(n)%num_rad
    write(UNIT=filtered_rad_unit) i, &
                iv%instid(n)%rad(i)%info%date_char, &
                iv%instid(n)%rad(i)%scanpos       , &
                iv%instid(n)%rad(i)%ifgat         , &
                iv%instid(n)%rad(i)%landsea_mask  , &
                iv%instid(n)%rad(i)%info%elv      , &
                iv%instid(n)%rad(i)%info%lat      , &
                iv%instid(n)%rad(i)%info%lon      , &
                iv%instid(n)%rad(i)%satzen        , &
                iv%instid(n)%rad(i)%satazi        , &
                ob%instid(n)%rad(i)%tb(:)         , &
                iv%instid(n)%rad(i)%tb(:)%error   , &
                iv%instid(n)%rad(i)%tb(:)%qc

  end do     ! end do pixels
  close(UNIT=filtered_rad_unit)
 end do    !! end do instruments

 return
 
end subroutine da_write_filtered_rad
