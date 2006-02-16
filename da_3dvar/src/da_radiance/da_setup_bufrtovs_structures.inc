SUBROUTINE da_setup_bufrtovs_structures( xp, ob, iv )

!------------------------------------------------------------------------------
! PURPOSE: Define, allocate and read of tovs raidance observation structure.
!
! METHOD:  Define, allocate and read of tovs radiance observation structure.
!
!   20/07/2005 -     Creation        Zhiquan Liu 
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

#ifdef DM_PARALLEL
   INCLUDE 'mpif.h'
#endif

   TYPE (xpose_type), INTENT(IN)   :: xp         ! Domain decomposition vars.
   TYPE ( y_type), INTENT(INOUT)   :: ob         ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)   :: iv         ! O-B structure.

   CHARACTER(len=200)           :: filename
   integer                     :: i, j, n, inunit, ios, alloc_status
   logical                     :: lprinttovs 
   
   integer                     :: myproc, ierr
!-------------------------------------------------------------------
!  [1.0] Initialize RTTOV coefs and innovations vector for radiance
!-------------------------------------------------------------------
CALL wrf_oi(__FILE__,__LINE__)
    inunit = bufrtovs_unit
    call da_rtm_init(iv,ob)
CALL wrf_oi(__FILE__,__LINE__)

!-------------------------------------------------------------------
!  [2.0] Read NCEP bufr tovs data in radiance innovations vector
!-------------------------------------------------------------------
   filename = ' '    ! initialise filename
CALL wrf_oi(__FILE__,__LINE__)

   if(num_fgat_time > 1) then
      filename = ' '

      do n=1, num_fgat_time
         iv%current_ob_time = n
         write(filename(1:10), fmt='(a, i2.2)') 'fgat_ob.', n

         if(use_Hirs2Obs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a, i2.2)')  ' Reading radiance 1b data from hirs2.bufr', n
            filename = ' '
            write(filename(1:12), fmt='(a, i2.2)') 'hirs2.bufr', n
            call da_read_bufrtovs ('hirs2', iv, xp, inunit, filename)
         endif

         if(use_MsuObs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a, i2.2)')  ' Reading radiance 1b data from msu.bufr', n
            filename = ' '
            write(filename(1:10), fmt='(a, i2.2)') 'msu.bufr', n
            call da_read_bufrtovs ('msu  ', iv, xp, inunit, filename)
         endif

         if(use_Hirs3Obs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a, i2.2)')  ' Reading radiance 1b data from hirs3.bufr', n
            filename = ' '
            write(filename(1:12), fmt='(a, i2.2)') 'hirs3.bufr', n
            call da_read_bufrtovs ('hirs3', iv, xp, inunit, filename)
         endif

         if(use_AmsuaObs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a, i2.2)')  ' Reading radiance 1b data from amsua.bufr', n
            filename = ' '
            write(filename(1:12), fmt='(a, i2.2)') 'amsua.bufr', n
            call da_read_bufrtovs ('amsua', iv, xp, inunit, filename)
         endif

         if(use_AmsubObs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a, i2.2)')  ' Reading radiance 1b data from amsub.bufr', n
            filename = ' '
            write(filename(1:12), fmt='(a, i2.2)') 'amsub.bufr', n
            call da_read_bufrtovs ('amsub',iv, xp, inunit, filename)
         endif

         if(use_AirsObs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a, i2.2)')  ' Reading airs 1b data from airs.bufr', n
            filename = ' '
            write(filename(1:11), fmt='(a, i2.2)') 'airs.bufr', n
            call da_read_bufrairs ('airs     ',iv, xp, inunit, filename)
         endif
CALL wrf_oi(__FILE__,__LINE__)

         if(use_Eos_AmsuaObs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a, i2.2)')  ' Reading eos_amsua 1b data from airs.bufr', n
            filename = ' '
            write(filename(1:11), fmt='(a, i2.2)') 'airs.bufr', n
            call da_read_bufrairs ('eos_amsua',iv, xp, inunit, filename)
         endif

         if(use_HsbObs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a, i2.2)')  ' Reading hsb 1b data from airs.bufr', n
            filename = ' '
            write(filename(1:11), fmt='(a, i2.2)') 'airs.bufr', n
            call da_read_bufrairs ('hsb      ',iv, xp, inunit, filename)
         endif

      enddo
   else
CALL wrf_oi(__FILE__,__LINE__)

      iv%current_ob_time = 1

         if(use_Hirs2Obs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a)')  ' Reading radiance 1b data from hirs2.bufr'
            filename = ' '
            write(filename(1:10), fmt='(a)') 'hirs2.bufr'
            call da_read_bufrtovs ('hirs2', iv, xp, inunit, filename)
         endif

         if(use_MsuObs) then
CALL wrf_oi(__FILE__,__LINE__)
            filename = ' '
            write(6,'(a)')  ' Reading radiance 1b data from msu.bufr'
            write(filename(1:8), fmt='(a)') 'msu.bufr'
            call da_read_bufrtovs ('msu  ', iv, xp, inunit, filename)
         endif

         if(use_Hirs3Obs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a)')  ' Reading radiance 1b data from hirs3.bufr'
            filename = ' '
            write(filename(1:10), fmt='(a)') 'hirs3.bufr'
            call da_read_bufrtovs ('hirs3', iv, xp, inunit, filename)
         endif

         if(use_AmsuaObs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a)')  ' Reading radiance 1b data from amsua.bufr'
            filename = ' '
            write(filename(1:10), fmt='(a)') 'amsua.bufr'
            call da_read_bufrtovs ('amsua', iv, xp, inunit, filename)
         endif

         if(use_AmsubObs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a)')  ' Reading radiance 1b data from amsub.bufr'
            filename = ' '
            write(filename(1:10), fmt='(a)') 'amsub.bufr'
            call da_read_bufrtovs ('amsub', iv, xp, inunit, filename)
         endif

         if(use_AirsObs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a)')  ' Reading airs 1b data from airs.bufr'
            filename = ' '
            write(filename(1:9), fmt='(a)') 'airs.bufr'
            call da_read_bufrairs ('airs     ',iv, xp, inunit, filename)
         endif

         if(use_Eos_AmsuaObs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a)')  ' Reading eos_amsua 1b data from airs.bufr'
            filename = ' '
            write(filename(1:9), fmt='(a)') 'airs.bufr'
            call da_read_bufrairs ('eos_amsua',iv, xp, inunit, filename)
         endif

         if(use_HsbObs) then
CALL wrf_oi(__FILE__,__LINE__)
            write(6,'(a)')  ' Reading hsb 1b data from airs.bufr'
            filename = ' '
            write(filename(1:9), fmt='(a)') 'airs.bufr'
            call da_read_bufrairs ('hsb      ',iv, xp, inunit, filename)
         endif

         if(use_kma1dvar) then
CALL wrf_oi(__FILE__,__LINE__)
           do i=1,rtminit_nsensor
            filename = ' '
            filename='kma1dvar-'//trim(iv%instid(i)%rttovid_string)//'.inv'
            write(6,'(a,a)')  ' Reading KMA 1dvar innovation from  ', filename
            call da_read_kma1dvar (i,iv, ob, xp, inunit, filename)
           end do
         endif
CALL wrf_oi(__FILE__,__LINE__)

   endif
CALL wrf_oi(__FILE__,__LINE__)

!-----------------------------------------------------------------------------
!  [3.0] create (smaller) ob structure:
!-----------------------------------------------------------------------------

!   CALL da_fill_obs_structures( iv, ob )   ! problem of cross 'use' with da_obs

 if (.not. use_kma1dvar) then
CALL wrf_oi(__FILE__,__LINE__)

   do i = 1,  ob % num_inst
      ob % instid(i) % num_rad = iv % instid(i) % num_rad
      if (ob % instid(i) % num_rad < 1) cycle
      allocate ( ob % instid(i) % rad(1:ob % instid(i)%num_rad), stat = alloc_status )
      If( alloc_status /= 0 ) Then
        CALL wrf_error_fatal3(__FILE__,__LINE__, &
          "mem allocation error to ob%instid(n)%rad")
      End If
      do j = 1, ob % instid(i) % num_rad
        allocate ( ob % instid(i) % rad(j) % tb(1:ob % instid(i) % nchan), stat = alloc_status )
        If( alloc_status /= 0 ) Then
        CALL wrf_error_fatal3(__FILE__,__LINE__, &
          "mem allocation error to ob%instid(n)%ichan")
        End If
        ob % instid(i) % rad(j)% tb(:) = &
        iv % instid(i) % rad(j)% tb(:)% inv
      end do
   end do

 end if
CALL wrf_oi(__FILE__,__LINE__)

   iv%current_ob_time = 1

!  4.0  print iv output
!------------------------------
lprinttovs = .false.
if ( lprinttovs ) then
#ifdef DM_PARALLEL
  call MPI_COMM_RANK( MPI_COMM_WORLD, myproc, ierr )
#endif
 do n = 1, iv%num_inst
  if (iv%instid(n)%num_rad < 1) cycle
#ifdef DM_PARALLEL
  write(filename,'(a,i2.2)') trim(iv%instid(n)%rttovid_string)//'.obs_',myproc
#else
  write(filename,'(a)') trim(iv%instid(n)%rttovid_string)//'.obs'
#endif
  open(14,file=trim(filename),form='formatted',iostat=ios)
  if( ios /= 0 ) Then
    CALL wrf_error_fatal3(__FILE__,__LINE__, &
      "Cannot open output file "//TRIM(filename))
  Endif
    write(14,'(a,a,i7,a,i5,a,20i3)') trim(iv%instid(n)%rttovid_string), &
                        ' number-of-pixels: ', iv%instid(n)%num_rad, &
                        ' channel-number: ', iv%instid(n)%nchan, &
                        ' index-of-channels : ', iv%instid(n)%ichan
    write(14, *) ' pixel-info: i date scanpos landsea_mask  elv lat lon  satzen satazi '                   
CALL wrf_oi(__FILE__,__LINE__)
  do i =1,iv%instid(n)%num_rad
    write(14,'(a,i7,2x,a,2i3,f6.0,4f8.2)') 'info: ', i, &
                             iv%instid(n)%rad(i)%info%date_char, &
                             iv%instid(n)%rad(i)%scanpos,   &
                             iv%instid(n)%rad(i)%landsea_mask, &
                             iv%instid(n)%rad(i)%info%elv,  &
                             iv%instid(n)%rad(i)%info%lat,  &
                             iv%instid(n)%rad(i)%info%lon, & 
                             iv%instid(n)%rad(i)%satzen,    &
                             iv%instid(n)%rad(i)%satazi
    write(14,'(a,19f11.2)') ' BT: ', iv%instid(n)%rad(i)%tb(:)%inv
  end do
  close(14)
 end do
end if
CALL wrf_oi(__FILE__,__LINE__)

 return

END SUBROUTINE da_setup_bufrtovs_structures

