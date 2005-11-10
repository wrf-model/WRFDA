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

   INCLUDE 'mpif.h'

   TYPE (xpose_type), INTENT(IN)   :: xp         ! Domain decomposition vars.
   TYPE ( y_type), INTENT(INOUT)   :: ob         ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)   :: iv         ! O-B structure.

   CHARACTER(len=20)           :: filename
   integer                     :: i, j, n, inunit, ios, alloc_status
   logical                     :: lprinttovs 
   
   integer                     :: myproc, ierr
!-------------------------------------------------------------------
!  [1.0] Initialize RTTOV coefs and innovations vector for radiance
!-------------------------------------------------------------------
    inunit = 55
    call da_rtm_init(iv)

!-------------------------------------------------------------------
!  [2.0] Read NCEP bufr tovs data in radiance innovations vector
!-------------------------------------------------------------------
   filename = ' '    ! initialise filename

   if(num_fgat_time > 1) then
      filename = ' '

      do n=1, num_fgat_time
         iv%current_ob_time = n
         write(filename(1:10), fmt='(a, i2.2)') 'fgat_ob.', n

         if(use_hirs2) then
            write(6,'(a, i2.2)')  ' Reading radiance 1b data from hirs2.bufr', n
            write(filename(1:12), fmt='(a, i2.2)') 'hirs2.bufr', n
            call da_read_bufrtovs ('hirs2', iv, xp, inunit, filename)
         endif

         if(use_msu) then
            write(6,'(a, i2.2)')  ' Reading radiance 1b data from msu.bufr', n
            write(filename(1:10), fmt='(a, i2.2)') 'msu.bufr', n
            call da_read_bufrtovs ('msu  ', iv, xp, inunit, filename)
         endif

         if(use_hirs3) then
            write(6,'(a, i2.2)')  ' Reading radiance 1b data from hirs3.bufr', n
            write(filename(1:12), fmt='(a, i2.2)') 'hirs3.bufr', n
            call da_read_bufrtovs ('hirs3', iv, xp, inunit, filename)
         endif

         if(use_amsua) then
            write(6,'(a, i2.2)')  ' Reading radiance 1b data from amsua.bufr', n
            write(filename(1:12), fmt='(a, i2.2)') 'amsua.bufr', n
            call da_read_bufrtovs ('amsua', iv, xp, inunit, filename)
         endif

         if(use_amsub) then
            write(6,'(a, i2.2)')  ' Reading radiance 1b data from amsub.bufr', n
            write(filename(1:12), fmt='(a, i2.2)') 'amsub.bufr', n
            call da_read_bufrtovs ('amsub', iv, xp, inunit, filename)
         endif

      enddo
   else

      iv%current_ob_time = 1

         if(use_hirs2) then
            write(6,'(a)')  ' Reading radiance 1b data from hirs2.bufr'
            write(filename(1:10), fmt='(a)') 'hirs2.bufr'
            call da_read_bufrtovs ('hirs2', iv, xp, inunit, filename)
         endif

         if(use_msu) then
            filename = ' '
            write(6,'(a)')  ' Reading radiance 1b data from msu.bufr'
            write(filename(1:8), fmt='(a)') 'msu.bufr'
            call da_read_bufrtovs ('msu  ', iv, xp, inunit, filename)
         endif

         if(use_hirs3) then
            write(6,'(a)')  ' Reading radiance 1b data from hirs3.bufr'
            write(filename(1:10), fmt='(a)') 'hirs3.bufr'
            call da_read_bufrtovs ('hirs3', iv, xp, inunit, filename)
         endif

         if(use_amsua) then
            write(6,'(a)')  ' Reading radiance 1b data from amsua.bufr'
            write(filename(1:10), fmt='(a)') 'amsua.bufr'
            call da_read_bufrtovs ('amsua', iv, xp, inunit, filename)
         endif

         if(use_amsub) then
            write(6,'(a)')  ' Reading radiance 1b data from amsub.bufr'
            write(filename(1:10), fmt='(a)') 'amsub.bufr'
            call da_read_bufrtovs ('amsub', iv, xp, inunit, filename)
         endif

   endif

!-----------------------------------------------------------------------------
!  [3.0] create (smaller) ob structure:
!-----------------------------------------------------------------------------

!   CALL da_fill_obs_structures( iv, ob )   ! problem of cross 'use' with da_obs

   ob % num_inst = iv % num_inst
  IF ( ob % num_inst > 0 ) THEN
      allocate ( ob % instid(1:ob % num_inst), stat = alloc_status )
      If( alloc_status /= 0 ) Then
        Write( *, '( "mem allocation error to ob%instid")' )
        Stop ' in da_setup_bufrtovs_structures '
      End If
   do i = 1,  ob % num_inst
      ob % instid(i) % num_rad = iv % instid(i) % num_rad
      if (ob % instid(i) % num_rad < 1) cycle
      allocate ( ob % instid(i) % rad(1:ob % instid(i)%num_rad), stat = alloc_status )
      If( alloc_status /= 0 ) Then
        Write( *, '( "mem allocation error to ob%instid(n)%rad")' )
        Stop ' in da_setup_bufrtovs_structures '
      End If
      ob % instid(i) % nchan = iv % instid(i) % nchan 
      allocate ( ob % instid(i) % ichan(1:ob % instid(i)%nchan), stat = alloc_status )
      If( alloc_status /= 0 ) Then
        Write( *, '( "mem allocation error to ob%instid(n)%ichan")' )
        Stop ' in da_setup_bufrtovs_structures '
      End If
      ob % instid(i) % ichan(:) = iv % instid(i) % ichan(:) 
      do j = 1, ob % instid(i) % num_rad
        allocate ( ob % instid(i) % rad(j) % tb(1:ob % instid(i) % nchan), stat = alloc_status )
        If( alloc_status /= 0 ) Then
          Write( *, '( "mem allocation error to ob%instid(n)%ichan")' ) 
          Stop ' in da_setup_bufrtovs_structures '
        End If
        ob % instid(i) % rad(j)% tb(:) = &
        iv % instid(i) % rad(j)% tb(:)% inv
      end do
   end do
  END IF

   iv%current_ob_time = 1

!  4.0  print iv output
!------------------------------
lprinttovs = .false.
if ( lprinttovs ) then
 call MPI_COMM_RANK( MPI_COMM_WORLD, myproc, ierr )
 do n = 1, iv%num_inst
  if (iv%instid(n)%num_rad < 1) cycle
  write(filename,'(a,i2.2)') trim(iv%instid(n)%rttovid_string)//'.obs_',myproc
  open(14,file=trim(filename),form='formatted',iostat=ios)
  if( ios /= 0 ) Then
      Write(*,*) 'error opening the output file ', filename
      Stop ' in da_setup_bufrtovs_structures '
  Endif
    write(14,'(a,a,i7,a,i5,a,20i3)') trim(iv%instid(n)%rttovid_string), &
                        ' number-of-pixels: ', iv%instid(n)%num_rad, &
                        ' channel-number: ', iv%instid(n)%nchan, &
                        ' index-of-channels : ', iv%instid(n)%ichan
    write(14, *) ' pixel-info: i date scanpos landsea_mask  elv lat lon  satzen satazi '                   
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

 return

END SUBROUTINE da_setup_bufrtovs_structures

