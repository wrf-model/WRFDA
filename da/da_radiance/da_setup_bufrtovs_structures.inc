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
   integer                     :: i, j, n, ios, alloc_status
   logical                     :: lprinttovs 
   
   integer                     :: myproc, ierr

   if (trace_use) call da_trace_entry("da_setup_bufrtovs_structures")

!-------------------------------------------------------------------
!  [1.0] Initialize RTTOV coefs and innovations vector for radiance
!-------------------------------------------------------------------
#ifdef RTTOV
    call da_rttov_init(iv,ob)
#endif

!-------------------------------------------------------------------
!  [2.0] Read NCEP bufr tovs data in radiance innovations vector
!-------------------------------------------------------------------
         if(use_hirs2obs) then
            write(6,'(a)')  ' Reading radiance 1b data from hirs2.bufr'
            filename = ' '
            write(filename(1:10), fmt='(a)') 'hirs2.bufr'
            call da_read_bufrtovs ('hirs2', iv, xp, bufrtovs_unit, filename)
         endif

         if(use_msuobs) then
            filename = ' '
            write(6,'(a)')  ' Reading radiance 1b data from msu.bufr'
            write(filename(1:8), fmt='(a)') 'msu.bufr'
            call da_read_bufrtovs ('msu  ', iv, xp, bufrtovs_unit, filename)
         endif

         if(use_hirs3obs) then
            write(6,'(a)')  ' Reading radiance 1b data from hirs3.bufr'
            filename = ' '
            write(filename(1:10), fmt='(a)') 'hirs3.bufr'
            call da_read_bufrtovs ('hirs3', iv, xp, bufrtovs_unit, filename)
         endif

         if(use_amsuaobs) then
            write(6,'(a)')  ' Reading radiance 1b data from amsua.bufr'
            filename = ' '
            write(filename(1:10), fmt='(a)') 'amsua.bufr'
            call da_read_bufrtovs ('amsua', iv, xp, bufrtovs_unit, filename)
         endif

         if(use_amsubobs) then
            write(6,'(a)')  ' Reading radiance 1b data from amsub.bufr'
            filename = ' '
            write(filename(1:10), fmt='(a)') 'amsub.bufr'
            call da_read_bufrtovs ('amsub', iv, xp, bufrtovs_unit, filename)
         endif

         if(use_airsobs) then
            write(6,'(a)')  ' Reading airs 1b data from airs.bufr'
            filename = ' '
            write(filename(1:9), fmt='(a)') 'airs.bufr'
            call da_read_bufrairs ('airs     ',iv, xp, bufrtovs_unit, filename)
         endif

         if(use_eos_amsuaobs) then
            write(6,'(a)')  ' Reading eos_amsua 1b data from airs.bufr'
            filename = ' '
            write(filename(1:9), fmt='(a)') 'airs.bufr'
            call da_read_bufrairs ('eos_amsua',iv, xp, bufrtovs_unit, filename)
         endif

         if(use_hsbobs) then
            write(6,'(a)')  ' Reading hsb 1b data from airs.bufr'
            filename = ' '
            write(filename(1:9), fmt='(a)') 'airs.bufr'
            call da_read_bufrairs ('hsb      ',iv, xp, bufrtovs_unit, filename)
         endif

         if(use_filtered_rad) then
            call da_read_filtered_rad (xp, iv)
         endif

         if(use_kma1dvar) then
           do i=1,rtminit_nsensor
            filename = ' '
            filename='kma1dvar-'//trim(iv%instid(i)%rttovid_string)//'.inv'
            write(6,'(a,a)')  ' Reading KMA 1dvar innovation from  ', filename
            call da_read_kma1dvar (i,iv, ob, xp, bufrtovs_unit, filename)
           end do
         endif

!   sorting obs into FGAT time bins
         call da_sort_rad(iv)

!-----------------------------------------------------------------------------
!  [3.0] create (smaller) ob structure:
!-----------------------------------------------------------------------------

 if (.not. use_kma1dvar) then

   do i = 1,  ob % num_inst
      ob % instid(i) % num_rad = iv % instid(i) % num_rad
      if (ob % instid(i) % num_rad < 1) cycle
      allocate ( ob % instid(i) % rad(1:ob % instid(i)%num_rad), stat = alloc_status )
      If( alloc_status /= 0 ) Then
        Write( *, '( "mem allocation error to ob%instid(n)%rad")' )
        Stop ' in da_setup_bufrtovs_structures '
      End If
      do j = 1, ob % instid(i) % num_rad
        allocate ( ob % instid(i) % rad(j) % tb(1:ob % instid(i) % nchan), stat = alloc_status )
        If( alloc_status /= 0 ) Then
          Write( *, '( "mem allocation error to ob%instid(n)%rad(j)%tb")' ) 
          Stop ' in da_setup_bufrtovs_structures '
        End If
        ob % instid(i) % rad(j)% tb(:) = &
        iv % instid(i) % rad(j)% tb(:)% inv
      end do
   end do

 end if

   if (trace_use) call da_trace_exit("da_setup_bufrtovs_structures")

END SUBROUTINE da_setup_bufrtovs_structures

