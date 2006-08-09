subroutine da_setup_bufrtovs_structures( xp, ob, iv )

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

   CHARACTER(len=200)          :: filename
   integer                     :: i, j, n, inunit, ios, alloc_status


   if (trace_use) call da_trace_entry("da_setup_bufrtovs_structures")

!-------------------------------------------------------------------
!  [1.0] Initialize RTTOV coefs and innovations vector for radiance
!-------------------------------------------------------------------
    inunit = bufrtovs_unit
    call da_rtm_init(iv,ob)

!    return

!-------------------------------------------------------------------
!  [2.0] Read NCEP bufr tovs data in radiance innovations vector
!-------------------------------------------------------------------
         if(use_Hirs2Obs) then
            write(UNIT=stdout,FMT='(a)')  ' Reading radiance 1b data from hirs2.bufr'
            filename = ' '
            write(UNIT=filename(1:10), fmt='(a)') 'hirs2.bufr'
            call da_read_bufrtovs ('hirs2', iv, xp, inunit, filename)
         endif

         if(use_MsuObs) then
            write(UNIT=stdout,FMT='(a)')  ' Reading radiance 1b data from msu.bufr'
            filename = ' '
            write(UNIT=filename(1:8), fmt='(a)') 'msu.bufr'
            call da_read_bufrtovs ('msu  ', iv, xp, inunit, filename)
         endif

         if(use_Hirs3Obs) then
            write(UNIT=stdout,FMT='(a)')  ' Reading radiance 1b data from hirs3.bufr'
            filename = ' '
            write(UNIT=filename(1:10), fmt='(a)') 'hirs3.bufr'
            call da_read_bufrtovs ('hirs3', iv, xp, inunit, filename)
         endif

         if(use_AmsuaObs) then
            write(UNIT=stdout,FMT='(a)')  ' Reading radiance 1b data from amsua.bufr'
            filename = ' '
            write(UNIT=filename(1:10), fmt='(a)') 'amsua.bufr'
            call da_read_bufrtovs ('amsua', iv, xp, inunit, filename)
         endif

         if(use_AmsubObs) then
            write(UNIT=stdout,FMT='(a)')  ' Reading radiance 1b data from amsub.bufr'
            filename = ' '
            write(UNIT=filename(1:10), fmt='(a)') 'amsub.bufr'
            call da_read_bufrtovs ('amsub', iv, xp, inunit, filename)
         endif

         if(use_AirsObs) then
            write(UNIT=stdout,FMT='(a)')  ' Reading airs 1b data from airs.bufr'
            filename = ' '
            write(UNIT=filename(1:9), fmt='(a)') 'airs.bufr'
            call da_read_bufrairs ('airs     ',iv, xp, inunit, filename)
         endif

         if(use_Eos_AmsuaObs) then
            write(UNIT=stdout,FMT='(a)')  ' Reading eos_amsua 1b data from airs.bufr'
            filename = ' '
            write(UNIT=filename(1:9), fmt='(a)') 'airs.bufr'
            call da_read_bufrairs ('eos_amsua',iv, xp, inunit, filename)
         endif

         if(use_HsbObs) then
            write(UNIT=stdout,FMT='(a)')  ' Reading hsb 1b data from airs.bufr'
            filename = ' '
            write(UNIT=filename(1:9), fmt='(a)') 'airs.bufr'
            call da_read_bufrairs ('hsb      ',iv, xp, inunit, filename)
         endif

         if(use_filtered_rad) then
            call da_read_filtered_rad (xp, iv)
         endif

         if(use_kma1dvar) then
           do i=1,rtminit_nsensor
            filename = ' '
            filename='kma1dvar-'//trim(iv%instid(i)%rttovid_string)//'.inv'
            write(UNIT=stdout,FMT='(a,a)')  ' Reading KMA 1dvar innovation from  ', filename
            call da_read_kma1dvar (i,iv, ob, xp, inunit, filename)
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
        call da_error(__FILE__,__LINE__, &
          (/"mem allocation error to ob%instid(n)%rad"/))
      End If
      do j = 1, ob % instid(i) % num_rad
        allocate ( ob % instid(i) % rad(j) % tb(1:ob % instid(i) % nchan), stat = alloc_status )
        If( alloc_status /= 0 ) Then
          call da_error(__FILE__,__LINE__, &
            (/"mem allocation error to ob%instid(n)%ichan"/))
        End If
        ob % instid(i) % rad(j)% tb(:) = &
        iv % instid(i) % rad(j)% tb(:)% inv
      end do
   end do

 end if


   if (trace_use) call da_trace_exit("da_setup_bufrtovs_structures")

 return

end subroutine da_setup_bufrtovs_structures

