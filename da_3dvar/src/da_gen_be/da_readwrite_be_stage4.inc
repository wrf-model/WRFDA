subroutine da_readwrite_be_stage4( outunit, nk, be_method, data_on_levels, variable )

   implicit none

   integer, intent(in)      :: outunit                    ! Output unit number.
   integer, intent(in)      :: nk                         ! Number of vertical levels/modes.
   character*3, intent(in)  :: be_method                  ! Be method ('NMC', or 'ENS').
   logical, intent(in)      :: data_on_levels             ! False if data is projected onto EOFs.
   character*10, intent(in) :: variable                   ! Variable name.

   character*80             :: filename                   ! Input filename.
   character*10             :: cvar                       ! Dummy variable name.
   character*2              :: ck                         ! Loop index -> character.
   character*1              :: k_label                    ! = 'k' if model level, 'm' if mode output.
   integer                  :: k                          ! Loop counter.
   integer                  :: kdum                       ! Dummy vertical index.
   integer                  :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   logical                  :: first_time                 ! True if first time through loop.

   real, allocatable        :: total_power(:)             ! Total Power spectrum.

   first_time = .true.

   if ( data_on_levels ) then
      k_label = 'k' !    Data on vertical levels.
   else
     k_label = 'm'  !    Data on vertical modes.
   end if

   do k = 1, nk
      write(ck,'(i2)')k
      if ( k < 10 ) ck = '0'//ck(2:2)

      filename = trim(variable)//'/'//trim(variable)
      filename = trim(filename)//'.'//trim(be_method)//'.'//k_label//ck//'.spectrum'

      open (iunit, file = filename, form='unformatted')
      read(iunit)cvar
      if ( trim(cvar) /=  trim(variable) ) then
         write(6,'(a)')' Variable name inconsistency in da_readwrite_be_stage4. Stop'
         stop
      end if

      read(iunit)max_wavenumber, kdum
      if ( kdum /= k ) then
         write(6,'(a)')' Inconsistent vertical label in da_readwrite_stage4. Stop'
         stop
      end if

      if ( first_time ) allocate( total_power(0:max_wavenumber) )
      read(iunit)total_power(:)
      close(iunit)

      write(outunit)variable
      write(outunit)max_wavenumber, k
      write(outunit)total_power(:)

      first_time = .false.
   end do

   deallocate( total_power )

end subroutine da_readwrite_be_stage4
