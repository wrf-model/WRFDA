subroutine da_readwrite_be_stage4( outunit, nk, be_method, uh_method, variable )

   implicit none

   integer, intent(in)      :: outunit                    ! Output unit number.
   integer, intent(in)      :: nk                         ! Number of vertical levels/modes.
   character*3, intent(in)  :: be_method                  ! Be method ('NMC', or 'ENS').
   character*5, intent(in)  :: uh_method

   character*10, intent(in) :: variable                   ! Variable name.

   character*80             :: filename                   ! Input filename.
   character*10             :: cvar                       ! Dummy variable name.
   character*2              :: ck                         ! Loop index -> character.
   integer                  :: k                          ! Loop counter.
   integer                  :: kdum                       ! Dummy vertical index.
   integer                  :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   logical                  :: first_time                 ! True if first time through loop.
   logical                  :: use_global_eofs            ! True if projected data uses global EOFs.
   logical                  :: data_on_levels             ! True if field in on model levels.
   real, allocatable        :: total_power(:)             ! Total Power spectrum.
   real, allocatable        :: scale_length(:)

   first_time = .true.

if (uh_method .eq. 'spectrum') then

   do k = 1, nk
      write(ck,'(i2)')k
      if ( k < 10 ) ck = '0'//ck(2:2)

      filename = trim(variable)//'/'//trim(variable)
      filename = trim(filename)//'.'//trim(be_method)//'.'//ck//'.spectrum'

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

      read(iunit)data_on_levels, use_global_eofs

      if ( first_time ) then
         if ( .not. data_on_levels ) then
            if ( use_global_eofs ) then
               write(6,'(a)')' Power spectra calculated for 2D field projected onto global EOFs.'
            else
               write(6,'(a)')' Power spectra calculated for 2D field projected onto local EOFs.'
            end if
         end if
         allocate( total_power(0:max_wavenumber) )
      end if

      read(iunit)total_power(:)
      close(iunit)

      write(outunit)variable
      write(outunit)max_wavenumber, k
      write(outunit)use_global_eofs
      write(outunit)total_power(:)

      first_time = .false.
   end do

   deallocate( total_power )

   elseif (uh_method == 'scale   ') then
      if ( first_time ) allocate( scale_length(1:nk) )

      filename = trim(variable)//'/'//'sl_print.'//trim(variable)
      open (iunit, file = filename)
      read(iunit,'(/)')

      do k=1,nk
      read(iunit,'(24x,e20.8)') scale_length(k)
!      print*,'scale_length ',variable,k,scale_length(k)
      enddo

      write(outunit) variable
      write(outunit) scale_length
! yrg mods:
      print '(5X,"ReadWrite scale length for ",a10," nk=",i3," from ",a40)', &
               variable, nk, filename

   deallocate (scale_length)

   endif


end subroutine da_readwrite_be_stage4
