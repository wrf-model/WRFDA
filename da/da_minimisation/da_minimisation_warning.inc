      subroutine da_minimisation_warning( info )

      implicit none
      
      integer, intent(in)       :: info

      integer                   :: iflag
      
!---------------------------------------------------------------------------
!     diagnostic messages:
!---------------------------------------------------------------------------

      if (lp <= 0)        return

      if (info == 0)      then
           iflag = -1
           write (lp,100) iflag
      else if (info == 2) then
           iflag = -2
           write (lp,105) iflag
      else if (info == 3) then
           iflag = -3
           write (lp,110) iflag
      else if (info == 4) then
           iflag = -4
           write (lp,115) iflag
      else if (info == 5) then
           iflag = -5
           write (lp,120) iflag
      else if (INFO == 6) then
           iflag = -6
           write (LP,125) iflag
      endif

 100  format (/' IFLAG= ',I2,/' IMPROPER INPUT PARAMETERS DURING', &
               ' THE LINE SEARCH.')
 105  format (/' IFLAG= ',I2,/' RELATIVE WIDTH OF THE INTERVAL OF', &
               ' UNCERTAINTY IN THE LINE SEARCH'/ &
               'IS OF THE ORDER OF MACHINE ROUNDOFF.')
 110  format (/' IFLAG= ',I2,/' NUMBER OF CALLS TO FUNCTION IN THE', &
               ' LINE SEARCH HAS REACHED 20.')
 115  format (/' IFLAG= ',I2,/' THE STEP IN THE LINE SEARCH IS', &
               ' TOO SMALL.')
 120  format (/' IFLAG= ',I2,/' THE STEP IN THE LINE SEARCH IS', &
               ' TOO LARGE.')
 125  format (/' IFLAG= ',I2,/' ROUNDING ERRORS PREVENT FURTHER', &
               ' PROGRESS IN THE LINE SEARCH.')

!     if (iflag < 0 ) stop

      end subroutine da_minimisation_warning
