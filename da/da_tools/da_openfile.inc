      subroutine da_openfile (Kunit, CDFILE, CDACCESS, CDSTATUS, CDFORM)
!--------------------------------------------------------------------------!
!
!                       RoutinE openFILE
!                     ********************
!
!  PURPOSE:
!  -------
!     open AN FILE AND REWinD
!
!   METHOD:
!   ------
!
!   inPUT:
!   -----
!      Kunit:          logical unit
!      CDFILE:         NAME OF FILE FOR outPUT OF DISCARED OBS.
!      CDA!ESS:       A!ESS (SEQUENTIAL OR ),
!      CDSTATUS:       STATUS (OLD, NEW OR UNKNOWN)
!      CDFORM:         FORM (FORMATTED OR UNFORMATTED)
!
!   outPUT:
!   ------
!      openED FILE
!
!   COMMON:                      NO
!   -------
!   EXTERNAL:                    NO
!   --------
!   REFERENCES:                  NO
!   ----------
!
!   MODifICATIONS:
!   --------------
!       ORIGinAL :  98-07 (F. VANDENBERGHE)
!       ADDITIONS : 98-11 Norm doCTOR (F. VANDENBERGHE)
!----------------------------------------------------------------------------!

!...ARGUMENTS FILE NAME AND logical unit

      integer        Kunit, iost
      character*(*)  CDFILE, CDACCESS, CDSTATUS, CDFORM

!------------------------------------------------------------------------------C

! 1.  open FILE
! -------------

      IOST = 0

      open ( unit   = Kunit, &
             FILE   = CDFILE, &
             ACCESS = CDACCESS, &
             STATUS = CDSTATUS, &
             FORM   = CDFORM, &
             ERR    =  2000, &
             iostat = IOST        )
 
      REWinD (Kunit)
 
      return
 
! 2.  ERROR PROCESSinG
! --------------------
 
 2000 continue
      call da_error(__FILE__,__LINE__, &
        (/"Cannot open file"//trim(CDFILE)/))

      end subroutine da_openfile

