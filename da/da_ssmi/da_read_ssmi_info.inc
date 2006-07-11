subroutine da_read_ssmi_info (iunit, ob, xb, xbx)

   !---------------------------------------------------------------------------
   ! Read the header of a MM5 3D-VAR 3.0 GTS observation file
   !---------------------------------------------------------------------------

   implicit none

   INTEGER,        INTENT (in)  :: iunit
   TYPE (xb_type), INTENT (in)  :: xb
   TYPE (xbx_type),INTENT (in)  :: xbx     ! Header & non-gridded vars.
   TYPE (ob_type), INTENT (out) :: ob


   INTEGER                      :: iost, i, ii
   CHARACTER (LEN = 120)        :: char_ned
   LOGICAL                      :: connected

   INTEGER                      :: nssmis,nothers
   INTEGER                      :: ixc, jxc, iproj, idd, maxnes
   INTEGER,  dimension(10)      :: nestix, nestjx, numnc, nesti, nestj
   REAL                         :: phic   , xlonc  , &
                                   truelat1_3dv, truelat2_3dv, &
                                   ts0    , ps0    , tlp     , ptop
   REAL   ,  dimension(10)      :: dis

   LOGICAL                      :: CHECK_WRONG, CHECK_SUBDOMAIN

   iost = -99999

   ! 1. OPEN FILE
   !    ---------

   IF( use_ssmiretrievalobs .or. use_ssmitbobs .or. &
       use_ssmt1obs .or. use_ssmt2obs ) then
      OPEN ( UNIT   = iunit,     &
           FORM   = 'FORMATTED',  &
           ACCESS = 'SEQUENTIAL', &
           IOSTAT =  iost,     &
           STATUS = 'OLD')

      IF (iost /= 0) THEN
         Use_SsmiRetrievalObs = .false.
         Use_SsmiTbObs  = .false.
         use_ssmt1obs   = .false.
         use_ssmt2obs   = .false.

         RETURN
      ENDIF
   ELSE
      RETURN
   END IF

   REWIND (UNIT = iunit)


   ! 2.  READ HEADER
   ! ===============

   ! 2.1 READ FIRST LINE
   !     ---------------

   READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) char_ned

   ! 2.2 PROCESS ERROR
   !     -------------

   IF (iost /= 0) THEN
      Use_SsmiRetrievalObs = .false.
      Use_SsmiTbObs  = .false.
      use_ssmt1obs   = .false.
      use_ssmt2obs   = .false.

      write(unit=stderr, fmt='(/a,i3/)') &
           'ERROR READING FILE UNIT:', iunit, &
           'ERROR CODE:', iost

      return
   else
      write(UNIT=stdout, fmt='(/2a/)') &
           'in da_read_ssmi_info.inc, char_ned=', trim(char_ned)
   ENDIF

   ! 2.3 READ NUMBER OF REPORTS
   !     ---------------------

   DO
 
      DO i = 0, 120-14

         SELECT CASE ( char_ned (I+1:I+5) )
         ! Number of observations
            CASE ('SSMI ') ; 
               if (Use_SsmiRetrievalObs) &
                  READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) &
                     ob%num_ssmi_retrieval
               if (Use_SsmiTbObs) then
                  READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) ob%num_ssmi_tb
               end if
            CASE ('OTHER') ; 
               READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nothers

               ! Geographic area and reference atmosphere definition

            CASE ('MISS.') ; 
                READ (char_ned (I+8:I+15),'(F8.0)', IOSTAT = iost) ob % missing
            CASE ('PHIC ') ; 
                READ (char_ned (I+8:I+14),'(F7.2)', IOSTAT = iost) phic
            CASE ('XLONC') ; 
                READ (char_ned (I+8:I+14),'(F7.2)', IOSTAT = iost) xlonc
            CASE ('TRUE1') ; 
                READ (char_ned (I+8:I+14),'(F7.2)', IOSTAT = iost) truelat1_3dv
            CASE ('TRUE2') ; 
                READ (char_ned (I+8:I+14),'(F7.2)', IOSTAT = iost) truelat2_3dv
            CASE ('TS0  ') ; 
                READ (char_ned (I+8:I+14),'(F7.2)', IOSTAT = iost) ts0
            CASE ('TLP  ') ; 
                READ (char_ned (I+8:I+14),'(F7.2)', IOSTAT = iost) tlp
            CASE ('PTOP ') ; 
                READ (char_ned (I+8:I+14),'(F7.0)', IOSTAT = iost) ptop
            CASE ('PS0  ') ; 
                READ (char_ned (I+8:I+14),'(F7.0)', IOSTAT = iost) ps0
            CASE ('IXC  ') ; 
                READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) ixc
            CASE ('JXC  ') ; 
                READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) jxc
            CASE ('IPROJ') ; 
                READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) iproj
            CASE ('IDD  ') ; 
                READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) idd
            CASE ('MAXNE') ; 
                READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) maxnes
            CASE DEFAULT ; READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nssmis
         END SELECT
      ENDDO

      READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) char_ned

      IF (iost /= 0) THEN
         Use_SsmiRetrievalObs = .false.
         Use_SsmiTbObs  = .false.
         use_ssmt1obs   = .false.
         use_ssmt2obs   = .false.

         write(unit=stderr, fmt='(/a,i3/)') &
            'ERROR READING FILE UNIT:', iunit, &
            'ERROR CODE:', iost

         return
      ENDIF
      if (char_ned(1:6) == 'NESTIX') EXIT

   ENDDO

   DO
      SELECT CASE ( char_ned (1:6) )

         ! Model domains definition

         CASE ('NESTIX') ;
            call da_read_ssmi_integer_array(nestix, maxnes, 8, 9)
         CASE ('NESTJX') ; 
            call da_read_ssmi_integer_array(nestjx, maxnes, 8, 9)
         CASE ('NUMC  ') ; 
            call da_read_ssmi_integer_array(numnc , maxnes, 8, 9)
         CASE ('DIS   ') ; 
            call da_read_ssmi_real_array   (dis   , maxnes, 8, 9)
         CASE ('NESTI ') ; 
            call da_read_ssmi_integer_array(nesti , maxnes, 8, 9)
         CASE ('NESTJ ') ; 
            call da_read_ssmi_integer_array(nestj , maxnes, 8, 9)
      END SELECT

      READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) char_ned
      if (char_ned(1:6) == 'INFO  ') EXIT

   ENDDO

   READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) char_ned

CONTAINS

#include "da_read_ssmi_integer_array.inc"
#include "da_read_ssmi_real_array.inc"

end subroutine da_read_ssmi_info
