SUBROUTINE DA_Read_SSMI_Info (iunit, ob, xb, xbx)

!-----------------------------------------------------------------------------!
! Read the header of a MM5 3D-VAR 3.0 GTS observation file
!-----------------------------------------------------------------------------!

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
!-----------------------------------------------------------------------------!

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

      write(unit=0, fmt='(/a,i3/)') &
           'ERROR READING FILE UNIT:', iunit, &
           'ERROR CODE:', iost

      return
   else
      write(unit=*, fmt='(/2a/)') &
           'in DA_Read_SSMI_Info.inc, char_ned=', trim(char_ned)
   ENDIF

! 2.3 READ NUMBER OF REPORTS
!     ---------------------

head_line_1:  DO
 
   DO i = 0, 120-14

     SELECT CASE ( char_ned (I+1:I+5) )
  ! Number of observations
     CASE ('SSMI ') ; 
         if (Use_SsmiRetrievalObs) &
            READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) ob%num_ssmi_retrieval
         if (Use_SsmiTbObs) &
            READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) ob%num_ssmi_tb
     CASE ('OTHER') ; READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nothers

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

      write(unit=0, fmt='(/a,i3/)') &
           'ERROR READING FILE UNIT:', iunit, &
           'ERROR CODE:', iost

      return
   ENDIF
   if (char_ned(1:6) == 'NESTIX') EXIT head_line_1

   ENDDO head_line_1

head_line_2:  DO

     SELECT CASE ( char_ned (1:6) )

  ! Model domains definition
    
      CASE ('NESTIX') ;
           CALL read_integer_array(nestix, maxnes, 8, 9)
      CASE ('NESTJX') ; 
           CALL read_integer_array(nestjx, maxnes, 8, 9)
      CASE ('NUMC  ') ; 
           CALL read_integer_array(numnc , maxnes, 8, 9)
      CASE ('DIS   ') ; 
           CALL read_real_array   (dis   , maxnes, 8, 9)
      CASE ('NESTI ') ; 
           CALL read_integer_array(nesti , maxnes, 8, 9)
      CASE ('NESTJ ') ; 
           CALL read_integer_array(nestj , maxnes, 8, 9)
     END SELECT

     READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) char_ned
     if (char_ned(1:6) == 'INFO  ') EXIT head_line_2

   ENDDO head_line_2

   READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) char_ned

CONTAINS

subroutine read_integer_array(aa,nn,i0,i_step)

  integer,                 intent(in) :: nn, i0, i_step
  integer,  dimension(:), intent(out) :: aa

  integer     :: i, ii

  do i = 1, nn
    ii = i_step*(i-1) + i0
    READ (char_ned (ii:ii+8),'((I7,2X))') aa(i)
  enddo
end subroutine read_integer_array

subroutine read_real_array(aa,nn,i0,i_step)

  integer,                 intent(in) :: nn, i0, i_step
  real   ,  dimension(:), intent(out) :: aa

  integer     :: i, ii

  do i = 1, nn
    ii = i_step*(i-1) + i0
    READ (char_ned (ii:ii+8),'((f7.2,2X))') aa(i)
  enddo
end subroutine read_real_array

END SUBROUTINE DA_Read_SSMI_Info
