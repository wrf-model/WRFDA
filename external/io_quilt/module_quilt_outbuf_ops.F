MODULE module_quilt_outbuf_ops
  INTEGER, PARAMETER :: tabsize = 1000
  INTEGER            :: num_entries
  TYPE outrec
    CHARACTER*80                       :: VarName, DateStr, MemoryOrder, Stagger, DimNames(3)
    INTEGER                            :: ndim
    INTEGER, DIMENSION(3)              :: DomainStart, DomainEnd, FieldType
    REAL,    POINTER, DIMENSION(:,:,:) :: rptr 
    INTEGER, POINTER, DIMENSION(:,:,:) :: iptr
  END TYPE outrec
  TYPE(outrec), DIMENSION(tabsize) :: outbuf_table
CONTAINS
  SUBROUTINE init_outbuf
    IMPLICIT NONE
    INTEGER i
    DO i = 1, tabsize
      outbuf_table(i)%VarName = ""
      outbuf_table(i)%DateStr = ""
      outbuf_table(i)%MemoryOrder = ""
      outbuf_table(i)%Stagger = ""
      outbuf_table(i)%DimNames(1) = ""
      outbuf_table(i)%DimNames(2) = ""
      outbuf_table(i)%DimNames(3) = ""
      outbuf_table(i)%ndim = 0
      NULLIFY( outbuf_table(i)%rptr )
      NULLIFY( outbuf_table(i)%iptr )
    ENDDO
    num_entries = 0
  END SUBROUTINE init_outbuf


  SUBROUTINE write_outbuf ( DataHandle )
    IMPLICIT NONE
    INTEGER , INTENT(IN)  :: DataHandle
    INTEGER               :: ii,ds1,de1,ds2,de2,ds3,de3
    INTEGER               :: Comm, IOComm, DomainDesc ! dummy
    INTEGER               :: Status
    CHARACTER*80          :: mess
    Comm = 0 ; IOComm = 0 ; DomainDesc = 0 
    DO ii = 1, num_entries
      WRITE(mess,*)'writing ', &
                    TRIM(outbuf_table(ii)%DateStr)," ",                                   &
                    TRIM(outbuf_table(ii)%VarName)," ",                                   &
                    TRIM(outbuf_table(ii)%MemoryOrder)
      ds1 = outbuf_table(ii)%DomainStart(1) ; de1 = outbuf_table(ii)%DomainEnd(1)
      ds2 = outbuf_table(ii)%DomainStart(2) ; de2 = outbuf_table(ii)%DomainEnd(2)
      ds3 = outbuf_table(ii)%DomainStart(3) ; de3 = outbuf_table(ii)%DomainEnd(3)

#ifndef QUILT_INTERNAL_IO
      CALL ext_ncd_write_field ( DataHandle ,                                     &
                             TRIM(outbuf_table(ii)%DateStr),                      &
                             TRIM(outbuf_table(ii)%VarName),                      &
			     outbuf_table(ii)%rptr(ds1:de1,ds2:de2,ds3:de3),      &
			     outbuf_table(ii)%FieldType,                          &  !*
			     Comm, IOComm, DomainDesc ,                           &
                             TRIM(outbuf_table(ii)%MemoryOrder),                  &
                             TRIM(outbuf_table(ii)%Stagger),                      &  !*
                             outbuf_table(ii)%DimNames ,                          &  !*
			     outbuf_table(ii)%DomainStart,                        &
			     outbuf_table(ii)%DomainEnd,                          &
			     outbuf_table(ii)%DomainStart,                        &
			     outbuf_table(ii)%DomainEnd,                          &
			     outbuf_table(ii)%DomainStart,                        &
			     outbuf_table(ii)%DomainEnd,                          &
			     Status )
#else
      CALL ext_int_write_field ( DataHandle ,                                     &
                             TRIM(outbuf_table(ii)%DateStr),                      &
                             TRIM(outbuf_table(ii)%VarName),                      &
                             outbuf_table(ii)%rptr(ds1:de1,ds2:de2,ds3:de3),      &
                             outbuf_table(ii)%FieldType,                          &  !*
                             Comm, IOComm, DomainDesc ,                           &
                             TRIM(outbuf_table(ii)%MemoryOrder),                  &
                             TRIM(outbuf_table(ii)%Stagger),                      &  !*
                             outbuf_table(ii)%DimNames ,                          &  !*
                             outbuf_table(ii)%DomainStart,                        &
                             outbuf_table(ii)%DomainEnd,                          &
                             outbuf_table(ii)%DomainStart,                        &
                             outbuf_table(ii)%DomainEnd,                          &
                             outbuf_table(ii)%DomainStart,                        &
                             outbuf_table(ii)%DomainEnd,                          &
                             Status )
#endif

      IF ( ASSOCIATED( outbuf_table(ii)%rptr) ) DEALLOCATE(outbuf_table(ii)%rptr)
      IF ( ASSOCIATED( outbuf_table(ii)%iptr) ) DEALLOCATE(outbuf_table(ii)%iptr)
      NULLIFY( outbuf_table(ii)%rptr )
      NULLIFY( outbuf_table(ii)%iptr )
    ENDDO
    CALL init_outbuf
  END SUBROUTINE write_outbuf

END MODULE module_quilt_outbuf_ops

! don't let other programs see the definition of this; type mismatches
! on inbuf will result;  may want to make a module program at some point 
  SUBROUTINE store_patch_in_outbuf( inbuf, DateStr, VarName , FieldType, MemoryOrder, Stagger, DimNames, &
				    DomainStart , DomainEnd , &
				    MemoryStart , MemoryEnd , &
				    PatchStart , PatchEnd )
    USE module_quilt_outbuf_ops
    IMPLICIT NONE
    INCLUDE 'wrf_io_flags.h'
    INTEGER ,                INTENT(IN) :: FieldType
    REAL    , DIMENSION(*) , INTENT(IN) :: inbuf
    INTEGER , DIMENSION(3) , INTENT(IN) :: DomainStart , DomainEnd , MemoryStart , MemoryEnd , PatchStart , PatchEnd
    CHARACTER*(*)    , INTENT(IN) :: DateStr , VarName, MemoryOrder , Stagger, DimNames(3)
! Local
    CHARACTER*120 mess
    INTEGER               :: l,m,n,ii,jj
    LOGICAL               :: found

    ! Fine the VarName if it's in the buffer already
    ii = 1
    found = .false.
    DO WHILE ( .NOT. found .AND. ii .LE. num_entries )
      IF ( TRIM(VarName) .EQ. TRIM(outbuf_table(ii)%VarName) ) THEN
	IF ( TRIM(DateStr) .EQ. TRIM(outbuf_table(ii)%DateStr) ) THEN
	  IF ( TRIM(MemoryOrder) .EQ. TRIM(outbuf_table(ii)%MemoryOrder) ) THEN
	    found = .true.
	  ELSE
	    CALL wrf_error_fatal("external/io_quilt/module_quilt_outbuf_ops.F: store_patch_in_outbuf: memory order disagreement")
	  ENDIF
	ELSE
	  CALL wrf_error_fatal("external/io_quilt/module_quilt_outbuf_ops.F: store_patch_in_outbuf: multiple dates in buffer")
	ENDIF
      ELSE
        ii = ii + 1
      ENDIF
    ENDDO
    IF ( .NOT. found ) THEN
      num_entries = num_entries + 1
      IF      ( FieldType .EQ. WRF_REAL4 ) THEN
        ALLOCATE( outbuf_table(num_entries)%rptr(DomainStart(1):DomainEnd(1),DomainStart(2):DomainEnd(2),DomainStart(3):DomainEnd(3)) )
      ELSE IF ( FieldType .EQ. WRF_REAL8 ) THEN
        ALLOCATE( outbuf_table(num_entries)%rptr(DomainStart(1):DomainEnd(1),DomainStart(2):DomainEnd(2),DomainStart(3):DomainEnd(3)) )
!      ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
!        ALLOCATE( outbuf_table(num_entries)%iptr(DomainStart(1):DomainEnd(1),DomainStart(2):DomainEnd(2),DomainStart(3):DomainEnd(3)) )
      ELSE
        write(mess,*)"external/io_quilt/module_quilt_outbuf_ops.F: store_patch_in_outbuf: unsupported type ", FieldType
	CALL wrf_error_fatal(mess)
      ENDIF
      outbuf_table(num_entries)%VarName = TRIM(VarName)
      outbuf_table(num_entries)%DateStr = TRIM(DateStr)
      outbuf_table(num_entries)%MemoryOrder = TRIM(MemoryOrder)
      outbuf_table(num_entries)%Stagger = TRIM(Stagger)
      outbuf_table(num_entries)%DimNames(1) = TRIM(DimNames(1))
      outbuf_table(num_entries)%DimNames(2) = TRIM(DimNames(2))
      outbuf_table(num_entries)%DimNames(3) = TRIM(DimNames(3))
      outbuf_table(num_entries)%DomainStart = DomainStart
      outbuf_table(num_entries)%DomainEnd = DomainEnd
      outbuf_table(num_entries)%FieldType = FieldType
      ii = num_entries
    ENDIF
    jj = 1
    DO n = PatchStart(3),PatchEnd(3)
      DO m = PatchStart(2),PatchEnd(2)
        DO l = PatchStart(1),PatchEnd(1)
          outbuf_table(ii)%rptr(l,m,n) = inbuf(jj)
	  jj = jj + 1
	ENDDO
      ENDDO
    ENDDO

!    jj = 1
!    write(77,*)PatchEnd(3)-PatchStart(3)+1,PatchEnd(1)-PatchStart(1)+1
!    DO n = PatchStart(3),PatchEnd(3)
!      DO m = PatchStart(2),PatchEnd(2)
!        DO l = PatchStart(1),PatchEnd(1)
!	  if ( m .eq. 1 ) write(77,*)inbuf(jj)
!	  jj = jj + 1
!	ENDDO
!      ENDDO
!    ENDDO


    RETURN

  END SUBROUTINE store_patch_in_outbuf



