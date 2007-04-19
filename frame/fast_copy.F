      SUBROUTINE fast_copy_lint ( outbuf, inbuf, n )
        IMPLICIT NONE
        INTEGER n
        INTEGER*8 inbuf(*), outbuf(*)
        ! Local
        INTEGER i
        DO i = 1,n
           outbuf(i) = inbuf(i)
        ENDDO
        RETURN
      END SUBROUTINE fast_copy_lint

      SUBROUTINE fast_zero_lint ( outbuf, n )
        IMPLICIT NONE
        INTEGER n
        INTEGER*8 outbuf(*)
        ! Local
        INTEGER i
        DO i = 1,n
           outbuf(i) = 0
        ENDDO
        RETURN
      END SUBROUTINE fast_zero_lint



! TBH:  TODO:  Move this to another file!  
! TBH:  TODO:  Improve very simple design if needed.  
! TBH:  TODO:   - Handle multiple "frames" separately
! TBH:  TODO:   - Add erasure of fields
! TBH:  TODO:     - move to a dynamic set of buffers
! TBH:  These very simple routines are used to read/write 
! TBH:  fields to/from memory.  Everything is stored in a 1D buffer.  
! TBH:  Extend later to perform I/O instead if needed.  
      MODULE module_4dvaropt

        ! maximum number fields to store
        INTEGER, PARAMETER :: MAX_NUM_FIELDS=1000
        ! total size of all stored fields in INTEGER*8 words
        INTEGER, PARAMETER :: MAX_NUM_WORDS=7000000

        ! start index of each stored field
        INTEGER :: field_starts(MAX_NUM_FIELDS)
        ! number of words in each stored field
        INTEGER :: field_counts(MAX_NUM_FIELDS)
        ! number fields currently stored
        INTEGER, SAVE :: num_fields=0
        ! buffer
        INTEGER*8 :: buffer(MAX_NUM_WORDS)

        ! debugging
        CHARACTER*256 :: msg

      CONTAINS

        SUBROUTINE module_4dvaropt_init
          IMPLICIT NONE
          field_starts = 0
          field_counts = 0
! TODO:  auto-init to NaN for debugging...  
!         buffer = 0
          RETURN
        END SUBROUTINE module_4dvaropt_init

      END MODULE module_4dvaropt



      SUBROUTINE clear_field_buffer
        USE module_4dvaropt
        IMPLICIT NONE
        num_fields = 0
        CALL module_4dvaropt_init
        RETURN
      END SUBROUTINE clear_field_buffer



      SUBROUTINE store_field_lint ( field_id , inbuf , numwords )
        USE module_4dvaropt
        IMPLICIT NONE
        INTEGER field_id, numwords
        INTEGER*8 inbuf(*)
        ! Local
        INTEGER newstart
        WRITE(msg,'(A,I10)') 'DEBUG store_field_lint:  field_id = ',field_id
        CALL wrf_debug( 10, TRIM(msg) )
        ! init if needed
        IF ( num_fields == 0 ) THEN
          CALL module_4dvaropt_init
        ENDIF
        num_fields = num_fields + 1
        IF ( num_fields > MAX_NUM_FIELDS ) THEN
          CALL wrf_error_fatal( "Too many fields in store_field_lint" )
        ENDIF
        ! remove this limitation later, if needed
        IF ( num_fields .ne. field_id ) THEN
          CALL wrf_error_fatal( "Must write fields in order in store_field_lint" )
        ENDIF
        IF ( num_fields == 1 ) THEN
          newstart = 1
        ELSE
          newstart = field_starts(num_fields-1) + field_counts(num_fields-1)
        ENDIF
        field_starts(num_fields) = newstart
        field_counts(num_fields) = numwords
        WRITE(msg,'(A,I10,A,I10)') 'DEBUG store_field_lint:  field_id = ',field_id,'  buffer end = ',(newstart+numwords-1)
        CALL wrf_debug( 10, TRIM(msg) )
        IF ( newstart+numwords-1 > MAX_NUM_WORDS ) THEN
          CALL wrf_error_fatal( "Ran out of memory in store_field_lint" )
        ENDIF
        CALL fast_copy_lint ( buffer(newstart), inbuf, numwords )
        RETURN
      END SUBROUTINE store_field_lint



      SUBROUTINE restore_field_lint ( field_id , outbuf , numwords )
        USE module_4dvaropt
        IMPLICIT NONE
        INTEGER field_id, numwords
        INTEGER*8 outbuf(*)
        ! Local
        INTEGER thisstart
        WRITE(msg,'(A,I10)') 'DEBUG restore_field_lint:  field_id = ',field_id
        CALL wrf_debug( 10, TRIM(msg) )
        IF (( field_id > num_fields ) .OR. ( field_id < 1 )) THEN
          CALL wrf_error_fatal( "Field ID out of range in restore_field_lint" )
        ENDIF
        IF ( numwords .ne. field_counts(field_id) ) THEN
          CALL wrf_error_fatal( "Incorrect count in restore_field_lint" )
        ENDIF
        thisstart = field_starts(field_id)
        WRITE(msg,'(A,I10,A,I10)') 'DEBUG restore_field_lint:  field_id = ',field_id,'  buffer end = ',(thisstart+numwords-1)
        CALL wrf_debug( 10, TRIM(msg) )
        CALL fast_copy_lint ( outbuf, buffer(thisstart), numwords )
        RETURN
      END SUBROUTINE restore_field_lint




