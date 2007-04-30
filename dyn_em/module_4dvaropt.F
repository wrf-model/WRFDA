
! TBH:  TODO:  Wrap assertions in #ifdef DEBUG to speed up store/restore.  

! TBH:  TODO:  Improve very simple design if needed.  
! TBH:  TODO:   - Add erasure of fields
! TBH:  TODO:     - move to a dynamic set of buffers
! TBH:  These very simple routines are used to read/write 
! TBH:  fields to/from memory.  Everything is stored in a 1D buffer.  
! TBH:  Extend later to perform I/O instead if needed.  

      MODULE module_4dvaropt

        IMPLICIT NONE

        ! Maximum numbers of buffers, frames and, fields.  
        ! Note that all data is actually stored in a single 1D array which is 
        ! addressed via (field_id, frame_id, buffer_id) for convenience.  
        INTEGER, PARAMETER :: MAX_NUM_BUFFERS=10

        ! maximum number frames (groups of fields) in any buffer
        INTEGER, PARAMETER :: MAX_NUM_FRAMES=25

        ! maximum number fields in any frame
        INTEGER, PARAMETER :: MAX_NUM_FIELDS=200

        ! total size of all stored fields in INTEGER*8 words
        INTEGER, PARAMETER :: MAX_NUM_WORDS=7000000

        ! start index of each stored field
        ! NOTE:  field_starts(0,:,:) is only used for initialization.  
        INTEGER, SAVE :: &
          field_starts(0:MAX_NUM_FIELDS, MAX_NUM_FRAMES, MAX_NUM_BUFFERS)

        ! number of words in each stored field
        ! NOTE:  field_counts(0,:,:) is only used for initialization.  
        INTEGER, SAVE :: &
          field_counts(0:MAX_NUM_FIELDS, MAX_NUM_FRAMES, MAX_NUM_BUFFERS)

        ! number fields currently stored in each frame of each buffer
        INTEGER, SAVE :: num_fields(MAX_NUM_FRAMES, MAX_NUM_BUFFERS)

        ! index of current fields for reading each frame of each buffer
        INTEGER, SAVE :: read_field_ids(MAX_NUM_FRAMES, MAX_NUM_BUFFERS)

        ! internal counters for storing/retrieving fields
        INTEGER, SAVE :: current_buffer
        INTEGER, SAVE :: current_frame

        ! buffer
        INTEGER*8 :: buffer(MAX_NUM_WORDS)
        ! points to next free spot in buffer
        INTEGER, SAVE :: buffer_position

        ! debugging
        CHARACTER*256 :: msg

      CONTAINS

        SUBROUTINE module_4dvaropt_start
          buffer_position = 1
          field_starts = 1
          field_counts = 0
          num_fields = 0
          ! points by default to an "empty" field
          read_field_ids = 0
          current_buffer = 1
          current_frame = 1
! TODO:  auto-init to NaN for debugging...  
!         buffer = 0
          RETURN
        END SUBROUTINE module_4dvaropt_start


        ! select specified frame of specified buffer for store or restore
        ! position read_field_ids at the beginning of the frame for restore
        SUBROUTINE select_frame( frame_id , buffer_id )
          INTEGER, INTENT(IN) :: frame_id
          INTEGER, INTENT(IN) :: buffer_id
          current_frame = frame_id
          current_buffer = buffer_id
          read_field_ids(current_frame, current_buffer) = 1
          WRITE(msg,'(A,I10,A,I10)') &
            'select_frame:   current_frame, current_buffer = ', &
            current_frame, " , ", current_buffer
          CALL wrf_debug( 10, TRIM(msg) )
          RETURN
        END SUBROUTINE select_frame


        SUBROUTINE module_4dvaropt_end
          ! print out actual number of words used at end
          WRITE(msg,'(A,I10,A)') 'Maximum buffer size = ',buffer_position-1, &
                                 ' INTEGER*8 words'
          CALL wrf_message( TRIM(msg) )
          RETURN
        END SUBROUTINE module_4dvaropt_end


      END MODULE module_4dvaropt


! TBH:  These routines are outside the module to avoid TKR match.  
! TBH:  TODO:  Overload interfaces and put back in module.  
        ! Store field at the end of the current frame of the current buffer.  
        SUBROUTINE store_field_lint ( inbuf , numwords )
          USE module_4dvaropt
          IMPLICIT NONE
          INTEGER numwords
          INTEGER*8 inbuf(*)
          ! Local
          INTEGER newstart, field_id
          WRITE(msg,'(A,I10,A,I10)') 'DEBUG store_field_lint:  current_frame,current_buffer = ',current_frame,",",current_buffer
          CALL wrf_debug( 10, TRIM(msg) )
          num_fields(current_frame,current_buffer) = &
            num_fields(current_frame,current_buffer) + 1
          field_id = num_fields(current_frame,current_buffer)
          WRITE(msg,'(A,I10)') 'DEBUG store_field_lint:  field_id = ',field_id
          CALL wrf_debug( 10, TRIM(msg) )
          IF ( field_id > MAX_NUM_FIELDS ) THEN
            CALL wrf_error_fatal( "Too many fields in store_field_lint" )
          ENDIF
          newstart = buffer_position
          field_starts(field_id,current_frame,current_buffer) = newstart
          field_counts(field_id,current_frame,current_buffer) = numwords
          WRITE(msg,'(A,I10,A,I10,A,I10)') &
            'DEBUG store_field_lint:  field_id = ',field_id,&
            '  buffer start = ',newstart,&
            '  buffer end = ',(newstart+numwords-1)
          CALL wrf_debug( 10, TRIM(msg) )
          buffer_position = buffer_position + numwords
          IF ( buffer_position-1 > MAX_NUM_WORDS ) THEN
            CALL wrf_error_fatal( "Ran out of memory in store_field_lint" )
          ENDIF
          CALL fast_copy_lint ( buffer(newstart), inbuf, numwords )
          RETURN
        END SUBROUTINE store_field_lint



        ! Read current field from current frame of current buffer and 
        ! increment current field id.  
        SUBROUTINE restore_field_lint ( outbuf , numwords )
          USE module_4dvaropt
          IMPLICIT NONE
          INTEGER numwords
          INTEGER*8 outbuf(*)
          ! Local
          INTEGER field_id, thisstart
          WRITE(msg,'(A,I10,A,I10)') 'DEBUG restore_field_lint:  current_frame,current_buffer = ',current_frame,",",current_buffer
          CALL wrf_debug( 10, TRIM(msg) )
          field_id = read_field_ids(current_frame, current_buffer)
          WRITE(msg,'(A,I10)') 'DEBUG restore_field_lint:  field_id = ',field_id
          CALL wrf_debug( 10, TRIM(msg) )
! TODO:  remove this assertion
          IF (( field_id > num_fields(current_frame,current_buffer) ) .OR. &
              ( field_id < 1 )) THEN
            CALL wrf_error_fatal( "Field ID out of range in restore_field_lint" )
          ENDIF
          thisstart = field_starts(field_id,current_frame,current_buffer)
          WRITE(msg,'(A,I10,A,I10,A,I10)') &
            'DEBUG restore_field_lint:  field_id = ',field_id, &
            '  buffer start = ',thisstart, &
            '  buffer end = ',(thisstart+field_counts(field_id,current_frame,current_buffer)-1)
          CALL wrf_debug( 10, TRIM(msg) )
          IF ( numwords .ne. field_counts(field_id,current_frame,current_buffer) ) THEN
            CALL wrf_error_fatal( "Incorrect count in restore_field_lint" )
          ENDIF
          CALL fast_copy_lint ( outbuf, buffer(thisstart), numwords )
          ! increment field_id for reading
          read_field_ids(current_frame, current_buffer) = field_id + 1
          RETURN
        END SUBROUTINE restore_field_lint



#if 0
! TBH:  SYSTEM_CLOCK and CPU_TIME both have resolution of 10ms on IBMs.  This 
! TBH:  is too coarse for what I'm doing at the moment...  See 
! TBH:  ../frame/microclock.c for a better solution borrowed from the RSLs.  

! TBH:  Hack to make -DBENCH work with a serial build.  
! TBH:  TODO:  Get rid of duplicated definition of RSL_INTERNAL_MICROCLOCK 
! TBH:  TODO:  in both RSL and RSL_LITE and put it where it can be shared by 
! TBH:  TODO:  serial build too!  Then get rid of this hack.  
#ifndef DM_PARALLEL
   FUNCTION rsl_internal_microclock ( ) RESULT ( microseconds )
      IMPLICIT NONE
      INTEGER :: microseconds
      LOGICAL, SAVE :: first_call = .FALSE.
      CHARACTER*256 :: msg
      INTEGER :: clock_count, count_rate, count_max
!CT ! Either CPU_TIME sucks, or I am not using it correctly...  
!CT      REAL :: seconds
!CT      CALL CPU_TIME(seconds)
!CT      microseconds = NINT(seconds * 1000000.)
      CALL SYSTEM_CLOCK( COUNT=clock_count, COUNT_RATE=count_rate, &
                         COUNT_MAX=count_max)
      IF (.NOT. first_call) THEN
        first_call = .TRUE.
        WRITE(msg,'(A,I10,A,I10)') 'DEBUG rsl_internal_microclock:  count_rate = ',count_rate,'  count_max = ',count_max
        CALL wrf_message( TRIM(msg) )
      ENDIF
      IF (count_rate <= 0) THEN
        CALL wrf_error_fatal ( "rsl_internal_microclock:  count_rate <= 0" )
      ENDIF
      microseconds = (clock_count * 1000000) / count_rate
   END FUNCTION rsl_internal_microclock
#endif

#endif


