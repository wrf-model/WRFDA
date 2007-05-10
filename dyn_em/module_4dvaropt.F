
#define ASSERT_4DVAR_OPT
#undef DEBUG_4DVAR_OPT

! TBH:  TODO:  Compile this module with "-O3 -qhot"
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
#ifdef DEBUG_4DVAR_OPT
          WRITE(msg,'(A,I10,A,I10)') &
            'select_frame:   current_frame, current_buffer = ', &
            current_frame, " , ", current_buffer
          CALL wrf_debug( 10, TRIM(msg) )
#endif
          RETURN
        END SUBROUTINE select_frame


        SUBROUTINE module_4dvaropt_end
          LOGICAL, SAVE :: first_call = .TRUE.
          IF ( first_call ) THEN
            ! print out actual number of words used at end
            WRITE(msg,'(A,I10,A)') 'MODULE_4DVAROPT:  Maximum buffer size = ', &
                                   buffer_position-1, ' INTEGER*8 words'
            CALL wrf_message( TRIM(msg) )
            first_call = .FALSE.
          ENDIF
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
#ifdef DEBUG_4DVAR_OPT
          WRITE(msg,'(A,I10,A,I10)') 'DEBUG store_field_lint:  current_frame,current_buffer = ',current_frame,",",current_buffer
          CALL wrf_debug( 10, TRIM(msg) )
#endif
          num_fields(current_frame,current_buffer) = &
            num_fields(current_frame,current_buffer) + 1
          field_id = num_fields(current_frame,current_buffer)
#ifdef DEBUG_4DVAR_OPT
          WRITE(msg,'(A,I10)') 'DEBUG store_field_lint:  field_id = ',field_id
          CALL wrf_debug( 10, TRIM(msg) )
#endif
#ifdef ASSERT_4DVAR_OPT
          IF ( field_id > MAX_NUM_FIELDS ) THEN
            CALL wrf_error_fatal( "Too many fields in store_field_lint" )
          ENDIF
#endif
          newstart = buffer_position
          field_starts(field_id,current_frame,current_buffer) = newstart
          field_counts(field_id,current_frame,current_buffer) = numwords
#ifdef DEBUG_4DVAR_OPT
          WRITE(msg,'(A,I10,A,I10,A,I10)') &
            'DEBUG store_field_lint:  field_id = ',field_id,&
            '  buffer start = ',newstart,&
            '  buffer end = ',(newstart+numwords-1)
          CALL wrf_debug( 10, TRIM(msg) )
#endif
          buffer_position = buffer_position + numwords
#ifdef ASSERT_4DVAR_OPT
          IF ( buffer_position-1 > MAX_NUM_WORDS ) THEN
            CALL wrf_error_fatal( "Ran out of memory in store_field_lint" )
          ENDIF
#endif
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
#ifdef DEBUG_4DVAR_OPT
          WRITE(msg,'(A,I10,A,I10)') 'DEBUG restore_field_lint:  current_frame,current_buffer = ',current_frame,",",current_buffer
          CALL wrf_debug( 10, TRIM(msg) )
#endif
          field_id = read_field_ids(current_frame, current_buffer)
#ifdef DEBUG_4DVAR_OPT
          WRITE(msg,'(A,I10)') 'DEBUG restore_field_lint:  field_id = ',field_id
          CALL wrf_debug( 10, TRIM(msg) )
#endif
#ifdef ASSERT_4DVAR_OPT
! TODO:  remove this assertion
          IF (( field_id > num_fields(current_frame,current_buffer) ) .OR. &
              ( field_id < 1 )) THEN
            CALL wrf_error_fatal( "Field ID out of range in restore_field_lint" )
          ENDIF
#endif
          thisstart = field_starts(field_id,current_frame,current_buffer)
#ifdef DEBUG_4DVAR_OPT
          WRITE(msg,'(A,I10,A,I10,A,I10)') &
            'DEBUG restore_field_lint:  field_id = ',field_id, &
            '  buffer start = ',thisstart, &
            '  buffer end = ',(thisstart+field_counts(field_id,current_frame,current_buffer)-1)
          CALL wrf_debug( 10, TRIM(msg) )
#endif
#ifdef ASSERT_4DVAR_OPT
          IF ( numwords .ne. field_counts(field_id,current_frame,current_buffer) ) THEN
            CALL wrf_error_fatal( "Incorrect count in restore_field_lint" )
          ENDIF
#endif
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



        ! Write 3D array arr3d into file named "arrname.filecount" .  
        ! Any points not used by this staggering are not written.  
        ! Allowed valued for stagger are 'u', 'v', 'w', 'm'.  
        ! File format is binary.  
        ! MPI communication is used if needed to gather the array.  
        ! The array must be of type REAL.  
        !
        ! The file is written in two records.  
        ! The first record contains ids,ideL,kds,kdeL,jds,jdeL.  
        ! The second record contains the array for the entire domain.  
        !
        ! NOTE:  This is a quick hack to make up for lack of SMS-style 
        ! NOTE:  automatic debugging features.  
        !
        SUBROUTINE write_field_global3d ( arr3d,    &
                     arrname, filecount, stagger,  &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     ips, ipe, jps, jpe, kps, kpe )
          USE module_dm
          IMPLICIT NONE
          REAL,              INTENT(IN) :: arr3d( ims:ime , kms:kme, jms:jme )
          CHARACTER (LEN=*), INTENT(IN) :: arrname
          INTEGER,           INTENT(IN) :: filecount
          CHARACTER (LEN=*), INTENT(IN) :: stagger
          INTEGER,           INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                           ims, ime, jms, jme, kms, kme, &
                                           ips, ipe, jps, jpe, kps, kpe
          REAL, ALLOCATABLE :: arr3d_local(:,:,:)
          REAL, ALLOCATABLE :: arr3d_global(:,:,:)
          INTEGER :: ideL, jdeL, kdeL, ipeL, jpeL, kpeL
          INTEGER :: i, j, k
          CHARACTER (LEN=200) :: FileName
          INTEGER, PARAMETER :: fileunit=83
          LOGICAL :: fileinuse
          LOGICAL, EXTERNAL :: wrf_dm_on_monitor
#ifdef DM_PARALLEL
          INCLUDE 'mpif.h'
          INTEGER ierr, mpi_comm_local
#endif
          ! adjust domain and patch
          ideL = ide-1
          IF ( ideL < ids ) ideL = ids
          jdeL = jde-1
          IF ( jdeL < jds ) jdeL = jds
          kdeL = kde-1
          IF ( kdeL < kds ) kdeL = kds
          ipeL = ipe
          jpeL = jpe
          kpeL = kpe
          IF (TRIM(stagger) == 'u') THEN
            ideL = ide
          ELSEIF (TRIM(stagger) == 'v') THEN
            jdeL = jde
          ELSEIF (TRIM(stagger) == 'w') THEN
            kdeL = kde
          ELSEIF (TRIM(stagger) /= 'm') THEN
            CALL wrf_error_fatal ( "write_field_global3d:  unknown stagger" )
          ENDIF
          IF ( ipeL > ideL ) ipeL = ideL
          IF ( jpeL > jdeL ) jpeL = jdeL
          IF ( kpeL > kdeL ) kpeL = kdeL

          ALLOCATE(arr3d_local(ids:ideL,kds:kdeL,jds:jdeL))
          ALLOCATE(arr3d_global(ids:ideL,kds:kdeL,jds:jdeL))

          ! yes, gather would be more efficient
          arr3d_local(ids:ideL,kds:kdeL,jds:jdeL) = 0.
          arr3d_local(ips:ipeL,kps:kpeL,jps:jpeL) = &
                arr3d(ips:ipeL,kps:kpeL,jps:jpeL)

#ifdef DM_PARALLEL
          ! yes, reduction is a hack here
          CALL wrf_get_dm_communicator( mpi_comm_local )
          CALL mpi_allreduce ( arr3d_local, arr3d_global , &
                               SIZE(arr3d_local), getrealmpitype(), MPI_SUM, &
                               mpi_comm_local, ierr )
#else
          ! yes, this copy is not really necessary
          arr3d_global = arr3d_local
#endif

          IF ( wrf_dm_on_monitor() ) THEN
            WRITE(FileName,FMT="(A,A,I4.4)") TRIM(arrname),'.',filecount
            INQUIRE ( UNIT=fileunit , OPENED = fileinuse )
            IF ( fileinuse ) THEN
              CALL wrf_error_fatal ( "write_field_global3d:  file unit in use" )
            ENDIF
!            OPEN ( unit=fileunit, file=TRIM(FileName), form='unformatted', &
!                   iostat=ierr )
!            WRITE( unit=fileunit ) ids,ideL,kds,kdeL,jds,jdeL
!            WRITE( unit=fileunit ) arr3d_global
            OPEN( UNIT   = fileunit,       &
                  FILE   = TRIM(FileName), &
                  FORM   = 'FORMATTED',    &
                  ACCESS = 'SEQUENTIAL',   &
                  IOSTAT =  ierr )
            WRITE ( fileunit,* ) ids
            WRITE ( fileunit,* ) ideL
            WRITE ( fileunit,* ) kds
            WRITE ( fileunit,* ) kdeL
            WRITE ( fileunit,* ) jds
            WRITE ( fileunit,* ) jdeL
            IF ( kds == kdeL ) THEN
              DO j = jds, jdeL
                DO i = ids, ideL
                  WRITE ( fileunit,* ) TRIM(arrname), &
                    '(',i,',',j,') = ', arr3d_global(i,kds,j)
                ENDDO
              ENDDO
            ELSE
              DO j = jds, jdeL
                DO k = kds, kdeL
                  DO i = ids, ideL
                    WRITE ( fileunit,* ) TRIM(arrname), &
                      '(',i,',',k,',',j,') = ', arr3d_global(i,k,j)
                  ENDDO
                ENDDO
              ENDDO
            ENDIF
            CLOSE ( fileunit )
          ENDIF

          DEALLOCATE(arr3d_local)
          DEALLOCATE(arr3d_global)

        END SUBROUTINE write_field_global3d


        ! Write 2D array arr2d into file named "arrname.filecount" .  
        ! Any points not used by this staggering are not written.  
        ! Allowed valued for stagger are 'u', 'v', 'w', 'm'.  
        ! File format is binary.  
        ! MPI communication is used if needed to gather the array.  
        ! The array must be of type REAL.  
        !
        ! The file is written in two records.  
        ! The first record contains ids,ideL,1,1,jds,jdeL.  
        ! The second record contains the array for the entire domain.  
        !
        ! Note that arguments kds, kde, kms, kme, kps, kpe are not used.  
        SUBROUTINE write_field_global2d ( arr2d,    &
                     arrname, filecount, stagger,  &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     ips, ipe, jps, jpe, kps, kpe )
          IMPLICIT NONE
          REAL,              INTENT(IN) :: arr2d( ims:ime , jms:jme )
          CHARACTER (LEN=*), INTENT(IN) :: arrname
          INTEGER,           INTENT(IN) :: filecount
          CHARACTER (LEN=*), INTENT(IN) :: stagger
          INTEGER,           INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                           ims, ime, jms, jme, kms, kme, &
                                           ips, ipe, jps, jpe, kps, kpe
          INTEGER :: i, j
          REAL :: arr3d_tmp(ims:ime,1:1,jms:jme)
          DO j = jms, jme
            DO i = ims, ime
              arr3d_tmp(i,1,j) = arr2d(i,j)
            ENDDO
          ENDDO
          CALL write_field_global3d ( arr3d_tmp,    &
                     arrname, filecount, stagger,  &
                     ids, ide, jds, jde, 1, 1, &
                     ims, ime, jms, jme, 1, 1, &
                     ips, ipe, jps, jpe, 1, 1 )

        END SUBROUTINE write_field_global2d


