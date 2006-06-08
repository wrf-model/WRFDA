!WRF:DRIVER_LAYER:UTIL
!

MODULE module_wrf_error
  INTEGER           :: wrf_debug_level = 0
  CHARACTER*256     :: wrf_err_message
  CHARACTER(LEN=256) :: errmsg(100)
  INTEGER            :: msgcount

  INTERFACE wrf_error_fatal3
!    SUBROUTINE wrf_error_fatal3_str( file_str, line, str)
!      CHARACTER(LEN=*)  :: file_str
!      INTEGER          :: line
!      CHARACTER(LEN=*) :: str
!    END SUBROUTINE wrf_error_fatal3_str

    SUBROUTINE wrf_error_fatal3( file_str, line, str)
      CHARACTER(LEN=*), INTENT(IN) :: file_str
      INTEGER ,         INTENT(IN) :: line
      CHARACTER(LEN=*), INTENT(IN) :: str
    END SUBROUTINE wrf_error_fatal3

    SUBROUTINE wrf_error_fatal3_strs( file_str, line, strs )
      CHARACTER(LEN=*), INTENT(IN) :: file_str
      INTEGER ,         INTENT(IN) :: line
      CHARACTER(LEN=*), INTENT(IN) :: strs(:)
    END SUBROUTINE wrf_error_fatal3_strs
  END INTERFACE wrf_error_fatal3

  INTERFACE wrf_warning3
    SUBROUTINE wrf_warning3( file_str, line, str)
      CHARACTER(LEN=*), INTENT(IN) :: file_str
      INTEGER,          INTENT(IN) :: line
      CHARACTER(LEN=*), INTENT(IN) :: str
    END SUBROUTINE wrf_warning3

    SUBROUTINE wrf_warning3_strs( file_str, line, strs )
      CHARACTER(LEN=*), INTENT(IN) :: file_str
      INTEGER,          INTENT(IN) :: line
      CHARACTER(LEN=*), INTENT(IN) :: strs(:)
    END SUBROUTINE wrf_warning3_strs
  END INTERFACE wrf_warning3

  INTERFACE wrf_message
!    SUBROUTINE wrf_message_str(str)
!      CHARACTER(LEN=*) :: str
!    END SUBROUTINE wrf_message_str

    SUBROUTINE wrf_message(str)
      CHARACTER(LEN=*), INTENT(IN) :: str
    END SUBROUTINE wrf_message

    SUBROUTINE wrf_message_strs(strs)
      CHARACTER(LEN=*), INTENT(IN) :: strs(:)
    END SUBROUTINE wrf_message_strs
  END INTERFACE wrf_message

  INTERFACE wrf_message2
!    SUBROUTINE wrf_message2_str(str)
!      CHARACTER(LEN=*) :: str
!    END SUBROUTINE wrf_message2_str

    SUBROUTINE wrf_message2(str)
      CHARACTER(LEN=*), INTENT(IN) :: str
    END SUBROUTINE wrf_message2

    SUBROUTINE wrf_message2_strs(strs)
      CHARACTER(LEN=*), INTENT(IN) :: strs(:)
    END SUBROUTINE wrf_message2_strs
  END INTERFACE wrf_message2

CONTAINS

  LOGICAL FUNCTION wrf_at_debug_level ( level )
    IMPLICIT NONE
    INTEGER , INTENT(IN) :: level
    wrf_at_debug_level = ( level .LE. wrf_debug_level )
    RETURN
  END FUNCTION wrf_at_debug_level

END MODULE module_wrf_error

SUBROUTINE init_module_wrf_error
END SUBROUTINE init_module_wrf_error


SUBROUTINE set_wrf_debug_level ( level )
  USE module_wrf_error
  IMPLICIT NONE
  INTEGER , INTENT(IN) :: level
  wrf_debug_level = level
  RETURN
END SUBROUTINE set_wrf_debug_level

SUBROUTINE get_wrf_debug_level ( level )
  USE module_wrf_error
  IMPLICIT NONE
  INTEGER , INTENT(OUT) :: level
  level = wrf_debug_level
  RETURN
END SUBROUTINE get_wrf_debug_level

SUBROUTINE wrf_debug( level , str )
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  INTEGER               :: debug_level
  CALL get_wrf_debug_level( debug_level )
  IF ( level .LE. debug_level ) THEN
    CALL wrf_message( str )
  ENDIF
  RETURN
END SUBROUTINE wrf_debug

SUBROUTINE wrf_message_strs(strs)
  USE module_utility
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: strs(:)
  INTEGER :: i
#if defined( DM_PARALLEL ) && ! defined( STUBMPI)
  DO I=1,SIZE(strs) 
    write(6,*) strs(i)
    write(0,*) strs(i)
  END DO
#endif
!TBH:  work around deficiencies in build-automation
#ifdef USE_LOGERR
  IF ( WRF_UTIL_IsInitialized() ) THEN
    DO I=1,SIZE(strs) 
      CALL WRF_UTIL_LogWrite( strs(i), WRF_UTIL_LOG_INFO )
    END DO
  ENDIF
#endif
END SUBROUTINE wrf_message_strs

SUBROUTINE wrf_message( str)
  USE module_utility
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: str
  INTEGER :: i
#if defined( DM_PARALLEL ) && ! defined( STUBMPI)
   write(6,*) str
     write(0,*) str
#endif
!TBH:  work around deficiencies in build-automation
#ifdef USE_LOGERR
  IF ( WRF_UTIL_IsInitialized() ) THEN
    CALL WRF_UTIL_LogWrite( str, WRF_UTIL_LOG_INFO )
  ENDIF
#endif
END SUBROUTINE wrf_message

! intentionally write to stderr only
SUBROUTINE wrf_message2_strs(strs )
  USE module_utility
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: strs(:)
  INTEGER :: i
  DO I=1,SIZE(strs) 
    write(0,*) strs(i)
  END DO
!TBH:  work around deficiencies in build-automation
#ifdef USE_LOGERR
  IF ( WRF_UTIL_IsInitialized() ) THEN
     DO I=1,SIZE(strs) 
      CALL WRF_UTIL_LogWrite( strs(i), WRF_UTIL_LOG_INFO )
    END DO
  ENDIF
#endif
END SUBROUTINE wrf_message2_strs

SUBROUTINE wrf_message2(str)
  USE module_utility
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: str
  INTEGER :: i
   write(0,*) str
!TBH:  work around deficiencies in build-automation
#ifdef USE_LOGERR
  IF ( WRF_UTIL_IsInitialized() ) THEN
    CALL WRF_UTIL_LogWrite( str, WRF_UTIL_LOG_INFO )
  ENDIF
#endif
END SUBROUTINE wrf_message2

SUBROUTINE wrf_error_fatal3_strs( file_str, line, strs )
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: file_str
  INTEGER ,         INTENT(IN) :: line  ! only print file and line if line > 0
  CHARACTER(LEN=*), INTENT(IN) :: strs(:)
  CHARACTER*256 :: line_str

  write(line_str,'(i6)') line
#if defined( DM_PARALLEL ) && ! defined( STUBMPI )
  CALL wrf_message( '-------------- FATAL CALLED ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message_strs(strs)
  CALL wrf_message( '-------------------------------------------' )
#else
  CALL wrf_message2( '-------------- FATAL CALLED ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message2_strs( strs )
  CALL wrf_message2( '-------------------------------------------' )
#endif
  CALL wrf_abort
END SUBROUTINE wrf_error_fatal3_strs

SUBROUTINE wrf_error_fatal3( file_str, line,str )
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: file_str
  INTEGER ,         INTENT(IN) :: line  ! only print file and line if line > 0
  CHARACTER(LEN=*), INTENT(IN) :: str
  CHARACTER*256 :: line_str

  write(line_str,'(i6)') line
#if defined( DM_PARALLEL ) && ! defined( STUBMPI )
  CALL wrf_message( '-------------- FATAL CALLED ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message(str)
  CALL wrf_message( '-------------------------------------------' )
#else
  CALL wrf_message2( '-------------- FATAL CALLED ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message2( str )
  CALL wrf_message2( '-------------------------------------------' )
#endif
  CALL wrf_abort
END SUBROUTINE wrf_error_fatal3

SUBROUTINE wrf_warning3( file_str, line, str )
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: file_str
  INTEGER,          INTENT(IN) :: line
  CHARACTER(LEN=*), INTENT(IN) :: str
  CHARACTER*256 :: line_str

  write(line_str,'(i6)') line
#if defined( DM_PARALLEL ) && ! defined( STUBMPI )
  CALL wrf_message( '-------------- WARNING      ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'WARNING FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message( str )
  CALL wrf_message( '-------------------------------------------' )
#else
  CALL wrf_message2( '-------------- WARNING ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'WARNING FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message2(str )
  CALL wrf_message2( '-------------------------------------------' )
#endif
END SUBROUTINE wrf_warning3

SUBROUTINE wrf_warning3_strs( file_str, line, strs )
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: file_str
  INTEGER,          INTENT(IN) :: line
  CHARACTER(LEN=*), INTENT(IN) :: strs(:)
  CHARACTER*256 :: line_str

  write(line_str,'(i6)') line
#if defined( DM_PARALLEL ) && ! defined( STUBMPI )
  CALL wrf_message( '-------------- WARNING      ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'WARNING FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message_strs( strs )
  CALL wrf_message( '-------------------------------------------' )
#else
  CALL wrf_message2( '-------------- WARNING ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'WARNING FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message2_strs(strs )
  CALL wrf_message2( '-------------------------------------------' )
#endif
END SUBROUTINE wrf_warning3_strs

SUBROUTINE wrf_error_fatal( str)
  IMPLICIT NONE
  CHARACTER(LEN=*) :: str
  CALL wrf_error_fatal3 ( ' ', 0, str)
END SUBROUTINE wrf_error_fatal

! Check to see if expected value == actual value
! If not, print message and exit.  
SUBROUTINE wrf_check_error( expected, actual, str, file_str, line )
  IMPLICIT NONE
  INTEGER , INTENT (IN) :: expected
  INTEGER , INTENT (IN) :: actual
  CHARACTER*(*) str
  CHARACTER*(*) file_str
  INTEGER , INTENT (IN) :: line
  CHARACTER (LEN=512)   :: rc_str
  CHARACTER (LEN=512)   :: str_with_rc

  IF ( expected .ne. actual ) THEN
    WRITE (rc_str,*) '  Routine returned error code = ',actual
    str_with_rc = TRIM(str // rc_str)
    CALL wrf_error_fatal3 ( file_str, line, str_with_rc )
  ENDIF
END SUBROUTINE wrf_check_error


