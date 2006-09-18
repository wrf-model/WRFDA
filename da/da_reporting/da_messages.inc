SUBROUTINE da_messages(strs)

#ifdef USE_LOGERR
   USE module_utility
#endif
   USE da_constants

   IMPLICIT NONE

   CHARACTER(LEN=*), INTENT(IN) :: strs(:)
   INTEGER :: i

#if defined( DM_PARALLEL ) && ! defined( STUBMPI)
   DO I=1,SIZE(strs) 
      write(unit=stdout,FMT='(A)') strs(i)
   END DO
#endif
!TBH:  work around deficiencies in build-automation
#ifdef USE_LOGERR
   IF ( WRFU_IsInitialized() ) THEN
      DO I=1,SIZE(strs) 
        CALL WRFU_LogWrite( strs(i), WRFU_LOG_INFO )
     END DO
   ENDIF
#endif

END SUBROUTINE da_messages

