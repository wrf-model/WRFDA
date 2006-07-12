SUBROUTINE da_messages2(strs )
#ifdef USE_LOGERR
  USE module_utility
#endif
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: strs(:)
  INTEGER :: i
  DO I=1,SIZE(strs) 
    write(0,*) strs(i)
  END DO
!TBH:  work around deficiencies in build-automation
#ifdef USE_LOGERR
  IF ( WRFU_IsInitialized() ) THEN
     DO I=1,SIZE(strs) 
      CALL WRFU_LogWrite( strs(i), WRFU_LOG_INFO )
    END DO
  ENDIF
#endif
END SUBROUTINE da_messages2


