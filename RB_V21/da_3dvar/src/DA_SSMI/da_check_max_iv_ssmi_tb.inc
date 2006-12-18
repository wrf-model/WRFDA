SUBROUTINE da_check_max_iv_ssmi_tb(it, ssmi_tb, &
                                   itb19v, itb19vf, itb19h, itb19hf, &
                                   itb22v, itb22vf,                  &
                                   itb37v, itb37vf, itb37h, itb37hf, &
                                   itb85v, itb85vf, itb85h, itb85hf  )

   IMPLICIT NONE

   INTEGER,             INTENT(IN)        :: it
   TYPE (ssmi_tb_type), INTENT(INOUT)     :: ssmi_tb

   INTEGER,    INTENT (INOUT)             :: itb19h, itb19v, itb22v
   INTEGER,    INTENT (INOUT)             :: itb37h, itb37v, itb85h, itb85v
   INTEGER,    INTENT (INOUT)             :: itb19hf, itb19vf, itb22vf 
   INTEGER,    INTENT (INOUT)             :: itb37hf, itb37vf, itb85hf, itb85vf

   INTEGER                                :: iunit ! Output unit.
   LOGICAL                                :: failed, print

!-----------------------------------------------------------------------------!
!  [1.0] Open diagnostic file:
!-----------------------------------------------------------------------------!

   iunit = check_max_iv_unit
   print = check_max_iv_print

   IF ( print )THEN
      WRITE (iunit,'(A)')  &
      '----------------------------------------------------------------'
      WRITE (UNIT = iunit, FMT = '(A,/)') 'MAX ERROR TEST QC:'
      WRITE (UNIT = iunit, FMT = '(/,9(A,F3.0,/))')  &
     'Error max test ratio for SSMI_tb_TB19h = ',max_error_tb, &
     'Error max test ratio for SSMI_tb_TB19v = ',max_error_tb, &
     'Error max test ratio for SSMI_tb_TB22v = ',max_error_tb, &
     'Error max test ratio for SSMI_tb_TB37h = ',max_error_tb, &
     'Error max test ratio for SSMI_tb_TB37v = ',max_error_tb, &
     'Error max test ratio for SSMI_tb_TB85h = ',max_error_tb, &
     'Error max test ratio for SSMI_tb_TB85v = ',max_error_tb
   ENDIF

!  itb19h  = 0; itb19v  = 0; itb22v  = 0; 
!  itb37h  = 0; itb37v  = 0; itb85h  = 0; itb85v  = 0
!  itb19hf = 0; itb19vf = 0; itb22vf = 0;
!  itb37hf = 0; itb37vf = 0; itb85hf = 0; itb85vf = 0

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

!  failed=.false. is defined inside the da_max_error_qc 
!  failed = .FALSE.

!  Tb19h

   CALL da_max_error_qc(it, ssmi_tb % info, ssmi_tb % tb19h,                  &
                            max_error_tb, itb19h, itb19hf, iunit, 'TB19h', &
                            failed, print)

!  Tb19v

   CALL da_max_error_qc(it, ssmi_tb % info, ssmi_tb % tb19v,                  &
                            max_error_tb, itb19v, itb19vf, iunit, 'TB19v', &
                            failed, print)

!  Tb22v

   CALL da_max_error_qc(it, ssmi_tb % info, ssmi_tb % tb22v,                  &
                            max_error_tb, itb22v, itb22vf, iunit, 'TB22v', &
                            failed, print)

!  Tb37h

   CALL da_max_error_qc(it, ssmi_tb % info, ssmi_tb % tb37h,                  &
                            max_error_tb, itb37h, itb37hf, iunit, 'TB37h', &
                            failed, print)

!  Tb37v

   CALL da_max_error_qc(it, ssmi_tb % info, ssmi_tb % tb37v,                  &
                            max_error_tb, itb37v, itb37vf, iunit, 'TB37v', &
                            failed, print)

!  Tb85h

   CALL da_max_error_qc(it, ssmi_tb % info, ssmi_tb % tb85h,                  &
                            max_error_tb, itb85h, itb85hf, iunit, 'TB85h', &
                            failed, print)

!  Tb85v

   CALL da_max_error_qc(it, ssmi_tb % info, ssmi_tb % tb85v,                  &
                            max_error_tb, itb85v, itb85vf, iunit, 'TB85v', &
                            failed, print)



!-----------------------------------------------------------------------------!
!  [3.0] Output test failed results:
!-----------------------------------------------------------------------------!

!  IF ( print .and. failed) THEN
!     WRITE (UNIT = iunit, FMT = '(/,10(2(A,I6),/))') &
!     'Number of failed ssmi tb19h observations:  ',itb19hf,' on ',itb19h, &
!     'Number of failed ssmi tb19v observations:  ',itb19vf,' on ',itb19v, &
!     'Number of failed ssmi tb22v observations:  ',itb22vf,' on ',itb22v, &
!     'Number of failed ssmi tb37h observations:  ',itb37hf,' on ',itb37h, &
!     'Number of failed ssmi tb37v observations:  ',itb37vf,' on ',itb37v, &
!     'Number of failed ssmi tb85h observations:  ',itb85hf,' on ',itb85h, &
!     'Number of failed ssmi tb85v observations:  ',itb85vf,' on ',itb85v
!  ENDIF

END SUBROUTINE da_check_max_iv_ssmi_tb
