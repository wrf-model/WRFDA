! Stubs John Bray wrote to get things to compile

MODULE JRB_Stubs

#include "ESMF_TimeMgr.inc"

USE ESMF_ClockMod
USE ESMF_TimeMod
USE ESMF_AlarmMod
USE ESMF_BaseMod
USE ESMF_Stubs

!   TYPE ESMF_GridComp
!      SEQUENCE
!      INTEGER :: dummy
!   END TYPE

!   TYPE ESMF_State
!      SEQUENCE
!      INTEGER :: dummy
!   END TYPE

  INTEGER :: dyn_em_sn=1
  INTEGER :: dyn_em_tl=2
  INTEGER :: dyn_em_ad=3
  INTEGER :: dyn_em_tst=4

CONTAINS

FUNCTION ESMF_ClockIsStartTime(clock, rc)

  LOGICAL :: ESMF_ClockIsStartTime
  TYPE(ESMF_Clock), INTENT(IN) :: clock
  INTEGER, INTENT(OUT) :: rc

  ESMF_ClockIsStartTime = .FALSE.

END FUNCTION ESMF_ClockIsStartTime
  
SUBROUTINE ESMF_ClockBack(clock, RingingAlarmList, &
                                NumRingingAlarms, rc)


  TYPE(ESMF_Clock), INTENT(INOUT) :: clock
  TYPE(ESMF_Alarm), DIMENSION(MAX_ALARMS), intent(out), optional :: &
                                        RingingAlarmList
  INTEGER, INTENT(OUT), OPTIONAl :: NumRingingAlarms
  INTEGER, INTENT(OUT) :: rc

END SUBROUTINE ESMF_ClockBack

END MODULE JRB_Stubs
