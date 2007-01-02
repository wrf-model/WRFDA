!WRF:MODEL_LAYER:bdyzone
!

MODULE module_bdyzone

!--set the bdyzone.  We are hardwiring this here and we'll
!--decide later where it should be set and stored

#ifdef DM_PARALLEL
   INTEGER, PARAMETER :: bdyzone = 2
#else
   INTEGER, PARAMETER :: bdyzone = 0
#endif

   INTEGER, PARAMETER :: bdyzone_x = bdyzone
   INTEGER, PARAMETER :: bdyzone_y = bdyzone

CONTAINS

  SUBROUTINE get_time_to_read_again ( newtime )
    IMPLICIT NONE
    REAL, INTENT(OUT) :: newtime
    newtime = 0.0
    RETURN
  END SUBROUTINE get_time_to_read_again

  SUBROUTINE set_time_to_read_again ( newtime )
    IMPLICIT NONE
    REAL, INTENT(IN) :: newtime
    RETURN
  END SUBROUTINE set_time_to_read_again

END MODULE module_bdyzone

SUBROUTINE get_bdyzone_x ( bzx )
  USE module_bdyzone
  IMPLICIT NONE
  INTEGER bzx
  bzx = bdyzone_x
  return
END SUBROUTINE get_bdyzone_x

SUBROUTINE get_bdyzone_y ( bzy)
  USE module_bdyzone
  IMPLICIT NONE
  INTEGER bzy
  bzy = bdyzone_y
  return
END SUBROUTINE get_bdyzone_y

SUBROUTINE get_bdyzone ( bz)
  USE module_bdyzone
  IMPLICIT NONE
  INTEGER bz
  bz = bdyzone
  return
END SUBROUTINE get_bdyzone

