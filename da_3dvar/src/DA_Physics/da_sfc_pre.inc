SUBROUTINE da_sfc_pre (psfcm, psm, tsm, qsm, hsm, ho, to, qvo)
!-----------------------------------------------------------------------------!
!
! Correct pressure between two levels. 
!
! Reference: make use of the hydrosatic equation:
!
!  P2 = P1 * exp [-G/R * (z2-z1) / (tv1 + tv2)/2)
!
! Where:
!  z1  = height at level 1
!  z1  = height at level 2
!  tv1 = temperature at level 1
!  tv2 = temperature at level 2
!  P1  = Pressure at level 1
!  P2  = Pressure at level 2
!-----------------------------------------------------------------------------!

      IMPLICIT NONE

      REAL, INTENT (out)   :: psfcm   ! model pressure at ho
      REAL, INTENT (in)    :: psm, tsm, qsm

      REAL, INTENT (in)           :: hsm, ho
      REAL, INTENT (in), OPTIONAL :: to, qvo
    
      REAL                 :: tvo, tvsm, tv, dz, arg0, arg

      REAL, PARAMETER      :: GASR =  gas_constant
      REAL, PARAMETER      :: G = gravity

!-----------------------------------------------------------------------------!

! 1.  MODEL AND OBSERVATION VIRTUAL TEMPERATURE
! ---------------------------------------------

      tvsm = tsm  * (1. + 0.608 * qsm)
      if (present(to) .and. present(qvo)) then
        tvo = to  * (1. + 0.608 * qvo)
      else if (present(to) .and. .not.present(qvo)) then
        tvo = to
      else
        tvo = tvsm
      endif

      tv  = 0.5 * (tvsm + tvo)

! 2. HEIGHT DIFFERENCE BEWTEEN MODEL SURFACE AND OBSERVATIONS
! ------------------------------------------------------------

      dz = hsm - ho
      arg0 = dz * g / gasr

! 3.  EXTRAPOLATE PRESSURE OBS TO MODEL SURFACE
! ---------------------------------------------

      arg = arg0    / tv 

      psfcm = psm * exp (arg)

END SUBROUTINE da_sfc_pre

