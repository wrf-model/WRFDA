SUBROUTINE da_sfc_pre_lin (psfcm_prime, psm_prime, tsm_prime, qsm_prime, &
                                        psm, tsm, qsm, hsm, ho, to, qvo)
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

!  .. Perturbation:
      REAL, INTENT (out)   :: psfcm_prime          ! model pressure at ho
      REAL, INTENT (in)    :: psm_prime, tsm_prime, &
                              qsm_prime            ! model surface p, t, q 
!  .. Basic state:
      REAL, INTENT (in)    :: psm, tsm, qsm ! model pressure at ho and
                                                   ! model surface p, t, q 
! .. Constant variables:
      REAL, INTENT (in)           :: hsm, ho
      REAL, INTENT (in), OPTIONAL :: to, qvo
! .. working array:
      REAL                 :: tvo, tvsm, tv, dz, arg0
      REAL                 :: tvsm_prime, tvo_prime, tv_prime, arg, arg_prime

      REAL, PARAMETER      :: GASR =  gas_constant
      REAL, PARAMETER      :: G = gravity

!-----------------------------------------------------------------------------!

! 1.  MODEL AND OBSERVATION VIRTUAL TEMPERATURE
! ---------------------------------------------

      tvsm_prime = tsm_prime * (1. + 0.608 * qsm) &
                 + qsm_prime * tsm * 0.608
      tvsm = tsm  * (1. + 0.608 * qsm)

      if (present(to) .and. present(qvo)) then
        tvo_prime = 0.0
        tvo = to  * (1. + 0.608 * qvo)
      else if (present(to) .and. .not.present(qvo)) then
        tvo_prime = 0.0
        tvo = to
      else
        tvo_prime = tvsm_prime
        tvo = tvsm
      endif
      
      tv_prime = 0.5 * (tvsm_prime + tvo_prime)
      tv  = 0.5 * (tvsm + tvo)

! 2. HEIGHT DIFFERENCE BEWTEEN MODEL SURFACE AND OBSERVATIONS
! ------------------------------------------------------------

      dz = hsm - ho
      arg0 = dz * g / gasr

! 3.  EXTRAPOLATE PRESSURE OBS TO MODEL SURFACE
! ---------------------------------------------

      arg_prime = - arg0 * tv_prime / (tv * tv)
      arg = arg0    / tv 

      psfcm_prime = exp(arg) *(psm_prime + psm * arg_prime)

END SUBROUTINE da_sfc_pre_lin

