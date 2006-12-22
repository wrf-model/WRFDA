subroutine da_intpsfc_prs (val, ho, po, hm, tm, qm, to, qo)
!------------------------------------------------------------------------------!
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
!------------------------------------------------------------------------------!

      implicit none

      real, intent (out)   :: val
      real, intent (in)    :: ho, po
      real, intent (in), optional :: to, qo
      real, intent (in)    :: hm, tm, qm    
      real                 :: tvo, tvm, tv, dz, arg

!------------------------------------------------------------------------------!

! 1.  MODEL AND OBSERVATION VIRTUAL TEMPERATURE
! ---------------------------------------------

      tvm = tm  * (1. + 0.608 * qm)
      if (present(to) .and. present(qo)) then
        tvo = to  * (1. + 0.608 * qo)
      else if (present(to) .and. .not.present(qo)) then
        tvo = to
      else
        tvo = tvm
      end if

      tv  = 0.5 * (tvm + tvo)

! 2. HEIGHT DifFERENCE BEWTEEN MODEL SURFACE AND OBSERVATIONS
! ------------------------------------------------------------

      dz = hm - ho

! 3.  EXTRAPOLATE PRESSURE OBS TO MODEL SURFACE
! ---------------------------------------------

      arg = dz * gravity / gas_constant
      arg = arg    / tv 

      val = po * exp (-arg)

end subroutine da_intpsfc_prs


