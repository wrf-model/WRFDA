!						***************************
!						*  module_cpt_constd.f90  *
!						*  PURSER 2001            *
! 						***************************
!MODULE MODULE_cpt_constd
!---------------------------------------------------------------------------
! Constants required by various "compact" numerical schemes on uniform grids
! These are all initialized by a call to subroutine set_cpt_consts
! These are for real(8) precision; 
! for single precision, use module_cpt_consts.
!---------------------------------------------------------------------------
!                                        & ! Coefficients for...
!     b4m,l4m1,                          & ! 4th-order midpoint interpolation 
!     b6m,k6m1,l6m1,                     & ! 6th-order midpoint interpolation
!     b8m,k8m1,l8m1,l8m2,                & ! 8th-order midpoint interpolation
!     b10m,k10m1,k10m2,l10m1,l10m2,      & ! 10th-ordr midpoint interpolation
!     b4d,b4d1,l4d1,                     & ! 4th-order same-grid differencing
!     b8d,b8d1,b8d2,k8d1,l8d1,l8d2,      & ! 8th-order same-grid differencing
!     b4qi,b4q,l4q1,                     & ! 4th-order invertible quadrature
!     b6qi,b6q,k6q1,l6q1,                & ! 6th-order invertible quadrature
!     b8qi,b8q,k8q1,l8q1,l8q2,           & ! 8th-order invertible quadrature
!     b10qi,b10q,k10q1,k10q2,l10q1,l10q2,& ! 10th-ordr invertible quadrature
!     l4t1,k4t1,                         & ! 4th-order invertible translation
!     l8t1,l8t2,k8t1,k8t2                  ! 8th-order invertible translation
!---------------------------------------------------------------------------
implicit none
REAL(8) ::                                                 &
     b4m_d,l4m1_d,                                         &   
     b6m_d,k6m1_d,l6m1_d,                                  &
     b8m_d,k8m1_d,l8m1_d,l8m2_d,                           &
     b10m_d,k10m1_d,k10m2_d,l10m1_d,l10m2_d,               &
     b4d_d,b4d1_d,l4d1_d,                                  &
     b8d_d,b8d1_d,b8d2_d,k8d1_d,l8d1_d,l8d2_d,             &
     b4qi_d,b4q_d,l4q1_d,                                  &
     b6qi_d,b6q_d,k6q1_d,l6q1_d,                           &
     b8qi_d,b8q_d,k8q1_d,l8q1_d,l8q2_d,                    &
     b10qi_d,b10q_d,k10q1_d,k10q2_d,l10q1_d,l10q2_d,       &
     l4t1_d,k4t1_d,                                        &
     l8t1_d,l8t2_d,k8t1_d,k8t2_d

INTEGER ::                                                 &
     fl4md,             fl6md,       fl8md,        fl10md, &
     fl4dd,                          fl8dd,                &
     fl4qd,       fk6qd,fl6qd, fk8qd,fl8qd, fk10qd,fl10qd, &
     fk4td,fl4td,              fk8td,fl8td
     
!END MODULE MODULE_cpt_constd

