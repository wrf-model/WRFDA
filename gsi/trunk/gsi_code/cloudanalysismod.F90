module cloudanalysismod
!$$$   module documentation block
!                .      .    .                                       .
! module:  cloud analysis module
! prgmmr:  Ming Hu             org: GSD/AMB           date: 2008-06-04
!
! abstract: 
!      This module contains variables pertinent for cloud analysis
!
! program history log:
!   2008-06-03 Hu
! 
! Subroutines Included:
!   sub init_cloudanalysis  - initialize cloud analysis related variables to default values
!
! Variable Definitions:
!   def cloud_analysis    - namelist logical for cloud analysis (=true) 
!
! attributes:
!   language: f90
!   machine:  linux cluster (wjet)
!
!$$$ end documentation block

  logical cloud_analysis


contains

  subroutine init_cloudanalysis
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: init_cloudanalysis 
! prgmmr:  Ming Hu             org: GSD/AMB           date: 2008-06-04
!
! abstract:  set defaults for cloud analysis related variables
!
! program history log:
!   2008-06-03  Hu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  liunx cluster (Wjet)
!
!$$$
    implicit none

!   Set logical flag
    cloud_analysis = .false.   ! .true. = turn on GSD cloud analysis

    return
  end subroutine init_cloudanalysis

end module cloudanalysismod
