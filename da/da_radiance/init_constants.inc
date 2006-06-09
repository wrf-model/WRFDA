subroutine init_constants(regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_constants       set regional or global constants
!     prgmmr:    treadon          org: np23           date: 2004-03-02
!
! abstract:  This routine sets constants specific to regional or global
!            applications of the gsi
!
! program history log:
!   2004-03-02  treadon
!   2004-06-16  treadon, documentation
!   2004-10-28  treadon - use intrinsic TINY function to set value
!                         for smallest machine representable positive
!                         number
!   2004-12-03  treadon - move derived constants to init_constants_derived
!   2005-03-03  treadon - add implicit none
!
!   input argument list:
!     regional - if .true., set regional gsi constants;
!                otherwise (.false.), use global constants
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none
    logical regional
    real(r_kind) reradius,g,r_d,r_v,cliq_wrf

!   Define regional constants here
    if (regional) then

!      Name given to WRF constants
       reradius = one/6370.e03_r_kind
       g        = 9.81_r_kind
       r_d      = 287.04_r_kind
       r_v      = 461.6_r_kind
       cliq_wrf = 4190.0_r_kind
       cp_mass  = 1004.67_r_kind

!      Transfer WRF constants into unified GSI constants
       rearth = one/reradius
       grav   = g
       rd     = r_d
       rv     = r_v
       cv     = cp-r_d
       cliq   = cliq_wrf

!   Define global constants here
    else
       rearth = 6.3712e+6_r_kind
       grav   = 9.80665e+0_r_kind
       rd     = 2.8705e+2_r_kind
       rv     = 4.6150e+2_r_kind
       cv     = 7.1760e+2_r_kind
       cliq   = 4.1855e+3_r_kind
       cp_mass= zero
    endif


!   Now define derived constants which depend on constants
!   which differ between global and regional applications.

!   Constants related to ozone assimilation
    ozcon = grav*21.4e-9_r_kind
    rozcon= one/ozcon

!   Constant used in vertical integral for precipitable water
    tpwcon = 100.0_r_kind/grav

!   Derived atmospheric constants
    fv         = rv/rd-one    ! used in virtual temperature equation 
    dldt       = cvap-cliq
    xa         = -(dldt/rv)
    xai        = -(dldti/rv)
    xb         = xa+hvap/(rv*ttp)
    xbi        = xai+hsub/(rv*ttp)
    eps        = rd/rv
    epsm1      = rd/rv-one
    omeps      = one-eps
    factor1    = (cvap-cliq)/rv
    factor2    = hvap/rv-factor1*t0c
    cpr        = cp*rd
    el2orc     = hvap*hvap/(rv*cp)
    rd_over_g  = rd/grav
    rd_over_cp = rd/cp
    g_over_rd  = grav/rd

    return
end subroutine init_constants
