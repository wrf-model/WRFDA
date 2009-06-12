
SUBROUTINE pcp_mxr (nx,ny,nz,t_3d,p_3d ,ref_3d                          &
           ,cldpcp_type_3d                                              &
           ,qr_3d,qs_3d,qh_3d,istatus )

!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  pcp_mxr calculate precipiation based on Kessler radar reflectivity equations
!
!   PRGMMR:                  ORG:                DATE:              
!
! ABSTRACT: 
!  This subroutine calculate precipiation based on Kessler radar reflectivity equations
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     nx          - no. of lons on subdomain (buffer points on ends)
!     ny          - no. of lats on subdomain (buffer points on ends)
!     nz          - no. of levels
!     t_3d        - 3D background potentional temperature (K)
!     p_3d        - 3D background pressure  (hPa)
!     ref_3d      - 3D reflectiviy in analysis grid (dBZ)
!     cldpcp_type_3d - 3D precipitation type
!
!   output argument list:
!     qr_3d       - rain mixing ration (g/kg)
!     qs_3d       - snow mixing ration (g/kg)
!     qh_3d       - hail mixing ration (g/kg)
!     istatus     -
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!   Old documents from CAPS
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Perform 3D precipitation mixing ratio (in g/kg) analysis using
!  radar reflectivity data. For rain water, using Kessler (1969)
!  formula:
!            qr(g/kg) = a*(rho*arg)**b                  (1)
!
!  Here arg = Z (mm**6/m**3), and dBZ = 10log10 (arg).
!  Coeffcients a=17300.0, and b=7/4.
!  rho represents the air density.
!
!  For snow and hail, using Rogers and Yau (1989) formula:
!
!            qs(g/kg) = c*(rho*arg)**d                  (2)
!
!  where, c=38000.0,  d=2.2
!
!
!-----------------------------------------------------------------------
!
!  AUTHOR: (Jian Zhang)
!  06/13/96
!
!  MODIFICATION HISTORY:
!  07/30/97 (J. Zhang)
!           Added precipitation type in the argument list so that
!           mixing ratios of different precip. types can be computed.
!  09/04/97 (J. Zhang)
!            Changed the radar echo thresholds for inserting precip.
!            from radar reflectivities.
!
!-----------------------------------------------------------------------
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
! 

!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_single,i_kind, r_kind
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  integer(i_kind),intent(in) :: nx,ny,nz              ! Model grid size
!
  REAL(r_kind),intent(in) :: ref_3d(nx,ny,nz)  ! radar reflectivity (dBZ)
  real(r_single),intent(in) :: t_3d(nx,ny,nz)  ! Temperature (deg. Kelvin)
  real(r_single),intent(in) :: p_3d(nx,ny,nz)  ! Pressure (Pascal)

  integer(i_kind),intent(in) :: cldpcp_type_3d(nx,ny,nz) ! cloud/precip type field
!
!  OUTPUT:
  INTEGER :: istatus
!
  REAL(r_single),intent(out) :: qr_3d(nx,ny,nz)     ! rain mixing ratio in (g/kg)
  REAL(r_single),intent(out) :: qs_3d(nx,ny,nz)     ! snow/sleet/frz-rain mixing ratio
                            ! in (g/kg)
  REAL(r_single),intent(out) :: qh_3d(nx,ny,nz)     ! hail mixing ratio in (g/kg)
!
!  LOCAL:
  REAL :: a,b,c,d                  ! Coef. for Z-qr relation.
  PARAMETER (a=17300.0, b=7.0/4.0)
  PARAMETER (c=38000.0, d=2.2)
  REAL :: rair                     ! Gas constant (J/deg/kg)
  PARAMETER (rair = 287.04)
  REAL :: thresh_ref
  PARAMETER (thresh_ref = 0.0)
  INTEGER :: pcptype
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  INTEGER :: i,j,k, iarg
  REAL :: arg,rhobar,br,dr
  PARAMETER (br=1.0/b, dr=1.0/d)
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!
!-----------------------------------------------------------------------
!
  istatus=0
!
!-----------------------------------------------------------------------
!
!  Compute the precip mixing ratio in g/kg from radar reflectivity
!  factor following Kessler (1969) or Rogers and Yau (1989).
!
!-----------------------------------------------------------------------
!
  DO k = 1,nz-1
    DO j = 2,ny-1
      DO i = 2,nx-1
        IF (ref_3d(i,j,k) >= thresh_ref) THEN    ! valid radar refl.
          rhobar = p_3d(i,j,k)/rair/t_3d(i,j,k)
          arg = 10.0**(0.1*ref_3d(i,j,k))
          iarg = cldpcp_type_3d(i,j,k)
          pcptype = iarg/16              ! precip. type

          IF (pcptype == 0) THEN       ! no precip
            PRINT*,'+++ NOTE: radar echo though no precip. +++'
          ELSE IF (pcptype == 1.OR.pcptype == 3) THEN   ! rain or Z R
            qr_3d(i,j,k) = (arg/a)**br/rhobar
          ELSE IF (pcptype == 2) THEN                   ! snow
            qs_3d(i,j,k) = (arg/c)**dr/rhobar
          ELSE IF (pcptype == 4.OR.pcptype == 5) THEN   ! hail or sleet
            qh_3d(i,j,k) = (arg/c)**dr/rhobar
          ELSE                                          ! unknown
            PRINT*,'+++ NOTE: unknown precip type. +++'
          END IF
        ELSE
          qr_3d(i,j,k) = 0.
          qs_3d(i,j,k) = 0.
          qh_3d(i,j,k) = 0.
        END IF
      END DO ! k
    END DO ! i
  END DO ! j
!
!-----------------------------------------------------------------------
!
  istatus = 1
!
  RETURN
END SUBROUTINE pcp_mxr

!
SUBROUTINE pcp_mxr_ferrier (nx,ny,nz,t_3d,p_3d ,ref_3d                  &
           ,cldpcp_type_3d                                              &
           ,qr_3d,qs_3d,qh_3d,istatus )
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  pcp_mxr calculate precipiation based on ferrier radar reflectivity equations
!
!   PRGMMR:                  ORG:                DATE:              
!
! ABSTRACT: 
!  This subroutine calculate precipiation based on ferrier radar reflectivity equations
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     nx          - no. of lons on subdomain (buffer points on ends)
!     ny          - no. of lats on subdomain (buffer points on ends)
!     nz          - no. of levels
!     t_3d        - 3D background potentional temperature (K)
!     p_3d        - 3D background pressure  (hPa)
!     ref_3d      - 3D reflectiviy in analysis grid (dBZ)
!     cldpcp_type_3d - 3D precipitation type
!
!   output argument list:
!     qr_3d       - rain mixing ration (g/kg)
!     qs_3d       - snow mixing ration (g/kg)
!     qh_3d       - hail mixing ration (g/kg)
!     istatus     -
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!   Old document from CAPS
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Perform 3D precipitation mixing ratio (in g/kg) analysis using
!  radar reflectivity data. For rain water, using Ferrier et al (1995)
!  formulation:
!
!
!     For rain water:
!
!          18
!        10   * 720                              1.75
!  Zer = --------------------------- * (rho * qr)
!          1.75      0.75       1.75
!        pi     * N0r     * rhor
!
!
!     For dry snow (t <= 0 C):
!
!
!          18           2                     0.25
!        10  * 720 * |K|                * rhos
!                       ice                                    1.75
!  Zes = ----------------------------------------- * (rho * qs)     t <= 0 C
!          1.75         2          0.75       2
!        pi        * |K|      * N0s     * rhoi
!                     water
!
!
!     For wet snow (t >= 0 C):
!
!
!              18
!            10   * 720                                 1.75
!  Zes =     ----------------------------   * (rho * qs)            t >  0 C
!              1.75      0.75       1.75
!            pi     * N0s     * rhos
!
!
!     For hail water:
!
!
!          /   18                       \ 0.95
!         /  10   * 720                  \              1.6625
!  Zeh = |   ---------------------------- | * (rho * qh)
!         \    1.75      0.75       1.75 /
!          \ pi     * N0h     * rhoh    /
!
!  Here Zx (mm**6/m**3, x=r,s,h), and dBZ = 10log10 (Zx).
!  rho represents the air density, rhor,rhos,rhoh are the density of
!  rain, snow and hail respectively. Other variables are all constants
!  for this scheme, see below.
!
!
!-----------------------------------------------------------------------
!
!  AUTHOR: (Donghai Wang and Eric Kemp)
!  07/20/2000
!
!  MODIFICATION HISTORY:
!
!  11/09/2000 Keith Brewster
!  Moved some parameters with real-valued exponentiation to be
!  computed at runtime due to compiler complaint.
!
!  04/07/2003 Keith Brewster
!  Restructured code to make more tractable.and consistent with 
!  the reflec_ferrier subroutine.
!
!-----------------------------------------------------------------------
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
! 


!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_single,i_kind, r_kind
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  INTEGER :: nx,ny,nz              ! Model grid size
!
  REAL(r_kind),intent(in) :: ref_3d(nx,ny,nz)  ! radar reflectivity (dBZ)
  REAL :: t_3d(nx,ny,nz)  ! Temperature (deg. Kelvin)
  REAL :: p_3d(nx,ny,nz)  ! Pressure (Pascal)

  INTEGER:: cldpcp_type_3d(nx,ny,nz) ! cloud/precip type field
!
!  OUTPUT:
  INTEGER :: istatus
!
  REAL(r_single) :: qr_3d(nx,ny,nz)     ! rain mixing ratio in (g/kg)
  REAL(r_single) :: qs_3d(nx,ny,nz)     ! snow/sleet/frz-rain mixing ratio
                            ! in (g/kg)
  REAL(r_single) :: qh_3d(nx,ny,nz)     ! hail mixing ratio in (g/kg)
!

  REAL,PARAMETER :: ki2 = 0.176 ! Dielectric factor for ice if other
                                !   than melted drop diameters are used.
  REAL,PARAMETER :: kw2=0.93 ! Dielectric factor for water.

  REAL,PARAMETER :: m3todBZ=1.0E+18 ! Conversion factor from m**3 to
                                    !   mm**6 m**-3.
  REAL,PARAMETER :: Zefact=720.0 ! Multiplier for Ze components.
  REAL,PARAMETER :: lg10div=0.10 ! Log10 multiplier (1/10)

  REAL,PARAMETER :: pi=3.1415926 ! Pi.
  REAL,PARAMETER :: N0r=8.0E+06 ! Intercept parameter in 1/(m^4) for rain.
  REAL,PARAMETER :: N0s=3.0E+06 ! Intercept parameter in 1/(m^4) for snow.
  REAL,PARAMETER :: N0h=4.0E+04 ! Intercept parameter in 1/(m^4) for hail.

  REAL,PARAMETER :: N0xpowf=3.0/7.0 ! Power to which N0r,N0s & N0h are
                                    !   raised.
  REAL,PARAMETER :: K2powf=4.0/7.0  ! Power to which K-squared 
                                    !  of ice, water are raised
  REAL,PARAMETER :: zkpowf=4.0/7.0  ! Power to which Zk is raised
  REAL,PARAMETER :: zepowf=4.0/7.0  ! Power to which Ze is raised
  REAL,PARAMETER :: zehpowf=(4.0/7.0)*1.0526  ! Power to which Zeh is raised

  REAL,PARAMETER :: rhoi=917.  ! Density of ice (kg m**-3)
  REAL,PARAMETER :: rhor=1000. ! Density of rain (kg m**-3)
  REAL,PARAMETER :: rhos=100.  ! Density of snow (kg m**-3)
  REAL,PARAMETER :: rhoh=913.  ! Density of hail (kg m**-3)

  REAL,PARAMETER :: rhoipowf=8.0/7.0  ! Power to which rhoi is raised.
  REAL,PARAMETER :: rhospowf=1.0/7.0  ! Power to which rhos is raised.
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  REAL :: thresh_ref
  PARAMETER (thresh_ref = 0.0)
  INTEGER :: i,j,k, iarg
  INTEGER :: pcptype
  REAL :: zkconst,zerf,zesnegf,zesposf,zehf,rfract
  REAL :: ze,zer,zeh,zes,rho,tc
!
!-----------------------------------------------------------------------
!
! Include Files
!
!-----------------------------------------------------------------------
!
!  INCLUDE 'phycst.inc'
!  INCLUDE 'arpsphycst.inc'

  REAL :: rd        ! Gas constant for dry air  (m**2/(s**2*K))
  PARAMETER( rd     = 287.0 )

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!
!-----------------------------------------------------------------------
!
!  Intiailize constant factors in the Ze terms for rain, snow and hail,
!  respectively, in Ferrier.
!
!  These are the inverse of those presented in the reflec_ferrier function.
!
!-----------------------------------------------------------------------
!
  istatus=0

  zkconst = (Zefact*m3todBZ) ** zkpowf

  zerf=1000.*(pi * (N0r**N0xpowf) * rhor )/zkconst

  zesnegf=1000.*(pi*(kw2**k2powf)*(N0s**N0xpowf)*(rhoi**rhoipowf)) /    &
          ( zkconst * (ki2**k2powf) * (rhos**rhospowf) )
          
  zesposf=1000.*( pi * (N0s**N0xpowf) * rhos) / zkconst
 
  zehf=1000.*( pi * (N0h**N0xpowf) * rhoh) / zkconst

!-----------------------------------------------------------------------
!
!  Compute the precip mixing ratio in g/kg from radar reflectivity
!  factor following Ferrier et al (1995).
!
!-----------------------------------------------------------------------
!

  DO k = 2,nz-1
    DO j = 2,ny-1
      DO i = 2,nx-1
        IF (ref_3d(i,j,k) >= thresh_ref) THEN    ! valid radar refl.
          rho = p_3d(i,j,k)/(rd*t_3d(i,j,k))
          ze = 10.0**(0.1*ref_3d(i,j,k))
          iarg = cldpcp_type_3d(i,j,k)
          pcptype = iarg/16              ! precip. type
          tc = t_3d(i,j,k) - 273.15
          IF (pcptype == 1) THEN   ! rain
            qr_3d(i,j,k) = zerf * (ze**zepowf) / rho
          ELSE IF (pcptype == 2) THEN                   ! snow
            IF (tc <= 0.0) THEN     !dry snow
              qs_3d(i,j,k) = zesnegf * (ze**zepowf) / rho
            ELSE IF (tc < 5.0) THEN     !wet snow
              rfract=0.20*tc
              zer=rfract*ze
              zes=(1.-rfract)*ze
              qs_3d(i,j,k) = zesposf * (zes**zepowf) / rho
              qr_3d(i,j,k) = zerf * (zer**zepowf) / rho
            ELSE
              qr_3d(i,j,k) = zerf * (ze**zepowf) / rho
            END IF
          ELSE IF (pcptype == 3) THEN   ! ZR
            qr_3d(i,j,k) = zerf * (ze**zepowf) / rho
          ELSE IF (pcptype == 4) THEN   ! sleet
            IF (tc <= 0.0) THEN     ! hail category
              qh_3d(i,j,k) = zehf * (ze**zehpowf) / rho
            ELSE IF( tc < 10. ) THEN
              rfract=0.10*tc
              zer=rfract*ze
              zeh=(1.-rfract)*ze
              qr_3d(i,j,k) = zerf * (zer**zepowf) / rho
              qh_3d(i,j,k) = zehf * (zeh**zehpowf) / rho
            ELSE
              qr_3d(i,j,k) = zerf * (ze**zepowf) / rho
            END IF
          ELSE IF (pcptype == 5) THEN   ! hail
            qh_3d(i,j,k) = zehf * (ze**zehpowf) / rho
          ELSE                                          ! unknown
            IF (tc <= 0.0) THEN     !dry snow
              qs_3d(i,j,k) = zesnegf * (ze**zepowf) / rho
            ELSE IF ( tc < 5.0 ) THEN     !wet snow
              rfract=0.20*tc
              zer=rfract*ze
              zes=(1.-rfract)*ze
              qs_3d(i,j,k) = zesposf * (zes**zepowf) / rho
              qr_3d(i,j,k) = zerf * (zer**zepowf) / rho
            ELSE ! rain
              qr_3d(i,j,k) = zerf * (ze**zepowf) / rho
            END IF
          END IF
        ELSE
          qr_3d(i,j,k) = -999.
          qs_3d(i,j,k) = -999.
          qh_3d(i,j,k) = -999.
        END IF
      END DO ! k
    END DO ! i
  END DO ! j
!  PRINT*,'Finish Ferrier ...'
!
!-----------------------------------------------------------------------
!
  istatus = 1
!
  RETURN
END SUBROUTINE pcp_mxr_ferrier
