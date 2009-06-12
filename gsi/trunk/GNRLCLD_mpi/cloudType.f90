SUBROUTINE cloudType(nlat,nlon,nsig,h_bk,t_bk,p_bk,radar_3d,       &
                 cld_cover_3d,cld_type_3d,wthr_type,cloudlayers_i)
!
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloudType      decide cloud type                             
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-20
!
! ABSTRACT: 
!  This subroutine decide cloud type  
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!   input argument list:
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     h_bk        - 3D background height  
!     t_bk        - 3D background potentional temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     radar_3d    - 3D radar reflectivity in analysis grid (dBZ)
!
!     cld_cover_3d- 3D cloud cover
!     wthr_type   - 3D weather type
!     cloudlayers_i - 3D cloud layer index
!
!   output argument list:
!     cld_type_3d - 3D cloud type
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

  use constants, only: rd_over_cp, h1000
  use kinds, only: r_single,i_kind,r_kind

  implicit none
  integer(i_kind),INTENT(IN) ::  nlat,nlon,nsig
!
!  surface observation
!
!
!  background
!
  real(r_single),INTENT(IN) :: h_bk(nlon,nlat,nsig)   ! height
  real(r_single),INTENT(IN) :: t_bk(nlon,nlat,nsig)   ! temperature
  real(r_single),INTENT(IN) :: p_bk(nlon,nlat,nsig)   ! pressure
!
! observation
!
  real(r_kind),INTENT(IN) :: radar_3d(nlon,nlat,nsig)   ! reflectivity
!
!
!  Variables for cloud analysis
!
  real (r_single),INTENT(IN) :: cld_cover_3d(nlon,nlat,nsig)
  integer(i_kind),INTENT(OUT) :: cld_type_3d(nlon,nlat,nsig)
  integer(i_kind),INTENT(IN) :: wthr_type(nlon,nlat)
!
!  cloud layers
!
  integer(i_kind) :: cloudlayers_i(nlon,nlat,21)  ! 5 =different layers
!                                      1= the number of layers
!                                      2,4,... bottom
!                                      3,5,... top
!
! cloud water and cloud ice
!
!-----------------------------------------------------------
!
! temp.
!
  INTEGER :: i,j,k,ilvl,nlvl
  INTEGER :: itype
  INTEGER :: kb,kt,k1
  real :: cld_base_m, cld_top_m

  real (r_single) :: zs_1d(nsig)
  real (r_single) :: dte_dz_1d(nsig)
  real (r_single) :: t_1d(nsig)
  real (r_single) :: p_pa_1d(nsig)
  real (r_single) :: p_mb_1d(nsig)
  real (r_single) :: tem1(nsig)
!
  CHARACTER (LEN=2) :: c2_type
!
!====================================================================
!  Begin
!
!-----------------------------------------------------------------------
!
!  Find Cloud Layers and Computing Output Field(s)
!  The procedure works column by column.
!
!-----------------------------------------------------------------------
!

  DO j = 2,nlat-1
  DO i = 2,nlon-1
!
    DO k = 1,nsig                      ! Initialize
      t_1d(k) = t_bk(i,j,k)*(p_bk(i,j,k)/h1000)**rd_over_cp
      zs_1d(k) = h_bk(i,j,k)
      p_pa_1d(k) = p_bk(i,j,k)*100.0
      p_mb_1d(k) = p_bk(i,j,k)
    END DO
!-----------------------------------------------------------------------
    nlvl=cloudlayers_i(i,j,1)
    if(nlvl > 0 ) then
      DO ilvl = 1, nlvl          ! loop through cloud layers
        kb=cloudlayers_i(i,j,2*ilvl)
        kt=cloudlayers_i(i,j,2*ilvl+1)

        CALL get_stability (nsig,t_1d,zs_1d,p_mb_1d             &
                        ,kb,kt,dte_dz_1d,tem1)

        cld_base_m = 0.5 * (zs_1d(kb-1) + zs_1d(kb))
        cld_top_m = 0.5 * (zs_1d(kt) + zs_1d(kt+1))
        DO k1 = kb,kt
          CALL get_cloudtype(t_1d(k1),dte_dz_1d(k1)           &
                  ,cld_base_m,cld_top_m,itype,c2_type)
!
          IF(radar_3d(i,j,k1) > 45.) THEN
            itype = 10 ! CB
          END IF

          cld_type_3d(i,j,k1) = itype
        END DO  !k1
      enddo   ! ilvl
    endif  ! nlvl > 0 

  ENDDO  ! i
  ENDDO  ! j

END SUBROUTINE cloudType

