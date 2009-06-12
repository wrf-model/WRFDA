SUBROUTINE cloudCover_radar(mype,nlat,nlon,nsig,h_bk,zh,z_lcl, &
                        grid_ref, &
                        cld_cover_3d,cld_type_3d,wthr_type)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloudCover_radar  cloud cover analysis using radar reflectivity
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-10
!
! ABSTRACT: 
!  This subroutine find cloud cover using radar reflectivity
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mype        - processor ID
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     h_bk        - 3D background height  
!     zh          - terrain
!     z_lcl       - lifting condensation level
!     grid_ref    - radar reflectivity in analysis grid
!
!   output argument list:
!     cld_cover_3d- 3D cloud cover
!     cld_type_3d - 3D cloud type
!     wthr_type   - 3D weather type
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
  use constants, only: deg2rad, rad2deg, pi
  use kinds, only: r_single,i_kind,r_kind
  use gridmod, only: regional_time

  implicit none

  integer(i_kind),intent(in):: mype
  integer(i_kind),intent(in):: nlat,nlon,nsig
!
!  surface observation
!
!
!  background
!
  real(r_single),intent(in) :: zh(nlon,nlat)           ! terrain
  real(r_single),intent(in) :: h_bk(nlon,nlat,nsig+1)   ! height
!
!
!  Variables for cloud analysis
!
  real (r_single),intent(inout) :: cld_cover_3d(nlon,nlat,nsig)
  integer(i_kind),intent(inout) :: cld_type_3d(nlon,nlat,nsig)
  integer(i_kind),intent(inout) :: wthr_type(nlon,nlat)
!
! Observation
!
  real(r_kind),intent(in) :: grid_ref(nlon,nlat,nsig)
!
  REAL :: ref_base1         ! "significant" radar echo at lower levels
  REAL :: ref_base2         ! "significant" radar echo at upper levels
  REAL :: hgt_thresh_ref    ! height criteria for "significant" radar
                            ! echo thresholds
!
  REAL :: cloud_base(nlon,nlat)
  REAL :: cloud_base_buf(nlon,nlat) ! Lowest SAO/IR base within search radius
!
  real (r_single) :: z_lcl(nlon,nlat)
!
!-----------------------------------------------------------
!
! temp.
!
  INTEGER :: i,j,k,k1

  REAL :: radar_cover
  PARAMETER(radar_cover=1.0)
  REAL :: thresh_cvr   ! lower radar echo threshold for cloud filling
  PARAMETER (thresh_cvr = 0.2)

  REAL :: unlimited_ceiling 

  INTEGER :: icount_below   ! tot # of rdr echo pts below cloud base
  INTEGER :: isearch_base   ! # of neighb cloud bases being succes. found
  INTEGER :: insert_count_tot  ! tot # of data pts inserted the radar_cover

  INTEGER :: i_out_bnd, i_lowecho
  REAL :: zs_1d(nsig)

  INTEGER :: kp1
  INTEGER :: icount_radar_lvl,insert_count_lvl
  REAL :: ref_thresh

  LOGICAL :: l_below_base
  LOGICAL :: l_unresolved(nlon,nlat)

!
!====================================================================
!  Begin
!
   ref_base1 = 15.0
   ref_base2 = 10.0
   hgt_thresh_ref = 2000.0
!
!-----------------------------------------------------------------------
!
!  Calculate Cloud Bases
!
!-----------------------------------------------------------------------
  unlimited_ceiling = 200000.

  DO j = 2,nlat-1
    DO i = 2,nlon-1
      cloud_base(i,j) = unlimited_ceiling
!
      DO k = nsig-1,1,-1
        IF(cld_cover_3d(i,j,k) < thresh_cvr .AND.    &
           cld_cover_3d(i,j,k+1) >= thresh_cvr) THEN
             cloud_base(i,j) = 0.5 * (h_bk(i,j,k) + h_bk(i,j,k+1))
        END IF
      END DO ! k
!
      IF (cloud_base(i,j) > z_lcl(i,j)) THEN
        cloud_base(i,j) = z_lcl(i,j)             ! using lcl
      END IF

      cloud_base_buf(i,j) = cloud_base(i,j)
      l_unresolved(i,j) = .false.

    END DO ! i
  END DO ! j

!
  icount_below = 0     ! tot # of rdr echo pts below cloud base
  isearch_base = 0     ! # of neighb cloud bases being succes. found
  insert_count_tot = 0 ! tot # of data pts inserted the radar_cover

!
!-----------------------------------------------------------------------
!
!  Essentially, this go downward to detect radar tops in time
!  to search for a new cloud base
!
!-----------------------------------------------------------------------
!
  i_out_bnd=0
  i_lowecho=0

  DO i = 2,nlon-1
    DO j = 2,nlat-1

      DO k=1,nsig
        zs_1d(k) = h_bk(i,j,k)
      END DO

      icount_radar_lvl = 0 ! # of lvls with refl > ref_thresh
      insert_count_lvl = 0 ! # of cloud lvls inserted radar_cover

      DO k = nsig-1,1,-1
        kp1 = MIN(k+1,nsig)
!
!-----------------------------------------------------------------------
!
!  Define the "significant" reflectivity value based on height (to
!  eliminate ground clutters and/or other non-weather radar echoes).
!
!-----------------------------------------------------------------------

        IF ((zs_1d(k)-zh(i,j)) <= hgt_thresh_ref) THEN
          ref_thresh = ref_base1
        ELSE
          ref_thresh = ref_base2
        END IF

        IF(grid_ref(i,j,k) > ref_thresh) THEN
          icount_radar_lvl = icount_radar_lvl + 1

          l_below_base = .false.

!
!-----------------------------------------------------------------------
!  Test if we are at echo top
!-----------------------------------------------------------------------
!
          IF(k == nsig-1 .OR. grid_ref(i,j,kp1) < ref_thresh) THEN
!
!-----------------------------------------------------------------------
!  Test if we are below the cloud base.
!-----------------------------------------------------------------------
!
            IF(zs_1d(k) < cloud_base_buf(i,j)) THEN
!
!-----------------------------------------------------------------------
!
!  Radar Echo Top is below the analyzed cloud base.
!  Using the lifting condensation level as the modified cloud base
!  if it is lower than the current buffer value.
!
!-----------------------------------------------------------------------
!
              cloud_base_buf(i,j)=MIN(z_lcl(i,j),cloud_base_buf(i,j))

              IF(cloud_base_buf(i,j) < zs_1d(k)) THEN
                isearch_base = isearch_base + 1
!                IF(isearch_base < 50) THEN ! limit log output
!                  WRITE(6,71) i,j,k,zs_1d(k),cloud_base(i,j)           &
!                  ,cloud_base_buf(i,j)
!71                 FORMAT(' Rdr Top < Bse*gp=',3I3,' zs=',f7.0          &
!                   & ,' cld_b=',f7.0,'lcl=',f7.0,' Resolved')
!                END IF

              ELSE ! Probably Unresolved base
!                WRITE(6,72) i,j,k,zs_1d(k),cloud_base(i,j)             &
!                ,cloud_base_buf(i,j)
!72               FORMAT(1X,' **** cloudCover_radar: Prob Unresolved ****'/                &
!                 &  1X,'Rdr Top < Bse*gp=',3I3,' zs=',f7.0              &
!                 & ,' cld_b=',f7.0,' lcl=',f7.0)

                IF(cloud_base_buf(i,j) == unlimited_ceiling) THEN
                  l_unresolved(i,j) = .true.
                  WRITE(6,'(a,i4,i4,a)')                                &
                   ' cloudCover_radar, Error, no LCL found for grid,',i,j,                &
                         ' aborting from INSERTRAD...'
                  call STOP2(114)
                END IF ! cloud_base(i,j).eq.unlimited_ceiling
                GO TO 600

              END IF ! cloud_base_buf(i,j) .lt. zs(i,j,k)

            END IF ! Blw Cld Base: zs.lt.cloud_base_buf(i,j)

          END IF ! At Echo Top: ref(k)>thr & (k.eq.nz .or. ref(kp1)<thr

!-----------------------------------------------------------------------
!
!  Loop through range of cloud grid levels for this model level
!
!-----------------------------------------------------------------------
!
          IF(zs_1d(k) > cloud_base_buf(i,j)) THEN
                             ! Insert radar if we are above cloud base
            cld_cover_3d(i,j,k) = radar_cover
            insert_count_lvl = insert_count_lvl + 1
            insert_count_tot = insert_count_tot + 1
          ELSE ! Radar Echo below cloud base
            l_below_base = .true.
          END IF

          IF(l_below_base) THEN
            icount_below = icount_below + 1

!            IF(icount_below <= 50) THEN
!              WRITE(6,81) i,j,k,zs_1d(k)                               &
!                         ,cloud_base_buf(i,j)
!              81              FORMAT(1X,'Rdr < Bse* g.p.:',3I3          &
!                                     ,' zs_1d(k)=',f7.0,' cld_base=',f7.0)
!            END IF

!            GO TO 600
          END IF ! l_below_base =.true.

        ELSEIF (grid_ref(i,j,k) > -100.0) then ! grid_ref(i,j,k) <= ref_thresh
!            cld_cover_3d(i,j,k) = 0
        ELSE ! grid_ref(i,j,k) <= ref_thresh
!            cld_cover_3d(i,j,k) = -2
          IF(cld_cover_3d(i,j,k) > 0.4 .AND. i_lowecho <= 25) THEN
            i_lowecho=i_lowecho+1
          END IF
!          GO TO 600
        END IF ! grid_ref(i,j,k) > ref_thresh ?

600     continue
      ENDDO  ! k
    ENDDO  ! i
  ENDDO    ! j
!

END SUBROUTINE cloudCover_radar

