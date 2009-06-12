SUBROUTINE interp_NSSL_ref(mype,nlon,nlat,nsig,Nmsclvl,ref_mos_3d,ref_mosaic21,g3_2,zh)
!
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  interp_NSSL_ref    radar observation vertical interpolation     
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-17
!
! ABSTRACT: 
!  This subroutine interpolate radar reflectivity vertically
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
!     Nmsclvl     -  vertical level of radar observation ref_mosaic21
!     ref_mosaic21-  radar reflectivity horizontally in analysis grid and vertically
!                      in mosaic grid (height)
!     g3_2        - 3D background height  
!     zh          - terrain
!
!   output argument list:
!     ref_mos_3d  - 3D radar reflectivity in analysis grid
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
  use kinds, only: r_kind,i_kind, r_single
  implicit none

  INTEGER(i_kind), intent(in) ::  mype
  INTEGER(i_kind), intent(in) ::  nlon,nlat,nsig
  real(r_single),intent(in)  :: zh(nlon,nlat)                         ! terrain
  real(r_single),intent(in)  :: g3_2(nlon,nlat,nsig)                  ! height
  real(r_kind),intent(out) :: ref_mos_3d(nlon,nlat,nsig)            ! reflectivity in grid

  real(r_kind),intent(in) :: ref_mosaic21(nlon,nlat,Nmsclvl)
  INTEGER :: istat_radar
  INTEGER :: Nmsclvl
  real :: msclvl(21),msclvlAll(31)
  INTEGER :: mscX,mscY
  DATA msclvl/1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 6, 7,  &
            8, 9, 10, 11, 12, 13, 14, 15, 16, 17/
  DATA msclvlAll/0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, &
                3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, &
                9, 10, 11, 12, 13, 14, 15, 16, 18/
  REAL :: heightGSI,upref,downref,wght
  INTEGER :: ilvl,numref
  Character*10 :: tilename(8)

  character(10) :: obstype
  integer :: lunin, is
  integer(i_kind):: nreal,nchanl,ilat1s,ilon1s
  character(20) :: isis
  real :: ref_mosaic

  INTEGER :: i,j, k2, k

!
  if(Nmsclvl < -888 ) then
!     ref_mos_3d=-9999.0
     write(6,*) 'interp_NSSL_ref: No radar reflectivity data in this subdomain !'
     return
  endif
!
  ref_mos_3d=-9999.0
  numref=0
  if (Nmsclvl == 31 ) then
      DO k=1,Nmsclvl
        msclvlAll(k)=msclvlAll(k)*1000.0
      ENDDO
  elseif( Nmsclvl == 21 ) then
      msclvlAll=0
      DO k=1,Nmsclvl
        msclvlAll(k)=msclvl(k)*1000.0
      ENDDO
  else
      write(6,*) 'interp_NSSL_ref: Wrong vertical radar mosaic levels'
      write(6,*) ' the level read in is:', msclvlAll
      call stop2(114)
  endif
  
  DO k2=1,nsig
  DO j=2,nlat-1
  DO i=2,nlon-1
      heightGSI=g3_2(i,j,k2)+zh(i,j)
      if(heightGSI >= msclvlAll(1) .and. heightGSI < msclvlAll(Nmsclvl) ) then
         do k=1,Nmsclvl-1
          if( heightGSI >=msclvlAll(k) .and. heightGSI < msclvlAll(k+1) ) ilvl=k
         enddo
         upref=ref_mosaic21(i,j,ilvl+1)
         downref=ref_mosaic21(i,j,ilvl)
         if(abs(upref) < 90.0 .and. abs(downref) <90.0 ) then
           wght=(heightGSI-msclvlAll(ilvl))/(msclvlAll(ilvl+1)-msclvlAll(ilvl))
           ref_mosaic=(1-wght)*downref + wght*upref
           numref=numref+1
         elseif( abs(upref+99.0) < 0.1 .or. abs(downref+99.0) <0.1 ) then
           ref_mosaic=-99.0
         else
           ref_mosaic=-9999.0
         endif
         ref_mos_3d(i,j,k2)=max(ref_mos_3d(i,j,k2),ref_mosaic)
      else
        ref_mos_3d(i,j,k2)=-9999.0
      endif
  ENDDO
  ENDDO
  ENDDO

!
  DO k2=1,nsig
  DO i=2,nlon-1
    ref_mos_3d(i,1,k2)=ref_mos_3d(i,2,k2)
    ref_mos_3d(i,nlat,k2)=ref_mos_3d(i,nlat-1,k2)
  ENDDO
  ENDDO
  DO k2=1,nsig
  DO j=2,nlat-1
    ref_mos_3d(1,j,k2)=ref_mos_3d(2,j,k2)
    ref_mos_3d(nlon,j,k2)=ref_mos_3d(nlon-1,j,k2)
  ENDDO
  ENDDO
  DO k2=1,nsig
    ref_mos_3d(nlon,nlat,k2)=ref_mos_3d(nlon-1,nlat-1,k2)
    ref_mos_3d(nlon,1,k2)=ref_mos_3d(nlon-1,2,k2)
    ref_mos_3d(1,nlat,k2)=ref_mos_3d(2,nlat-1,k2)
    ref_mos_3d(1,j,k2)=ref_mos_3d(2,2,k2)
  ENDDO

  if(numref>0) istat_radar=1

END SUBROUTINE interp_NSSL_ref
