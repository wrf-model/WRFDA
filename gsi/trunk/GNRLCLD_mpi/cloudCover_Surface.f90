SUBROUTINE cloudCover_Surface(mype,nlat,nlon,nsig,i_radius,thunderRadius,&
                        t_bk,p_bk,q,h_bk,zh,  &
                        mxst_p,NVARCLD_P,numsao,OI,OJ,OCLD,OWX,Oelvtn,&
                        cld_cover_3d,cld_type_3d,wthr_type,pcp_type_3d)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloudCover_Surface  cloud cover analysis using surface observation
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-30
!
! ABSTRACT: 
!  This subroutine find cloud cover using surface observations
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
!     i_radius    -
!     thunderRadius -
!
!     t_bk        - 3D background potentional temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     q           - 3D mositure
!     h_bk        - 3D background height  
!     zh          - terrain
!
!     mxst_p      -  maximum observation number
!     NVARCLD_P   -  first dimension of OLCD
!     numsao      -  observation number
!     OI          -  observation x location
!     OJ          -  observation y location
!     OLCD        -  cloud amount, cloud height, visibility
!     OWX         -  weather observation
!     Oelvtn      -  observation elevation
!
!   output argument list:
!     cld_cover_3d- 3D cloud cover
!     cld_type_3d - 3D cloud type
!     wthr_type   - 3D weather type
!     pcp_type_3d - 3D weather precipitation type
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
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

  use kinds, only: r_single,i_kind
  use gridmod, only: regional_time
  use gridmod, only: jlon1,ilat1,istart,jstart
  use gridmod, only: nlat_regional,nlon_regional

  implicit none

  integer(i_kind),intent(in) :: mype
  integer(i_kind),intent(in) :: i_radius
  integer(i_kind),intent(in) :: nlat,nlon,nsig
  real(r_single),intent(in) :: thunderRadius
!
!  surface observation
!
  INTEGER(i_kind), intent(in) :: mxst_p,NVARCLD_P

!  PARAMETER (LSTAID_P=9)

  INTEGER,intent(in) :: numsao
  real(r_single),intent(in) :: OI(mxst_p)  ! x location
  real(r_single),intent(in) :: OJ(mxst_p)  ! y location
  INTEGER(i_kind),intent(in):: OCLD(NVARCLD_P,mxst_p)  ! cloud amount, cloud height,
                                            ! visibility
  CHARACTER*10,intent(in)   :: OWX(mxst_p)      ! weather
  real(r_single),intent(in) :: Oelvtn(mxst_p)  ! elevation

!
!  background
!
  real(r_single),intent(in) :: t_bk(nlon,nlat,nsig)   ! temperature
  real(r_single),intent(in) :: p_bk(nlon,nlat,nsig)   ! pressure
  real(r_single),intent(in) :: zh(nlon,nlat)           ! terrain
  real(r_single),intent(in) :: q(nlon,nlat,nsig)     ! mositure
  real(r_single),intent(in) :: h_bk(nlon,nlat,nsig)   ! height
!
!
!  Variables for cloud analysis
!
  real (r_single),intent(inout) :: cld_cover_3d(nlon,nlat,nsig)
  integer(i_kind),intent(inout) :: cld_type_3d(nlon,nlat,nsig)
  integer(i_kind),intent(inout) :: wthr_type(nlon,nlat)
  real (r_single) :: vis2qc(nlon,nlat)
  integer(i_kind),intent(inout) :: pcp_type_3d(nlon,nlat,nsig)
!
! Observation ID's
! -- FIRST_STA - pointer to first METAR ob in grid volume
  INTEGER(i_kind),allocatable :: first_sta(:,:)
  INTEGER(i_kind) :: ibw,ibe,ibn,ibs,isprd
!  integer :: first_sta(nlon+22,nlat+22)

! -- NEXT_STA  - pointer to next ob found in grid volume
!         from the previous ob in that volume
  INTEGER(i_kind),allocatable :: next_sta(:)
  INTEGER(i_kind) :: idw,ids
!  integer :: next_sta (mxst_p)
!
  integer ::  aninc_cld_p

  real  ::  cloud_zthick_p

  data  cloud_zthick_p    /300./
  data  aninc_cld_p    /10/


!
! temp.
!
!
  INTEGER     null_p
  REAL        spval_p
  PARAMETER ( null_p     = -1       )
  PARAMETER ( spval_p    =  99999.0 )

  INTEGER :: i,j,k,k1
  INTEGER :: i1,j1,ic
  INTEGER :: nx_p, ny_p, nztn_p
  INTEGER :: ista, ista_prev, ista_prev2
  INTEGER :: nsta_cld
  INTEGER :: sta_cld(numsao)
  INTEGER :: ista_min
  INTEGER :: iptsta,jptsta
  INTEGER :: ich, iob,job 
  
  REAL :: min_dist, dist
  REAL :: zdiff
  REAL :: zlev_clr,cloud_dz,cl_base_ista,betav
!
!
!
  real (r_single) :: tbk_k(nlon,nlat,nsig)
  real (r_single) :: cv_bk(nlon,nlat,nsig)
  real (r_single) :: z_lcl(nlon,nlat)
  REAL :: cf_model_base,t_model_base, ht_base
  REAL :: t_dry_adiabat,t_inversion_strength

  LOGICAL :: l_cf,l_inversion
  LOGICAL :: if_cloud_exist



!====================================================================
!  Begin
!
!  set constant name consitent with RUC code to resue the code
!
   nx_p=nlon
   ny_p=nlat
   nztn_p=nsig

   vis2qc=-9999.0

!*****************************************************************
! -- Initialize FIRST_STA and NEXT_STA arrays for assimilation
!      of sfc cloud  obs
!*****************************************************************
!    -- First set both arrays to -1
!-----------------------------------------------------------
   isprd=10
   ibw=max(1,jstart(mype+1)-1-isprd)
   ibe=min(nlon_regional,jstart(mype+1)+jlon1(mype+1)+isprd)
   ibs=max(1,istart(mype+1)-1-isprd)
   ibn=min(nlat_regional,istart(mype+1)+ilat1(mype+1)+isprd)
   allocate(first_sta(ibw:ibe,ibs:ibn))
   allocate(next_sta(numsao))
   first_sta = null_p
   next_sta = null_p

! -- Then fill arrays based on where obs are
!-----------------------------------------------------------
   do ista = 1,numsao
     if (ocld(1,ista) >= 0 .and. ocld(1,ista) < 10000) then  
! when cloud data are available 

        i = int(oi(ista))
        j = int(oj(ista))

        if (first_sta(i,j).eq.null_p) then
            first_sta(i,j) = ista
        else
            ista_prev = first_sta(i,j)
775         continue
            ista_prev2= next_sta(ista_prev)
            if (ista_prev2 .eq. null_p) then
              next_sta(ista_prev) = ista
            else 
              ista_prev = ista_prev2
              go to 775
            end if
        end if
     endif
   enddo
   deallocate(next_sta)

!*****************************************************************
!  analysis of surface/METAR cloud observations
! *****************************************************************

!   rad_clear = 120000./dx_p
!   icnt_clear = 0
!   icnt_cloudy = 0
   idw=jstart(mype+1)-2
   ids=istart(mype+1)-2
   DO j = 2,ny_p-1,aninc_cld_p
   DO i = 2,nx_p-1,aninc_cld_p
      nsta_cld = 0

!sb -- Find all stations w/ cloud data within encompassing box
!mh   The box is decide by the influence radius of the analysis   
      do j1 = max(ibs,ids+j-i_radius),min(ibn,ids+j+aninc_cld_p+i_radius)
      do i1 = max(ibw,idw+i-i_radius),min(ibe,idw+i+aninc_cld_p+i_radius)
         if (first_sta(i1,j1).ne.null_p) then
              nsta_cld = nsta_cld + 1
              sta_cld(nsta_cld) = first_sta(i1,j1)
           end if
      enddo   ! j1
      enddo   ! i1

!           do ic=1,nsta_cld
!             ista = sta_cld(ic)
!             write(6,*) '    ',ocld(1,ista),oi(ista),oj(ista)
!           end do
!
!sb - This is the big grid pt loop.  Walk thru each
!            individual grid points within 10x10 box.
      do j1 = j,min(ny_p-1,j+aninc_cld_p - 1)
      do i1 = i,min(nx_p-1,i+aninc_cld_p - 1)

!sb - Find closest cloud station to grid point
          min_dist = 1.e10
          do ic= 1,nsta_cld
             ista = sta_cld(ic)
             dist = sqrt( (float(idw+i1)-oi(ista))**2   &
                         +(float(ids+j1)-oj(ista))**2 )
             if (dist.lt.min_dist .and. dist.lt.i_radius) then
                 min_dist = dist
                 ista_min = ista
             end if
          end do  ! ic find the closest cloud station

          if (min_dist.lt.1.e9) then
             ista = ista_min
!             iptsta = int(oi(ista)+0.5)
!             jptsta = int(oj(ista)+0.5)

! -- find out if any precip is present
             do ich=1,1
               if ( owx(ista)(ich:ich+1).eq.'SH'  ) wthr_type(i1,j1)=16
               if ( owx(ista)(ich:ich+1).eq.'TH' .and. &
                    min_dist < thunderRadius ) wthr_type(i1,j1)=1
               if ( owx(ista)(ich:ich+1).eq.'RA'  ) wthr_type(i1,j1)=11
               if ( owx(ista)(ich:ich+1).eq.'SN'  ) wthr_type(i1,j1)=12
               if ( owx(ista)(ich:ich+1).eq.'PL'  ) wthr_type(i1,j1)=13
               if ( owx(ista)(ich:ich+1).eq.'DZ'  ) wthr_type(i1,j1)=14
               if ( owx(ista)(ich:ich+1).eq.'UP'  ) wthr_type(i1,j1)=15
               if ( owx(ista)(ich:ich+1).eq.'BR'  ) wthr_type(i1,j1)=21
               if ( owx(ista)(ich:ich+1).eq.'FG'  ) wthr_type(i1,j1)=22
             enddo

!       Consider clear condition case
!       -----------------------------
           if (ocld(1,ista).eq.0) then

             do ic=1,6
               if(float(abs(ocld(6+ic,ista))) < 55555) then
                 write(6,*) 'cloudCover_Surface: Observed cloud above the clear level !!!'
                 write(6,*) 'cloudCover_Surface: some thing is wrong in surface cloud observation !'
                 write(6,*) 'cloudCover_Surface: check the station no.', ista, 'at process ', mype
                 write(6,*) ic,ocld(6+ic,ista)
                 call stop2(114)
               endif
             enddo
! clean the whole column if its CLD
             do k=1,nztn_p
                cld_cover_3d(i1,j1,k)=0.0
                pcp_type_3d(i1,j1,k)=0
             end do
             wthr_type(i1,j1)=0

! -- Now consider non-clear obs
!    --------------------------
           else

             do ic = 1,6
               if (ocld(ic,ista).gt.0 .and. ocld(ic,ista).lt.100) then
!             if  ( csza(i,j).ge.0.10 .and. sat_ctp(i1,j1).gt.1010.0 &
!                 .and. sat_ctp(i1,j1).lt.1050.)  go to 1850

                   cloud_dz = cloud_zthick_p
                   if(ocld(ic,ista) == 4) then
                     if(wthr_type(i1,j1) > 10 .and. wthr_type(i1,j1) < 20) cloud_dz = 1000.  
                                         ! precipitation + highest level
                     if(wthr_type(i1,j1) == 1) cloud_dz = 10000.  ! thunderstorm
                   endif

! --- calculate cloud ceiling level, not exactly, FEW SCT are also considered now
!                   iob = int(oi(ista)-idw+0.5)
!                   job = int(oj(ista)-ids+0.5)
!                   cl_base_ista = (float(ocld(6+ic,ista))+zh(iob,job))
                   cl_base_ista = (float(ocld(6+ic,ista))+Oelvtn(ista))

                   do k=1,nztn_p
                     zdiff = cl_base_ista - h_bk(i1,j1,k)
                     if (zdiff.lt.0) then
                       if (abs(zdiff).lt.cloud_dz) then
                         if(ocld(ic,ista) == 1 ) then
                            cld_cover_3d(i1,j1,k)=max(cld_cover_3d(i1,j1,k),0.1)
                            pcp_type_3d(i1,j1,k)=0
                         elseif (ocld(ic,ista) == 2 ) then
                            cld_cover_3d(i1,j1,k)=max(cld_cover_3d(i1,j1,k),0.3)
                         elseif (ocld(ic,ista) == 3 ) then
                            cld_cover_3d(i1,j1,k)=max(cld_cover_3d(i1,j1,k),0.7)
                         elseif (ocld(ic,ista) == 4 ) then
                            cld_cover_3d(i1,j1,k)=max(cld_cover_3d(i1,j1,k),1.01)
                            if(wthr_type(i1,j1) == 1) then
!                               cld_type_3d(i1,j1,k)=10
                               pcp_type_3d(i1,j1,k)=1
                            endif
                            if(wthr_type(i1,j1) > 10 .and. wthr_type(i1,j1) < 20)  then
!                               cld_type_3d(i1,j1,k)=5 
                               pcp_type_3d(i1,j1,k)=1
                             endif
                         else
                            write(6,*) 'cloudCover_Surface: wrong cloud coverage observation!'
                            call stop2(114)
                         endif
                       endif
                     else
!  ---- Clear up to cloud base of first cloud level
                       if (ic.eq.1) cld_cover_3d(i1,j1,k)=0
                       if (ocld(ic,ista) == 1) pcp_type_3d(i1,j1,k)=0
                       if (ocld(ic,ista) == 3 .or. ocld(ic,ista) == 4) then
                          if( (wthr_type(i1,j1) > 10 .and. wthr_type(i1,j1) < 20)  &
                              .or. (wthr_type(i1,j1) == 1) )  then 
                             pcp_type_3d(i1,j1,k)=1
                          endif
                       endif
                     end if
                   end do  ! end K loop
!  ----clean cloud above stratusphere
                   do k=1,nztn_p
                     if( h_bk(i1,j1,k) > 14000 )              &
                         cld_cover_3d(i1,j1,k)=0 
                   enddo
!
                end if     ! end if ocld > 0
              end do      ! end IC loop

           end if      ! end if cloudy ob

! -- Use visibility for low-level cloud whether
           if (wthr_type(i1,j1) < 30 .and. wthr_type(i1,j1) > 20  &
               .and. ocld(13,ista).lt.5000) then
              cld_type_3d(i1,j1,1) = 2
              cld_type_3d(i1,j1,2) = 2
              betav = 3.912 / (float(ocld(13,ista)) / 1000.)
              vis2qc(i1,j1) = ( (betav/144.7) ** 1.14 ) / 1000.
           endif  ! cloud or clear

         endif   ! if cloud ob is within min distance

      enddo  ! i1
      enddo  ! j1  end of the big grid pt loop

   ENDDO   ! j=1,nx_p   
   ENDDO   ! i=1,ny_p

  deallocate(first_sta)
!
!   Determine if the layer is dry or it has inversion.
!  (in either case, the cloud will be cleared out)
!
   IF(.true.) THEN     ! Set inversion strength flag
     call BckgrndCC(nlon,nlat,nsig,    &
                 t_bk,p_bk,q,h_bk,zh,  &
                 cv_bk,tbk_k,z_lcl)    ! out

     DO j = 2,nlat-1
     DO i = 2,nlon-1

        if_cloud_exist=.false.
        do k=nsig-1,2,-1
          if(cld_cover_3d(i,j,k) > 0.01) then
            cf_model_base = cv_bk(i,j,k)
            t_model_base = tbk_k(i,j,k)
            ht_base=h_bk(i,j,k)
            if_cloud_exist=.true.
          endif
        enddo
!
! note, do we need to consider cloud base from background
        if(if_cloud_exist) then
          do k=2, nsig-1
            if(cld_cover_3d(i,j,k) > 0.01) then
              l_cf=.false.
              l_inversion=.false.
              t_dry_adiabat = tbk_k(i,j,2) -.0098 * (h_bk(i,j,k) - h_bk(i,j,2))
              t_inversion_strength = tbk_k(i,j,k) - t_dry_adiabat

              IF( (tbk_k(i,j,k) > t_model_base)  .and. &
                  (tbk_k(i,j,k) > 283.15) .and.  &   !   temp check
                  (t_inversion_strength > 4.) ) then ! delta theta chk
                      l_inversion = .true.           ! Inversion exists
              endif
              IF( (cv_bk(i,j,k) < cf_model_base - 0.3) .and. &
                  (h_bk(i,j,k) - ht_base >= 500.) ) THEN
                    l_cf = .true.           ! Dry layer exists
              ENDIF 
              if(l_inversion) then
                 cld_cover_3d(i,j,k) =0.0
              endif
            endif ! in cloud
          enddo  ! k
        endif !   if_cloud_exist = true

     ENDDO   ! i
     ENDDO   ! j

   END IF     ! .true. for dry-inversion check.

END SUBROUTINE cloudCover_Surface

