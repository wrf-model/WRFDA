subroutine mape_ctp (ib,jb,nx,ny,nn_obs,numsao,data_s,w_pcld,w_tcld,w_frac)

!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  mape_ctp   map GOES cloud product to analysis grid              
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-03_10
!
! ABSTRACT: 
!  This subroutine map GOES cloud product to analysis grid
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     ib       - begin i point of this domain
!     jb       - begin j point of this domain
!     nx       - no. of lons on subdomain (buffer points on ends)
!     ny       - no. of lats on subdomain (buffer points on ends)
!     nn_obs   - 1st dimension of observation arry data_s
!     numsao   - number of observation
!     data_s   -  observation array for GOES cloud products
!
!   output argument list:
!     w_pcld   - GOES cloud top pressure in analysis grid
!     w_tcld   - GOES cloud top temperature in analysis grid
!     w_frac   - GOES cloud coverage in analysis grid
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
!     adapted according to RUC subroutine rd_cld
! *
! * This routine reads NESDIS (Madison, WI) cloud product produced
! *  from GOES sounder data. The original product is reprocessed onto
! *   MAPS40 grid boxes. There could be more than one cloud product
! *    in a grid-box, so we use the nearest one that falls in the
! *     grid. The routine combines GOES-8 and 10 products.
!
! ===== History =====
!
! * Internal variables:
!     CTP_E, CTP_W           Soft-linked filename for ascii GOES Clouds
!
! * Working variables:
!
! * Working variables used for sorting max size of 10:
!     Pxx, Txx, xdist,xxxdist     (R4)
!     Fxx, Nxx, index, jndex      (I4)
!     ioption              (I4)  = 1  if selection is nearest neighbor
!                                = 2  if selection is median of samples
!
!
! * Output variables on gridpoint (Nx,Ny):
!     w_pcld, w_tcld (R4)   Cloud-top pressure and temperature
!     w_frac         (R4)   Effective fractional cloud coverage, option=1
!                           fractional coverage within RUC grid, option=2
!     w_eca          (R4)   Effective fractional cloud regardless option
!                             (effective cloud amount - eca)
!     nlev_cld       (I4)   Number of cloud levels. TO BE USED LATER
!                            to incorporate multi-level cloud
!
! * Calling routines
!     sorting
!     sortmed
!
! *
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

      use kinds, only: r_kind,r_single,i_kind
      use constants, only: zero,one_tenth,one,deg2rad
      use gridmod, only: regional,nsig,tll2xy,txy2ll
                         
      implicit none

      integer  nfov
      parameter (nfov=60)

      character header*80
! input-file variables:
      INTEGER(i_kind),intent(in) :: Nx, Ny
      INTEGER(i_kind),intent(in) :: ib, jb
      INTEGER,intent(in) :: numsao, nn_obs
      real(r_kind),dimension(nn_obs,numsao):: data_s

! Working
      real     Pxx(Nx,Ny,nfov),Txx(Nx,Ny,nfov)  
      real     xdist(Nx,Ny,nfov), xxxdist(nfov)
      real     fr,sqrt, qc, type
      integer  Nxx(Nx,Ny,nfov),index(Nx,Ny), jndex(nfov)
      integer  ioption 
      integer  ipt,ixx,ii,jj,i,med_pt,igrid,jgrid  &
               ,ncount,ncount1,ncount2,ii1,jj1,nobs,n

      real(r_kind) :: xc
      real(r_kind) :: yc

! Output
      real(r_single), intent(out) ::  w_pcld(Nx,Ny), w_tcld(Nx,Ny)
      real(r_single), intent(out) ::  w_frac(Nx,Ny)
      real(r_single) ::  w_eca(Nx,Ny)
      integer(i_kind)  :: nlev_cld(Nx,Ny)
      integer  :: ios

!
! * Initialize outputs since GOES sounder do not scan all MAPS domain
!
      do jj=1,Ny
      do ii=1,Nx
         w_eca (ii,jj) =-99999.
         index(ii,jj) = 0
      enddo
      enddo

! -- set ios as failed unless valid data points are found below
      ios = 0

! -----------------------------------------------------------
! -----------------------------------------------------------
!     Map each FOV onto RUC grid points 
! -----------------------------------------------------------
! -----------------------------------------------------------
      do 30 ipt=1,numsao
 
          xc=data_s(2,ipt) - ib + 1.0
          yc=data_s(3,ipt) - jb + 1.0
 
! * XC,YC should be within subdomain boundary, i.e., XC,YC >0
 
          if(XC .ge. 1. .and. XC .lt. Nx .and.        &
               YC .ge. 1. .and. YC .lt. Ny) then
             ii1 = int(xc+0.5)
             jj1 = int(yc+0.5)

             do jj = max(1,jj1-2), min(ny,jj1+2)
             if (jj1-1.ge.1 .and. jj1+1.le.ny) then
               do ii = max(1,ii1-2), min(nx,ii1+2)
               if (ii1-1.ge.1 .and. ii1+1.le.nx) then
             
! * We check multiple data within gridbox

                 if (index(ii,jj).lt.nfov) then
                   index(ii,jj) = index(ii,jj) + 1
 
                   Pxx(ii,jj,index(ii,jj)) = data_s(4,ipt)
                   Txx(ii,jj,index(ii,jj)) = data_s(6,ipt)
                   Nxx(ii,jj,index(ii,jj)) = int(data_s(5,ipt))
                   nlev_cld(ii,jj) = 1
                   xdist(ii,jj,index(ii,jj)) = sqrt(      &
                      (XC+1-ii)**2 + (YC+1-jj)**2)
                 end if
               endif
               enddo ! ii
             endif
             enddo  ! jj
          endif  ! observation is in the domain
   30 continue
!
! * ioption = 1 is nearest neighrhood
! * ioption = 2 is median of cloudy fov
      ioption = 2
!
      do jj = 1,Ny
      do ii = 1,Nx
         if (index(ii,jj) .lt. 3 ) then
!           w_pcld(ii,jj) = Pxx(ii,jj,1)
!           w_tcld(ii,jj) = Txx(ii,jj,1)
!           w_frac(ii,jj) = float(Nxx(ii,jj,1))/100.
!           w_eca(ii,jj) =  float(Nxx(ii,jj,1))/100.

         elseif(index(ii,jj) .ge. 3) then

! * We decided to use nearest neighborhood for ECA values,
! *     a kind of convective signal from GOES platform...

             do i=1,index(ii,jj)
               jndex(i) = i
               xxxdist(i) = xdist(ii,jj,i)
             enddo
             call sorting(xxxdist,index(ii,jj),jndex)
            w_eca(ii,jj) = float(Nxx(ii,jj,jndex(1)))/100.
! * Sort to find closest distance if more than one sample
             if(ioption .eq. 1) then    !nearest neighborhood
                do i=1,index(ii,jj)
                  jndex(i) = i
                  xxxdist(i) = xdist(ii,jj,i)
                enddo
                call sorting(xxxdist,index(ii,jj),jndex)
                w_pcld(ii,jj) = Pxx(ii,jj,jndex(1))
                w_tcld(ii,jj) = Txx(ii,jj,jndex(1))
                w_frac(ii,jj) = float(Nxx(ii,jj,jndex(1)))/100.
             endif
! * Sort to find median value 
             if(ioption .eq. 2) then    !pick median 
                do i=1,index(ii,jj)
                  jndex(i) = i
                  xxxdist(i) = Pxx(ii,jj,i)
                enddo
                call sortmed(xxxdist,index(ii,jj),jndex,fr)
                med_pt = index(ii,jj)/2  + 1
                w_pcld(ii,jj) = Pxx(ii,jj,jndex(med_pt))
                w_tcld(ii,jj) = Txx(ii,jj,jndex(med_pt))
                w_frac(ii,jj) = fr
             endif
         endif
      enddo  !ii
      enddo  !jj
 
      return
end subroutine mape_ctp
 
subroutine sorting(d,n,is) 
      dimension d(n),is(n)
      nm1 = n-1 
      do 10 i=1,nm1 
      ip1 = i+1 
        do 10 j=ip1,n 
        if(d(i).le.d(j)) goto 10 
          temp = d(i) 
          d(i) = d(j) 
          d(j) = temp 
          iold  = is(i) 
          is(i) = is(j) 
          is(j) = iold 
   10 continue 
      return 
end subroutine  sorting

subroutine sortmed(p,n,is,f) 
      real p(n)
      integer is(n)
! * count cloudy fov
      real    f
      integer cfov
      cfov = 0
      do i=1,n
         if(p(i) .lt. 999.) cfov = cfov + 1
      enddo
      f = float(cfov)/(max(1,n))
! cloud-top pressure is sorted high cld to clear
      nm1 = n-1 
      do 10 i=1,nm1 
      ip1 = i+1 
        do 10 j=ip1,n 
        if(p(i).le.p(j)) goto 10 
          temp = p(i) 
          p(i) = p(j) 
          p(j) = temp 
          iold  = is(i) 
          is(i) = is(j) 
          is(j) = iold 
   10 continue 
      return 
end subroutine sortmed
