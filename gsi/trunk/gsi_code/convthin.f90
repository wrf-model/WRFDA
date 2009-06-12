module convthin

use kinds, only: r_kind,i_kind
use gridmod, only: nsig
implicit none

! ---- from satthin.F90 ---------------
! also changes for 3D thinning grids
integer(i_kind) mlat,superp,maxthin,itxmax
integer(i_kind) ier0,ier1,ier2,ier3,ier4
integer(i_kind) mlonx,mlonj,levp
integer(i_kind),dimension(0:51):: istart_val

!integer(i_kind),allocatable,dimension(:):: mlon,icount,ibest_obs
integer(i_kind),allocatable,dimension(:):: mlon
integer(i_kind),allocatable,dimension(:,:):: icount,ibest_obs,kount

real(r_kind) rlat_min,rlat_max,rlon_min,rlon_max,dlat_grid,dlon_grid
!real(r_kind),allocatable,dimension(:):: glat
real(r_kind),allocatable,dimension(:):: glat,pcoord
real(r_kind),allocatable,dimension(:):: super_val,super_val1
!real(r_kind),allocatable,dimension(:,:):: glon,hll,sli,sst,sno 
real(r_kind),allocatable,dimension(:,:):: glon,hll,sli,sst,sno,score_crit
!real(r_kind),allocatable,dimension(:):: score_crit
logical use_all

contains

  subroutine make3grids(rmesh,pmesh,pflag)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    make3grids                            
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine sets up dimensions for and allocates
!            thinning grids.
!
! program history log:
!   2002-10-17  treadon
!   2004-06-22  treadon - update documentation
!   2004-12-09  treadon - allocate thinning grids consistent with analysis domain
!   2006-01-27  kistler - added vertical dimension
!
!   input argument list:
!     rmesh - mesh size (km) of thinning grid.  If (rmesh <= one), 
!             then no thinning of the data will occur.  Instead,
!             all data will be used without thinning.
!     pmesh - number of pressure-type levels
!     pflag - type of pressure-type levels; 1:linear p,2:linear ln(p),3:sigma,4:hygrid
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: rearth_equator,two,deg2rad,zero,half,one,quarter,pi
	use satthin, only:dlat_grid,dlon_grid

    implicit none

    real(r_kind),parameter:: r360 = 360.0_r_kind
    logical odd
    integer(i_kind) i,j,k

    real(r_kind) delonx,delat,dgv,halfpi,dx,dy
    real(r_kind) twopi
    real(r_kind),intent(in) ::  rmesh,pmesh,pflag
    real(r_kind) factor,factors,delon
    real(r_kind) rkm2dg,glatm

!   If there is to be no thinning, simply return to calling routine
    use_all=.false.
    if(abs(rmesh) <= one)then
      use_all=.true.
      itxmax=2.e6
      return
    end if

!   Set constants
    halfpi = half*pi
    twopi  = two*pi
    rkm2dg = r360/(twopi*rearth_equator)*1.e3_r_kind

!   Set up dimensions and allocate arrays for thinning grids
!	horizontal
    if (rmesh<zero) rkm2dg=one
    dx    = rmesh*rkm2dg
    dy    = dx
    mlat  = dlat_grid/dy + half
    mlonx = dlon_grid/dx + half
	delat = dlat_grid/mlat
	delonx= dlon_grid/mlonx
    dgv  = delat*half
    mlat=max(2,mlat);   mlonx=max(2,mlonx)

!	vertical
    if (pflag == 0.0) then
		levp=nsig
    !	if (pflag == 1.0) then
	!	levp=nint(1000./pmesh)
	else
		print*, 'pflag=0 only valid option at this time' ;stop 1
	endif
    allocate(mlon(mlat),glat(mlat),glon(mlonx,mlat),hll(mlonx,mlat),stat=ier0)
	if (ier0 >0 ) return


!   Set up thinning grid lon & lat.  The lon & lat represent the location of the
!   lower left corner of the thinning grid box.
    itxmax=0
    do j = 1,mlat
       glat(j) = rlat_min + (j-1)*delat
       glat(j) = glat(j)*deg2rad
       glatm = glat(j) + dgv*deg2rad

       factor = abs(cos(abs(glatm)))
       if (rmesh>zero) then
          mlonj   = nint(mlonx*factor)	
          mlon(j) = max(2,mlonj)
          delon = dlon_grid/mlon(j)
       else
          delon = factor*rmesh
          delon = min(delon,r360)
          mlon(j) = dlon_grid/delon
       endif

       glat(j) = min(max(-halfpi,glat(j)),halfpi)
       do i = 1,mlon(j)
          itxmax=itxmax+1
          hll(i,j)=itxmax
          glon(i,j) = rlon_min + (i-1)*delon
          glon(i,j) = glon(i,j)*deg2rad
          glon(i,j) = min(max(zero,glon(i,j)),twopi)
       enddo

    end do
    
!   Allocate  and initialize arrays
    allocate(kount(2,levp),stat=ier1)
    allocate(icount(itxmax,levp),stat=ier2)
    allocate(ibest_obs(itxmax,levp),stat=ier3)
    allocate(score_crit(itxmax,levp),stat=ier4)

    if( ier1 == 0) kount=0
    if( ier2 == 0) icount=0
    if( ier4 == 0) score_crit = 9.99e6_r_kind

    return
  end subroutine make3grids
  subroutine del3grids
! abstract:  This routine deallocates arrays from make3grids
    deallocate(mlon,glat,glon,hll)
    deallocate(icount)
    deallocate(kount)
    deallocate(ibest_obs)
    deallocate(score_crit)
  end subroutine del3grids
  subroutine map3grids(pcoord,levp,dlat_earth,dlon_earth,pob,crit1,ithin,iobs,itx,itt,iobsout,ip,iuse)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    map3grids
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine maps convential observations to a 3d thinning grid.
!
! program history log:
!   2002-10-17  treadon
!   2004-06-22  treadon - update documentation
!   2004-07-23  derber - modify code to thin obs as read in
!   2004-12-08  li, xu - fix bug --> set iuse=.true. when use_all=.true.
!   2005-10-14  treadon - variable name change (dlat0,dlon0) --> d*_earth
!   2006-01-25  kistler - extend 2d to 3d 
!
!   input argument list:
!     pcoord     - veritical coordinate values
!     levp       - number of vertical levels
!     dlat_earth - earth relative observation latitude (radians)
!     dlon_earth - earth relative observation longitude (radians)
!     pob        - observation pressure ob
!     crit1      - quality indicator for observation (smaller = better)
!     ithin      - number of obs to retain per thinning grid box
!
!   output argument list:
!     iobs  - observation counter
!     itx   - combined (i,j) index of observation on thinning grid
!     itt   - superobs thinning counter
!     iobsout- location for observation to be put
!     ip    - vertical index
!     iuse  - .true. if observation should be used
!     
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_kind,i_kind
    use constants, only: one, half
    implicit none

	logical ,intent(out):: iuse
    real(r_kind),intent(in):: pcoord(levp),dlat_earth,dlon_earth,crit1
    integer(i_kind),intent(in):: ithin,levp
    integer(i_kind),intent(inout):: iobs
    integer(i_kind),intent(out):: itx,itt,iobsout,ip

    integer(i_kind) kk,ix,iy
    real(r_kind) dlat1(1),dlon1(1),pob1(1)
    real(r_kind) dx,dy,dp,dxx,dyy,pob
    real(r_kind) dist1,crit
!   real(r_kind) dista,distb,ratio


!   If using all data (no thinning), simply return to calling routine
    if(use_all)then
       iuse=.true.
       iobs=iobs+1
       itt=0
       iobsout=iobs
       return
    end if

!   Compute (i,j,k) indices of coarse mesh grid (grid number 1) which 
!   contains the current observation.
    dlat1=dlat_earth
    dlon1=dlon_earth

	pob1=pob
    call grdcrd(pob1,1,pcoord,levp,-1)
	ip=int(pob1(1))
	dp=pob1(1)-ip
	ip=max(1,min(ip,levp))

    call grdcrd(dlat1,1,glat,mlat,1)
    iy=int(dlat1(1))
    dy=dlat1(1)-iy
    iy=max(1,min(iy,mlat))

    call grdcrd(dlon1,1,glon(1,iy),mlon(iy),1)
    ix=int(dlon1(1))
    dx=dlon1(1)-ix
    ix=max(1,min(ix,mlon(iy)))

    dxx=half-min(dx,one-dx)
    dyy=half-min(dy,one-dy)
    dist1=dxx*dxx+dyy*dyy+half
    itx=hll(ix,iy)
    itt=istart_val(ithin)+itx
    if(ithin == 0) itt=0

!   Increment obs counter on coarse mesh grid.  Also accumulate observation
!   score and distance functions

    icount(itx,ip)=icount(itx,ip)+1
!   ratio=1.e9
!   if ( dx > zero ) ratio=dy/dx
!   dista=sin(two*atan(ratio))
!   distb=sin(pi*dx)                !dista+distb is max at grid box center
!   dist1=one - quarter*(dista + distb)  !dist1 is min at grid box center and 
                                    !ranges from 1 (at corners)to 
                                    !.5 (at center of box)
    crit=crit1*dist1
    iuse=.false.

    if(icount(itx,ip) == 1)then

!   Increment obs counter

      iuse=.true.
      iobs=iobs+1
      score_crit(itx,ip)= crit
      ibest_obs(itx,ip) = iobs
      iobsout=iobs

    end if
    if(crit < score_crit(itx,ip) .and. icount(itx,ip) > 1)then
      iuse=.true.
      score_crit(itx,ip)= crit
      iobsout=ibest_obs(itx,ip)
    end if

    return
  end subroutine map3grids
  subroutine thin_count(ithin)
	use kinds,     only: r_kind,i_kind
	use gridmod,   only: nsig

	implicit none
	integer, intent(in) :: ithin ! number of obs kept in each of the itxmax,nsig grid boxes
	integer(i_kind)  i,k
	do k=nsig,1,-1
		do i=1,itxmax
			if(icount(i,k) >= 1) then
				kount(1,k)=kount(1,k)+ithin
				kount(2,k)=kount(2,k)+icount(i,k)
			endif
		enddo
	enddo
  end subroutine thin_count
end module convthin
