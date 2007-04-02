module gsi_thinning
!$$$ subprogram documenation block
!
! abstract:  This module contains code which may be used to selectively
!            thin satellite data.
!
! program history log:
!   2006-10-28 Jianjun Xu, developed from GSI 
!   2007-03-30 Zhiquan Liu, modify and add comments
!
!$$$ end documentation block
  use gsi_kinds, only: r_kind,i_kind
  use gsi_constants, only: deg2rad,rearth_equator,zero,two,pi,half,one,quarter,&
       rad2deg
  implicit none

  real(r_kind),parameter:: r90   = 90.0_r_kind
  real(r_kind),parameter:: r360  = 360.0_r_kind
  real(r_kind),parameter:: r999  = 999.0_r_kind
  real(r_kind),parameter:: r1000 = 1000.0_r_kind 

! mlat: lat #, mlonx: max lon #, itxmax: grid box #
  integer(i_kind) mlat,maxthin,itxmax,dthin,mlonx,mlony
  integer(i_kind),dimension(0:51):: istart_val

! mlon(mlat): lon # in each lat   
  integer(i_kind),allocatable,dimension(:):: mlon,icount,ibest_obs
  integer(i_kind),allocatable,dimension(:,:):: isli

! lat/lon range inside tile
  real(r_kind) rlat_min,rlat_max,rlon_min,rlon_max,dlat_grid,dlon_grid
! glat(mlat): lat #, glon(mlat,mlonx), hll(mlat,mlonx)
  integer(i_kind),allocatable,dimension(:,:) :: hll
  real(r_kind),allocatable,dimension(:)   :: glat
  real(r_kind),allocatable,dimension(:,:) :: glon,sli,sno
  real(r_kind),allocatable,dimension(:)   :: score_crit

contains

  subroutine makegvals (obstype,rmesh)
! compute dimention of thinning box
! output (mlat,mlonx,dlat_grid,dlon_grid), istart_val
    implicit none

    character(5), intent(in) :: obstype 
    real(r_kind), intent(in) :: rmesh

    logical odd
    integer(i_kind) i,ii,j,k,nlat,nlon
    integer(i_kind) icnt,mlonj
    real(r_kind) delonx,delat,dgv,dx,dy
    real(r_kind) twopi,dlon_g,dlat_g,dlon_e,dlat_e
    real(r_kind) factor,factors,delon
    real(r_kind) rkm2dg,glatm,glatx

    if (obstype == 'hirs2') dthin=1
    if (obstype == 'hirs3') dthin=1
    if (obstype == 'hirs4') dthin=1
    if (obstype == 'msu  ') dthin=2
    if (obstype == 'amsua') dthin=2
    if (obstype == 'amsub') dthin=3
    if (obstype == 'mhs  ') dthin=3

!   Initialize variables, set constants
    maxthin=dthin

    istart_val=0
    twopi  = two*pi
    rkm2dg = r360/(twopi*rearth_equator)*r1000

    if (rlon_min < zero) rlon_min = rlon_min + r360
    if (rlon_max < zero) rlon_max = rlon_max + r360

       dlat_grid = rlat_max - rlat_min
       dlon_grid = rlon_max - rlon_min

       dx    = rmesh*rkm2dg
       dy    = dx
       mlat  = dlat_grid/dy + half
       mlonx = dlon_grid/dx + half
       delat = dlat_grid/mlat
       delonx= dlon_grid/mlonx
       dgv   = delat*half

       mlat=max(2,mlat);   mlonx=max(2,mlonx)
    
      do ii=1,maxthin
       istart_val(ii+1)=istart_val(ii)
          icnt=0
          do j = 1,mlat
             glatx = rlat_min + (j-1)*delat
             glatx = glatx*deg2rad
             glatm = glatx + dgv*deg2rad
             factor = abs(cos(abs(glatm)))
             mlonj = nint(mlonx*factor)
             mlonj = max(2,mlonj)
             do i = 1,mlonj
                icnt=icnt+1
                istart_val(ii+1)=istart_val(ii+1)+1
             enddo
          enddo
      end do
    !print *, mlat,mlonx,dlat_grid,dlon_grid
    !print *, istart_val
    return
  end subroutine makegvals

  subroutine makegrids(rmesh)
! making thinning box
! output: mlon(mlat),glat(mlat),glon(mlonx,mlat),hll(mlonx,mlat)

    implicit none
    real(r_kind), intent(in) :: rmesh

    logical odd
    integer(i_kind) i,j,k
    integer(i_kind) mlonj
    real(r_kind) delonx,delat,dgv,halfpi,dx,dy
    real(r_kind) twopi
    real(r_kind) factor,factors,delon
    real(r_kind) rkm2dg,glatm

    allocate(mlon(mlat),glat(mlat),glon(mlonx,mlat),hll(mlonx,mlat))

!   Set constants
	delat = dlat_grid/mlat
	delonx= dlon_grid/mlonx
        dgv  = delat*half
        halfpi=pi*half
        twopi=pi*two
!   Set up thinning grid lon & lat.  The lon & lat represent the location of the
!   lower left corner of the thinning grid box.

       itxmax=0
      do j = 1,mlat
       glat(j) = rlat_min + (j-1)*delat
       glat(j) = glat(j)*deg2rad
       glatm = glat(j) + dgv*deg2rad

       factor = abs(cos(abs(glatm)))
          mlonj   = nint(mlonx*factor)	
          mlon(j) = max(2,mlonj)
          delon = dlon_grid/mlon(j)

       glat(j) = min(max(-halfpi,glat(j)),halfpi)
       do i = 1,mlon(j)
          itxmax=itxmax+1
          hll(i,j)=itxmax
          glon(i,j) = rlon_min + (i-1)*delon
          glon(i,j) = glon(i,j)*deg2rad
          glon(i,j) = min(max(zero,glon(i,j)),twopi)
       enddo
       !write(6,'(f10.5,i8,2i10)') glat(j)*rad2deg, mlon(j),hll(1,j),hll(mlon(j),j)
       !write(6,'(10f8.3)')   (glon(i,j)*rad2deg,i=1,mlon(j))

    end do

!   Allocate  and initialize arrays
    allocate(icount(itxmax))
    allocate(ibest_obs(itxmax))
    allocate(score_crit(itxmax))

    do j=1,itxmax
       icount(j)     = 0
       ibest_obs(j)  = 0
       score_crit(j) = 9.99e6_r_kind
    end do
    !write(6,'(10f12.2)') (score_crit(j),j=1,itxmax)

    return
  end subroutine makegrids

  subroutine map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse)
    implicit none
    logical,intent(out):: iuse
    integer(i_kind),intent(in):: ithin
    integer(i_kind),intent(out):: itt,itx
    real(r_kind),intent(in):: dlat_earth,dlon_earth,crit1
    real(r_kind),intent(out):: dist1

    integer(i_kind) ix,iy
    real(r_kind) dlat1,dlon1,dx,dy,dxx,dyy


!   Compute (i,j) indices of coarse mesh grid (grid number 1) which
!   contains the current observation.
    dlat1=dlat_earth
    dlon1=dlon_earth
    
    call grdcrd(dlat1,1,glat,mlat,1)
    iy=int(dlat1)
    dy=dlat1-iy
    iy=max(1,min(iy,mlat))

    call grdcrd(dlon1,1,glon(1,iy),mlon(iy),1)
    ix=int(dlon1)
    dx=dlon1-ix
    ix=max(1,min(ix,mlon(iy)))

    dxx=half-min(dx,one-dx)
    dyy=half-min(dy,one-dy)
    dist1=dxx*dxx+dyy*dyy+half
    itx=hll(ix,iy)
    itt=istart_val(ithin)+itx
    if(ithin == 0) itt=0
    iuse=.true.
    if(dist1*crit1 > score_crit(itx) .and. icount(itx) == 0)iuse=.false.

    !write(6,'(a,3f10.3)') 'dlat_earth dlon_earth crit1 ',dlat_earth*rad2deg,dlon_earth*rad2deg,crit1
    !write(6,'(2i5,3f10.3,i10,e12.5,2x,L)') ix,iy,dx,dy,dist1,itx,score_crit(itx),iuse
    return
  end subroutine map2tgrid

  subroutine grdcrd(d,nd,x,nx,flg)
  implicit none
  integer(i_kind) nd,id,ix,nx
  integer(i_kind),intent(in):: flg
  real(r_kind) d
  real(r_kind),dimension(nx):: x
! Treat "normal" case in which nx>1
  if(nx>1) then
        if (flg.eq.1) then

!          Case in which x is in increasing order
           if(d<=x(1)) then
              ix=1
           else
              ix=isrchf(nx-1,x,d,flg)-1
           end if
           if(ix==nx) ix=ix-1

        else if (flg.eq.(-1)) then

!          Case in which x is in decreasing order
           if(d>=x(1)) then
              ix=1
           else
              ix=isrchf(nx-1,x,d,flg)-1
           end if
        end if
        d=float(ix)+(d-x(ix))/(x(ix+1)-x(ix))

! Treat special case of nx=1
  elseif (nx==1) then
        d = one
  endif

  return
end subroutine grdcrd 

  subroutine checkob(dist1,crit1,itx,iuse)
    implicit none
    logical,intent(inout):: iuse
    integer(i_kind),intent(in):: itx
    real(r_kind),intent(in):: dist1,crit1

!   If data (no thinning), simply return to calling routine
    if(.not. iuse .or. icount(itx)==0)return
    if(crit1*dist1 > score_crit(itx))iuse=.false.

    return
  end subroutine checkob

  subroutine finalcheck(dist1,crit1,iobs,itx,iobsout,iuse,sis)
    implicit none
    logical,intent(inout):: iuse
    integer(i_kind),intent(inout):: iobs,iobsout
    integer(i_kind),intent(in):: itx
    real(r_kind),intent(in):: dist1,crit1
    character(20),intent(in):: sis

    real(r_kind) crit

    if(.not. iuse)return

!   If using all data (no thinning), simply return to calling routine


    crit=crit1*dist1

    if(icount(itx) == 0)then

!   Increment obs counter

      if(iobs < itxmax)then
       iobs=iobs+1
       iobsout=iobs
       score_crit(itx)= crit
       ibest_obs(itx) = iobs
       icount(itx)=icount(itx)+1
      else
       iuse = .false.
       write(6,*)' ndata > maxobs when reading data for ',sis,itxmax
      end if

    else if(crit < score_crit(itx))then
      score_crit(itx)= crit
      iobsout=ibest_obs(itx)
      icount(itx)=icount(itx)+1
    else
      iuse = .false.
    end if


    return
  end subroutine finalcheck

  subroutine destroygrids
    implicit none
    deallocate(mlon,glat,glon,hll)
    deallocate(icount)
    deallocate(ibest_obs)
    deallocate(score_crit)
    return
  end subroutine destroygrids

  subroutine destroy_sfc
    implicit none
    deallocate(sli,sno,isli)
    return
  end subroutine destroy_sfc

function isrchf(nx1,x,y,flg)
!                .      .    .                                       .
! subprogram:    isrchf
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: get grid coordinates from monotonically increasing or
!           decreasing points
!
! program history log:
!   2005-03-07  treadon - add doc block
!
!   input argument list:
!     nx1    - number of input points
!     x      - grid values
!     y      - target value
!     flg    - marks order of values in x
!              (1=increasing, -1=decreasing)
!
!   output argument list:
!     isrchf  - array index of input grid value near target value
!
  implicit none
  integer(i_kind):: isrchf
  integer(i_kind),intent(in):: nx1
  integer(i_kind),intent(in):: flg
  real(r_kind),intent(in):: y
  real(r_kind),dimension(nx1),intent(in):: x

  integer(i_kind) k

  if(flg.eq.1) then
    do k=1,nx1
      if(y<=x(k)) then
        isrchf=k

        go to 100
      end if
    end do
  else
    do k=1,nx1
      if(y>=x(k)) then
         isrchf=k
        go to 100
      end if
    end do
  end if

  isrchf=nx1+1
  if(nx1<=0) isrchf=0

100 continue
  return
end  function isrchf

end module gsi_thinning 
