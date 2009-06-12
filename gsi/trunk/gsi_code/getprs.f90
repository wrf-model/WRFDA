subroutine getprs(ps,prs)
! subprogram:    getprs       get 3d pressure or 3d pressure deriv
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: calculate 3d pressure and its horizontal derivatives
!
! program history log:
!   2005-09-29  kleist
!   2006-04-12  treadon - replace sigi with bk5
!   2006-07-31  kleist  - analysis variable change from ln(ps) to ps
!   2007-05-08  kleist  - add generalized vert coord and derivative call
!   2007-07-26  cucurull- compute 3d pressure and derivatives in different subroutines
!                       - remove gues_tv from argument list; clean up code
!
!   input argument list:
!     ps       - surface pressure
!
!   output argument list:
!     prs        - 3d pressure
!
!$$$
  use kinds,only: r_kind,i_kind
  use constants,only: zero,half,one_tenth,rd_over_cp,one
  use gridmod,only: nsig,lat2,lon2,ak5,bk5,ck5,tref5,idvc5
  use gridmod,only: regional,wrf_nmm_regional,eta1_ll,eta2_ll,pdtop_ll,pt_ll
  use guess_grids, only: ges_tv,ntguessig
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2),intent(in):: ps
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(out):: prs

! Declare local variables
  real(r_kind) kapr,trk
  integer(i_kind) i,j,k,k2,it

! Declare local parameter
  real(r_kind),parameter:: ten = 10.0_r_kind

  kapr=one/rd_over_cp
  prs=zero 
  it=ntguessig

  if(wrf_nmm_regional) then
    do k=1,nsig+1
      do j=1,lon2
        do i=1,lat2
          prs(i,j,k)=one_tenth* &
                           (eta1_ll(k)*pdtop_ll + &
                            eta2_ll(k)*(ten*ps(i,j)-pdtop_ll-pt_ll) + &
                            pt_ll)
        end do
      end do
    end do
  else
    k=1
    k2=nsig+1
    do j=1,lon2
      do i=1,lat2
        prs(i,j,k)=ps(i,j)
        prs(i,j,k2)=zero
      end do
    end do
    do k=2,nsig
      do j=1,lon2
        do i=1,lat2
          if (idvc5.ne.3) then
            prs(i,j,k)=ak5(k)+bk5(k)*ps(i,j)
          else
            trk=(half*(ges_tv(i,j,k-1,it)+ges_tv(i,j,k,it))/tref5(k))**kapr
            prs(i,j,k)=ak5(k)+(bk5(k)*ps(i,j))+(ck5(k)*trk)
          end if
        end do
      end do
    end do
  end if

  return
end subroutine getprs

subroutine getprs_horiz(ps_x,ps_y,mype,prs,prs_x,prs_y)
!
!
!   input argument list:
!     prs      - 3d pressure
!     ps_x     - dps/dx
!     ps_y     - dps/dy
!
!   output argument list:
!     prs_x      - dp/dx
!     prs_y      - dp/dy
!
!$$$
  use kinds,only: r_kind,i_kind
  use constants,only: zero
  use gridmod,only: nsig,lat2,lon2
  use gridmod,only: regional,wrf_nmm_regional,eta2_ll
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2),intent(in):: ps_x,ps_y
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: prs
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(out)::prs_x,prs_y
  integer(i_kind),intent(in):: mype

! Declare local variables
  integer(i_kind) i,j,k,k2

! Declare local parameter
  real(r_kind),parameter:: ten = 10.0_r_kind

  prs_x=zero ; prs_y=zero

  if(wrf_nmm_regional) then
    do k=1,nsig+1
      do j=1,lon2
        do i=1,lat2
          prs_x(i,j,k)=eta2_ll(k)*ps_x(i,j)
          prs_y(i,j,k)=eta2_ll(k)*ps_y(i,j)
        end do
      end do
    end do
  end if

  if (.not.regional) then
    call mp_compact_dlon1(prs,prs_x,.false.,nsig+1,mype)
    call mp_compact_dlat1(prs,prs_y,.false.,nsig+1,mype)
  end if

  return
end subroutine getprs_horiz


subroutine getprs_tl(ps,t,prs)
!
! subprogram:    getprs_tl    TLM of getprs
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: TLM of routine that gets 3d pressure and its derivatives
!
! program history log:
!   2005-09-29  kleist
!   2006-04-12  treadon - replace sigi with bk5
!   2006-07-31  kleist  - analysis variable change from ln(ps) to ps
!   2007-05-08  kleist  - add generalized vert coord and derivative call
!   2007-07-26  cucurull- compute 3d pressure and derivatives in different subroutines 
!                       - remove gues_tv from argument list; clean up code;
!                       - fix buf for t dimension
!
!   input argument list:
!     ps       - surface pressure
!
!   output argument list:
!     prs        - 3d pressure
!
!$$$
  use kinds,only: r_kind,i_kind
  use constants,only: zero,one,rd_over_cp,half
  use jfunc,only: iter
  use gridmod,only: nsig,lat2,lon2,ak5,bk5,ck5,idvc5,tref5
  use gridmod,only: regional,wrf_nmm_regional,eta2_ll
  use guess_grids, only: ges_tv,ntguessig
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2),intent(in):: ps
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: t
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(out):: prs

! Declare local variables
  real(r_kind) kapr,kaprm1,trk,tc1,t9trm
  integer(i_kind) i,j,k,k2,it

  kapr=one/rd_over_cp
  kaprm1=kapr-one
  it=ntguessig
  prs=zero 

  if(wrf_nmm_regional) then
    do k=1,nsig+1
      do j=1,lon2
        do i=1,lat2
          prs(i,j,k)=eta2_ll(k)*ps(i,j)
        end do
      end do
    end do
  else
    k=1
    k2=nsig+1
    do j=1,lon2
      do i=1,lat2
        prs(i,j,k)=ps(i,j)
        prs(i,j,k2)=zero
      end do
    end do
    do k=2,nsig
      do j=1,lon2
        do i=1,lat2
          if (idvc5.ne.3) then
            prs(i,j,k)=bk5(k)*ps(i,j)
          else
            t9trm=half*(ges_tv(i,j,k-1,it)+ges_tv(i,j,k,it))/tref5(k)
            tc1=half/tref5(k)
            trk=kapr*tc1*(t(i,j,k-1)+t(i,j,k))*(t9trm**kaprm1)
            prs(i,j,k)=bk5(k)*ps(i,j) + ck5(k)*trk
          end if
        end do
      end do
    end do
  end if

  return
end subroutine getprs_tl

subroutine getprs_horiz_tl(ps_x,ps_y,mype,prs,prs_x,prs_y)
!
!   input argument list:
!     prs      - 3d pressure
!     ps_x     - dps/dx
!     ps_y     - dps/dy
!
!   output argument list:
!     prs_x      - dp/dx
!     prs_y      - dp/dy
!
!
!$$$
  use kinds,only: r_kind,i_kind
  use constants,only: zero
  use gridmod,only: nsig,lat2,lon2
  use gridmod,only: regional,wrf_nmm_regional,eta2_ll
  implicit none
! Declare passed variables
  real(r_kind),dimension(lat2,lon2),intent(in):: ps_x,ps_y
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: prs
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(out):: prs_x,prs_y
  integer(i_kind),intent(in):: mype

! Declare local variables
  integer(i_kind) i,j,k

  prs_x=zero ; prs_y=zero

  if(wrf_nmm_regional) then
    do k=1,nsig+1
      do j=1,lon2
        do i=1,lat2
            prs_x(i,j,k)=eta2_ll(k)*ps_x(i,j)
            prs_y(i,j,k)=eta2_ll(k)*ps_y(i,j)
        end do
      end do
    end do
  endif

  if (.not.regional) then
    call mp_compact_dlon1(prs,prs_x,.false.,nsig+1,mype)
    call mp_compact_dlat1(prs,prs_y,.false.,nsig+1,mype)
  end if

  return
end subroutine getprs_horiz_tl


subroutine getprs_ad(ps,t,prs)
!
! subprogram:    getprs_ad    adjoint of getprs_tl
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: adjoint of linear routine that gets 3d pressure and derivs
!
! program history log:
!   2005-09-29  kleist
!   2006-04-12  treadon - replace sigi with bk5
!   2006-07-31  kleist  - analysis variable change from ln(ps) to ps
!   2007-05-08  kleist  - add generalized vert coord and derivative call
!   2007-07-26  cucurull- compute 3d pressure and derivatives in different subroutines
!                       - remove gues_tv from argument list; clean up code
!
! usage:
!   input argument list:
!     prs        - 3d pressure
!
!   output argument list:
!     ps       - log surface pressure
!
!  notes:
!     Adjoint check performed and verified on 2005-08-29 by d. kleist
!
!$$$
  use kinds,only: r_kind,i_kind
  use gridmod,only: nsig,lat2,lon2,ak5,bk5,ck5,tref5,idvc5
  use gridmod,only: regional,wrf_nmm_regional,eta2_ll
  use guess_grids, only: ges_tv,ntguessig 
  use constants,only: zero,half,one,rd_over_cp

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout):: prs
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: t
  real(r_kind),dimension(lat2,lon2),intent(inout):: ps

! Declare local variables
  real(r_kind) kapr,kaprm1,trk,tc1,t9trm
  integer(i_kind) i,j,k,it

  kapr=one/rd_over_cp
  kaprm1=kapr-one
  it=ntguessig

  if(wrf_nmm_regional) then
    do k=1,nsig+1
      do j=1,lon2
        do i=1,lat2
          ps(i,j) = ps(i,j) + eta2_ll(k)*prs(i,j,k)
        end do
      end do
    end do
  else
    do k=2,nsig
      do j=1,lon2
        do i=1,lat2
          if (idvc5.ne.3) then
            ps(i,j) = ps(i,j) + bk5(k)*prs(i,j,k)
          else
            t9trm=half*(ges_tv(i,j,k-1,it)+ges_tv(i,j,k,it))/tref5(k)
            tc1=half/tref5(k)
            ps(i,j) = ps(i,j) + bk5(k)*prs(i,j,k)
            trk = ck5(k)*prs(i,j,k)
            t(i,j,k-1) = t(i,j,k-1) + kapr*tc1*trk*(t9trm**kaprm1)
            t(i,j,k)   = t(i,j,k)   + kapr*tc1*trk*(t9trm**kaprm1)
          end if
        end do
      end do
    end do
    k=1
    do j=1,lon2
      do i=1,lat2
        ps(i,j)=ps(i,j) + prs(i,j,k)
      end do
    end do
  end if

  do k=1,nsig+1
   do j=1,lon2
     do i=1,lat2
      prs(i,j,k)=zero
     end do
   end do
  end do 
 
  return
end subroutine getprs_ad


subroutine getprs_horiz_ad(ps_x,ps_y,mype,prs,prs_x,prs_y)
!
!   input argument list:
!     prs_x      - dp/dx
!     prs_y      - dp/dy
!
!   output argument list:
!     prs      - 3d pressure
!     ps_x     - d(ln(ps))/dx
!     ps_y     - d(ln(ps))/dy
!
!  notes:
!     Adjoint check performed and verified on 2005-08-29 by d. kleist
!
!$$$
  use kinds,only: r_kind,i_kind
  use gridmod,only: nsig,lat2,lon2
  use gridmod,only: regional,wrf_nmm_regional,eta2_ll

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: prs_x,prs_y
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout):: prs
  real(r_kind),dimension(lat2,lon2),intent(inout):: ps_x,ps_y
  integer(i_kind),intent(in):: mype

! Declare local variables
  integer(i_kind) i,j,k

! Adjoint of horizontal derivatives
  if (.not.regional) then
    call mp_compact_dlon1_ad(prs,prs_x,.false.,nsig+1,mype)
    call mp_compact_dlat1_ad(prs,prs_y,.false.,nsig+1,mype)
  end if

  if(wrf_nmm_regional) then
    do k=1,nsig+1
      do j=1,lon2
        do i=1,lat2
          ps_y(i,j) = ps_y(i,j) + eta2_ll(k)*prs_y(i,j,k)
          ps_x(i,j) = ps_x(i,j) + eta2_ll(k)*prs_x(i,j,k)
        end do
      end do
    end do
  end if

  return
end subroutine getprs_horiz_ad
