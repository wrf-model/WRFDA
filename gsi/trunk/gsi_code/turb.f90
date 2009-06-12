subroutine turb(pges,tges,uges,vges,oges,z,p_t,termu,termv,termt)
!$$$
!    Calculate tendencies of wind and virtual temperature due to
!    vertical turbulent mixing 
!$$$
  use constants,only: rd_over_cp
  use kinds,only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  use turbmod, only: pim1,pim2,pim3,pih1,pih2,pih3,use_pbl
  implicit none

! Declare local parameters
  real(r_kind),parameter:: r200 = 200.0_r_kind

! Declare passed variables
  real(r_kind),dimension(lat2,lon2),intent(in):: z
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: pges,p_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: uges,vges,tges
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: termu,termv,termt,oges

! Declare local variables
  real(r_kind):: px
  integer(i_kind) i,j,k
  
  if(.not. use_pbl)return
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
       oges(i,j,k)=tges(i,j,k)*(                  &
                 r200/( pges(i,j,k)+pges(i,j,k+1) ))**rd_over_cp
      end do
    end do
  end do

  call turbvars(pges,tges,uges,vges,oges,z)
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        termt(i,j,k)=termt(i,j,k)+tges(i,j,k)*(p_t (i,j,k)+p_t (i,j,k+1))* &
                rd_over_cp/(pges(i,j,k)+pges(i,j,k+1))
        px=tges(i,j,k)/oges(i,j,k)
        termu(i,j,k)=termu(i,j,k)+pim2(i,j,k)*uges(i,j,k  )
        termv(i,j,k)=termv(i,j,k)+pim2(i,j,k)*vges(i,j,k  )
        termt(i,j,k)=termt(i,j,k)+pih2(i,j,k)*oges(i,j,k  )*px
        if(k > 1)then
          termu(i,j,k)=termu(i,j,k)+pim1(i,j,k)*uges(i,j,k-1)
          termv(i,j,k)=termv(i,j,k)+pim1(i,j,k)*vges(i,j,k-1)
          termt(i,j,k)=termt(i,j,k)+pih1(i,j,k)*oges(i,j,k-1)*px
        end if
        if(k < nsig)then
          termu(i,j,k)=termu(i,j,k)+pim3(i,j,k)*uges(i,j,k+1)
          termv(i,j,k)=termv(i,j,k)+pim3(i,j,k)*vges(i,j,k+1)
          termt(i,j,k)=termt(i,j,k)+pih3(i,j,k)*oges(i,j,k+1)*px
        end if
      end do
    end do
  end do
  return
end subroutine turb

subroutine turb_tl(pges,tges,oges,t,u,v,prs,p_t,termu,termv,termt)
!$$$
!    Tangent linear of turb
!$$$
  use constants,only: rd_over_cp
  use kinds,only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  use turbmod, only: pim1,pim2,pim3,pih1,pih2,pih3,use_pbl
  implicit none

! Declare local parameters
  real(r_kind),parameter:: r200 = 200.0_r_kind

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: pges,p_t,prs
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: t,u,v,tges,oges
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: termu,termv,termt

! Declare local variables
  real(r_kind),dimension(nsig):: turbu,turbv,turbo
  real(r_kind),dimension(lat2,lon2,nsig):: o
  real(r_kind):: px
  integer(i_kind) i,j,k

  if(.not. use_pbl)return
  
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        px=rd_over_cp/(pges(i,j,k)+pges(i,j,k+1))
        o(i,j,k)=oges(i,j,k)*(t(i,j,k)/tges(i,j,k)-px*(prs(i,j,k)+prs(i,j,k+1))) 
        termt(i,j,k)=termt(i,j,k)+tges(i,j,k)*px*(p_t (i,j,k)+p_t (i,j,k+1))
      end do
    end do
  end do
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        px=tges(i,j,k)/oges(i,j,k)
        termu(i,j,k)=termu(i,j,k)+pim2(i,j,k)*u(i,j,k  )
        termv(i,j,k)=termv(i,j,k)+pim2(i,j,k)*v(i,j,k  )
        termt(i,j,k)=termt(i,j,k)+pih2(i,j,k)*o(i,j,k  )*px
        if(k > 1)then
          termu(i,j,k)=termu(i,j,k)+pim1(i,j,k)*u(i,j,k-1)
          termv(i,j,k)=termv(i,j,k)+pim1(i,j,k)*v(i,j,k-1)
          termt(i,j,k)=termt(i,j,k)+pih1(i,j,k)*o(i,j,k-1)*px
        end if
        if(k < nsig)then
          termu(i,j,k)=termu(i,j,k)+pim3(i,j,k)*u(i,j,k+1)
          termv(i,j,k)=termv(i,j,k)+pim3(i,j,k)*v(i,j,k+1)
          termt(i,j,k)=termt(i,j,k)+pih3(i,j,k)*o(i,j,k+1)*px
        end if
      end do
    end do
  end do

  return
end subroutine turb_tl

subroutine turb_ad(pges,tges,oges,termu,termv,termt,t,u,v,prs,p_t)
!$$$
!    Adjoint of turb
!$$$
  use constants,only: rd_over_cp,zero
  use kinds,only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  use turbmod, only: pim1,pim2,pim3,pih1,pih2,pih3,use_pbl
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: tges,oges
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: pges
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: termu,termv,termt
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: u,v,t
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout):: p_t,prs

! Declare local parameters
  real(r_kind),parameter:: r200 = 200.0_r_kind

! Declare local variables
  real(r_kind),dimension(nsig):: turbu,turbv,turbo
  real(r_kind),dimension(lat2,lon2,nsig):: o
  integer(i_kind) i,j,k
  real(r_kind) paux,taux,px
  
  if(.not. use_pbl)return
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        o(i,j,k)=zero
      end do
    end do
  end do
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        px=termt(i,j,k)*tges(i,j,k)/oges(i,j,k)
        u(i,j,k)=u(i,j,k)+pim2(i,j,k)*termu(i,j,k)
        v(i,j,k)=v(i,j,k)+pim2(i,j,k)*termv(i,j,k)
        o(i,j,k)=o(i,j,k)+pih2(i,j,k)*px
        if(k > 1)then
          u(i,j,k-1)=u(i,j,k-1)+pim1(i,j,k)*termu(i,j,k)
          v(i,j,k-1)=v(i,j,k-1)+pim1(i,j,k)*termv(i,j,k)
          o(i,j,k-1)=o(i,j,k-1)+pih1(i,j,k)*px
        end if
        if(k < nsig)then
          u(i,j,k+1)=u(i,j,k+1)+pim3(i,j,k)*termu(i,j,k)
          v(i,j,k+1)=v(i,j,k+1)+pim3(i,j,k)*termv(i,j,k)
          o(i,j,k+1)=o(i,j,k+1)+pih3(i,j,k)*px
        end if
      end do
    end do
  end do
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        t(i,j,k)=t(i,j,k)+oges(i,j,k)*o(i,j,k)/tges(i,j,k) 
        px=rd_over_cp/(pges(i,j,k)+pges(i,j,k+1))
        paux=-o(i,j,k)*px*oges(i,j,k)
        prs(i,j,k  )=prs(i,j,k  )+paux
        prs(i,j,k+1)=prs(i,j,k+1)+paux
        taux=tges(i,j,k)*px*termt(i,j,k)
        p_t(i,j,k  )=p_t(i,j,k  )+taux
        p_t(i,j,k+1)=p_t(i,j,k+1)+taux
      end do
    end do
  end do

  return
end subroutine turb_ad
