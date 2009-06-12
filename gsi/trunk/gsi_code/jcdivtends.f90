subroutine jcdivtends(u_t,v_t,t_t,p_t,mype,dtrs,atrs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    jcdivtends      divergence tendency
!   prgmmr: kleist           org: np20                date: 2007-04-16
!
! abstract: TLM of routine that gets div and agv tendency 
!
! program history log:
!   2007-04-16  kleist
!
! usage:
!   input argument list:
!     ut        - u tendency on subdomain
!     vt        - v tendency on subdomain
!     tt        - t tendency on subdomain
!     pt        - p tendency on subdomain
!
!   output argument list:
!     dtrs     - rescaled time tendency of divergence
!     atrs     - rescaled time tendency of ageostrophic voriticity
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds,only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,nsig1o
  use mpimod, only: levs_id
  use tendsmod, only: dp_over_pbar,t_over_pbar
  use constants, only: rd,zero,half
  implicit none

! Passed Variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: p_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: dtrs,atrs
  integer(i_kind),intent(in):: mype
  
! Local Variables
  real(r_kind),dimension(lat2,lon2):: sumkm1,sum2km1,termt,sumk,sum2k
  real(r_kind),dimension(lat2,lon2,nsig):: div_t,agv_t,massv_t,utx,vty,futy,&
       fvtx,philap
  real(r_kind) tmp,tmp2
  integer(i_kind) i,j,k,nnn

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        massv_t(i,j,k)=zero
        utx(i,j,k)=zero
        vty(i,j,k)=zero
        futy(i,j,k)=zero
        fvtx(i,j,k)=zero
        philap(i,j,k)=zero
      end do
    end do
  end do

  do j=1,lon2
    do i=1,lat2
      sumkm1(i,j)=zero
      sum2km1(i,j)=zero
      termt(i,j)=zero
      sumk(i,j)=zero
      sum2k(i,j)=zero
    end do
  end do

  do k=1,nsig
    tmp = rd*t_over_pbar(k)
    tmp2 = dp_over_pbar(k)
    do j=1,lon2
      do i=1,lat2
        termt(i,j)=(p_t(i,j,k)+p_t(i,j,k+1))*tmp
      end do
    end do

    do j=1,lon2
      do i=1,lat2
        sumk(i,j)=tmp* ( (p_t(i,j,k)-p_t(i,j,k+1)) - &
             tmp2*(p_t(i,j,k)+p_t(i,j,k+1)) )
        sum2k(i,j)= t_t(i,j,k)*tmp2
        termt(i,j) = termt(i,j) + sumkm1(i,j) + rd*sum2km1(i,j) + &
             half*sumk(i,j) + half*rd*sum2k(i,j)

! load up the km1 arrays for next k loop
        sumkm1(i,j)=sumkm1(i,j)+sumk(i,j)
        sum2km1(i,j)=sum2km1(i,j)+sum2k(i,j)
      end do
    end do
    do j=1,lon2
      do i=1,lat2
        massv_t(i,j,k)=termt(i,j)
      end do
    end do
  end do  ! end do k
! get derivatives here
  nnn=0
  do k=1,nsig1o
    if (levs_id(k)/=0) nnn=nnn+1
  end do
  call get_tend_derivs(u_t,v_t,massv_t,utx,vty,futy,fvtx,philap,nnn,mype)

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        div_t(i,j,k)=utx(i,j,k) + vty(i,j,k)
        agv_t(i,j,k)=fvtx(i,j,k)-futy(i,j,k)-philap(i,j,k)
      end do
    end do
  end do

! Call rescaling routine to put in units of energy
  dtrs=zero ; atrs=zero
  call jcrescale(div_t,agv_t,dtrs,atrs)

  return
end subroutine jcdivtends

subroutine jcdivtends_ad(u_t,v_t,t_t,p_t,mype,dtrs,atrs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    jcdivtends_ad      adjoint of divergence tendency
!   prgmmr: kleist           org: np20                date: 2007-04-16
!
! abstract: ADJ of routine that gets div and agv tendency
!
! program history log:
!   2007-04-16  kleist
!
! usage:
!   input argument list:
!     dtrs     - rescaled time tendency of divergence
!     atrs     - rescaled time tendency of ageostrophic voriticity
!
!   output argument list:
!     ut        - u tendency on subdomain
!     vt        - v tendency on subdomain
!     tt        - t tendency on subdomain
!     pt        - p tendency on subdomain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds,only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,nsig1o
  use mpimod, only: levs_id
  use tendsmod, only: dp_over_pbar,t_over_pbar
  use constants, only: rd,zero,half
  implicit none

! Passed Variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout):: p_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: dtrs,atrs
  integer(i_kind),intent(in):: mype

! Local Variables
  real(r_kind),dimension(lat2,lon2):: sumkm1,sum2km1,termt,sumk,sum2k
  real(r_kind),dimension(lat2,lon2,nsig):: div_t,agv_t,massv_t,utx,vty,futy,&
       fvtx,philap
  real(r_kind),dimension(lat2,lon2,nsig):: ut_tmp,vt_tmp,tt_tmp
  real(r_kind),dimension(lat2,lon2,nsig+1):: pt_tmp
  real(r_kind) tmp,tmp2
  integer(i_kind) i,j,k,nnn

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        ut_tmp(i,j,k)=zero
        vt_tmp(i,j,k)=zero
        tt_tmp(i,j,k)=zero
        pt_tmp(i,j,k)=zero
        massv_t(i,j,k) = zero
        div_t(i,j,k)=zero
        agv_t(i,j,k)=zero
      end do
    end do
  end do
  k=nsig+1
  do j=1,lon2
    do i=1,lat2
      pt_tmp(i,j,k)=zero
    end do
  end do

  call jcrescale_ad(div_t,agv_t,dtrs,atrs)
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        fvtx(i,j,k) = agv_t(i,j,k)
        futy(i,j,k) = -agv_t(i,j,k)
        philap(i,j,k) = -agv_t(i,j,k)
        utx(i,j,k) = div_t(i,j,k)
        vty(i,j,k) = div_t(i,j,k)
      end do
    end do
  end do

! adjoint of get derivatives
  nnn=0
  do k=1,nsig1o
    if (levs_id(k)/=0) nnn=nnn+1
  end do
  call tget_tend_derivs(ut_tmp,vt_tmp,massv_t,utx,vty,futy,fvtx,philap,nnn,mype)
! adjoint of transforation to mass variable tendency
  do k=nsig,1,-1
    tmp = rd*t_over_pbar(k)
    tmp2 = dp_over_pbar(k)
    do j=1,lon2
      do i=1,lat2
        termt(i,j)=massv_t(i,j,k)
      end do
    end do
    do j=1,lon2
      do i=1,lat2
        sumk(i,j)=sumkm1(i,j) + half*termt(i,j)
        sum2k(i,j)=sum2km1(i,j) + half*rd*termt(i,j)
        sumkm1(i,j) = sumkm1(i,j) + termt(i,j)
        sum2km1(i,j) = sum2km1(i,j) + rd*termt(i,j)
       
        tt_tmp(i,j,k)=tt_tmp(i,j,k) + sum2k(i,j)*tmp2
        pt_tmp(i,j,k)=pt_tmp(i,j,k) - tmp*tmp2*sumk(i,j) + &
             tmp*sumk(i,j)
        pt_tmp(i,j,k+1)=pt_tmp(i,j,k+1) - tmp*tmp2*sumk(i,j) - &
             tmp*sumk(i,j)
      end do
    end do
    do j=1,lon2
      do i=1,lat2
        pt_tmp(i,j,k) = pt_tmp(i,j,k) + tmp*termt(i,j)
        pt_tmp(i,j,k+1) = pt_tmp(i,j,k+1) + tmp*termt(i,j)
      end do
    end do
  end do  ! end do k

! ADD CONTRIBUTION FROM _TMP VARS BACK IN
  do k=1,nsig+1
    do j=1,lon2
      do i=1,lat2
        p_t(i,j,k)=p_t(i,j,k)+pt_tmp(i,j,k)
      end do
    end do
  end do
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        t_t(i,j,k)=t_t(i,j,k)+tt_tmp(i,j,k)
        u_t(i,j,k)=u_t(i,j,k)+ut_tmp(i,j,k)
        v_t(i,j,k)=v_t(i,j,k)+vt_tmp(i,j,k)
      end do
    end do
  end do

  return
end subroutine jcdivtends_ad

subroutine jcrescale(divt,agvt,dtrs,atrs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    jcrescale     
!
!   prgmmr: kleist           org: np22                date: 2006-08-01
!
! abstract: rescales divergence and ageostrophic vorticity 
!           tendencies by inverse Laplacian for dynamic constraint
!           term
!
! program history log:
!   2006-08-01  kleist
!
!   input argument list:
!     divt      - divergence tendency
!     agvt      - ageostrophic vorticity tendency 
!
!   output argument list:
!     dtrs      - rescaled divt 
!     atrs      - rescaled agvt 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,lat2,nsig,iglobal,lon1,itotsub,lon2,lat1,&
       nlat,nlon,ltosi,ltosj,ltosi_s,ltosj_s
  use mpimod, only: iscuv_s,ierror,mpi_comm_world,irduv_s,ircuv_s,&
       isduv_s,isduv_g,iscuv_g,nuvlevs,irduv_g,ircuv_g,mpi_rtype,&
       strip,reorder,reorder2
  use specmod, only: ncd2,nc
  use jcmod, only: jcresc1,jcresc2
  use mod_vtrans, only: nvmodes_keep,vtrans2,vtrans2_inv
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: divt,agvt
  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: dtrs,atrs

! Declare local variables
  integer(i_kind) i,j,k,kk,isize,ni1,ni2

  real(r_kind),dimension(lat1,lon1,nsig):: dsm,asm
  real(r_kind),dimension(lat2,lon2,nsig):: dtmp,atmp
  real(r_kind),dimension(itotsub,nuvlevs):: work1,work2
  real(r_kind),dimension(nlat,nlon):: work3,work4
  real(r_kind),dimension(nc):: spc1,spc2
  real(r_kind),dimension(:,:,:),allocatable:: dhat,ahat

! Initialize variables
  isize=max(iglobal,itotsub)

! Allocate vertical mode arrays
  allocate(dhat(lat2,lon2,nvmodes_keep),ahat(lat2,lon2,nvmodes_keep))

! zero out output arrays
  dtrs=zero ; atrs=zero ; dhat=zero ; ahat=zero

! zero out work arrays
  do k=1,nuvlevs
    do j=1,isize
      work1(j,k)=zero
      work2(j,k)=zero
    end do
  end do

! do vertical mode decomp here
  call vtrans2(divt,agvt,dhat,ahat)
  
! NOW put dhat/ahat back into divt/agvt
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        if (k.le.nvmodes_keep) then
           dtmp(i,j,k)=dhat(i,j,k)
           atmp(i,j,k)=ahat(i,j,k)
         else 
           dtmp(i,j,k)=zero
           atmp(i,j,k)=zero
         end if
      end do
    end do
  end do

! The following are now actually operating on
! vertical mode structures
!   strip off endpoints arrays on subdomains
    call strip(dtmp,dsm,nsig)
    call strip(atmp,asm,nsig)

!   put st/vp on global slabs
    call mpi_alltoallv(dsm(1,1,1),iscuv_g,isduv_g,&
         mpi_rtype,work1(1,1),ircuv_g,irduv_g,mpi_rtype,&
         mpi_comm_world,ierror)
    call mpi_alltoallv(asm(1,1,1),iscuv_g,isduv_g,&
         mpi_rtype,work2(1,1),ircuv_g,irduv_g,mpi_rtype,&
         mpi_comm_world,ierror)

!   reorder work arrays before calling of 
!   st,vp --> u,v routine
    call reorder(work1,nuvlevs)
    call reorder(work2,nuvlevs)

!   perform scalar g2s on work array
    do k=1,nuvlevs
      spc1=zero ; spc2=zero

      work3=zero ; work4=zero
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        work3(ni1,ni2)=work1(kk,k)
        work4(ni1,ni2)=work2(kk,k)
      end do

      call g2s0(spc1,work3)
      call g2s0(spc2,work4)

!   inverse laplacian
      do i=2,ncd2
        spc1(2*i-1)=spc1(2*i-1)*jcresc1(i)
        spc1(2*i)=spc1(2*i)*jcresc1(i)
        spc2(2*i-1)=spc2(2*i-1)*jcresc2(i,k)
        spc2(2*i)=spc2(2*i)*jcresc2(i,k)
      end do

      work3=zero ; work4=zero
      call s2g0(spc1,work3)
      call s2g0(spc2,work4)

      do kk=1,itotsub
        ni1=ltosi_s(kk); ni2=ltosj_s(kk)
        work1(kk,k)=work3(ni1,ni2)
        work2(kk,k)=work4(ni1,ni2)
      end do
    end do  !end do nuvlevs

!   reorder the work array for the mpi communication
    call reorder2(work1,nuvlevs)
    call reorder2(work2,nuvlevs)

!   get u,v back on subdomains
    call mpi_alltoallv(work1(1,1),iscuv_s,isduv_s,&
         mpi_rtype,dtrs(1,1,1),ircuv_s,irduv_s,mpi_rtype,&
         mpi_comm_world,ierror)
    call mpi_alltoallv(work2(1,1),iscuv_s,isduv_s,&
         mpi_rtype,atrs(1,1,1),ircuv_s,irduv_s,mpi_rtype,&
         mpi_comm_world,ierror)

! now put dtrs and atrs back into full subdomain fields
! from the mode coeffs.
    dhat=zero ; ahat=zero
    do k=1,nvmodes_keep
      do j=1,lon2
        do i=1,lat2
          dhat(i,j,k)=dtrs(i,j,k)
          ahat(i,j,k)=atrs(i,j,k)
        end do
      end do
    end do

    call vtrans2_inv(dhat,ahat,dtrs,atrs)

  return
end subroutine jcrescale


subroutine jcrescale_ad(divt,agvt,dtrs,atrs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    jcrescale_ad
!
!   prgmmr: kleist           org: np22                date: 2006-08-01
!
! abstract: adjoint of divergence and ageostrophic vorticity tendency
!           rescaling routine
!
! program history log:
!   2006-08-01  kleist
!
!   input argument list:
!     divt      - divergence tendency
!     agvt      - ageostrophic vorticity tendency
!
!   output argument list:
!     dtrs      - rescaled divt
!     atrs      - rescaled agvt
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,lat2,nsig,iglobal,lon1,itotsub,lon2,lat1,&
       nlat,nlon,ltosi,ltosj,ltosi_s,ltosj_s
  use mpimod, only: iscuv_s,ierror,mpi_comm_world,irduv_s,ircuv_s,&
       isduv_s,isduv_g,iscuv_g,nuvlevs,irduv_g,ircuv_g,mpi_rtype,&
       strip,reorder,reorder2
  use specmod, only: enn1,ncd2,nc
  use mod_vtrans, only: nvmodes_keep,vtrans2_ad,vtrans2_inv_ad
  use jcmod, only: jcresc1,jcresc2
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: dtrs,atrs
  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: divt,agvt

! Declare local variables
  integer(i_kind) i,j,k,kk,isize,ni1,ni2

  real(r_kind),dimension(lat1,lon1,nsig):: dsm,asm
  real(r_kind),dimension(itotsub,nuvlevs):: work1,work2
  real(r_kind),dimension(nlat,nlon):: work3,work4
  real(r_kind),dimension(nc):: spc1,spc2
  real(r_kind),dimension(:,:,:),allocatable:: dhat,ahat
  real(r_kind),dimension(lat2,lon2,nsig):: dtmp,atmp


! Initialize variables
  isize=max(iglobal,itotsub)

  allocate( dhat(lat2,lon2,nvmodes_keep),ahat(lat2,lon2,nvmodes_keep) )

! zero out work arrays
  do k=1,nuvlevs
    do j=1,isize
      work1(j,k)=zero
      work2(j,k)=zero
    end do
  end do

  dhat=zero ; ahat=zero ; dtmp=zero ; atmp=zero
  call vtrans2_inv_ad(dhat,ahat,dtrs,atrs)

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        if (k.le.nvmodes_keep) then
          dtmp(i,j,k)=dhat(i,j,k)
          atmp(i,j,k)=ahat(i,j,k)
        end if
      end do
    end do
  end do

!   strip off endpoints arrays on subdomains
    call strip(dtmp,dsm,nsig)
    call strip(atmp,asm,nsig)

!   put st/vp on global slabs
    call mpi_alltoallv(dsm(1,1,1),iscuv_g,isduv_g,&
         mpi_rtype,work1(1,1),ircuv_g,irduv_g,mpi_rtype,&
         mpi_comm_world,ierror)
    call mpi_alltoallv(asm(1,1,1),iscuv_g,isduv_g,&
         mpi_rtype,work2(1,1),ircuv_g,irduv_g,mpi_rtype,&
         mpi_comm_world,ierror)

!   reorder work arrays before calling of
!   st,vp --> u,v routine
    call reorder(work1,nuvlevs)
    call reorder(work2,nuvlevs)

!   perform scalar g2s on work array
    do k=1,nuvlevs
      spc1=zero ; spc2=zero

      work3=zero ; work4=zero
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        work3(ni1,ni2)=work1(kk,k)
        work4(ni1,ni2)=work2(kk,k)
      end do

      call s2g0_ad(spc1,work3)
      call s2g0_ad(spc2,work4)

! adjoint of inverse laplacian
      do i=2,ncd2
        spc1(2*i-1)=spc1(2*i-1)*jcresc1(i)
        spc1(2*i)=spc1(2*i)*jcresc1(i)
        spc2(2*i-1)=spc2(2*i-1)*jcresc2(i,k)
        spc2(2*i)=spc2(2*i)*jcresc2(i,k)
      end do

      work3=zero ; work4=zero
      call g2s0_ad(spc1,work3)
      call g2s0_ad(spc2,work4)

      do kk=1,itotsub
        ni1=ltosi_s(kk); ni2=ltosj_s(kk)
        work1(kk,k)=work3(ni1,ni2)
        work2(kk,k)=work4(ni1,ni2)
      end do

    end do  !end do nuvlevs

!   reorder the work array for the mpi communication
    call reorder2(work1,nuvlevs)
    call reorder2(work2,nuvlevs)

!   get u,v back on subdomains
    call mpi_alltoallv(work1(1,1),iscuv_s,isduv_s,&
         mpi_rtype,dtmp(1,1,1),ircuv_s,irduv_s,mpi_rtype,&
         mpi_comm_world,ierror)
    call mpi_alltoallv(work2(1,1),iscuv_s,isduv_s,&
         mpi_rtype,atmp(1,1,1),ircuv_s,irduv_s,mpi_rtype,&
         mpi_comm_world,ierror)

    do k=1,nvmodes_keep
      do j=1,lon2
        do i=1,lat2
          dhat(i,j,k)=dtmp(i,j,k)
          ahat(i,j,k)=atmp(i,j,k)
        end do
      end do
    end do

    divt=zero ; agvt=zero
    call vtrans2_ad(divt,agvt,dhat,ahat)

  return
end subroutine jcrescale_ad
