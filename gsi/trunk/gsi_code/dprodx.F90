module dprodxmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dprodxmod    module for dprodx and its tangent linear dprodx_tl
!
! abstract: module for dprodx and its tangent linear dprodx_tl
!
! program history log:
!   2005-05-11  Yanqiu zhu - wrap dprodx and its tangent linear dprodx_tl into one module
!
implicit none

PRIVATE
PUBLIC dprodx
PUBLIC dprodx_tl
PUBLIC dplev
PUBLIC fplev
PUBLIC dplev_tl

contains

#ifdef ibm_sp
real(r_double) function dprodx(n1,nlx,dx,dy,yd,xd,out1,iter,mype)
#else
real(8) function dprodx(n1,nlx,dx,dy,yd,xd,out1,iter,mype)
#endif
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dprodx   calculates dot product for control vector type vectors
!   prgmmr: derber           org: np23                  date: 2004-05-13
!
! abstract: calculates dot product for control vector type vectors.  Note first n1
!           elements independent n1-nlx duplicated between vectors
!
! program history log:
!   2004-05-13  derber, document
!   2004-07-15  treadon - cosmetic cleanup
!   2004-07-28  treadon - add only on use declarations; add intent in/out
!   2004-12-03  treadon - replace mpe_iallreduce (IBM extension) with
!                         standard mpi_allreduce
!   2005-05-27  derber  - modified to reflect changes in minimization
!   2005-06-06  treadon - comment out computation of dx*dx since not used
!   2005-11-20  derber  - modify to improve reproducibility
!
!   input argument list:
!     n1       - length of independent control vector
!     nlx      - length of control vector
!     dx       - input vector 1
!     dy       - input vector 2
!     yd       - input vector 3
!     xd       - input vector 4
!     iter     - iteration number
!
!   output argument list
!     out1     - array containing 2 dot product
!              - out(1) = dx*dy
!              - out(2) = yd*dy
!              - out(3) = dx*dx
!     dprodx   - out(2)/out(1)from previous iteration
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,r_double,i_kind,r_quad
  use constants, only: zero,izero

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: n1,nlx,iter,mype  	
  real(r_kind),dimension(nlx),intent(in)::dx,dy,yd,xd
  real(r_kind),dimension(3),intent(out):: out1

! Declare local variables
  integer(i_kind) i,j,k

  real(r_kind):: out1save,t1,t2
  real(r_quad):: b1,b2

! Zero solution
  out1save = out1(1)
  out1(1) = zero
  out1(2) = zero
  out1(3) = zero
  dprodx = zero
  if(nlx <= izero)return

! Code for independent part of vector
  if(n1 > izero ) then
     t1=dplev(dx,dy,mype)
     t2=dplev(yd,dy,mype)

  end if


! Dot product for duplicated part of vector.  Done on all processors so no
! communication necessary
  b1=zero
  b2=zero
  if(nlx-n1 > izero ) then
     do i = n1+1,nlx
        b1 = b1 + dx(i)*dy(i)
        b2 = b2 + yd(i)*dy(i)
!       out1(3) = out1(3) + dx(i)*dx(i)
     end do
  end if
  out1(1)=t1+b1
  out1(2)=t2+b2
! if(mype == 0)write(6,*)' t1,t2,b1,b2',t1,t2,b1,b2

! Sum independent and duplicated parts

  if(out1save > 1.e-16 .and. iter > 0)dprodx = out1(2)/out1save

  return
end function dprodx

subroutine dprodx_tl(n1,nlx,dx,dy,yd,xd,out1,iter,mype,dprodxtmp, &
                          dx_tl,dy_tl,yd_tl,xd_tl,out1_tl,dprodxtmp_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dprodx_tl   the tangent linear of the operator that calculates 
!                               dot product for control vector type vectors
!   prgmmr: yanqiu zhu           org: GMAO                  date: 2005-03-31
!
! abstract: the tangent linear of the operator that calculates dot product for control 
!           vector type vectors.  Note first n1 elements independent n1-nlx duplicated 
!           between vectors
!
! program history log:
!   2005-03-31  yanqiu zhu - tangent linear of dprodx
!   2005-05-27  derber  - modified to reflect changes in minimization
!   2005-06-06  treadon - comment out computation of dx*dx since not used
!   2005-11-20  derber  - modify to improve reproducibility
!
!   input argument list:
!     n1       - length of independent control vector
!     nlx      - length of control vector
!     dx       - input vector 1
!     dy       - input vector 2
!     yd       - input vector 3
!     xd       - input vector 4
!     dx_tl     - input tangent linear vector 1
!     dy_tl     - input tangent linear vector 2
!     yd_tl     - input tangent linear vector 3
!     xd_tl     - input tangent linear vector 4
!     iter     - iteration number
!
!   output argument list
!     out1     - array containing 2 dot product
!              - out(1) = dx*dy
!              - out(2) = yd*dy
!              - out(3) = dx*dx
!     dprodxtmp- out(2)/out(1)from previous iteration
!     out1_tl      - tangent linear of out1
!     dprodxtmp_tl - tangent linear of dprodxtmp
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,r_double,i_kind
  use constants, only: zero,izero
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype,mpi_sum,npe
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: n1,nlx,iter,mype
  real(r_kind),dimension(nlx),intent(in)::dx,dy,yd,xd
  real(r_kind),dimension(3),intent(out):: out1
  real(r_kind),dimension(nlx),intent(in)::dx_tl,dy_tl,yd_tl,xd_tl
  real(r_kind),dimension(3),intent(out):: out1_tl

! Declare local variables
  integer(i_kind) i,j,mm1
  real(r_kind),dimension(3,npe):: dtemp1,dtemp2
  real(r_kind):: out1save
  real(r_kind),dimension(3,npe):: dtemp1_tl,dtemp2_tl
  real(r_kind):: out1save_tl
#ifdef ibm_sp
  real(r_double) :: dprodxtmp
  real(r_double) :: dprodxtmp_tl
  real(r_double) :: dplevtmp
  real(r_double) :: dplevtmp_tl
#else
  real(8) :: dprodxtmp
  real(8) :: dprodxtmp_tl
  real(8) :: dplevtmp
  real(8) :: dplevtmp_tl
#endif


! Zero solution
  out1save = out1(1)
  out1(1) = zero
  out1(2) = zero
  out1(3) = zero
  dprodxtmp = zero

  out1save_tl = out1_tl(1)
  out1_tl(1) = zero
  out1_tl(2) = zero
  out1_tl(3) = zero
  dprodxtmp_tl = zero

  if(nlx <= izero)return


! Code for independent part of vector
  if(n1 > izero ) then
     mm1=mype+1
     dtemp1=zero
     dtemp1_tl=zero
     call dplev_tl(dx,dy,dplevtmp,dx_tl,dy_tl,dplevtmp_tl)
     dtemp1(1,mm1)=dplevtmp
     dtemp1_tl(1,mm1)=dplevtmp_tl
     call dplev_tl(yd,dy,dplevtmp,yd_tl,dy_tl,dplevtmp_tl)
     dtemp1(2,mm1)=dplevtmp
     dtemp1_tl(2,mm1)=dplevtmp_tl
!    call dplev_tl(dx,dx,dplevtmp,dx_tl,dx_tl,dplevtmp_tl)
!    dtemp1(3,mm1)=dplevtmp
!    dtemp1_tl(3,mm1)=dplevtmp_tl
     dtemp1(3,mm1)  = zero
     dtemp1_tl(3,mm1)= zero
     call mpi_allreduce(dtemp1,dtemp2,3*npe,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
     call mpi_allreduce(dtemp1_tl,dtemp2_tl,3*npe,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
     do j=1,3
      do i=1,npe
       out1(j)=out1(j)+dtemp2(j,i)
       out1_tl(j)=out1_tl(j)+dtemp2_tl(j,i)
      end do
     end do
  end if


! Dot product for duplicated part of vector.  Done on all processors so no
! communication necessary
  if(nlx-n1 > izero ) then
     do i = n1+1,nlx
        out1_tl(1) = out1_tl(1) + dx_tl(i)*dy(i) + dx(i)*dy_tl(i)
        out1_tl(2) = out1_tl(2) + yd_tl(i)*dy(i) + yd(i)*dy_tl(i)
!       out1_tl(3) = out1_tl(3) + dx_tl(i)*dx(i) + dx(i)*dx_tl(i)
        out1(1) = out1(1) + dx(i)*dy(i)
        out1(2) = out1(2) + yd(i)*dy(i)
!       out1(3) = out1(3) + dx(i)*dx(i)
     end do
  end if

! Sum independent and duplicated parts
  if(out1save > 1.e-16 .and. iter > 0) then
     dprodxtmp = out1(2)/out1save
     dprodxtmp_tl = out1_tl(2)/out1save - out1(2)/(out1save*out1save)*out1save_tl
  end if
  return
end subroutine dprodx_tl


#ifdef ibm_sp
real(r_double) function dplev(dx,dy,mype)
#else
real(8) function dplev(dx,dy,mype)
#endif

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dplev   calculates dot product for data on subdomain
!   prgmmr: derber           org: np23                date: 2004-05-13
!
! abstract: calculates dot product for data on subdomain.  Note loops over
!           streamfunction, velocity potential, temperature, etc. Also, only 
!           over interior of subdomain.
!
! program history log:
!   2004-05-13  derber, document
!   2004-07-28  treadon - add only on use declarations; add intent in/out
!
!   input argument list:
!     dx       - input vector 1
!     dy       - input vector 2
!
!   output argument list
!     dplev    - dot product
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  
  use kinds, only: r_kind,r_double,i_kind
  use jfunc, only:  nval_levs
  use gridmod, only:  nlat,nlon,lat2,lon2,lat1,lon1,&
     ltosi,ltosj,iglobal,itotsub,ijn,displs_g
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype,strip
  use constants, only:  zero
  implicit none

! Declar passed variables
  real(r_kind),dimension(lat2,lon2,nval_levs),intent(in)::dx,dy
  integer(i_kind),intent(in)::mype

! Declare local variables
  real(r_kind),dimension(lat1*lon1):: zsm
  real(r_kind),dimension(iglobal):: work1
  real(r_kind),dimension(lat2,lon2):: sum
  real(r_kind),dimension(nlat,nlon):: sumall

  integer(i_kind) i,j,k,mm1,kk
  real(r_kind) e,y,temp
  
  mm1=mype+1
  sum=zero
  do k=1,nval_levs
     do j=1,lon2
        do i=1,lat2
           sum(i,j)=sum(i,j)+dx(i,j,k)*dy(i,j,k)
        end do
     end do
  end do
  do j=1,lon1*lat1
    zsm(j)=zero
  end do

  call strip(sum,zsm,1)

  call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
     work1,ijn,displs_g,mpi_rtype,&
     mpi_comm_world,ierror)

  do k=1,iglobal
    i=ltosi(k) ; j=ltosj(k)
    sumall(i,j)=work1(k)
  end do
! if(mype == 0)then
! do j=1,nlon
!   do i=1,nlat
!     write(500,*)i,j,sumall(i,j)
!   end do
! end do
! end if
  dplev=zero
  e=zero
  do j=1,nlon
    do i=1,nlat
!  Compensated summation version of sum
      temp=dplev
      y=sumall(i,j)+e
      dplev=temp+y
      e=(temp-dplev)+y
!     dplev=dplev+sumall(i,j)
    end do
  end do
    
  return
end function dplev
#ifdef ibm_sp
real(r_double) function fplev(dx,mype)
#else
real(8) function fplev(dx,mype)
#endif

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dplev   calculates dot product for data on subdomain
!   prgmmr: derber           org: np23                date: 2004-05-13
!
! abstract: calculates dot product for data on subdomain.  Note loops over
!           streamfunction, velocity potential, temperature, etc. Also, only 
!           over interior of subdomain.
!
! program history log:
!   2004-05-13  derber, document
!   2004-07-28  treadon - add only on use declarations; add intent in/out
!
!   input argument list:
!     dx       - input vector 1
!     dy       - input vector 2
!
!   output argument list
!     dplev    - dot product
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  
  use kinds, only: r_kind,r_double,i_kind
  use jfunc, only:  nval_levs
  use gridmod, only:  nlat,nlon,lat2,lon2,lat1,lon1,&
     ltosi,ltosj,iglobal,itotsub,ijn,displs_g
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype,strip
  use constants, only:  zero
  implicit none

! Declar passed variables
  real(r_kind),dimension(lat2,lon2),intent(in)::dx
  integer(i_kind),intent(in)::mype

! Declare local variables
  real(r_kind),dimension(lat1*lon1):: zsm
  real(r_kind),dimension(iglobal):: work1
  real(r_kind),dimension(lat2,lon2):: sum
  real(r_kind),dimension(nlat,nlon):: sumall

  integer(i_kind) i,j,k,mm1,kk
  
  mm1=mype+1
  fplev=zero
  do j=1,lon1*lat1
    zsm(j)=zero
  end do

  call strip(dx,zsm,1)

  call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
     work1,ijn,displs_g,mpi_rtype,&
     mpi_comm_world,ierror)

  do k=1,iglobal
    i=ltosi(k) ; j=ltosj(k)
    sumall(i,j)=work1(k)
  end do
! if(mype == 0)then
! do j=1,nlon
!   do i=1,nlat
!     write(500,*)i,j,sumall(i,j)
!   end do
! end do
! end if
  do j=1,nlon
    do i=1,nlat
      fplev=fplev+sumall(i,j)*sumall(i,j)
    end do
  end do
    
  return
end function fplev


subroutine dplev_tl(dx,dy,dplevtmp,dx_tl,dy_tl,dplevtmp_tl)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   dplev_tl   the tangent linear of the operator that calculates 
!                             dot product for data on subdomain
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-03-31
!
! abstract: the tangent linear of the operator that calculates dot product 
!           for data on subdomain.  Note loops over streamfunction, velocity 
!           potential, temperature, etc. Also, only over interior of subdomain.
!
! program history log:
!   2005-03-31  yanqiu zhu - tangent linear of dplev
!
!   input argument list:
!     dx       - input vector 1
!     dy       - input vector 2
!     dx_tl     - input tangent linear vector 1
!     dy_tl     - input tangent linear vector 2
!
!   output argument list
!     dplevtmp    - dot product
!     dplevtmp_tl  - tangent linear of dplevtmp
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  
  use kinds, only: r_kind,r_double,i_kind
  use jfunc, only:  nval_levs
  use gridmod, only:  lat2,lon2
  use constants, only:  zero
  implicit none

! Declar passed variables
  real(r_kind),dimension(lat2,lon2,nval_levs),intent(in)::dx,dy
  real(r_kind),dimension(lat2,lon2,nval_levs),intent(in)::dx_tl,dy_tl
#ifdef ibm_sp
  real(r_double) :: dplevtmp
  real(r_double) :: dplevtmp_tl
#else
  real(8) :: dplevtmp
  real(8) :: dplevtmp_tl
#endif

! Declare local variables
  integer(i_kind) i,j,k
  
  dplevtmp=zero
  dplevtmp_tl=zero
  do k=1,nval_levs
     do j=2,lon2-1
        do i=2,lat2-1
           dplevtmp_tl=dplevtmp_tl+dx_tl(i,j,k)*dy(i,j,k)+dx(i,j,k)*dy_tl(i,j,k)
           dplevtmp=dplevtmp+dx(i,j,k)*dy(i,j,k)
        end do
     end do
  end do
  
  return
end subroutine dplev_tl

end module dprodxmod
