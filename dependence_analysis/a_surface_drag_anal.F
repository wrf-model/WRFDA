!Zhang Xiaoyan 11/02/2006

!*************************************************************
! TBH:  OVERWRITE a_muu, a_muv, a_z, a_z_at_w, a_u, a_v
! TBH:  HORIZONTAL DEPENDENCE:  z         ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  z_at_w    ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_z       ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_z_at_w  ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  xland     ( [i-1,i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  u         ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  v         ( [i-1,i,i+1], [j-1,j,j+1] )
subroutine a_surface_drag(ru_tendf, a_ru_tendf, rv_tendf, a_rv_tendf, u, a_u, v, a_v, xland, muu, a_muu, muv, a_muv, z, &
&a_z, z_at_w, a_z_at_w, ide, jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte )

!USE module_big_step_utilities_em
implicit none

!==============================================
! declare arguments
!==============================================
integer, intent(in) :: ime
integer, intent(in) :: ims
integer, intent(in) :: jme
integer, intent(in) :: jms
real, intent(inout) :: a_muu(ims:ime,jms:jme)
real, intent(inout) :: a_muv(ims:ime,jms:jme)
integer, intent(in) :: kme
integer, intent(in) :: kms
real, intent(inout) :: a_ru_tendf(ims:ime,kms:kme,jms:jme)
real, intent(inout) :: a_rv_tendf(ims:ime,kms:kme,jms:jme)
real, intent(inout) :: a_u(ims:ime,kms:kme,jms:jme)
real, intent(inout) :: a_v(ims:ime,kms:kme,jms:jme)
real, intent(inout) :: a_z(ims:ime,kms:kme,jms:jme)
real, intent(inout) :: a_z_at_w(ims:ime,kms:kme,jms:jme)
!real epsilon
integer, intent(in) :: ide
integer, intent(in) :: ite 
integer, intent(in) :: its
integer, intent(in) :: jde
integer, intent(in) :: jte
integer, intent(in) :: jts
integer, intent(in) :: kte
integer, intent(in) :: kts
real, intent(in) :: muu(ims:ime,jms:jme)
real, intent(in) :: muv(ims:ime,jms:jme)
real, intent(inout) :: ru_tendf(ims:ime,kms:kme,jms:jme)
real, intent(inout) :: rv_tendf(ims:ime,kms:kme,jms:jme)
real, intent(in) :: u(ims:ime,kms:kme,jms:jme)
real, intent(in) :: v(ims:ime,kms:kme,jms:jme)
real, intent(in) :: xland(ims:ime,jms:jme)
real, intent(in) :: z(ims:ime,kms:kme,jms:jme)
real, intent(in) :: z_at_w(ims:ime,kms:kme,jms:jme)

!==============================================
! declare local variables
!==============================================
real a_cd
real a_tao_xz
real a_tao_yz
real a_v0_u
real a_v0_ui
real a_v0_v
real a_v0_vi
real a_zu
real a_zv
real a_zwt
real cd 
integer i
integer i_end
integer i_endu
integer i_start
integer j
integer j_end
integer j_endv
integer j_start
integer k
real tao_xz
real tao_yz
real v0_u
real v0_v
real zu
real zv
real zwt

REAL, PARAMETER :: epsilon = 1.e-10
    write(6,*)'ADJ',epsilon !zzma
!----------------------------------------------
! RESET LOCAL ADJOINT VARIABLES
!----------------------------------------------
a_cd = 0.
a_tao_xz = 0.
a_tao_yz = 0.
a_v0_u = 0.
a_v0_v = 0.
a_zu = 0.
a_zv = 0.
a_zwt = 0.

!----------------------------------------------
! ROUTINE BODY
! ROUTINE BODY
!----------------------------------------------
!----------------------------------------------
! FUNCTION AND TAPE COMPUTATIONS
!----------------------------------------------
i_start = its
i_end = min(ite,ide-1)
i_endu = ite
j_start = jts
j_end = min(jte,jde-1)
j_endv = jte
do j = j_start, j_end
  do i = i_start, i_endu
    v0_u = sqrt(u(i,kts,j)**2+((v(i,kts,j)+v(i,kts,j+1)+v(i-1,kts,j)+v(i-1,kts,j+1))/4)**2)+epsilon
    if (xland(i,j) .eq. xland(i-1,j)) then
      if (xland(i,j) .lt. 1.5) then
        cd = 0.01
      else 
        cd = 0.001
        cd = max(cd,0.0001*v0_u)
        cd = min(cd,0.003)
      endif
    else 
      cd = 0.003
    endif
    tao_xz = cd*v0_u*u(i,kts,j)
    do k = kts, kte
      zu = 0.5*(z(i,k,j)+z(i-1,k,j)-z_at_w(i,kts,j)-z_at_w(i-1,kts,j))
      if (zu .lt. 1000.) then
        zwt = 2.*(1000.-zu)/1000.
!X        ru_tendf(i,k,j) = ru_tendf(i,k,j)-zwt*0.5*muu(i,j)*tao_xz/1000.
      endif
    end do
  end do
end do
do j = j_start, j_endv
  do i = i_start, i_end
    v0_v = sqrt(v(i,kts,j)**2+((u(i,kts,j)+u(i,kts,j-1)+u(i+1,kts,j)+u(i+1,kts,j-1))/4)**2)+epsilon
    if (xland(i,j) .eq. xland(i,j-1)) then
      if (xland(i,j) .lt. 1.5) then
        cd = 0.01
      else
        cd = 0.001
        cd = max(cd,0.0001*v0_v)
        cd = min(cd,0.003)
      endif
    else
      cd = 0.003
    endif
    tao_yz = cd*v0_v*v(i,kts,j)
    do k = kts, kte
      zv = 0.5*(z(i,k,j)+z(i,k,j-1)-z_at_w(i,kts,j)-z_at_w(i,kts,j-1))
      if (zv .lt. 1000.) then
        zwt = 2.*(1000.-zv)/1000.
!X        rv_tendf(i,k,j) = rv_tendf(i,k,j)-zwt*0.5*muv(i,j)*tao_yz/1000.
      endif
    end do
  end do
end do

!----------------------------------------------
! ADJOINT COMPUTATIONS
!----------------------------------------------
! recompute : i_start
! recompute : i_end
! recompute : i_endu
! recompute : j_start
! recompute : j_end
! recompute : j_endv
! TBH:  OVERWRITE a_muv, a_z, a_z_at_w, a_u, a_v
! TBH:  HORIZONTAL DEPENDENCE:  z         ( [    i    ], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  z_at_w    ( [    i    ], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  a_z       ( [    i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_z_at_w  ( [    i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  xland     ( [    i    ], [j-1,j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  u         ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  v         ( [i-1,i,i+1], [j-1,j,j+1] )
!TBH:  BUG:  need redundant computations for non-owner computes
do j = j_endv, j_start, -1
  do i = i_end, i_start, -1
    v0_v = sqrt(v(i,kts,j)**2+((u(i,kts,j)+u(i,kts,j-1)+u(i+1,kts,j)+u(i+1,kts,j-1))/4)**2)+epsilon
! recompute : v0_v
! recompute : v0_v
    if (xland(i,j) .eq. xland(i,j-1)) then
      if (xland(i,j) .lt. 1.5) then
        cd = 0.01
      else
        cd = 0.001
        cd = max(cd,0.0001*v0_v)
        cd = min(cd,0.003)
      endif
    else
      cd = 0.003
    endif
! recompute : cd
! recompute : cd
    tao_yz = cd*v0_v*v(i,kts,j)
! recompute : tao_yz
! recompute : tao_yz
    do k = kts, kte
      a_zv = 0.
      a_zwt = 0.
      zv = 0.5*(z(i,k,j)+z(i,k,j-1)-z_at_w(i,kts,j)-z_at_w(i,kts,j-1))
! recompute : zv
! recompute : zv
      if (zv .lt. 1000.) then
        zwt = 2.*(1000.-zv)/1000.
! recompute : zwt
! recompute : zwt
        a_muv(i,j) = a_muv(i,j)-a_rv_tendf(i,k,j)*(0.5*zwt*tao_yz/1000.)
        a_tao_yz = a_tao_yz-a_rv_tendf(i,k,j)*(0.5*zwt*muv(i,j)/1000.)
        a_zwt = a_zwt-a_rv_tendf(i,k,j)*(0.5*muv(i,j)*tao_yz/1000.)
        a_zv = a_zv-0.002*a_zwt
        a_zwt = 0.
      endif
      a_z(i,k,j-1) = a_z(i,k,j-1)+0.5*a_zv
      a_z(i,k,j) = a_z(i,k,j)+0.5*a_zv
      a_z_at_w(i,kts,j-1) = a_z_at_w(i,kts,j-1)-0.5*a_zv
      a_z_at_w(i,kts,j) = a_z_at_w(i,kts,j)-0.5*a_zv
      a_zv = 0.
    end do
    a_cd = a_cd+a_tao_yz*v0_v*v(i,kts,j)
    a_v(i,kts,j) = a_v(i,kts,j)+a_tao_yz*cd*v0_v
    a_v0_v = a_v0_v+a_tao_yz*cd*v(i,kts,j)
    a_tao_yz = 0.
    if (xland(i,j) .eq. xland(i,j-1)) then
      if (xland(i,j) .lt. 1.5) then
        a_cd = 0.
      else
        cd = 0.001
! recompute : cd
! recompute : cd
        cd = max(cd,0.0001*v0_v)
! recompute : cd
! recompute : cd
        a_cd = a_cd*(0.5+sign(0.5,0.003-cd))
! recompute pos : ASSIGN_STMT surface_drag.f90:90
! recompute vars : cd
        cd = 0.001
! recompute : cd
! recompute vars : cd
        a_v0_v = a_v0_v+0.0001*a_cd*(0.5-sign(0.5,cd-0.0001*v0_v))
        a_cd = a_cd*(0.5+sign(0.5,cd-0.0001*v0_v))
        a_cd = 0.
      endif
    else
      a_cd = 0.
    endif
    a_v0_vi = a_v0_v*(1./(2.*sqrt(v(i,kts,j)**2+((u(i,kts,j)+u(i,kts,j-1)+u(i+1,kts,j)+u(i+1,kts,j-1))/4)**2)))
    a_u(i+1,kts,j-1) = a_u(i+1,kts,j-1)+2*a_v0_vi/float(4)*((u(i,kts,j)+u(i,kts,j-1)+u(i+1,kts,j)+u(i+1,kts,j-1))/float(4))
    a_u(i,kts,j-1) = a_u(i,kts,j-1)+2*a_v0_vi/float(4)*((u(i,kts,j)+u(i,kts,j-1)+u(i+1,kts,j)+u(i+1,kts,j-1))/float(4))
    a_u(i+1,kts,j) = a_u(i+1,kts,j)+2*a_v0_vi/float(4)*((u(i,kts,j)+u(i,kts,j-1)+u(i+1,kts,j)+u(i+1,kts,j-1))/float(4))
    a_u(i,kts,j) = a_u(i,kts,j)+2*a_v0_vi/float(4)*((u(i,kts,j)+u(i,kts,j-1)+u(i+1,kts,j)+u(i+1,kts,j-1))/float(4))
    a_v(i,kts,j) = a_v(i,kts,j)+2*a_v0_vi*v(i,kts,j)
    a_v0_v = 0.
  end do
end do
! TBH:  OVERWRITE a_muu, a_z, a_z_at_w, a_u, a_v
! TBH:  HORIZONTAL DEPENDENCE:  z         ( [i-1,i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  z_at_w    ( [i-1,i,i+1], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_z       ( [i-1,i    ], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  a_z_at_w  ( [i-1,i    ], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  xland     ( [i-1,i    ], [    j    ] )
! TBH:  HORIZONTAL DEPENDENCE:  u         ( [i-1,i,i+1], [j-1,j,j+1] )
! TBH:  HORIZONTAL DEPENDENCE:  v         ( [i-1,i,i+1], [j-1,j,j+1] )
!TBH:  BUG:  need redundant computations for non-owner computes
do j = j_end, j_start, -1
  do i = i_endu, i_start, -1
    v0_u = sqrt(u(i,kts,j)**2+((v(i,kts,j)+v(i,kts,j+1)+v(i-1,kts,j)+v(i-1,kts,j+1))/4)**2)+epsilon
! recompute : v0_u
! recompute : v0_u
    if (xland(i,j) .eq. xland(i-1,j)) then
      if (xland(i,j) .lt. 1.5) then
        cd = 0.01
      else
        cd = 0.001
        cd = max(cd,0.0001*v0_u)
        cd = min(cd,0.003)
      endif
    else
      cd = 0.003
    endif
! recompute : cd
! recompute : cd
    tao_xz = cd*v0_u*u(i,kts,j)
! recompute : tao_xz
! recompute : tao_xz
    do k = kts, kte
      a_zu = 0.
      a_zwt = 0.
      zu = 0.5*(z(i,k,j)+z(i-1,k,j)-z_at_w(i,kts,j)-z_at_w(i-1,kts,j))
! recompute : zu
! recompute : zu
      if (zu .lt. 1000.) then
        zwt = 2.*(1000.-zu)/1000.
! recompute : zwt
! recompute : zwt
        a_muu(i,j) = a_muu(i,j)-a_ru_tendf(i,k,j)*(0.5*zwt*tao_xz/1000.)
        a_tao_xz = a_tao_xz-a_ru_tendf(i,k,j)*(0.5*zwt*muu(i,j)/1000.)
        a_zwt = a_zwt-a_ru_tendf(i,k,j)*(0.5*muu(i,j)*tao_xz/1000.)
        a_zu = a_zu-0.002*a_zwt
        a_zwt = 0.
      endif
      a_z(i-1,k,j) = a_z(i-1,k,j)+0.5*a_zu
      a_z(i,k,j) = a_z(i,k,j)+0.5*a_zu
      a_z_at_w(i-1,kts,j) = a_z_at_w(i-1,kts,j)-0.5*a_zu
      a_z_at_w(i,kts,j) = a_z_at_w(i,kts,j)-0.5*a_zu
      a_zu = 0.
    end do
    a_cd = a_cd+a_tao_xz*v0_u*u(i,kts,j)
    a_u(i,kts,j) = a_u(i,kts,j)+a_tao_xz*cd*v0_u
    a_v0_u = a_v0_u+a_tao_xz*cd*u(i,kts,j)
    a_tao_xz = 0.
    if (xland(i,j) .eq. xland(i-1,j)) then
      if (xland(i,j) .lt. 1.5) then
        a_cd = 0.
      else
        cd = 0.001
! recompute : cd
! recompute : cd
        cd = max(cd,0.0001*v0_u)
! recompute : cd
! recompute : cd
        a_cd = a_cd*(0.5+sign(0.5,0.003-cd))
! recompute pos : ASSIGN_STMT surface_drag.f90:54
! recompute vars : cd
        cd = 0.001
! recompute : cd
! recompute vars : cd
        a_v0_u = a_v0_u+0.0001*a_cd*(0.5-sign(0.5,cd-0.0001*v0_u))
        a_cd = a_cd*(0.5+sign(0.5,cd-0.0001*v0_u))
        a_cd = 0.
      endif
    else
      a_cd = 0.
    endif
    a_v0_ui = a_v0_u*(1./(2.*sqrt(u(i,kts,j)**2+((v(i,kts,j)+v(i,kts,j+1)+v(i-1,kts,j)+v(i-1,kts,j+1))/4)**2)))
    a_u(i,kts,j) = a_u(i,kts,j)+2*a_v0_ui*u(i,kts,j)
    a_v(i-1,kts,j+1) = a_v(i-1,kts,j+1)+2*a_v0_ui/float(4)*((v(i,kts,j)+v(i,kts,j+1)+v(i-1,kts,j)+v(i-1,kts,j+1))/float(4))
    a_v(i,kts,j+1) = a_v(i,kts,j+1)+2*a_v0_ui/float(4)*((v(i,kts,j)+v(i,kts,j+1)+v(i-1,kts,j)+v(i-1,kts,j+1))/float(4))
    a_v(i-1,kts,j) = a_v(i-1,kts,j)+2*a_v0_ui/float(4)*((v(i,kts,j)+v(i,kts,j+1)+v(i-1,kts,j)+v(i-1,kts,j+1))/float(4))
    a_v(i,kts,j) = a_v(i,kts,j)+2*a_v0_ui/float(4)*((v(i,kts,j)+v(i,kts,j+1)+v(i-1,kts,j)+v(i-1,kts,j+1))/float(4))
    a_v0_u = 0.
  end do
end do


end subroutine a_surface_drag
