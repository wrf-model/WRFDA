subroutine turbvars(pges,tges,uges,vges,oges,zges)
!$$$
! Calculate coeficients 'pim' and 'pih' used to describe vertical
! turbulent mixing in the time tendency model 
!$$$
  use kinds,only: r_kind,i_kind 
  use gridmod,only: lat2,lon2,nsig
  use constants,only: one,zero,two,half,rd_over_g,rd_over_cp,grav
  use turbmod,only: pim1,pim2,pim3,pih1,pih2,pih3,use_pbl
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: tges,uges,vges,oges
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: pges
  real(r_kind),dimension(lat2,lon2),intent(in):: zges

  real(r_kind),dimension(nsig+1):: zi
  real(r_kind),dimension(nsig):: rdzi
  real(r_kind),dimension(nsig):: zl,rdzl,dzl 


! Declare local parameters
  real(r_kind),parameter:: eps_m   = 0.002_r_kind
! Constants used in MY20 PBL parameterization  

 real(r_kind) ricmy20,a0my20,b0my20,c0my20,d0my20,f1my20,f2my20, &
              f3my20,f4my20,f5my20,f6my20,f7my20,f8my20,karmy20, &
              l0my20,b1my20,kar0my20,alf0my20

!>>> These are original coefficients from MY(1974)

! parameter( ricmy20= 0.2338021249_r_kind )
! parameter( a0my20= 0.7162162662_r_kind )
! parameter( b0my20= 0.1886792481_r_kind )
! parameter( c0my20= 0.3197414279_r_kind )
! parameter( d0my20= 0.03559985757_r_kind )
! parameter( f1my20= 2.339999914_r_kind )
! parameter( f2my20= 0.2293333411_r_kind )
! parameter( f3my20= 1.074666739_r_kind )
! parameter( f4my20= 1.000000000_r_kind )
! parameter( f5my20= 2.600000143_r_kind )
! parameter( f6my20= 9.619999886_r_kind )
! parameter( f7my20= 3.440000057_r_kind )
! parameter( f8my20= 13.78000069_r_kind )
! parameter( b1my20=15.0_r_kind )

!>>> These coefficients recalculated from Eta model based
!>>> Janjic (1990) implementation

  parameter( ricmy20=0.5046048164_r_kind ) 
  parameter( a0my20=0.6892600656_r_kind )
  parameter( b0my20=0.2228188217_r_kind )
  parameter( c0my20=0.2009073496_r_kind )
  parameter( d0my20=0.04964822531_r_kind )
  parameter( f1my20=1.972262979_r_kind )
  parameter( f2my20=0.2222222388_r_kind )
  parameter( f3my20=1.163989186_r_kind )
  parameter( f4my20=1.003753304_r_kind )
  parameter( f5my20=2.629684210_r_kind )
  parameter( f6my20=8.561278343_r_kind )
  parameter( f7my20=2.639554262_r_kind )
  parameter( f8my20=11.84619045_r_kind )
  parameter( b1my20=11.87799326209552761_r_kind )

  parameter( karmy20=0.4_r_kind )
  parameter( l0my20=80._r_kind )
  parameter( alf0my20=0.1_r_kind)
  integer(i_kind) i,j,k
 
  real(r_kind),dimension(nsig):: tloc,uloc,vloc,oloc
  real(r_kind),dimension(nsig+1):: ploc
  real(r_kind),dimension(nsig):: km,kh
  real(r_kind),dimension(nsig):: omlh,omlm,omrh,omrm,zmix
  real(r_kind) dodz,dudz,dvdz,ssq,ri,rf,sh,sm,aux,lmix
  real(r_kind) oml,omr
  real(r_kind) upl0,nml0,kterm,l0
  
  if(.not. use_pbl)return

  do j=1,lon2
    do i=1,lat2
     rdzi(1)=zero 
     km  (1)=zero
     kh  (1)=zero
     
      do k=1,nsig
        tloc(k)=tges(i,j,k)
        uloc(k)=uges(i,j,k)
        vloc(k)=vges(i,j,k)
        ploc(k)=pges(i,j,k)
	oloc(k)=oges(i,j,k)
      end do
        ploc(nsig+1)=pges(i,j,nsig+1)
        zi(1) = zges(i,j)
      do k=1,nsig
        zi(k+1)=zi(k)-rd_over_g*two*tloc(k) &
                 *(ploc(k+1)-ploc(k))/(ploc(k+1)+ploc(k))
        zl(k)=half*(zi(k+1)+zi(k))
        dzl(k)=(zi(k+1)-zi(k))
        rdzl(k)=1./dzl(k)
      end do

      upl0=zero
      nml0=zero
      do k=1,nsig
        kterm=dzl(k)*(uloc(k)**2+vloc(k)**2)
        upl0=upl0+zl(k)*kterm
        nml0=nml0+      kterm
      end do
      l0=alf0my20*upl0/nml0
      if(l0>l0my20) l0=l0my20
      kar0my20=karmy20/l0

      do k=2,nsig
	rdzi(k)=one/(zl(k)-zl(k-1))
      end do
      do k=2,nsig
        dodz=(oloc(k)-oloc(k-1))*rdzi(k)
        dudz=(uloc(k)-uloc(k-1))*rdzi(k)
        dvdz=(vloc(k)-vloc(k-1))*rdzi(k)
	ssq=dudz**2+dvdz**2
        if(ssq < eps_m) ssq=eps_m
        ri=grav*dodz/(tloc(k)*ssq)
	if(ri > ricmy20) ri=ricmy20
        rf=a0my20*(ri+b0my20-sqrt(ri**2-c0my20*ri+d0my20))
        sh=f1my20*(f2my20-f3my20*rf)*(one-rf)
        sm=f4my20*(f5my20-f6my20*rf)/(f7my20-f8my20*rf)*sh
        zmix(k)=zi(k)-zi(1)
        lmix=karmy20*zmix(k)/(one+kar0my20*zmix(k))
        aux=sqrt(ssq*b1my20*(one-rf)*sm)*lmix**2
        km(k)=aux*sm 
        kh(k)=aux*sh 
      end do
      do k=1,nsig-1
        oml=rdzl(k)*rdzi(k+1)
	omlh(k)=oml*kh(k+1)
	omlm(k)=oml*km(k+1)
      end do
      omlh(nsig)=zero
      omlm(nsig)=zero
      do k=2,nsig
        omr=rdzl(k)*rdzi(k)
        omrh(k)=omr*kh(k)
        omrm(k)=omr*km(k)
      end do
      omrh(1)=zero
      omrm(1)=zero
      do k=1,nsig
        pim1(i,j,k)=omrm(k)
        pim2(i,j,k)=-(omlm(k)+omrm(k))
        pim3(i,j,k)=omlm(k)
        pih1(i,j,k)=omrh(k)
        pih2(i,j,k)=-(omlh(k)+omrh(k))
        pih3(i,j,k)=omlh(k)
      end do
    end do
  end do

  return
end subroutine turbvars
