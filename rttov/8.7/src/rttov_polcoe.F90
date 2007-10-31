SUBROUTINE rttov_polcoe(x,y,n,cof)
  !
  ! Description:
  ! Numerical Recipes Routine for cubic interpolation
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1       07/10/2004  Added history
  !  1.1     29/03/2005  Add end of header comment (J. Cameron)
  !

  Use parkind1, Only : jpim     ,jprb
  Implicit None
  Integer(Kind=jpim) :: n,NMAX
  Real(Kind=jprb) :: cof(n),x(n),y(n)
  PARAMETER (NMAX=15)
  Integer(Kind=jpim) :: i,j,k
  Real(Kind=jprb) :: b,ff,phi,s(NMAX)

  !- End of header --------------------------------------------------------

  do  i=1,n
     s(i)=0._JPRB
     cof(i)=0._JPRB
  enddo
  s(n)=-x(1)
  do i=2,n
     do j=n+1-i,n-1
        s(j)=s(j)-x(i)*s(j+1)
     enddo
     s(n)=s(n)-x(i)
  enddo
  do j=1,n
     phi=n
     do k=n-1,1,-1
        phi=k*s(k+1)+x(j)*phi
     enddo
     ff=y(j)/phi
     b=1._JPRB
     do k=n,1,-1
        cof(k)=cof(k)+b*ff
        b=s(k)+x(j)*b
     enddo
  enddo

END SUBROUTINE rttov_polcoe
!  (C) Copr. 1986-92 Numerical Recipes Software
