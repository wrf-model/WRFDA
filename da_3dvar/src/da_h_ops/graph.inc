!						***************************
!						*  graph                  *
!						*  wu     2001            *
! 						***************************
SUBROUTINE graph(u,v,t,q,p,mix,mjx,mkx,iunit)      
!----------------------------------------------------------------------------
! Set m*m matrix p to the nth power of m*m matrix a using binary combination
! of factors.
!----------------------------------------------------------------------------
INTEGER,                      INTENT(IN) :: mix,mjx,mkx,iunit
REAL, DIMENSION(mix,mjx,mkx), INTENT(IN) :: u,v,t,q
REAL, DIMENSION(mix,mjx), INTENT(IN) :: p            
real :: sumu,sumv,sumt,sumq,sump,total
!----------------------------------------------------------------------------
REAL*4, DIMENSION(mjx,mix)                :: plot
!----------------------------------------------------------------------------

integer  :: i, j, k ! Loop counters
if(iunit==123)then
open(123,file='anl',form='unformatted')
else if(iunit==124)then
open(124,file='inc',form='unformatted')
endif
sumu=0.
sumv=0.
sumt=0.
sumq=0.
sump=0.
do k=1,mkx
 do j=1,mjx
  do i=1,mix
plot(j,i)=u(i,j,k)
sumu=sumu+u(i,j,k)**2
  enddo
enddo
write(iunit)plot
enddo
do k=1,mkx
 do j=1,mjx
  do i=1,mix
plot(j,i)=v(i,j,k)
sumv=sumv+v(i,j,k)**2
  enddo
enddo
write(iunit)plot
enddo
do k=1,mkx
 do j=1,mjx
  do i=1,mix
plot(j,i)=t(i,j,k)
sumt=sumt+t(i,j,k)**2
  enddo
enddo
write(iunit)plot
enddo
do k=1,mkx
 do j=1,mjx
  do i=1,mix
plot(j,i)=q(i,j,k)
sumq=sumq+q(i,j,k)**2
  enddo
enddo
write(iunit)plot
enddo
 do j=1,mjx
  do i=1,mix
plot(j,i)=p(i,j)
sump=sump+p(i,j)**2
  enddo
enddo
write(iunit)plot
 total=sumu+sumv+sumt+sumq+sump
write(6,*)'graph energy=',iunit,sumu,sumv,sumt,sumq,sump,total
END SUBROUTINE graph      
