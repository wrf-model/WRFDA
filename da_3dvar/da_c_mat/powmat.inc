!						***************************
!						*  powmat.f90             *
!						*  PURSER 2001            *
! 						***************************
SUBROUTINE powmat(a,p,m,n)
!----------------------------------------------------------------------------
! Set m*m matrix p to the nth power of m*m matrix a using binary combination
! of factors.
!----------------------------------------------------------------------------
IMPLICIT NONE
INTEGER,              INTENT(IN) :: m,n
REAL, DIMENSION(m,m), INTENT(IN) :: a
REAL, DIMENSION(m,m), INTENT(OUT):: p 
!----------------------------------------------------------------------------
INTEGER i,k,nt,nu
REAL, DIMENSION(m,m):: t
!----------------------------------------------------------------------------
IF (n < 0) THEN
  CALL wrf_error_fatal3(__FILE__,__LINE__, &
    "n is illegally negative")
END IF
p=0.; DO i=1,m; p(i,i)=1.; ENDDO
t=a
nt=n
DO k=1,50
   IF(nt == 0)RETURN
   nu=nt/2; IF(nt-nu*2 == 1)p=MATMUL(t,p); nt=nu
   t=MATMUL(t,t)
ENDDO
CALL wrf_error_fatal3(__FILE__,__LINE__, &
  "No solution obtained in powmat")
END SUBROUTINE powmat

SUBROUTINE dpowmat(a,p,m,n)
!----------------------------------------------------------------------------
! Set m*m matrix p to the nth power of m*m matrix a using binary combination
! of factors.
!----------------------------------------------------------------------------
IMPLICIT NONE
INTEGER,                 INTENT(IN) :: m,n
REAL(8), DIMENSION(m,m), INTENT(IN) :: a
REAL(8), DIMENSION(m,m), INTENT(OUT):: p 
!----------------------------------------------------------------------------
INTEGER i,k,nt,nu
REAL(8), DIMENSION(m,m):: t
!----------------------------------------------------------------------------
if(n < 0) THEN
  CALL wrf_error_fatal3(__FILE__,__LINE__, &
    "n is illegally negative")
END IF
p=0.; DO i=1,m; p(i,i)=1.; ENDDO
t=a
nt=n
DO k=1,50
   IF(nt == 0)RETURN
   nu=nt/2; IF(nt-nu*2 == 1)p=MATMUL(t,p); nt=nu
   t=MATMUL(t,t)
ENDDO
CALL wrf_error_fatal3(__FILE__,__LINE__, &
  "No solution obtained in dpowmat")
END SUBROUTINE dpowmat
