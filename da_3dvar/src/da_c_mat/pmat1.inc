!						********************
!						* module_pmat1.f90 *
!						* PURSER 1994/1999 *
!                                               *   FUJITA 1999    *
!						********************
!
!
!   Routines for basic algebraic operations on general matrices and vectors
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!  These routines, perform basic algebraic operations on real vectors and
!  matrices. The task performed by each routine is, as far as possible,
!  encoded in each routine~s name; three letters describe the
!  operation, the remainder defining the type of operand and, if needed to
!  resolve an ambiguity, the type of result.
!
!  OPERATIONS:
!   DET     evaluate log-determinant
!   DIF     differentiate
!   INT     integrate
!   INV     invert the matrix, or linear system involving the matrix operand
!   L1L     Cholesky LU decomposition, where U is just L-transpose
!   L1U     L-U decomposition of first arg, with 1~s along diagonal of L and U
!   LDL     Cholesky LDU decomposition, where U is just L-transpose and D diag.
!   LDU     LDU decomposition
!   NOR     evaluate norm of operand
!   POL     polynomial (first argument) of second argument
!   POW     raise operand to some integer power
!   SWP     swap first two operands
!   TRC     evaluate trace of operand
!   U1L     back substitution with matrix decomposed into LU form, 1~s on diag.
!   UDL     back substitution with matrix decomposed into LDU form
!   WRT     write out
!   ZER     set operand to zero
!
!  OPERAND TYPES:
!   B	    banded matrix
!   C	    circulant matrix
!   D	    diagonal matrix
!   H	    symmetric or hermitian matrix
!   L	    lower triangular matrix
!   M	    matrix (rectangular, in general)
!   P	    polynomial or power-series coefficient vector
!   Q	    sQuare matrix with Fortran dimension same as logical dimension
!   R	    row of a matrix
!   S	    scalar
!   T	    transpose of the matrix
!   U	    upper triangular matrix
!   V	    vector, or column of a matrix
!   X	    field of parallel X-vectors (aligned like "columns" of a matrix)
!   Y	    field of parallel Y-vectors (aligned like "rows" of a matrix)
!
!------------------------------------------------------------------------------

FUNCTION pro333(d,e,f) RESULT(pro_res) ! TRIPLE PRODUCT OF 3 3-VECTORS
REAL                   :: pro_res
REAL,    INTENT(IN   ) :: d(3), e(3), f(3)
REAL                   :: g(3)
CALL CRO33(E,F,G)
pro_res=DOT_PRODUCT(d,g)
END FUNCTION pro333
FUNCTION dpro333(d,e,f) RESULT(pro_res) ! TRIPLE PRODUCT OF 3 3-VECTORS
REAL(8)                :: pro_res
REAL(8), INTENT(IN   ) :: d(3), e(3), f(3)
REAL(8)                :: g(3)
CALL CRO33_d(E,F,G)
pro_res=DOT_PRODUCT(d,g)
END FUNCTION dpro333

SUBROUTINE cro33(a,b,c) ! SPECIAL CASE OF 3-DIMENSIONS: CROSS-PRODUCT
REAL,    INTENT(IN   ) :: a(3), b(3)
REAL,    INTENT(OUT  ) :: c(3)
c(1)=a(2)*b(3)-a(3)*b(2)
c(2)=a(3)*b(1)-a(1)*b(3)
c(3)=a(1)*b(2)-a(2)*b(1)
END SUBROUTINE cro33
SUBROUTINE dcro33(a,b,c) ! SPECIAL CASE OF 3-DIMENSIONS: CROSS-PRODUCT
REAL(8), INTENT(IN   ) :: a(3), b(3)
REAL(8), INTENT(OUT  ) :: c(3)
c(1)=a(2)*b(3)-a(3)*b(2)
c(2)=a(3)*b(1)-a(1)*b(3)
c(3)=a(1)*b(2)-a(2)*b(1)
END SUBROUTINE dcro33

FUNCTION norv(d) RESULT(norv_res)! NORM OF VECTOR..
REAL                   :: norv_res
REAL,    INTENT(IN   ) :: d(:)
norv_res=SQRT(DOT_PRODUCT(D,D))
END FUNCTION norv
FUNCTION dnorv(d) ! NORM OF VECTOR..
REAL(8)                :: dnorv
REAL(8), INTENT(IN   ) :: d(:)
dnorv=SQRT(DOT_PRODUCT(d,d))
END FUNCTION dnorv

FUNCTION norq(d) ! Norm of a matrix
REAL                   :: norq
REAL,    INTENT(IN   ) :: d(:,:)
INTEGER                :: i2
norq=0.; DO i2=1,SIZE(d,2); norq=norq+dot_PRODUCT(d(:,i2),d(:,i2)); ENDDO
norq=SQRT(norq)
END FUNCTION norq
FUNCTION dnorq(d) ! norm of a matrix
REAL(8)                :: dnorq
REAL(8), INTENT(IN   ) :: d(:,:)
INTEGER                :: i2
dnorq=0.; DO i2=1,SIZE(d,2); dnorq=dnorq+dot_PRODUCT(d(:,i2),d(:,i2)); ENDDO
dnorq=SQRT(dnorq)
END FUNCTION dnorq


SUBROUTINE swpvv(d,e)
REAL,    INTENT(INOUT) :: d(:), e(:)
REAL                   :: t(SIZE(d))
t = d; d = e; e = t
END SUBROUTINE swpvv
SUBROUTINE dswpvv(d,e)
REAL(8), INTENT(INOUT) :: d(:), e(:)
REAL(8)                :: t(SIZE(d))
t = d; d = e; e = t
END SUBROUTINE dswpvv

SUBROUTINE mulmd(a,d,b)
REAL,    INTENT(INOUT) :: a(:,:),b(:,:) 
REAL,    INTENT(IN   ) :: d(:)
INTEGER                :: j
DO j=1,SIZE(a,2); b(:,j)=a(:,j)*d(j); ENDDO
END SUBROUTINE mulmd
SUBROUTINE dmulmd(a,d,b)
REAL(8), INTENT(INOUT) :: a(:,:),b(:,:)
REAL(8), INTENT(IN   ) :: d(:)
INTEGER                :: j
DO j=1,SIZE(a,2); b(:,j)=a(:,j)*d(j); ENDDO
END SUBROUTINE dmulmd

SUBROUTINE multd(a,d,b)
REAL,    INTENT(INOUT) :: a(:,:),b(:,:) 
REAL,    INTENT(IN   ) :: d(:)
INTEGER                :: j
DO j=1,SIZE(a,1); b(:,j)=a(j,:)*d(j); ENDDO
END SUBROUTINE multd
SUBROUTINE dmultd(a,d,b)
REAL(8), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(8), INTENT(IN   ) :: d(:)
INTEGER                :: j
DO j=1,SIZE(a,1); b(:,j)=a(j,:)*d(j); ENDDO
END SUBROUTINE dmultd

SUBROUTINE muldm(d,a,b)
REAL,    INTENT(INOUT) :: a(:,:),b(:,:) 
REAL,    INTENT(IN   ) :: d(:)
INTEGER                :: i
DO i=1,SIZE(a,1); b(i,:)=d(i)*a(i,:); ENDDO
END SUBROUTINE muldm
SUBROUTINE dmuldm(d,a,b)
REAL(8), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(8), INTENT(IN   ) :: d(:)
INTEGER                :: i
DO i=1,SIZE(a,1); b(i,:)=d(i)*a(i,:); ENDDO
END SUBROUTINE dmuldm

SUBROUTINE muldt(d,a,b)
REAL,    INTENT(INOUT) :: a(:,:),b(:,:) 
REAL,    INTENT(IN   ) :: d(:)
INTEGER                :: i
DO i=1,SIZE(a,2); b(i,:)=d(i)*a(:,i); ENDDO
END SUBROUTINE muldt
SUBROUTINE dmuldt(d,a,b)
REAL(8), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(8), INTENT(IN   ) :: d(:)
INTEGER                :: i
DO i=1,SIZE(a,2); b(i,:)=d(i)*a(:,i); ENDDO
END SUBROUTINE dmuldt

SUBROUTINE mulpp(a,b,c) !  multiply polynomials
REAL,    INTENT(IN   ) :: a(0:), b(0:)
REAL,    INTENT(OUT  ) :: c(0:)
c=0.; CALL madpp(a,b,c)
END SUBROUTINE mulpp
SUBROUTINE dmulpp(a,b,c)
REAL(8), INTENT(IN   ) :: a(0:), b(0:)
REAL(8), INTENT(OUT  ) :: c(0:)
c=0; CALL dmadpp(a,b,c)
END SUBROUTINE dmulpp

SUBROUTINE msbpp(a,b,c)
REAL,    INTENT(IN   ) :: a(0:), b(0:)
REAL,    INTENT(INOUT) :: c(0:)
c=-c; CALL madpp(a,b,c); c=-c
END SUBROUTINE msbpp
SUBROUTINE dmsbpp(a,b,c)
REAL(8), INTENT(IN   ):: a(0:), b(0:)
REAL(8), INTENT(INOUT):: c(0:)
c=-c; CALL dmadpp(a,b,c); c=-c
END SUBROUTINE dmsbpp

SUBROUTINE madpp(a,b,c)
REAL,    INTENT(IN   ) :: a(0:), b(0:)
REAL,    INTENT(INOUT) :: c(0:)
INTEGER                :: j
DO j=1,mcmax(a,b,SIZE(a)-1); c(j-1)=c(j-1)+SUM(a(j-1:0:-1)*b(0:j-1)); enddo
END SUBROUTINE madpp
SUBROUTINE dmadpp(a,b,c)
REAL(8), INTENT(IN   ):: a(0:), b(0:)
REAL(8), INTENT(INOUT):: c(0:)
INTEGER               :: j
DO j=1,dmcmax(a,b,SIZE(a)-1); c(j-1)=c(j-1)+SUM(a(j-1:0:-1)*b(0:j-1)); enddo
END SUBROUTINE dmadpp

SUBROUTINE difp(a,b) ! Symbolically differentiate polynomial
REAL,    INTENT(IN   ) :: a(0:)
REAL,    INTENT(OUT  ) :: b(0:)
INTEGER                :: i
b=0.; DO i=1,SIZE(a)-1; b(i-1)=i*a(i); ENDDO
END SUBROUTINE difp
SUBROUTINE ddifp(a,b)
REAL(8), INTENT(IN   ):: a(0:)
REAL(8), INTENT(OUT  ):: b(0:)
INTEGER               :: i
b=0.; DO i=1,SIZE(a)-1; b(i-1)=i*a(i); ENDDO
END SUBROUTINE ddifp

SUBROUTINE intp(a,b) ! Symbolically integrate polynomial
REAL,    INTENT(IN   ) :: a(0:)
REAL,    INTENT(OUT  ) :: b(0:)
INTEGER                :: i
DO i=1,SIZE(a)-1; b(i)=a(i-1)/i; ENDDO; b(0)=0.
END SUBROUTINE intp
SUBROUTINE dintp(a,b)
REAL(8), INTENT(IN   ):: a(0:)
REAL(8), INTENT(OUT  ):: b(0:)
INTEGER               :: i
DO i=1,SIZE(a)-1; b(i)=a(i-1)/i; ENDDO; b(0)=0.
END SUBROUTINE dintp

SUBROUTINE invp(a,b) ! Symbolically invert polynomial or power-series
REAL,    INTENT(IN   ) :: a(0:)
REAL,    INTENT(OUT  ) :: b(0:)
INTEGER                :: i
REAL                   :: b0
b0=1./a(0); b(0)=b0; DO i=1,SIZE(a)-1; b(i)=-b0*SUM(b(i-1:0:-1)*a(1:i)); ENDDO
END SUBROUTINE invp
SUBROUTINE dinvp(a,b)
REAL(8), INTENT(IN   ):: a(0:)
REAL(8), INTENT(OUT  ):: b(0:)
INTEGER               :: i
REAL(8)               :: b0
b0=1./a(0); b(0)=b0; DO i=1,SIZE(a)-1; b(i)=-b0*SUM(b(i-1:0:-1)*a(1:i)); ENDDO
END SUBROUTINE dinvp

SUBROUTINE powp(a,b,n) ! Raise power series A to the power N and output as B
INTEGER,               INTENT(IN) :: n     
REAL,    dimension(0:),INTENT(IN) :: a
REAL,    dimension(0:),INTENT(OUT):: b
REAL,    DIMENSION(0:SIZE(a)-1)   :: t
INTEGER                           :: k
b(0)=1.; b(1:) = 0.0; DO k=1,n; CALL mulpp(a,b,t); b=t; ENDDO
END SUBROUTINE powp
SUBROUTINE dpowp(A,B,N)	   
INTEGER,              INTENT(IN) :: n  
REAL(8),dimension(0:),INTENT(IN) :: a
REAL(8),dimension(0:),INTENT(OUT):: b
REAL(8),DIMENSION(0:SIZE(a)-1)   :: t
INTEGER                          :: k
B(0)=1.; b(1:) = 0.0; DO k=1,n; CALL mulpp_d(a,b,t); b=t; ENDDO
END SUBROUTINE dpowp

SUBROUTINE polps(a,s1,s2) ! Apply series A to scalar S1 to obtain S2
REAL,   dimension(0:),INTENT(IN) :: a
REAL,                 INTENT(IN) :: s1
REAL,                 INTENT(OUT):: s2
INTEGER                          :: m,k
m=SIZE(a)-1; s2=a(m); DO k=m-1,0,-1; s2=s2*s1+a(k); ENDDO
END SUBROUTINE polps
SUBROUTINE dpolps(a,s1,s2)
REAL(8),dimension(0:),INTENT(IN) :: a
REAL(8),              INTENT(IN) :: s1
REAL(8),              INTENT(OUT):: s2
INTEGER                          :: m,k
m=SIZE(a)-1; s2=a(m); DO k=m-1,0,-1; s2=s2*s1+a(k); ENDDO
END SUBROUTINE dpolps

SUBROUTINE polpp(a,b,c) ! Apply power series A to power series B and put
                        ! the result out as power-series C.
REAL,   dimension(0:),INTENT(INOUT):: a,b,c
REAL,   DIMENSION(0:SIZE(a)-1)     :: t
INTEGER                            :: k
t=0.
do k=size(a),1,-1; c=t; c(0)=c(0)+a(k); CALL mulpp(b,c,t); enddo 
c=t; c(0)=c(0)+a(0)
END SUBROUTINE polpp
SUBROUTINE dpolpp(a,b,c) 
REAL(8),dimension(0:),INTENT(INOUT):: a,b,c
REAL(8),DIMENSION(0:SIZE(a)-1)     :: t
INTEGER                            :: k
t=0.
do k=size(a),1,-1; c=t; c(0)=c(0)+a(k); CALL mulpp_d(b,c,t); enddo 
c=t; c(0)=c(0)+a(0)
END SUBROUTINE dpolpp

SUBROUTINE prgv(d)
REAL, PARAMETER        :: crit=1.E-30
REAL, INTENT(INOUT)    :: d(:)
INTEGER                :: i
DO i=1,SIZE(d); IF(ABS(d(i)) <= crit)d(i)=0.; ENDDO
END SUBROUTINE prgv
SUBROUTINE dprgv(d)
REAL(8), PARAMETER     :: crit=1.D-30
REAL(8), INTENT(INOUT) :: d(:)
INTEGER                :: i,m
DO i=1,SIZE(d); IF(ABS(d(i)) <= crit)d(i)=0.; ENDDO
END SUBROUTINE dprgv

SUBROUTINE mulcc(a,b,c,m)  ! Multiply circulant matrices of period M
INTEGER, INTENT(IN   ) :: m
REAL,    INTENT(IN   ) :: a(0:m-1), b(0:m-1)
REAL,    INTENT(OUT  ) :: c(0:m-1)
c(0:m-1) = 0.; CALL madcc(a,b,c,m)
END SUBROUTINE mulcc
SUBROUTINE dmulcc(a,b,c,m)  ! Multiply circulant matrices of period M
INTEGER, INTENT(IN   ) :: m
REAL(8), INTENT(IN   ) :: a(0:m-1), b(0:m-1)
REAL(8), INTENT(OUT  ) :: c(0:m-1)
c=0.; CALL dmadcc(a,b,c,m)
END SUBROUTINE dmulcc

SUBROUTINE msbcc(a,b,c,m)
INTEGER, INTENT(IN   ) :: m
REAL,    INTENT(IN   ) :: a(0:m-1), b(0:m-1)
REAL,    INTENT(INOUT) :: c(0:m-1)
c=-c; CALL madcc(a,b,c,m); c=-c
END SUBROUTINE msbcc
SUBROUTINE dmsbcc(a,b,c,m)
INTEGER, INTENT(IN   ) :: m
REAL(8), INTENT(IN   ) :: a(0:m-1), b(0:m-1)
REAL(8), INTENT(INOUT) :: c(0:m-1)
c=-c; CALL dmadcc(a,b,c,m); c=-c
END SUBROUTINE dmsbcc

SUBROUTINE madcc(a,b,c,m)
INTEGER, INTENT(IN   ) :: m
REAL,    INTENT(IN   ) :: a(0:m-1), b(0:m-1)
REAL,    INTENT(INOUT) :: c(0:m-1)
INTEGER                :: j
DO j=0,m-1
  c(j:  ) = c(j:  ) + a(:m-j-1)*b(j)
  c(:j-1) = c(:j-1) + a(m-j:  )*b(j)
ENDDO
END SUBROUTINE madcc
SUBROUTINE dmadcc(a,b,c,m)
INTEGER, INTENT(IN   ) :: m
REAL(8), INTENT(IN   ) :: a(0:m-1), b(0:m-1)
REAL(8), INTENT(INOUT) :: c(0:m-1)
INTEGER                :: j
DO j=0,m-1
  c(j:  ) = c(j:  ) + a(:m-j-1)*b(j)
  c(:j-1) = c(:j-1) + a(m-j:  )*b(j)
ENDDO
END SUBROUTINE dmadcc

SUBROUTINE zerl(a)       ! Zero out the strictly lower triangle of elements
REAL,    INTENT(INOUT) :: a(:,:)
INTEGER                :: j
DO j=1,SIZE(a,1); a(j+1:,j) = 0; ENDDO
END SUBROUTINE zerl
SUBROUTINE dzerl(a)
REAL(8), INTENT(INOUT) :: a(:,:)
INTEGER                :: j
DO j=1,SIZE(a,1); a(j+1:,j) = 0; ENDDO
END SUBROUTINE dzerl

SUBROUTINE zeru(a)       ! Zero out the strictly upper triangle of elements
REAL,    INTENT(INOUT) :: a(:,:)
INTEGER                :: j
DO j=1,SIZE(a,1); a(:j-1,j) = 0; ENDDO
END SUBROUTINE zeru
SUBROUTINE dzeru(a)
REAL(8), INTENT(INOUT) :: a(:,:)
INTEGER                :: j
DO j=1,SIZE(a,1); a(:j-1,j) = 0; ENDDO
END SUBROUTINE dzeru

!------------------------------------------------------------------------------
!   R.J.Purser, NCEP, Washington D.C.	1996
!		    SUBROUTINE	LDUM
!  perform l-d-u decomposition of square matrix a in place with
!
!  <-> a    square matrix to be factorized
!  <-- ipiv array encoding the pivoting sequence
!  <-- d    indicator for possible sign change of determinant
!------------------------------------------------------------------------------
SUBROUTINE ldum(a,ipiv,d)
REAL,    INTENT(INOUT) :: a(:,:) 
REAL,    INTENT(OUT  ) :: d
INTEGER, INTENT(OUT  ) :: ipiv(:)
INTEGER                :: m,i, j, jp, ibig, jm
REAL                   :: s(SIZE(a,1)),  aam, aa, abig,  ajj, ajji, aij
m=SIZE(a,1)
DO i=1,m
  aam=0.
  DO j=1,m
    aa=ABS(a(i,j))
    IF(aa > aam)aam=aa
  ENDDO
  IF(aam == 0.)THEN
    PRINT '(" row ",i3," of matrix in ldum vanishes")',i
    STOP
  ENDIF
  s(i)=1./aam
ENDDO
d=1.
ipiv(m)=m
DO j=1,m-1
  jp=j+1
  abig=s(j)*ABS(a(j,j))
  ibig=j
  DO i=jp,m
    aa=s(i)*ABS(a(i,j))
    IF(aa > abig)THEN
      ibig=i
      abig=aa
    ENDIF
  ENDDO
!  swap rows, recording changed sign of determinant
  ipiv(j)=ibig
  IF(ibig /= j)THEN
    d=-d
    CALL swpvv(a(j,:),a(ibig,:))
    s(ibig)=s(j)
  ENDIF
  ajj=a(j,j)
  IF(ajj == 0.)THEN
    jm=j-1
    PRINT '(" failure in ldum:"/" matrix singular, rank=",i3)',jm
    STOP
  ENDIF
  ajji=1./ajj
  DO i=jp,m
    aij=ajji*a(i,j)
    a(i,j)=aij
    a(i,jp:m) = a(i,jp:m) - aij*a(j,jp:m)
  ENDDO
ENDDO
END SUBROUTINE ldum
SUBROUTINE DLDUM(A,IPIV,D)
REAL(8), INTENT(INOUT) :: a(:,:) 
REAL(8), INTENT(OUT  ) :: d
INTEGER, INTENT(OUT  ) :: ipiv(:)
INTEGER                :: m,i, j, jp, ibig, jm
REAL(8)                :: s(SIZE(a,1)),  aam, aa, abig,  ajj, ajji, aij
m=SIZE(a,1)
DO i=1,m
  aam=0.
  DO j=1,m
    aa=ABS(a(i,j))
    IF(aa > aam)aam=aa
  ENDDO
  IF(aam == 0.d0)THEN
    PRINT '(" row ",i3," of matrix in dldum vanishes")',i
    STOP
  ENDIF
  s(i)=1./aam
ENDDO
d=1.
ipiv(m)=m
DO j=1,m-1
  jp=j+1
  abig=s(j)*ABS(a(j,j))
  ibig=j
  DO i=jp,m
    aa=s(i)*ABS(a(i,j))
    IF(aa > abig)THEN
      ibig=i
      abig=aa
    ENDIF
  ENDDO
!  swap rows, recording changed sign of determinant
  ipiv(j)=ibig
  IF(ibig /= j)THEN
    d=-d
    CALL swpvv_d(a(j,:),a(ibig,:))
    s(ibig)=s(j)
  ENDIF
  ajj=a(j,j)
  IF(ajj == 0.d0)THEN
    jm=j-1
    PRINT '(" Failure in dldum:"/" matrix singular, rank=",i3)',jm
    STOP
  ENDIF
  ajji=1./ajj
  DO i=jp,m
    aij=ajji*a(i,j)
    a(i,j)=aij
    a(i,jp:m) = a(i,jp:m) - aij*a(j,jp:m)
  ENDDO
ENDDO
END SUBROUTINE dldum

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!		    SUBROUTINE UDLMM
!  use l-u factors in A to back-substitute for several rhs in B, using ipiv to
!  define the pivoting permutation used in the l-u decomposition.
!
!  --> A    L-D-U factorization of linear system matrux
!  <-> B    rt-hand-sides vectors on input, corresponding solutions on return
!  --> IPIV array encoding the pivoting sequence
!------------------------------------------------------------------------------
SUBROUTINE udlmm(a,b,ipiv)
INTEGER, DIMENSION(:),  INTENT(IN)    :: ipiv 
REAL,    DIMENSION(:,:),INTENT(IN)    :: a 
REAL,    DIMENSION(:,:),INTENT(INOUT) :: b 
INTEGER                               :: m,i, k, l
REAL                                  :: s,aiii
m=SIZE(a,1)
DO k=1,SIZE(b,2) !loop over columns of b
  DO i=1,m
    l=ipiv(i)
    s=b(l,k)
    b(l,k)=b(i,k)
    s = s - SUM(b(1:i-1,k)*a(i,1:i-1))
    b(i,k)=s
  ENDDO
  b(m,k)=b(m,k)/a(m,m)
  DO i=m-1,1,-1
    aiii=1./a(i,i)
    b(i,k) = b(i,k) - SUM(b(i+1:m,k)*a(i,i+1:m))
    b(i,k)=b(i,k)*aiii
  ENDDO
ENDDO
END SUBROUTINE udlmm
SUBROUTINE dudlmm(a,b,ipiv)
INTEGER, DIMENSION(:),  INTENT(IN   ) :: ipiv 
REAL(8), DIMENSION(:,:),INTENT(IN   ) :: a 
REAL(8), DIMENSION(:,:),INTENT(INOUT) :: b 
INTEGER                               :: m,i, k, l
REAL(8)                               :: s,aiii
m=SIZE(a,1)
DO k=1, SIZE(b,2)!loop over columns of b
  DO i=1,m
    l=ipiv(i)
    s=b(l,k)
    b(l,k)=b(i,k)
    s = s - SUM(b(1:i-1,k)*a(i,1:i-1))
    b(i,k)=s
  ENDDO
  b(m,k)=b(m,k)/a(m,m)
  DO i=m-1,1,-1
    aiii=1./a(i,i)
    b(i,k) = b(i,k) - SUM(b(i+1:m,k)*a(i,i+1:m))
    b(i,k)=b(i,k)*aiii
  ENDDO
ENDDO
END SUBROUTINE dudlmm

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!		    SUBROUTINE UDLMV
!  use l-u factors in A to back-substitute for 1 rhs in B, using ipiv to
!  define the pivoting permutation used in the l-u decomposition.
!
!  --> A    L-D-U factorization of linear system matrux
!  <-> B    right-hand-side vector on input, corresponding solution on return
!  --> IPIV array encoding the pivoting sequence
!------------------------------------------------------------------------------
SUBROUTINE udlmv(a,b,ipiv)
INTEGER, DIMENSION(:),  INTENT(IN)    :: ipiv 
REAL,    DIMENSION(:,:),INTENT(IN)    :: a 
REAL,    DIMENSION(:),  INTENT(INOUT) :: b 
INTEGER                               :: m,i, l
REAL                                  :: s,aiii
m=SIZE(a,1)
DO i=1,m
   l=ipiv(i)
   s=b(l)
   b(l)=b(i)
   s = s - SUM(b(1:i-1)*a(i,1:i-1))
   b(i)=s
ENDDO
b(m)=b(m)/a(m,m)
DO i=m-1,1,-1
   aiii=1./a(i,i)
   b(i) = b(i) - SUM(b(i+1:m)*a(i,i+1:m))
   b(i)=b(i)*aiii
ENDDO
END SUBROUTINE udlmv
SUBROUTINE dudlmv(a,b,ipiv)
INTEGER,  DIMENSION(:),  INTENT(IN   ) :: ipiv(:) 
REAL(8),  DIMENSION(:,:),INTENT(IN   ) :: a(:,:) 
REAL(8),  DIMENSION(:),  INTENT(INOUT) :: b(:) 
INTEGER                                :: m,i, l
REAL(8)                                :: s,aiii
m=SIZE(a,1)
DO i=1,m
   l=ipiv(i)
   s=b(l)
   b(l)=b(i)
   s = s - SUM(b(1:i-1)*a(i,1:i-1))
   b(i)=s
ENDDO
b(m)=b(m)/a(m,m)
DO i=m-1,1,-1
   aiii=1./a(i,i)
   b(i) = b(i) - SUM(b(i+1:m)*a(i,i+1:m))
   b(i)=b(i)*aiii
ENDDO
END SUBROUTINE dudlmv

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!		    SUBROUTINE LINVAN
!
!   Take square matrix W and seek row and column scalings to produce non-
!   vanishing elements of rescaled W having magnitudes as close to unity
!   as possible. The approach is make the geometric mean of the nonvanishing
!   elements of each row and of each column +1 or -1. Having rescaled the
!   matrix and the r.h.s. vector AB, compute the product P of row-vector
!   norms, then compute the determinant D and solve the linear system.
!   Rescale the solution vector (now AB) and put the conditioning indicator
!   formed by the ratio D/P into the first element of W.
!
! <-> W:    Generalized Vandermonde matrix in, conditioning indicator out.
! <-> AB:   R.h.s. vector in, solution vector of numerical coefficients out.
!------------------------------------------------------------------------------
SUBROUTINE linvan(w,ab)
INTEGER, PARAMETER  :: nit=20
REAL, INTENT(INOUT) :: w(:,:), ab(:)
REAL                :: d1(SIZE(w,1)), d2(SIZE(w,1)), &
                       w2(SIZE(w,1),SIZE(w,1)),v(SIZE(w,1))
INTEGER             :: i, j, it, jt, ipiv(SIZE(w,1)), nc
REAL                :: p, e, dw, c, d, s, d2j
REAL,ALLOCATABLE    :: wv(:,:) ! work variable for ab(nc) and v(nn)

nc = SIZE(w,DIM=1)
ALLOCATE(wv(nc,1))

w2=w                ! Preserve original W and AB for use
v = ab(1:nc)	    ! in later "clean-up" operation.

d1 = 1.0 	    ! Row scaling factors set to default
d2 = 1.0 	    ! Column scaling factors set to default

C=1.E-16	    ! Set initial criterion for "negligible" elements of W

! In first attempt to estimate row and column scalings, use logarithms
! to avoid the risk of under- or over-flows of the line products of W:
DO i=1,nc
  p=0.
  e=0.
  DO j=1,nc
    dw=ABS(w(i,j))
    IF(dw > c)THEN
      e=e+1.
      p=p+LOG(dw)
    ENDIF
  ENDDO
  IF(E == 0.)STOP 'W effectively singular in LINVAN'
  d1(i)=EXP(-p/e)
ENDDO
CALL muldm(d1,w2,w)

DO j=1,nc
  p=0.
  e=0.
  DO i=1,nc
    dw=ABS(w(i,j))
    IF(dw > c)THEN
      e=e+1.
      p=p+LOG(dw)
    ENDIF
  ENDDO
  IF(E == 0.)STOP 'W effectively singular in LINVAN'
  d2(j)=EXP(-p/e)
ENDDO
CALL mulmd(w,d2,w)

c=1.e-8  ! reset the criterion for "negligible" elements

! revert to iterations of the more efficient method without logarithms:
DO jt=1,2
DO it=1,nit	    !	perform nit relaxation iterations
  DO i=1,nc	    !	do rows:
    p=1.
    e=0.
    DO j=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
        e=e+1.
        p=p*dw
      ENDIF
    ENDDO
    p=1./(p**(1./e))
    w(i,:) = w(i,:) * p            ! rescale this row of w..
    d1(i)=d1(i)*p			     ! ..and update d1 consistently
  ENDDO
  DO j=1,nc	    !	do columns:
    p=1.
    e=0.
    d2j=d2(j)
    DO i=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
        e=e+1.
        p=p*dw
      ENDIF
    ENDDO
    p=1./(p**(1./e))
    w(:,j) = w(:,j) * p        ! rescale this column of w..
    d2(j)=d2(j)*p		       ! ..and update d2 consistently
  ENDDO
ENDDO
c=1.e-3	    ! final setting for criterion for "negligible" elements
ENDDO
ab(1:nc) = d1(1:nc) * ab(1:nc) ! rescale r.h.s vector by d1
p=1.			     ! p becomes product of row-lengths:
DO i=1,nc
   p=p*SQRT(dot_PRODUCT(w(i,:),w(i,:)))
ENDDO
CALL ldum(w,ipiv,d)
DO i=1,nc
  d=d*w(i,i)		      ! d becomes the determinant of w
ENDDO
wv(:,1) = ab ! convert shape of array
CALL udlmm(w,wv(:,1:1),ipiv)
ab = d2 * wv(:,1) ! rescale solution vector by d2
!     ab(1:nc) = d2(1:nc) * ab(1:nc) ! rescale solution vector by d2
!  note: it is very likely that round-off errors have accumulated during
!  the iterative rescaling of w. we invoke original matrix elements w2 and
!  substitute the tentative solution vector into the original (unscaled)
!  equation in order to estimate the residual components of roundoff error.

!  begin "clean-up" process. substitute solution vector in original
!  equation and leave the residual difference in v
v=v-MATMUL(w2,ab)
v = d1 * v    ! rescale the residual vector by d1
wv(:,1) = v ! convert shape of array
CALL udlmm(w,wv(:,1:1),ipiv) ! solve linear system with this rhs.
ab=ab+wv(:,1)*d2 ! add residual solution vector, 
                                      ! scaled, to ab

	  DEALLOCATE(wv)
w(1,1)=d/p  ! this ratio is an indicator of the overall conditioning
            ! when d/p is very small, treat the results with suspicion!
END SUBROUTINE linvan
SUBROUTINE dlinvan(w,ab)
INTEGER, PARAMETER     :: nit=20
REAL(8), INTENT(INOUT) :: w(:,:), ab(:)
REAL(8)                :: d1(SIZE(w,1)), d2(SIZE(w,1)), &
                          w2(SIZE(w,1),SIZE(w,1)),v(SIZE(w,1))
INTEGER                :: i, j, it, jt, ipiv(SIZE(w,1)), nc
REAL(8)                :: p, e, dw, c, d, s, d2j
REAL(8),ALLOCATABLE    :: wv(:,:) ! work variable for ab(nc) and v(nn)

nc = SIZE(w,DIM=1)
ALLOCATE(wv(nc,1))

w2=w                ! Preserve original W and AB for use
v = ab(1:nc)	    ! in later "clean-up" operation.

d1 = 1.0 	    ! Row scaling factors set to default
d2 = 1.0 	    ! Column scaling factors set to default

C=1.E-16	    ! Set initial criterion for "negligible" elements of W

! In first attempt to estimate row and column scalings, use logarithms
! to avoid the risk of under- or over-flows of the line products of W:
DO i=1,nc
  p=0.
  e=0.
  DO j=1,nc
    dw=ABS(w(i,j))
    IF(dw > c)THEN
      e=e+1.
      p=p+LOG(dw)
    ENDIF
  ENDDO
  IF(e == 0.d0)STOP 'w effectively singular in linvan'
  d1(i)=EXP(-p/e)
ENDDO
CALL muldm_d(d1,w2,w)

DO j=1,nc
  p=0.
  e=0.
  DO i=1,nc
    dw=ABS(w(i,j))
    IF(dw > c)THEN
      e=e+1.
      p=p+LOG(dw)
    ENDIF
  ENDDO
  IF(e == 0.)STOP 'w effectively singular in linvan'
  d2(j)=EXP(-p/e)
ENDDO
CALL mulmd_d(w,d2,w)

c=1.e-8  ! reset the criterion for "negligible" elements

! revert to iterations of the more efficient method without logarithms:
DO jt=1,2
DO it=1,nit	    !	perform nit relaxation iterations
  DO i=1,nc	    !	do rows:
    p=1.
    e=0.
    DO j=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
        e=e+1.
        p=p*dw
      ENDIF
    ENDDO
    p=1./(p**(1./e))
    w(i,:) = w(i,:) * p            ! rescale this row of w..
    d1(i)=d1(i)*p			     ! ..and update d1 consistently
  ENDDO
  DO j=1,nc	    !	do columns:
    p=1.
    e=0.
    d2j=d2(j)
    DO i=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
        e=e+1.
        p=p*dw
      ENDIF
    ENDDO
    p=1./(p**(1./e))
    w(:,j) = w(:,j) * p        ! rescale this column of w..
    d2(j)=d2(j)*p		       ! ..and update d2 consistently
  ENDDO
ENDDO
c=1.e-3	    ! final setting for criterion for "negligible" elements
ENDDO
ab(1:nc) = d1(1:nc) * ab(1:nc) ! rescale r.h.s vector by d1
p=1.			     ! p becomes product of row-lengths:
DO i=1,nc
   p=p*SQRT(dot_PRODUCT(w(i,:),w(i,:)))
ENDDO
CALL ldum_d(w,ipiv,d)
DO i=1,nc
  d=d*w(i,i)		      ! d becomes the determinant of w
ENDDO
wv(:,1) = ab ! convert shape of array
CALL udlmm_d(w,wv(:,1:1),ipiv)
ab = d2 * wv(:,1) ! rescale solution vector by d2
!     ab(1:nc) = d2(1:nc) * ab(1:nc) ! Rescale solution vector by D2
!  Note: it is very likely that round-off errors have accumulated during
!  the iterative rescaling of W. We invoke original matrix elements W2 and
!  substitute the tentative solution vector into the original (unscaled)
!  equation in order to estimate the residual components of roundoff error.

!  Begin "clean-up" process. Substitute solution vector in original
!  equation and leave the residual difference in V
v=v-MATMUL(w2,ab)
v = d1 * v    ! Rescale the residual vector by D1
wv(:,1) = v ! Convert shape of array
CALL UDLMM_d(w,wv(:,1:1),ipiv) ! Solve linear system with THIS rhs.
ab=ab+wv(:,1)*d2 ! Add residual solution vector, 
                                      ! scaled, to AB

	  DEALLOCATE(wv)
w(1,1)=d/p  ! this ratio is an indicator of the overall conditioning
            ! When D/P is very small, treat the results with suspicion!
END SUBROUTINE dlinvan

subroutine condm(d,a)
REAL,DIMENSION(:),INTENT(IN)::d; REAL,DIMENSION(:,:),INTENT(OUT)::a
call copdm(d,a); a=-a
END SUBROUTINE condm
subroutine dcondm(d,a)
REAL(8),DIMENSION(:),INTENT(IN)::d; REAL(8),DIMENSION(:,:),INTENT(OUT)::a
call dcopdm(d,a); a=-a
END SUBROUTINE dcondm

SUBROUTINE copdm(d,a)
REAL,DIMENSION(:),INTENT(IN)::d; REAL,DIMENSION(:,:),INTENT(OUT)::a; INTEGER i
a=0.; DO i=1,SIZE(a,1); a(i,i)= d(i); ENDDO
END SUBROUTINE copdm
SUBROUTINE dcopdm(d,a)
REAL(8),DIMENSION(:),INTENT(IN)::d; 
REAL(8),DIMENSION(:,:),INTENT(OUT)::a; INTEGER i
a=0.; DO i=1,SIZE(a,1); a(i,i)= d(i); ENDDO
END SUBROUTINE dcopdm

subroutine consm(s,a)
REAL,INTENT(IN) :: s; REAL,DIMENSION(:,:),INTENT(OUT):: a
call copsm(s,a); a=-a
END SUBROUTINE consm
subroutine dconsm(s,a)
REAL(8),INTENT(IN) :: s; REAL(8),DIMENSION(:,:),INTENT(OUT):: a
call dcopsm(s,a); a=-a
END SUBROUTINE dconsm

SUBROUTINE copsm(s,a)
REAL,INTENT(IN) :: s; REAL,DIMENSION(:,:),INTENT(OUT):: a; INTEGER i
a=0.; DO i=1,SIZE(a,1); a(i,i)= s; ENDDO
end subroutine copsm
SUBROUTINE dcopsm(s,a)
REAL(8),INTENT(IN) :: s; REAL(8),DIMENSION(:,:),INTENT(OUT):: a; INTEGER i
a=0.; DO i=1,SIZE(a,1); a(i,i)= s; ENDDO
end subroutine dcopsm

SUBROUTINE l1lm(a,b) ! Cholesky, M -> L*U, U(i,j)=L(j,i)
REAL, INTENT(IN)    :: a(:,:)
REAL, INTENT(INOUT) :: b(:,:)
INTEGER             :: m,j, jm, jp, i
REAL                :: s, bjji
m=SIZE(a,1)
DO j=1,m
  jm=j-1
  jp=j+1
  s = a(j,j) - SUM(b(j,1:jm)*b(j,1:jm))
  IF(S <= 0.)THEN
    PRINT '(" L1LM detects non-positivity at diagonal index",i2)',J
    STOP
  ENDIF
  b(j,j)=SQRT(s)
  bjji=1./b(j,j)
  DO i=jp,m
    s = a(i,j) - SUM(b(i,1:jm)*b(j,1:jm))
    b(i,j)=s*bjji
  ENDDO
  b(1:jm,j) = 0.0
ENDDO
END SUBROUTINE l1lm
SUBROUTINE DL1LM(A,B) ! Cholesky, M -> L*U, U(i,j)=L(j,i)
REAL(8), INTENT(IN)    :: a(:,:) 
REAL(8), INTENT(INOUT) :: b(:,:) 
INTEGER                :: m,j, jm, jp, i
REAL(8)                :: s, bjji
m=SIZE(a,1)
DO j=1,m
  jm=j-1
  jp=j+1
  s = a(j,j) - SUM(b(j,1:jm)*b(j,1:jm))
  IF(s <= 0.d0)THEN
    PRINT '(" L1LM detects non-positivity at diagonal index",i2)',J
    STOP
  ENDIF
  b(j,j)=SQRT(s)
  bjji=1./b(j,j)
  DO i=jp,m
    s = a(i,j) - SUM(b(i,1:jm)*b(j,1:jm))
    b(i,j)=s*bjji
  ENDDO
  b(1:jm,j) = 0.0
ENDDO
RETURN
END SUBROUTINE dl1lm

SUBROUTINE ldlm(a,b,d) ! Modified Cholesky decompose Q --> L*D*U, U(i,j)=L(j,i)
REAL, INTENT(IN)    :: a(:,:)
REAL, INTENT(INOUT) :: b(:,:)
REAL, INTENT(OUT)   :: d(:)
INTEGER             :: m,j, jm, jp, i
REAL                :: s, bjji
m=SIZE(a,1)
DO j=1,m
  jm=j-1
  jp=j+1
  d(j)=a(j,j) - SUM(b(1:jm,j)*b(j,1:jm))
  
  b(j,j) = 1.
  IF(d(j) == 0.)THEN
    PRINT '(" LDLM detects singularity at diagonal index",i2)',J
    STOP
  ENDIF
  bjji=1./d(j)
  DO i=jp,m
     b(j,i)=a(i,j) - DOT_PRODUCT(b(1:jm,j),b(i,1:jm))
     b(i,j)=b(j,i)*bjji
  ENDDO
ENDDO
CALL zeru(b)
RETURN
END SUBROUTINE ldlm
SUBROUTINE dldlm(a,b,d) ! Modified Cholesky  Q --> L*D*U, U(i,j)=L(j,i)
REAL(8), INTENT(IN)    :: a(:,:)
REAL(8), INTENT(INOUT) :: b(:,:)
REAL(8), INTENT(OUT)   :: d(:)
INTEGER                :: m,j, jm, jp, i
REAL(8)                :: s, bjji
m=SIZE(a,1)
DO j=1,m; jm=j-1; jp=j+1
  d(j)=a(j,j) - SUM(b(1:jm,j)*b(j,1:jm))
  b(j,j) = 1.
  IF(d(j) == 0.d0)THEN
    PRINT '(" DLDLM detects singularity at diagonal index",i2)',J
    STOP
  ENDIF
  bjji=1./d(j)
  DO i=jp,m
     b(j,i)=a(i,j) - DOT_PRODUCT(b(1:jm,j),b(i,1:jm))
     b(i,j)=b(j,i)*bjji
  ENDDO
ENDDO
CALL zeru_d(b)
RETURN
END SUBROUTINE dldlm

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!		    SUBROUTINE INVH
!  Invert in place, a symmetric matrix
!
!  <-> A    symmetric square matrix, output as inverse of input
!
!  LIMITATION
!     This routine incorporates no pivoting - it is intended for matrices
!     that are already diagonally dominant
!------------------------------------------------------------------------------
SUBROUTINE invh(a)
REAL,    INTENT(INOUT)      :: a(:,:) 
INTEGER                     :: m,i,j
REAL,   DIMENSION(SIZE(a,1)):: d
m=SIZE(a,1)
!  PERFORM L.D.U DECOMPOSITION OF THE SYMMETRIC MATRIX:
CALL ldlm(a,a,d)

!  INVERT (IN PLACE) THE LOWER TRIANGULAR PART OF A, (ASSUMING UNIT
!  DIAGONAL ELEMENTS), AND INVERT THE DIAGONAL PART OF A (ASSUMING
!  ZERO OFF-DIAGONAL ELEMENTS). PUT TRANSPOSE OF LOWER, TIMES DIAGONAL,
!  INTO UPPER PART OF A.
DO j=1,m
  a(j,j)=1./d(j)
  DO i=j+1,m
    a(i,j)=-a(i,j)-SUM(a(j+1:i-1,j)*a(i,j+1:i-1)) 
  ENDDO
ENDDO

!  MULTIPLY: THE TRANSPOSE OF THE LOWER PART OF A (ASSUMING UNIT DIAGS),
!  TIMES THE DIAGONAL PART (ASSUMING ZERO OFF-DIAGS), TIMES THE LOWER
!  PART. THIS PRODUCT IS THE SYMMETRIC INVERSE OF THE ORIGINAL B.
DO i=2,m
  a(1:i-1,i) = a(i,1:i-1) * a(i,i) 
ENDDO
DO i=1,m
  DO j=1,i
    a(j,i)=a(j,i) + SUM(a(i+1:m,i)*a(j,i+1:m))
    a(i,j)=a(j,i)
  ENDDO
ENDDO
END SUBROUTINE invh
SUBROUTINE dinvh(a)
REAL(8), INTENT(INOUT)      :: a(:,:) 
INTEGER                     :: m,i,j
REAL(8),DIMENSION(SIZE(a,1)):: d
m=SIZE(a,1)
!  PERFORM L.D.U DECOMPOSITION OF THE SYMMETRIC MATRIX:
CALL ldlm_d(a,a,d)

!  INVERT (IN PLACE) THE LOWER TRIANGULAR PART OF A, (ASSUMING UNIT
!  DIAGONAL ELEMENTS), AND INVERT THE DIAGONAL PART OF A (ASSUMING
!  ZERO OFF-DIAGONAL ELEMENTS). PUT TRANSPOSE OF LOWER, TIMES DIAGONAL,
!  INTO UPPER PART OF A.
DO j=1,m
  a(j,j)=1./d(j)
  DO i=j+1,m
    a(i,j)=-a(i,j)-SUM(a(j+1:i-1,j)*a(i,j+1:i-1)) 
  ENDDO
ENDDO

!  MULTIPLY: THE TRANSPOSE OF THE LOWER PART OF A (ASSUMING UNIT DIAGS),
!  TIMES THE DIAGONAL PART (ASSUMING ZERO OFF-DIAGS), TIMES THE LOWER
!  PART. THIS PRODUCT IS THE SYMMETRIC INVERSE OF THE ORIGINAL B.
DO i=2,m
  a(1:i-1,i) = a(i,1:i-1) * a(i,i) 
ENDDO
DO i=1,m
  DO j=1,i
    a(j,i)=a(j,i) + SUM(a(i+1:m,i)*a(j,i+1:m))
    a(i,j)=a(j,i)
  ENDDO
ENDDO
END SUBROUTINE dinvh

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE INVL
!     Invert lower triangular matrix in place if A are same
!------------------------------------------------------------------------------
SUBROUTINE invl(a)
REAL, INTENT(INOUT) :: a(:,:) 
INTEGER             :: m,j, i
m=SIZE(a,1)
DO j=m,1,-1
  a(1:j-1,j) = 0.0
  a(j,j)=1./a(j,j)
  DO i=j+1,m
    a(i,j)=-a(i,i)*SUM(a(j:i-1,j)*a(i,j:i-1))
  ENDDO
ENDDO
END SUBROUTINE invl
SUBROUTINE dinvl(a)
REAL(8), INTENT(INOUT) :: a(:,:) 
INTEGER                :: m,j, i
m=SIZE(a,1)
DO j=m,1,-1
  a(1:j-1,j) = 0.0
  a(j,j)=1./a(j,j)
  DO i=j+1,m
    a(i,j)=-a(i,i)*SUM(a(j:i-1,j)*a(i,j:i-1))
  ENDDO
ENDDO
END SUBROUTINE dinvl

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE LINLV
!     Solve linear system involving lower triangular system matrix.
! --> a: system matrix
! <-> u: vector, input as rhs, output as solution.
!------------------------------------------------------------------------------
SUBROUTINE linlv(a,u)
REAL,    INTENT(IN   ) :: a(:,:)
REAL,    INTENT(INOUT) :: u(:)
INTEGER             :: i
if(size(a,1) /= size(a,2) .or. size(a,1) /= size(u))&
     stop 'In linlv; incompatible array dimensions'
DO i=1,SIZE(u); u(i)=(u(i) - SUM(u(:i-1)*a(i,:i-1)))/a(i,i); ENDDO
end subroutine linlv
SUBROUTINE dlinlv(a,u)
REAL(8), INTENT(IN   ) :: a(:,:)
REAL(8), INTENT(INOUT) :: u(:)
INTEGER                :: i
if(size(a,1) /= size(a,2) .or. size(a,1) /= size(u))&
     stop 'In dlinlv; incompatible array dimensions'
DO i=1,SIZE(u); u(i)=(u(i) - SUM(u(:i-1)*a(i,:i-1)))/a(i,i); ENDDO
end subroutine dlinlv

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE LINUV
!     Solve linear system involving upper triangular system matrix.
! --> a: system matrix
! <-> u: vector, input as rhs, output as solution.
!------------------------------------------------------------------------------
subroutine linuv(a,u)
REAL,    INTENT(IN   ) :: a(:,:)
REAL,    INTENT(INOUT) :: u(:)
INTEGER             :: i
if(size(a,1) /= size(a,2) .or. size(a,1) /= size(u))&
     stop 'In linuv; incompatible array dimensions'
DO i=size(u),1,-1; u(i)=(u(i) - SUM(a(i+1:,i)*u(i+1:)))/a(i,i); ENDDO
END SUBROUTINE linuv
subroutine dlinuv(a,u)
REAL(8), INTENT(IN   ) :: a(:,:)
REAL(8), INTENT(INOUT) :: u(:)
INTEGER             :: i
if(size(a,1) /= size(a,2) .or. size(a,1) /= size(u))&
     stop 'In dlinuv; incompatible array dimensions'
DO i=size(u),1,-1; u(i)=(u(i) - SUM(a(i+1:,i)*u(i+1:)))/a(i,i); ENDDO
END SUBROUTINE dlinuv

FUNCTION trcm(a) RESULT(trc_res)	    ! Trace of square matrix A
REAL             :: trc_res
REAL, INTENT(IN) :: a(:,:)
INTEGER          :: i
trc_res=0.; DO i=1,SIZE(a,1); trc_res=trc_res+a(i,i); ENDDO
END FUNCTION trcm
FUNCTION dtrcm(a) RESULT(trc_res)	    ! Trace of square matrix A
REAL(8)             :: trc_res
REAL(8), INTENT(IN) :: a(:,:)
INTEGER              :: m,i
trc_res=0.; DO i=1,SIZE(a,1); trc_res=trc_res+a(i,i); ENDDO
END FUNCTION dtrcm

SUBROUTINE invmt(a)
REAL,DIMENSION(:,:),INTENT(INOUT):: a
INTEGER m,i,j,jp,l
REAL d
INTEGER,DIMENSION(SIZE(a,1)):: ipiv
m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to invmt is not square'
! Perform a pivoted L-D-U decomposition on matrix a:
CALL ldum(a,ipiv,d)

! Invert upper triangular portion U in place:
DO i=1,m; a(i,i)=1./a(i,i); ENDDO
DO i=1,m-1
   DO j=i+1,m; a(i,j)=-a(j,j)*DOT_PRODUCT(a(i:j-1,j),a(i,i:j-1)); ENDDO
ENDDO

! Invert lower triangular portion L in place:
DO j=1,m-1; jp=j+1
   DO i=jp,m; a(i,j)=-a(i,j)-DOT_PRODUCT(a(jp:i-1,j),a(i,jp:i-1)); ENDDO
ENDDO

!  Form the product of U**-1 and L**-1 in place
DO j=1,m-1; jp=j+1
   DO i=1,j; a(i,j)=a(i,j)+DOT_PRODUCT(a(jp:m,j),a(i,jp:m)); ENDDO
   DO i=jp,m; a(i,j)=DOT_PRODUCT(a(i:m,j),a(i,i:m));         ENDDO
ENDDO

!  Permute columns according to ipiv
DO j=m-1,1,-1; l=ipiv(j); CALL swpvv(a(:,j),a(:,l)); ENDDO
END SUBROUTINE invmt
SUBROUTINE dinvmt(a)
REAL(8),DIMENSION(:,:),INTENT(INOUT):: a
INTEGER                             :: m,i,j,jp,l
REAL(8)                             :: d
INTEGER,DIMENSION(SIZE(a,1))        :: ipiv
m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to dinvmt is not square'
! Perform a pivoted L-D-U decomposition on matrix a:
CALL ldum_d(a,ipiv,d)

! Invert upper triangular portion U in place:
DO i=1,m; a(i,i)=1./a(i,i); ENDDO
DO i=1,m-1
   DO j=i+1,m; a(i,j)=-a(j,j)*DOT_PRODUCT(a(i:j-1,j),a(i,i:j-1)); ENDDO
ENDDO

! Invert lower triangular portion L in place:
DO j=1,m-1; jp=j+1
   DO i=jp,m; a(i,j)=-a(i,j)-DOT_PRODUCT(a(jp:i-1,j),a(i,jp:i-1)); ENDDO
ENDDO

!  Form the product of U**-1 and L**-1 in place
DO j=1,m-1; jp=j+1
   DO i=1,j; a(i,j)=a(i,j)+DOT_PRODUCT(a(jp:m,j),a(i,jp:m)); ENDDO
   DO i=jp,m; a(i,j)=DOT_PRODUCT(a(i:m,j),a(i,i:m));         ENDDO
ENDDO

!  Permute columns according to ipiv
DO j=m-1,1,-1; l=ipiv(j); CALL swpvv_d(a(:,j),a(:,l)); ENDDO
END SUBROUTINE dinvmt

SUBROUTINE linmmt(a,b)
REAL,   DIMENSION(:,:),INTENT(INOUT):: a,b
INTEGER,DIMENSION(SIZE(a,1))        :: ipiv
INTEGER                             :: m
REAL                                :: d
m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to linmmt is not square'
IF(m /= SIZE(b,1))STOP 'matrix and vectors in linmmt have unmatched sizes'
CALL ldum(a,ipiv,d); CALL udlmm(a,b,ipiv)
END SUBROUTINE linmmt
SUBROUTINE dlinmmt(a,b)
REAL(8),DIMENSION(:,:),INTENT(INOUT):: a,b
INTEGER,DIMENSION(SIZE(a,1))        :: ipiv
INTEGER                             :: m 
REAL(8)                             :: d
m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to linmmt_d is not square'
IF(m /= SIZE(b,1))STOP 'matrix and vectors in linmmt_d have unmatched sizes'
CALL ldum_d(a,ipiv,d); CALL udlmm_d(a,b,ipiv)
END SUBROUTINE dlinmmt

SUBROUTINE linmvt(a,b)
REAL,   DIMENSION(:,:),INTENT(INOUT):: a
REAL,   DIMENSION(:),  INTENT(INOUT):: b
INTEGER,DIMENSION(SIZE(a,1))        :: ipiv
REAL                                :: d
IF(SIZE(a,1) /= SIZE(a,2).or. SIZE(a,1) /= SIZE(b))&
     STOP 'In linmvt; incompatible array dimensions'
CALL ldum(a,ipiv,d); CALL udlmm(a,b,ipiv)
END SUBROUTINE linmvt
SUBROUTINE dlinmvt(a,b)
REAL(8),DIMENSION(:,:),INTENT(INOUT):: a
REAL(8),DIMENSION(:),  INTENT(INOUT):: b
INTEGER,DIMENSION(SIZE(a,1))        :: ipiv
REAL(8)                             :: d
IF(SIZE(a,1) /= SIZE(a,2).or. SIZE(a,1) /= SIZE(b))&
     STOP 'In dlinmvt; incompatible array dimensions'
CALL ldum_d(a,ipiv,d); CALL udlmm_d(a,b,ipiv)
END SUBROUTINE dlinmvt

FUNCTION mcmax(a,b,m) RESULT(mmx_res)
INTEGER             :: mmx_res
INTEGER, INTENT(IN) :: m
REAL,    INTENT(IN) :: a(0:m), b(0:m)
INTEGER             :: ma, mb
mmx_res=0		       ! default for when ALL elements of c are zero
DO ma=m,0,-1	               ! seek last nonzero coefficient of polynomial a
  IF(a(ma) /= 0.)THEN
    DO mb=m,0,-1	       ! seek last nonzero coefficient of polynomial b
      IF(b(mb) /= 0.)THEN
        mmx_res=MIN(m,ma+mb)+1 ! hence, 1+last non-0 element of their product
        RETURN
      ENDIF
    ENDDO
    RETURN
  ENDIF
ENDDO
END FUNCTION mcmax
FUNCTION dmcmax(a,b,m) RESULT(mmx_res)
INTEGER             :: mmx_res
INTEGER, INTENT(IN) :: m
REAL(8), INTENT(IN) :: a(0:m), b(0:m)
INTEGER             :: ma, mb
mmx_res=0		       ! default for when all elements of c are zero
DO ma=m,0,-1	               ! seek last nonzero coefficient of polynomial a
  IF(a(ma) /= 0.d0)THEN
    DO mb=m,0,-1	       ! seek last nonzero coefficient of polynomial b
      IF(b(mb) /= 0.d0)THEN
        mmx_res=MIN(m,ma+mb)+1 ! hence, 1+last non-0 element of their product
        RETURN
      ENDIF
    ENDDO
    RETURN
  ENDIF
ENDDO
RETURN
END FUNCTION dmcmax
!END MODULE MODULE_pmat1


