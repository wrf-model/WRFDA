!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!      	    SUBROUTINE TURNING
! turning conditions for multiple applications of the constant-coefficient 
! 1st-order recursive filter. 
!
!  --> m    Dimension of the boudaries (1 for 1 pass, 2 for 2 passes, etc)
!  <-- w    Array to store the boundries
!  --> A    Filter parameter (alpha)
!------------------------------------------------------------------------------
      SUBROUTINE DA_RF_Turning_Conditions (M,W,A)

      IMPLICIT NONE
      INTEGER ,INTENT(IN) :: m
      REAL,INTENT(IN):: a
      REAL,DIMENSION(m),INTENT(OUT) :: w
      REAL   ,DIMENSION(m,m)        :: ut
      REAL   ,DIMENSION(m)          :: w_d,g,h
      INTEGER mm,j,i
      REAL    hg,a_d,t

      if (m .eq.0) then
          return
      endif

      mm=m-1

      a_d=a ! Double-precision version of input, a
      g=0.
      h=0.
      ut=0.
      w_d=0.
      h(m)=a_d
      g(m)=a_d

      DO j=m-1,1,-1
         g(j)=g(j+1)*a_d
      ENDDO

      ut(1,1)=1.

      DO j=2,m
         DO i=1,mm
            h(i)=h(i)-a_d*h(i+1)
         ENDDO
         h(m)=h(m)+a_d
         ut(j,j-1)=-a_d
         ut(j,j)  =1.
      ENDDO

      hg=DOT_PRODUCT(h,g)

      t=(1.-a_d)*a_d/(1.-hg)

      DO j=1,m
         w_d(j)=h(j)*t
      ENDDO

      CALL dlinmm(ut,w_d,m,1,m,m)

      w_d(m)=w_d(m)-a_d
      w(m:1:-1)=w_d

      RETURN
      END SUBROUTINE DA_RF_Turning_Conditions

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!                   ENTRY DLINMM
!  invert linear systems sharing the same square system matrix
!
!  <-> A    square system matrix on entry, its L-D-U factorization on return
!  <-> B    right-hand-sides on entry, corresponding matrix of solution
!           vectors on return
!  --> M    degree of (active part of) B and A
!  --> MM   number of right-hand-side vectors (active columns of b)
!  --> NA   first fortran dimension of A
!  --> NB   first fortran dimension of B
!------------------------------------------------------------------------------
      SUBROUTINE     DLINMM(a,b,m,mm,na,nb)

      INTEGER            :: m, mm, na, nb
      REAL               :: A(NA,*),B(NB,*)

      INTEGER, PARAMETER :: NN=500
      INTEGER            :: IPIV(NN)
      REAL               :: d

      CALL DLDUM(A,IPIV,D,M,NA)
      CALL DUDLMM(A,B,IPIV,M,MM,NA,NB)
      RETURN
      END SUBROUTINE DLINMM

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!   R.J.Purser, NCEP, Washington D.C.      1996
!      	    SUBROUTINE DLDUM
!  perform l-d-u decomposition of square matrix a in place with
!  IMPLICIT pivoting
!
!  --> a    square matrix to be factorized
!  <-- ipiv array encoding the pivoting sequence
!  <-- d    indicator for possible sign change of determinant
!  --> m    degree of (active part of) a
!  --> na   first fortran dimension of a
!
!   LIMITATION:
!      S is an array, internal to this routine, containing the
!    scaling factors of each row used for pivoting decisions. It is given a
!    fortran dimension of NN=500 in the parameter statement below.
!    If the order of the linear system exceeds NN, increase NN.
!------------------------------------------------------------------------------
      SUBROUTINE DLDUM(A,IPIV,D,M,NA)

      INTEGER            :: m, na
      REAL               :: A(NA,*)
      INTEGER            :: IPIV(*)
      REAL               :: d
      INTEGER            :: i, j, k, jp, ibig, jm
      REAL               :: aam, aa, abig, t, ajj, ajji, aij


      INTEGER, PARAMETER :: NN=500
      REAL               :: S(NN)

      IF(M.GT.NN) STOP 'MATRIX TOO LARGE FOR LDUM'

      DO I=1,M
       AAM=0.
       DO J=1,M
      AA=ABS(A(I,J))
      IF(AA.GT.AAM)AAM=AA
       ENDDO
       IF(AAM.EQ.0.)THEN
      PRINT'('' ROW '',I3,'' OF MATRIX IN LUFM VANISHES'')',I
      STOP
       ENDIF
       S(I)=1./AAM
      ENDDO
      D=1.
      IPIV(M)=M
      DO J=1,M-1
       JP=J+1
       ABIG=S(J)*ABS(A(J,J))
       IBIG=J
       DO I=JP,M
      AA=S(I)*ABS(A(I,J))
      IF(AA.GT.ABIG)THEN
       IBIG=I
       ABIG=AA
      ENDIF
       ENDDO
!  swap rows, recording changed sign of determinant
       IPIV(J)=IBIG
       IF(IBIG.NE.J)THEN
      D=-D
      DO K=1,M
       T=A(J,K)
       A(J,K)=A(IBIG,K)
       A(IBIG,K)=T
      ENDDO
      S(IBIG)=S(J)
       ENDIF
       AJJ=A(J,J)
       IF(AJJ.EQ.0.)THEN
      JM=J-1
      PRINT'('' FAILURE IN LDUM:''/'' MATRIX SINGULAR, RANK='',i3)',JM
      STOP
       ENDIF
       AJJI=1./AJJ
       DO I=JP,M
      AIJ=AJJI*A(I,J)
      A(I,J)=AIJ
      DO K=JP,M
       A(I,K)=A(I,K)-AIJ*A(J,K)
      ENDDO
       ENDDO
      ENDDO
      RETURN
      END SUBROUTINE DLDUM

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!      	    SUBROUTINE DUDLMM
!  use l-u factors in a to back-substitute for mm rhs in b, using ipiv to
!  define the pivoting permutation used in the l-u decomposition.
!
!  --> A    L-D-U factorization of linear system matrux
!  <-> B    right-hand-sides on entry, corresponding matrix of solution
!          vectors on return
!  --> IPIV array encoding the pivoting sequence
!  --> M    degree of (active part of) B and A
!  --> MM   number of right-hand-side vectors (active columns of B)
!  --> NA   first fortran dimension of A
!  --> NB   first fortran dimension of B
!------------------------------------------------------------------------------
      SUBROUTINE DUDLMM(A,B,IPIV,M,MM,NA,NB)

      INTEGER     :: m, mm, na, nb
      INTEGER     :: ipiv(*)
      REAL        :: a(na,*)
      REAL        :: b(na,*)

      INTEGER     :: i, k, l
      REAL        :: s, aiii

      DO K=1,MM !loop over columns of B
       DO I=1,M
      L=IPIV(I)
      S=B(L,K)
      B(L,K)=B(I,K)
      CALL DDSBVR(B(1,K),A(I,1),S,I-1,NA)
      B(I,K)=S
       ENDDO
       B(M,K)=B(M,K)/A(M,M)
       DO I=M-1,1,-1
      AIII=1./A(I,I)
      CALL DDSBVR(B(I+1,K),A(I,I+1),B(I,K),M-I,NA)
      B(I,K)=B(I,K)*AIII
       ENDDO
      ENDDO
      RETURN
      END SUBROUTINE DUDLMM

      SUBROUTINE DDSBVR(D,A,S,M,NA)

      INTEGER :: m, na, i
      REAL    :: D(M), A(na,*)
      REAL    :: s

      DO I=1,M
       S=S-D(I)*A(1,I)
      ENDDO
      RETURN
      END SUBROUTINE DDSBVR

