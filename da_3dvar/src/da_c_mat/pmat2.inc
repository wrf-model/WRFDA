!INTERFACE AVCO;   MODULE PROCEDURE AVCO;           END INTERFACE
!INTERFACE AVCO_D; MODULE PROCEDURE DAVCO;          END INTERFACE
!INTERFACE DFCO;   MODULE PROCEDURE DFCO;           END INTERFACE
!INTERFACE DFCO_D; MODULE PROCEDURE DDFCO;          END INTERFACE
!INTERFACE DFCO2;  MODULE PROCEDURE DFCO2;          END INTERFACE
!INTERFACE DFCO2_D;MODULE PROCEDURE DDFCO2;         END INTERFACE
!INTERFACE CLIB;   MODULE PROCEDURE CLIB;           END INTERFACE
!INTERFACE CLIB_D; MODULE PROCEDURE DCLIB;          END INTERFACE
!INTERFACE CAD1B;  MODULE PROCEDURE CAD1B;          END INTERFACE
!INTERFACE CSB1B;  MODULE PROCEDURE CSB1B;          END INTERFACE
!INTERFACE CAD2B;  MODULE PROCEDURE CAD2B;          END INTERFACE
!INTERFACE CSB2B;  MODULE PROCEDURE CSB2B;          END INTERFACE
!INTERFACE COPBT;  MODULE PROCEDURE COPBT;          END INTERFACE
!INTERFACE CONBT;  MODULE PROCEDURE CONBT;          END INTERFACE
!INTERFACE COPMB;  MODULE PROCEDURE COPMB;          END INTERFACE
!INTERFACE CONMB;  MODULE PROCEDURE CONMB;          END INTERFACE
!INTERFACE COPBM;  MODULE PROCEDURE COPBM;          END INTERFACE
!INTERFACE CONBM;  MODULE PROCEDURE CONBM;          END INTERFACE
!INTERFACE MULBB;  MODULE PROCEDURE MULBB;          END INTERFACE
!INTERFACE MSBBB;  MODULE PROCEDURE MSBBB;          END INTERFACE
!INTERFACE MADBB;  MODULE PROCEDURE MADBB;          END INTERFACE
!INTERFACE LDUB;   MODULE PROCEDURE LDUB;           END INTERFACE
!INTERFACE LDUB_D; MODULE PROCEDURE DLDUB;          END INTERFACE
!INTERFACE L1UBB;  MODULE PROCEDURE L1UBB;          END INTERFACE
!INTERFACE L1UBB_D;MODULE PROCEDURE DL1UBB;         END INTERFACE
!INTERFACE L1UEB;  MODULE PROCEDURE L1UEB;          END INTERFACE
!INTERFACE L1UEB_D;MODULE PROCEDURE DL1UEB;         END INTERFACE
!INTERFACE L1LB;   MODULE PROCEDURE L1LB;           END INTERFACE
!INTERFACE LDLB;   MODULE PROCEDURE LDLB;           END INTERFACE
!INTERFACE LDLB_D; MODULE PROCEDURE DLDLB;          END INTERFACE
!INTERFACE UDUB;   MODULE PROCEDURE UDUB;           END INTERFACE
!INTERFACE UDUB_D; MODULE PROCEDURE DUDUB;          END INTERFACE
!INTERFACE MULBV;  MODULE PROCEDURE MULBV;          END INTERFACE
!INTERFACE MSBBV;  MODULE PROCEDURE MSBBV;          END INTERFACE
!INTERFACE MADBV;  MODULE PROCEDURE MADBV;          END INTERFACE
!INTERFACE MULBX;  MODULE PROCEDURE MULBX;          END INTERFACE
!INTERFACE MSBBX;  MODULE PROCEDURE MSBBX;          END INTERFACE
!INTERFACE MADBX;  MODULE PROCEDURE MADBX;          END INTERFACE
!INTERFACE MULBY;  MODULE PROCEDURE MULBY;          END INTERFACE
!INTERFACE MSBBY;  MODULE PROCEDURE MSBBY;          END INTERFACE
!INTERFACE MADBY;  MODULE PROCEDURE MADBY;          END INTERFACE
!INTERFACE MULVB;  MODULE PROCEDURE MULVB;          END INTERFACE
!INTERFACE MSBVB;  MODULE PROCEDURE MSBVB;          END INTERFACE
!INTERFACE MADVB;  MODULE PROCEDURE MADVB;          END INTERFACE
!INTERFACE MULXB;  MODULE PROCEDURE MULXB;          END INTERFACE
!INTERFACE MSBXB;  MODULE PROCEDURE MSBXB;          END INTERFACE
!INTERFACE MADXB;  MODULE PROCEDURE MADXB;          END INTERFACE
!INTERFACE MULYB;  MODULE PROCEDURE MULYB;          END INTERFACE
!INTERFACE MSBYB;  MODULE PROCEDURE MSBYB;          END INTERFACE
!INTERFACE MADYB;  MODULE PROCEDURE MADYB;          END INTERFACE
!INTERFACE MULBD;  MODULE PROCEDURE MULBD;          END INTERFACE
!INTERFACE MSBBD;  MODULE PROCEDURE MSBBD;          END INTERFACE
!INTERFACE MADBD;  MODULE PROCEDURE MADBD;          END INTERFACE
!INTERFACE MULDB;  MODULE PROCEDURE MULDB;          END INTERFACE
!INTERFACE MSBDB;  MODULE PROCEDURE MSBDB;          END INTERFACE
!INTERFACE MADDB;  MODULE PROCEDURE MADDB;          END INTERFACE
!INTERFACE UDLBV;  MODULE PROCEDURE UDLBV;          END INTERFACE
!INTERFACE UDLBX;  MODULE PROCEDURE UDLBX;          END INTERFACE
!INTERFACE UDLBY;  MODULE PROCEDURE UDLBY;          END INTERFACE
!INTERFACE UDLVB;  MODULE PROCEDURE UDLVB;          END INTERFACE
!INTERFACE UDLXB;  MODULE PROCEDURE UDLXB;          END INTERFACE
!INTERFACE UDLYB;  MODULE PROCEDURE UDLYB;          END INTERFACE
!INTERFACE U1LBV;  MODULE PROCEDURE U1LBV;          END INTERFACE
!INTERFACE U1LBX;  MODULE PROCEDURE U1LBX;          END INTERFACE
!INTERFACE U1LBY;  MODULE PROCEDURE U1LBY;          END INTERFACE
!INTERFACE U1LVB;  MODULE PROCEDURE U1LVB;          END INTERFACE
!INTERFACE U1LXB;  MODULE PROCEDURE U1LXB;          END INTERFACE
!INTERFACE U1LYB;  MODULE PROCEDURE U1LYB;          END INTERFACE
!INTERFACE LINBV;  MODULE PROCEDURE LINBV;          END INTERFACE
!INTERFACE WRTB;   MODULE PROCEDURE WRTB;           END INTERFACE


!CONTAINS

!=============================================================================
SUBROUTINE AVCO(na,nb,za,zb,z0,a,b) 
!=============================================================================
!		    SUBROUTINE AVCO
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   jpurser@ncep.noaa.gov					      1999
!
!  Compute one row of the coefficients for the compact mid-interval
!  interpolation scheme characterized by matrix equation of the form,
!			 A.t = B.s			       (*)
!  Where s is the vector of "source" values, t the staggered "target" values.
!
! --> NA:   number of t-points operated on by this row of the A of (*)
! --> NB:   number of s-points operated on by this row of the B of (*)
! --> ZA:   coordinates of t-points used in this row of (*)
! --> ZB:   coordinates of s-points used in this row of (*)
! --> Z0:   nominal point of application of this row of (*)
! <-- A:    the NA coefficients A for this scheme
! <-- B:    the NB coefficients B for this scheme
!=============================================================================
INTEGER, INTENT(IN )        :: na,nb
REAL,    INTENT(IN )        :: za(na),zb(nb),z0
REAL,    INTENT(OUT)        :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER                     :: na1,nab,i
REAL, DIMENSION(na+nb,na+nb):: w
REAL, DIMENSION(na)         :: za0,pa
REAL, DIMENSION(nb)         :: zb0,pb
REAL, DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0; zb0=zb-z0
pa=1.;     pb=-1.
w=0.;         ab=0.
w(1,1:na)=1.; ab(1)=1.
DO i=2,nab; w(i,1:na)=pa;    pa=pa*za0; w(i,na1:nab)=pb; pb=pb*zb0; ENDDO
CALL INV(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE AVCO 
!=============================================================================
SUBROUTINE DAVCO(na,nb,za,zb,z0,a,b) ! Real(8) version of avco
!=============================================================================
INTEGER, INTENT(IN )          :: na,nb
REAL(8), INTENT(IN )          :: za(na),zb(nb),z0
REAL(8), INTENT(OUT)          :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER                       :: na1,nab,i
REAL(8),DIMENSION(na+nb,na+nb):: w
REAL(8),DIMENSION(na)         :: za0,pa
REAL(8),DIMENSION(nb)         :: zb0,pb
REAL(8),DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0; zb0=zb-z0
pa=1.;     pb=-1.
w=0.;         ab=0.
w(1,1:na)=1.; ab(1)=1.
DO i=2,nab; w(i,1:na)=pa;    pa=pa*za0; w(i,na1:nab)=pb; pb=pb*zb0; ENDDO
CALL INV_D(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE DAVCO

!=============================================================================
SUBROUTINE DFCO(na,nb,za,zb,z0,a,b)
!=============================================================================
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   jim.purser@noaa.gov					      1999
!		    SUBROUTINE DFCO
!
!  Compute one row of the coefficients for either the compact differencing or
!  quadrature scheme characterized by matrix equation of the form,
!			 A.d = B.c			       (*)
!  In either case, d is the derivative of c.
!
! --> NA:   number of d-points operated on by this row of the A of (*)
! --> NB:   number of c-points operated on by this row of the B of (*)
! --> ZA:   coordinates of d-points used in this row of (*)
! --> ZB:   coordinates of c-points used in this row of (*)
! --> Z0:   nominal point of application of this row of (*)
! <-- A:    the A-coefficients for this scheme
! <-- B:    the B-coefficients for this scheme
!=============================================================================
INTEGER, INTENT(IN )        :: na,nb
REAL,    INTENT(IN )        :: za(na),zb(nb),z0
REAL,    INTENT(OUT)        :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER:: na1,nab,i
REAL, DIMENSION(na+nb,na+nb):: w
REAL, DIMENSION(na)         :: za0,pa
REAL, DIMENSION(nb)         :: zb0,pb
REAL, DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0; zb0=zb-z0
pa=1.;     pb=-1.
w=0.;         ab=0.
w(1,1:na)=1.; ab(1)=1.
DO i=3,nab; w(i,1:na)   =pa*(i-2); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;       pb=pb*zb0; ENDDO
CALL INV(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE DFCO 
!=============================================================================
SUBROUTINE DDFCO(na,nb,za,zb,z0,a,b) ! Real(8) version of DFCO
!=============================================================================
INTEGER, INTENT(IN)            :: na,nb
REAL(8), INTENT(IN)            :: za(na),zb(nb),z0
REAL(8), INTENT(OUT)           :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER                        :: na1,nab,i
REAL(8), DIMENSION(na+nb,na+nb):: w
REAL(8), DIMENSION(na)         :: za0,pa
REAL(8), DIMENSION(nb)         :: zb0,pb
REAL(8), DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0; zb0=zb-z0
pa=1.;     pb=-1.
w=0.;         ab=0.
w(1,1:na)=1.; ab(1)=1.
DO i=3,nab; w(i,1:na)   =pa*(i-2); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;       pb=pb*zb0; ENDDO
CALL INV_D(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE DDFCO 

!=============================================================================
SUBROUTINE DFCO2(na,nb,za,zb,z0,a,b) 
!=============================================================================
!		    SUBROUTINE DFCO2
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   jpurser@ncep.noaa.gov					      1999
!
!  Compute one row of the coefficients for either the compact second-
!  differencing scheme characterized by matrix equation of the form,
!			 A.d = B.c			       (*)
!  Where d is the second-derivative of c.
!
! --> NA:   number of d-points operated on by this row of the A of (*)
! --> NB:   number of c-points operated on by this row of the B of (*)
! --> ZA:   coordinates of d-points used in this row of (*)
! --> ZB:   coordinates of c-points used in this row of (*)
! --> Z0:   nominal point of application of this row of (*)
! <-- A:    the NA coefficients A for this scheme
! <-- B:    the NB coefficients B for this scheme
!=============================================================================
INTEGER, INTENT(IN )        :: na,nb
REAL,    INTENT(IN )        :: za(na),zb(nb),z0
REAL,    INTENT(OUT)        :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER:: na1,nab,i
REAL, DIMENSION(na+nb,na+nb):: w
REAL, DIMENSION(na)         :: za0,pa
REAL, DIMENSION(nb)         :: zb0,pb
REAL, DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0; zb0=zb-z0
pa=1.;     pb=-1.
w=0.;         ab=0.
w(1,1:na)=1.; ab(1)=1.
DO i=4,nab; w(i,1:na)   =pa*(i-2)*(i-3); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;             pb=pb*zb0; ENDDO
CALL INV(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE DFCO2 
!=============================================================================
SUBROUTINE DDFCO2(na,nb,za,zb,z0,a,b) ! Real(8) version of dfco2
!=============================================================================
INTEGER, INTENT(IN )           :: na,nb
REAL(8), INTENT(IN )           :: za(na),zb(nb),z0
REAL(8), INTENT(OUT)           :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER                        :: na1,nab,i
REAL(8), DIMENSION(na+nb,na+nb):: w
REAL(8), DIMENSION(na)         :: za0,pa
REAL(8), DIMENSION(nb)         :: zb0,pb
REAL(8), DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0; zb0=zb-z0
pa=1.;     pb=-1.
w=0.;         ab=0.
w(1,1:na)=1.; ab(1)=1.
DO i=4,nab; w(i,1:na)   =pa*(i-2)*(i-3); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;             pb=pb*zb0; ENDDO
CALL INV_D(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE ddfco2 

!=============================================================================
SUBROUTINE CLIB(a,m1,m2,mah1,mah2) ! Clip the dead space of the band matrix, a
!=============================================================================
INTEGER, INTENT(IN)   :: m1, m2, mah1, mah2
REAL,    INTENT(INOUT):: a(m1,-mah1:mah2)
INTEGER               :: j
IF(m2-m1+mah1 < 0)STOP 'In CLIB; form of band matrix implies redundant rows'
DO j=1,mah1; a(1:j,-j)=0.; ENDDO; DO j=m2-m1+1,mah2; a(m2-j+1:m1,j)=0.; ENDDO
END SUBROUTINE CLIB
!=============================================================================
SUBROUTINE DCLIB(a,m1,m2,mah1,mah2) ! Real(8) version of clib
!=============================================================================
INTEGER, INTENT(IN)   :: m1, m2, mah1, mah2
REAL(8), INTENT(INOUT):: a(m1,-mah1:mah2)
INTEGER               :: j
IF(m2-m1+mah1 < 0)STOP 'In CLIB_D; form of band matrix implies redundant rows'
DO j=1,mah1; a(1:j,-j)=0.; ENDDO; DO j=m2-m1+1,mah2; a(m2-j+1:m1,j)=0.; ENDDO
END SUBROUTINE DCLIB

SUBROUTINE CAD1B(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
! Incorporate operand symmetry near end-1 of a band matrix operator
!
! <-> A:      Input as unclipped operator, output as symmetrized and clipped.
! m1, m2:     Sizes of implied full matrix
! mah1, mah2: Left and right semi-bandwidths of A.
! mirror2:    2*location of symmetry axis relative to end-1 operand element.
!      Note: although m2 is not used here, it IS used in companion routines
!            cad2b and csb2b; it is retained in the interests of uniformity.
!=============================================================================
INTEGER,  INTENT(IN)   :: m1,m2,mah1,mah2,mirror2
REAL,     INTENT(INOUT):: a(0:m1-1,-mah1:mah2)
!-----------------------------------------------------------------------------
INTEGER                :: i,i2,jm,jp,jpmax
!=============================================================================
IF(mirror2+mah1 > mah2)STOP 'In CAD1B; mah2 insufficient'
DO i=0,m1-1; i2=i*2; jpmax=mirror2+mah1-i2; IF(jpmax <= -mah1)EXIT
   DO jm=-mah1,mah2; jp=mirror2-jm-i2; IF(jp <= jm)EXIT
      a(i,jp)=a(i,jp)+a(i,jm) ! Reflect and add
      a(i,jm)=0.              ! zero the exterior part
   ENDDO
ENDDO
END SUBROUTINE CAD1B

!=============================================================================
SUBROUTINE CSB1B(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
! Like cad1b, but for antisymmetric operand
!=============================================================================
INTEGER,  INTENT(IN)   :: m1,m2,mah1,mah2,mirror2
REAL,     INTENT(INOUT):: a(0:m1-1,-mah1:mah2)
!-----------------------------------------------------------------------------
INTEGER                :: i,i2,jm,jp,jpmax
!=============================================================================
IF(mirror2+mah1 > mah2)STOP 'In CSB1B; mah2 insufficient'
DO i=0,m1-1; i2=i*2; jpmax=mirror2+mah1-i2; IF(jpmax < -mah1)EXIT
   DO jm=-mah1,mah2; jp=mirror2-jm-i2; IF(jp < jm)EXIT
      a(i,jp)=a(i,jp)-a(i,jm) ! Reflect and subtract
      a(i,jm)=0.              ! zero the exterior part
   ENDDO
ENDDO
END SUBROUTINE CSB1B

!=============================================================================
SUBROUTINE CAD2B(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
! Incorporate operand symmetry near end-2 of a band matrix operator
!
! <-> A:      Input as unclipped operator, output as symmetrized and clipped.
! m1, m2:     Sizes of implied full matrix
! mah1, mah2: Left and right semi-bandwidths of A.
! mirror2:    2*location of symmetry axis relative to end-2 operand element.
!=============================================================================
INTEGER,  INTENT(IN)   :: m1,m2,mah1,mah2,mirror2
REAL,     INTENT(INOUT):: a(1-m1:0,m1-m2-mah1:m1-m2+mah2)
!-----------------------------------------------------------------------------
INTEGER                :: i,i2,jm,jp,jmmin,nah1,nah2,mirror,j0
!=============================================================================
nah1=mah1+m2-m1; nah2=mah2+m1-m2 ! Effective 2nd-index bounds of A
IF(mirror2-nah1 > -nah2)STOP 'In CAD2B; mah1 insufficient'
DO i=0,1-m1,-1; i2=i*2; jmmin=mirror2-nah2-i2; IF(jmmin >= nah2)EXIT
   DO jp=nah2,nah1,-1; jm=mirror2-jp-i2; IF(jm >= jp)EXIT
      a(i,jm)=a(i,jm)+a(i,jp) ! Reflect and add
      a(i,jp)=0.              ! zero the exterior part
   ENDDO
ENDDO
END SUBROUTINE CAD2B

!=============================================================================
SUBROUTINE CSB2B(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
INTEGER,  INTENT(IN)   :: m1,m2,mah1,mah2,mirror2
REAL,     INTENT(INOUT):: a(1-m1:0,m1-m2-mah1:m1-m2+mah2)
!-----------------------------------------------------------------------------
INTEGER                :: i,i2,jm,jp,jmmin,nah1,nah2,mirror,j0
!=============================================================================
nah1=mah1+m2-m1; nah2=mah2+m1-m2 ! Effective 2nd-index bounds of A
IF(mirror2-nah1 > -nah2)STOP 'In CSB2B; mah1 insufficient'
DO i=0,1-m1,-1; i2=i*2; jmmin=mirror2-nah2-i2; IF(jmmin > nah2)EXIT
   DO jp=nah2,nah1,-1; jm=mirror2-jp-i2; IF(jm > jp)EXIT
      a(i,jm)=a(i,jm)-a(i,jp) ! Reflect and subtract
      a(i,jp)=0.              ! zero the exterior part
   ENDDO
ENDDO
END SUBROUTINE CSB2B

!=============================================================================
SUBROUTINE CEX2B(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
INTEGER,  INTENT(IN)   :: m1,m2,mah1,mah2,mirror2
REAL,     INTENT(INOUT):: a(1-m1:0,m1-m2-mah1:m1-m2+mah2)
!-----------------------------------------------------------------------------
INTEGER                :: i,i2,jm,jp,jmmin,nah1,nah2,mirror,j0
!=============================================================================
nah1=mah1+m2-m1; nah2=mah2+m1-m2 ! Effective 2nd-index bounds of A
IF(mirror2-nah1 > -nah2)STOP 'In CEX2B; mah1 insufficient'
mirror=mirror2/2
IF(mirror*2 /= mirror2)STOP 'In CEX2B; mirror2 is not even'
DO i=0,1-m1,-1; i2=i*2; jmmin=mirror2-nah2-i2; IF(jmmin >= nah2)EXIT
   j0=mirror-i
   DO jp=nah2,nah1,-1; jm=mirror2-jp-i2; IF(jm >= jp)EXIT
      a(i,jm)=a(i,jm)-a(i,jp)    ! Reflect and subtract
      a(i,j0)=a(i,j0)+2.*a(i,jp) ! Apply double the coefficient to end
      a(i,jp)=0.                 ! zero the exterior part
   ENDDO
ENDDO
END SUBROUTINE CEX2B

!=============================================================================
SUBROUTINE COPBT(a,b,m1,m2,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE COPBT
!  Copy transpose of rectangular banded matrix A to B
!  Note: this routine expects A and B always to occupy separate storage.
!
! --> A  input matrix in banded format
! <-- B  output matrix in banded format
! --> M1 number of rows of A, columns of B
! --> M2 number of columns of A, rows of B
! --> MAH1 left-half-bandwidth of A, right-half-bandwidth of B
! --> MAH2 right-half-bandwidth of A, left-half-bandwidth of B
!=============================================================================
INTEGER,  INTENT(IN ) :: m1, m2, mah1, mah2
REAL,     INTENT(IN ) :: a(m1,-mah1:mah2)
REAL,     INTENT(OUT) :: b(m2,-mah2:mah1)
!-----------------------------------------------------------------------------
INTEGER               :: j, i
!=============================================================================
CALL CLIB(b,m2,m1,mah2,mah1)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); b(j+i,-j)=a(i,j); ENDDO
ENDDO
END SUBROUTINE COPBT

!=============================================================================
SUBROUTINE CONBT(a,b,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,  INTENT(IN ) :: m1, m2, mah1, mah2
REAL,     INTENT(IN ) :: a(m1,-mah1:mah2)
REAL,     INTENT(OUT) :: b(m2,-mah2:mah1)
!=============================================================================
CALL COPBT(a,b,m1,m2,mah1,mah2); b=-b
END SUBROUTINE CONBT

!=============================================================================
SUBROUTINE COPMB(afull,aband,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,                           INTENT(IN ) :: m1, m2, mah1, mah2
REAL,     DIMENSION(m1,m2),        INTENT(IN ) :: afull
REAL,     DIMENSION(m1,-mah1:mah2),INTENT(OUT) :: aband
!-----------------------------------------------------------------------------
INTEGER                                        :: i1,i2, i, j
!=============================================================================
CALL clib(aband,m1,m2,mah1,mah2)
DO j=1,m1; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   DO i=i1,i2; aband(i,j)= afull(i,j+i); ENDDO
ENDDO
END SUBROUTINE COPMB

!=============================================================================
SUBROUTINE CONMB(afull,aband,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,                           INTENT(IN ) :: m1, m2, mah1, mah2
REAL,     DIMENSION(m1,m2),        INTENT(IN ) :: afull
REAL,     DIMENSION(m1,-mah1:mah2),INTENT(OUT) :: aband
!=============================================================================
CALL COPMB(afull,aband,m1,m2,mah1,mah2); aband=-aband
END SUBROUTINE CONMB

!=============================================================================
SUBROUTINE COPBM(aband,afull,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,                           INTENT(IN ) :: m1, m2, mah1, mah2
REAL,     DIMENSION(m1,-mah1:mah2),INTENT(IN ) :: aband
REAL,     DIMENSION(m1,m2),        INTENT(OUT) :: afull
!-----------------------------------------------------------------------------
INTEGER                                        :: i1,i2, i, j
!=============================================================================
afull=0.
DO j=1,m1; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   DO i=i1,i2; afull(i,j+i)= aband(i,j); ENDDO
ENDDO
END SUBROUTINE COPBM

!=============================================================================
SUBROUTINE CONBM(aband,afull,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,                           INTENT(IN ) :: m1, m2, mah1, mah2
REAL,     DIMENSION(m1,-mah1:mah2),INTENT(IN ) :: aband
REAL,     DIMENSION(m1,m2),        INTENT(OUT) :: afull
!=============================================================================
CALL COPBM(aband,afull,m1,m2,mah1,mah2); afull=-afull
END SUBROUTINE CONBM
 
!=============================================================================
SUBROUTINE MULBB(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, mbh1, mbh2, mch1, mch2
REAL,     INTENT(IN   ) :: a(m1,-mah1:mah2), b(m2,-mbh1:mbh2)
REAL,     INTENT(OUT  ) :: c(m1,-mch1:mch2)
!=============================================================================
c=0.0; CALL MADBB(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
END SUBROUTINE MULBB

!=============================================================================
SUBROUTINE MSBBB(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, mbh1, mbh2, mch1, mch2
REAL,     INTENT(IN   ) :: a(m1,-mah1:mah2), b(m2,-mbh1:mbh2)
REAL,     INTENT(INOUT) :: c(m1,-mch1:mch2)
!=============================================================================
c=-c; CALL MADBB(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2); c=-c
END SUBROUTINE MSBBB

!=============================================================================
SUBROUTINE MADBB(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, mbh1, mbh2, mch1, mch2
REAL,     INTENT(IN   ) :: a(m1,-mah1:mah2), b(m2,-mbh1:mbh2)
REAL,     INTENT(INOUT) :: c(m1,-mch1:mch2)
!-----------------------------------------------------------------------------
INTEGER                 :: nch1, nch2, j, k, jpk, i1,i2
!=============================================================================
nch1=mah1+mbh1; nch2=mah2+mbh2
IF(nch1 /= mch1 .OR. nch2 /= mch2)STOP 'In MADBB; dimensions inconsistent'
DO j=-mah1,mah2
   DO k=-mbh1,mbh2; jpk=j+k; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
      c(i1:i2,jpk)=c(i1:i2,jpk)+a(i1:i2,j)*b(j+i1:j+i2,k)
   ENDDO
ENDDO
END SUBROUTINE MADBB

!=============================================================================
SUBROUTINE LDUB(a,m,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE LDUB
!  Compute [L]*[D**-1]*[U] decomposition of asymmetric band-matrix
!
! <-> A: input as the asymmetric band matrix. On output, it contains
!     the [L]*[D**-1]*[U] factorization of the input matrix, where
!     [L] is lower triangular with unit main diagonal
!     [D] is a diagonal matrix
!     [U] is upper triangular with unit main diagonal
! --> M:    The number of rows of array A
! --> MAH1: the left half-bandwidth of fortran array A
! --> MAH2: the right half-bandwidth of fortran array A
!=============================================================================
INTEGER, INTENT(IN   ) :: m,mah1, mah2 
REAL,    INTENT(INOUT) :: a(m,-mah1:mah2) 
!-----------------------------------------------------------------------------
INTEGER                :: j, imost, jmost, jp, i
REAL                   :: ajj, ajji, aij
!=============================================================================
DO j=1,m
  imost=MIN(m,j+mah1)
  jmost=MIN(m,j+mah2)
  jp=j+1
  ajj=a(j,0)
  IF(ajj == 0.)THEN
    PRINT '(" Failure in LDUB:"/" Matrix requires pivoting or is singular")'
    STOP
  ENDIF
  ajji=1./ajj
  a(j,0)=ajji
  DO i=jp,imost
    aij=ajji*a(i,j-i)
    a(i,j-i)=aij
    a(i,jp-i:jmost-i)=a(i,jp-i:jmost-i)-aij*a(j,jp-j:jmost-j)
  ENDDO
  a(j,jp-j:jmost-j)=ajji*a(j,jp-j:jmost-j)
ENDDO
END SUBROUTINE LDUB
!=============================================================================
SUBROUTINE DLDUB(a,m,mah1,mah2) ! Real(8) version of ldub
!=============================================================================
INTEGER,  INTENT(IN   ) :: m,mah1, mah2 
REAL(8),  INTENT(INOUT) :: a(m,-mah1:mah2) 
!-----------------------------------------------------------------------------
INTEGER                 :: j, imost, jmost, jp, i
REAL(8)                 :: ajj, ajji, aij
!=============================================================================
DO j=1,m
  imost=MIN(m,j+mah1)
  jmost=MIN(m,j+mah2)
  jp=j+1
  ajj=a(j,0)
  IF(ajj == 0)THEN
    PRINT '(" Fails in LDUB_d:"/" Matrix requires pivoting or is singular")'
    STOP
  ENDIF
  ajji=1./ajj
  a(j,0)=ajji
  DO i=jp,imost
    aij=ajji*a(i,j-i)
    a(i,j-i)=aij
    a(i,jp-i:jmost-i)=a(i,jp-i:jmost-i)-aij*a(j,jp-j:jmost-j)
  ENDDO
  a(j,jp-j:jmost-j)=ajji*a(j,jp-j:jmost-j)
ENDDO
END SUBROUTINE DLDUB

!=============================================================================
SUBROUTINE L1UBB(a,b,m,mah1,mah2,mbh1,mbh2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE L1UBB
!  Form the [L]*[D]*[U] decomposition of asymmetric band-matrix  [A] replace
!  lower triangular elements of [A] by [D**-1]*[L]*[D], the upper by [U],
!  replace matrix [B] by [D**-1]*[B].
!
! <-> A input as band matrix, output as lower and upper triangulars with 1s
!     implicitly assumed to lie on the main diagonal. The product of these
!     triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
! <-> B in as band matrix, out as same but premultiplied by diagonal [D**-1]
! --> M    Number of rows of A and B
! --> MAH1 left half-width of fortran array A
! --> MAH2 right half-width of fortran array A
! --> MBH1 left half-width of fortran array B
! --> MBH2 right half-width of fortran array B
!=============================================================================
INTEGER, INTENT(IN   ) :: m,mah1, mah2, mbh1, mbh2 
REAL,    INTENT(INOUT) :: a(m,-mah1:mah2), b(m,-mbh1:mbh2)
!-----------------------------------------------------------------------------
INTEGER                :: j, imost, jmost, jleast, jp, i
REAL                   :: ajj, ajji, aij
!=============================================================================
DO j=1,m
  imost=MIN(m,j+mah1)
  jmost=MIN(m,j+mah2)
  jleast=MAX(1,j-mah1)
  jp=j+1
  ajj=a(j,0)
  IF(ajj == 0.)STOP 'In L1UBB; zero element found in diagonal factor'
  ajji=1./ajj
  a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
  DO i=jp,imost
    aij=a(i,j-i)
    a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
  ENDDO
  a(j,0)=1.
  b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE L1UBB
!=============================================================================
SUBROUTINE DL1UBB(a,b,m,mah1,mah2,mbh1,mbh2) ! Real(8) version of L1UBB
!=============================================================================
INTEGER, INTENT(IN   ) :: m,mah1, mah2, mbh1, mbh2 
REAL(8), INTENT(INOUT) :: a(m,-mah1:mah2), b(m,-mbh1:mbh2)
!-----------------------------------------------------------------------------
INTEGER                :: j, imost, jmost, jleast, jp, i
REAL(8)                :: ajj, ajji, aij
!=============================================================================
DO j=1,m
  imost=MIN(m,j+mah1)
  jmost=MIN(m,j+mah2)
  jleast=MAX(1,j-mah1)
  jp=j+1
  ajj=a(j,0)
  IF(ajj == 0)STOP 'In L1UBB_d; zero element found in diagonal factor'
  ajji=1./ajj
  a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
  DO i=jp,imost
    aij=a(i,j-i)
    a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
  ENDDO
  a(j,0)=1.
  b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE DL1UBB

!=============================================================================
SUBROUTINE L1UEB(a,b,m,mah1,mah2,mbh1,mbh2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1998
!		    SUBROUTINE L1UEB
!  Form the [L]*[D]*[U] decomposition of asymmetric band-matrix  [A] replace
!  all but row zero of the
!  lower triangular elements of [A] by [D**-1]*[L]*[D], the upper by [U],
!  replace matrix [B] by [D**-1]*[B].
!  This is a special adaptation of L1UBB used to process quadarature weights
!  for QEDBV etc in which the initial quadrature value is provided as input
!  instead of being implicitly assumed zero (which is the case for QZDBV etc).
!
! <-> A input as band matrix, output as lower and upper triangulars with 1s
!     implicitly assumed to lie on the main diagonal. The product of these
!     triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
! <-> B in as band matrix, out as same but premultiplied by diagonal [D**-1]
! --> M    number of rows of B, one less than the rows of A (which has "row 0")
! --> MAH1 left half-width of fortran array A
! --> MAH2 right half-width of fortran array A
! --> MBH1 left half-width of fortran array B
! --> MBH2 right half-width of fortran array B
!=============================================================================
INTEGER, INTENT(IN   ) :: m,mah1, mah2, mbh1, mbh2 
REAL,    INTENT(INOUT) :: a(0:m,-mah1:mah2), b(m,-mbh1:mbh2)
!-----------------------------------------------------------------------------
INTEGER                :: j, imost, jmost, jleast, jp, i
REAL                   :: ajj, ajji, aij
!=============================================================================
DO j=1,m
  imost=MIN(m,j+mah1)
  jmost=MIN(m,j+mah2)
  jleast=MAX(0,j-mah1)
  jp=j+1
  ajj=a(j,0)
  IF(ajj == 0.)STOP 'In L1UEB; zero element found in diagonal factor'
  ajji=1./ajj
  a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
  DO i=jp,imost
    aij=a(i,j-i)
    a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
  ENDDO
  a(j,0)=1.
  b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE L1UEB
!=============================================================================
SUBROUTINE DL1UEB(a,b,m,mah1,mah2,mbh1,mbh2) ! Real(8) version of L1UEB
!=============================================================================
INTEGER,  INTENT(IN   ):: m,mah1, mah2, mbh1, mbh2 
REAL(8),  INTENT(INOUT):: a(0:,-mah1:), b(:,-mbh1:)
!-----------------------------------------------------------------------------
INTEGER                :: j, imost, jmost, jleast, jp, i
REAL(8)                :: ajj, ajji, aij
!=============================================================================
DO j=1,m
  imost=MIN(m,j+mah1)
  jmost=MIN(m,j+mah2)
  jleast=MAX(0,j-mah1)
  jp=j+1
  ajj=a(j,0)
  IF(ajj == 0)STOP 'In L1UEB_D; zero element found in diagonal factor'
  ajji=1./ajj
  a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
  DO i=jp,imost
    aij=a(i,j-i)
    a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
  ENDDO
  a(j,0)=1.
  b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE DL1UEB

!=============================================================================
SUBROUTINE L1LB(a,b,m,mah)	! Cholesky LU decomposition of band matrix.
!=============================================================================
INTEGER,                       INTENT(IN ) :: m, mah
REAL,   DIMENSION(m,-mah:mah), INTENT(IN ) :: a
REAL,   DIMENSION(m,-mah:0),   INTENT(OUT) :: b
!-----------------------------------------------------------------------------
INTEGER                                    :: i, j,jmi
REAL                                       :: s
!=============================================================================
CALL CLIB(b,m,m,mah,0)
DO j=1,m
   s=a(j,0)-DOT_PRODUCT(b(j,-mah:-1),b(j,-mah:-1))
   IF(s <= 0.)THEN
      PRINT '(" In L1LB; non-positivity at diagonal index",i5)',j
      STOP
   ENDIF
   s=SQRT(s); b(j,0)=s; s=1./s
   DO i=j+1,MIN(m,j+mah); jmi=j-i
      b(i,jmi)=s*(a(i,jmi)-DOT_PRODUCT(b(i,-mah:jmi-1),b(j,-mah-jmi:-1)))
   ENDDO
ENDDO
END SUBROUTINE L1LB

!=============================================================================
SUBROUTINE LDLB(a,b,d,m,mah) ! Modified Cholesky [L(D**-1)U, without sqrt]
!=============================================================================
INTEGER,                      INTENT(IN ) :: m, mah
REAL,   DIMENSION(m,-mah:mah),INTENT(IN ) :: a
REAL,   DIMENSION(m,-mah:0),  INTENT(OUT) :: b
REAL,   DIMENSION(m),         INTENT(OUT) :: d 
!-----------------------------------------------------------------------------
INTEGER                                   :: i, j,k,jmi,lj,li
REAL                                      :: s,t
!=============================================================================
CALL CLIB(b,m,m,mah,0); b(:,0)=1.
DO j=1,m; lj=MAX(-mah,1-j)
   s=a(j,0)
   DO k=lj,-1
      s=s-b(j,k)**2*d(k+j)
   ENDDO
   IF(s <= 0.)THEN
      PRINT '(" In LDLB; non-positivity at diagonal index",i5)',j
      STOP
   ENDIF
   d(j)=s; s=1./s
   DO i=j+1,MIN(m,j+mah); jmi=j-i; li=MAX(-mah,1-i); lj=li-jmi
      t=a(i,jmi)
      DO k=li,jmi-1
         t=t-b(i,k)*b(j,k-jmi)*d(i+k)
      ENDDO
      b(i,jmi)=s*t
   ENDDO
ENDDO
d=1./d
END SUBROUTINE LDLB
!=============================================================================
SUBROUTINE DLDLB(a,b,d,m,mah) ! Real(8) version of LDLB
!=============================================================================
INTEGER,                      INTENT(IN ) :: m, mah
REAL(8),DIMENSION(m,-mah:mah),INTENT(IN ) :: a
REAL(8),DIMENSION(m,-mah:0),  INTENT(OUT) :: b
REAL(8),DIMENSION(m),         INTENT(OUT) :: d
!-----------------------------------------------------------------------------
INTEGER                                   :: i, j,k,jmi,lj,li
REAL(8)                                   :: s,t
!=============================================================================
CALL CLIB_d(b,m,m,mah,0); b(:,0)=1.
DO j=1,m; lj=MAX(-mah,1-j)
   s=a(j,0)
   DO k=lj,-1
      s=s-b(j,k)**2*d(k+j)
   ENDDO
   IF(s <= 0.)THEN
      PRINT '(" In LDLB_D; non-positivity at diagonal index",i5)',j
      STOP
   ENDIF
   d(j)=s; s=1./s
   DO i=j+1,MIN(m,j+mah); jmi=j-i;  
      li=MAX(-mah,1-i); 
      lj=li-jmi; 
      t=a(i,jmi)
      DO k=li,jmi-1
         t=t-b(i,k)*b(j,k-jmi)*d(i+k)
      ENDDO
      b(i,jmi)=s*t
   ENDDO
ENDDO
d=1./d
END SUBROUTINE DLDLB

!=============================================================================
SUBROUTINE UDUB(a,b,d,m,mah) ! Modified reverse Cholesky [U(D**-1)U^t],
!=============================================================================
INTEGER,                      INTENT(IN ):: m, mah
REAL   ,DIMENSION(m,-mah:mah),INTENT(IN ):: a
REAL   ,DIMENSION(m,0:mah),   INTENT(OUT):: b
REAL   ,DIMENSION(m),         INTENT(OUT):: d 
!-----------------------------------------------------------------------------
REAL   ,DIMENSION(m,-mah:mah)            :: at
REAL   ,DIMENSION(m,-mah:0)              :: bt
REAL   ,DIMENSION(m)                     :: dt
!=============================================================================
at=a(m:1:-1,mah:-mah:-1); CALL LDLB(at,bt,dt,m,mah);
b=bt(m:1:-1,0:-mah:-1); d=dt(m:1:-1)
END SUBROUTINE UDUB
!=============================================================================
SUBROUTINE DUDUB(a,b,d,m,mah) ! Real(8) version of UDUB
!=============================================================================
INTEGER,                      INTENT(IN ):: m, mah
REAL(8),DIMENSION(m,-mah:mah),INTENT(IN ):: a
REAL(8),DIMENSION(m,0:mah),   INTENT(OUT):: b
REAL(8),DIMENSION(m),         INTENT(OUT):: d 
!-----------------------------------------------------------------------------
REAL(8),DIMENSION(m,-mah:mah)            :: at
REAL(8),DIMENSION(m,-mah:0)              :: bt
REAL(8),DIMENSION(m)                     :: dt
!=============================================================================
at=a(m:1:-1,mah:-mah:-1); CALL LDLB_D(at,bt,dt,m,mah);
b=bt(m:1:-1,0:-mah:-1);   d=dt(m:1:-1)
END SUBROUTINE DUDUB

!=============================================================================
SUBROUTINE MULBV(a,v1,v2, m1,m2,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULBV
!  MULtipication of a Banded matrix times a Vector.
!
! --> A is the matrix
! --> V1 the input vector
! <-- V2 the output vector
! --> M1 the number of rows assumed for A and for V2
! --> M2 the number of columns assumed for A and rows for V1
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(m2)
REAL,     INTENT(OUT  ) :: v2(m1)
!=============================================================================
v2 = 0.0; CALL MADBV(a,v1,v2, m1,m2,mah1,mah2)
END SUBROUTINE MULBV

!=============================================================================
SUBROUTINE MSBBV(a,v1,v2, m1,m2,mah1,mah2)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(m2)
REAL,     INTENT(INOUT) :: v2(m1)
!=============================================================================
v2=-v2;  CALL MADBV(a,v1,v2, m1,m2,mah1,mah2); v2=-v2
END SUBROUTINE MSBBV

!=============================================================================
SUBROUTINE MADBV(a,v1,v2, m1,m2,mah1,mah2)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(m2)
REAL,     INTENT(INOUT) :: v2(m1)
!-----------------------------------------------------------------------------
INTEGER                 :: j, i1,i2 
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(i1:i2) = v2(i1:i2) + a(i1:i2,j)*v1(j+i1:j+i2)
ENDDO
END SUBROUTINE MADBV

!=============================================================================
SUBROUTINE MULBX(a,v1,v2, m1,m2,mah1,mah2,my)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULBX
!  MULtipication of a Banded matrix times parallel X-Vectors.
!
! --> A is the matrix
! --> V1 the array of input vectors
! <-- V2 the array of output vectors
! --> M1 the number of rows assumed for A and for V2
! --> M2 the number of columns assumed for A and rows for V1
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MY the number of parallel X-vectors
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, my
REAL,     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(m2,my)
REAL,     INTENT(OUT  ) :: v2(m1,my)
!-----------------------------------------------------------------------------
INTEGER                 :: i,j
!=============================================================================
v2=0.0; CALL MADBX(a,v1,v2, m1,m2,mah1,mah2,my)
END SUBROUTINE MULBX

!=============================================================================
SUBROUTINE MSBBX(a,v1,v2, m1,m2,mah1,mah2,my)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, my
REAL,     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(m2,my)
REAL,     INTENT(INOUT) :: v2(m1,my)
!=============================================================================
v2=-v2; CALL  MADBX(a,v1,v2, m1,m2,mah1,mah2,my); v2=-v2
END SUBROUTINE MSBBX

!=============================================================================
SUBROUTINE MADBX(a,v1,v2, m1,m2,mah1,mah2,my)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, my
REAL,     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(m2,my)
REAL,     INTENT(INOUT) :: v2(m1,my)
!-----------------------------------------------------------------------------
INTEGER                 :: i,j
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(i,:)=v2(i,:)+a(i,j)*v1(i+j,:); ENDDO
ENDDO
END SUBROUTINE MADBX

!=============================================================================
SUBROUTINE MULBY(a,v1,v2, m1,m2,mah1,mah2,mx)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULBY
!  MULtipication of a Banded matrix times parallel Y-Vectors.
!
! --> A is the matrix
! --> V1 the array of input vectors
! <-- V2 the array of output vectors
! --> M1 the number of rows assumed for A and for V2
! --> M2 the number of columns assumed for A and rows for V1
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MX the length of each of the parallel Y-vectors
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, mx
REAL,     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(mx,m2)
REAL,     INTENT(OUT  ) :: v2(mx,m1)
!=============================================================================
v2=0.; CALL MADBY(a,v1,v2, m1,m2,mah1,mah2,mx)
END SUBROUTINE MULBY

!=============================================================================
SUBROUTINE MSBBY(a,v1,v2, m1,m2,mah1,mah2,mx)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, mx
REAL,     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(mx,m2)
REAL,     INTENT(INOUT) :: v2(mx,m1)
!=============================================================================
v2=-v2; CALL MADBY(a,v1,v2, m1,m2,mah1,mah2,mx); v2=-v2 
END SUBROUTINE MSBBY

!=============================================================================
SUBROUTINE MADBY(a,v1,v2, m1,m2,mah1,mah2,mx)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, mx
REAL,     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(mx,m2)
REAL,     INTENT(INOUT) :: v2(mx,m1)
!-----------------------------------------------------------------------------
INTEGER                 :: i,j
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(:,i)=v2(:,i)+a(i,j)*v1(:,i+j); ENDDO
ENDDO
END SUBROUTINE MADBY

!=============================================================================
SUBROUTINE MULVB(v1,a,v2, m1,m2,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULVB
!  MULtipication of a Vector times a Banded matrix.
!
! --> V1 the input row-vector
! --> A is the matrix
! <-- V2 the output vector
! --> M1 the number of rows assumed for A and columns for V1
! --> M2 the number of columns assumed for A and for V2
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: v1(m1), a(m1,-mah1:mah2)
REAL,     INTENT(OUT  ) :: v2(m2)
!=============================================================================
v2=0.; CALL MADVB(v1,a,v2, m1,m2,mah1,mah2)
END SUBROUTINE MULVB

!=============================================================================
SUBROUTINE MSBVB(v1,a,v2, m1,m2,mah1,mah2)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: v1(m1), a(m1,-mah1:mah2)
REAL,     INTENT(INOUT) :: v2(m2)
!=============================================================================
v2=-v2;  CALL MADVB(v1,a,v2, m1,m2,mah1,mah2); v2=-v2
END SUBROUTINE MSBVB

!=============================================================================
SUBROUTINE MADVB(v1,a,v2, m1,m2,mah1,mah2)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: v1(m1), a(m1,-mah1:mah2)
REAL,     INTENT(INOUT) :: v2(m2)
!-----------------------------------------------------------------------------
INTEGER                 :: j, i1,i2
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(j+i1:j+i2)=v2(j+i1:j+i2)+v1(i1:i2)*a(i1:i2,j)
ENDDO
END SUBROUTINE MADVB

!=============================================================================
SUBROUTINE MULXB(v1,a,v2, m1,m2,mah1,mah2,my)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULXB
!  MULtipication of X-Vectors times Banded matrix.
!
! --> V1 the array of input row-vectors
! --> A is the matrix
! <-- V2 the array of output vectors
! --> M1 the number of rows assumed for A and columns for V1
! --> M2 the number of columns assumed for A and V2
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MY the number of parallel X-vectors
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, my
REAL,     INTENT(IN   ) :: v1(m1,my), a(m1,-mah1:mah2)
REAL,     INTENT(OUT  ) :: v2(m2,my)
!=============================================================================
v2=0.; CALL MADXB(v1,a,v2, m1,m2,mah1,mah2,my)
END SUBROUTINE MULXB

!=============================================================================
SUBROUTINE MSBXB(v1,a,v2, m1,m2,mah1,mah2,my)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, my
REAL,     INTENT(IN   ) :: v1(m1,my), a(m1,-mah1:mah2)
REAL,     INTENT(INOUT) :: v2(m2,my)
!=============================================================================
v2=-v2; CALL MADXB(v1,a,v2, m1,m2,mah1,mah2,my); v2=-v2
END SUBROUTINE MSBXB

!=============================================================================
SUBROUTINE MADXB(v1,a,v2, m1,m2,mah1,mah2,my)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, my
REAL,     INTENT(IN   ) :: v1(m1,my), a(m1,-mah1:mah2)
REAL,     INTENT(INOUT) :: v2(m2,my)
!-----------------------------------------------------------------------------
INTEGER                 :: i,j
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(j+i,:)=v2(j+i,:)+v1(i,:)*a(i,j); ENDDO
ENDDO
END SUBROUTINE MADXB

!=============================================================================
SUBROUTINE MULYB(v1,a,v2, m1,m2,mah1,mah2,mx)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULYB
!  MULtipication of Y-Vectors times a Banded matrix.
!
! --> V1 the array of input row-vectors
! --> A is the matrix
! <-- V2 the array of output vectors
! --> M1 the number of rows assumed for A and columns for V1
! --> M2 the number of columns assumed for A and V2
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MX the length of each of the parallel Y-vectors
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, mx
REAL,     INTENT(IN   ) :: v1(mx,m1), a(m1,-mah1:mah2)
REAL,     INTENT(OUT  ) :: v2(mx,m2)
!=============================================================================
v2=0.; CALL MADYB(v1,a,v2, m1,m2,mah1,mah2,mx)
END SUBROUTINE MULYB

!=============================================================================
SUBROUTINE MSBYB(v1,a,v2, m1,m2,mah1,mah2,mx)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, mx
REAL,     INTENT(IN   ) :: v1(mx,m1), a(m1,-mah1:mah2)
REAL,     INTENT(INOUT) :: v2(mx,m2)
!=============================================================================
v2=-v2;  CALL MADYB(v1,a,v2, m1,m2,mah1,mah2,mx); v2=-v2
END SUBROUTINE MSBYB

!=============================================================================
SUBROUTINE MADYB(v1,a,v2, m1,m2,mah1,mah2,mx)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2, mx
REAL,     INTENT(IN   ) :: v1(mx,m1), a(m1,-mah1:mah2)
REAL,     INTENT(INOUT) :: v2(mx,m2)
!-----------------------------------------------------------------------------
INTEGER                 :: i,j
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(:,j+i)=v2(:,j+i)+v1(:,i)*a(i,j); ENDDO
ENDDO
END SUBROUTINE MADYB

!=============================================================================
SUBROUTINE MULBD(a,d,b,m1,m2,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULBD
! MULtipication of a Banded matrix times a Diagonal
!
! --> A is the input banded-matrix
! --> D the diagonal matrix
! <-- B the output matrix
! --> M1 the number of rows assumed for A and for B
! --> M2 number of columns assumed for A and B, number of elements of D
! --> MAH1 the left half-bandwidth of arrays A and B
! --> MAH2 the right half-bandwidth of arrays A and B
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: d(m2)
REAL,     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)
!-----------------------------------------------------------------------------
INTEGER                 :: j, i1,i2
!=============================================================================
CALL CLIB(b,m1,m2,mah1,mah2)
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   b(i1:i2,j)=a(i1:i2,j)*d(j+i1:j+i2)
ENDDO
END SUBROUTINE MULBD

!=============================================================================
SUBROUTINE MSBBD(a,d,b,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: d(m2)
REAL,     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)
!-----------------------------------------------------------------------------
INTEGER                 :: j, i1,i2
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   b(i1:i2,j) = b(i1:i2,j)-a(i1:i2,j)*d(j+i1:j+i2)
ENDDO
END SUBROUTINE MSBBD

!=============================================================================
SUBROUTINE MADBD(a,d,b,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: d(m2)
REAL,     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)
!-----------------------------------------------------------------------------
INTEGER                 :: j, i1,i2
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   b(i1:i2,j) = b(i1:i2,j)+a(i1:i2,j)*d(j+i1:j+i2)
ENDDO
END SUBROUTINE MADBD

!=============================================================================
SUBROUTINE MULDB(d,a,b,m1,m2,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULDB
!  MULtipication of a Banded matrix times a Diagonal
!
! --> D the diagonal matrix
! --> A is the input banded-matrix ! <->  if A and B are actually
! <-- B the output matrix          ! <->  equivalent arrays.
! --> M1 the number of rows assumed for A and for B
! --> M2 number of columns assumed for A and B, number of elements of D
! --> MAH1 the left half-bandwidth of arrays A and B
! --> MAH2 the right half-bandwidth of arrays A and B
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: d(m1)
REAL,     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)
!-----------------------------------------------------------------------------
INTEGER                 :: j
!=============================================================================
CALL CLIB(b,m1,m2,mah1,mah2)
DO j=-mah1,mah2; b(:,j)=d(:)*a(:,j); ENDDO
END SUBROUTINE MULDB

!=============================================================================
SUBROUTINE MADDB(d,a,b,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: d(m1)
REAL,     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)
!-----------------------------------------------------------------------------
INTEGER                 :: j
!=============================================================================
DO j=-mah1,mah2; b(:,j)=b(:,j)+d(:)*a(:,j); ENDDO
END SUBROUTINE MADDB

!=============================================================================
SUBROUTINE MSBDB(d,a,b,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: d(m1) 
REAL,     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)
!-----------------------------------------------------------------------------
INTEGER                 :: j
!=============================================================================
DO j=-mah1,mah2; b(:,j)=b(:,j)-d(:)*a(:,j); ENDDO
END SUBROUTINE MSBDB

!=============================================================================
SUBROUTINE UDLBV(a,v, m,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE UDLBV
!  BACk-substitution step of linear inversion involving
!  Banded matrix and Vector.
!
! --> A encodes the (L)*(D**-1)*(U) factorization of the linear-system
!     matrix, as supplied by subroutine LDUB
! <-> V input as right-hand-side vector, output as solution vector
! --> M the number of rows assumed for A and for V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
!=============================================================================
INTEGER,  INTENT(IN   ) :: m, mah1, mah2
REAL,     INTENT(IN   ) :: a(m,-mah1:mah2)
REAL,     INTENT(INOUT) :: v(m)
!-----------------------------------------------------------------------------
INTEGER                 :: i, j
REAL                    :: vj
!=============================================================================
DO j=1,m
   vj=v(j)
   DO i=j+1,MIN(m,j+mah1); v(i)=v(i)-a(i,j-i)*vj; ENDDO; v(j)=a(j,0)*vj
ENDDO
DO j=m,2,-1
   vj=v(j)
   DO i=MAX(1,j-mah2),j-1; v(i)=v(i)-a(i,j-i)*vj; ENDDO
ENDDO
END SUBROUTINE UDLBV

!=============================================================================
SUBROUTINE UDLBX(a,v, mx,mah1,mah2,my)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE UDLBX
!  BACk-substitution step of parallel linear inversion involving
!  Banded matrix and X-Vectors.
!
! --> A encodes the (L)*(D**-1)*(U) factorization of the linear-system
!     matrix, as supplied by subroutine LDUB or, if N=NA, by LDUB
! <-> V input as right-hand-side vectors, output as solution vectors
! --> MX the number of rows assumed for A and length of
!     X-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MY number of parallel X-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN   ) :: mx, mah1, mah2, my
REAL,     INTENT(IN   ) :: a(mx,-mah1:mah2)
REAL,     INTENT(INOUT) :: v(mx,my)
!-----------------------------------------------------------------------------
INTEGER                 :: jx, ix
!=============================================================================
DO jx=1,mx
   DO ix=jx+1,MIN(mx,jx+mah1); v(ix,:) = v(ix,:) - a(ix,jx-ix)*v(jx,:); ENDDO
   v(jx,:) = a(jx,0) * v(jx,:)
ENDDO
DO jx=mx,2,-1
   DO ix=MAX(1,jx-mah2),jx-1; v(ix,:) = v(ix,:) - a(ix,jx-ix)*v(jx,:); ENDDO
ENDDO
END SUBROUTINE UDLBX

!=============================================================================
SUBROUTINE UDLBY(a,v, my,mah1,mah2,mx)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE UDLBY
!  BACk-substitution step of parallel linear inversion involving
!  Banded matrix and Y-Vectors.
!
! --> A encodes the (L)*(D**-1)*(U) factorization of the linear-system
!     matrix, as supplied by subroutine LDUB or, if N=NA, by LDUB
! <-> V input as right-hand-side vectors, output as solution vectors
! --> MY the number of rows assumed for A and length of
!     Y-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MX number of parallel Y-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN   ) :: my, mah1, mah2, mx
REAL,     INTENT(IN   ) :: a(my,-mah1:mah2)
REAL,     INTENT(INOUT) :: v(mx,my)
!-----------------------------------------------------------------------------
INTEGER                 :: iy, jy
!=============================================================================
DO jy=1,my
   DO iy=jy+1,MIN(my,jy+mah1); v(:,iy) = v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
   v(:,jy)=a(jy,0)*v(:,jy)
ENDDO
DO jy=my,2,-1
   DO iy=MAX(1,jy-mah2),jy-1; v(:,iy)=v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
ENDDO
END SUBROUTINE UDLBY

!=============================================================================
SUBROUTINE UDLVB(v,a, m,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE UDLVB
!  BACk-substitution step of linear inversion involving
!  row-Vector and Banded matrix.
!
! <-> V input as right-hand-side row-vector, output as solution vector
! --> A encodes the (L)*(D**-1)*(U) factorization of the linear-system
!     matrix, as supplied by subroutine LDUB
! --> M the number of rows assumed for A and columns for V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
!=============================================================================
INTEGER,  INTENT(IN   ) :: m, mah1, mah2
REAL,     INTENT(IN   ) :: a(m,-mah1:mah2)
REAL,     INTENT(INOUT) :: v(m)
!-----------------------------------------------------------------------------
INTEGER                 :: i, j
REAL                    :: vi
!=============================================================================
DO i=1,m
   vi=v(i)
   DO j=i+1,MIN(m,i+mah2); v(j)=v(j)-vi*a(i,j-i); ENDDO
   v(i)=vi*a(i,0)
ENDDO
DO i=m,2,-1
   vi=v(i)
   DO j=MAX(1,i-mah1),i-1; v(j)=v(j)-vi*a(i,j-i); ENDDO
ENDDO
END SUBROUTINE UDLVB

!=============================================================================
SUBROUTINE UDLXB(v,a, mx,mah1,mah2,my)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE UDLXB
!  BACk-substitution step of parallel linear inversion involving
!  Banded matrix and row-X-Vectors.
!
! <-> V input as right-hand-side vectors, output as solution vectors
! --> A encodes the (L)*(D**-1)*(U) factorization of the linear-system
!     matrix, as supplied by subroutine LDUB
! --> MX the number of rows assumed for A and length of
!     X-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MY number of parallel X-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN   ) :: mx, mah1, mah2, my
REAL,     INTENT(IN   ) :: a(mx,-mah1:mah2)
REAL,     INTENT(INOUT) :: v(mx,my)
!-----------------------------------------------------------------------------
INTEGER                 :: ix, jx
!=============================================================================
DO ix=1,mx
   DO jx=ix+1,MIN(mx,ix+mah2); v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
   v(ix,:)=v(ix,:)*a(ix,0)
ENDDO
DO ix=mx,2,-1
   DO jx=MAX(1,ix-mah1),ix-1; v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
ENDDO
END SUBROUTINE UDLXB

!=============================================================================
SUBROUTINE UDLYB(v,a, my,mah1,mah2,mx)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE UDLYB
!  BACk-substitution step of parallel linear inversion involving
!  Banded matrix and row-Y-Vectors.
!
! <-> V input as right-hand-side vectors, output as solution vectors
! --> A encodes the (L)*(D**-1)*(U) factorization of the linear-system
!     matrix, as supplied by subroutine LDUB
! --> MY the number of rows assumed for A and length of
!     Y-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MX number of parallel Y-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN   ) :: my, mah1, mah2, mx
REAL,     INTENT(IN   ) :: a(my,-mah1:mah2)
REAL,     INTENT(INOUT) :: v(mx,my)
!-----------------------------------------------------------------------------
INTEGER                 :: iy, jy
!=============================================================================
DO iy=1,my
   DO jy=iy+1,MIN(my,iy+mah2); v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
   v(:,iy)=v(:,iy)*a(iy,0)
ENDDO
DO iy=my,2,-1
   DO jy=MAX(1,iy-mah1),iy-1; v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
ENDDO
END SUBROUTINE UDLYB

!=============================================================================
SUBROUTINE U1LBV(a,v, m,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE U1LBV
!  BACk-substitution step ((U**-1)*(L**-1)) of linear inversion involving
!  special Banded matrix and right-Vector.
!
! --> A encodes the [L]*[U] factorization of the linear-system
!     matrix, as supplied by subroutine L1UBB
! <-> V input as right-hand-side vector, output as solution vector
! --> M the number of rows assumed for A and for V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
!=============================================================================
INTEGER,  INTENT(IN   ) :: m, mah1, mah2
REAL,     INTENT(IN   ) :: a(m,-mah1:mah2)
REAL,     INTENT(INOUT) :: v(m)
!-----------------------------------------------------------------------------
INTEGER                 :: i, j
REAL                    :: vj
!=============================================================================
DO j=1,m
   vj=v(j)
   DO i=j+1,MIN(m,j+mah1); v(i)=v(i)-a(i,j-i)*vj; ENDDO
ENDDO
DO j=m,2,-1
   vj=v(j)
   DO i=MAX(1,j-mah2),j-1; v(i)=v(i)-a(i,j-i)*vj; ENDDO
ENDDO
END SUBROUTINE U1LBV

!=============================================================================
SUBROUTINE U1LBX(a,v, mx,mah1,mah2,my)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE U1LBX
!  Special BaCk-substitution step of parallel linear inversion involving
!  Banded matrix and X-right-Vectors.
!
! --> A encodes the [L]*[U] factorization of the linear-system
!     matrix, as supplied by subroutine L1UBB
! <-> V input as right-hand-side vectors, output as solution vectors
! --> MX the number of rows assumed for A and length of
!     X-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MY number of parallel X-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN   ) :: mx, mah1, mah2, my
REAL,     INTENT(IN   ) :: a(mx,-mah1:mah2)
REAL,     INTENT(INOUT) :: v(mx,my)
!-----------------------------------------------------------------------------
INTEGER                 :: ix, jx
!=============================================================================
DO jx=1,mx
   DO ix=jx+1,MIN(mx,jx+mah1); v(ix,:)=v(ix,:)-a(ix,jx-ix)*v(jx,:); ENDDO
ENDDO
DO jx=mx,2,-1
   DO ix=MAX(1,jx-mah2),jx-1; v(ix,:)=v(ix,:)-a(ix,jx-ix)*v(jx,:); ENDDO
ENDDO
END SUBROUTINE U1LBX

!=============================================================================
SUBROUTINE U1LBY(a,v, my,mah1,mah2,mx)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE U1LBY
!  Special BaCk-substitution step of parallel linear inversion involving
!  Banded matrix and Y-right-Vectors.
!
! --> A encodes the [L]*[U] factorization of the linear-system
!     matrix, as supplied by subroutine L1UBB
! <-> V input as right-hand-side vectors, output as solution vectors
! --> MY the number of rows assumed for A and length of
!     Y-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MX number of parallel Y-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN   ) :: my, mah1, mah2, mx
REAL,     INTENT(IN   ) :: a(my,-mah1:mah2)
REAL,     INTENT(INOUT) :: v(mx,my)
!-----------------------------------------------------------------------------
INTEGER                 :: iy, jy
!=============================================================================
DO jy=1,my
   DO iy=jy+1,MIN(my,jy+mah1); v(:,iy)=v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
ENDDO
DO jy=my,2,-1
   DO iy=MAX(1,jy-mah2),jy-1; v(:,iy)=v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
ENDDO
END SUBROUTINE U1LBY

!=============================================================================
SUBROUTINE U1LVB(v,a, m,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE U1LVB
!  Special BaCk-substitution step of linear inversion involving
!  left-Vector and Banded matrix.
!
! <-> V input as right-hand-side row-vector, output as solution vector
! --> A encodes the special [L]*[U] factorization of the linear-system
!     matrix, as supplied by subroutine L1UBB
! --> M the number of rows assumed for A and columns for V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
!=============================================================================
INTEGER, INTENT(IN   ) :: m, mah1, mah2
REAL,    INTENT(IN   ) :: a(m,-mah1:mah2)
REAL,    INTENT(INOUT) :: v(m)
!-----------------------------------------------------------------------------
INTEGER                :: i, j
REAL                   :: vi
!=============================================================================
DO i=1,m
   vi=v(i)
   DO j=i+1,MIN(m,i+mah2); v(j)=v(j)-vi*a(i,j-i); ENDDO
ENDDO
DO i=m,2,-1
   vi=v(i)
   DO j=MAX(1,i-mah1),i-1; v(j)=v(j)-vi*a(i,j-i); ENDDO
ENDDO
END SUBROUTINE U1LVB

!=============================================================================
SUBROUTINE U1LXB(v,a, mx,mah1,mah2,my)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE U1LXB
!  Special BaCk-substitution step of parallel linear inversion involving
!  Banded matrix and X-left-Vectors.
!
! <-> V input as right-hand-side vectors, output as solution vectors
! --> A encodes the special [L]*[U] factorization of the linear-system
!     matrix, as supplied by subroutine L1UBB
! --> MX the number of rows assumed for A and length of
!     X-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MY number of parallel X-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN   ) :: mx, mah1, mah2, my
REAL,     INTENT(IN   ) :: a(mx,-mah1:mah2)
REAL,     INTENT(INOUT) :: v(mx,my)
!-----------------------------------------------------------------------------
INTEGER                 :: ix, jx
!=============================================================================
DO ix=1,mx
   DO jx=ix+1,MIN(mx,ix+mah2); v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
ENDDO
DO ix=mx,2,-1
   DO jx=MAX(1,ix-mah1),ix-1;  v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
ENDDO
END SUBROUTINE U1LXB

!=============================================================================
SUBROUTINE U1LYB(v,a, my,mah1,mah2,mx)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE U1LYB
!  Special BaCk-substitution step of parallel linear inversion involving
!  special Banded matrix and Y-left-Vectors.
!
! <-> V input as right-hand-side vectors, output as solution vectors
! --> A encodes the [L]*[U] factorization of the linear-system
!     matrix, as supplied by subroutine L1UBB
! --> MY the number of rows assumed for A and length of
!     Y-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MX number of parallel Y-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN   ) :: my, mah1, mah2, mx
REAL,     INTENT(IN   ) :: a(my,-mah1:mah2)
REAL,     INTENT(INOUT) :: v(mx,my)
!-----------------------------------------------------------------------------
INTEGER                 :: iy, jy
!=============================================================================
DO iy=1,my
   DO jy=iy+1,MIN(my,iy+mah2); v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
ENDDO
DO iy=my,2,-1
   DO jy=MAX(1,iy-mah1),iy-1;  v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
ENDDO
END SUBROUTINE U1LYB

!=============================================================================
SUBROUTINE LINBV(a,v,m,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE LINBV
!   Solve LINear system with square Banded-matrix and vector V
!
! <-> A system matrix on input, its [L]*[D**-1]*[U] factorization on exit
! <-> V vector of right-hand-sides on input, solution vector on exit
! --> M order of matrix A
! --> MAH1 left half-bandwidth of A
! --> MAH2 right half-bandwidth of A
!=============================================================================
INTEGER, INTENT(IN   ) :: m, mah1, mah2
REAL,    INTENT(INOUT) :: a(m,-mah1:mah2), v(m)
!=============================================================================
CALL LDUB(a,m,mah1,mah2)
CALL UDLBV(a,v,m,mah1,mah2)
END SUBROUTINE LINBV

!=============================================================================
SUBROUTINE WRTB(a,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,  INTENT(IN) :: m1, m2, mah1, mah2
REAL,     INTENT(IN) :: a(m1,-mah1:mah2)
!-----------------------------------------------------------------------------
INTEGER              :: i1, i2, i, j1, j2, j, nj1
!=============================================================================
DO i1=1,m1,20
   i2=MIN(i1+19,m1)
   PRINT '(7x,6(i2,10x))',(j,j=-mah1,mah2)
   DO i=i1,i2
      j1=MAX(-mah1,1-i)
      j2=MIN(mah2,m2-i)
      nj1=j1+mah1
      IF(nj1==0)PRINT '(1x,i3,6(1x,e11.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==1)PRINT '(1x,i3,12x,5(1x,e11.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==2)PRINT '(1x,i3,24x,4(1x,e11.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==3)PRINT '(1x,i3,36x,3(1x,e11.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==4)PRINT '(1x,i3,48x,2(1x,e11.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==5)PRINT '(1x,i3,60x,1(1x,e11.5))',i,(a(i,j),j=j1,j2)
   ENDDO
   READ(*,*)
ENDDO
END SUBROUTINE WRTB

