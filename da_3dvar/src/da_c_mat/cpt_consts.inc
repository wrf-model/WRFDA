!						***************************
!						*  module_cpt_consts.f90  *
!						*  PURSER 2001            *
! 						***************************
!MODULE MODULE_cpt_consts
!---------------------------------------------------------------------------
! Constants required by various "compact" numerical schemes on uniform grids
! These are all initialized by a call to subroutine set_cpt_consts
! These are for standard precision;
! for real(8) precision, use module_cpt_constd.
!
! The following compact schemes are accommodated:
!     4m  : 4th-order midpoint interpolation
!     6m  : 6th-order midpoint interpolation
!     8m  : 8th-order midpoint interpolation
!     10m : 10th-ordr midpoint interpolation
!     4d  : 4th-order same-grid differencing
!     8d  : 8th-order same-grid differencing
!     4qi : 4th-order staggered differencing (inverse of 4q)
!     4q  : 4th-order invertible quadrature
!     6qi : 6th-order staggered differencing (inverse of 6q)
!     6q  : 6th-order invertible quadrature
!     8qi : 8th-order staggered differencing (inverse of 8q)
!     8q  : 8th-order invertible quadrature
!     10qi: 10th-ordr staggered differencing (inverse of 10q)
!     10q : 10th-ordr invertible quadrature
!     4t  : 4th-order invertible JLM translation
!     4ti : 4th-order inverse of 4t
!     8t  : 8th-order invertible JLM translation
!     8ti : 8th-order inverse of 8t
!---------------------------------------------------------------------------
!USE MODULE_pmat1 , ONLY : inv, inv_d

!CONTAINS

!============================================================================
!  R. J. Purser, jpurser@ncep.noaa.gov
!
!  Overloaded module procedures, generic name "set_ext", for supplying
!  a variety of smooth extrapolation operators, in either single or
!  double precision, for extending data smoothly into "halo" regions
!  during the course of explicit, recursive, or mixed filtering operations.
!  This strategy allows generic filters (with simple constant coefficients)
!  to be applied right up to the horizontal domain~s boundaries.
!
!  Seven modes of application are accommodated, depending on the number
!  (1,..,8) of subroutine arguments. These modes are as follows:
!  1. set_ext(v): 
!                 v a real vector of size np is returned containing weights
!                 for np-th degree polynomial extrapolation one step beyond
!                 the end of the data given. Iterated application allows
!                 further steps of extrapolation, clearly.
!  2. set_ext(np, v): 
!                 Same as 1 above, except np is now explicitly stated.
!  3. set_ext(np,nl, w): 
!                 np is the degree of extrapolating polynomial, integer
!                 nl is the width of the halo for which real matrix
!                 w (dimensions (nl,np)) gives all the weights directly.
!  4. set_ext(np,nl, L, w): 
!                 For the case of a recursive filter of order nl, a halo
!                 of width nl containing suitable values is needed at
!                 filter~s incoming boundary. If resulting filtered data
!                 are to be consistent with polynomial extrapolation of degree
!                 np, the (nl,np)-dimensioned real weights, w, can be used
!                 to generate the nl halo values from the first np input data.
!  5. set_ext(nm,nl, M,L, w):
!                 Similar to 4 above, except polynomial extrapolation is
!                 replaced by extrapolation consistent with nm-th order
!                 outgoing recursive filter with coefficients M.
!  6. set_ext(np,nl,nk, L,K, w):
!                 Similar to 4 above, except incoming filter is of mixed
!                 type with nl recursive coefficients L and nk explicit
!                 coefficients K. (If L acts on "output" to left of current
!                 value, then K acts on the unchanged 'input' to the right.)
!                 The dimensions of w are (nl,np+nk) for this case.
!  7. set_ext(nm,nl,nk, M,L,K, w):
!                 Similar to 6 above, except polynomial extrapolation is
!                 replaced by extrapolation consistent with nm-th order
!                 outgoing recursive filter with coefficients M.
!  8. set_ext(np,nm,nm,nk, M,L,K, w):
!                 Similar to 6 above, except polynomial extrapolation is
!                 replaced by extrapolation consistent with the (np+nm)-th
!                 order filter formed by compounding polynomial extrapolation
!                 of order np with a given filter M of nm-th order.
!============================================================================ 
!============================================================================
SUBROUTINE set_ext2(np,pqn)
!============================================================================
  INTEGER,                 INTENT(IN   ):: np
  REAL,DIMENSION(np),      INTENT(OUT  ):: pqn
!----------------------------------------------------------------------------
  INTEGER                               :: i,k
  REAL,DIMENSION(0:np)                  :: pascal
!============================================================================
! Set pascal to the alternating (-,+) np-th row of pascal~s triangle:
pascal=0
pascal(0)=-1
DO k=1,np
   DO i=k,1,-1
      pascal(i)=pascal(i)-pascal(i-1)
   ENDDO
ENDDO
pqn=pascal(1:np)
END SUBROUTINE set_ext2

!============================================================================
SUBROUTINE set_ext7(nm,nl,nk, mqn,lqn,kqn, wext)
!============================================================================
  INTEGER,                 INTENT(IN   ):: nm,nl,nk
  REAL,DIMENSION(nm),      INTENT(IN   ):: mqn
  REAL,DIMENSION(nl),      INTENT(IN   ):: lqn
  REAL,DIMENSION(nk),      INTENT(IN   ):: kqn
  REAL,DIMENSION(nl,nk+nm),INTENT(OUT  ):: wext
!============================================================================
CALL set_ext8(0,nm,nl,nk, mqn,lqn,kqn, wext)
END SUBROUTINE set_ext7

!============================================================================
SUBROUTINE set_ext8(np,nm,nl,nk, mqn,lqn,kqn, wext)
!============================================================================
  INTEGER,                    INTENT(IN   ):: np,nm,nl,nk
  REAL,DIMENSION(nm),         INTENT(IN   ):: mqn
  REAL,DIMENSION(nl),         INTENT(IN   ):: lqn
  REAL,DIMENSION(nk),         INTENT(IN   ):: kqn
  REAL,DIMENSION(nl,nk+np+nm),INTENT(OUT  ):: wext
!----------------------------------------------------------------------------
  REAL,DIMENSION(nl+np+nm,nl+np+nm)        :: am
  REAL,DIMENSION(nl+np+nm,nk+np+nm)        :: bm
  REAL,DIMENSION(np+nm)                    :: pmqn
  INTEGER                                  :: npm,npml,npmk,i,k
!============================================================================
npm=np+nm
npml=npm+nl
npmk=npm+nk
IF(npmk==0)RETURN
CALL set_mext(np,nm,mqn,pmqn)
am=0.
bm=0.
IF(nl > 0)THEN
   am(1,1)=1.; 
   DO k=1,npm; am(1,k+1)=pmqn(k); ENDDO !am(1,2:npm+1)=pmqn
   DO i=2,nl ! Diagonally repeat this pattern to make nl copies:
      DO k=2,npml; am(i,k)=am(i-1,k-1); ENDDO ! am(i,2:npml)=am(i-1,1:npml-1)
   ENDDO
ENDIF
! Set up L and K coefficients at left of rows nl+1 of am and bm:
am(nl+1,nl+1)=1; am(nl+1,nl:1:-1)=lqn

bm(nl+1,1)   =1; bm(nl+1,2:nk+1) =kqn

DO k=nl+2,npml ! Diagonally repeat these patterns to make nm copies of each:
   am(k,2:npml)=am(k-1,1:npml-1)
   bm(k,2:npmk)=bm(k-1,1:npmk-1)
ENDDO

! Invert the linear equations with system matrix am and right-hand sides bm:
CALL inv(am,bm)

! Copy the extrapolation coefficients from the first nl rows of the solution
! matrix, but reverse the order for consistency with convention that halo
! indices and interior indices both increase away from their common boundary:
wext=bm(NL:1:-1,:)
END SUBROUTINE set_ext8

!============================================================================
SUBROUTINE set_ext6(np,nl,nk, lqn,kqn, wext)
!============================================================================
  INTEGER,                 INTENT(IN   ):: np,nl,nk
  REAL,DIMENSION(nl),      INTENT(IN   ):: lqn
  REAL,DIMENSION(nk),      INTENT(IN   ):: kqn
  REAL,DIMENSION(nl,nk+np),INTENT(OUT  ):: wext
!----------------------------------------------------------------------------
  REAL,DIMENSION(np)                    :: pqn
!============================================================================
CALL set_ext2(np,pqn) ! Set P-filter coefficients to polynomial extrapolation
CALL set_ext7(np,nl,nk, pqn,lqn,kqn, wext)
END SUBROUTINE set_ext6

!============================================================================
SUBROUTINE set_ext4(np,nl, lqn, wext)
!============================================================================
  INTEGER,                 INTENT(IN   ):: np,nl
  REAL,DIMENSION(nl),      INTENT(IN   ):: lqn
  REAL,DIMENSION(nl,np),   INTENT(OUT  ):: wext
!----------------------------------------------------------------------------
  REAL,DIMENSION(0:0)                     :: kqn
!============================================================================
CALL set_ext6(np,nl,0, lqn,kqn, wext)
END SUBROUTINE set_ext4

!============================================================================
SUBROUTINE set_ext3(np,nl, wext)
!============================================================================
  INTEGER,                 INTENT(IN   ):: np,nl
  REAL,DIMENSION(nl,np),   INTENT(OUT  ):: wext
!----------------------------------------------------------------------------
  REAL,DIMENSION(nl)                    :: lqn
!============================================================================
lqn=0
CALL set_ext4(np,nl, lqn, wext)
END SUBROUTINE set_ext3

!============================================================================
SUBROUTINE set_ext5(nm,nl, mqn,lqn, wext)
!============================================================================
  INTEGER,                 INTENT(IN   ):: nm,nl
  REAL,DIMENSION(nm),      INTENT(IN   ):: mqn
  REAL,DIMENSION(nl),      INTENT(IN   ):: lqn
  REAL,DIMENSION(nl,nm),   INTENT(OUT  ):: wext
!----------------------------------------------------------------------------
  REAL,DIMENSION(0:0)                     :: kqn
!============================================================================
CALL set_ext7(nm,nl,0, mqn,lqn,kqn, wext)
END SUBROUTINE set_ext5

!============================================================================
SUBROUTINE set_ext1(pqn)
!============================================================================
  REAL,DIMENSION(:),       INTENT(OUT  ):: pqn
!----------------------------------------------------------------------------
  INTEGER                               :: np
!============================================================================
np=SIZE(pqn)
CALL set_ext2(np,pqn)
END SUBROUTINE set_ext1

!============================================================================
SUBROUTINE set_ext2_d(np,pqn)
!============================================================================
  INTEGER,                     INTENT(IN   ):: np
  REAL(8), DIMENSION(np),      INTENT(OUT  ):: pqn
!----------------------------------------------------------------------------
  INTEGER                                   :: i,k
  REAL(8), DIMENSION(0:np)                  :: pascal
!============================================================================
! Set pascal to the alternating (-,+) np-th row of pascal~s triangle:
pascal=0
pascal(0)=-1
DO k=1,np
   DO i=k,1,-1
      pascal(i)=pascal(i)-pascal(i-1)
   ENDDO
ENDDO
pqn=pascal(1:np)
END SUBROUTINE set_ext2_d

!============================================================================
SUBROUTINE set_ext7_d(nm,nl,nk, mqn,lqn,kqn, wext)
!============================================================================
  INTEGER,                    INTENT(IN   ):: nm,nl,nk
  REAL(8),DIMENSION(nm),      INTENT(IN   ):: mqn
  REAL(8),DIMENSION(nl),      INTENT(IN   ):: lqn
  REAL(8),DIMENSION(nk),      INTENT(IN   ):: kqn
  REAL(8),DIMENSION(nl,nk+nm),INTENT(OUT  ):: wext
!============================================================================
CALL set_ext8_d(0,nm,nl,nk, mqn,lqn,kqn, wext)
END SUBROUTINE set_ext7_d

!============================================================================
SUBROUTINE set_ext8_d(np,nm,nl,nk, mqn,lqn,kqn, wext)
!============================================================================
  INTEGER,                       INTENT(IN   ):: np,nm,nl,nk
  REAL(8),DIMENSION(nm),         INTENT(IN   ):: mqn
  REAL(8),DIMENSION(nl),         INTENT(IN   ):: lqn
  REAL(8),DIMENSION(nk),         INTENT(IN   ):: kqn
  REAL(8),DIMENSION(nl,nk+np+nm),INTENT(OUT  ):: wext
!----------------------------------------------------------------------------
  REAL(8),DIMENSION(nl+np+nm,nl+np+nm)        :: am
  REAL(8),DIMENSION(nl+np+nm,nk+np+nm)        :: bm
  REAL(8),DIMENSION(np+nm)                    :: pmqn
  INTEGER                                     :: npm,npml,npmk,i,k
!============================================================================
npm=np+nm
npml=npm+nl
npmk=npm+nk
CALL set_mext_d(np,nm,mqn,pmqn)
am=0.
bm=0.
IF(nl > 0)THEN
   am(1,1)=1.; am(1,2:npm+1)=pmqn
   DO k=2,nl ! Diagonally repeat this pattern to make nl copies:
      am(k,2:npml)=am(k-1,1:npml-1)
   ENDDO
ENDIF
! Set up L and K coefficients at left of rows nl+1 of am and bm:
am(nl+1,nl+1)=1; am(nl+1,nl:1:-1)=lqn
bm(nl+1,1)   =1; bm(nl+1,2:nk+1) =kqn
DO k=nl+2,npml ! Diagonally repeat these patterns to make nm copies of each:
   am(k,2:npml)=am(k-1,1:npml-1)
   bm(k,2:npmk)=bm(k-1,1:npmk-1)
ENDDO

! Invert the linear equations with system matrix am and right-hand sides bm:
CALL inv_d(am,bm)

! Copy the extrapolation coefficients from the first nl rows of the solution
! matrix, but reverse the order for consistency with convention that halo
! indices and interior indices both increase away from their common boundary:
wext=bm(nl:1:-1,:)
END SUBROUTINE set_ext8_d

!============================================================================
SUBROUTINE set_ext6_d(np,nl,nk, lqn,kqn, wext)
!============================================================================
  INTEGER,                     INTENT(IN   ):: np,nl,nk
  REAL(8), DIMENSION(nl),      INTENT(IN   ):: lqn
  REAL(8), DIMENSION(nk),      INTENT(IN   ):: kqn
  REAL(8), DIMENSION(nl,nk+np),INTENT(OUT  ):: wext
!----------------------------------------------------------------------------
  REAL(8), DIMENSION(np)                    :: pqn
!============================================================================
CALL set_ext2_d(np,pqn) ! Set P-filter coefficients to polynomial extrapolation
CALL set_ext7_d(np,nl,nk, pqn,lqn,kqn, wext)
END SUBROUTINE set_ext6_d

!============================================================================
SUBROUTINE set_ext4_d(np,nl, lqn, wext)
!============================================================================
  INTEGER,                     INTENT(IN   ):: np,nl
  REAL(8), DIMENSION(nl),      INTENT(IN   ):: lqn
  REAL(8), DIMENSION(nl,np),   INTENT(OUT  ):: wext
!----------------------------------------------------------------------------
  REAL(8), DIMENSION(0:0)                     :: kqn
!============================================================================
CALL set_ext6_d(np,nl,0, lqn,kqn, wext)
END SUBROUTINE set_ext4_d

!============================================================================
SUBROUTINE set_ext3_d(np,nl, wext)
!============================================================================
  INTEGER,                     INTENT(IN   ):: np,nl
  REAL(8), DIMENSION(nl,np),   INTENT(OUT  ):: wext
!----------------------------------------------------------------------------
  REAL(8), DIMENSION(nl)                    :: lqn
!============================================================================
lqn=0
CALL set_ext4_d(np,nl, lqn, wext)
END SUBROUTINE set_ext3_d

!============================================================================
SUBROUTINE set_ext5_d(nm,nl, mqn,lqn, wext)
!============================================================================
  INTEGER,                    INTENT(IN   ):: nm,nl
  REAL(8),DIMENSION(nm),      INTENT(IN   ):: mqn
  REAL(8),DIMENSION(nl),      INTENT(IN   ):: lqn
  REAL(8),DIMENSION(nl,nm),   INTENT(OUT  ):: wext
!----------------------------------------------------------------------------
  REAL(8),DIMENSION(0:0)                     :: kqn
!============================================================================
CALL set_ext7_d(nm,nl,0, mqn,lqn,kqn, wext)
END SUBROUTINE set_ext5_d

!============================================================================
SUBROUTINE set_ext1_d(pqn)
!============================================================================
  REAL(8), DIMENSION(:),       INTENT(OUT  ):: pqn
!----------------------------------------------------------------------------
  INTEGER                                   :: np
!============================================================================
np=SIZE(pqn)
CALL set_ext2_d(np,pqn)
END SUBROUTINE set_ext1_d

!============================================================================
SUBROUTINE set_mext4(np,nm,mqn,pmqn)
!============================================================================
  INTEGER,                 INTENT(IN   ):: np,nm
  REAL,DIMENSION(nm),      INTENT(IN   ):: mqn 
  REAL,DIMENSION(np+nm),   INTENT(OUT  ):: pmqn
!----------------------------------------------------------------------------
  INTEGER                               :: i,k
  REAL,DIMENSION(0:np+nm)               :: pascal
!============================================================================
! Set pascal to the alternating (-,+) np-th row of pascal~s triangle
! convolved with the complete kernel of the input filter M:
pascal=0
pascal(0)=1
pascal(1:nm)=mqn
DO k=1,np
   DO i=k+nm,1,-1
      pascal(i)=pascal(i)-pascal(i-1)
   ENDDO
ENDDO
pmqn=pascal(1:np+nm)
END SUBROUTINE set_mext4

!============================================================================
SUBROUTINE set_mext5(nk,nm,kqn,mqn,kmqn)
!============================================================================
  INTEGER,                 INTENT(IN   ):: nk,nm
  REAL,DIMENSION(nk),      INTENT(IN   ):: kqn 
  REAL,DIMENSION(nm),      INTENT(IN   ):: mqn 
  REAL,DIMENSION(nk+nm),   INTENT(OUT  ):: kmqn
!----------------------------------------------------------------------------
  INTEGER                               :: k
  REAL,DIMENSION(0:nk+nm)               :: pascal
!============================================================================
! Convolve filter kernels K and M to get new filter, KM
pascal=0
pascal(0)=1
pascal(1:nm)=mqn
pascal(1:nk)=pascal(1:nk)+kqn
DO k=1,nk
   pascal(1+k:nm+k)=pascal(1+k:nm+k)+kqn(k)*mqn
ENDDO
kmqn=pascal(1:nk+nm)
END SUBROUTINE set_mext5

!============================================================================
SUBROUTINE set_mext4_d(np,nm,mqn,pmqn)
!============================================================================
  INTEGER,                     INTENT(IN   ):: np,nm
  REAL(8), DIMENSION(nm),      INTENT(IN   ):: mqn 
  REAL(8), DIMENSION(np+nm),   INTENT(OUT  ):: pmqn
!----------------------------------------------------------------------------
  INTEGER                                   :: i,k
  REAL(8), DIMENSION(0:np+nm)               :: pascal
!============================================================================
! Set pascal to the alternating (-,+) np-th row of pascal~s triangle
! convolved with the complete kernel of the input filter M:
pascal=0
pascal(0)=1
pascal(1:nm)=mqn
DO k=1,np
   DO i=k+nm,1,-1
      pascal(i)=pascal(i)-pascal(i-1)
   ENDDO
ENDDO
pmqn=pascal(1:np+nm)
END SUBROUTINE set_mext4_d

!============================================================================
SUBROUTINE set_mext5_d(nk,nm,kqn,mqn,kmqn)
!============================================================================
  INTEGER,                    INTENT(IN   ):: nk,nm
  REAL(8),DIMENSION(nk),      INTENT(IN   ):: kqn 
  REAL(8),DIMENSION(nm),      INTENT(IN   ):: mqn 
  REAL(8),DIMENSION(nk+nm),   INTENT(OUT  ):: kmqn
!----------------------------------------------------------------------------
  INTEGER                                  :: k
  REAL(8),DIMENSION(0:nk+nm)               :: pascal
!============================================================================
! Convolve filter kernels K and M to get new filter, KM
pascal=0
pascal(0)=1
pascal(1:nm)=mqn
pascal(1:nk)=pascal(1:nk)+kqn
DO k=1,nk
   pascal(1+k:nm+k)=pascal(1+k:nm+k)+kqn(k)*mqn
ENDDO
kmqn=pascal(1:nk+nm)
END SUBROUTINE set_mext5_d

!============================================================================
SUBROUTINE set_cpt_wextsall(np)
  IMPLICIT NONE
  INTEGER,INTENT(IN):: np

CALL set_cpt_wexts('4m', np)
CALL set_cpt_wexts('8m', np)
CALL set_cpt_wexts('10m',np)
CALL set_cpt_wexts('4d', np)
CALL set_cpt_wexts('8d', np)
CALL set_cpt_wexts('4q', np)
CALL set_cpt_wexts('6q', np)
CALL set_cpt_wexts('8q', np)
CALL set_cpt_wexts('10q',np)
CALL set_cpt_wexts('4t', np)
CALL set_cpt_wexts('8t', np)
CALL set_cpt_wexts('6m', np)
END SUBROUTINE  set_cpt_wextsall

!============================================================================
SUBROUTINE set_cpt_wexts(ch,np)
IMPLICIT NONE
CHARACTER(len=*),INTENT(IN) :: ch
INTEGER,         INTENT(IN) :: np
REAL,DIMENSION(0:0):: N0
REAL,DIMENSION(1):: I1, J1, L1, K1, N1
REAL,DIMENSION(2):: L2, K2, N2
REAL,DIMENSION(3):: K3
DATA I1/-1./
DATA J1/1./
DATA N1/0./
DATA N2/0.,0./
IF(np<0 .OR. np>nnp)STOP 'In set_cpt_wexts; np out of bounds'
SELECT CASE(ch)
CASE('4m')
!   PRINT *, ' Initialize extrapolation weights for case 4m'
   np4m=np
   IF( ALLOCATED(w_L_4m)) &
        DEALLOCATE(wj__4m, w___4m, w_L_4m, wLL_4m, wL__4m)
   ALLOCATE(                                          wj__4m(1,np+1),  &
                                                      w___4m(1,np  ),  &
                                                      w_L_4m(1,np  ),  &
                                                      wLL_4m(1,np+1),  &
                                                      wL__4m(1,np+1)   )
   L1(1)=l4m1
   CALL set_ext(np,1,1,0,J1,N1,N0,wj__4m)
   CALL set_ext(np,0,1,0,N0,N1,N0,w___4m)
   CALL set_ext(np,0,1,0,N0,L1,N0,w_L_4m)
   CALL set_ext(np,1,1,0,L1,L1,N0,wLL_4m)
   CALL set_ext(np,1,1,0,L1,N1,N0,wL__4m)
CASE('6m')
!   PRINT '(" Initialize extrapolation weights for case 6m")'
   np6m=np
   IF( ALLOCATED(wK__6m)) &
        DEALLOCATE(wJ__6m, wK__6m, w_LK6m, wLLK6m, wL__6m, w_L_6m)
   ALLOCATE(                                          wJ__6m(1,np+2),  &
                                                      wK__6m(1,np+1),  &
                                                      w_LK6m(1,np+1),  &
                                                      wLLK6m(1,np+2),  &
                                                      wL__6m(1,np+1),  &
                                                      w_L_6m(1,np  )   )
   L1(1)=l6m1
   K1(1)=k6m1
   CALL set_mext(1,1,J1,K1,K2)
   CALL set_ext(np,2,1,0,K2,N1,N0,wJ__6m)
   CALL set_ext(np,1,1,0,K1,N1,N0,wK__6m)
   CALL set_ext(np,0,1,1,N0,L1,K1,w_LK6m)
   CALL set_ext(np,1,1,1,L1,L1,K1,wLLK6m)
   CALL set_ext(np,1,1,0,L1,N1,N0,wL__6m)
   CALL set_ext(np,0,1,0,N0,L1,N0,w_L_6m)
CASE('8m')
!   PRINT '(" Initialize extrapolation weights for case 8m")'
   np8m=np
   IF( ALLOCATED(wK__8m)) &
        DEALLOCATE(wJ__8m, wK__8m, w_LK8m, wLLK8m, wL__8m, w_L_8m)
   ALLOCATE(                                          wJ__8m(1,np+2),  &
                                                      wK__8m(1,np+1),  &
                                                      w_LK8m(2,np+1),  &
                                                      wLLK8m(2,np+3),  &
                                                      wL__8m(1,np+2),  &
                                                      w_L_8m(2,np  )   )
   L2(1)=l8m1; L2(2)=l8m2
   K1(1)=k8m1
   CALL set_mext(1,1,J1,K1,K2)
   CALL set_ext(np,2,1,0,K2,N1,N0,wJ__8m)
   CALL set_ext(np,1,1,0,K1,N1,N0,wK__8m)
   CALL set_ext(np,0,2,1,N0,L2,K1,w_LK8m)
   CALL set_ext(np,2,2,1,L2,L2,K1,wLLK8m)
   CALL set_ext(np,2,1,0,L2,N1,N0,wL__8m)
   CALL set_ext(np,0,2,0,N0,L2,N0,w_L_8m)
CASE('10m')
!   PRINT '(" Initialize extrapolation weights for case 10m")'
   np10m=np
   IF( ALLOCATED(wK__10m)) &
        DEALLOCATE(wJ__10m, wK__10m, w_LK10m, wLLK10m, wL__10m, w_L_10m)
   ALLOCATE(                                          wJ__10m(1,np+3), &
                                                      wK__10m(2,np+2), &
                                                      w_LK10m(2,np+2), &
                                                      wLLK10m(2,np+4), &
                                                      wL__10m(1,np+2), &
                                                      w_L_10m(2,np  )  )
   L2(1)=l10m1; L2(2)=l10m2
   K2(1)=k10m1; K2(2)=k10m2
   CALL set_mext(1,2,J1,K2,K3)
   CALL set_ext(np,3,1,0,K3,N1,N0,wJ__10m)
   CALL set_ext(np,2,2,0,K2,N2,N0,wK__10m)
   CALL set_ext(np,0,2,2,N0,L2,K2,w_LK10m)
   CALL set_ext(np,2,2,2,L2,L2,K2,wLLK10m)
   CALL set_ext(np,2,1,0,L2,N1,N0,wL__10m)
   CALL set_ext(np,0,2,0,N0,L2,N0,w_L_10m)

CASE('4d')
!   PRINT '(" Initialize extrapolation weights for case 4d")'
   np4d=np
   IF( ALLOCATED(w_L_4d)) &
        DEALLOCATE(wI__4d, w_L_4d, wLL_4d, wL__4d)
   ALLOCATE(                                          wI__4d(1,np+1),  &
                                                      w_L_4d(1,np  ),  &
                                                      wLL_4d(1,np+1),  &
                                                      wL__4d(1,np+1)   )
   L1(1)=l4d1
   CALL set_ext(np,1,1,0,I1,N1,N0,wI__4d)
   CALL set_ext(np,0,1,0,N0,L1,N0,w_L_4d)
   CALL set_ext(np,1,1,0,L1,L1,N0,wLL_4d)
   CALL set_ext(np,1,1,0,L1,N1,N0,wL__4d)

CASE('8d')
!   PRINT '(" Initialize extrapolation weights for case 8d")'
   np8d=np
   IF( ALLOCATED(wK__8d)) &
        DEALLOCATE(wI__8d, wK__8d, w_LK8d, wLLK8d, wL__8d, w_L_8d)
   ALLOCATE(                                          wI__8d(1,np+2),  &
                                                      wK__8d(1,np+1),  &
                                                      w_LK8d(2,np+1),  &
                                                      wLLK8d(2,np+3),  &
                                                      wL__8d(1,np+2),  &
                                                      w_L_8d(2,np  )   )
   L2(1)=l8d1; L2(2)=l8d2
   K1(1)=k8d1
   CALL set_mext(1,1,I1,K1,K2)
   CALL set_ext(np,2,1,0,K2,N1,N0,wI__8d)
   CALL set_ext(np,1,1,0,K1,N1,N0,wK__8d)
   CALL set_ext(np,0,2,1,N0,L2,K1,w_LK8d)
   CALL set_ext(np,2,2,1,L2,L2,K1,wLLK8d)
   CALL set_ext(np,2,1,0,L2,N1,N0,wL__8d)
   CALL set_ext(np,0,2,0,N0,L2,N0,w_L_8d)

CASE('4q')
!   PRINT '(" Initialize extrapolation weights for case 4q")'
   np4q=np
   IF( ALLOCATED(w_L_4q)) &
        DEALLOCATE(wI__4q, w_L_4q, wLL_4q, wL__4q, w___4q)
   ALLOCATE(                                          wI__4q(1,np+1),  &
                                                      w_L_4q(1,np  ),  &
                                                      wLL_4q(1,np+1),  &
                                                      wL__4q(1,np+1),  &
                                                      w___4q(1,np  )   )
   L1(1)=l4q1
   CALL set_ext(np,1,1,0,I1,N1,N0,wI__4q)
   CALL set_ext(np,0,1,0,N0,L1,N0,w_L_4q)
   CALL set_ext(np,1,1,0,L1,L1,N0,wLL_4q)
!---
   CALL set_ext(np,1,1,0,L1,N1,N0,wL__4q)
   CALL set_ext(np,0,1,0,N0,N1,N0,w___4q)
CASE('6q')
!   PRINT '(" Initialize extrapolation weights for case 6q")'
   np6q=np
   IF(ALLOCATED(wK__6q)) &
        DEALLOCATE(wI__6q, wK__6q, w_LK6q, wLLK6q, w_L_6q, &
                   wL__6q, w_KL6q, wKKL6q,         w_K_6q)
   ALLOCATE(                                          wI__6q(1,np+2),  &
                                                      wK__6q(1,np+1),  &
                                                      w_LK6q(1,np+1),  &
                                                      wLLK6q(1,np+2),  &
                                                      w_L_6q(1,np  ),  &
                                                      wL__6q(1,np+1),  &
                                                      w_KL6q(1,np+1),  &
                                                      wKKL6q(1,np+2),  &
                                                      w_K_6q(1,np  )   )
   L1(1)=l6q1
   K1(1)=k6q1
   CALL set_mext(1,1,I1,K1,K2)
   CALL set_ext(np,2,1,0,K2,N1,N0,wI__6q)
   CALL set_ext(np,1,1,0,K1,N1,N0,wK__6q)
   CALL set_ext(np,0,1,1,N0,L1,K1,w_LK6q)
   CALL set_ext(np,1,1,1,L1,L1,K1,wLLK6q)
   CALL set_ext(np,0,1,0,N0,L1,N0,w_L_6q)
!---
   CALL set_ext(np,1,1,0,L1,N1,N0,wL__6q)
   CALL set_ext(np,0,1,1,N0,K1,L1,w_KL6q)
   CALL set_ext(np,1,1,1,K1,K1,L1,wKKL6q)
   CALL set_ext(np,0,1,0,N0,K1,N0,w_K_6q)
CASE('8q')
!   PRINT '(" Initialize extrapolation weights for case 8q")'
   np8q=np
   IF( ALLOCATED(wK__8q)) &
        DEALLOCATE(wI__8q, wK__8q, w_LK8q, wLLK8q, w_L_8q, &
                           wL__8q, w_KL8q, wKKL8q, w_K_8q)
   ALLOCATE(                                          wI__8q(1,np+2),  &
                                                      wK__8q(1,np+1),  &
                                                      w_LK8q(2,np+1),  &
                                                      wLLK8q(2,np+3),  &
                                                      w_L_8q(2,np  ),  &
                                                      wL__8q(1,np+2),  &
                                                      w_KL8q(2,np+2),  &
                                                      wKKL8q(1,np+3),  &
                                                      w_K_8q(1,np  )   )
   L2(1)=l8q1; L2(2)=l8q2
   K1(1)=k8q1
   CALL set_mext(1,1,I1,K1,K2)
   CALL set_ext(np,2,1,0,K2,N1,N0,wI__8q)
   K2(1)=k8q1; K2(2)=0.
   CALL set_ext(np,1,1,0,K1,N1,N0,wK__8q)
   CALL set_ext(np,0,2,1,N0,L2,K1,w_LK8q)
   CALL set_ext(np,2,2,1,L2,L2,K1,wLLK8q)
   CALL set_ext(np,0,2,0,N0,L2,N0,w_L_8q)
!---
   CALL set_ext(np,2,1,0,L2,N1,N0,wL__8q)
   CALL set_ext(np,0,2,2,N0,K2,L2,w_KL8q)
   CALL set_ext(np,1,1,2,K1,K1,L2,wKKL8q)
   CALL set_ext(np,0,1,0,N0,K1,N0,w_K_8q)

CASE('10q')
!   PRINT '(" Initialize extrapolation weights for case 10q")'
   np10q=np
   IF( ALLOCATED(wK__10q)) &
        DEALLOCATE(wI__10q, wK__10q, w_LK10q, wLLK10q, w_L_10q, &
                            wL__10q, w_KL10q, wKKL10q, w_K_10q)
   ALLOCATE(                                          wI__10q(1,np+3), &
                                                      wK__10q(2,np+2), &
                                                      w_LK10q(2,np+2), &
                                                      wLLK10q(2,np+4), &
                                                      w_L_10q(2,np  ), &
                                                      wL__10q(2,np+2), &
                                                      w_KL10q(2,np+2), &
                                                      wKKL10q(2,np+4), &
                                                      w_K_10q(2,np  )  )
   L2(1)=l10q1; L2(2)=l10q2
   K2(1)=k10q1; K2(2)=k10q2
   CALL set_mext(1,2,I1,K2,K3)
   CALL set_ext(np,3,1,0,K3,N1,N0,wI__10q)
   CALL set_ext(np,2,2,0,K2,N2,N0,wK__10q)
   CALL set_ext(np,0,2,2,N0,L2,K2,w_LK10q)
   CALL set_ext(np,2,2,2,L2,L2,K2,wLLK10q)
   CALL set_ext(np,0,2,0,N0,L2,N0,w_L_10q)
!---
   CALL set_ext(np,2,2,0,L2,N2,N0,wL__10q)
   CALL set_ext(np,0,2,2,N0,K2,L2,w_KL10q)
   CALL set_ext(np,2,2,2,K2,K2,L2,wKKL10q)
   CALL set_ext(np,0,2,0,N0,K2,N0,w_K_10q)

CASE('4t')
!   PRINT '(" Initialize extrapolation weights for case 4t")'
   np4t=np
   IF( ALLOCATED(wK__4t)) &
      DEALLOCATE(wK__4t, w_LL4t, wLKK4t, wL__4t, w_KK4t, wKLL4t, &
                 w_L_4t, w_K_4t)
   ALLOCATE(                                          wK__4t(1,np+1),  &
                                                      w_LL4t(1,np+1),  &
                                                      wLKK4t(1,np+2),  &    
                                                      wL__4t(1,np+1),  &
                                                      w_KK4t(1,np+1),  &
                                                      wKLL4t(1,np+2),  &
                                                      w_L_4t(1,np  ),  &   
                                                      w_K_4t(1,np  )   )
   L1(1)=l4t1
   K1(1)=k4t1
   CALL set_ext(np,1,1,0,K1,N1,N0,wK__4t)
   CALL set_ext(np,0,1,1,N0,L1,L1,w_LL4t)
   CALL set_ext(np,1,1,1,L1,K1,K1,wLKK4t)
!---
   CALL set_ext(np,1,1,0,L1,N1,N0,wL__4t)
   CALL set_ext(np,0,1,1,N0,K1,K1,w_KK4t)
   CALL set_ext(np,1,1,1,K1,L1,L1,wKLL4t)

   CALL set_ext(np,0,1,0,N0,L1,N0,w_L_4t)
   CALL set_ext(np,0,1,0,N0,K1,N0,w_K_4t)

CASE('8t')
!   PRINT '(" Initialize extrapolation weights for case 8t")'
   np8t=np
   IF( ALLOCATED(wk__8t)) &
        DEALLOCATE(wK__8t, w_LL8t, wLKK8t, wL__8t, w_KK8t, wKLL8t, &
                   w_L_8t, w_K_8t)
   ALLOCATE(                                          wK__8t(2,np+2),  &
                                                      w_LL8t(2,np+2),  &
                                                      wLKK8t(2,np+4),  &
                                                      wL__8t(2,np+2),  &
                                                      w_KK8t(2,np+2),  &
                                                      wKLL8t(2,np+4),  &
                                                      w_L_8t(2,np  ),  &   
                                                      w_K_8t(2,np  )   )

   L2(1)=l8t1; L2(2)=l8t2
   K2(1)=k8t1; K2(2)=k8t2
   CALL set_ext(np,2,2,0,K2,N2,N0,wK__8t)
   CALL set_ext(np,0,2,2,N0,L2,L2,w_LL8t)
   CALL set_ext(np,2,2,2,L2,K2,K2,wLKK8t)
!---
   CALL set_ext(np,2,2,0,L2,N2,N0,wL__8t)
   CALL set_ext(np,0,2,2,N0,K2,K2,w_KK8t)
   CALL set_ext(np,2,2,2,K2,L2,L2,wKLL8t)

   CALL set_ext(np,0,2,0,N0,L2,N0,w_L_8t)
   CALL set_ext(np,0,2,0,N0,K2,N0,w_K_8t)
CASE default
   PRINT '(" In set_cpt_wexts; compact operation code not recognized:")'
   WRITE(*,*)ch
END SELECT

END SUBROUTINE set_cpt_wexts
     
!============================================================================
SUBROUTINE set_cpt_consts
!----------------------------------------------------------------------------
! Set up the constants used for various constant-coefficient compact schemes
! for uniform grids. The recursive coefficients are obtained in each case
! by performing an L-D-U decomposition of an effectively infinite banded
! symmetric convolution matrix representing the operator that appears on
! the left-hand-side of the definition of the compact scheme.
!---------------------------------------------------------------------------- 
!USE MODULE_cpt_constd
IMPLICIT NONE
INTEGER, PARAMETER:: noq=3
REAL(8) :: s0,s1,s2,a0,a1,a2,sl1,sl2,su1,su2,al1,al2,au1,au2
! Coefficients for 4th-order midpoint-interpolation:
s0=6; s1=1;                      CALL ldlcc1(s0,s1,a0,l4m1_d); b4m_d=4/a0

! Coefficients for 6th-order midpoint-interpolation:
s0=20; s1=6;                     CALL ldlcc1(s0,s1,a0,l6m1_d)
s1=1/a0; s0=15/a0-s1;            CALL ldlcc1(s0,s1,b6m_d,k6m1_d)

! Coefficients for 8th-order midpoint-interpolation:
s0=70; s1=28; s2=1;              CALL ldlcc2(s0,s1,s2,a0,l8m1_d,l8m2_d)
s1=8/a0; s0=56/a0-s1;            CALL ldlcc1(s0,s1,b8m_d,k8m1_d)

! Coefficients for 10th-order midpoint-interpolation:
s0=252; s1=120; s2=10;           CALL ldlcc2(s0,s1,s2,a0,l10m1_d,l10m2_d)
s2=1/a0;s1=45/a0-s2;s0=210/a0-s1;CALL ldlcc2(s0,s1,s2,b10m_d,k10m1_d,k10m2_d)

! Coefficients for 4th-order differencing, same grid:
s0=4; s1=1;                      CALL ldlcc1(s0,s1,a0,l4d1_d); 
b4d_d=3/a0; b4d1_d=b4d_d

! Coefficients for 8th-order differencing, same grid:
s0=216; s1=96; s2=6;             CALL ldlcc2(s0,s1,s2,a0,l8d1_d,l8d2_d)
b8d1_d=160/a0; b8d2_d=25/a0
s0=b8d1_d; s1=b8d2_d;            CALL ldlcc1(s0,s1,b8d_d,k8d1_d)


! 4th-order invertible quadrature/differencing, grids d -> c/c -> d:
s0=22; s1=1;                     CALL ldlcc1(s0,s1,a0,l4q1_d)
b4qi_d=24/a0; b4q_d=1/b4qi_d

! 6th-order invertible quadrature/differencing, grids d -> c/c -> d:
s0=62; s1=9;                     CALL ldlcc1(s0,s1,a0,l6q1_d)
s1=17/(3*a0); s0=63/a0+s1;       CALL ldlcc1(s0,s1,b6qi_d,k6q1_d); 
b6q_d=1/b6qi_d

! 8th-order invertible quadrature/differencing, grids d -> c/c -> d:
s0=51338; s1=12228; s2=183;      CALL ldlcc2(s0,s1,s2,a0,l8q1_d,l8q2_d)
s1=29360/(3*a0); s0=46800/a0+s1; CALL ldlcc1(s0,s1,b8qi_d,k8q1_d); 
b8q_d=1/b8qi_d

! 10th-order invertible quadrature/differencing, grids d -> c/c -> d:
s0=1731174; s1=581100; s2=29025; CALL ldlcc2(s0,s1,s2,a0,l10q1_d,l10q2_d)
s2=69049/(5*a0); s1=1515525/(3*a0)+s2; 
s0=1366850/a0+s1;               CALL ldlcc2(s0,s1,s2,b10qi_d,k10q1_d,k10q2_d) 
b10q_d=1/b10qi_d

! 4th-order invertible midpoint J.L.McGregor translation:
sl1=1; s0=10; su1=5;         CALL lducc1(sl1,s0,su1,l4t1_d,a0,k4t1_d)

! 8th-order invertible midpoint J.L.McGregor translation:
sl2=1; sl1=36; s0=126; su1=84; su2=9
CALL lducc2(sl2,sl1,s0,su1,su2,l8t2_d,l8t1_d,a0,k8t1_d,k8t2_d)

! Copy to ordinary precision counterparts:
b4m  =b4m_d;                  l4m1 =l4m1_d
b6m  =b6m_d;                  k6m1 =k6m1_d;  
                              l6m1 =l6m1_d
b8m  =b8m_d;                  k8m1 =k8m1_d;  
                              l8m1 =l8m1_d;  l8m2 =l8m2_d
b10m =b10m_d;                 k10m1=k10m1_d; k10m2=k10m2_d; 
                              l10m1=l10m1_d; l10m2=l10m2_d
b4d=b4d_d; b4d1 =b4d1_d;      l4d1 =l4d1_d
b8d  =b8d_d;                  k8d1 =k8d1_d
b8d1 =b8d1_d;  b8d2 =b8d2_d;  l8d1 =l8d1_d;  l8d2 =l8d2_d
b4qi =b4qi_d;  b4q  =b4q_d;   l4q1 =l4q1_d
b6qi =b6qi_d;  b6q  =b6q_d;   k6q1 =k6q1_d;  l6q1 =l6q1_d
b8qi =b8qi_d;  b8q  =b8q_d;   k8q1 =k8q1_d;  l8q1 =l8q1_d;  l8q2 =l8q2_d
b10qi=b10qi_d; b10q =b10q_d;  k10q1=k10q1_d; k10q2=k10q2_d; 
                              l10q1=l10q1_d; l10q2=l10q2_d  
                              l4t1=l4t1_d; 
                              k4t1=k4t1_d
                              l8t1=l8t1_d;   l8t2=l8t2_d; 
                              k8t1=k8t1_d;   k8t2=k8t2_d
! Compute safe fade-away margins for decay tails of recursions
! at both single (...) and double (...d) precision:
CALL decays1(l4m1,       fl4m, fl4md)
CALL decays1(l6m1,       fl6m, fl6md)
CALL decays2(l8m1,l8m2,  fl8m, fl8md)
CALL decays2(l10m1,l10m2,fl10m,fl10md)
CALL decays1(l4d1,       fl4d, fl4dd)
CALL decays2(l8d1, l8d2, fl8d, fl8dd)
                                       CALL decays1(l4q1,       fl4q, fl4qd)
CALL decays1(k6q1,       fk6q, fk6qd); CALL decays1(l6q1,       fl6q, fl6qd)
CALL decays1(k8q1,       fk8q, fk8qd); CALL decays2(l8q1, l8q2, fl8q, fl8qd)
CALL decays2(k10q1,k10q2,fk10q,fk10qd);CALL decays2(l10q1,l10q2,fl10q,fl10qd)
CALL decays1(l4t1,       fl4t, fl4td); CALL decays1(k4t1,       fk4t, fk4td)
CALL decays2(l8t1, l8t2, fl8t, fl8td); CALL decays2(k8t1, k8t2, fk8t, fk8td)

! Set default coefficients for boundary extrapolation operators:
CALL set_cpt_wextsall(1)

END SUBROUTINE set_cpt_consts

!END MODULE MODULE_cpt_consts

SUBROUTINE ldlcc1(s0,s1,a0,a1)
  IMPLICIT NONE
  REAL(8),INTENT(IN)     :: s0,s1
  REAL(8),INTENT(OUT)    :: a0,a1
  REAL(8),DIMENSION(0:1) :: s,a
  s(0)=s0; s(1)=s1; CALL ldlcc(s,a,1); a0=a(0); a1=a(1)
END SUBROUTINE ldlcc1

SUBROUTINE ldlcc2(s0,s1,s2,a0,a1,a2)
  IMPLICIT NONE
  REAL(8),INTENT(IN)     :: s0,s1,s2
  REAL(8),INTENT(OUT)    :: a0,a1,a2
  REAL(8),DIMENSION(0:2) :: s,a
  s(0)=s0; s(1)=s1; s(2)=s2; CALL ldlcc(s,a,2); a0=a(0); a1=a(1); a2=a(2)
END SUBROUTINE ldlcc2

SUBROUTINE ldlcc3(s0,s1,s2,s3,a0,a1,a2,a3)
  IMPLICIT NONE
  REAL(8),INTENT(IN)     :: s0,s1,s2,s3
  REAL(8),INTENT(OUT)    :: a0,a1,a2,a3
  REAL(8),DIMENSION(0:3) :: s,a
  s(0)=s0; s(1)=s1; s(2)=s2; s(3)=s3;
  CALL ldlcc(s,a,3)
  a0=a(0); a1=a(1); a2=a(2); a3=a(3)
END SUBROUTINE ldlcc3

SUBROUTINE lducc1(sl1,s0,su1,al1,a0,au1)
  IMPLICIT NONE
  REAL(8),INTENT(IN)     :: sl1,s0,su1
  REAL(8),INTENT(OUT)    :: al1,a0,au1
  REAL(8),DIMENSION(-1:1):: s,a
  s(-1)=sl1; s(0)=s0; s(1)=su1; 
  CALL lducc(s,a,1); 
  al1=a(-1); a0=a(0); au1=a(1)
END SUBROUTINE lducc1

SUBROUTINE lducc2(sl2,sl1,s0,su1,su2,al2,al1,a0,au1,au2)
  IMPLICIT NONE
  REAL(8),INTENT(IN)     :: sl2,sl1,s0,su1,su2
  REAL(8),INTENT(OUT)    :: al2,al1,a0,au1,au2
  REAL(8),DIMENSION(-2:2):: s,a
  s(-2)=sl2; s(-1)=sl1; s(0)=s0; s(1)=su1; s(2)=su2 
  CALL lducc(s,a,2); 
  al2=a(-2); al1=a(-1); a0=a(0); au1=a(1); au2=a(2)
END SUBROUTINE lducc2

SUBROUTINE ldlcc(s,a,noq)
!============================================================================
! Perform the L-D-L^t decomposition of a symmetric convolution, by treating
! the convolution as a semi-infinite banded matrix and applying the
! L-D-L^t variant (no square-roots) of the Cholesky method.
! --> s: the non-negative half of the symmetric convolution coefficients.
! <-- a: the diagonal, D [a(0)], and the components of the Cholesky 
!        factor, L [a(1:noq)]. The main diagonal of L is assumed to be 1~s.
!============================================================================
IMPLICIT NONE
INTEGER,                   INTENT(IN) :: noq
REAL(8), DIMENSION(0:noq), INTENT(IN) :: s
REAL(8), DIMENSION(0:noq), INTENT(OUT):: a
!----------------------------------------------------------------------------
INTEGER, PARAMETER                    :: ncy=500
INTEGER                               :: noqp,i,j,k,icy
REAL(8)                               :: tt,dif,difo
REAL(8), DIMENSION(0:noq,-noq:0)      :: t
!============================================================================
t=0.
noqp=noq+1
DO i=0,noq
   t(i,0)=1.
   DO j=-i,0
      tt=s(-j)
      DO k=-i,j-1
         tt=tt-t(i,k)*t(i+j,k-j)*t(i+k,0)
      ENDDO
      t(i,j)=tt/t(i+j,0)
   ENDDO
ENDDO
a(:)=t(noq,0:-noq:-1)
difo=0.
DO icy=1,ncy
   DO i=0,noq
      t(i,0)=1.
      DO j=-noq,0
         tt=s(-j)
         DO k=-noq,j-1
            tt=tt-t(i,k)*t(MOD(noqp+i+j,noqp),k-j) &
                 *t(MOD(noqp+i+k,noqp),0)
         ENDDO
         t(i,j)=tt/t(MOD(noqp+i+j,noqp),0)
      ENDDO
   ENDDO
   dif=MAXVAL(ABS(a-t(noq,0:-noq:-1)))
   IF(icy > 3 .AND. dif >= difo)THEN
      RETURN
   ENDIF
   difo=dif
   a(:)=t(noq,0:-noq:-1)
ENDDO
PRINT '(" no convergence obtained after",i5," iterations")',ncy
PRINT '(" Final magnitude of dif: ",e12.6)',dif
END SUBROUTINE ldlcc

!============================================================================
SUBROUTINE lducc(a,ldu,noq)
!============================================================================
!  Factorization of asymmetrical convolution. Regard each convolution
!  operator as an infinite circulant (diagonally-constant) matrix.
! --> a:   Representative row of input convolution operator.
! <-- ldu: The nontrivial components, in their proper places,
!           of the L-D-U decompostion of a, where the (trivial) main diagonal
!           components of L and of U are all ones.
!============================================================================
IMPLICIT NONE
INTEGER,                    INTENT(IN ) :: noq
REAL(8),DIMENSION(-noq:noq),INTENT(IN ) :: a
REAL(8),DIMENSION(-noq:noq),INTENT(OUT) :: ldu
!----------------------------------------------------------------------------
INTEGER, PARAMETER:: ncy=500
INTEGER:: noqp,nohp,i,j,k,id,jd,kd,nd
REAL(8):: t
REAL(8),DIMENSION(0:noq*2,0:noq*2):: b
!============================================================================
nohp=noq*2+1
noqp=noq+1
nd=ncy/nohp
nd=nd*nohp-1
DO kd=0,nd; k=MOD(kd,nohp)

! New lower-band elements:
   DO jd=MAX(0,kd-noq),kd-1; j=MOD(jd,nohp)
      t=0
      DO id=MAX(0,kd-noq),jd-1; i=MOD(id,nohp)
         t=t+b(k,i)*b(i,i)*b(i,j)
      ENDDO
      b(k,j)=(a(jd-kd)-t)/b(j,j)
   ENDDO

! New upper-band elements:
   DO id=MAX(0,kd-noq),kd-1; i=MOD(id,nohp)
      t=0
      DO jd=MAX(0,kd-noq),id-1; j=MOD(jd,nohp)
         t=t+b(i,j)*b(j,j)*b(j,k)
      ENDDO
      b(i,k)=(a(kd-id)-t)/b(i,i)
   ENDDO

! New diagonal element:
      t=0
      DO id=MAX(0,kd-noq),kd-1; i=MOD(id,nohp)
         t=t+b(k,i)*b(i,i)*b(i,k)
      ENDDO
      b(k,k)=(a(0)-t)
ENDDO

DO i=1,noq
   ldu(-i)=b(i,0) ! elements of L below the diagonal
   ldu(i) =b(0,i) ! elements of U above the diagonal
ENDDO
ldu(0)=b(0,0)     ! the constant diagonal element.
END SUBROUTINE lducc

SUBROUTINE decays1(a1,fs,fd)
IMPLICIT NONE
REAL,   INTENT(IN) :: a1
INTEGER,INTENT(OUT):: fs,fd
REAL,DIMENSION(1):: a
a(1)=a1; CALL decays(1,a,fs,fd)
END SUBROUTINE decays1

SUBROUTINE decays2(a1,a2,fs,fd)
IMPLICIT NONE
REAL,   INTENT(IN) :: a1,a2
INTEGER,INTENT(OUT):: fs,fd
REAL,DIMENSION(2)  :: a
a(1)=a1; a(2)=a2; CALL decays(2,a,fs,fd)
END SUBROUTINE decays2

SUBROUTINE decays(m,a,fs,fd)
IMPLICIT NONE
INTEGER,          INTENT(IN) :: m
REAL,DIMENSION(m),INTENT(IN) :: a
INTEGER,          INTENT(OUT):: fs,fd
INTEGER,PARAMETER:: nn=200
INTEGER:: i
REAL,DIMENSION(1-m:nn):: p
REAL:: s,d
s=EPSILON(1.)
d=EPSILON(1.d0)
p=0
p(0)=1
DO i=1,nn
   p(i)=-dot_PRODUCT(a(1:m),p(i-1:i-m:-1))
ENDDO
p(nn)=ABS(p(nn))
DO i=nn-1,0,-1
   p(i)=p(i+1)+ABS(p(i))
ENDDO
DO i=0,nn,m
   fs=i
   IF(p(i)<s)EXIT
ENDDO
DO i=fs,nn,m
   fd=i
   IF(p(i)<d)EXIT
ENDDO
END SUBROUTINE decays

