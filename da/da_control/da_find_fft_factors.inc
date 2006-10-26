subroutine da_find_fft_factors( n, n_ok, fft_factors )

   !---------------------------------------------------------------------------
   ! PURPOSE: Calculates prime factors of input number.
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN)        :: n
   LOGICAL, INTENT(OUT)       :: n_ok
   INTEGER, INTENT(OUT)       :: fft_factors(:)

   INTEGER                    :: i, k, l
   INTEGER                    :: nfax, nu, ifac
   INTEGER                    :: jfax(num_fft_factors)
   INTEGER                    :: lfax(7)

   DATA lfax /6,8,5,4,3,2,1/

   !---------------------------------------------------------------------------
   ! [1.0] Find factors of vector size (8,6,5,4,3,2; ONLY ONE 8 ALLOWED):
   !---------------------------------------------------------------------------

   n_ok = .FALSE.
   fft_factors(:) = 0

   ! LOOK FOR SIXES FIRST, STORE FACTORS IN DESCENDING ORDER
   NU=N
   IFAC=6
   K=0
   L=1

20 CONTINUE

   IF (MOD(NU,IFAC).NE.0) GO TO 30
   
   ! 6 is a factor:
   K=K+1
   JFAX(K)=IFAC
   IF (IFAC.NE.8) GO TO 25
   IF (K.EQ.1) GO TO 25
   JFAX(1)=8
   JFAX(K)=6

25 CONTINUE
   NU=NU/IFAC
   IF (NU.EQ.1) GO TO 50
   IF (IFAC.NE.8) GO TO 20

30 CONTINUE
   L=L+1
   IFAC=LFAX(L)
   IF (IFAC .GT. 1) GO TO 20

   ! Illegal factors:
   ! write (UNIT=stderr,FMT='(a,i4,a)') 'N = ', N, ' contains illegal factors.'
   
   GOTO 9

   ! NOW REVERSE ORDER OF FACTORS
50 CONTINUE
   NFAX=K
   fft_factors(1)=NFAX
   DO I=1,NFAX
      fft_factors(NFAX+2-I)=JFAX(I)
   END DO
   
   n_ok = .TRUE.
      
9  CONTINUE

end subroutine da_find_fft_factors
