SUBROUTINE DA_TPRH_To_Q_Adj( xb, xp, xa )

!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of DA_TPRH_To_Q_Adj.
!
!  METHOD: Standard adjoint coding.
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)    :: xb           ! First guess structure.
   TYPE (xpose_type), INTENT(IN) :: xp           ! Dimensions and xpose buffers.
   TYPE (x_type), INTENT(INOUT)  :: xa           ! Increment structure

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.
   integer                       :: ks, ke       ! 3rd dim. end points.
   INTEGER                       :: i, j, k      ! Loop counter.
   REAL :: qs_prime_over_qs(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte) ! Temp.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   is = xp%its; ie = xp%ite
   js = xp%jts; je = xp%jte
   ks = xp%kts; ke = xp%kte   

   if ( Testing_3DVAR ) then
      is = xb%its-1
      js = xb%jts-1

      ie = xb%ite+1
      je = xb%jte+1

      if ( is < xb%ids ) is = xb%ids
      if ( js < xb%jds ) js = xb%jds

      if ( ie > xb%ide ) ie = xb%ide
      if ( je > xb%jde ) je = xb%jde
   endif

!------------------------------------------------------------------------------
!  [2.0] Calculate relative humidity increment:
!------------------------------------------------------------------------------

   do k = ks, ke
      do j = js, je
         do i = is, ie

            qs_prime_over_qs(i,j,k) = xb % q(i,j,k) * xa % q(i,j,k)

            xa % rh(i,j,k) = xa % rh(i,j,k) + qs_prime_over_qs(i,j,k) / &
                             xb % rh(i,j,k)
         end do
      end do
   end do

!------------------------------------------------------------------------------
!  [2.0] Calculate saturation specific humidity ratio qs~/qs:
!------------------------------------------------------------------------------

   CALL DA_TP_To_Qs_Adj( xb, xp, xa, qs_prime_over_qs )

END SUBROUTINE DA_TPRH_To_Q_Adj

SUBROUTINE DA_TPRH_To_Q_Adj1( t, p, es, q, rh, &
                              t_prime, p_prime, rh_prime, q_prime )

!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of DA_TPRH_To_Q_Adj.
!
!  METHOD: Standard adjoint coding.
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE

   REAL, INTENT(IN)              :: t        ! Temperature.
   REAL, INTENT(IN)              :: es       ! Saturation vapour pressure.
   REAL, INTENT(IN)              :: p        ! Pressure.
   REAL, INTENT(IN)              :: q        ! Specific humidity.
   REAL, INTENT(IN)              :: rh       ! Relative Humidity.
   REAL, INTENT(INOUT)           :: t_prime  ! Temperature increment.
   REAL, INTENT(INOUT)           :: p_prime  ! Pressure increment.
   REAL, INTENT(INOUT)           :: rh_prime ! Pressure increment.
   REAL, INTENT(IN)              :: q_prime  ! Pressure increment.
   
   REAL                          :: temp     ! Temporary storage.
   REAL                          :: qs_prime_over_qs ! qs~/qs.

  temp = q * q_prime

!------------------------------------------------------------------------------
!  [2.0] Calculate relative humidity increment:
!------------------------------------------------------------------------------

  rh_prime = rh_prime + temp / rh
  qs_prime_over_qs = temp

!------------------------------------------------------------------------------
!  [1.0] Calculate saturation specific humidity ratio qs~/qs:
!------------------------------------------------------------------------------

  CALL DA_TP_To_Qs_Adj1( t, p, es, t_prime, p_prime, qs_prime_over_qs )

END SUBROUTINE DA_TPRH_To_Q_Adj1

!SUBROUTINE DA_TPRH_To_Q_Adj( t, p, es, q, rh, &
!                             t_prime, p_prime, rh_prime, q_prime, n )

!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of DA_TPRH_To_Q_Adj.
!
!  METHOD: Standard adjoint coding.
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

!   IMPLICIT NONE

!   INTEGER        i, n
!   REAL           t, es, p, q, rh,t_prime, p_prime, rh_prime, q_prime  
!   dimension      t       (n) ! Temperature.
!   dimension      es      (n) ! Saturation vapour pressure.
!   dimension      p       (n) ! Pressure.
!   dimension      q       (n) ! Specific humidity.
!   dimension      rh      (n) ! Relative Humidity.
!   dimension      t_prime (n) ! Temperature increment.
!   dimension      p_prime (n) ! Pressure increment.
!   dimension      rh_prime(n) ! Pressure increment.
!   dimension      q_prime (n) ! Pressure increment.
   
!   real        temp, qs_prime_over_qs  ! Temporary storage.
!   dimension   qs_prime_over_qs(n)     ! qs~/qs.

!   do i = 1,n
!   temp = q(i) * q_prime(i)

!------------------------------------------------------------------------------
!  [2.0] Calculate relative humidity increment:
!------------------------------------------------------------------------------

!   rh_prime(i) = rh_prime(i) + temp / rh(i)
!   qs_prime_over_qs(i) = temp
!   enddo

!------------------------------------------------------------------------------
!  [1.0] Calculate saturation specific humidity ratio qs~/qs:
!------------------------------------------------------------------------------

!   CALL DA_TP_To_Qs_Adj( t, p, es, t_prime, p_prime, qs_prime_over_qs, n )

!END SUBROUTINE DA_TPRH_To_Q_Adj

