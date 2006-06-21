subroutine da_tprh_to_q_adj( xb, xp, xa )

!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of da_tprh_to_q_adj.
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

   if ( Testing_WRFVAR ) then
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

   call da_tp_to_qs_adj( xb, xp, xa, qs_prime_over_qs )

end subroutine da_tprh_to_q_adj

!subroutine da_tprh_to_q_adj( t, p, es, q, rh, &
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

!   call da_tp_to_qs_adj( t, p, es, t_prime, p_prime, qs_prime_over_qs, n )

!end subroutine da_tprh_to_q_adj

