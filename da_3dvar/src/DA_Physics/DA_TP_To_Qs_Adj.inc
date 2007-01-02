SUBROUTINE DA_TP_To_Qs_Adj( xb, xp, xa, qs_prime_over_qs )

!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of DA_TP_To_Qs_Lin.
!
!  METHOD: Standard adjoint coding.
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)    :: xb           ! First guess structure.
   TYPE (xpose_type), INTENT(IN) :: xp           ! Dimensions and xpose buffers.
   TYPE (x_type), INTENT(INOUT)  :: xa           ! Increment structure.
   REAL, INTENT(IN) :: qs_prime_over_qs(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte)

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.
   integer                       :: ks, ke       ! 3rd dim. end points.
   integer                       :: i, j, k      ! Loop counters.
   real                          :: temp         ! Temporary array.
   real                          :: es_prime_over_es ! Sat Vap pressure ratio.

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
!  [3.0] Calculate saturation specific humidity increment:
!------------------------------------------------------------------------------

   do k = ks, ke
      do j = js, je
         do i = is, ie

            temp = qs_prime_over_qs(i,j,k) / &
                   ( xb % p(i,j,k) - rd_over_rv1 * xb % es(i,j,k) )
   
            es_prime_over_es = temp * xb % p(i,j,k)

            xa % p(i,j,k) = xa % p(i,j,k) - temp
   
!------------------------------------------------------------------------------
!  [2.0] Calculate saturation vapour pressure increment:
!------------------------------------------------------------------------------

            temp = xb % t(i,j,k) + es_gammakelvin

            xa % t(i,j,k) = xa % t(i,j,k) + es_gammabeta * es_prime_over_es / &
                            ( temp * temp )
         end do
      end do
   end do

END SUBROUTINE DA_TP_To_Qs_Adj

SUBROUTINE DA_TP_To_Qs_Adj1( t, p, es, t_prime, p_prime, &
                             qs_prime_over_qs )

!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of DA_TP_To_Qs_Lin.
!
!  METHOD: Standard adjoint coding.
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   REAL, INTENT(IN)              :: t                ! Temperature.
   REAL, INTENT(IN)              :: p                ! Pressure.
   REAL, INTENT(IN)              :: es               ! Sat. vapour pressure.
   REAL, INTENT(INOUT)           :: t_prime          ! Temperature increment.
   REAL, INTENT(INOUT)           :: p_prime          ! Pressure increment.
   REAL, INTENT(IN)              :: qs_prime_over_qs ! qs~/qs.
   
   REAL                          :: temp             ! Temporary storage.
   REAL                          :: es_prime_over_es ! es~/es
      
!------------------------------------------------------------------------------
!  [3.0] Calculate saturation specific humidity increment:
!------------------------------------------------------------------------------

   temp = qs_prime_over_qs / ( p - rd_over_rv1 * es )
   
   es_prime_over_es = temp * p

   p_prime = p_prime - temp
   
!------------------------------------------------------------------------------
!  [2.0] Calculate saturation vapour pressure increment:
!------------------------------------------------------------------------------

   temp = t + es_gammakelvin

   t_prime = t_prime + es_gammabeta * es_prime_over_es / ( temp * temp )

END SUBROUTINE DA_TP_To_Qs_Adj1

!SUBROUTINE DA_TP_To_Qs_Adj( t, p, es, t_prime, p_prime, &
!                            qs_prime_over_qs, n )

!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of DA_TP_To_Qs_Lin.
!
!  METHOD: Standard adjoint coding.
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

!   IMPLICIT NONE

!   integer      i, n
!   real         t, p, es, t_prime, p_prime, qs_prime_over_qs
!   dimension    t               (n) ! Temperature.
!   dimension    p               (n) ! Pressure.
!   dimension    es              (n) ! Sat. vapour pressure.
!   dimension    t_prime         (n) ! Temperature increment.
!   dimension    p_prime         (n) ! Pressure increment.
!   dimension    qs_prime_over_qs(n) ! qs~/qs.
   
!   REAL         temp             ! Temporary storage.
!   REAL         es_prime_over_es ! es~/es
!      
!   do i = 1,n
!------------------------------------------------------------------------------
!  [3.0] Calculate saturation specific humidity increment:
!------------------------------------------------------------------------------

!   temp = qs_prime_over_qs(i) / ( p(i) - rd_over_rv1 * es(i) )
  ! 
!   es_prime_over_es = temp * p(i)

!   p_prime(i) = p_prime(i) - temp
   
!------------------------------------------------------------------------------
!  [2.0] Calculate saturation vapour pressure increment:
!------------------------------------------------------------------------------

!   temp = t(i) + es_gammakelvin

!   t_prime(i) = t_prime(i) + es_gammabeta * es_prime_over_es / ( temp * temp )
!   enddo

!END SUBROUTINE DA_TP_To_Qs_Adj

