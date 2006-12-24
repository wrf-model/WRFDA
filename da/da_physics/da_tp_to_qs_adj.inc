subroutine da_tp_to_qs_adj( xb, xp, xa, qs_prime_over_qs )

   !---------------------------------------------------------------------------
   ! Purpose: Adjoint of da_tp_to_qs_lin
   !---------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)    :: xb           ! First guess structure.
   type (xpose_type), intent(in) :: xp           ! Dimensions and xpose buffers.
   type (x_type), intent(inout)  :: xa           ! Increment structure.
   real, intent(in) :: qs_prime_over_qs(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte)

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.
   integer                       :: ks, ke       ! 3rd dim. end points.
   integer                       :: i, j, k      ! Loop counters.
   real                          :: temp         ! Temporary array.
   real                          :: es_prime_over_es ! Sat Vap pressure ratio.

   !---------------------------------------------------------------------------
   ! [1.0] Initialise:
   !---------------------------------------------------------------------------

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
   end if

   !---------------------------------------------------------------------------
   ! [3.0] Calculate saturation specific humidity increment:
   !---------------------------------------------------------------------------

   do k = ks, ke
      do j = js, je
         do i = is, ie

            temp = qs_prime_over_qs(i,j,k) / &
                   ( xb % p(i,j,k) - rd_over_rv1 * xb % es(i,j,k) )
   
            es_prime_over_es = temp * xb % p(i,j,k)

            xa % p(i,j,k) = xa % p(i,j,k) - temp
   
   !---------------------------------------------------------------------------
   ! [2.0] Calculate saturation vapour pressure increment:
   !---------------------------------------------------------------------------

            temp = xb % t(i,j,k) + es_gammakelvin

            xa % t(i,j,k) = xa % t(i,j,k) + es_gammabeta * es_prime_over_es / &
                            ( temp * temp )
         end do
      end do
   end do

end subroutine da_tp_to_qs_adj

!subroutine da_tp_to_qs_adj( t, p, es, t_prime, p_prime, &
!                            qs_prime_over_qs, n )

!------------------------------------------------------------------------------
!  Purpose: Adjoint of da_tp_to_qs_lin.
!
!  METHOD: Standard adjoint coding.
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

!   implicit none

!   integer      i, n
!   real         t, p, es, t_prime, p_prime, qs_prime_over_qs
!   dimension    t               (n) ! Temperature.
!   dimension    p               (n) ! Pressure.
!   dimension    es              (n) ! Sat. vapour pressure.
!   dimension    t_prime         (n) ! Temperature increment.
!   dimension    p_prime         (n) ! Pressure increment.
!   dimension    qs_prime_over_qs(n) ! qs~/qs.
   
!   real         temp             ! Temporary storage.
!   real         es_prime_over_es ! es~/es
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
!   end do

!end subroutine da_tp_to_qs_adj


