subroutine da_tp_to_qs_lin( xb, xp, xa, qs_prime_over_qs )

!------------------------------------------------------------------------------
!  PURPOSE: Convert es/p/es_prime to saturation specific humidity increment.
!
!  METHOD: qs~ = qs * ( p es'/es - p' ) / ( p - (1-rd_over_rv) es ).
!          use Rogers & Yau (1989) formula: es = a exp( bTc / (T_c + c) ).
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!           02/21/2002 - Modified for parallel version.     Al Bourgeois
!------------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)    :: xb           ! First guess structure.
   type (xpose_type), intent(in) :: xp           ! Dimensions and xpose buffers.
   type (x_type), intent(inout)  :: xa           ! Increment structure.
   real, intent(out) :: qs_prime_over_qs(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte)

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

   do k = ks, ke
      do j = js, je
         do i = is, ie

            temp = xb % t(i,j,k) + es_gammakelvin
   
!------------------------------------------------------------------------------
!  [2.0] Calculate saturation vapour pressure increment:
!------------------------------------------------------------------------------

            es_prime_over_es = es_gammabeta * xa % t(i,j,k) / ( temp * temp )

!------------------------------------------------------------------------------
!  [3.0] Calculate saturation specific humidity increment:
!------------------------------------------------------------------------------

            qs_prime_over_qs(i,j,k) = ( xb % p(i,j,k) * es_prime_over_es - &
                                        xa % p(i,j,k) ) / &
                                      ( xb % p(i,j,k) - rd_over_rv1 * &
                                        xb % es(i,j,k) )
         end do
      end do
   end do

end subroutine da_tp_to_qs_lin


