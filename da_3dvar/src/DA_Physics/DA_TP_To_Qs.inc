SUBROUTINE DA_TP_To_Qs( t, p, es, qs)

!------------------------------------------------------------------------------
!  PURPOSE: Convert T/p to saturation specific humidity.
!
!  METHOD: qs = es_alpha * es / ( p - ( 1 - rd_over_rv ) * es ).
!          Use Rogers & Yau (1989) formula: es = a exp( bTc / (T_c + c) ).
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!  MODIFIED: 10/01/2002					Wei Huang
!------------------------------------------------------------------------------

   IMPLICIT NONE

   real, intent(in)  :: t, p
   real, intent(out) :: es, qs
   
   REAL              :: t_c              ! T in degreesC.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   t_c = t - t_kelvin
   
!------------------------------------------------------------------------------
!  [2.0] Calculate saturation vapour pressure:
!------------------------------------------------------------------------------

   es = es_alpha * exp( es_beta * t_c / ( t_c + es_gamma ) )
   
!------------------------------------------------------------------------------
!  [3.0] Calculate saturation specific humidity:
!------------------------------------------------------------------------------

   qs = rd_over_rv * es / ( p - rd_over_rv1 * es )

END SUBROUTINE DA_TP_To_Qs

SUBROUTINE DA_TP_To_Qs1( xb, xp, es, qs)

!------------------------------------------------------------------------------
!  PURPOSE: Convert T/p to saturation specific humidity.
!
!  METHOD: qs = es_alpha * es / ( p - ( 1 - rd_over_rv ) * es ).
!          Use Rogers & Yau (1989) formula: es = a exp( bTc / (T_c + c) ).
!
!  This DA_TP_To_Qs1 was added and called by the corrected subroutine
!       DA_TPQ_To_RH_Lin.
!
!  10/30/2003                                               Yong-Run Guo
!------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)    :: xb           ! First guess structure.
   TYPE (xpose_type), INTENT(IN) :: xp           ! Dimensions and xpose buffers.
   REAL, dimension(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte), INTENT(out):: &
                                    es, qs

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.
   integer                       :: ks, ke       ! 3rd dim. end points.
   integer                       :: i, j, k      ! Loop counters.
   real                          :: t_c          ! Working variable.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   is = xp%its; ie = xp%ite
   js = xp%jts; je = xp%jte
   ks = xp%kts; ke = xp%kte      

   do k = ks, ke
      do j = js, je
         do i = is, ie

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

         t_c = xb % t(i,j,k) - t_kelvin
   
!------------------------------------------------------------------------------
!  [2.0] Calculate saturation vapour pressure:
!------------------------------------------------------------------------------

         es(i,j,k) = es_alpha * exp( es_beta * t_c / ( t_c + es_gamma ) )
   
!------------------------------------------------------------------------------
!  [3.0] Calculate saturation specific humidity:
!------------------------------------------------------------------------------

         qs(i,j,k) = rd_over_rv * es(i,j,k) / &
                     (xb % p(i,j,k) - rd_over_rv1 * es(i,j,k))

         end do
      end do
   end do

END SUBROUTINE DA_TP_To_Qs1

