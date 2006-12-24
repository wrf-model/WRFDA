subroutine da_tp_to_qs1( xb, xp, es, qs)

   !---------------------------------------------------------------------------
   !  Purpose: Convert T/p to saturation specific humidity.
   !
   !  METHOD: qs = es_alpha * es / ( p - ( 1 - rd_over_rv ) * es ).
   !          use Rogers & Yau (1989) formula: es = a exp( bTc / (T_c + c) ).
   !
   !  This da_tp_to_qs1 was added and called by the corrected subroutine
   !       da_tpq_to_rh_lin.
   !---------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)    :: xb           ! First guess structure.
   type (xpose_type), intent(in) :: xp           ! Dimensions and xpose buffers.
   real, dimension(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte), intent(out):: &
                                    es, qs

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.
   integer                       :: ks, ke       ! 3rd dim. end points.
   integer                       :: i, j, k      ! Loop counters.
   real                          :: t_c          ! Working variable.

   !---------------------------------------------------------------------------
   ! [1.0] Initialise:
   !---------------------------------------------------------------------------

   is = xp%its; ie = xp%ite
   js = xp%jts; je = xp%jte
   ks = xp%kts; ke = xp%kte      

   do k = ks, ke
      do j = js, je
         do i = is, ie

            !------------------------------------------------------------------
            ! [1.0] Initialise:
            !------------------------------------------------------------------

            t_c = xb % t(i,j,k) - t_kelvin
   
            !------------------------------------------------------------------
            ! [2.0] Calculate saturation vapour pressure:
            !------------------------------------------------------------------

            es(i,j,k) = es_alpha * exp( es_beta * t_c / ( t_c + es_gamma ) )
   
            !------------------------------------------------------------------
            ! [3.0] Calculate saturation specific humidity:
            !------------------------------------------------------------------

            qs(i,j,k) = rd_over_rv * es(i,j,k) / &
                     (xb % p(i,j,k) - rd_over_rv1 * es(i,j,k))

         end do
      end do
   end do

end subroutine da_tp_to_qs1


