SUBROUTINE da_get_innov_vector_satem ( it, xb, xp, ob, iv )
!-------------------------------------------------------------------------
!  History: 01/2002 - Creation of F90 version. Qingnong Xiao
!
!
!     Additions:        03/19/2003                    S. R. H. Rizvi
!
!               1. DA_Check_Max_IV is called for each outer iteration
!
!               2. Extra diagnostics added for generating detailed information
!                  about rejected obs in various outer iterations
!
!                       08/30/2005                    Syed RH Rizvi  
!               3. Correction for data holes in thicknesses           
!
!-------------------------------------------------------------------------
   IMPLICIT NONE

   INTEGER, INTENT(IN)             :: it       ! External iteration.
   TYPE (xb_type), INTENT(IN)      :: xb       ! first guess state.
   TYPE (xpose_type), INTENT(IN)   :: xp       ! Domain decomposition vars.
   TYPE (y_type),  INTENT(IN)      :: ob       ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)   :: iv       ! O-B structure.

   INTEGER                         :: n        ! Loop counter.
   INTEGER                         :: i, j, k  ! Index dimension.
   INTEGER                         :: num_levs ! Number of obs levels.
   REAL                            :: dx, dxm  ! Interpolation weights.
   REAL                            :: dy, dym  ! Interpolation weights.
   REAL, DIMENSION(1:max_ob_levels) :: model_thickness !Model thickness at ob loc
  INTEGER           :: ithickness,ithicknessf
!
   REAL, DIMENSION(xp%kts-1:xp%kte+1)   :: pre_ma,tv_ma
   INTEGER                         :: layer1,layer2,ks,ke
   REAL                            :: tv1,tv2,pres2

   IF ( iv % num_satem > 0 ) THEN
     ithickness = 0 ; ithicknessf = 0

     DO n=iv%ob_numb(iv%current_ob_time-1)%satem + 1, iv%ob_numb(iv%current_ob_time)%satem

       if(iv%satem(n)%loc%proc_domain_with_halo) then

         num_levs = iv % satem(n) % info % levels

         if ( num_levs < 1 ) cycle

         model_thickness(:) = 0.0

!        [1.0] Get cross pt. horizontal interpolation weights:

         i = iv%satem(n)%loc%i
         dy = iv%satem(n)%loc%dy
         dym = iv%satem(n)%loc%dym
         j = iv%satem(n)%loc%j
         dx = iv%satem(n)%loc%dx
         dxm = iv%satem(n)%loc%dxm
         ks = xp%kts; ke=xp%kte

!------------------------------------------------------------------------

!        [2.0] Calculate vertical profile of virtual temperature at obs pt.

         call Tv_profile(xp,xb,i,j,dx,dxm,dy,dym,pre_ma,tv_ma)

!        [3.0] Find model vertical position of pressure and do interp.

         call find_layer(layer2,tv2,iv%satem(n)%ref_p,pre_ma,tv_ma,ks,ke)
         pres2 = iv%satem(n)%ref_p

!        [4.0] Thickness innovations calculation
 
         do k = 1, num_levs

            iv % satem(n) % thickness(k) % inv = 0.0

               call find_layer(layer1,tv1,iv%satem(n)%p(k),pre_ma,tv_ma,ks,ke)
               call thickness(pre_ma,tv_ma,ks,ke,tv1,tv2,layer1,layer2,   &
                    iv%satem(n)%p(k),pres2,model_thickness(k))

            IF ( ABS(ob % satem(n) % thickness(k) - missing_r) > 1. .and. &
                 iv % satem(n) % thickness(k)%qc /= missing_data ) THEN


               iv % satem(n) % thickness(k) % inv =     &
                     ob % satem(n) % thickness(k) - model_thickness(k)

!               write(1,"(A, 2F10.3,F10.0,A,F5.0,A)") &
!                    "observed, model_thickness, layer = ", &
!                     ob%satem(n)%thickness(k), &
!                     model_thickness(k), 0.01*pres2, " -", &
!                     iv%satem(n)%p(k)*0.01,'hPa'
            ENDIF

               pres2 = iv%satem(n)%p(k)
               layer2 = layer1
               tv2 = tv1

         END DO

!------------------------------------------------------------------------
!        [5.0] Perform optional maximum error check:
!------------------------------------------------------------------------

    IF ( check_max_iv ) &
    CALL da_check_max_iv_satem(it, iv % satem(n), ithickness,ithicknessf )

!------------------------------------------------------------------------
!        [6.0] Perform land/ocean check
!------------------------------------------------------------------------

         if(xb%landmask(i,j  ) /= 0. .or. xb%landmask(i+1,j  ) /= 0. .or.  &
            xb%landmask(i,j+1) /= 0. .or. xb%landmask(i+1,j+1) /= 0.) then
            iv % satem(n) % thickness(1) % inv = 0.
          ithicknessf = ithicknessf + 1
         endif

       endif

     END DO
  write(unit = check_max_iv_unit, FMT ='(A,i5,A)')'For outer iteration ',it, &
                   ', Total Rejections for Satem follows:'
WRITE (UNIT = check_max_iv_unit, FMT = '(/,10(2(A,I6),/))') &
'Number of failed thicknesses observations:     ',&
                   ithicknessf, ' on ',ithickness


   ENDIF

END SUBROUTINE da_get_innov_vector_satem

