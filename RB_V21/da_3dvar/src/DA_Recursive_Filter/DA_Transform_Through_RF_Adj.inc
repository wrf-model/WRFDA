SUBROUTINE DA_Transform_Through_RF_Adj( mz, grid_box_area, rf_alpha, val,&
                                        xp, field, &
                                        ids,ide, jds,jde, kds,kde,  &
                                        ims,ime, jms,jme, kms,kme,  &
                                        its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Control variable transform through the recursive filter.
!
!  METHOD: 1) Transform to nondimensional v_hat space.
!          2) Perform recursive filter in non-dimensional space (ds = 1).
!          3) Scale recursive filter output.
!          4) Transform filtered field to dimensional space.
!          5) Optionally scale by background error.
!
!  HISTORY: 08/07/2000 - Creation of F90 version.           Dale Barker
!           02/27/2003 - Order of the filter operation      S. R. H. Rizvi
!                        was reversed  
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in) :: mz                ! Vertical truncation.
   integer, intent(in) :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in) :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in) :: its,ite, jts,jte, kts,kte ! tile   dims.

   real, intent(in)    :: grid_box_area(ims:ime,jms:jme) ! From first guess. 
   real, intent(in)    :: rf_alpha(mz)      ! RF scale parameter. 
   real, intent(in)    :: val(jts:jte,mz)   ! Error standard deviation.
   real, intent(inout) :: field(ims:ime,jms:jme,kms:kme) ! Field to be transformed. 

   type (xpose_type), intent(inout) :: xp   ! Dimensions and xpose buffers.
      
   integer                 :: rf_passes_over_two ! rf_passes / 2
   integer                 :: i, j, m, n, pass   ! Loop counters.
   real                    :: p_x(ims:ime,jms:jme)! SQRT(Grid box area).

   real                    :: val_j(xp%jtsy:xp%jtey)

!-------------------------------------------------------------------------
!  [1.0]: Initialise:
!-------------------------------------------------------------------------  

   rf_passes_over_two = rf_passes / 2

!  [1.1] Define inner product (square root of grid box area):
   p_x(its:ite,jts:jte) = SQRT( grid_box_area(its:ite,jts:jte) )

!-------------------------------------------------------------------------
!  [4.0]: Perform 1D recursive filter in y-direction:
!-------------------------------------------------------------------------
   
!  [4.3] Optionally scale by background error:

!  be_s % val = Gridpoint standard deviation - only required for
!  vert_corr = 1 as scaling is performed in vertical transform
!  for vert_corr = 2:

   IF ( vert_corr == 1 ) THEN
      do m = 1, mz
      DO i = its, ite
         field(i,jts:jte,m) = field(i,jts:jte,m) * val(jts:jte,m)
      END DO
      END DO
   END IF

!  [4.2] Transform filtered field to dimensional space:

   do m = 1, mz
      xp % v1z(its:ite,jts:jte,m) = field(its:ite,jts:jte,m) * &
                                    p_x(its:ite,jts:jte)
   end do

!  [4.1] Apply (i',j',k -> i',j,k' ) (xp % v1z -> xp % v1y)
!--convert vertical column to y-stripe

   call transpose_z2y ( xp )

!-------------------------------------------------------------------------
!  [3.0]: Perform 1D recursive filter in y-direction:
!-------------------------------------------------------------------------

!  [3.2] Apply 1D filter in y direction:

   n=xp%jtey-xp%jtsy+1
   do m = xp%ktsy, min(xp%ktey, mz)
      do i = xp%itsy, xp%itey
         val_j(xp%jtsy:xp%jtey) = xp % v1y(i,xp%jtsy:xp%jtey,m)
         do pass = rf_passes_over_two, 1, -1
            call DA_Recursive_Filter_1D_adj( pass, rf_alpha(m), &
                                             val_j, n)
         end do
         xp % v1y(i,xp%jtsy:xp%jtey,m) = val_j(xp%jtsy:xp%jtey)
      end do
   end do

!  [3.1] Apply (i',j,k' -> i,j',k') (xp % v1y -> xp % v1x)
!--convert from y-stripe to x-stripe

   call transpose_y2x ( xp )

!-------------------------------------------------------------------------
!  [2.0]: Perform 1D recursive filter in x-direction:
!-------------------------------------------------------------------------

!  [2.2] Apply 1D filter in x direction:

   n = xp%itex-xp%itsx+1

   do m = xp%ktsx, min(xp%ktex,mz)
      do j = xp%jtsx, xp%jtex
         do pass = rf_passes_over_two, 1, -1
            call DA_Recursive_Filter_1D_adj( pass, rf_alpha(m), &
                             xp % v1x(xp%itsx:xp%itex,j,m), n)
         end do
      end do
   end do

!  [2.1] Apply (i,j',k' -> i',j',k) (xp % v1x -> xp % v1z)
!--convert from x-stripe to vertical column

   call transpose_x2z ( xp )

!-------------------------------------------------------------------------
!  [1.0]: Initialise:
!-------------------------------------------------------------------------  

!  [1.2] Transform to nondimensional v_hat space:

   do m = 1, mz
      field(its:ite,jts:jte,m) = xp % v1z(its:ite,jts:jte,m) / &
                                  p_x(its:ite,jts:jte)
   end do

END SUBROUTINE DA_Transform_Through_RF_Adj

