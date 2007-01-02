subroutine DA_Balance_Equation_Adj( xb, xbx, xp, phi_b, u, v, &
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )
 
!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of DA_Balance_Equation.
!
!  METHOD:  Standard adjoint code. See forward routine for details.
!
!  ASSUMPTIONS: Various (see documentation).
!
!  HISTORY: 03/20/2000 - Creation of F90 version.        Dale Barker
!
!           01/31/2005 - Updated for global              Syed RH Rizvi 
!
!------------------------------------------------------------------------------

   implicit none
   
   integer, intent(in):: ids,ide, jds,jde, kds,kde! domain dims.
   integer, intent(in):: ims,ime, jms,jme, kms,kme! memory dims.
   integer, intent(in):: its,ite, jts,jte, kts,kte! tile   dims.

   type (xb_type), intent(in) :: xb               ! First guess structure.
   type (xbx_type),intent(in) :: xbx              ! Header & non-gridded vars.
   type (xpose_type),intent(inout)  :: xp         ! Dimensions and xpose buffers. 
   real, intent(in)           :: phi_b(ims:ime,jms:jme,kms:kme) ! Balanced mass increment.
   real, intent(inout)        :: u(ims:ime,jms:jme,kms:kme) ! u wind comp. (dot pts)
   real, intent(inout)        :: v(ims:ime,jms:jme,kms:kme) ! v wind comp. (dot pts)

   integer                    :: i, j, k          ! Loop counters.
   integer                    :: is, ie           ! 1st dim. end points.
   integer                    :: js, je           ! 2nd dim. end points.
   integer                    :: ks, ke           ! 3rd dim. end points.

   real, dimension(ims:ime,jms:jme) :: coefx, &   ! Multiplicative coefficient.
                                       coefy, &   ! Multiplicative coefficient.
                                       term_x, &  ! Balance eqn x term
                                       term_y     ! Balance eqn y term

   real, dimension(ims:ime,jms:jme,kms:kme) :: del2phi_b  ! Del**2 phi_b/M**2
   real                       :: coeff1, coeff2   ! Multiplicative coefficient.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

!  Computation to check for edge of domain:
   is = its-1; ie = ite+1; js = jts-1; je = jte+1
   if (.not. global .and. its == ids ) is = ids+1
   if (.not. global .and. ite == ide ) ie = ide-1
   if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1

   if( fg_format == 3) then
   coefx(is:ie,js:je) = xb%coefx(is:ie,js:je)
   coefy(is:ie,js:je) = xb%coefy(is:ie,js:je)

   else if( fg_format == 1 .or. fg_format==2) then

   coefx(is:ie,js:je) = xb%coefz(is:ie,js:je)
   coefy(is:ie,js:je) = coefx(is:ie,js:je)
   else
   print*,' Wrong FG_FORMAT = ',fg_format
   end if

!  [1.1] Multiplicative coefficent for conversion RHS->Del**2 phi_b/M**2:

   del2phi_b(:,:,:) = 0.0

!------------------------------------------------------------------------------
!  [3.0] Solve Del**2 phi_b = RHS for phi_b:
!------------------------------------------------------------------------------

   CALL DA_Solve_PoissonEqn_FST_Adj( xb, xbx, xp, phi_b, &
                                 del2phi_b, &
                                 ids,ide, jds,jde, kds,kde,      &
                                 ims,ime, jms,jme, kms,kme,      &
                                 its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!     [2.0] Calculate RHS of balance equation in gridpt space:
!------------------------------------------------------------------------------

   DO k = kts, kte
   
!     [2.4] Del**2 Phi_b boundary conditions (null as zero boundary conditions):

!     [2.3] Take divergence to get Del**2 phi_b/M**2:

      term_x(ims:ime,jms:jme) = 0.0
      term_y(ims:ime,jms:jme) = 0.0

      DO j = je, js, -1
         DO i = ie, is, -1

            coeff1 = coefx(i,j) * del2phi_b(i,j,k)
            coeff2 = coefy(i,j) * del2phi_b(i,j,k)

            term_x(i+1,j) = term_x(i+1,j) - coeff1
            term_x(i-1,j) = term_x(i-1,j) + coeff1
            term_y(i,j+1) = term_y(i,j+1) - coeff2
            term_y(i,j-1) = term_y(i,j-1) + coeff2

         END DO
      END DO

!     [2.2] Include cyclostrophic terms in balance eqn if requested:

      IF ( balance_type == balance_cyc .OR. &
           balance_type == balance_geocyc ) THEN
         CALL DA_Balance_CycloTerm_Adj( xb % rho(:,:,k),xb % u(:,:,k),&
                                        xb % v(:,:,k), u(:,:,k), v(:,:,k),&
                                        xb % coefx(:,:), xb % coefy(:,:),&
                                        term_x(:,:), term_y(:,:), &
                                        ids,ide, jds,jde, kds,kde,  &
                                        ims,ime, jms,jme, kms,kme,  &
                                        its,ite, jts,jte, kts,kte )
      END IF

      
!     [2.1] Calculate geostrophic terms in balance eqn:
 
      IF ( balance_type == balance_geo .OR. &
           balance_type == balance_geocyc ) THEN

         CALL DA_Balance_GeoTerm_Adj( xb % cori, xb % rho(:,:,k), &
                                      term_x, term_y, &
                                      u(:,:,k), v(:,:,k), &
                                      ims,ime, jms,jme, its,ite, jts,jte )
      END IF
      
   END DO
END SUBROUTINE DA_Balance_Equation_Adj
