subroutine DA_PsiChi_To_UV_Adj( u, v, map_factor, ds, psi, chi, &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Calculate wind components u and v from psi and chi.
!
!  METHOD:  u = m * ( -dpsi/dy + dchi/dx )
!           v = m * (  dpsi/dx + dchi/dy )
!
!  ASSUMPTIONS: Unstaggered grid.
!               Lateral boundary conditions - dpsi/dn, dchi/dn = 0 (FCT)
!               dx = dy = ds on grid.
!
!  HISTORY: 02/03/2000 - Creation of F90 version.           Dale Barker
!           10/30/2001 - Parallel version.                  Dale Barker
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
   integer, intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
   integer, intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims.
   
   real, intent(inout):: u(ims:ime,jms:jme,kms:kme)   ! u wind comp.
   real, intent(inout):: v(ims:ime,jms:jme,kms:kme)   ! v wind comp.
   real, intent(in)   :: map_factor(ims:ime,jms:jme)  ! Map fact.
   real, intent(in)   :: ds                           ! Resolution.
   real, intent(inout):: psi(ims:ime,jms:jme,kms:kme) ! Stream function
   real, intent(inout):: chi(ims:ime,jms:jme,kms:kme) ! Velocity potential
   
   integer            :: i, j, k                      ! Loop counters.
   integer            :: is, ie                       ! 1st dim. end points.
   integer            :: js, je                       ! 2nd dim. end points.
   integer            :: ks, ke                       ! 3rd dim. end points.
   real               :: coeff(ims:ime,jms:jme)       ! Multiplicative coefficient.
   real               :: coeff_u                      ! Multiplicative coefficient.
   real               :: coeff_v                      ! Multiplicative coefficient.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

!  Computation to check for edge of domain:
   is = its; ie = ite; js = jts; je = jte
   if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
   if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1

   coeff(is-1:ie+1,js-1:je+1) = 0.5 * map_factor(is-1:ie+1,js-1:je+1) / ds

   do k = kts, kte
   
!------------------------------------------------------------------------------
!     [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!     [4.1] Bottom-left point:

      if ( its == ids .AND. jts == jds ) then
         u(its+1,jts,k) = u(its+1,jts,k) + 0.5 * u(its,jts,k)
         u(its,jts+1,k) = u(its,jts+1,k) + 0.5 * u(its,jts,k)
         v(its+1,jts,k) = v(its+1,jts,k) + 0.5 * v(its,jts,k)
         v(its,jts+1,k) = v(its,jts+1,k) + 0.5 * v(its,jts,k)
      end if

!     [4.2] Top-left point:

      if ( ite == ide .AND. jts == jds ) then
         u(ite-1,jts,k) = u(ite-1,jts,k) + 0.5 * u(ite,jts,k)
         u(ite,jts+1,k) = u(ite,jts+1,k) + 0.5 * u(ite,jts,k)
         v(ite-1,jts,k) = v(ite-1,jts,k) + 0.5 * v(ite,jts,k)
         v(ite,jts+1,k) = v(ite,jts+1,k) + 0.5 * v(ite,jts,k)
      end if
   
!     [4.3] Bottom-right point:

      if ( its == ids .AND. jte == jde ) then
         u(its+1,jte,k) = u(its+1,jte,k) + 0.5 * u(its,jte,k)
         u(its,jte-1,k) = u(its,jte-1,k) + 0.5 * u(its,jte,k)
         v(its+1,jte,k) = v(its+1,jte,k) + 0.5 * v(its,jte,k)
         v(its,jte-1,k) = v(its,jte-1,k) + 0.5 * v(its,jte,k)
      end if

!     [4.4] Top-right point:

      if ( ite == ide .AND. jte == jde ) then
         u(ite-1,jte,k) = u(ite-1,jte,k) + 0.5 * u(ite,jte,k)
         u(ite,jte-1,k) = u(ite,jte-1,k) + 0.5 * u(ite,jte,k)
         v(ite-1,jte,k) = v(ite-1,jte,k) + 0.5 * v(ite,jte,k)
         v(ite,jte-1,k) = v(ite,jte-1,k) + 0.5 * v(ite,jte,k)
      end if
      
   enddo

!------------------------------------------------------------------------------
!  [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

   is = its-1; ie = ite+1; js = jts-1; je = jte+1
   if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
   if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1

   do k = kts, kte
   
!     [3.4] Northern boundaries:

      if ( jte == jde ) then
         j = jte

         do i = ie, is, -1
            coeff_u = coeff(i,j) * u(i,j,k)
            coeff_v = coeff(i,j) * v(i,j,k)

            psi(i+1,j,k) = psi(i+1,j,k) + coeff_v
            psi(i-1,j,k) = psi(i-1,j,k) - coeff_v
            chi(i,j  ,k) = chi(i,j  ,k) + coeff_v
            chi(i,j-2,k) = chi(i,j-2,k) - coeff_v

            psi(i,j  ,k) = psi(i,j  ,k) - coeff_u
            psi(i,j-2,k) = psi(i,j-2,k) + coeff_u
            chi(i+1,j,k) = chi(i+1,j,k) + coeff_u
            chi(i-1,j,k) = chi(i-1,j,k) - coeff_u
         end do
      end if
      
!     [3.3] Southern boundaries:

      if ( jts == jds ) then
         j = jts

         do i = ie, is, -1
            coeff_u = coeff(i,j) * u(i,j,k)
            coeff_v = coeff(i,j) * v(i,j,k)

            psi(i+1,j,k) = psi(i+1,j,k) + coeff_v
            psi(i-1,j,k) = psi(i-1,j,k) - coeff_v
            chi(i,j+2,k) = chi(i,j+2,k) + coeff_v
            chi(i,j  ,k) = chi(i,j  ,k) - coeff_v

            psi(i,j+2,k) = psi(i,j+2,k) - coeff_u
            psi(i,j  ,k) = psi(i,j  ,k) + coeff_u
            chi(i+1,j,k) = chi(i+1,j,k) + coeff_u
            chi(i-1,j,k) = chi(i-1,j,k) - coeff_u
         end do
      end if
      
!     [3.2] Eastern boundaries:

      if ( ite == ide ) then
         i = ite

         do j = je, js, -1
            coeff_u = coeff(i,j) * u(i,j,k)
            coeff_v = coeff(i,j) * v(i,j,k)

            psi(i  ,j,k) = psi(i  ,j,k) + coeff_v
            psi(i-2,j,k) = psi(i-2,j,k) - coeff_v
            chi(i,j+1,k) = chi(i,j+1,k) + coeff_v
            chi(i,j-1,k) = chi(i,j-1,k) - coeff_v

            psi(i,j+1,k) = psi(i,j+1,k) - coeff_u
            psi(i,j-1,k) = psi(i,j-1,k) + coeff_u
            chi(i  ,j,k) = chi(i  ,j,k) + coeff_u
            chi(i-2,j,k) = chi(i-2,j,k) - coeff_u
         end do
      end if

!     [3.1] Western boundaries:
      if ( its == ids ) then
         i = its

         do j = je, js, -1
            coeff_u = coeff(i,j) * u(i,j,k)
            coeff_v = coeff(i,j) * v(i,j,k)

            psi(i+2,j,k) = psi(i+2,j,k) + coeff_v
            psi(i  ,j,k) = psi(i  ,j,k) - coeff_v
            chi(i,j+1,k) = chi(i,j+1,k) + coeff_v
            chi(i,j-1,k) = chi(i,j-1,k) - coeff_v

            psi(i,j+1,k) = psi(i,j+1,k) - coeff_u
            psi(i,j-1,k) = psi(i,j-1,k) + coeff_u
            chi(i+2,j,k) = chi(i+2,j,k) + coeff_u
            chi(i  ,j,k) = chi(i  ,j,k) - coeff_u
         end do
      end if

!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

      do j = je, js, -1
         do i = ie, is, -1
            coeff_u = coeff(i,j) * u(i,j,k)
            coeff_v = coeff(i,j) * v(i,j,k)

            psi(i+1,j,k) = psi(i+1,j,k) + coeff_v
            psi(i-1,j,k) = psi(i-1,j,k) - coeff_v
            chi(i,j+1,k) = chi(i,j+1,k) + coeff_v
            chi(i,j-1,k) = chi(i,j-1,k) - coeff_v

            psi(i,j+1,k) = psi(i,j+1,k) - coeff_u
            psi(i,j-1,k) = psi(i,j-1,k) + coeff_u
            chi(i+1,j,k) = chi(i+1,j,k) + coeff_u
            chi(i-1,j,k) = chi(i-1,j,k) - coeff_u
         end do
      end do
   end do

end subroutine DA_PsiChi_To_UV_Adj

