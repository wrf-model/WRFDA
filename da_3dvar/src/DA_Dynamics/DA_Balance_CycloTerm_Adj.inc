subroutine DA_Balance_CycloTerm_Adj( ds, rho, ub, vb, term_x, term_y, &
                                     map_factor, u, v, &
                                     ids,ide, jds,jde, kds,kde,  &
                                     ims,ime, jms,jme, kms,kme,  &
                                     its,ite, jts,jte, kts,kte )
 
!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of DA_Balance_CycloTerm.
!
!  METHOD:  Standard adjoint code. See forward routine for details.
!
!  ASSUMPTIONS: Various (see documentation).
!
!  HISTORY: 03/20/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   implicit none
   
   integer, intent(in)  :: ids,ide, jds,jde, kds,kde    ! domain dims.
   integer, intent(in)  :: ims,ime, jms,jme, kms,kme    ! memory dims.
   integer, intent(in)  :: its,ite, jts,jte, kts,kte    ! tile   dims.

   real, intent(in)     :: ds                           ! Resolution.

   real, dimension(ims:ime,jms:jme), intent(in) :: rho, &           ! Density.
                                                   ub, &            ! Background u wind
                                                   vb, &            ! Background u wind
                                                   term_x, &        ! x component of term
                                                   term_y, &        ! y component of term
                                                   map_factor       ! Map factors.

   real, dimension(ims:ime,jms:jme), intent(inout) :: u, &     ! u wind increment
                                                      v        ! v wind increment

   integer              :: i, j                         ! Loop counters.
   integer              :: is, ie                       ! 1st dim. end points.
   integer              :: js, je                       ! 2nd dim. end points.
   integer              :: ks, ke                       ! 3rd dim. end points.
   real                 :: inv_2ds                      ! 1/2ds.
   real                 :: data(ims:ime,jms:jme)        ! Work array.

   real                 :: var, varb
   
!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   inv_2ds = 0.5 / ds

!  Computation to check for edge of domain:
   is = its; ie = ite; js = jts; je = jte
   if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
   if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1

!------------------------------------------------------------------------------
!  [3.0] Calculate term_y = rho M ( u'dv/dx + v'dv/dy + udv'/dx + vdv'/dy ):
!------------------------------------------------------------------------------

!  [3.7] Multiply by 0.5 * rho * M / ds and add to term_y

   data(its:ite,jts:jte) = inv_2ds * rho(its:ite,jts:jte) * &
                           map_factor(its:ite,jts:jte) * &
                           term_y(its:ite,jts:jte)
!  [3.6] Corner points:
   
   if ( its == ids .AND. jts == jds ) then
      data(its,jts+1) = data(its,jts+1) + 0.5 * data(its,jts)
      data(its+1,jts) = data(its+1,jts) + 0.5 * data(its,jts)
   end if

   if ( ite == ide .AND. jts == jds ) then
      data(ite-1,jts) = data(ite-1,jts) + 0.5 * data(ite,jts)
      data(ite,jts+1) = data(ite,jts+1) + 0.5 * data(ite,jts)
   end if
   
   if ( its == ids .AND. jte == jde ) then
      data(its,jde-1) = data(its,jde-1) + 0.5 * data(its,jde)
      data(its+1,jde) = data(its+1,jde) + 0.5 * data(its,jde)
   end if

   if ( ite == ide .AND. jte == jde ) then 
      data(ite-1,jte) = data(ite-1,jte) + 0.5 * data(ite,jte)
      data(ite,jte-1) = data(ite,jte-1) + 0.5 * data(ite,jte)
   end if
   
!  [3.5] Right boundaries:

   if ( jte == jde ) then
      j = jte

      do i = is, ie
         varb = 3.0*vb(i,j)-4.0*vb(i,j+1)+vb(i,j+2)

         var = vb(i,j) * data(i,j)

         u(i,j) = u(i,j) + data(i,j) * ( vb(i+1,j) - vb(i-1,j) )
         v(i,j) = v(i,j) + data(i,j) * varb
         v(i+1,j) = v(i+1,j) + ub(i,j) * data(i,j)
         v(i-1,j) = v(i-1,j) - ub(i,j) * data(i,j)

         v(i,j) = v(i,j) + 3.0*var
         v(i,j-1) = v(i,j-1) -4.0*var
         v(i,j-2) = v(i,j-2) + var
      end do
   end if
   
!  [3.4] Left boundaries:

   if ( jts == jds ) then
      j = jts

      do i = is, ie
         varb = -3.0*vb(i,j)+4.0*vb(i,j+1)-vb(i,j+2)

         var = vb(i,j) * data(i,j)

         u(i,j) = u(i,j) + data(i,j) * ( vb(i+1,j) - vb(i-1,j) )
         v(i,j) = v(i,j) + data(i,j) * varb
         v(i+1,j) = v(i+1,j) + ub(i,j) * data(i,j)
         v(i-1,j) = v(i-1,j) - ub(i,j) * data(i,j)

         v(i,j) = v(i,j) - 3.0*var
         v(i,j+1) = v(i,j+1) +4.0*var
         v(i,j+2) = v(i,j+2) - var
      end do
   end if

!  [3.3] Top boundaries:

   if ( ite == ide ) then
      i = ite

      do j = js, je
         varb = 3.0*vb(i,j)-4.0*vb(i-1,j)+vb(i-2,j)

         var = ub(i,j) * data(i,j)
         u(i,j) = u(i,j) + data(i,j) * varb
         v(i,j) = v(i,j) + data(i,j) * ( vb(i,j+1) - vb(i,j-1) )
         v(i,j+1) = v(i,j+1) + vb(i,j) * data(i,j)
         v(i,j-1) = v(i,j-1) - vb(i,j) * data(i,j)

         v(i,j) = v(i,j) + 3.0*var
         v(i-1,j) = v(i-1,j) -4.0**var
         v(i-2,j) = v(i-2,j) + var
      end do
   end if

!  [3.2] Bottom boundaries:

   if ( its == ids ) then
      i = its

      do j = js, je
         varb = -3.0*vb(i,j)+4.0*vb(i-1,j)-vb(i-2,j)

         var = ub(i,j) * data(i,j)
         u(i,j) = u(i,j) + data(i,j) * varb
         v(i,j) = v(i,j) + data(i,j) * ( vb(i,j+1) - vb(i,j-1) )
         v(i,j+1) = v(i,j+1) + vb(i,j) * data(i,j)
         v(i,j-1) = v(i,j-1) - vb(i,j) * data(i,j)

         v(i,j) = v(i,j) - 3.0*var
         v(i+1,j) = v(i+1,j) +4.0**var
         v(i+2,j) = v(i+2,j) - var
      end do
   end if
   
!  [3.1] Interior points:

   do j = je, js, -1
      do i = ie, is, -1
         u(i,j) = u(i,j) + ( vb(i+1,j) - vb(i-1,j) ) * data(i,j)
         v(i,j) = v(i,j) + ( vb(i,j+1) - vb(i,j-1) ) * data(i,j)
         v(i+1,j) = v(i+1,j) + ub(i,j) * data(i,j)
         v(i-1,j) = v(i-1,j) - ub(i,j) * data(i,j)
         v(i,j+1) = v(i,j+1) + vb(i,j) * data(i,j)
         v(i,j-1) = v(i,j-1) - vb(i,j) * data(i,j)
      end do
   end do
   
!------------------------------------------------------------------------------
!  [2.0] Calculate term_x = rho M ( u'du/dx + v'du/dy + udu'/dx + vdu'/dy ):
!------------------------------------------------------------------------------

!  [2.7] Multiply by 0.5 * rho * M / ds and add to term_x:

   data(its:ite,jts:jte) = inv_2ds * rho(its:ite,jts:jte) * &
                           map_factor(its:ite,jts:jte) * term_x(its:ite,jts:jte)

!  [2.6] Corner points:

   if ( its == ids .AND. jts == jds ) then
      data(its,jts+1) = data(its,jts+1) + 0.5 * data(its,jts)
      data(its+1,jts) = data(its+1,jts) + 0.5 * data(its,jts)
   end if

   if ( ite == ide .AND. jts == jds ) then
      data(ite-1,jts) = data(ite-1,jts) + 0.5 * data(ite,jts)
      data(ite,jts+1) = data(ite,jts+1) + 0.5 * data(ite,jts)
   end if
   
   if ( its == ids .AND. jte == jde ) then
      data(its,jde-1) = data(its,jde-1) + 0.5 * data(its,jde)
      data(its+1,jde) = data(its+1,jde) + 0.5 * data(its,jde)
   end if

   if ( ite == ide .AND. jte == jde ) then 
      data(ite-1,jte) = data(ite-1,jte) + 0.5 * data(ite,jte)
      data(ite,jte-1) = data(ite,jte-1) + 0.5 * data(ite,jte)
   end if

!  [2.5] Right boundaries:

   if ( jte == jde ) then
      j = jte

      do i = is, ie
         varb = 3.0*ub(i,j)-4.0*ub(i,j-1)+ub(i,j-2)

         var = vb(i,j) * data(i,j)
         u(i+1,j) = u(i+1,j) + ub(i,j) *data(i,j)
         u(i-1,j) = u(i-1,j) - ub(i,j) *data(i,j)
         v(i,j) = v(i,j) + data(i,j) * varb
         u(i,j) = u(i,j) + data(i,j) * ( ub(i+1,j) - ub(i-1,j) )
         u(i,j) = u(i,j) + 3.0*var
         u(i,j-1) = u(i,j-1) -4.0*var
         u(i,j-2) = u(i,j-2) + var
      end do
   end if
   
!  [2.4] Left boundaries:

   if ( jts == jds ) then
      j = jts

      do i = is, ie
         varb = -3.0*ub(i,j)+4.0*ub(i,j+1)-ub(i,j+2)

         var = vb(i,j) * data(i,j)
         u(i+1,j) = u(i+1,j) + ub(i,j) *data(i,j)
         u(i-1,j) = u(i-1,j) - ub(i,j) *data(i,j)
         v(i,j) = v(i,j) + data(i,j) * varb
         u(i,j) = u(i,j) + data(i,j) * ( ub(i+1,j) - ub(i-1,j) )
         u(i,j) = u(i,j) - 3.0*var
         u(i,j+1) = u(i,j+1) +4.0*var
         u(i,j+2) = u(i,j+2) - var
      end do
   end if

!  [2.3] Top boundaries:

   if ( ite == ide ) then
      i = ite

      do j = js, je
         varb = 3.0*ub(i,j)-4.0*ub(i-1,j)+ub(i-2,j)

         var = ub(i,j) * data(i,j)
         u(i,j+1) = u(i,j+1) + vb(i,j) * data(i,j)
         u(i,j-1) = u(i,j-1) - vb(i,j) * data(i,j)
         v(i,j) = v(i,j) + data(i,j) * ( ub(i,j+1) - ub(i,j-1) )
         u(i,j) = u(i,j) + data(i,j) * varb
         u(i,j) = u(i,j) + 3.0*var
         u(i-1,j) =  u(i-1,j) - 4.0*var
         u(i-2,j) =  u(i-2,j) + var
      end do
   end if

!  [2.2] Bottom boundaries:

   if ( its == ids ) then
      i = its

      do j = js, je
         varb = -3.0*ub(i,j)+4.0*ub(i+1,j)-ub(i+2,j)

         var = ub(i,j) * data(i,j)
         u(i,j+1) = u(i,j+1) + vb(i,j) * data(i,j)
         u(i,j-1) = u(i,j-1) - vb(i,j) * data(i,j)
         v(i,j) = v(i,j) + data(i,j) * ( ub(i,j+1) - ub(i,j-1) )
         u(i,j) = u(i,j) + data(i,j) * varb
         u(i,j) = u(i,j) - 3.0*var
         u(i+1,j) =  u(i+1,j) + 4.0*var
         u(i+2,j) =  u(i+2,j) - var
      end do
   end if

!  [2.1] Interior points:

   do j = je, js, -1
      do i = ie, is, -1
         u(i,j) = u(i,j) + ( ub(i+1,j) - ub(i-1,j) ) * data(i,j)
         v(i,j) = v(i,j) + ( ub(i,j+1) - ub(i,j-1) ) * data(i,j)
         u(i+1,j) = u(i+1,j) + ub(i,j) * data(i,j)
         u(i-1,j) = u(i-1,j) - ub(i,j) * data(i,j)
         u(i,j+1) = u(i,j+1) + vb(i,j) * data(i,j)
         u(i,j-1) = u(i,j-1) - vb(i,j) * data(i,j)
      end do
   end do

end subroutine DA_Balance_CycloTerm_Adj

