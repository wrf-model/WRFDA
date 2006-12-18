subroutine DA_Balance_CycloTerm_Lin( rho, ub, vb, u, v, coefx,  &
                                     coefy, term_x, term_y,     &
                                     ids,ide, jds,jde, kds,kde, &
                                     ims,ime, jms,jme, kms,kme, &
                                     its,ite, jts,jte, kts,kte )
                                     
!------------------------------------------------------------------------------
!  PURPOSE: Calculates linearised cyclostrophic term in balance equation.
!
!  METHOD:  Term is rho (u.grad ) u on a single level.
!
!  ASSUMPTIONS: Various (see documentation).
!
!  HISTORY: 03/02/2000 - Creation of F90 version.           Dale Barker
!
!           01/31/2005 - Updated for Global                 Syed RH Rizvi 
!------------------------------------------------------------------------------

   implicit none
   
   integer, intent(in)  :: ids,ide, jds,jde, kds,kde    ! domain dims.
   integer, intent(in)  :: ims,ime, jms,jme, kms,kme    ! memory dims.
   integer, intent(in)  :: its,ite, jts,jte, kts,kte    ! tile   dims.

   real, dimension(ims:ime,jms:jme), intent(in) :: rho, &           ! Density.
                                                   ub, &            ! Background u wind
                                                   vb, &            ! Background u wind
                                                   u, &             ! u wind increment
                                                   v, &             ! v wind increment
                                                   coefx, coefy     ! Multiplicative const.

   real, dimension(ims:ime,jms:jme), intent(inout) :: term_x, &     ! x component of term.
                                                      term_y        ! y component of term.

   integer              :: i, j                         ! Loop counters.
   integer              :: is, ie                       ! 1st dim. end points.
   integer              :: js, je                       ! 2nd dim. end points.
   integer              :: ks, ke                       ! 3rd dim. end points.
   
   real                 :: data(ims:ime,jms:jme)        ! Temporary storage.

   real                 :: varb, var

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

!  Computation to check for edge of domain:
   is = its; ie = ite; js = jts; je = jte
   if (.not. global .and. its == ids ) is = ids+1
   if (.not. global .and. ite == ide ) ie = ide-1
   if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1

!------------------------------------------------------------------------------
!  [2.0] Calculate term_x = rho M ( u'du/dx + v'du/dy + udu'/dx + vdu'/dy ):
!------------------------------------------------------------------------------

!  [2.1] Interior points:

   do j = js, je
      do i = is, ie
         data(i,j) = coefx(i,j)*u(i,j) * ( ub(i+1,j) - ub(i-1,j) ) + &
                     coefy(i,j)*v(i,j) * ( ub(i,j+1) - ub(i,j-1) ) + &
                     coefx(i,j)*ub(i,j) * ( u(i+1,j) - u(i-1,j) ) + &
                     coefy(i,j)*vb(i,j) * ( u(i,j+1) - u(i,j-1) )
      end do
   end do

 if (global) go to 101      ! For global only interior points needed  

!  [2.2] Bottom boundaries:

   if ( its == ids ) then
      i = its

      do j = js, je 
         var  = -3.0*u (i,j)+4.0*u(i+1,j)-u(i+2,j)
         varb = -3.0*ub(i,j)+4.0*ub(i+1,j)-ub(i+2,j)

         data(i,j) = coefx(i,j)* u(i,j) * varb + &
                     coefy(i,j)* v(i,j) * ( ub(i,j+1) - ub(i,j-1) ) + &
                     coefx(i,j)*ub(i,j) * var + &
                     coefy(i,j)*vb(i,j) * ( u(i,j+1) - u(i,j-1) )
      end do
   end if

!  [2.3] Top boundaries:

   if ( ite == ide ) then
      i = ite

      do j = js, je
         var  = 3.0*u (i,j)-4.0*u (i-1,j)+u (i-2,j)
         varb = 3.0*ub(i,j)-4.0*ub(i-1,j)+ub(i-2,j)

         data(i,j) = coefx(i,j)*u(i,j) * varb + &
                     coefy(i,j)*v(i,j) * ( ub(i,j+1) - ub(i,j-1) ) + &
                     coefx(i,j)*ub(i,j) * var + &
                     coefy(i,j)*vb(i,j) * ( u(i,j+1) - u(i,j-1) )
      end do
   end if

!  [2.4] Left boundaries:

   if ( jts == jds ) then
      j = jts

      do i = is, ie
         var  = -3.0*u (i,j)+4.0*u (i,j+1)-u (i,j+2)
         varb = -3.0*ub(i,j)+4.0*ub(i,j+1)-ub(i,j+2)

         data(i,j) = coefx(i,j)*u(i,j) * ( ub(i+1,j) - ub(i-1,j) ) + &
                     coefy(i,j)*v(i,j) * varb + &
                     coefx(i,j)*ub(i,j) * ( u(i+1,j) - u(i-1,j) ) + &
                     coefy(i,j)*vb(i,j) * var
      end do
   end if

!  [2.5] Right boundaries:

   if ( jte == jde ) then
      j = jte

      do i = is, ie
         var  = 3.0*u (i,j)-4.0*u (i,j-1)+u (i,j-2)
         varb = 3.0*ub(i,j)-4.0*ub(i,j-1)+ub(i,j-2)

         data(i,j) = coefx(i,j)*u(i,j) * ( ub(i+1,j) - ub(i-1,j) ) + &
                     coefy(i,j)*v(i,j) * varb + &
                     coefx(i,j)*ub(i,j) * ( u(i+1,j) - u(i-1,j) ) + &
                     coefy(i,j)*vb(i,j) * var
      end do
   end if

!  [2.6] Corner points:

   if ( its == ids .AND. jts == jds ) then
      data(its,jts) = 0.5 * ( data(its,jts+1) + data(its+1,jts) )
   end if

   if ( ite == ide .AND. jts == jds ) then
      data(ite,jts) = 0.5 * ( data(ite-1,jts) + data(ite,jts+1) )
   end if
   
   if ( its == ids .AND. jte == jde ) then
      data(its,jde) = 0.5 * ( data(its,jde-1) + data(its+1,jde) )
   end if

   if ( ite == ide .AND. jte == jde ) then 
      data(ite,jte) = 0.5 * ( data(ite-1,jte) + data(ite,jte-1) )
   end if
      
101  continue

!  [2.7] Multiply by rho and add to term_x:

   term_x(its:ite,jts:jte) = rho(its:ite,jts:jte) * data(its:ite,jts:jte) + &
                             term_x(its:ite,jts:jte)
!------------------------------------------------------------------------------
!  [3.0] Calculate term_y = rho M ( u'dv/dx + v'dv/dy + udv'/dx + vdv'/dy ):
!------------------------------------------------------------------------------

!  [3.1] Interior points:

   do j = js, je
      do i = is, ie
         data(i,j) = coefx(i,j)*u(i,j) * ( vb(i+1,j) - vb(i-1,j) ) + &
                     coefy(i,j)*v(i,j) * ( vb(i,j+1) - vb(i,j-1) ) + &
                     coefx(i,j)*ub(i,j) * ( v(i+1,j) - v(i-1,j) ) + &
                     coefy(i,j)*vb(i,j) * ( v(i,j+1) - v(i,j-1) )
      end do
   end do
   
 if (global) go to 102      ! For global only interior points needed  
!  [3.2] Bottom boundaries:

   if ( its == ids ) then
      i = its

      do j = js, je
         var  = -3.0*v (i,j)+4.0*v (i+1,j)-v (i+2,j)
         varb = -3.0*vb(i,j)+4.0*vb(i+1,j)-vb(i+2,j)

         data(i,j) = coefx(i,j)*u(i,j) * varb + &
                     coefy(i,j)*v(i,j) * ( vb(i,j+1) - vb(i,j-1) ) + &
                     coefx(i,j)*ub(i,j) * var + &
                     coefy(i,j)*vb(i,j) * ( v(i,j+1) - v(i,j-1) )
      end do
   end if

!  [3.3] Top boundaries:

   if ( ite == ide ) then
      i = ite

      do j = js, je
         var  = 3.0*v (i,j)-4.0*v (i-1,j)+v (i-2,j)
         varb = 3.0*vb(i,j)-4.0*vb(i-1,j)+vb(i-2,j)

         data(i,j) = coefx(i,j)*u(i,j) * varb + &
                     coefy(i,j)*v(i,j) * ( vb(i,j+1) - vb(i,j-1) ) + &
                     coefx(i,j)*ub(i,j) * var + &
                     coefy(i,j)*vb(i,j) * ( v(i,j+1) - v(i,j-1) )
      end do
   end if

!  [3.4] Left boundaries:

   if ( jts == jds ) then
      j = jts

      do i = is, ie
         varb = -3.0*vb(i,j)+4.0*vb(i,j+1)-vb(i,j+2)
         var  = -3.0*v (i,j)+4.0*v (i,j+1)-v (i,j+2)

         data(i,j) = coefx(i,j)*u(i,j) * ( vb(i+1,j) - vb(i-1,j) ) + &
                     coefy(i,j)*v(i,j) * varb + &
                     coefx(i,j)*ub(i,j) * ( v(i+1,j) - v(i-1,j) ) + &
                     coefy(i,j)*vb(i,j) * var
      end do
   end if

!  [3.5] Right boundaries:

   if ( jte == jde ) then
      j = jte

      do i = is, ie
         varb = 3.0*vb(i,j)-4.0*vb(i,j-1)+vb(i,j-2)
         var  = 3.0*v (i,j)-4.0*v (i,j-1)+v (i,j-2)

         data(i,j) = coefx(i,j)*u(i,j) * ( vb(i+1,j) - vb(i-1,j) ) + &
                     coefy(i,j)*v(i,j) * varb + &
                     coefx(i,j)*ub(i,j) * ( v(i+1,j) - v(i-1,j) ) + &
                     coefy(i,j)*vb(i,j) * var
      end do
   end if

!  [3.6] Corner points:
   
   if ( its == ids .AND. jts == jds ) then
      data(its,jts) = 0.5 * ( data(its,jts+1) + data(its+1,jts) )
   end if

   if ( ite == ide .AND. jts == jds ) then
      data(ite,jts) = 0.5 * ( data(ite-1,jts) + data(ite,jts+1) )
   end if
   
   if ( its == ids .AND. jte == jde ) then
      data(its,jde) = 0.5 * ( data(its,jde-1) + data(its+1,jde) )
   end if

   if ( ite == ide .AND. jte == jde ) then 
      data(ite,jte) = 0.5 * ( data(ite-1,jte) + data(ite,jte-1) )
   end if

102  continue

!  [3.7] Multiply by  rho and add to term_y

   term_y(its:ite,jts:jte) = rho(its:ite,jts:jte) * data(its:ite,jts:jte) + &
                             term_y(its:ite,jts:jte)
end subroutine DA_Balance_CycloTerm_Lin
