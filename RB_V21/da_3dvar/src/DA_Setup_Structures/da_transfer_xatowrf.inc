SUBROUTINE DA_Transfer_XatoWRF( xbx, grid, &
!
#include <em_dummy_args.inc>
!
                              )

!------------------------------------------------------------------------------
!  PURPOSE: Convert analysis increments into WRF increments 
!
!  HISTORY: 11/14/2002	 Manuel Pondeca	
!
!           Use non-linear Eq. to diagnose theta and ph perturbation
!           analysis.              05/12/2004  Modified by Y.-R. Guo      
!           
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xbx_type), INTENT(INOUT)     :: xbx          ! Header & non-gridded vars.

   TYPE(domain), TARGET               :: grid

!  Definitions of dummy arguments to solve

#include <em_dummy_decl.inc>

!---------------------------------------------------------------------------

   integer :: i, j, k, n

   integer :: is, ie, js, je, ks, ke

   real    :: sdmd, s1md

!---------------------------------------------------------------------------

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

!=>ARRAYS TO HOLD WRF INCREMENTS ON THE C-grid 

   REAL, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme) :: u_cgrid,  v_cgrid, &
                                                                 q_cgrid, ph_cgrid

   REAL, dimension(xp%ims:xp%ime,xp%jms:xp%jme)               :: mu_cgrid

   REAL    :: t_full  , p_full, rho_full, q_full, ph_full , ph_xb_hd, &
              qvf1, qvf2, qvf1_b, qvf2_b

!---------------------------------------------------------------------------
!--SET ARRAY RANGE INDICES FOR PROCESSOR SUBDOMAIN.
!---------------------------------------------------------------------------

   is = xp % its
   ie = xp % ite
   js = xp % jts
   je = xp % jte
   ks = xp % kts
   ke = xp % kte

! .. To keep the background PH perturbation:

   do j=js,je
   do i=is,ie
     do k=ks, ke+1
       ph_cgrid(i,j,k) = ph_2(i,j,k)
     enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [1.0] Get the mixing ratio of moisture first, as it is easy.
!------------------------------------------------------------------------------

   do k=ks,ke
   do j=js,je
   do i=is,ie
      if((xb%q(i,j,k)+xa%q(i,j,k)) < 0.0) then
         q_cgrid(i,j,k) =-xb%q(i,j,k)/(1.0 - xb%q(i,j,k))**2
      else
         q_cgrid(i,j,k) = xa%q(i,j,k)/(1.0 - xb%q(i,j,k))**2
      endif
   enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [2.0] COMPUTE increments of dry-column air mass per unit area
!------------------------------------------------------------------------------

   do j=js,je
   do i=is,ie
      sdmd=0.0
      s1md=0.0
      do k=ks,ke
         sdmd=sdmd+q_cgrid(i,j,k)*dnw(k)
         s1md=s1md+(1.0+moist(i,j,k,P_QV))*dnw(k)
      enddo

      mu_cgrid(i,j)=-(xa%psfc(i,j)+xb%psac(i,j)*sdmd)/s1md
   enddo
   enddo

!------------------------------------------------------------------------------
!  [3.0] COMPUTE pressure increments 
!------------------------------------------------------------------------------

! Tangent linear code for xa%p (based on WRF "real.init.code") 
! developed by Y.-R. Guo 05/13/2004:

   do j=js,je
   do i=is,ie
     k = ke
     qvf1   = 0.5*(q_cgrid(i,j,k)+q_cgrid(i,j,k))
     qvf1_b = 0.5*(moist(i,j,k,P_QV)+moist(i,j,k,P_QV))
     qvf2   = - qvf1 / ((1.+qvf1_b)*(1.+qvf1_b))
     qvf2_b = 1./(1.+qvf1)
     qvf1   = qvf1*qvf2_b + qvf1_b*qvf2
     qvf1_b = qvf1_b*qvf2_b
     xa%p(i,j,k) = (-0.5/rdnw(k)) * &
                ( (mu_cgrid(i,j)+qvf1*mub(i,j)) / qvf2_b &
                 -(mu_2(i,j)+qvf1_b*mub(i,j))*qvf2/(qvf2_b*qvf2_b) )

     do k = ke-1,1,-1
       qvf1   = 0.5*(q_cgrid(i,j,k)+q_cgrid(i,j,k+1))
       qvf1_b = 0.5*(moist(i,j,k,P_QV)+moist(i,j,k+1,P_QV))
       qvf2   = - qvf1 / ((1.+qvf1_b)*(1.+qvf1_b))
       qvf2_b = 1./(1.+qvf1_b)
       qvf1   = qvf1*qvf2_b + qvf1_b*qvf2
       qvf1_b = qvf1_b*qvf2_b
       xa%p(i,j,k) = xa%p(i,j,k+1)  &
                  - (1./rdn(k+1)) * &
                  ( (mu_cgrid(i,j)+qvf1*mub(i,j)) / qvf2_b &
                   -(mu_2(i,j)+qvf1_b*mub(i,j))*qvf2/(qvf2_b*qvf2_b) )
     enddo
   enddo
   enddo

! ----------------------------------------------------------------------------

!--Adjust xa%q to make xb%q + xa%q > 0.0

   if ( check_rh == 2 ) THEN
!     Shu-Hua~s TPW conservation:
      CALL da_check_rh( xb, xa, xp )
   else if ( check_rh == 1 ) then
!     Simple resetting to max/min values:
      call da_check_rh_simple( xb, xa, xp )
   end if

   do k=ks,ke
   do j=js,je
   do i=is,ie
      q_cgrid(i,j,k) = xa%q(i,j,k)/(1.0 - xb%q(i,j,k))**2
   enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [4.0] CONVERT TEMPERATURE INCREMENTS INTO THETA INCREMENTS 
!        EVALUATE ALSO THE INCREMENTS OF (1/rho) AND GEOPOTENTIAL
!------------------------------------------------------------------------------

   if(print_detail > 10) then
      write(unit=*, fmt='(a, e24.12)') &
        'sum(abs(xa%t(is:ie,js:je,ks:ke)))=', sum(abs(xa%t(is:ie,js:je,ks:ke))), &
        'sum(abs(xa%p(is:ie,js:je,ks:ke)))=', sum(abs(xa%p(is:ie,js:je,ks:ke))), &
        'sum(abs(xb%t(is:ie,js:je,ks:ke)))=', sum(abs(xb%t(is:ie,js:je,ks:ke))), &
        'sum(abs(xb%p(is:ie,js:je,ks:ke)))=', sum(abs(xb%p(is:ie,js:je,ks:ke))), &
        'sum(abs(t_2 (is:ie,js:je,ks:ke)))=', sum(abs(t_2 (is:ie,js:je,ks:ke)))

      write(unit=*, fmt='(2(2x, a, e20.12))') &
        'maxval(xa%u(is:ie,js:je,ks:ke))=', maxval(xa%u(is:ie,js:je,ks:ke)), &
        'minval(xa%u(is:ie,js:je,ks:ke))=', minval(xa%u(is:ie,js:je,ks:ke)), &
        'maxval(xa%v(is:ie,js:je,ks:ke))=', maxval(xa%v(is:ie,js:je,ks:ke)), &
        'minval(xa%v(is:ie,js:je,ks:ke))=', minval(xa%v(is:ie,js:je,ks:ke)), &
        'maxval(xa%t(is:ie,js:je,ks:ke))=', maxval(xa%t(is:ie,js:je,ks:ke)), &
        'minval(xa%t(is:ie,js:je,ks:ke))=', minval(xa%t(is:ie,js:je,ks:ke)), &
        'maxval(xa%q(is:ie,js:je,ks:ke))=', maxval(xa%q(is:ie,js:je,ks:ke)), &
        'minval(xa%q(is:ie,js:je,ks:ke))=', minval(xa%q(is:ie,js:je,ks:ke)), &
        'maxval(xa%p(is:ie,js:je,ks:ke))=', maxval(xa%p(is:ie,js:je,ks:ke)), &
        'minval(xa%p(is:ie,js:je,ks:ke))=', minval(xa%p(is:ie,js:je,ks:ke)), &
        'maxval(xa%psfc(is:ie,js:je))   =', maxval(xa%psfc(is:ie,js:je)), &
        'minval(xa%psfc(is:ie,js:je))   =', minval(xa%psfc(is:ie,js:je))
   endif

   do j=js,je
   do i=is,ie

     ph_full  = ht(i,j) * gravity
     ph_xb_hd = ht(i,j) * gravity
     do k = ks, ke
! To obtain all of the full fields: t, p, q(mixing ratio), rho

      t_full   = xa%t(i,j,k) + xb%t(i,j,k)
      p_full   = xa%p(i,j,k) + xb%p(i,j,k)
      q_full   = moist(i,j,k,P_QV) + q_cgrid(i,j,k)

! Note: According to WRF, this is the dry air density used to
!       compute the geopotential height: 
      rho_full = p_full / (gas_constant*t_full*(1.0+q_full/rd_over_rv))

! To compute the theta increment with the full fields:
      t_2(i,j,k) = t_full*(ps0/p_full)**kappa - ts0

! The full field of analysis ph:
         ph_full  = ph_full  &
                  - xb%dnw(k) * (xb%psac(i,j)+mu_cgrid(i,j)) / rho_full
!
! background hydrostatic phi:
         ph_xb_hd  = ph_xb_hd  &
                  - xb%dnw(k) * xb%psac(i,j) / xb%rho(i,j,k)
    
! The analysis perturbation = Hydro_ph - base_ph + nonhydro_xb_ph:
         ph_2(i,j,k+1) = ph_full - phb(i,j,k+1) &
                        + (xb%hf(i,j,k+1)*gravity - ph_xb_hd)
     enddo

   enddo
   enddo

! .. To compute the geopotential height increment:

   do k=ks, ke+1
     do j=js,je
     do i=is,ie
       ph_cgrid(i,j,k) = ph_2(i,j,k) - ph_cgrid(i,j,k)
     enddo
     enddo
   enddo

! ========================
! Write out the increment:
! ========================

   if (write_increments) then

      print '(/"WRITE OUT INCREMENT FOR PLOTTING......")'

      call DA_Write_Increment ( xp, xb, xa, q_cgrid, mu_cgrid, ph_cgrid )

   endif
! ----------------------------------------------------------------------------

!==> CONVERT FROM A-GRID TO C-GRID

#ifdef DM_PARALLEL
!--Fill the halo region for u and v.

   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)

!--Fill the boundary

!--The southern boundary
   if(js == xp%jds) then
      xa%v(is:ie,js-1,ks:ke)=2.0*xa%v(is:ie,js  ,ks:ke) &
                            -    xa%v(is:ie,js+1,ks:ke)
   endif

!--The northern boundary
   if(je == xp%jde) then
      xa%v(is:ie,je+1,ks:ke)=2.0*xa%v(is:ie,je  ,ks:ke) &
                            -    xa%v(is:ie,je-1,ks:ke)
   endif

!--The western boundary
   if(is == xp%ids) then
      xa%u(is-1,js:je,ks:ke)=2.0*xa%u(is  ,js:je,ks:ke) &
                            -    xa%u(is+1,js:je,ks:ke)
   endif

!--The eastern boundary
   if(ie == xp%ide) then
      xa%u(ie+1,js:je,ks:ke)=2.0*xa%u(ie  ,js:je,ks:ke) &
                            -    xa%u(ie-1,js:je,ks:ke)
   endif

   do j=js,je+1
   do k=ks,ke
      do i=is,ie+1
         u_cgrid(i,j,k)=0.5*(xa%u(i-1,j  ,k)+xa%u(i,j,k))
         v_cgrid(i,j,k)=0.5*(xa%v(i  ,j-1,k)+xa%v(i,j,k))
      enddo
   enddo
   enddo

!---------------------------------------------------------------------------
!  For later plot and comparation purpose only, zero out the unused var.
!---------------------------------------------------------------------------
!--The northern boundary
   if(je == xp%jde) then
      u_cgrid(is:ie+1,je+1,ks:ke)=0.0
   endif

!--The eastern boundary
   if(ie == xp%ide) then
      v_cgrid(ie+1,js:je+1,ks:ke)=0.0
   endif
!---------------------------------------------------------------------------

#else

!--The eastern boundary
   xa%u(ie+1,js:je,ks:ke)=2.0*xa%u(ie  ,js:je,ks:ke) &
                         -    xa%u(ie-1,js:je,ks:ke)

!--The northern boundary
   xa%v(is:ie,je+1,ks:ke)=2.0*xa%v(is:ie,je  ,ks:ke) &
                         -    xa%v(is:ie,je-1,ks:ke)

   do k=ks,ke
      do j=js,je
      do i=is+1,ie+1
         u_cgrid(i,j,k)=0.5*(xa%u(i-1,j,k)+xa%u(i,j,k))
      enddo
      enddo

      do j=js+1,je+1
      do i=is,ie
         v_cgrid(i,j,k)=0.5*(xa%v(i,j-1,k)+xa%v(i,j,k))
      enddo
      enddo
   enddo

!--The western boundary
   u_cgrid(is,js:je,ks:ke)=2.0*xa%u(is  ,js:je,ks:ke) &
                          - u_cgrid(is+1,js:je,ks:ke)

!--The southern boundary
   v_cgrid(is:ie,js,ks:ke)=2.0*xa%v(is:ie,js  ,ks:ke) &
                          - v_cgrid(is:ie,js+1,ks:ke)
#endif

!------------------------------------------------------------------------------
!  [5.0] ADD INCREMENT TO THE ORIGINAL GUESS AND UPDATE "grid"
!------------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         mu_2(i,j) = mu_2(i,j) + mu_cgrid(i,j)
         mu0(i,j) = mub(i,j) + mu_2(i,j)
         w_2(i,j,ke+1)=  w_2(i,j,ke+1) + xa%w(i,j,ke+1)
      enddo

      do k=ks,ke
      do i=is,ie
         u_2(i,j,k) = u_2(i,j,k) + u_cgrid(i,j,k)
         v_2(i,j,k) = v_2(i,j,k) + v_cgrid(i,j,k)
         w_2(i,j,k) = w_2(i,j,k) +    xa%w(i,j,k)
         moist(i,j,k,P_QV) = moist(i,j,k,P_QV) + q_cgrid(i,j,k)
!--------make sure qv is positive.
         if(moist(i,j,k,P_QV) < 1.0e-6) moist(i,j,k,P_QV) = 1.0e-6

        if ( size(moist,dim=4) >= 4 ) then
         moist(i,j,k,p_qc) = moist(i,j,k,p_qc) + xa%qcw(i,j,k)
         moist(i,j,k,p_qr) = moist(i,j,k,p_qr) + xa%qrn(i,j,k)
         if(moist(i,j,k,p_qc) < 0.0) moist(i,j,k,p_qc) = 0.0
         if(moist(i,j,k,p_qr) < 0.0) moist(i,j,k,p_qr) = 0.0
        endif

        if ( size(moist,dim=4) >= 6 ) then
         moist(i,j,k,p_qi) = moist(i,j,k,p_qi) + xa%qci(i,j,k)
         moist(i,j,k,p_qs) = moist(i,j,k,p_qs) + xa%qsn(i,j,k)
         if(moist(i,j,k,p_qi) < 0.0) moist(i,j,k,p_qi) = 0.0
         if(moist(i,j,k,p_qs) < 0.0) moist(i,j,k,p_qs) = 0.0
        endif

        if ( size(moist,dim=4) >= 7 ) then
         moist(i,j,k,p_qg) = moist(i,j,k,p_qg) + xa%qgr(i,j,k)
         if(moist(i,j,k,p_qg) < 0.0) moist(i,j,k,p_qg) = 0.0
        endif

      enddo
      enddo
   enddo

!--The northern boundary
   if(je == xp%jde) then
      j=je+1
      do k=ks,ke
      do i=is,ie
         v_2(i,j,k) = v_2(i,j,k) + v_cgrid(i,j,k)
      enddo
      enddo
   endif

!--The eastern boundary
   if(ie == xp%ide) then
      i=ie+1
      do k=ks,ke
      do j=js,je
         u_2(i,j,k) = u_2(i,j,k) + u_cgrid(i,j,k)
      enddo
      enddo
   endif

   if(print_detail > 10) then
      if(print_detail > 100) then
      write(unit=*, fmt='(/5a/)') ' simple varbles:'

      if(ie == xp%ide) then
         print *, ' '

         do k=ks+5,ke,10
            do j=js,je,10
               write(unit=*, fmt='(3(a,i5),a,e20.12)') &
                    'u_2(', xp%ide+1, ',', j, ',', k, ')=', &
                     u_2(xp%ide+1,j,k)
            enddo
            print *, ' '
         enddo
      endif

      if(je == xp%jde) then
         print *, ' '

         do k=ks+5,ke,10
            do i=is,ie,10
               write(unit=*, fmt='(3(a,i5),a,e20.12)') &
                    'v_2(', i, ',', xp%jde+1, ',', k, ')=', &
                     v_2(i, xp%jde+1,k)
            enddo
            print *, ' '
         enddo
      endif
      endif

      write(unit=*, fmt='(2(2x, a, e20.12))') &
        'maxval(mu_cgrid(is:ie,js:je))       =', maxval(mu_cgrid(is:ie,js:je)), &
        'minval(mu_cgrid(is:ie,js:je))       =', minval(mu_cgrid(is:ie,js:je)), &
        'maxval(u_cgrid(is:ie,js:je,ks:ke))  =', maxval(u_cgrid(is:ie,js:je,ks:ke)), &
        'minval(u_cgrid(is:ie,js:je,ks:ke))  =', minval(u_cgrid(is:ie,js:je,ks:ke)), &
        'maxval(v_cgrid(is:ie,js:je,ks:ke))  =', maxval(v_cgrid(is:ie,js:je,ks:ke)), &
        'minval(v_cgrid(is:ie,js:je,ks:ke))  =', minval(v_cgrid(is:ie,js:je,ks:ke)), &
        'maxval(q_cgrid(is:ie,js:je,ks:ke))  =', maxval(q_cgrid(is:ie,js:je,ks:ke)), &
        'minval(q_cgrid(is:ie,js:je,ks:ke))  =', minval(q_cgrid(is:ie,js:je,ks:ke))

      do k=ks,ke
         write(unit=*, fmt='(a, i3)') 'k=', k

         write(unit=*, fmt='(2(2x, a, e20.12))') &
        'maxval(u_cgrid(is:ie,js:je,k))  =', maxval(u_cgrid(is:ie,js:je,k)), &
        'minval(u_cgrid(is:ie,js:je,k))  =', minval(u_cgrid(is:ie,js:je,k)), &
        'maxval(v_cgrid(is:ie,js:je,k))  =', maxval(v_cgrid(is:ie,js:je,k)), &
        'minval(v_cgrid(is:ie,js:je,k))  =', minval(v_cgrid(is:ie,js:je,k)), &
        'maxval(q_cgrid(is:ie,js:je,k))  =', maxval(q_cgrid(is:ie,js:je,k)), &
        'minval(q_cgrid(is:ie,js:je,k))  =', minval(q_cgrid(is:ie,js:je,k))
     enddo
   endif

END SUBROUTINE DA_Transfer_XatoWRF

