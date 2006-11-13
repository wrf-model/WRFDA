subroutine da_transfer_xatowrf( xbx, grid, &
#include "em_dummy_new_args.inc"
!
                              )

   !---------------------------------------------------------------------------
   !  PURPOSE: Convert analysis increments into WRF increments
   !---------------------------------------------------------------------------

   implicit none
   
   type (xbx_type), intent(inout)     :: xbx          ! Header & non-gridded vars.

   type(domain), target               :: grid

   ! Definitions of dummy arguments to solve

#include "em_dummy_new_decl.inc"

   integer :: i, j, k, n

   integer :: is, ie, js, je, ks, ke

   real    :: sdmd, s1md

   ! ARRAYS TO HOLD WRF inCREMENTS ON THE C-grid 

   real, dimension(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme,grid%xp%kms:grid%xp%kme) :: &
      u_cgrid, v_cgrid, q_cgrid, ph_cgrid

   real, dimension(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme)               :: mu_cgrid

   real    :: t_full  , p_full, rho_full, q_full, ph_full , ph_xb_hd, &
              qvf1, qvf2, qvf1_b, qvf2_b

   if (trace_use) call da_trace_entry("da_transfer_xatowrf")

   !------------------------------------------------------------------------
   ! SET ARRAY RANGE inDICES FOR PROCESSOR SUBdoMAin.
   !------------------------------------------------------------------------

   is = grid%xp % its
   ie = grid%xp % ite
   js = grid%xp % jts
   je = grid%xp % jte
   ks = grid%xp % kts
   ke = grid%xp % kte

   ! To keep the background PH perturbation:

   do j=js,je
      do i=is,ie
         do k=ks, ke+1
            ph_cgrid(i,j,k) = grid%em_ph_2(i,j,k)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [1.0] Get the mixing ratio of moisture first, as it is easy.
   !---------------------------------------------------------------------------

   do k=ks,ke
      do j=js,je
         do i=is,ie
            if((grid%xb%q(i,j,k)+grid%xa%q(i,j,k)) < 0.0) then
               q_cgrid(i,j,k) =-grid%xb%q(i,j,k)/(1.0 - grid%xb%q(i,j,k))**2
            else
               q_cgrid(i,j,k) = grid%xa%q(i,j,k)/(1.0 - grid%xb%q(i,j,k))**2
            end if
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [2.0] COMPUTE increments of dry-column air mass per unit area
   !---------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         sdmd=0.0
         s1md=0.0
         do k=ks,ke
            sdmd=sdmd+q_cgrid(i,j,k)*grid%em_dnw(k)
            s1md=s1md+(1.0+moist(i,j,k,P_QV))*grid%em_dnw(k)
         end do

         mu_cgrid(i,j)=-(grid%xa%psfc(i,j)+grid%xb%psac(i,j)*sdmd)/s1md
      end do
   end do

   !---------------------------------------------------------------------------
   ! [3.0] COMPUTE pressure increments 
   !---------------------------------------------------------------------------

   ! Tangent linear code for grid%xa%p (based on WRF "real.init.code") 
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
         grid%xa%p(i,j,k) = (-0.5/grid%em_rdnw(k)) * &
                    ( (mu_cgrid(i,j)+qvf1*grid%em_mub(i,j)) / qvf2_b &
                     -(grid%em_mu_2(i,j)+qvf1_b*grid%em_mub(i,j))*qvf2/(qvf2_b*qvf2_b) )

         do k = ke-1,1,-1
            qvf1   = 0.5*(q_cgrid(i,j,k)+q_cgrid(i,j,k+1))
            qvf1_b = 0.5*(moist(i,j,k,P_QV)+moist(i,j,k+1,P_QV))
            qvf2   = - qvf1 / ((1.+qvf1_b)*(1.+qvf1_b))
            qvf2_b = 1./(1.+qvf1_b)
            qvf1   = qvf1*qvf2_b + qvf1_b*qvf2
            qvf1_b = qvf1_b*qvf2_b
            grid%xa%p(i,j,k) = grid%xa%p(i,j,k+1)  &
                       - (1./grid%em_rdn(k+1)) * &
                       ( (mu_cgrid(i,j)+qvf1*grid%em_mub(i,j)) / qvf2_b &
                        -(grid%em_mu_2(i,j)+qvf1_b*grid%em_mub(i,j))*qvf2/(qvf2_b*qvf2_b) )
         end do
      end do
   end do

   ! Adjust grid%xa%q to make grid%xb%q + grid%xa%q > 0.0

   if ( check_rh == 2 ) then
      ! Shu-Hua~s TPW conservation:
      call da_check_rh( grid%xb, grid%xa, grid%xp )
   else if ( check_rh == 1 ) then
      ! Simple resetting to max/min values:
      call da_check_rh_simple( grid%xb, grid%xa, grid%xp )
   end if

   do k=ks,ke
      do j=js,je
         do i=is,ie
            q_cgrid(i,j,k) = grid%xa%q(i,j,k)/(1.0 - grid%xb%q(i,j,k))**2
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [4.0] CONVERT TEMPERATURE inCREMENTS inTO THETA inCREMENTS 
   !       EVALUATE ALSO THE inCREMENTS OF (1/rho) AND GEOPOTENTIAL
   !---------------------------------------------------------------------------

   if (print_detail_xa) then
      write(unit=stdout, fmt='(a, e24.12)') &
         'sum(abs(grid%xa%t(is:ie,js:je,ks:ke)))=', &
         sum(abs(grid%xa%t(is:ie,js:je,ks:ke))), &
         'sum(abs(grid%xa%p(is:ie,js:je,ks:ke)))=', &
         sum(abs(grid%xa%p(is:ie,js:je,ks:ke))), &
         'sum(abs(grid%xb%t(is:ie,js:je,ks:ke)))=', &
         sum(abs(grid%xb%t(is:ie,js:je,ks:ke))), &
         'sum(abs(grid%xb%p(is:ie,js:je,ks:ke)))=', &
         sum(abs(grid%xb%p(is:ie,js:je,ks:ke))), &
         'sum(abs(grid%em_t_2 (is:ie,js:je,ks:ke)))=', &
         sum(abs(grid%em_t_2 (is:ie,js:je,ks:ke)))

       write(unit=stdout, fmt='(2(2x, a, e20.12))') &
          'maxval(grid%xa%u(is:ie,js:je,ks:ke))=', &
          maxval(grid%xa%u(is:ie,js:je,ks:ke)), &
          'minval(grid%xa%u(is:ie,js:je,ks:ke))=', & 
          minval(grid%xa%u(is:ie,js:je,ks:ke)), &
          'maxval(grid%xa%v(is:ie,js:je,ks:ke))=', &
          maxval(grid%xa%v(is:ie,js:je,ks:ke)), &
          'minval(grid%xa%v(is:ie,js:je,ks:ke))=', &
          minval(grid%xa%v(is:ie,js:je,ks:ke)), &
          'maxval(grid%xa%t(is:ie,js:je,ks:ke))=', &
          maxval(grid%xa%t(is:ie,js:je,ks:ke)), &
          'minval(grid%xa%t(is:ie,js:je,ks:ke))=', &
          minval(grid%xa%t(is:ie,js:je,ks:ke)), &
          'maxval(grid%xa%q(is:ie,js:je,ks:ke))=', &
          maxval(grid%xa%q(is:ie,js:je,ks:ke)), &
          'minval(grid%xa%q(is:ie,js:je,ks:ke))=', &
          minval(grid%xa%q(is:ie,js:je,ks:ke)), &
          'maxval(grid%xa%p(is:ie,js:je,ks:ke))=', &
          maxval(grid%xa%p(is:ie,js:je,ks:ke)), &
          'minval(grid%xa%p(is:ie,js:je,ks:ke))=', &
          minval(grid%xa%p(is:ie,js:je,ks:ke)), &
          'maxval(grid%xa%psfc(is:ie,js:je))   =', &
          maxval(grid%xa%psfc(is:ie,js:je)), &
          'minval(grid%xa%psfc(is:ie,js:je))   =', &
          minval(grid%xa%psfc(is:ie,js:je))
   end if

   do j=js,je
      do i=is,ie

         ph_full  = grid%ht(i,j) * gravity
         ph_xb_hd = grid%ht(i,j) * gravity
         do k = ks, ke
            ! To obtain all of the full fields: t, p, q(mixing ratio), rho
            t_full   = grid%xa%t(i,j,k) + grid%xb%t(i,j,k)
            p_full   = grid%xa%p(i,j,k) + grid%xb%p(i,j,k)
            q_full   = moist(i,j,k,P_QV) + q_cgrid(i,j,k)

            ! Note: According to WRF, this is the dry air density used to
            !       compute the geopotential height: 
            rho_full = p_full / (gas_constant*t_full*(1.0+q_full/rd_over_rv))

            ! To compute the theta increment with the full fields:
            grid%em_t_2(i,j,k) = t_full*(ps0/p_full)**kappa - ts0

            ! The full field of analysis ph:
            ph_full  = ph_full  &
                       - grid%xb%dnw(k) * (grid%xb%psac(i,j)+mu_cgrid(i,j)) / rho_full

            ! background hydrostatic phi:
            ph_xb_hd  = ph_xb_hd  &
                       - grid%xb%dnw(k) * grid%xb%psac(i,j) / grid%xb%rho(i,j,k)

            ! The analysis perturbation = Hydro_ph - base_ph + nonhydro_xb_ph:
            grid%em_ph_2(i,j,k+1) = ph_full - grid%em_phb(i,j,k+1) &
                            + (grid%xb%hf(i,j,k+1)*gravity - ph_xb_hd)
         end do
      end do
   end do

   ! .. To compute the geopotential height increment:

   do k=ks, ke+1
     do j=js,je
        do i=is,ie
           ph_cgrid(i,j,k) = grid%em_ph_2(i,j,k) - ph_cgrid(i,j,k)
        end do
     end do
   end do

   ! ========================
   ! Write out the increment:
   ! ========================

   if (write_increments) then
      write(unit=stdout,fmt='(/"write out inCREMENT FOR PLOTTinG......")')
      call da_write_increments ( grid%xp, grid%xb, grid%xa, q_cgrid, mu_cgrid, ph_cgrid )
   end if

   ! CONVERT FROM A-GRID TO C-GRID

   !TBH:  NOTE that grid%xp%halo_id3 = HALO_PSICHI_UV_ADJ which is currently defined 
   !TBH:  in the Registry as "dyn_em 24:grid%xa%u,grid%xa%v,grid%xa%psfc".  Clearly it is not 
   !TBH:  necessary to update halos in grid%xa%psfc here!  Also, 24-point stencil is 
   !TBH:  too thick, 9-point should suffice.  Apparently, grid%xp%halo_id3 is used 
   !TBH:  in many places!  This needs to be fixed.  

   ! Fill the halo region for u and v.

   call wrf_dm_halo(grid%xp%domdesc,grid%xp%comms,grid%xp%halo_id3)

   ! Fill the boundary

   ! The southern boundary
   if(js == grid%xp%jds) then
      grid%xa%v(is:ie,js-1,ks:ke)=2.0*grid%xa%v(is:ie,js  ,ks:ke) &
                            -    grid%xa%v(is:ie,js+1,ks:ke)
   end if

   ! The northern boundary
   if(je == grid%xp%jde) then
      grid%xa%v(is:ie,je+1,ks:ke)=2.0*grid%xa%v(is:ie,je  ,ks:ke) &
                            -    grid%xa%v(is:ie,je-1,ks:ke)
   end if

   ! The western boundary
   if(is == grid%xp%ids) then
      grid%xa%u(is-1,js:je,ks:ke)=2.0*grid%xa%u(is  ,js:je,ks:ke) &
                            -    grid%xa%u(is+1,js:je,ks:ke)
   end if

   ! The eastern boundary
   if(ie == grid%xp%ide) then
      grid%xa%u(ie+1,js:je,ks:ke)=2.0*grid%xa%u(ie  ,js:je,ks:ke) &
                            -    grid%xa%u(ie-1,js:je,ks:ke)
   end if

   do k=ks,ke
      do j=js,je+1
         do i=is,ie+1
            u_cgrid(i,j,k)=0.5*(grid%xa%u(i-1,j  ,k)+grid%xa%u(i,j,k))
            v_cgrid(i,j,k)=0.5*(grid%xa%v(i  ,j-1,k)+grid%xa%v(i,j,k))
         end do
      end do
   end do

   !------------------------------------------------------------------------
   ! For later plot and comparation purpose only, zero out the unused var.
   !------------------------------------------------------------------------

   ! The northern boundary
   if(je == grid%xp%jde) then
      u_cgrid(is:ie+1,je+1,ks:ke)=0.0
   end if

   ! The eastern boundary
   if (ie == grid%xp%ide) then
      v_cgrid(ie+1,js:je+1,ks:ke)=0.0
   end if

   !---------------------------------------------------------------------------
   ! [5.0] ADD inCREMENT TO THE ORIGinAL GUESS AND UPDATE "grid"
   !---------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         grid%em_mu_2(i,j) = grid%em_mu_2(i,j) + mu_cgrid(i,j)
         grid%em_mu0(i,j) = grid%em_mub(i,j) + grid%em_mu_2(i,j)
         grid%em_w_2(i,j,ke+1)=  grid%em_w_2(i,j,ke+1) + grid%xa%w(i,j,ke+1)
      end do

      do k=ks,ke
         do i=is,ie
            grid%em_u_2(i,j,k) = grid%em_u_2(i,j,k) + u_cgrid(i,j,k)
            grid%em_v_2(i,j,k) = grid%em_v_2(i,j,k) + v_cgrid(i,j,k)
            grid%em_w_2(i,j,k) = grid%em_w_2(i,j,k) + grid%xa%w(i,j,k)
            moist(i,j,k,P_QV) = grid%moist(i,j,k,P_QV) + q_cgrid(i,j,k)
            ! make sure qv is positive.
            if(moist(i,j,k,P_QV) < 1.0e-6) moist(i,j,k,P_QV) = 1.0e-6

            if ( size(moist,dim=4) >= 4 ) then
               moist(i,j,k,p_qc) = moist(i,j,k,p_qc) + grid%xa%qcw(i,j,k)
               moist(i,j,k,p_qr) = moist(i,j,k,p_qr) + grid%xa%qrn(i,j,k)
               if(moist(i,j,k,p_qc) < 0.0) moist(i,j,k,p_qc) = 0.0
               if(moist(i,j,k,p_qr) < 0.0) moist(i,j,k,p_qr) = 0.0
            end if

            if ( size(moist,dim=4) >= 6 ) then
               moist(i,j,k,p_qi) = moist(i,j,k,p_qi) + grid%xa%qci(i,j,k)
               moist(i,j,k,p_qs) = moist(i,j,k,p_qs) + grid%xa%qsn(i,j,k)
               if(moist(i,j,k,p_qi) < 0.0) moist(i,j,k,p_qi) = 0.0
               if(moist(i,j,k,p_qs) < 0.0) moist(i,j,k,p_qs) = 0.0
            end if

            if ( size(moist,dim=4) >= 7 ) then
               moist(i,j,k,p_qg) = moist(i,j,k,p_qg) + grid%xa%qgr(i,j,k)
               if(moist(i,j,k,p_qg) < 0.0) moist(i,j,k,p_qg) = 0.0
            end if
         end do
      end do
   end do

   ! The northern boundary
   if(je == grid%xp%jde) then
      j=je+1
      do k=ks,ke
         do i=is,ie
            grid%em_v_2(i,j,k) = grid%em_v_2(i,j,k) + v_cgrid(i,j,k)
         end do
      end do
   end if

   ! The eastern boundary
   if(ie == grid%xp%ide) then
      i=ie+1
      do k=ks,ke
         do j=js,je
            grid%em_u_2(i,j,k) = grid%em_u_2(i,j,k) + u_cgrid(i,j,k)
         end do
      end do
   end if

   if(print_detail_xa) then
      write(unit=stdout, fmt='(/5a/)') ' simple variables:'

      if(ie == grid%xp%ide) then
         print *, ' '

         do k=ks+5,ke,10
            do j=js,je,10
               write(unit=stdout, fmt='(3(a,i5),a,e20.12)') &
                    'grid%em_u_2(', grid%xp%ide+1, ',', j, ',', k, ')=', &
                     grid%em_u_2(grid%xp%ide+1,j,k)
            end do
            write(unit=stdout, fmt=*) ' '
         end do
      end if

      if(je == grid%xp%jde) then
         write(unit=stdout, fmt=*) ' '

         do k=ks+5,ke,10
            do i=is,ie,10
               write(unit=stdout, fmt='(3(a,i5),a,e20.12)') &
                    'grid%em_v_2(', i, ',', grid%xp%jde+1, ',', k, ')=', &
                     grid%em_v_2(i, grid%xp%jde+1,k)
            end do
            write(unit=stdout, fmt=*) ' '
         end do
      end if

      write(unit=stdout, fmt='(2(2x, a, e20.12))') &
         'maxval(mu_cgrid(is:ie,js:je))       =', &
         maxval(mu_cgrid(is:ie,js:je)), &
         'minval(mu_cgrid(is:ie,js:je))       =', &
         minval(mu_cgrid(is:ie,js:je)), &
         'maxval(u_cgrid(is:ie,js:je,ks:ke))  =', &
         maxval(u_cgrid(is:ie,js:je,ks:ke)), &
         'minval(u_cgrid(is:ie,js:je,ks:ke))  =', &
         minval(u_cgrid(is:ie,js:je,ks:ke)), &
         'maxval(v_cgrid(is:ie,js:je,ks:ke))  =', &
         maxval(v_cgrid(is:ie,js:je,ks:ke)), &
         'minval(v_cgrid(is:ie,js:je,ks:ke))  =', &
         minval(v_cgrid(is:ie,js:je,ks:ke)), &
         'maxval(q_cgrid(is:ie,js:je,ks:ke))  =', &
         maxval(q_cgrid(is:ie,js:je,ks:ke)), &
         'minval(q_cgrid(is:ie,js:je,ks:ke))  =', &
         minval(q_cgrid(is:ie,js:je,ks:ke))

      do k=ks,ke
         write(unit=stdout, fmt='(a, i3)') 'k=', k

         write(unit=stdout, fmt='(2(2x, a, e20.12))') &
            'maxval(u_cgrid(is:ie,js:je,k))  =', maxval(u_cgrid(is:ie,js:je,k)), &
            'minval(u_cgrid(is:ie,js:je,k))  =', minval(u_cgrid(is:ie,js:je,k)), &
            'maxval(v_cgrid(is:ie,js:je,k))  =', maxval(v_cgrid(is:ie,js:je,k)), &
            'minval(v_cgrid(is:ie,js:je,k))  =', minval(v_cgrid(is:ie,js:je,k)), &
            'maxval(q_cgrid(is:ie,js:je,k))  =', maxval(q_cgrid(is:ie,js:je,k)), &
            'minval(q_cgrid(is:ie,js:je,k))  =', minval(q_cgrid(is:ie,js:je,k))
      end do
   end if

   if (trace_use) call da_trace_exit("da_transfer_xatowrf")

end subroutine da_transfer_xatowrf

