subroutine da_transfer_wrftoxb( xbx, grid, &
#include <em_dummy_new_args.inc>
                              )

   !---------------------------------------------------------------------------
   ! PURPOSE: Transfers fields from WRF to first guess structure.
   !---------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xbx_type), INTENT(INOUT)     :: xbx        ! Header & non-gridded vars.

   TYPE(domain), TARGET               :: grid

   ! Definitions of dummy arguments to solve

#include <em_dummy_new_decl.inc>

   integer :: i, j, k, n

   integer :: is, ie, js, je, ks, ke

   real    :: theta, tmpvar, zrhom

   real    :: ssmi_psfc, ssmi_ta,  ssmi_gamma, zcld, &
              ssmi_tpw,  ssmi_alw, ssmi_dum,   ssmi_htpw 

   REAL, DIMENSION(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme) :: rgh_fac

   character(len=19) :: current_date

   real :: loc_psac_mean

   real, dimension(grid%xp%jds:grid%xp%jde) :: loc_latc_mean

   integer :: ierror, size2d

   real, dimension(grid%xp%kms:grid%xp%kme) :: DDT

   REAL   :: qvf1, cvpm, cpovcv, ppb, ttb, albn, aln
   REAL, ALLOCATABLE :: arrayglobal(:,:)

   !---------------------------------------------------------------------------
   ! Set xb array range indices for processor subdomain.
   !---------------------------------------------------------------------------

   IF (trace_use) call da_trace_entry("da_transfer_wrftoxb")

   is = grid%xp % its
   ie = grid%xp % ite
   js = grid%xp % jts
   je = grid%xp % jte
   ks = grid%xp % kts
   ke = grid%xp % kte

   grid%xb % map  = grid%map_proj
   ! grid%xb % ds   = 1.0/grid%rdx
   grid%xb % ds   = grid%dx

   grid%xb % mix = grid%xp % ide - grid%xp % ids + 1
   grid%xb % mjy = grid%xp % jde - grid%xp % jds + 1
   grid%xb % mkz = grid%xp % kde - grid%xp % kds + 1

   !rizvi   xbx%big_header%bhi(5,5) = grid%start_year
   !rizvi   xbx%big_header%bhi(6,5) = grid%start_month
   !rizvi   xbx%big_header%bhi(7,5) = grid%start_day
   !rizvi   xbx%big_header%bhi(8,5) = grid%start_hour

   !---------------------------------------------------------------------------
   ! WRF-specific fields:
   !---------------------------------------------------------------------------

   ptop = grid%p_top
   ps0  = 100000.0    ! Base sea level pressure
   ts0  = 300.0       ! Base potential temperature for all levels.
   tis0 = 290.0       ! Base sea level temperature
   tlp  = 50.0        ! temperature difference from 1000mb to 300mb

   grid%xb%sigmaf(ke+1) = grid%em_znw(ke+1)

   grid%xb%znw(ke+1) = grid%em_znw(ke+1)
   grid%xb%znu(ke+1) = 0.0
 
   do k=ks,ke
      grid%xb%sigmah(k) = grid%em_znu(k)
      grid%xb%sigmaf(k) = grid%em_znw(k)

      grid%xb%znu(k) = grid%em_znu(k)
      grid%xb%znw(k) = grid%em_znw(k)
      grid%xb%dn(k)  = grid%em_dn(k)
      grid%xb%dnw(k) = grid%em_dnw(k)
   enddo

   grid%xb % ptop = ptop
   grid%xb % ps0  = ps0
   grid%xb % ts0  = ts0
   grid%xb % tlp  = tlp
      
   !---------------------------------------------------------------------------
   ! Convert WRF fields to xb:
   !---------------------------------------------------------------------------

   if (print_detail_xb) then
      write(unit=stdout, fmt='(3a, i8)') &
         'file:', __FILE__, ', line:', __LINE__

      write(unit=stdout, fmt=*) 'is,ie=', is,ie
      write(unit=stdout, fmt=*) 'js,je=', js,je
      write(unit=stdout, fmt=*) 'ks,ke=', ks,ke

      write(unit=stdout, fmt='(/5a/)') &
         'lvl         dnw                dn 		rdnw       rdn'

      do k=ks,ke+1
         write(unit=stdout, fmt='(i3,8f16.8)') k, &
            grid%em_dnw(k), grid%em_dn(k), grid%em_rdnw(k), grid%em_rdn(k)
      enddo

      write(unit=stdout, fmt='(/5a/)') &
         'lvl         znu                 znw		rdnw       rdn'

      do k=ks,ke+1
         write(unit=stdout, fmt='(i3,8f16.8)') k, &
            grid%xb%sigmah(k), grid%xb%sigmaf(k), grid%em_rdnw(k), grid%em_rdn(k)
      enddo

      write(unit=stdout, fmt='(/5a/)') &
         'lvl         phb                 ph_2'

      do k=ks,ke
         write(unit=stdout, fmt='(i3,8e20.12)') k, &
               grid%em_phb(is,js,k), grid%em_ph_2(is,js,k)
      enddo

      write(unit=stdout, fmt='(/5a/)') ' simple variables:'

      if (je == grid%xp%jde) then
         write(unit=stdout, fmt=*) ' '

         do k=ks+5,ke,10
            do i=is,ie,10
               write(unit=stdout, fmt='(3(a,i5),a,e20.12)') &
                    'grid%em_v_2(', i, ',', grid%xp%jde+1, ',', k, ')=', &
                     grid%em_v_2(i, grid%xp%jde+1,k)
            enddo
            write(unit=stdout, fmt=*) ' '
         enddo
      endif

      if (ie == grid%xp%ide) then
         write(unit=stdout, fmt=*) ' '

         do k=ks+5,ke,10
            do j=js,je,10
               write(unit=stdout, fmt='(3(a,i5),a,e20.12)') &
                  'grid%em_u_2(', grid%xp%ide+1, ',', j, ',', k, ')=', &
                  grid%em_u_2(grid%xp%ide+1,j,k)
            enddo
            write(unit=stdout, fmt=*) ' '
         enddo
      endif

      write(unit=stdout, fmt='(/5a/)') ' simple variables:'

      write(unit=stdout, fmt=*) ' grid%em_u_1(is,js,ks)=',  grid%em_u_1(is,js,ks)
      write(unit=stdout, fmt=*) ' grid%em_v_1(is,js,ks)=',  grid%em_v_1(is,js,ks)
      write(unit=stdout, fmt=*) ' grid%em_w_1(is,js,ks)=',  grid%em_w_1(is,js,ks)
      write(unit=stdout, fmt=*) ' grid%em_t_1(is,js,ks)=',  grid%em_t_1(is,js,ks)
      write(unit=stdout, fmt=*) 'grid%em_ph_1(is,js,ks)=', grid%em_ph_1(is,js,ks)


      write(unit=stdout, fmt=*) ' grid%em_u_2(is,je,ks)=',  grid%em_u_2(is,je,ks)
      write(unit=stdout, fmt=*) ' grid%em_v_2(ie,js,ks)=',  grid%em_v_2(ie,js,ks)
      write(unit=stdout, fmt=*) ' grid%em_w_2(is,js,ks)=',  grid%em_w_2(is,js,ks)
      write(unit=stdout, fmt=*) ' grid%em_t_2(is,js,ks)=',  grid%em_t_2(is,js,ks)
      write(unit=stdout, fmt=*) 'grid%em_ph_2(is,js,ks)=', grid%em_ph_2(is,js,ks)
      write(unit=stdout, fmt=*) ' grid%em_phb(is,js,ks)=',  grid%em_phb(is,js,ks)

      write(unit=stdout, fmt=*) &
         'grid%sm31,grid%em31,grid%sm32,grid%em32, grid%sm33,grid%em33=', &
         grid%sm31,grid%em31,grid%sm32,grid%em32,grid%sm33,grid%em33

      write(unit=stdout, fmt=*) 'grid%p_top=', grid%p_top
      write(unit=stdout, fmt=*) 'grid%em_znu(ks)=', grid%em_znu(ks)
      write(unit=stdout, fmt=*) 'grid%em_mu0(is,js)=', grid%em_mu0(is,js)
      write(unit=stdout, fmt=*) 'grid%em_mub(is,js)=', grid%em_mub(is,js)
      write(unit=stdout, fmt=*) 'grid%em_mu_2(is,js)=', grid%em_mu_2(is,js)

      write(unit=stdout, fmt=*) 'hbot(is,js)=', grid%hbot(is,js)
      write(unit=stdout, fmt=*) 'htop(is,js)=', grid%htop(is,js)

      write(unit=stdout, fmt=*) 'grid%p_top=', grid%p_top
      write(unit=stdout, fmt=*) 'num_moist=', num_moist
      write(unit=stdout, fmt=*) 'P_QV=', P_QV

      write(unit=stdout, fmt=*) 'moist(is,js,ks,2)=', moist(is,js,ks,2)
      write(unit=stdout, fmt=*) ' '
   endif

   !---------------------------------------------------------------
   ! Need this to exchange values in the halo region.
   ! grid%xa%u and grid%xa%v are used as tempory arrays and so
   ! it is easy to use the existing exchange scheme.
   !
   ! Note, this is needed as u_2 and v_2 has no guarantee
   ! the most east column, and the most north raw are
   ! properly initailized for each tile.
   !                            Wei Huang, 03/06/03
   !---------------------------------------------------------------

   do j=js,je
      do k=ks,ke
         do i=is,ie+1
            grid%xa%u(i,j,k) = grid%em_u_2(i,j,k)
         enddo
      enddo
   enddo

   do j=js,je+1
      do k=ks,ke
         do i=is,ie
            grid%xa%v(i,j,k) = grid%em_v_2(i,j,k)
         enddo
      enddo
   enddo

   ! Fill the halo region for u and v.

   call wrf_dm_halo(grid%xp%domdesc,grid%xp%comms,grid%xp%halo_id3)


   if (print_detail_xb) then
      write(unit=stdout, fmt=*) 'is,ie,js,je,ks,ke=', is,ie,js,je,ks,ke
      write(unit=stdout, fmt=*) 'grid%xp%its, grid%xp%ite=', grid%xp%its, grid%xp%ite
      write(unit=stdout, fmt=*) 'grid%xp%jts, grid%xp%jte=', grid%xp%jts, grid%xp%jte
      write(unit=stdout, fmt=*) 'grid%xp%kts, grid%xp%kte=', grid%xp%kts, grid%xp%kte
      write(unit=stdout, fmt=*) 'grid%xp%ims, grid%xp%ime=', grid%xp%ims, grid%xp%ime
      write(unit=stdout, fmt=*) 'grid%xp%jms, grid%xp%jme=', grid%xp%jms, grid%xp%jme
      write(unit=stdout, fmt=*) 'grid%xp%kms, grid%xp%kme=', grid%xp%kms, grid%xp%kme
      write(unit=stdout, fmt=*) 'grid%xp%ids, grid%xp%ide=', grid%xp%ids, grid%xp%ide
      write(unit=stdout, fmt=*) 'grid%xp%jds, grid%xp%jde=', grid%xp%jds, grid%xp%jde
      write(unit=stdout, fmt=*) 'grid%xp%kds, grid%xp%kde=', grid%xp%kds, grid%xp%kde

      write(unit=stdout, fmt=*) 'size(grid%xb%u, dim=1)=', size(grid%xb%u, dim=1)
      write(unit=stdout, fmt=*) 'size(grid%xb%u, dim=2)=', size(grid%xb%u, dim=2)
      write(unit=stdout, fmt=*) 'size(grid%xb%u, dim=3)=', size(grid%xb%u, dim=3)
      write(unit=stdout, fmt=*) 'size(grid%xb%v, dim=1)=', size(grid%xb%v, dim=1)
      write(unit=stdout, fmt=*) 'size(grid%xb%v, dim=2)=', size(grid%xb%v, dim=2)
      write(unit=stdout, fmt=*) 'size(grid%xb%v, dim=3)=', size(grid%xb%v, dim=3)

      write(unit=stdout, fmt=*) 'size(grid%xa%u, dim=1)=', size(grid%xa%u, dim=1)
      write(unit=stdout, fmt=*) 'size(grid%xa%u, dim=2)=', size(grid%xa%u, dim=2)
      write(unit=stdout, fmt=*) 'size(grid%xa%u, dim=3)=', size(grid%xa%u, dim=3)
      write(unit=stdout, fmt=*) 'size(grid%xa%v, dim=1)=', size(grid%xa%v, dim=1)
      write(unit=stdout, fmt=*) 'size(grid%xa%v, dim=2)=', size(grid%xa%v, dim=2)
      write(unit=stdout, fmt=*) 'size(grid%xa%v, dim=3)=', size(grid%xa%v, dim=3)

      write(unit=stdout, fmt=*) 'size(grid%em_u_2, dim=1)=', size(grid%em_u_2, dim=1)
      write(unit=stdout, fmt=*) 'size(grid%em_u_2, dim=2)=', size(grid%em_u_2, dim=2)
      write(unit=stdout, fmt=*) 'size(grid%em_u_2, dim=3)=', size(grid%em_u_2, dim=3)
      write(unit=stdout, fmt=*) 'size(grid%em_v_2, dim=1)=', size(grid%em_v_2, dim=1)
      write(unit=stdout, fmt=*) 'size(grid%em_v_2, dim=2)=', size(grid%em_v_2, dim=2)
      write(unit=stdout, fmt=*) 'size(grid%em_v_2, dim=3)=', size(grid%em_v_2, dim=3)
   endif

   do j=js,je
      k = ke+1

      do i=is,ie
         grid%em_p(i,j,k) = 0.0
         grid%xb%map_factor(i,j) = grid%msft(i,j)
         grid%xb%cori(i,j) = grid%f(i,j)
         grid%xb%tgrn(i,j) = grid%sst(i,j)
         if(grid%xb%tgrn(i,j) < 100.0) grid%xb%tgrn(i,j) = grid%tmn(i,j)
         grid%xb%lat(i,j) = grid%xlat(i,j)
         grid%xb%lon(i,j) = grid%xlong(i,j)
         grid%xb%terr(i,j) = grid%ht(i,j)
         grid%xb%snow(i,j) = grid%snowc(i,j)
         grid%xb%lanu(i,j) = grid%lu_index(i,j)
         grid%xb%landmask(i,j) = grid%landmask(i,j)
         grid%xb%xland(i,j) = grid%xland(i,j)
         ! Z. Liu below are variables used by RTTOV
         grid%xb%tsk(i,j) = grid%tsk(i,j)
         grid%xb%smois(i,j) = grid%smois(i,j,1)
         grid%xb%tslb(i,j) = grid%tslb(i,j,1)
         grid%xb%xice(i,j) = grid%xice(i,j)
         grid%xb%ivgtyp(i,j) = grid%ivgtyp(i,j)
         grid%xb%isltyp(i,j) = grid%isltyp(i,j)
         grid%xb%vegfra(i,j) = grid%vegfra(i,j)
         ! convert snow water equivalent kg/m2 to snow depth mm
         grid%xb%snowh(i,j) = grid%snow(i,j)*10.     
      enddo

#if 0
      ! Adapted the code from "real.init.code" by Y.-R. Guo 05/13/2004:

      do i=is,ie
         k = ke
         qvf1 = 0.5*(grid%moist(i,j,k,P_QV)+grid%moist(i,j,k,P_QV))
         qvf2 = 1./(1.+qvf1)
         qvf1 = qvf1*qvf2
         grid%xb%p(i,j,k) = -0.5*(grid%mu_2(i,j)+qvf1*grid%mub(i,j))/grid%em_rdnw(k)/qvf2

         do k = ke-1,1,-1
            qvf1 = 0.5*(moist(i,j,k,P_QV)+grid%moist(i,j,k+1,P_QV))
            qvf2 = 1./(1.+qvf1)
            qvf1 = qvf1*qvf2
            grid%p(i,j,k) = grid%p(i,j,k+1) - (grid%mu_2(i,j)+qvf1*grid%mub(i,j))/qvf2/rdn(k+1)
         enddo
      enddo
#endif

      ! Adapted the code from WRF module_big_step_utilities_em.F ----
      !         subroutine calc_p_rho_phi      Y.-R. Guo (10/20/2004)

      cvpm =  - (1. - gas_constant/cp)
      cpovcv = cp / (cp - gas_constant)

      do k=ks,ke
         do i=is,ie
            ! The base specific volume (from real.init.code)
            ppb  = grid%em_znu(k) * grid%em_mub(i,j) + ptop
            ttb  = (tis0 + tlp*log(ppb/ps0)) * (ps0/ppb)**kappa
            albn = (gas_constant/ps0) * ttb * (ppb/ps0)**cvpm

            qvf1 = 1. + moist(i,j,k,P_QV) / rd_over_rv
            aln  = -1. / (grid%em_mub(i,j)+grid%em_mu_2(i,j)) * ( albn*grid%em_mu_2(i,j) &
                + grid%em_rdnw(k) *(grid%em_ph_2(i,j,k+1) - grid%em_ph_2(i,j,k)) )
            ! total pressure:
            grid%xb%p(i,j,k) = ps0 * ( (gas_constant*(ts0+grid%em_t_2(i,j,k))*qvf1) / &
                            (ps0*(aln+albn)) )**cpovcv
            ! total density
            grid%xb%rho(i,j,k)= 1.0 / (albn+aln)
            ! pressure purtubation:
            grid%em_p(i,j,k) = grid%xb%p(i,j,k) - ppb
         enddo
      enddo

      do k=ks,ke+1
         do i=is,ie
            grid%xb%hf(i,j,k) = (grid%em_phb(i,j,k)+grid%em_ph_2(i,j,k))/gravity
            grid%xa%w (i,j,k) = grid%em_w_2(i,j,k)
            grid%xb%w (i,j,k) = grid%em_w_2(i,j,k)
         enddo
       enddo

      do k=ks,ke
         do i=is,ie
            grid%xb%u(i,j,k) = 0.5*(grid%xa%u(i,j,k)+grid%xa%u(i+1,j,k))
            grid%xb%v(i,j,k) = 0.5*(grid%xa%v(i,j,k)+grid%xa%v(i,j+1,k))
            grid%xb%wh(i,j,k)= 0.5*(grid%xb%w(i,j,k)+grid%xb%w(i,j,k+1))
            grid%xb%h(i,j,k) = 0.5*(grid%xb%hf(i,j,k)+grid%xb%hf(i,j,k+1))

            grid%xb%q(i,j,k) = moist(i,j,k,P_QV)

            theta = ts0 + grid%em_t_2(i,j,k)
            grid%xb%t(i,j,k) = theta*(grid%xb%p(i,j,k)/ps0)**kappa

            ! Convert to specific humidity from mixing ratio of water vapor:
            grid%xb%q(i,j,k)=grid%xb%q(i,j,k)/(1.0+grid%xb%q(i,j,k))

            if (grid%xb%q(i,j,k) < 1.0e-6) &
               grid%xb%q(i,j,k) = 1.0e-6
   
            ! Background qrn needed for Radar Radial velocity assimilation:

            if (size(moist,dim=4) >= 4 ) then
               grid%xb%qcw(i,j,k) = moist(i,j,k,p_qc)
               grid%xb%qrn(i,j,k) = moist(i,j,k,p_qr)
               grid%xb%qt (i,j,k) = grid%xb%q(i,j,k) + grid%xb%qcw(i,j,k) + grid%xb%qrn(i,j,k)
            endif

            if (size(moist,dim=4) >= 6 ) then
               grid%xb%qci(i,j,k) = moist(i,j,k,p_qi)
               grid%xb%qsn(i,j,k) = moist(i,j,k,p_qs)
            endif

            if (size(moist,dim=4) >= 7 ) then
               grid%xb%qgr(i,j,k) = moist(i,j,k,p_qg)
            endif
         enddo
      enddo

      do i=is,ie
         grid%xb%psac(i,j) = grid%em_mub(i,j)+grid%em_mu_2(i,j)
         grid%xb%psfc(i,j) = grid%em_mub(i,j)+grid%em_p(i,j,ks)+grid%p_top

         if (grid%xb%tgrn(i,j) < 100.0) &
            grid%xb%tgrn(i,j) = grid%xb%t(i,j,ks)+0.0065*(grid%xb%h(i,j,ks+1)-grid%xb%hf(i,j,ks))
      enddo
   enddo

   grid%xb%ztop = grid%xb%hf(is,js,ke+1)

   if (print_detail_xb) then
      write(unit=stdout, fmt=*) ' '
      if (print_detail_xb) then
         write(unit=stdout, fmt='(/5a/)') &
            'lvl         h                 p                t'

         do k=ks,ke
            write(unit=stdout, fmt='(i3,8e20.12)') k, &
               grid%xb%h(is,js,k), grid%xb%p(is,js,k), grid%xb%t(is,js,k)
         enddo
      endif

      write(unit=stdout, fmt=*) ' '
      write(unit=stdout, fmt=*) 'grid%xb%u(is,je,ke)=', grid%xb%u(is,je,ke)
      write(unit=stdout, fmt=*) 'grid%xb%v(ie,js,ke)=', grid%xb%v(ie,js,ke)
      write(unit=stdout, fmt=*) 'grid%xb%w(is,js,ke)=', grid%xb%w(is,js,ke)
      write(unit=stdout, fmt=*) 'grid%xb%t(is,js,ke)=', grid%xb%t(is,js,ke)
      write(unit=stdout, fmt=*) 'grid%xb%p(is,js,ke)=', grid%xb%p(is,js,ke)
      write(unit=stdout, fmt=*) 'grid%xb%q(is,js,ke)=', grid%xb%q(is,js,ke)
      write(unit=stdout, fmt=*) 'grid%xb%h(is,js,ke)=', grid%xb%h(is,js,ke)
      write(unit=stdout, fmt=*) 'grid%xb%hf(is,js,ke)=', grid%xb%hf(is,js,ke)
      write(unit=stdout, fmt=*) 'grid%xb%map_factor(is,js)=', grid%xb%map_factor(is,js)
      write(unit=stdout, fmt=*) 'grid%xb%cori(is,js)=', grid%xb%cori(is,js)
      write(unit=stdout, fmt=*) 'grid%xb%tgrn(is,js)=', grid%xb%tgrn(is,js)
      write(unit=stdout, fmt=*) 'grid%xb%lat(is,js)=', grid%xb%lat(is,js)
      write(unit=stdout, fmt=*) 'grid%xb%lon(is,js)=', grid%xb%lon(is,js)
      write(unit=stdout, fmt=*) 'grid%xb%terr(is,js)=', grid%xb%terr(is,js)
      write(unit=stdout, fmt=*) 'grid%xb%snow(is,js)=', grid%xb%snow(is,js)
      write(unit=stdout, fmt=*) 'grid%xb%lanu(is,js)=', grid%xb%lanu(is,js)
      write(unit=stdout, fmt=*) 'grid%xb%landmask(is,js)=', grid%xb%landmask(is,js)
      write(unit=stdout, fmt=*) '(ie,je)=', ie,je                   !hcl
      write(unit=stdout, fmt=*) 'grid%xb%lat(ie,je)=', grid%xb%lat(ie,je)     !hcl
      write(unit=stdout, fmt=*) 'grid%xb%lon(ie,je)=', grid%xb%lon(ie,je)     !hcl
      write(unit=stdout, fmt=*) ' '
   endif

   !---------------------------------------------------------------------------
   ! [3.0] Calculate vertical inner product for use in vertical transform:
   !---------------------------------------------------------------------------
      
   IF ( vertical_ip == 1 ) THEN
      ! Vertical inner product is SQRT(Delta p):
      DO k=ks,ke
         grid%xb % vertical_inner_product(is:ie,js:je,k) = &
              SQRT( grid%xb % psac(is:ie,js:je) * grid%xb%sigmah(k) )
      END DO 
   ELSE IF ( vertical_ip == 2 ) THEN

      ! Vertical inner product is Delta p:
      DO k=1,grid%xb%mkz
         grid % xb % vertical_inner_product(is:ie,js:je,k) = &
         grid % xb % psac(is:ie,js:je) * grid%xb%sigmah(k)
      END DO
   END IF

   !---------------------------------------------------------------------------
   ! Roughness
   !---------------------------------------------------------------------------

   current_date = '2000-01-01_00:00:00'

   write(current_date(1:19), fmt='(i4.4, 5(a1, i2.2))') &
      grid%start_year, '-', &
      grid%start_month, '-', &
      grid%start_day, '_', &
      grid%start_hour, ':', &
      grid%start_minute, ':', &
      grid%start_second

   xbx % mminlu = 'USGS'

   call da_roughness_from_lanu(19, xbx % mminlu, current_date, grid%xp, &
#ifdef DEREF_KLUDGE
      grid%xb % lanu(grid%xp%ims,grid%xp%jms), grid%xb % rough(grid%xp%ims,grid%xp%jms))
#else
      grid%xb % lanu, grid%xb % rough)
#endif

   !---------------------------------------------------------------------------
   ! Calculate 1/grid box areas:
   !---------------------------------------------------------------------------

   if (print_detail_xb) then
      write(unit=stdout, fmt='(/a, e24.16)') &
         'grid%xb % ds=', grid%xb % ds

      write(unit=stdout, fmt='(a, e24.16/)') &
           'grid%xb % map_factor(is,js)=', grid%xb % map_factor(is,js)
   endif

   do j=js,je
      do i=is,ie
         if ( grid%xb%ztop < grid%xb%hf(i,j,ke+1) ) &
             grid%xb%ztop = grid%xb%hf(i,j,ke+1)

         tmpvar = grid%xb%ds / grid%xb%map_factor(i,j)

         grid%xb % grid_box_area(i,j) = tmpvar*tmpvar

         ! Calculate surface variable(wind, moisture, temperature)
         ! sfc variables: 10-m wind, and 2-m T, Q, at cross points

         call da_sfc_wtq(grid%xb%psfc(i,j), grid%xb%tgrn(i,j), &
                      grid%xb%p(i,j,ks), grid%xb%t(i,j,ks), grid%xb%q(i,j,ks), &
                      grid%xb%u(i,j,ks), grid%xb%v(i,j,ks), &
                      grid%xb%p(i,j,ks+1), grid%xb%t(i,j,ks+1), grid%xb%q(i,j,ks+1), &
                      grid%xb%h(i,j,ks), grid%xb%rough(i,j),grid%xb%xland(i,j), &
                      grid%xb%u10(i,j), grid%xb%v10(i,j), grid%xb%t2(i,j), grid%xb%q2(i,j), &
                      grid%xb%regime(i,j))
      enddo
   enddo

   !---------------------------------------------------------------------------
   ! Calculate saturation vapour pressure and relative humidity:
   !---------------------------------------------------------------------------

   do j=js,je
      do k=ks,ke
         do i=is,ie
            call da_tpq_to_rh( grid%xb % t(i,j,k), grid%xb % p(i,j,k), grid%xb % q(i,j,k), &
                         grid%xb %es(i,j,k), grid%xb %qs(i,j,k), grid%xb %rh(i,j,k) )
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! Calculate dew point temperature:
   !---------------------------------------------------------------------------

#ifdef DEREF_KLUDGE
   call da_trh_to_td(grid%xb % rh(grid%xp%ims,grid%xp%jms,grid%xp%kms), &
                     grid%xb %  t(grid%xp%ims,grid%xp%jms,grid%xp%kms), &
                     grid%xb % td(grid%xp%ims,grid%xp%jms,grid%xp%kms), grid%xp)
#else
   call da_trh_to_td(grid%xb % rh, grid%xb % t, grid%xb % td, grid%xp)
#endif

   if (print_detail_xb) then
      i=is; j=js; k=ks

      write(unit=stdout, fmt=*) 'i,j,k=', i,j,k
      write(unit=stdout, fmt=*) 'grid%xb % td(i,j,k)=', grid%xb % td(i,j,k)
      write(unit=stdout, fmt=*) 'grid%xb % es(i,j,k)=', grid%xb % es(i,j,k)
      write(unit=stdout, fmt=*) 'grid%xb % rh(i,j,k)=', grid%xb % rh(i,j,k)
      write(unit=stdout, fmt=*) 'grid%xb % qs(i,j,k)=', grid%xb % qs(i,j,k)
      write(unit=stdout, fmt=*) ' '
   endif

   !---------------------------------------------------------------------------
   ! Sea level pressure and total precipitable water
   !---------------------------------------------------------------------------

   if (print_detail_xb) then
      write(unit=stdout, fmt='(3a, i8)') &
          'file:', __FILE__, ', line:', __LINE__

     write(unit=stdout, fmt='(a)') &
          'Sea level pressure and total precipitable water'
   endif

   call da_wrf_tpq_2_slp ( grid%xb )

   ! do j = js,je
   !    do i = is,ie
   !       call da_tpq_to_slp( grid%xb%t(i,j,:), grid%xb%q(i,j,:), grid%xb%p(i,j,:), &
   !          grid%xb%terr(i,j), &
   !                        grid%xb%psfc(i,j), grid%xb%slp(i,j), grid%xp )
   !    end do
   ! end do

   call da_integrat_dz( grid%xb )

   !---------------------------------------------------------------------------
   ! Surface Wind speed
   !---------------------------------------------------------------------------

   if (print_detail_xb) then
      write(unit=stdout, fmt='(3a, i8)') &
         'file:', __FILE__, ', line:', __LINE__

      write(unit=stdout, fmt='(a)') &
         'Surface Wind speed'
   endif

   tmpvar = log(10./0.0001)

   do j=js,je
      do i=is,ie
         rgh_fac(i,j) = 1.0/log(grid%xb%h(i,j,ks)/0.0001)

         grid%xb%speed(i,j) = sqrt(grid%xb%u(i,j,ks)*grid%xb%u(i,j,ks) &
                         + grid%xb%v(i,j,ks)*grid%xb%v(i,j,ks) + 1.0e-6) &
                    *tmpvar*rgh_fac(i,j)
      enddo
   enddo

   !---------------------------------------------------------------------------
   ! Brightness temperature SH Chen
   !---------------------------------------------------------------------------

   if (print_detail_xb) then
      write(unit=stdout, fmt='(3a, i8)') &
         'file:', __FILE__, ', line:', __LINE__

      write(unit=stdout, fmt='(a)') &
          'Brightness temperature'
   endif

   if ( Use_SsmiTbObs)   &
      call da_transform_xtotb( grid%xb )

   !---------------------------------------------------------------------------
   ! GPS Refractivity linked by Y.-R. Guo 05/28/2004
   !---------------------------------------------------------------------------

   if (print_detail_xb) then
      write(unit=stdout, fmt='(3a, i8)') &
         'file:', __FILE__, ', line:', __LINE__

      write(unit=stdout, fmt='(a)') &
         'GPS Refractivity'
   endif

   call da_transform_xtogpsref(grid%xb, grid%xp)

   !---------------------------------------------------------------------------
   ! Ground-based GPS ZTD must follow the GPS Refractivity calculation.
   !---------------------------------------------------------------------------

   ! if (Use_GpsztdObs) call da_transform_xtoztd( grid%xb )

   !---------------------------------------------------------------------------
   ! Calculate means for later use in setting up background errors.
   !---------------------------------------------------------------------------

   ! if (.not. associated(xbx % latc_mean)) then
   allocate ( xbx % latc_mean(grid%xp%jds:grid%xp%jde) )
   if (trace_use) call da_trace("da_transfer_wrftoxb",&
      message="allocated xbx%latc_mean")
   ! endif

   size2d = (grid%xp%ide-grid%xp%ids+1)*(grid%xp%jde-grid%xp%jds+1)

   tmpvar = 1.0/real(size2d)


   ! Bitwise-exact reduction preserves operation order of serial code for
   ! testing, at the cost of much-increased run-time.  Turn it off when not
   ! testing.  This will always be .FALSE. for a serial or 1-process MPI run.
   IF ( testing_dm_exact ) THEN
      ALLOCATE( arrayglobal( grid%xp%ids:grid%xp%ide, grid%xp%jds:grid%xp%jde ) )
      call da_wv_patch_to_global( grid%xb%psac, arrayglobal, grid%xp%domdesc, &
                               grid%xp%ids, grid%xp%ide, grid%xp%jds, grid%xp%jde,   &
                               grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme,   &
                               grid%xp%its, grid%xp%ite, grid%xp%jts, grid%xp%jte )
      loc_psac_mean = tmpvar*SUM( arrayglobal(grid%xp%ids:grid%xp%ide,grid%xp%jds:grid%xp%jde) )
      DEALLOCATE( arrayglobal )
   ELSE
      loc_psac_mean = tmpvar*SUM( grid%xb % psac(grid%xp%its:grid%xp%ite,grid%xp%jts:grid%xp%jte) )
   ENDIF

   tmpvar = 1.0/real(grid%xp%ide-grid%xp%ids+1)

   IF ( testing_dm_exact ) THEN
      ALLOCATE( arrayglobal( grid%xp%ids:grid%xp%ide, grid%xp%jds:grid%xp%jde ) )
      call da_wv_patch_to_global( grid%xb%lat, arrayglobal, grid%xp%domdesc, &
                               grid%xp%ids, grid%xp%ide, grid%xp%jds, grid%xp%jde,  &
                               grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme,  &
                               grid%xp%its, grid%xp%ite, grid%xp%jts, grid%xp%jte )
      do j=grid%xp%jds,grid%xp%jde
         loc_latc_mean(j) = tmpvar*SUM( arrayglobal(grid%xp%ids:grid%xp%ide, j) )
      enddo
      DEALLOCATE( arrayglobal )
   ELSE
      do j=js,je
         loc_latc_mean(j) = tmpvar*SUM( grid%xb % lat(is:ie, j) )
      enddo
   ENDIF

   IF ( testing_dm_exact ) THEN
      ! Broadcast result from monitor to other tasks.
      call wrf_dm_bcast_real( loc_psac_mean, 1 )
      xbx % psac_mean = loc_psac_mean
      ! Broadcast result from monitor to other tasks.
      call wrf_dm_bcast_real( loc_latc_mean, (grid%xp%jde-grid%xp%jds+1) )
      xbx % latc_mean = loc_latc_mean
   ELSE
      xbx % psac_mean = wrf_dm_sum_real( loc_psac_mean )
      call wrf_dm_sum_reals( loc_latc_mean, xbx % latc_mean )
   ENDIF

   if (print_detail_xb) then
      ! write(unit=stdout, fmt=*) 'loc_psac_mean  =', loc_psac_mean
      write(unit=stdout, fmt=*) 'xbx % psac_mean=', xbx % psac_mean

      ! write(unit=stdout, fmt=*) 'loc_latc_mean  =', loc_latc_mean(js)
      write(unit=stdout, fmt=*) 'xbx % latc_mean=', xbx % latc_mean(js)
   endif

   ! Calculate time step from one dimensional cloud model parameterization

   IF ( DT_cloud_model ) THEN
      do j = grid%xp%jts, grid%xp%jte
         do i = grid%xp%its, grid%xp%ite
            call da_cloud_model ( grid%xb%t(I,J,:),   grid%xb%p(I,J,:), grid%xb%q(I,J,:), &
               grid%xb%qcw(I,J,:),  &
                             grid%xb%qrn(I,J,:), grid%xb%h(I,J,:), grid%xb%hf(I,J,:),     &
                             DDT, grid%xp%kts, grid%xp%kte  )

            do k = grid%xp%kts, grid%xp%kte
               grid%xb%delt(i,j,k) = DDT(k)
            enddo
         enddo
      enddo
   ENDIF

   IF (trace_use) call da_trace_exit("da_transfer_wrftoxb")

end subroutine da_transfer_wrftoxb

