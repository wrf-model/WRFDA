SUBROUTINE da_transfer_xbtowrf( xbx, grid, config_flags, &
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

   TYPE (grid_config_rec_type), intent(inout) :: config_flags

!  Definitions of dummy arguments to solve

#include <em_dummy_decl.inc>

!---------------------------------------------------------------------------
   REAL , PARAMETER :: sea_level_pressure_base    = 100000.
   REAL , PARAMETER :: sea_level_temperature_base =    290.
   REAL , PARAMETER :: temp_diff_1000_to_300_mb   =     50.

   REAL , PARAMETER :: g            = 9.81  ! acceleration due to gravity (m {s}^-2)
   REAL , PARAMETER :: r_d          = 287.
   REAL , PARAMETER :: r_v          = 461.6
   REAL , PARAMETER :: cp           = 7.*r_d/2.
   REAL , PARAMETER :: cv           = cp-r_d
   REAL , PARAMETER :: cpv          = 4.*r_v
   REAL , PARAMETER :: cvv          = cpv-r_v
   REAL , PARAMETER :: cvpm         = -cv/cp
   REAL , PARAMETER :: cliq         = 4190.
   REAL , PARAMETER :: cice         = 2106.
   REAL , PARAMETER :: psat         = 610.78
   REAL , PARAMETER :: rcv          = r_d/cv
   REAL , PARAMETER :: rcp          = r_d/cp
   REAL , PARAMETER :: rovg         = r_d/g
   REAL , PARAMETER :: c2           = cp * rcv

   REAL , PARAMETER :: p1000mb      = 100000.
   REAL , PARAMETER :: t0           = 300.
   REAL , PARAMETER :: p0           = p1000mb
   REAL , PARAMETER :: cpovcv       = cp/(cp-r_d)
   REAL , PARAMETER :: cvovcp       = 1./cpovcv
   REAL , PARAMETER :: rvovrd       = r_v/r_d

   integer, parameter :: P_QV=2
!---------------------------------------------------------------------------
   integer :: i, j, k, n

   integer :: is, ie, js, je, ks, ke
!---------------------------------------------------------------------------

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   REAL    :: qvf1, qvf2, qvf

   real    :: p00, t00, a, p_surf

   real, dimension(xp%ims:xp%ime, xp%jms:xp%jme, xp%kms:xp%kme) :: p_wet
   real, dimension(xp%ims:xp%ime, xp%jms:xp%jme) :: psdmptop

!---------------------------------------------------------------------------

   p00 = sea_level_pressure_base
   t00 = sea_level_temperature_base
   a   = temp_diff_1000_to_300_mb

!---------------------------------------------------------------------------
!--SET ARRAY RANGE INDICES FOR PROCESSOR SUBDOMAIN.
!---------------------------------------------------------------------------

   is = xp % its
   ie = xp % ite
   js = xp % jts
   je = xp % jte
   ks = xp % kts
   ke = xp % kte

!------------------------------------------------------------------------------
!  mixing ratio of moisture
!------------------------------------------------------------------------------

   znw(ke+1) = xb%znw(ke+1)

   do k=ke,ks,-1
      znu(k) = xb%znu(k)
      znw(k) = xb%znw(k)

      dnw(k) = znw(k+1)-znw(k)
      rdnw(k) = 1.0/dnw(k)

      do j=js,je
      do i=is,ie
         moist_2(i,j,k,P_QV) = xb%q(i,j,k)/(1.0 - xb%q(i,j,k))
      enddo
      enddo
   enddo

   do k=ks+1,ke
      dn(k) = 0.5*(dnw(k)+dnw(k-1))
      rdn(k) = 1.0/dn(k)
      fnp(k) = .5* dnw(k  )/dn(k)
      fnm(k) = .5* dnw(k-1)/dn(k)
   enddo

   if(print_detail > 0) then
     write(unit=*, fmt='(3a, i8)') &
          'file:', "./DA_Setup_Structures/da_transfer_wrftoxb.inc", ', line:', 117

     print *, 'is,ie=', is,ie
     print *, 'js,je=', js,je
     print *, 'ks,ke=', ks,ke

     if(print_detail > 10) then
     write(unit=*, fmt='(/5a/)') &
          'lvl         dnw                dn            rdnw       rdn'

     do k=ks,ke+1
        write(unit=*, fmt='(i3,8f16.8)') k, &
              dnw(k), dn(k), rdnw(k), rdn(k)
     enddo

     write(unit=*, fmt='(/5a/)') &
          'lvl         znu                 znw          rdnw       rdn'

     do k=ks,ke+1
        write(unit=*, fmt='(i3,8f16.8)') k, &
              xb%znu(k), xb%znw(k), rdnw(k), rdn(k)
     enddo
     endif
     print *, ' '
   endif

!------------------------------------------------------------------------------
!  dry-column air mass per unit area
!------------------------------------------------------------------------------

   do j=js,je
   do i=is,ie
      p_wet(i,j,ke+1) = 0.0

      do k=ke,ks,-1
         p_wet(i,j,k) = p_wet(i,j,k+1) &
                      + xb%rho(i,j,k)*xb%q(i,j,k)*(xb%hf(i,j,k+1)-xb%hf(i,j,k))
      enddo

      psdmptop(i,j) = xb%psfc(i,j) - xb%ptop - p_wet(i,j,ks)
   enddo
   enddo

!------------------------------------------------------------------------------

!--CONVERT FROM A-GRID TO C-GRID

   do k=ks,ke
      do j=js,je
         do i=is+1,ie
            u_2(i,j,k)=0.5*(xb%u(i,j,k)+xb%u(i-1,j,k))
         enddo
         u_2(is,j,k)=2.0*xb%u(is+1,j,k)-xb%u(is+2,j,k)
         u_2(ie+1,j,k)=2.0*xb%u(ie,j,k)-xb%u(ie-1,j,k)
      enddo


      do j=js+1,je
         do i=is,ie
            v_2(i,j,k)=0.5*(xb%v(i,j,k)+xb%v(i,j-1,k))
         enddo
      enddo

      do i=is,ie
         u_2(i,js,k)=2.0*xb%u(i,js+1,k)-xb%u(i,js+2,k)
         u_2(i,je+1,k)=2.0*xb%u(i,je,k)-xb%u(i,je-1,k)
      enddo
   enddo

!------------------------------------------------------------------------------

   p_top = xb%ptop

   do j=js,je
   do i=is,ie
      !  Base state pressure is a function of eta level and terrain, only, plus
      !  the hand full of constants: p00 (sea level pressure, Pa), t00 (sea level
      !  temperature, K), and A (temperature difference, from 1000 mb to 300 mb, K).

      p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*ht(i,j)/a/r_d ) **0.5 )

      !  Base state mu is defined as base state surface pressure minus p_top

      mub(i,j) = p_surf - p_top

      !  Compute the perturbation mass and the full mass from the base surface pressure
      !  (a function of elevation) and the dry surface pressure.

      mu_2(i,j) = psdmptop(i,j)-mub(i,j)
      mu0(i,j) = psdmptop(i,j)

      do k=ks,ke
         pb(i,j,k) = znu(k)*mub(i,j) + p_top
         t_init(i,j,k) = (t00 + A*LOG(pb(i,j,k)/p00))*(p00/pb(i,j,k))**(r_d/cp) - t0
         xb%p(i,j,k) = xb%rho(i,j,k) * gas_constant * xb%t(i,j,k)
         t_2(i,j,k) = xb%t(i,j,k)*(p00/xb%p(i,j,k))**(r_d/cp) - t0
         alb(i,j,k) = (r_d/p1000mb)*(t_init(i,j,k)+t0)*(pb(i,j,k)/p1000mb)**cvpm
      enddo

      !  Integrate base geopotential, starting at terrain elevation.  This assures that
      !  the base state is in exact hydrostatic balance with respect to the model equations.
      !  This field is on full levels.

      phb(i,j,ks) = ht(i,j) * g
      DO k= ks+1,ke+1
         phb(i,j,k) = phb(i,j,k-1) - dnw(k-1)*mub(i,j)*alb(i,j,k-1)
      END DO
   enddo
   enddo

   !  Fill in the outer rows and columns to allow us to be sloppy.

   IF ( ie .EQ. xp%ide ) THEN
      i = xp%ide
      DO j = js, je
         mub(i,j) = mub(i-1,j)
         mu_2(i,j) = mu_2(i-1,j)
         phb(i,j,ke+1) = phb(i-1,j,ke+1)
         DO k = ks, ke
            pb(i,j,k) = pb(i-1,j,k)
            alb(i,j,k) = alb(i-1,j,k)
            phb(i,j,k) = phb(i-1,j,k)
            t_init(i,j,k) = t_init(i-1,j,k)
            t_2(i,j,k) = t_2(i-1,j,k)
         END DO
      END DO
   END IF

   IF ( je .EQ. xp%jde ) THEN
      j = xp%jde
      DO i = is, ie
         mub(i,j) = mub(i,j-1)
         mu_2(i,j) = mu_2(i,j-1)
         phb(i,j,ke+1) = phb(i,j-1,ke+1)
         DO k = ks, ke
            pb(i,j,k) = pb(i,j-1,k)
            alb(i,j,k) = alb(i,j-1,k)
            phb(i,j,k) = phb(i,j-1,k)
            t_init(i,j,k) = t_init(i,j-1,k)
            t_2(i,j,k) = t_2(i,j-1,k)
         END DO
      END DO
   END IF

   DO j = js, je
   DO i = is, ie
      !  Assign the potential temperature (perturbation from t0) and qv on all the mass
      !  point locations.

      !  Integrate the hydrostatic equation (from the RHS of the bigstep vertical momentum
      !  equation) down from the top to get the pressure perturbation.  First get the pressure
      !  perturbation, moisture, and inverse density (total and perturbation) at the top-most level.

      k = ke

      qvf1 = 0.5*(moist_2(i,j,k,P_QV)+moist_2(i,j,k,P_QV))
      qvf2 = 1./(1.+qvf1)
      qvf1 = qvf1*qvf2

      p(i,j,k) = - 0.5*(mu_2(i,j)+qvf1*mub(i,j))/rdnw(k)/qvf2
      qvf = 1. + rvovrd*moist_2(i,j,k,P_QV)

      alt(i,j,k) = (r_d/p1000mb)*(t_2(i,j,k)+t0)*qvf*(((p(i,j,k)+pb(i,j,k))/p1000mb)**cvpm)
      al(i,j,k) = alt(i,j,k) - alb(i,j,k)

      !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
      !  inverse density fields (total and perturbation).

      DO k=ke-1,ks,-1
         qvf1 = 0.5*(moist_2(i,j,k,P_QV)+moist_2(i,k+1,j,P_QV))
         qvf2 = 1./(1.+qvf1)
         qvf1 = qvf1*qvf2
         p(i,j,k) = p(i,j,k+1) - (mu_2(i,j) + qvf1*mub(i,j))/qvf2/rdn(k+1)
         qvf = 1. + rvovrd*moist_2(i,j,k,P_QV)
         alt(i,j,k) = (r_d/p1000mb)*(t_2(i,j,k)+t0)*qvf* &
                     (((p(i,j,k)+pb(i,j,k))/p1000mb)**cvpm)
         al(i,j,k) = alt(i,j,k) - alb(i,j,k)
      END DO

      !  This is the hydrostatic equation used in the model after the small timesteps.  In
      !  the model, al (inverse density) is computed from the geopotential.

      DO k= ks+1,ke+1
         ph_2(i,j,k) = ph_2(i,j,k-1) - &
                       dnw(k-1) * ((mub(i,j)+mu_2(i,j))*al(i,j,k-1) + mu_2(i,j)*alb(i,j,k-1))
         ph0(i,j,k) = ph_2(i,j,k) + phb(i,j,k)
      END DO

   END DO
   END DO

!---------------------------------------------------------------------------------------

   cen_lat  = phic
   cen_lon  = xlonc

   config_flags%map_proj = xb%map
   config_flags%cen_lat  = phic
   config_flags%cen_lon  = xlonc
   config_flags%truelat1 = truelat1_3dv
   config_flags%truelat2 = truelat2_3dv

   config_flags%moad_cen_lat = cen_lat
   config_flags%stand_lon    = cen_lon

   CALL set_map_proj ( grid%id , map_proj )
   CALL set_cen_lat ( grid%id , cen_lat )
   CALL set_cen_lon ( grid%id , cen_lon )
   CALL set_truelat1 ( grid%id , truelat1 )
   CALL set_truelat2 ( grid%id , truelat2 )

   CALL set_moad_cen_lat ( grid%id , moad_cen_lat )
   CALL set_stand_lon ( grid%id , stand_lon )

   CALL set_mminlu ( xbx%mminlu(1:4) )

!--Now the basic variables.

   do j=js,je
   do i=is,ie
      f(i,j) = xb%cori(i,j)

      ht(i,j) = xb%terr(i,j)

      sst(i,j) = xb%tgrn(i,j)
      tmn(i,j) = xb%tgrn(i,j)

      msft(i,j) = xb%map_factor(i,j)
      msfu(i,j) = xb%map_factor(i,j)
      msfv(i,j) = xb%map_factor(i,j)
      xlat(i,j) = xb%lat(i,j)

      xlong(i,j) = xb%lon(i,j)
      snowc(i,j) = xb%snow(i,j)
      xland(i,j) = xb%landmask(i,j)

      lu_index(i,j) = xb%lanu(i,j)
   enddo
   enddo

END SUBROUTINE da_transfer_xbtowrf

