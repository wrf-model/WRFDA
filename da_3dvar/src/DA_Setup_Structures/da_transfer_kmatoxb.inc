SUBROUTINE DA_Transfer_KMAToXb( xbx, grid, &
!
#include <em_dummy_args.inc>
!
                              )

!------------------------------------------------------------------------------
!  PURPOSE: Transfers fields from KMA to first guess (xb) structure.
!
!  HISTORY:   08/06/2004	                 Syed RH Rizvi
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
   real    :: tmpvar, tmpvar1, earth_radius_sq,conv
   real    :: tmpu,tmp_u,tmpv,tmp_v,tmpp,tmp_p,tmpps,tmp_ps
   real    :: ddx, ddy                                        
   REAL, DIMENSION(xp%ims:xp%ime,xp%jms:xp%jme)               :: rgh_fac
   character(len=19) :: current_date
! 
   REAL, DIMENSION(xp%kms:xp%kme) :: TV, ALPHA, PHALF, PHALFL                     

   real :: pu, pd, coef, delp, hydro, rgasg, shgt
!---------------------------------------------------------------------------

   real, dimension(xp%jds:xp%jde) :: loc_latc_mean

   integer :: ierror

!---------------------------------------------------------------------------

      conv = pi/180. 
      earth_radius_sq = earth_radius*1000. * earth_radius*1000. * &
                        conv*(360.0/(coarse_ix-1))*(180.0/(coarse_jy-2))*conv
      COEF=0.6080                                               
      RGASG = gas_constant/gravity                                            
!---------------------------------------------------------------------------
!--Set xb array range indices for processor subdomain.
!---------------------------------------------------------------------------
   is = xp % its
   ie = xp % ite
   js = xp % jts
   je = xp % jte
   ks = xp % kts
   ke = xp % kte

   xb % ds    = grid%dx
   xb % kma_a = grid%kma_a
   xb % kma_b = grid%kma_b

   if(print_detail > 0) then
     print *, ' '
     write(unit=*, fmt='(/a/)') &
          'lvl         kma_a                 kma_b'

     do k=ks,ke
        write(unit=*, fmt='(i3,8e20.12)') k, xb%kma_a(k), xb%kma_b(k)
     enddo
   endif

   xb % mix = xp % ide - xp % ids + 1
   xb % mjy = xp % jde - xp % jds + 1
   xb % mkz = xp % kde - xp % kds + 1

!---------------------------------------------------------------------------
!--KMA-specific fields:
!---------------------------------------------------------------------------
!   Fix ptop as 10 hPa.

   ptop = 1000.0     
   ps0  = 100000.0
   ts0  = 300.0
   tis0 = 0.0
   tlp  = 0.0

   xb % ptop = ptop
   xb % ps0  = ps0
   xb % ts0  = ts0
   xb % tlp  = tlp
      
!---------------------------------------------------------------------------
!--Convert KMA fields to xb:
!---------------------------------------------------------------------------

       xb%lat(is:ie,js:je) =  xlat(is:ie,js:je)
       xb%lon(is:ie,js:je) = xlong(is:ie,js:je)
!--Fill halo region for u and v.

   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)
   tmpvar  = 1.0/real(xp%ide-xp%ids+1)
!---------------------------------------------------------------
!  transfer u,v,t & q(specific humidity in g/g)
!---------------------------------------------------------------
  do k=ks,ke
   do j=js,je
     do i=is,ie
         xb%u(i,j,k) = u_2(i,j,k)
         xb%v(i,j,k) = v_2(i,j,k)
         xb%t(i,j,k) = t_2(i,j,k)
         xb%q(i,j,k) = moist(i,j,k,P_QV)
     end do
   end do
  end do
!---------------------------------------------------------------------------------
!  Fix wind at Poles
!---------------------------------------------------------------------------------
#ifndef DEREF_KLUDGE
    call DA_Get_VPoles(xb%u,xb%v,xb%lon,          &
       xp%ids,xp%ide,xp%jds,xp%jde,xp%kds,xp%kde, &
       xp%ims,xp%ime,xp%jms,xp%jme,xp%kms,xp%kme, &
       xp%its,xp%ite,xp%jts,xp%jte,xp%kts,xp%kte  )
#else
    call DA_Get_VPoles(xb%u(xp%ims,xp%jms,xb%kms),&
                       xb%v(xp%ims,xp%jms,xb%kms),&
                       xb%lon(xp%ims,xp%jms),     &
       xp%ids,xp%ide,xp%jds,xp%jde,xp%kds,xp%kde, &
       xp%ims,xp%ime,xp%jms,xp%jme,xp%kms,xp%kme, &
       xp%its,xp%ite,xp%jts,xp%jte,xp%kts,xp%kte  )

#endif
!---------------------------------------------------------------------------------
   if(print_detail > 0) then
   print *, 'xp%its, xp%ite=', xp%its, xp%ite
   print *, 'xp%jts, xp%jte=', xp%jts, xp%jte
   print *, 'xp%kts, xp%kte=', xp%kts, xp%kte
   print *, 'xp%ims, xp%ime=', xp%ims, xp%ime
   print *, 'xp%jms, xp%jme=', xp%jms, xp%jme
   print *, 'xp%kms, xp%kme=', xp%kms, xp%kme
   print *, 'xp%ids, xp%ide=', xp%ids, xp%ide
   print *, 'xp%jds, xp%jde=', xp%jds, xp%jde
   print *, 'xp%kds, xp%kde=', xp%kds, xp%kde

   print *, 'size(xb%u, dim=1)=', size(xb%u, dim=1)
   print *, 'size(xb%u, dim=2)=', size(xb%u, dim=2)
   print *, 'size(xb%u, dim=3)=', size(xb%u, dim=3)
   print *, 'size(xb%v, dim=1)=', size(xb%v, dim=1)
   print *, 'size(xb%v, dim=2)=', size(xb%v, dim=2)
   print *, 'size(xb%v, dim=3)=', size(xb%v, dim=3)

   print *, 'size(xa%u, dim=1)=', size(xa%u, dim=1)
   print *, 'size(xa%u, dim=2)=', size(xa%u, dim=2)
   print *, 'size(xa%u, dim=3)=', size(xa%u, dim=3)
   print *, 'size(xa%v, dim=1)=', size(xa%v, dim=1)
   print *, 'size(xa%v, dim=2)=', size(xa%v, dim=2)
   print *, 'size(xa%v, dim=3)=', size(xa%v, dim=3)

   print *, 'size(u_2, dim=1)=', size(u_2, dim=1)
   print *, 'size(u_2, dim=2)=', size(u_2, dim=2)
   print *, 'size(u_2, dim=3)=', size(u_2, dim=3)
   print *, 'size(v_2, dim=1)=', size(v_2, dim=1)
   print *, 'size(v_2, dim=2)=', size(v_2, dim=2)
   print *, 'size(v_2, dim=3)=', size(v_2, dim=3)
   endif
!
      ALPHA(ke) = LOG(2.0)                                           

!------------------------------------------------------------------------------
   do 600 j=js,je
     do 700 i=is,ie
!------------------------------------------------------------------------------
       xb%cori(i,j) = f(i,j)
       xb%psfc(i,j) = psfc(i,j)
       xb%terr(i,j) = ht(i,j)

!------------------------------------------------------------------------------
!  Syed RH Rizvi
! 
! The following code is to be activated after
! getting sst, land use, land mask and snow information for KMA grid
! This infor needs to be packed in "KMA2NETCDF" convertor
!                              
!------------------------------------------------------------------------------
!
#if 0
!------------------------------------------------------------------------------

      xb%tgrn(i,j) = sst(i,j)
      if(xb%tgrn(i,j) < 100.0) xb%tgrn(i,j) = tmn(i,j)
      xb%lanu(i,j)     = lu_index(i,j)
      xb%landmask(i,j) = landmask(i,j)
      xb%xland(i,j) = xland(i,j)
      xb%snow(i,j)     = snowc(i,j)
!------------------------------------------------------------------------------
!
#endif 
!------------------------------------------------------------------------------

! Since data is on full levels get full level pr & ht.
! Note p = A + B * Psfc formula needs Psfc to be in hPa 

      DO K = ks,ke-1    
          PU  = xb%KMA_A(K+1) + xb%KMA_B(K+1)*xb%psfc(i,j)/100.0
          PD  = xb%KMA_A(K  ) + xb%KMA_B(K  )*xb%psfc(i,j)/100.0
          if(PU == PD )then
          print*,'PU, PD equal and so denominator will be zero'
          print*,' i, j, k = ',i,j,k
          print*,' KMA_A ',KMA_A(k),KMA_A(K+1)
          print*,' KMA_B ',KMA_B(k),KMA_B(K+1)
          print*,' psfc(Pa) = ',xb%psfc(i,j)
            stop 
          end if
          xb%p(i,j,k)= 100.0 * exp ( ( PD*LOG(PD)-PU*LOG(PU) )/(PD-PU) -1.0 )
      END DO  
        xb%p(i,j,ke) = 100.0*(xb%KMA_A(ke)+xb%KMA_B(ke)*xb%psfc(i,j)/100.0)/2.0
!

     do k=ks,ke
      if(xb%t(i,j,k) <= 0.0 ) then
        print*,i,j,k,' ---> Problem in  temp = ',i,j,k,xb%t(i,j,k)   
      stop
      end if

         xb%rho(i,j,k)=xb%p(i,j,k)/( gas_constant*  &
               (1.0+COEF*xb%q(I,J,K))*xb%t(I,J,K) )   
     enddo
!
!  compute full level height
!
       DO K = ks,ke    
        PHALF(K) = xb%KMA_A(K) + xb%KMA_B(K)*xb%psfc(I,J)/100.0                             
        TV   (K) = (1.0+COEF*xb%q(I,J,K))*xb%t(I,J,K)*rgasg    
       ENDDO                                                           

        DO K = ks,ke-1                                              
          DELP     = PHALF(K) - PHALF(K+1)                              
          PHALFL(K)= LOG(PHALF(K)/PHALF(K+1))                           
          ALPHA(K) = 1.0-PHALF(K+1)*PHALFL(K)/DELP                     
        ENDDO  

        SHGT = xb%terr(i,j)
        DO K = ks, ke                                               
          xb%h(I,J,K) = SHGT + ALPHA(K)*TV(K)                           
        ENDDO 

        HYDRO = 0.0                                                    
        DO K = ks+1, ke
          HYDRO = HYDRO + TV(K-1)*PHALFL(K-1)                           
          xb%h(I,J,K) = xb%h(I,J,K) + HYDRO                             
        ENDDO                                                        

!------------------------------------------------------------------------------
  700   CONTINUE                                                        
  600   CONTINUE                                                        
!------------------------------------------------------------------------------
!  Sigma values are needed for interpolating 
!  background error statistics in vertical
!  Fix sigmah values based on global average surface pressure 
!                    and level wise pressure
!------------------------------------------------------------------------------

  tmp_ps  = sum(xb%psfc(is:ie,js:je)) /real((xp%ide-xp%ids+1)*(xp%jde-xp%jds+1))

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE(tmp_ps,tmpps,1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, IERROR )
#else
   tmpps = tmp_ps                    
#endif

 do k=ks,ke
  tmp_p  = sum(xb%p(is:ie,js:je,k)) /real((xp%ide-xp%ids+1)*(xp%jde-xp%jds+1))
#ifdef DM_PARALLEL
   call MPI_ALLREDUCE(tmp_p,tmpp,1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, IERROR )
#else
   tmpp = tmp_p                    
#endif

! 0.01 is added to see that sigmah should not become negative
  xb%sigmah(ke+ks-k) = (tmpp - ptop+0.01 ) / (tmpps- ptop+0.01 ) 
  if( xb%sigmah(ke+ks-k) < 0.0)then
   print*,' average surface pressure = ',tmpps  
   print*,' average pressure at this level= ',k,' = ',tmpp  
   print*,k,' negative sigmah(',ke+ks-k,') = ',xb%sigmah(ke+ks-k) 
   stop
  end if
 end do
!
!Fix ztop
!
   xb%ztop = xb%h(is,js,ke)

   if(print_detail > 0) then
     print *, ' '
     if(print_detail > 10) then
     write(unit=*, fmt='(/5a/)') &
          'lvl         h                 p                t'

     do k=ks,ke
        write(unit=*, fmt='(i3,8e20.12)') k, &
              xb%h(is,js,k), xb%p(is,js,k), xb%t(is,js,k)
     enddo
     endif

     print *, ' '
     print *, 'xb%u(is,je,ke)=', xb%u(is,je,ke)
     print *, 'xb%v(ie,js,ke)=', xb%v(ie,js,ke)
     print *, 'xb%w(is,js,ke)=', xb%w(is,js,ke)
     print *, 'xb%t(is,js,ke)=', xb%t(is,js,ke)
     print *, 'xb%p(is,js,ke)=', xb%p(is,js,ke)
     print *, 'xb%q(is,js,ke)=', xb%q(is,js,ke)
     print *, 'xb%hf(is,js,ke)=', xb%hf(is,js,ke)
     print *, 'xb%map_factor(is,js)=', xb%map_factor(is,js)
     print *, 'xb%cori(is,js)=', xb%cori(is,js)
     print *, 'xb%tgrn(is,js)=', xb%tgrn(is,js)
     print *, 'xb%lat(is,js)=', xb%lat(is,js)
     print *, 'xb%lon(is,js)=', xb%lon(is,js)
     print *, 'xb%terr(is,js)=', xb%terr(is,js)
     print *, 'xb%snow(is,js)=', xb%snow(is,js)
     print *, 'xb%lanu(is,js)=', xb%lanu(is,js)
     print *, 'xb%landmask(is,js)=', xb%landmask(is,js)
     print *, ' '
   endif

!------------------------------------------------------------------------------
         write(current_date(1:19), fmt='(i4.4, 5(a1, i2.2))') &
         grid%start_month, '-', &
         grid%start_day,   '_', &
         grid%start_hour,  ':', &
         grid%start_minute,':', &
         grid%start_second

  print*,'Current date is ',current_date 

!------------------------------------------------------------------------------
!  Syed RH Rizvi
! 
! Following code for calculating roughness length needs to be activated  
! after getting land use data for KMA grid 
!                              
!------------------------------------------------------------------------------
!
#if 0
!------------------------------------------------------------------------------
!  Set proper value for "mminlu" if using KMA info
!
  xbx % mminlu = 'USGS'
!------------------------------------------------------------------------------
  call roughness_from_lanu(19, xbx % mminlu, current_date, xp, &
#ifdef DEREF_KLUDGE
                            xb % lanu(xp%ims,xp%jms), xb % rough(xp%ims,xp%jms))
#else
                            xb % lanu, xb % rough)
#endif
!------------------------------------------------------------------------------
!
# endif
!------------------------------------------------------------------------------
   do j=js,je
   do i=is,ie
      if( xb%ztop < xb%hf(i,j,ke+1) ) xb%ztop = xb%hf(i,j,ke+1)
!------------------------------------------------------------------------------
! Calculate grid_box area and vertical inner product for use in vertical transform:
!------------------------------------------------------------------------------
      xb % grid_box_area(i,j) = earth_radius_sq * cos(xlat(i,j)*conv)
   IF ( vertical_ip == 1 ) THEN

!     Vertical inner product is SQRT(Delta p):
      DO k=1,xb%mkz
      xb % vertical_inner_product(i,j,k) = SQRT(xb%p(i,j,k)-xb%p(i,j,k+1))
      END DO

   ELSE IF ( vertical_ip == 2 ) THEN

!     Vertical inner product is Delta p:
      DO k=1,xb%mkz
      xb % vertical_inner_product(i,j,k) = xb%p(i,j,k)-xb%p(i,j,k+1)
      END DO

   END IF
!------------------------------------------------------------------------------
!  Syed RH Rizvi
! 
! Following code for calculating roughness length needs to be activated  
! to calculate surface variables (10-m wind, and 2-m T, Q) , 
! After testing KMA-3DVAR for SFC_ASSIM_OPTIONS = 2
!
#if 0
!------------------------------------------------------------------------------

      call sfc_wtq(xb%psfc(i,j), xb%tgrn(i,j), &
                   xb%p(i,j,ks), xb%t(i,j,ks), xb%q(i,j,ks), &
                   xb%u(i,j,ks), xb%v(i,j,ks), &
                   xb%p(i,j,ks+1), xb%t(i,j,ks+1), xb%q(i,j,ks+1), &
                   xb%h(i,j,ks), xb%rough(i,j),xb%landmask(i,j), &
                   xb%u10(i,j), xb%v10(i,j), xb%t2(i,j), xb%q2(i,j), &
                   xb%regime(i,j))
!------------------------------------------------------------------------------
!
# endif
!------------------------------------------------------------------------------
   enddo
   enddo
!------------------------------------------------------------------------------
!--Calculate saturation vapour pressure and relative humidity:
!------------------------------------------------------------------------------
   do j=js,je
   do k=ks,ke
   do i=is,ie
      call DA_TPQ_To_RH( xb % t(i,j,k), xb % p(i,j,k), xb % q(i,j,k), &
                         xb %es(i,j,k), xb %qs(i,j,k), xb %rh(i,j,k) )
   end do
   end do
   end do

!------------------------------------------------------------------------------
!--Calculate dew point temperature:
!------------------------------------------------------------------------------

#ifdef DEREF_KLUDGE
   call DA_TRH_To_TD(xb % rh(xp%ims,xp%jms,xp%kms), &
                     xb %  t(xp%ims,xp%jms,xp%kms), &
                     xb % td(xp%ims,xp%jms,xp%kms), xp)
#else
   call DA_TRH_To_TD(xb % rh, xb % t, xb % td, xp)
#endif

   if(print_detail > 10) then
      i=is; j=js; k=ks

      print *, 'i,j,k=', i,j,k
      print *, 'xb % td(i,j,k)=', xb % td(i,j,k)
      print *, 'xb % es(i,j,k)=', xb % es(i,j,k)
      print *, 'xb % rh(i,j,k)=', xb % rh(i,j,k)
      print *, 'xb % qs(i,j,k)=', xb % qs(i,j,k)
      print *, ' '
   endif

!------------------------------------------------------------------------------
!--Sea level pressure and total precipitable water
!------------------------------------------------------------------------------

   if(print_detail > 0) then
     write(unit=*, fmt='(3a, i8)') &
          'file:', __FILE__, ', line:', __LINE__

     write(unit=*, fmt='(a)') &
          'Sea level pressure and total precipitable water'
   endif

!------------------------------------------------------------------------------
!  Syed RH Rizvi
! 
! Following code for calculating roughness length needs to be activated  
! for calculating roughness length if sea level pressure is desired
!
#if 0
   call DA_WRF_TPQ_2_SLP ( xb )
!------------------------------------------------------------------------------
!
#endif
!------------------------------------------------------------------------------
   call INTEGRAT_dZ( xb )
!------------------------------------------------------------------------------
!  Syed RH Rizvi
! 
! Following code for calculating roughness length needs to be activated  
! for surface wind speed for SFC_ASSIM_OPTIONS = 2 
! 
! It is not working because KMA terrain height output from wave to grid 
! transform is negative at many grid points
!
#if 0
!------------------------------------------------------------------------------
   if(print_detail > 0) then
     write(unit=*, fmt='(3a, i8)') &
          'file:', __FILE__, ', line:', __LINE__

     write(unit=*, fmt='(a)') &
          'Surface Wind speed'
   endif

   tmpvar = log(10./0.0001)
   do j=js,je
   do i=is,ie
      if(xb%hf(i,j,ks) <= 0) then
      print*,' zero hf at i/j ',i,j,' ks= ',ks
      stop
      end if
      rgh_fac(i,j) = 1.0/log(xb%hf(i,j,ks)/0.0001)

      xb%speed(i,j) = sqrt(xb%u(i,j,ks)*xb%u(i,j,ks) &
                         + xb%v(i,j,ks)*xb%v(i,j,ks) + 1.0e-6) &
                    *tmpvar*rgh_fac(i,j)
   enddo
   enddo
!------------------------------------------------------------------------------
!
#endif
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Syed RH Rizvi
! 
! Following code for calculating roughness length needs to be activated  
! if SSMI brightness temperature are used
! 
#if 0
!------------------------------------------------------------------------------

  if(print_detail > 0) then
   write(unit=*, fmt='(3a, i8)') &
   'file:', __FILE__, ', line:', __LINE__

    write(unit=*, fmt='(a)') &
         'Brightness temperature'
 endif
!
  call DA_Transform_XToTb( xa )
!------------------------------------------------------------------------------
!
#endif
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!--Calculate means for later use in setting up background errors.
!------------------------------------------------------------------------------
   allocate ( xbx % latc_mean(xp%jds:xp%jde) )

   tmpvar = 1.0/real(xp%ide-xp%ids+1)
   loc_latc_mean(:) = 0.0

   do j=js,je
      loc_latc_mean(j) = tmpvar*SUM( xb % lat(is:ie, j) )
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( loc_latc_mean, xbx % latc_mean, (xp%jde-xp%jds+1), &
                       MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, IERROR )
#else
   xbx % latc_mean = loc_latc_mean
#endif

END SUBROUTINE DA_Transfer_KMAToXb

