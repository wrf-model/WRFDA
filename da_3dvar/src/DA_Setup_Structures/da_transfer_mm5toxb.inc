SUBROUTINE DA_Transfer_MM5ToXb( ix, jy, kz, xp, xb_mm5, xb, xbx )

!------------------------------------------------------------------------------
!  PURPOSE: Transfers fields from MM5 to first guess structure.
!
!  METHOD:  
!
!  HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!           08/10/2004 - Change to WRF orientation.         Wei Huang
!           09/27/2004 - Updated Gps Refractivity           Syed RH Rizvi
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   INTEGER, intent(in)                :: ix           ! Array size.
   INTEGER, intent(in)                :: jy           ! Array size.
   INTEGER, intent(in)                :: kz           ! Array size.   
   TYPE (xpose_type), intent(in)      :: xp           ! Domain decomposition vars.
   TYPE (mm5_model_type), INTENT(IN)  :: xb_mm5       ! MM5 model structure.
   TYPE (xb_type), INTENT(INOUT)      :: xb           ! First guess state
   TYPE (xbx_type),INTENT(INOUT)      :: xbx          ! Header & non-gridded vars.

   INTEGER                            :: is,ie   ! Range of 1st dimension of arrays.
   INTEGER                            :: js,je   ! Range of 2nd dimension of arrays.
   INTEGER                            :: ks,ke   ! Range of 3rd dimension of arrays.
   INTEGER                            :: i,j,k,n ! Loop counter

   REAL                               :: tmpvar

   REAL, DIMENSION(xp%ims:xp%ime,xp%jms:xp%jme) :: rgh_fac

   real, dimension(xp%kms:xp%kme) :: DDT

   IF (trace_use) CALL da_trace_entry("da_transfer_mm5toxb")


! Set xb array range indices for processor subdomain.
   is = xp%its
   ie = xp%ite
   js = xp%jts
   je = xp%jte
   ks = xp%kts
   ke = xp%kte

   xb%map  = xb_mm5%big_header%bhi(7,1)
   xb%ds   = xb_mm5%big_header%bhr(9,1)

!---------------------------------------------------------------------------
!  [1.0] MM5-specific fields:
!---------------------------------------------------------------------------

   ptop = xb_mm5%big_header%bhr(2,2)
   ps0  = xb_mm5%big_header%bhr(2,5)
   ts0  = xb_mm5%big_header%bhr(3,5)
   tlp  = xb_mm5%big_header%bhr(4,5)
   tis0 = xb_mm5%big_header%bhr(5,5)

   if(print_detail > 0) then
      do k=ks,ke
         print *, 'xb_mm5%sigmah (', k, ')=', xb_mm5%sigmah (k)
      enddo
   endif

   xb%znw(ke+1) = 0.0
   xb%sigmaf(ke+1) = 0.0

   do k=ke,ks,-1
      n=ke+ks-k
      xb%znu(k) = xb_mm5%sigmah(n)

      xb%znw(k) = 2.0*xb%znu(k) - xb%znw(k+1)
      xb%dnw(k) = xb%znw(k+1) - xb%znw(k)

      xb%sigmah(k) = xb%znu(k)
      xb%sigmaf(k) = xb%znw(k)
   enddo

   xb%ptop = xb_mm5%big_header%bhr(2,2)
   xb%ps0  = xb_mm5%big_header%bhr(2,5)
   xb%tlp  = xb_mm5%big_header%bhr(4,5)
   xb%tis0 = xb_mm5%big_header%bhr(5,5)

   xb%num_of_var = xb_mm5%num_of_var
   xbx%mminlu = xb_mm5%big_header%bhic(23,1)(1:4)
   xbx%big_header = xb_mm5%big_header
   xbx%sub_header(:) = xb_mm5%sub_header(:)

!  Calculate means for later use in setting up background errors.  !ajb

   xbx%psac_mean = SUM( xb_mm5%psac(1:jy,1:ix) ) / REAL(ix*jy)

   allocate ( xbx%latc_mean(1:jy) )

   if(xb%map == 2) then
      do j = 1, jy    
        xbx%latc_mean(j) = SUM( xb_mm5%latc(1:jy,1:ix) ) / REAL(jy*ix)
      end do
   else
      do j = 1, jy    
        xbx%latc_mean(j) = SUM( xb_mm5%latc(j,1:ix) ) / REAL(ix)
      end do
   endif

!---------------------------------------------------------------------------
!  [2.0] Transfer across MM5 fields read in:
!---------------------------------------------------------------------------

   call DA_Ref_height(ptop, xb%ztop)

   do j=js,je
   do i=is,ie
      xb%psac(i,j) = xb_mm5%psac(j, i)

      do k=ks,ke
         call DA_Ref_height(xb%psac(i,j) * xb%znu(k) + ptop, xb%h(i,j,k))
         call DA_Ref_height(xb%psac(i,j) * xb%znw(k) + ptop, xb%hf(i,j,k))

         n=ks+kz-k
         xb%p(i,j,k) = xb_mm5%pp_c(j,i,n) + xb%Psac(i,j) * xb%znu(k) + ptop
         xb%u(i,j,k) = xb_mm5%u(j,i,n)
         xb%v(i,j,k) = xb_mm5%v(j,i,n)
         xb%t(i,j,k) = xb_mm5%t(j,i,n)
         xb%q(i,j,k) = xb_mm5%q(j,i,n)
         xb%qcw(i,j,k) = xb_mm5%qcw(j,i,n)
         xb%qrn(i,j,k) = xb_mm5%qrn(j,i,n)
         xb%qci(i,j,k) = xb_mm5%qci(j,i,n)
         xb%qsn(i,j,k) = xb_mm5%qsn(j,i,n)
         xb%qgr(i,j,k) = xb_mm5%qgr(j,i,n)
         xb%w(i,j,k) = xb_mm5%w(j,i,n+1)

         xb%rho(i,j,k) = xb%p(i,j,k) / ( gas_constant * xb%t(i,j,k) )
      enddo

      xb%hf(i,j,ke+1) = xb%ztop
      xb%w (i,j,ke+1) = xb_mm5%w (j,i,1)

      xb%psfc(i,j) = xb_mm5%pp_c(j,i,kz)+xb%psac(i,j)+ptop

      xb%lat(i,j)  = xb_mm5%latc (j,i)
      xb%lon(i,j)  = xb_mm5%lonc (j,i)
      xb%cori(i,j) = xb_mm5%cori (j,i)
      xb%tgrn(i,j) = xb_mm5%tgrn (j,i)
      xb%terr(i,j) = xb_mm5%terr (j,i)
      xb%lanu(i,j) = xb_mm5%lanu (j,i)
      xb%snow(i,j) = xb_mm5%snow (j,i)
      xb%landmask(i,j)   = xb_mm5%landmask(j,i)
      xb%map_factor(i,j) = xb_mm5%msfc(j,i)
   enddo
   enddo
  
!------------------------------------------------------------------------------
!  [3.0] Calculate vertical inner product for use in vertical transform:
!------------------------------------------------------------------------------
      
!  Use hydrostatic definition of deltap(i,j,k) = pstar(i,j) * abs(xb%dnw(k))

   IF ( vertical_ip == 1 ) THEN
   
!     Vertical inner product is SQRT(Delta p):
      DO k = ks, ke
         xb%vertical_inner_product(is:ie,js:je,k) = &
                                     SQRT( xb%psac(is:ie,js:je) * abs(xb%dnw(k)) ) 
      END DO
      
   ELSE IF ( vertical_ip == 2 ) THEN

!     Vertical inner product is Delta p:
      DO k = ks, ke
         xb%vertical_inner_product(is:ie,js:je,k) = &
                                           xb%psac(is:ie,js:je) * abs(xb%dnw(k))
      END DO
   
   END IF

!------------------------------------------------------------------------------
!  Calculate saturation vapour pressure and relative humidity:
!------------------------------------------------------------------------------

   do j = js,je
   do k = ks, ke
   do i = is,ie
      call DA_TPQ_To_RH( xb%t(i,j,k), xb%p(i,j,k), xb%q(i,j,k), &
                         xb%es(i,j,k), xb%qs(i,j,k), xb%rh(i,j,k) )
   end do
   end do
   end do
                          
!  Calculate dew point temperature:

   call DA_TRH_To_TD ( xb%rh, xb%t, xb%td, xp )

    IF ( cv_options_hum == 3 ) THEN

!  Total water qt=qv+qc+qr

    xb % qt (:,:,:) = xb % q (:,:,:) + xb % qcw (:,:,:) + xb % qrn (:,:,:)

    END IF

!  Sea level pressure and total precipitable water

   call DA_WRF_TPQ_2_SLP ( xb )

!  do j = js,je
!  do i = is,ie
!     call DA_TPQ_To_SLP( xb%t(i,j,:), xb%q(i,j,:), xb%p(i,j,:), xb%terr(i,j), &
!                        xb%psfc(i,j), xb%slp(i,j), xp )
!  end do
!  enddo

   CALL INTEGRAT_dZ ( xb )

!  Roughness

   call roughness_from_lanu(19, xbx%mminlu, xbx%sub_header(1)%current_date, &
                            xp, xb%lanu, xb%rough)

!  Surface Wind speed SH Chen

   tmpvar = log(10./0.0001)

   do j=js,je
   do i=is,ie
      rgh_fac(i,j) = 1.0/log(xb%h(i,j,ks)/0.0001)

      xb%speed(i,j) = sqrt(xb%u(i,j,ks)*xb%u(i,j,ks) &
                         + xb%v(i,j,ks)*xb%v(i,j,ks) + 1.0e-6) &
                    *tmpvar*rgh_fac(i,j)
   enddo
   enddo

!------------------------------------------------------------------------------
!--Brightness temperature
!------------------------------------------------------------------------------

   call DA_Transform_XToTb( xb )
!------------------------------------------------------------------------------
!  GpsRefractivity 
   call da_transform_XTogpsref(xb, xp)
!------------------------------------------------------------------------------


   do j=js,je
   do i=is,ie
      tmpvar = xb%ds / xb%map_factor(i,j)

      xb % grid_box_area(i,j) = tmpvar*tmpvar

!-----Calculate surface variable(wind, moisture, temperature)
!-----sfc variables: 10-m wind, and 2-m T, Q, at cross points

      call sfc_wtq(xb%psfc(i,j), xb%tgrn(i,j), &
                   xb%p(i,j,ks), xb%t(i,j,ks), xb%q(i,j,ks), &
                   xb%u(i,j,ks), xb%v(i,j,ks), &
                   xb%p(i,j,ks+1), xb%t(i,j,ks+1), xb%q(i,j,ks+1), &
                   xb%h(i,j,ks), xb%rough(i,j),xb%landmask(i,j), &
                   xb%u10(i,j), xb%v10(i,j), xb%t2(i,j), xb%q2(i,j), &
                   xb%regime(i,j))
   enddo
   enddo

!---Calculate time step from one dimensional cloud model parameterization-------------

   IF ( DT_cloud_model ) THEN

     do j = xp%jts, xp%jte
     do i = xp%its, xp%ite

       CALL DA_CLOUD_MODEL ( xb%t(I,J,:),   xb%p(I,J,:), xb%q(I,J,:), xb%qcw(I,J,:),  &
                             xb%qrn(I,J,:), xb%h(I,J,:), xb%hf(I,J,:),                &
                             DDT, xp%kts, xp%kte  )

     do k = xp%kts, xp%kte
       xb%delt(i,j,k) = DDT(k)
     enddo

     enddo
     enddo

   ENDIF

   IF (trace_use) CALL da_trace_exit("da_transfer_mm5toxb")


END SUBROUTINE DA_Transfer_MM5ToXb

