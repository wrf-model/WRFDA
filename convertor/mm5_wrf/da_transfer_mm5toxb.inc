SUBROUTINE DA_Transfer_MM5ToXb( xp, xb_mm5, xb, xbx )

!------------------------------------------------------------------------------
!  PURPOSE: Transfers fields from MM5 to first guess structure.
!
!  METHOD:  
!
!  HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xpose_type), intent(in)      :: xp           ! Domain decomposition vars.
   TYPE (mm5_model_type), INTENT(IN)  :: xb_mm5       ! MM5 model structure.
   TYPE (xb_type), INTENT(INOUT)      :: xb           ! First guess state
   TYPE (xbx_type),INTENT(INOUT)      :: xbx          ! Header & non-gridded vars.

   INTEGER                            :: is,ie   ! Range of 1st dimension of arrays.
   INTEGER                            :: js,je   ! Range of 2nd dimension of arrays.
   INTEGER                            :: ks,ke   ! Range of 3rd dimension of arrays.
   INTEGER                            :: i,j,k,n ! Loop counter

   REAL                               :: rpf, rph

! Set xb array range indices for processor subdomain.
   is = xp%its
   ie = xp%ite
   js = xp%jts
   je = xp%jte
   ks = xp%kts
   ke = xp%kte

   xb%map  = xb_mm5%big_header%bhi(7,1)
   xb%ds   = xb_mm5%big_header%bhr(9,1)

   print *, 'is,ie=', is,ie
   print *, 'js,je=', js,je
   print *, 'ks,ke=', ks,ke
   print *, 'xb%map, xb%ds=', xb%map, xb%ds

!---------------------------------------------------------------------------
!  [1.0] MM5-specific fields:
!---------------------------------------------------------------------------

   ptop = xb_mm5%big_header%bhr(2,2)
   ps0  = xb_mm5%big_header%bhr(2,5)
   ts0  = xb_mm5%big_header%bhr(3,5)
   tlp  = xb_mm5%big_header%bhr(4,5)
   tis0 = xb_mm5%big_header%bhr(5,5)

   xb%znw(ks) = 1.0

   do k=ks,ke
      xb%znu(k) = xb_mm5%sigmah(ke+ks-k)
      xb%znw(k+1) = 2.0*xb%znu(k) - xb%znw(k)
   enddo

   xb%ptop = xb_mm5%big_header%bhr(2,2)
   xb%ps0  = xb_mm5%big_header%bhr(2,5)
   xb%ts0  = xb_mm5%big_header%bhr(3,5)
   xb%tlp  = xb_mm5%big_header%bhr(4,5)
   xb%tis0 = xb_mm5%big_header%bhr(5,5)

   xb%num_of_var = xb_mm5%num_of_var
   xbx%mminlu = xb_mm5%big_header%bhic(23,1)(1:4)
   xbx%big_header = xb_mm5%big_header
   xbx%sub_header(:) = xb_mm5%sub_header(:)

   call DA_Ref_height(ptop, xb%ztop)

   do j=js,je
   do i=is,ie
      xb%hf(i,j,ke+1)    = xb%ztop
      xb%map_factor(i,j) = xb_mm5%msfc(j,i)
      xb%psac(i,j)  = xb_mm5%psac(j,i)
      xb%psfc(i,j)  = xb_mm5%pp_c(j,i,ke)+xb_mm5%psac(j,i)+ptop
      xb%cori (i,j) = xb_mm5%cori (j,i)
      xb%tgrn (i,j) = xb_mm5%tgrn (j,i)
      xb%lat  (i,j) = xb_mm5%latc (j,i)
      xb%lon  (i,j) = xb_mm5%lonc (j,i)
      xb%terr (i,j) = xb_mm5%terr (j,i)
      xb%lanu (i,j) = xb_mm5%lanu (j,i)
      xb%snow (i,j) = xb_mm5%snow (j,i)
      xb%landmask (i,j) = xb_mm5%landmask (j,i)
      xb%w(i,j,ks) = xb_mm5%w(j,i,ke+1)
   enddo
   enddo

   do k=ks,ke
      n = ke+ks-k
      do j=js,je
      do i=is,ie
         rpf =xb_mm5%psac(j,i)*xb%znw(k)+ptop
         rph =xb_mm5%psac(j,i)*xb%znu(k)+ptop
         call DA_Ref_height(rpf, xb%hf(i,j,k))
         call DA_Ref_height(rph, xb%h(i,j,k))

         xb%p(i,j,k) = xb_mm5%pp_c(j,i,n) + rph

         xb%u(i,j,k) = 0.25*(xb_mm5%u(j  ,i,n)+xb_mm5%u(j  ,i+1,k) &
                           + xb_mm5%u(j+1,i,n)+xb_mm5%u(j+1,i+1,k))
         xb%v(i,j,k) = 0.25*(xb_mm5%v(j  ,i,n)+xb_mm5%v(j  ,i+1,k) &
                           + xb_mm5%v(j+1,i,n)+xb_mm5%v(j+1,i+1,k))
         xb%t(i,j,k) = xb_mm5%t(j,i,n)
         xb%q(i,j,k) = xb_mm5%q(j,i,n)

         xb%w(i,j,k+1) = xb_mm5%w(j,i,n)

         xb%rho(i,j,k) = xb%p(i,j,k) / ( gas_constant * xb%t(i,j,k) )
      enddo
      enddo
   enddo

END SUBROUTINE DA_Transfer_MM5ToXb

