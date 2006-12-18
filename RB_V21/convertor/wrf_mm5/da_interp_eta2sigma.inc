subroutine da_interp_eta2sigma(xb)

!------------------------------------------------------------------------------
!  PURPOSE: convert fields from MM5 to WRF first guess structure.
!
!  METHOD:  
!
!  HISTORY: 07/23/2004 - Creation of F90 version.
!           Wei Huang
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xb_type), INTENT(INOUT)      :: xb           ! First guess state

   INTEGER                            :: is,ie   ! Range of 1st dimension of arrays.
   INTEGER                            :: js,je   ! Range of 2nd dimension of arrays.
   INTEGER                            :: ks,ke   ! Range of 3rd dimension of arrays.
   INTEGER                            :: i,j,k   ! Loop counter
   INTEGER                            :: kz, kep1, kzp1

   real, dimension(xb%kts:xb%kte)     :: us, vs, ts, qs, hs, ue, ve, te, qe, he
   real, dimension(xb%kts:xb%kte+1)   :: ws, we, fs, fe

   real, dimension(xb%kts:xb%kte)     :: sigmah
   real, dimension(xb%kts:xb%kte+1)   :: sigmaf

   real :: pf, ph

!--Set xb array range indices for processor subdomain.

   is = xb%its
   ie = xb%ite
   js = xb%jts
   je = xb%jte
   ks = xb%kts
   ke = xb%kte

   kz = ke-ks+1
  
   kep1 = ke+1
   kzp1 = kz+1

!---------------------------------------------------------------------------

   do k=ks, ke
      sigmah(k) = xb%znu(ke+ks-k)
   enddo

   do k=ks, kep1
      sigmaf(k) = xb%znw(kep1+ks-k)
   enddo

!---------------------------------------------------------------------------

   do j=js,je
   do i=is,ie
      ws(kep1) = xb%w(i,j,kep1)
      fs(kep1) = xb%hf(i,j,kep1)

      do k=ks,ke
         us(k) = xb%u(i,j,k)
         vs(k) = xb%v(i,j,k)
         ts(k) = xb%t(i,j,k)
         qs(k) = xb%q(i,j,k)
         ws(k) = xb%w(i,j,k)

         fs(k) = xb%hf(i,j,k)
         hs(k) = xb%h (i,j,k)

         pf = xb%psac(i,j)*sigmaf(k) + xb%ptop
         ph = xb%psac(i,j)*sigmah(k) + xb%ptop

         call DA_Ref_height(pf, fe(k))
         call DA_Ref_height(ph, he(k))
      enddo

      pf = xb%psac(i,j)*sigmaf(kep1) + xb%ptop

      call DA_Ref_height(pf, fe(kep1))

      if(print_detail > 0) then
         if(i == is .and. j == js) then
            do k=ks,ke
               write(unit=*, fmt='(a, 3i3, 2x, a, 10f10.2)') &
                    'wrf_mm5: i,j,k=', i,j,k, &
                    'u, v, w, t, q, hs, fs=', us(k), vs(k), ws(k), ts(k), qs(k), hs(k), fs(k), xb%p(i,j,k)
            enddo

            write(unit=*, fmt='(a, i3, a, f10.2)') 'fs(', kep1, ')=', fs(kep1)
            write(unit=*, fmt='(a, 3i3, a, f10.2)') 'xb%hf(', i, j, kep1, ')=', xb%hf(i,j,kep1)
         endif
      endif

      call da_convertor_v_interp( us(xb%kts:xb%kte), hs(xb%kts:xb%kte), kz, &
                                  ue(xb%kts:xb%kte), he(xb%kts:xb%kte), kz)
      call da_convertor_v_interp( vs(xb%kts:xb%kte), hs(xb%kts:xb%kte), kz, &
                                  ve(xb%kts:xb%kte), he(xb%kts:xb%kte), kz)
      call da_convertor_v_interp( ts(xb%kts:xb%kte), hs(xb%kts:xb%kte), kz, &
                                  te(xb%kts:xb%kte), he(xb%kts:xb%kte), kz)
      call da_convertor_v_interp( qs(xb%kts:xb%kte), hs(xb%kts:xb%kte), kz, &
                                  qe(xb%kts:xb%kte), he(xb%kts:xb%kte), kz)

      call da_convertor_v_interp( ws(xb%kts:xb%kte+1), fs(xb%kts:xb%kte+1), kzp1, &
                                  we(xb%kts:xb%kte+1), fe(xb%kts:xb%kte+1), kzp1)

      do k=ks, ke
         xb%u(i,j,k) = ue(k)
         xb%v(i,j,k) = ve(k)
         xb%t(i,j,k) = te(k)
         xb%q(i,j,k) = qe(k)
         xb%w(i,j,k) = we(k)
      enddo

      xb%w(i,j,kep1) = we(kep1)

      if(print_detail > 0) then
         if(i == is .and. j == js) then
            do k=ks,ke
               write(unit=*, fmt='(a, 3i3, 2x, a, 10f10.2)') &
                    'wrf_mm5: i,j,k=', i,j,k, &
                    'u, v, w, t, q, he, fe, p=', ue(k), ve(k), we(k), te(k), qe(k), he(k), fe(k), xb%p(i,j,k)
            enddo

            write(unit=*, fmt='(a, i3, a, f10.2)') 'fe(', kep1, ')=', fe(kep1)
         endif
      endif

   enddo
   enddo
  
end subroutine da_interp_eta2sigma

