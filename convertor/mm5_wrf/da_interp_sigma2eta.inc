subroutine da_interp_sigma2eta(xb)

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
   
   TYPE (xb_type), INTENT(INOUT)    :: xb      ! First guess state

   INTEGER                          :: is,ie   ! Range of 1st dimension of arrays.
   INTEGER                          :: js,je   ! Range of 2nd dimension of arrays.
   INTEGER                          :: ks,ke   ! Range of 3rd dimension of arrays.
   INTEGER                          :: i,j,k   ! Loop counter
   INTEGER                          :: kz, kep1, kzp1

   real, dimension(xb%kts:xb%kte)   :: us, vs, ts, qs, rhos, ue, ve, te, qe, rhoe
   real, dimension(xb%kts:xb%kte+1) :: ws, we, hs, he

   real, dimension(xb%kts:xb%kte)   :: ps_h_dry, ps_h_wet, pe_h_dry, pe_h_wet, pps_h
   real, dimension(xb%kts:xb%kte+1) :: ps_f_dry, ps_f_wet, pe_f_dry, pe_f_wet, pps_f

   real :: mub

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

   do j=js,je
   do i=is,ie
      ws(kep1) = xb%w(i,j,kep1)
      we(kep1) = ws(kep1)
      hs(kep1) = xb%hf(i,j,kep1)
      he(kep1) = hs(kep1)

      do k=ks,ke
         us(k) = xb%u(i,j,k)
         vs(k) = xb%v(i,j,k)
         ts(k) = xb%t(i,j,k)
         qs(k) = xb%q(i,j,k)
         ws(k) = xb%w(i,j,k)
         hs(k) = xb%hf(i,j,k)

         rhos(k) = xb%rho(i,j,k)

         pps_h(k) = xb%p(i,j,k) - xb%psac(i,j)*xb%znu(k) - xb%ptop
      enddo

      pps_f(ks) = pps_h(ks)
      pps_f(kep1) = 0.0

      do k=ks, ke-1
         pps_f(k+1) = 0.5*(pps_h(k) + pps_h(k+1))
      enddo

      ps_f_wet(kep1) = 0.0
      ps_f_dry(kep1) = xb%ptop

      do k=ke,ks,-1
         ps_f_wet(k) = ps_f_wet(k+1) + xb%rho(i,j,k)*qs(k)*(hs(k+1)-hs(k))
         ps_f_dry(k) = xb%psac(i,j)*xb%znw(k)+xb%ptop+pps_f(k) - ps_f_wet(k+1)
 
         ps_h_wet(k) = 0.5*(ps_f_wet(k)+ps_f_wet(k+1))
         ps_h_dry(k) = xb%p(i,j,k) - ps_h_wet(k)
      enddo

      mub = ps_f_dry(ks) - xb%ptop

      do k=ks, ke
         pe_f_dry(k) = mub*xb%znw(k) + xb%ptop
         pe_h_dry(k) = mub*xb%znu(k) + xb%ptop
      enddo

      if(print_detail > 0) then
         if(i == is .and. j == js) then
            do k=ks,ke
               write(unit=*, fmt='(a, 3i3, 2x, a, 10f10.2)') &
                    'mm5_wrf: i,j,k=', i,j,k, &
                    'u, v, w, t, q, h, ps, pe, p=', us(k), vs(k), ws(k), ts(k), qs(k), hs(k), &
                     ps_h_dry(k), pe_h_dry(k), xb%p(i,j,k)
            enddo

            write(unit=*, fmt='(a, i3,  a, f10.2)') 'hs(', kep1, ')=', hs(kep1)
         endif
      endif

      pe_f_dry(kep1) = xb%ptop

      call da_convertor_v_interp(us(xb%kts:xb%kte), ps_h_dry(xb%kts:xb%kte), kz, &
                                 ue(xb%kts:xb%kte), pe_h_dry(xb%kts:xb%kte), kz)

      call da_convertor_v_interp(vs(xb%kts:xb%kte), ps_h_dry(xb%kts:xb%kte), kz, &
                                 ve(xb%kts:xb%kte), pe_h_dry(xb%kts:xb%kte), kz)

      call da_convertor_v_interp(ts(xb%kts:xb%kte), ps_h_dry(xb%kts:xb%kte), kz, &
                                 te(xb%kts:xb%kte), pe_h_dry(xb%kts:xb%kte), kz)

      call da_convertor_v_interp(qs(xb%kts:xb%kte), ps_h_dry(xb%kts:xb%kte), kz, &
                                 qe(xb%kts:xb%kte), pe_h_dry(xb%kts:xb%kte), kz)

      call da_convertor_v_interp(rhos(xb%kts:xb%kte), ps_h_dry(xb%kts:xb%kte), kz, &
                                 rhoe(xb%kts:xb%kte), pe_h_dry(xb%kts:xb%kte), kz)

      call da_convertor_v_interp(hs(xb%kts:xb%kte+1), ps_f_dry(xb%kts:xb%kte+1), kzp1, &
                                 he(xb%kts:xb%kte+1), pe_f_dry(xb%kts:xb%kte+1), kzp1)

      call da_convertor_v_interp(ws(xb%kts:xb%kte+1), ps_f_dry(xb%kts:xb%kte+1), kzp1, &
                                 we(xb%kts:xb%kte+1), pe_f_dry(xb%kts:xb%kte+1), kzp1)

      do k=ks, ke
         xb%u(i,j,k) = ue(k)
         xb%v(i,j,k) = ve(k)
         xb%t(i,j,k) = te(k)
         xb%q(i,j,k) = qe(k)
         xb%w(i,j,k) = we(k)

         xb%hf(i,j,k) = he(k)

         xb%rho(i,j,k) = rhoe(k)
      enddo

      xb%w (i,j,kep1) = we(kep1)
      xb%hf(i,j,kep1) = he(kep1)

      if(print_detail > 0) then
         if(i == is .and. j == js) then
            do k=ks,ke
               write(unit=*, fmt='(a, 3i4, 2x, a, 10f9.2)') &
                    'mm5_wrf: i,j,k=', i,j,k, &
                    'u, v, w, t, q, h, ps, pe, p=', ue(k), ve(k), we(k), te(k), qe(k), he(k), &
                     ps_h_dry(k), pe_h_dry(k), xb%p(i,j,k)
            enddo
         endif
      endif
   enddo
   enddo
  
end subroutine da_interp_sigma2eta

