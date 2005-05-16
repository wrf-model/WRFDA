subroutine DA_Transform_VToVv_Global (xb, xbx, be, cv, vv, &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             its, ite, jts, jte, kts, kte )
!--------------------------------------------------------------------------
! Purpose   : Control variable transform for global 3DVAR 
!
! History   : Creation          12/02/2004     Syed RH Rizvi
!--------------------------------------------------------------------------
   implicit none

   type (xb_type), intent(in)           :: xb   ! first guess.
   type (xbx_type),intent(in)           :: xbx  ! For header & non-grid arrays.
   type (be_type), intent(in)           :: be   ! background errors.
   real, intent(in)                     :: cv(:)! control variable.
   type (vp_type),intent(out)           :: vv   ! grdipt/eof cv (local).
   integer, intent(in)              :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)              :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)              :: its,ite, jts,jte, kts,kte ! tile   dims

   integer :: i, j, k, cv_s, cv_e

!-------------------------------------------------------------------------
  
   cv_s = 1        
   do k = 1, be%v1%mz
      cv_e = cv_s + 2*xbx%c_cv_size - 1 
      call da_v_to_vv_spectral(xbx, be%v1%power(0:xbx%max_wavenumber,k), cv(cv_s:cv_e), vv%v1(ims:ime,jms:jme,k), &
                               ids, ide, jds, jde, &
                               ims, ime, jms, jme, &
                               its, ite, jts, jte )
      cv_s = cv_e + 1
   end do

   do k = 1, be%v2%mz
      cv_e = cv_s + 2*xbx%c_cv_size - 1
      call da_v_to_vv_spectral(xbx, be%v2%power(0:xbx%max_wavenumber,k), cv(cv_s:cv_e), vv%v2(ims:ime,jms:jme,k), &
                               ids, ide, jds, jde, &
                               ims, ime, jms, jme, &
                               its, ite, jts, jte )
      cv_s = cv_e + 1
   end do

   do k = 1, be%v3%mz
      cv_e = cv_s + 2*xbx%c_cv_size - 1
      call da_v_to_vv_spectral(xbx, be%v3%power(0:xbx%max_wavenumber,k), cv(cv_s:cv_e), vv%v3(ims:ime,jms:jme,k), &
                               ids, ide, jds, jde, &
                               ims, ime, jms, jme, &
                               its, ite, jts, jte )
      cv_s = cv_e + 1
   end do

   do k = 1, be%v4%mz
      cv_e = cv_s + 2*xbx%c_cv_size - 1
      call da_v_to_vv_spectral(xbx, be%v4%power(0:xbx%max_wavenumber,k), cv(cv_s:cv_e), vv%v4(ims:ime,jms:jme,k), &
                               ids, ide, jds, jde, &
                               ims, ime, jms, jme, &
                               its, ite, jts, jte )
      cv_s = cv_e + 1
   end do

   do k = 1, be%v5%mz
      cv_e = cv_s + 2*xbx%c_cv_size - 1
      call da_v_to_vv_spectral(xbx, be%v5%power(0:xbx%max_wavenumber,k), cv(cv_s:cv_e), vv%v5(ims:ime,jms:jme,k), &
                               ids, ide, jds, jde, &
                               ims, ime, jms, jme, &
                               its, ite, jts, jte )
      cv_s = cv_e + 1
   end do

end subroutine DA_Transform_VToVv_Global

