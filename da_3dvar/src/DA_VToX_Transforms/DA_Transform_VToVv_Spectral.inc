subroutine DA_Transform_VToVv_Spectral (xb, xbx, be, cv, vv, &
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



   integer :: i, j, k, cv_s, cv_e, r_cvsize 

   real, allocatable     :: rcv(:)      ! real local  control variable.
   real, allocatable     :: power(:)    ! local power spectrum
   real, allocatable     :: field(:,:)  ! local 2D-field      
!-------------------------------------------------------------------------
   allocate (rcv(1:2*xbx%c_cv_size) )
   allocate (power(0:xbx%max_wavenumber))
   allocate (field(1:xbx%ni , 1:xbx%nj) )
   r_cvsize = 2*xbx%c_cv_size
   cv_s = 1        
   if( be%v1%mz > 0)then 
    do k = 1, be%v1%mz
    cv_e = cv_s + r_cvsize - 1 
    rcv (1:r_cvsize) = cv(cv_s:cv_e) 
    power(0:xbx%max_wavenumber) = be%v1%power(0:xbx%max_wavenumber,k)
    call da_v_to_vv_spectral(xbx%ni,xbx%nj,xbx%max_wavenumber, &
                      xbx%inc,xbx%lenr,xbx%lensav, xbx%lenwrk, &
                      xbx%alp_size,r_cvsize,xbx%alp, xbx%wsave,&
                      power, rcv, field)
    vv%v1(1:xbx%ni,1:xbx%nj,k) = field(1:xbx%ni,1:xbx%nj)
    cv_s = cv_e + 1
    end do
   end if
!
   if( be%v2%mz > 0)then 
    do k = 1, be%v2%mz
    cv_e = cv_s + r_cvsize - 1 
    rcv (1:r_cvsize) = cv(cv_s:cv_e) 
    power(0:xbx%max_wavenumber) = be%v2%power(0:xbx%max_wavenumber,k)
    call da_v_to_vv_spectral(xbx%ni,xbx%nj,xbx%max_wavenumber, &
                      xbx%inc,xbx%lenr,xbx%lensav, xbx%lenwrk, &
                      xbx%alp_size,r_cvsize,xbx%alp, xbx%wsave,&
                      power, rcv, field)
    vv%v2(1:xbx%ni,1:xbx%nj,k) = field(1:xbx%ni,1:xbx%nj)
    cv_s = cv_e + 1
    end do
   end if
!
   if( be%v3%mz > 0)then 
    do k = 1, be%v3%mz
    cv_e = cv_s + r_cvsize - 1 
    rcv (1:r_cvsize) = cv(cv_s:cv_e) 
    power(0:xbx%max_wavenumber) = be%v3%power(0:xbx%max_wavenumber,k)
    call da_v_to_vv_spectral(xbx%ni,xbx%nj,xbx%max_wavenumber, &
                      xbx%inc,xbx%lenr,xbx%lensav, xbx%lenwrk, &
                      xbx%alp_size,r_cvsize,xbx%alp, xbx%wsave,&
                      power, rcv, field)
    vv%v3(1:xbx%ni,1:xbx%nj,k) = field(1:xbx%ni,1:xbx%nj)
    cv_s = cv_e + 1
    end do
   end if
!
   if( be%v4%mz > 0)then 
    do k = 1, be%v4%mz
    cv_e = cv_s + r_cvsize - 1 
    rcv (1:r_cvsize) = cv(cv_s:cv_e) 
    power(0:xbx%max_wavenumber) = be%v4%power(0:xbx%max_wavenumber,k)
    call da_v_to_vv_spectral(xbx%ni,xbx%nj,xbx%max_wavenumber, &
                      xbx%inc,xbx%lenr,xbx%lensav, xbx%lenwrk, &
                      xbx%alp_size,r_cvsize,xbx%alp, xbx%wsave,&
                      power, rcv, field)
    vv%v4(1:xbx%ni,1:xbx%nj,k) = field(1:xbx%ni,1:xbx%nj)
    cv_s = cv_e + 1
    end do
   end if
!
   if( be%v5%mz > 0)then 
    do k = 1, be%v5%mz
    cv_e = cv_s + r_cvsize - 1 
    rcv (1:r_cvsize) = cv(cv_s:cv_e) 
    power(0:xbx%max_wavenumber) = be%v5%power(0:xbx%max_wavenumber,k)
    call da_v_to_vv_spectral(xbx%ni,xbx%nj,xbx%max_wavenumber, &
                      xbx%inc,xbx%lenr,xbx%lensav, xbx%lenwrk, &
                      xbx%alp_size,r_cvsize,xbx%alp, xbx%wsave,&
                      power, rcv, field)
    vv%v5(1:xbx%ni,1:xbx%nj,k) = field(1:xbx%ni,1:xbx%nj)
    cv_s = cv_e + 1
    end do
   end if
!
end subroutine DA_Transform_VToVv_Spectral
