subroutine DA_Transform_VToVv_Spectral_Adj (xb, xbx, be, cv,vv, &
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
   real, intent(out)                    :: cv(:)! control variable.
   type (vp_type),intent(in)            :: vv   ! grdipt/eof cv (local).
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
!
   cv_s = 1        
   if( be%v1%mz > 0)then 
    do k = 1, be%v1%mz
    field(1:xbx%ni,1:xbx%nj) = vv%v1(1:xbx%ni,1:xbx%nj,k)
    power(0:xbx%max_wavenumber) = be%v1%power(0:xbx%max_wavenumber,k)
    call da_v_to_vv_spectral_adj(xbx%ni,xbx%nj,xbx%max_wavenumber, &
                      xbx%inc,xbx%lenr,xbx%lensav, xbx%lenwrk, &
                      xbx%alp_size,r_cvsize,xbx%alp, xbx%wsave,&
                      power, rcv, field)
    print*,'after 1'
    cv_e = cv_s + r_cvsize - 1 
    cv(cv_s:cv_e) = rcv (1:r_cvsize) 
    cv_s = cv_e + 1
    end do
   end if
!
   if( be%v2%mz > 0)then 
    do k = 1, be%v2%mz
    field(1:xbx%ni,1:xbx%nj) = vv%v2(1:xbx%ni,1:xbx%nj,k)
    power(0:xbx%max_wavenumber) = be%v2%power(0:xbx%max_wavenumber,k)
    print*,'before 2'
    call da_v_to_vv_spectral_adj(xbx%ni,xbx%nj,xbx%max_wavenumber, &
                      xbx%inc,xbx%lenr,xbx%lensav, xbx%lenwrk, &
                      xbx%alp_size,r_cvsize,xbx%alp, xbx%wsave,&
                      power, rcv, field)
    print*,'after 2'
    cv_e = cv_s + r_cvsize - 1 
    cv(cv_s:cv_e) = rcv (1:r_cvsize) 
    cv_s = cv_e + 1
    end do
   end if
!
   if( be%v3%mz > 0)then 
    do k = 1, be%v3%mz
    field(1:xbx%ni,1:xbx%nj) = vv%v3(1:xbx%ni,1:xbx%nj,k)
    power(0:xbx%max_wavenumber) = be%v3%power(0:xbx%max_wavenumber,k)
    print*,'before 3'
    call da_v_to_vv_spectral_adj(xbx%ni,xbx%nj,xbx%max_wavenumber, &
                      xbx%inc,xbx%lenr,xbx%lensav, xbx%lenwrk, &
                      xbx%alp_size,r_cvsize,xbx%alp, xbx%wsave,&
                      power, rcv, field)
    print*,'after 3'
    cv_e = cv_s + r_cvsize - 1 
    cv(cv_s:cv_e) = rcv (1:r_cvsize) 
    cv_s = cv_e + 1
    end do
   end if
!
   if( be%v4%mz > 0)then 
    do k = 1, be%v4%mz
    field(1:xbx%ni,1:xbx%nj) = vv%v4(1:xbx%ni,1:xbx%nj,k)
    power(0:xbx%max_wavenumber) = be%v4%power(0:xbx%max_wavenumber,k)
    print*,'before 4'
    call da_v_to_vv_spectral_adj(xbx%ni,xbx%nj,xbx%max_wavenumber, &
                      xbx%inc,xbx%lenr,xbx%lensav, xbx%lenwrk, &
                      xbx%alp_size,r_cvsize,xbx%alp, xbx%wsave,&
                      power, rcv, field)
    print*,'after 4'
    cv_e = cv_s + r_cvsize - 1 
    cv(cv_s:cv_e) = rcv (1:r_cvsize) 
    cv_s = cv_e + 1
    end do
   end if
!
   if( be%v5%mz > 0)then 
    do k = 1, be%v5%mz
    field(1:xbx%ni,1:xbx%nj) = vv%v5(1:xbx%ni,1:xbx%nj,k)
    power(0:xbx%max_wavenumber) = be%v5%power(0:xbx%max_wavenumber,k)
    print*,'before 5'
    call da_v_to_vv_spectral_adj(xbx%ni,xbx%nj,xbx%max_wavenumber, &
                      xbx%inc,xbx%lenr,xbx%lensav, xbx%lenwrk, &
                      xbx%alp_size,r_cvsize,xbx%alp, xbx%wsave,&
                      power, rcv, field)
    print*,'after 5'
    cv_e = cv_s + r_cvsize - 1 
    cv(cv_s:cv_e) = rcv (1:r_cvsize) 
    cv_s = cv_e + 1
    end do
   end if
!
end subroutine DA_Transform_VToVv_Spectral_Adj
