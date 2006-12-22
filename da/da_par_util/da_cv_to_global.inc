subroutine da_cv_to_global( cv_size, cv_size_global, x, xp, mzs, xg )

   ! Gathers local cv-array x into domain cv-array xg(:).  
   ! Global cv-array xg will only be valid on the "monitor" task.  
   !
   ! Must be called by all MPI tasks.  

   implicit none

   integer,           intent(in) :: cv_size        ! Size of local cv-array
   integer,           intent(in) :: cv_size_global ! Size of domain cv-array
   real,              intent(in) :: x(1:cv_size)   ! local cv-array
   type (xpose_type), intent(in) :: xp   ! decomposed dimensions
   integer,           intent(in) :: mzs(:)  ! mz for each variable
                                         ! (to identify empty or 2D arrays)
   real,           intent(inout) :: xg(1:cv_size_global) ! global cv-array

#ifdef DM_PARALLEL

   ! Local declarations
   type (vp_type) :: vv_x    ! Grdipt/EOF cv_array (local)
   type (vp_type) :: vv_xg   ! Grdipt/EOF cv_array (global)
   type (xpose_type) :: xpg  ! global dimensions
   integer :: ids, ide, jds, jde, kds, &
              ims, ime, jms, jme, kms, &
              ips, ipe, jps, jpe, kps

   !
   ! Gather to mimic serial summation order.  
   !

   ! k?e varies with variable v1 - v5
   ids = xp%ids; ide = xp%ide; jds = xp%jds; jde = xp%jde; kds = xp%kds
   ims = xp%ims; ime = xp%ime; jms = xp%jms; jme = xp%jme; kms = xp%kms
   ips = xp%ips; ipe = xp%ipe; jps = xp%jps; jpe = xp%jpe; kps = xp%kps

   ! TOdo:  encapsulate this crap!  
   ! allocate vv_x
   allocate( vv_x%v1(xp%ims:xp%ime,xp%jms:xp%jme,mzs(1)) )
   allocate( vv_x%v2(xp%ims:xp%ime,xp%jms:xp%jme,mzs(2)) )
   allocate( vv_x%v3(xp%ims:xp%ime,xp%jms:xp%jme,mzs(3)) )
   allocate( vv_x%v4(xp%ims:xp%ime,xp%jms:xp%jme,mzs(4)) )
   allocate( vv_x%v5(xp%ims:xp%ime,xp%jms:xp%jme,mzs(5)) )
   allocate( vv_x%alpha(xp%ims:xp%ime,xp%jms:xp%jme,mzs(6)) )

   call da_cv_to_vv ( cv_size, x, xp, mzs, vv_x )

   if ( rootproc ) then
      ! allocate vv_xg
      allocate( vv_xg%v1(xp%ids:xp%ide,xp%jds:xp%jde,mzs(1)) )
      allocate( vv_xg%v2(xp%ids:xp%ide,xp%jds:xp%jde,mzs(2)) )
      allocate( vv_xg%v3(xp%ids:xp%ide,xp%jds:xp%jde,mzs(3)) )
      allocate( vv_xg%v4(xp%ids:xp%ide,xp%jds:xp%jde,mzs(4)) )
      allocate( vv_xg%v5(xp%ids:xp%ide,xp%jds:xp%jde,mzs(5)) )
      allocate( vv_xg%alpha(xp%ids:xp%ide,xp%jds:xp%jde,mzs(6)) )
   else
      ! Allocate dummy array for non-monitor process to keep Fortran
      ! CICO happy...
      allocate( vv_xg%v1(1,1,1) )
      allocate( vv_xg%v2(1,1,1) )
      allocate( vv_xg%v3(1,1,1) )
      allocate( vv_xg%v4(1,1,1) )
      allocate( vv_xg%v5(1,1,1) )
      allocate( vv_xg%alpha(1,1,1) )
   end if

   ! TOdo:  encapsulate this crap!  
   ! gather to global data structures
   ! it is possible to gather straight into a globally-sized cv-array
   ! TOdo:  add this optimization later
   call da_wv_patch_to_global( vv_x%v1, vv_xg%v1,       &
                            xp%domdesc, mzs(1),    &
                            ids, ide, jds, jde, kds, &
                            ims, ime, jms, jme, kms, &
                            ips, ipe, jps, jpe, kps )
   call da_wv_patch_to_global( vv_x%v2, vv_xg%v2,       &
                            xp%domdesc, mzs(2),    &
                            ids, ide, jds, jde, kds, &
                            ims, ime, jms, jme, kms, &
                            ips, ipe, jps, jpe, kps )
   call da_wv_patch_to_global( vv_x%v3, vv_xg%v3,       &
                            xp%domdesc, mzs(3),    &
                            ids, ide, jds, jde, kds, &
                            ims, ime, jms, jme, kms, &
                            ips, ipe, jps, jpe, kps )
   call da_wv_patch_to_global( vv_x%v4, vv_xg%v4,       &
                            xp%domdesc, mzs(4),    &
                            ids, ide, jds, jde, kds, &
                            ims, ime, jms, jme, kms, &
                            ips, ipe, jps, jpe, kps )
   call da_wv_patch_to_global( vv_x%v5, vv_xg%v5,       &
                            xp%domdesc, mzs(5), &
                            ids, ide, jds, jde, kds, &
                            ims, ime, jms, jme, kms, &
                            ips, ipe, jps, jpe, kps )
   call da_wv_patch_to_global( vv_x%alpha, vv_xg%alpha,       &
                            xp%domdesc, mzs(6),    &
                            ids, ide, jds, jde, kds, &
                            ims, ime, jms, jme, kms, &
                            ips, ipe, jps, jpe, kps )

   ! deallocate vv_x
   deallocate (vv_x%v1, vv_x%v2, vv_x%v3, vv_x%v4, vv_x%v5 )

   if ( rootproc ) then
      ! finally, collapse data back into a globally-sized cv-array
      xpg%ids = xp%ids; xpg%ide = xp%ide
      xpg%ims = xp%ids; xpg%ime = xp%ide
      xpg%its = xp%ids; xpg%ite = xp%ide
      xpg%jds = xp%jds; xpg%jde = xp%jde
      xpg%jms = xp%jds; xpg%jme = xp%jde
      xpg%jts = xp%jds; xpg%jte = xp%jde
      xpg%kds = xp%kds; xpg%kde = xp%kde
      xpg%kms = xp%kds; xpg%kme = xp%kde
      xpg%kts = xp%kds; xpg%kte = xp%kde
      call da_vv_to_cv( vv_xg, xpg, mzs, cv_size_global, xg )
   end if

   ! deallocate vv_xg
   deallocate( vv_xg%v1, vv_xg%v2, vv_xg%v3, vv_xg%v4, vv_xg%v5, vv_xg%alpha )

#endif

end subroutine da_cv_to_global


