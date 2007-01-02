subroutine set_tst_trnsf_fld(xp, va, vb, typical_rms )

   implicit none

   type (xpose_type), intent(in) :: xp   ! Dimensions and xpose buffers.

   real, dimension(xp%ims:xp%ime, xp%jms:xp%jme, xp%kms:xp%kme), intent(out) :: va
   real, dimension(xp%ims:xp%ime, xp%jms:xp%jme, xp%kms:xp%kme), intent(in)  :: vb

   real, intent(in)  :: typical_rms

   real, dimension(xp%ids:xp%ide, xp%jds:xp%jde, xp%kds:xp%kde) :: gbuf

   integer                    :: size3d
   integer                    :: k             ! loop counter.
   real                       :: scale         ! scale factor.
   real                       :: xy_dim_inv    ! 1 / real( iy * jx )
   real                       :: field_mean    ! mean field.
   real                       :: field_rms     ! rms field.

   size3d = (xp%ide-xp%ids+1)*(xp%jde-xp%jds+1)*(xp%kde-xp%kds+1)

   call local_to_global( xp, vb, gbuf, 3 )
   call wrf_dm_bcast_real( gbuf, size3d )

   xy_dim_inv  = 1.0 / real((xp%ide-xp%ids+1)*(xp%jde-xp%jds+1))

   do k = xp%kts,xp%kte
      field_mean = sum(gbuf(xp%ids:xp%ide, xp%jds:xp%jde, k)) * xy_dim_inv

      gbuf(xp%ids:xp%ide, xp%jds:xp%jde, k) = gbuf(xp%ids:xp%ide, xp%jds:xp%jde, k) - field_mean

      field_rms = sqrt( 1.0e-6+sum(gbuf(xp%ids:xp%ide, xp%jds:xp%jde, k)**2) * xy_dim_inv)

      scale = typical_rms / field_rms

      va(xp%its:xp%ite, xp%jts:xp%jte, k) = scale * gbuf(xp%its:xp%ite, xp%jts:xp%jte, k)
   end do

end subroutine set_tst_trnsf_fld

