subroutine da_local_to_global(xp, x, xg, dim)

   !---------------------------------------------------------------------------
   ! PURPOSE: Make xg global-grid array by gathering the subdomain arrays x from 
   !           each processor. The global array xg is stored only on the monitor
   !           processor (typically, processor 0). 
   !---------------------------------------------------------------------------

    type (XPOSE_type), intent(in):: xp                                    ! Domain decomposition vars.
    real,  intent(in)   :: x (xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme)  ! Local-grid input array.
    real,  intent(out)  :: xg(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)  ! Global-grid output array.
    integer, intent(in) :: dim                         ! Number of dimensions (=2 for 2D, =3 for 3D). 

    integer                      :: glen(3) ! Global grid dimensions
    integer                      :: llen(3) ! Local grid dimensions

   ! Set global and local index extent arrays.
   glen(1) = xp%ide-xp%ids+1
   llen(1) = xp%ime-xp%ims+1
   glen(2) = xp%jde-xp%jds+1
   llen(2) = xp%jme-xp%jms+1
   if (dim .eq. 2) then
      glen(3) = 1
      llen(3) = 1
      call rsl_write(xg,IO2D_IJ_inTERNAL,x,0,true_RSL_real,glen,llen)
   elseif (dim.eq.3) then
      glen(3) = xp%kde-xp%kds+1
      llen(3) = xp%kme-xp%kms+1
      call rsl_write(xg,IO3D_IJK_inTERNAL,x,0,true_RSL_real,glen,llen)
   else
      write(unit=message(1),fmt='(A,I5,A)') &
         "dim=",dim,"must be 2 or 3"
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

end subroutine da_local_to_global

