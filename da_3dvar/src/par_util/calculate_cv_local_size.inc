SUBROUTINE calculate_cv_local_size( xbx, xp, be, cv, cv_size )

!------------------------------------------------------------------------------
!  PURPOSE: Calculate the size of the 1-dimensional control variable array.
!           
!------------------------------------------------------------------------------

    type (xbx_type), intent(in)   :: xbx   ! For header & non-grid arrays.
    type (xpose_type), intent(in) :: xp    ! Dimensions and xpose buffers.
    type (be_type), intent(in)    :: be    ! Background error structure.
    type (cv_type), intent(out)   :: cv    ! Local-processor CV structure.
    integer, intent(out)       :: cv_size  ! Size of (local-grid) control variable.

    integer             :: iy, jx          ! Local horizontal domain dimensions.
    integer             :: ij              ! Product of horizontal dims.

    if ( cv_options <= 5) then
    iy = xp % ite - xp % its + 1
    jx = xp % jte - xp % jts + 1
    ij = iy * jx

    cv % size1 = ij * be % v1 % mz
    cv % size2 = ij * be % v2 % mz
    cv % size3 = ij * be % v3 % mz
    cv % size4 = ij * be % v4 % mz
    cv % size5 = ij * be % v5 % mz
    end if

    if ( cv_options == 4 ) then
    cv % size1  = 2* xbx%c_cv_size * be % v1 % mz
    cv % size2  = 2* xbx%c_cv_size * be % v2 % mz
    cv % size3  = 2* xbx%c_cv_size * be % v3 % mz
    cv % size4  = 2* xbx%c_cv_size * be % v4 % mz
    cv % size5  = 2* xbx%c_cv_size * be % v5 % mz
    end if

    cv % size = cv % size1 + cv % size2 + cv % size3 + cv % size4 + cv % size5
    cv_size = cv % size

    RETURN
END SUBROUTINE calculate_cv_local_size

