
SUBROUTINE cv_to_vv ( cv_size, cv_array, xp, be, vv )

!------------------------------------------------------------------------------
!  PURPOSE: Fill (local) vv arrays from 1D (local) cv array.
!------------------------------------------------------------------------------

   INTEGER,        INTENT(IN)    :: cv_size     ! Length of CV array.
   REAL,           INTENT(IN)    :: cv_array(1:cv_size) ! Control variables v.
   TYPE (xpose_type), INTENT(IN) :: xp          ! Dimensions and xpose buffers.
   TYPE (be_type), intent(in)    :: be          ! Background error structure.
   TYPE (vp_type), INTENT(OUT)   :: vv          ! Grdipt/EOF cv_array.

   integer   :: is,ie       ! Local grid range in y coordinate.
   integer   :: js,je       ! Local grid range in x coordinate.
   integer   :: ix,jy       ! Local grid horizontal dimensions.
   integer   :: mz          ! Max vertical coordinate for v1 through v5 arrays.
   integer   :: cv_s,cv_e   ! Starting and ending indices into CV array.
   integer   :: ijm         ! Size of interior of v1 through v5 arrays.

   is = xp % its
   ie = xp % ite
   js = xp % jts
   je = xp % jte
   ix = ie-is+1
   jy = je-js+1
   cv_e = 0

!Fill v1
   mz = be % v1 % mz
   if ( mz > 0 ) then
     ijm = ix * jy * mz
     cv_s = cv_e + 1
     cv_e = cv_s + ijm - 1
     vv % v1(is:ie,js:je,1:mz) = RESHAPE( cv_array(cv_s:cv_e),(/ix, jy, mz/) )
   end if
!Fill v2
   mz = be % v2 % mz
   if ( mz > 0 ) then
     ijm = ix * jy * mz
     cv_s = cv_e + 1
     cv_e = cv_s + ijm - 1
     vv % v2(is:ie,js:je,1:mz) = RESHAPE( cv_array(cv_s:cv_e),(/ix, jy, mz/) )
   end if
!Fill v3
   mz = be % v3 % mz
   if ( mz > 0 ) then
     ijm = ix * jy * mz
     cv_s = cv_e + 1
     cv_e = cv_s + ijm - 1
     vv % v3(is:ie,js:je,1:mz) = RESHAPE( cv_array(cv_s:cv_e),(/ix, jy, mz/) )
   end if
!Fill v4
   mz = be % v4 % mz
   if ( mz > 0 ) then
     ijm = ix * jy * mz
     cv_s = cv_e + 1
     cv_e = cv_s + ijm - 1
     vv % v4(is:ie,js:je,1:mz) = RESHAPE( cv_array(cv_s:cv_e),(/ix, jy, mz/) )
   end if
!Fill v5
   mz = be % v5 % mz
   if ( mz > 0 ) then
     ijm = ix * jy * mz
     cv_s = cv_e + 1
     cv_e = cv_s + ijm - 1
     vv % v5(is:ie,js:je,1:mz) = RESHAPE( cv_array(cv_s:cv_e),(/ix, jy, mz/) )
   end if

#ifdef DEBUG_RESHAPE
   print *, ' '
   print *, 'FILE:', __FILE__, '  LINE:', __LINE__
   print *, 'sum(vv)=', sum(vv % v1(is:ie,js:je,1:be % v1 % mz)) &
                      + sum(vv % v2(is:ie,js:je,1:be % v2 % mz)) &
                      + sum(vv % v3(is:ie,js:je,1:be % v3 % mz)) &
                      + sum(vv % v4(is:ie,js:je,1:be % v4 % mz))
   print *, 'sum(cv)=', sum(cv_array(1:cv_size))
   print *, ' '
#endif

END SUBROUTINE cv_to_vv

