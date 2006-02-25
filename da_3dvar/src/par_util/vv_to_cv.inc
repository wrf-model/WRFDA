SUBROUTINE vv_to_cv(vv, xp, mzs, cv_size, cv_array )

!------------------------------------------------------------------------------
!  PURPOSE: Fill (local) 1D cv array from 3D (local) vv arrays.
!------------------------------------------------------------------------------

   TYPE (vp_type), INTENT(IN)    :: vv          ! Grdipt/EOF cv_array.
   TYPE (xpose_type), INTENT(IN) :: xp          ! Dimensions and xpose buffers.
   INTEGER,        INTENT(IN)    :: mzs(:) ! Background error structure levels.
   INTEGER,        INTENT(IN)    :: cv_size     ! Length of CV array.
   REAL,           INTENT(OUT)   :: cv_array(1:cv_size) ! Control variables v.

   integer   :: is,ie       ! Local grid range in y coordinate.
   integer   :: js,je       ! Local grid range in x coordinate.
   integer   :: ix,jy       ! Local grid horizontal dimensions.
   integer   :: mz          ! Max vertical coordinate for v1 through v5 arrays.
   integer   :: cv_s,cv_e   ! Starting and ending indices into CV array.
   integer   :: ijm         ! Size of interior of v1 through v5 arrays.

#ifdef NORESHAPE
   integer   :: i,j,k,ijk
#endif

   is = xp % its
   ie = xp % ite
   js = xp % jts
   je = xp % jte
   ix = ie-is+1
   jy = je-js+1
   cv_e = 0

!Store v1
   mz = mzs(1)


   if ( mz > 0 ) then
     ijm = ix * jy * mz
     cv_s = cv_e + 1
     cv_e = cv_s + ijm - 1
#ifdef NORESHAPE
     ijk = 0
     do k=1,mz
     do j=js,je
     do i=is,ie
        ijk = ijk + 1
        cv_array(ijk) = vv%v1(i,j,k)
     enddo
     enddo
     enddo
#else
     cv_array(cv_s:cv_e) = RESHAPE( vv % v1(is:ie,js:je,1:mz), (/ijm/) )
#endif
   end if
!Store v2
   mz = mzs(2)
   if ( mz > 0 ) then
     ijm = ix * jy * mz
     cv_s = cv_e + 1
     cv_e = cv_s + ijm - 1
#ifdef NORESHAPE
     do k=1,mz
     do j=js,je
     do i=is,ie
        ijk = ijk + 1
        cv_array(ijk) = vv%v2(i,j,k)
     enddo
     enddo
     enddo
#else
     cv_array(cv_s:cv_e) = RESHAPE( vv % v2(is:ie,js:je,1:mz), (/ijm/) )
#endif
   end if
!Store v3
   mz = mzs(3)
   if ( mz > 0 ) then
     ijm = ix * jy * mz
     cv_s = cv_e + 1
     cv_e = cv_s + ijm - 1

#ifdef NORESHAPE
     do k=1,mz
     do j=js,je
     do i=is,ie
        ijk = ijk + 1
        cv_array(ijk) = vv%v3(i,j,k)
     enddo
     enddo
     enddo
#else
     cv_array(cv_s:cv_e) = RESHAPE( vv % v3(is:ie,js:je,1:mz), (/ijm/) )
#endif
   end if
!Store v4
   mz = mzs(4)
   if ( mz > 0 ) then
     ijm = ix * jy * mz
     cv_s = cv_e + 1
     cv_e = cv_s + ijm - 1
#ifdef NORESHAPE
     do k=1,mz
     do j=js,je
     do i=is,ie
        ijk = ijk + 1
        cv_array(ijk) = vv%v4(i,j,k)
     enddo
     enddo
     enddo
#else
     cv_array(cv_s:cv_e) = RESHAPE( vv % v4(is:ie,js:je,1:mz), (/ijm/) )
#endif
   end if
!Store v5
   mz = mzs(5)
   if ( mz > 0 ) then
     ijm = ix * jy * mz
     cv_s = cv_e + 1
     cv_e = cv_s + ijm - 1
#ifdef NORESHAPE
     do k=1,mz
     do j=js,je
     do i=is,ie
        ijk = ijk + 1
        cv_array(ijk) = vv%v5(i,j,k)
     enddo
     enddo
     enddo
#else
     cv_array(cv_s:cv_e) = RESHAPE( vv % v5(is:ie,js:je,1:mz), (/ijm/) )
#endif
   end if

#ifdef DEBUG_RESHAPE
   write(unit=stdout, fmt='(2a, 2(a, i8)/)') &
       'file:', __FILE__, ', line:', __LINE__

   write(unit=stdout, fmt='(a, i4)') &
        'mzs(1)=', mzs(1), &
        'mzs(2)=', mzs(2), &
        'mzs(3)=', mzs(3), &
        'mzs(4)=', mzs(4), &
        'mzs(5)=', mzs(5)

   WRITE(UNIT=stdout) ' '
   WRITE(UNIT=stdout,FMT='(A,F12.2)') &
     'sum(vv)=', sum(vv % v1(is:ie,js:je,1:mzs(1))) &
                      + sum(vv % v2(is:ie,js:je,1:mzs(2))) &
                      + sum(vv % v3(is:ie,js:je,1:mzs(3))) &
                      + sum(vv % v4(is:ie,js:je,1:mzs(4)))
   WRITE(UNIT=stdout,FMT='(A,F12.2)') &
     'sum(cv)=', sum(cv_array(1:cv_size))
   WRITE(UNIT=stdout) ' '
#endif

END SUBROUTINE vv_to_cv

