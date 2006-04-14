SUBROUTINE vv_to_cv(vv, xp, mzs, cv_size, rcv )

!------------------------------------------------------------------------------
!  PURPOSE: Fill (local) 1D cv array from 3D (local) vv arrays.
!------------------------------------------------------------------------------

   TYPE (vp_type), INTENT(IN)    :: vv          ! Grdipt/EOF rcv.
   TYPE (xpose_type), INTENT(IN) :: xp          ! Dimensions and xpose buffers.
   INTEGER,        INTENT(IN)    :: mzs(:) ! Background error structure levels.
   INTEGER,        INTENT(IN)    :: cv_size     ! Length of CV array.
   REAL,           INTENT(OUT)   :: rcv(1:cv_size) ! Control variables v.

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
        rcv(ijk) = vv%v1(i,j,k)
     enddo
     enddo
     enddo
#else
     rcv(cv_s:cv_e) = RESHAPE( vv % v1(is:ie,js:je,1:mz), (/ijm/) )
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
        rcv(ijk) = vv%v2(i,j,k)
     enddo
     enddo
     enddo
#else
     rcv(cv_s:cv_e) = RESHAPE( vv % v2(is:ie,js:je,1:mz), (/ijm/) )
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
        rcv(ijk) = vv%v3(i,j,k)
     enddo
     enddo
     enddo
#else
     rcv(cv_s:cv_e) = RESHAPE( vv % v3(is:ie,js:je,1:mz), (/ijm/) )
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
        rcv(ijk) = vv%v4(i,j,k)
     enddo
     enddo
     enddo
#else
     rcv(cv_s:cv_e) = RESHAPE( vv % v4(is:ie,js:je,1:mz), (/ijm/) )
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
        rcv(ijk) = vv%v5(i,j,k)
     enddo
     enddo
     enddo
#else
     rcv(cv_s:cv_e) = RESHAPE( vv % v5(is:ie,js:je,1:mz), (/ijm/) )
#endif
   end if

!Store alpha:
   mz = mzs(6)
   if ( mz > 0 ) then
     ijm = ix * jy * mz
     cv_s = cv_e + 1
     cv_e = cv_s + ijm - 1
#ifdef NORESHAPE
     do k=1,mz
     do j=js,je
     do i=is,ie
        ijk = ijk + 1
        rcv(ijk) = vv%alpha(i,j,k)
     enddo
     enddo
     enddo
#else
     rcv(cv_s:cv_e) = RESHAPE( vv % alpha(is:ie,js:je,1:mz), (/ijm/) )
#endif
   end if

END SUBROUTINE vv_to_cv

