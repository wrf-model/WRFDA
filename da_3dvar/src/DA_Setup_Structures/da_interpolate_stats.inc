subroutine da_interpolate_stats( kzs, kz, meanp_stats, meanp_xb, &
                                 be_evec_glo, be_eval_glo, &
                                 be_lengthscale, e, l, s )

!------------------------------------------------------------------------------
!  PURPOSE: Interpolate statistics to new domain.
!
!  METHOD:  Interpolation of vertical background error covariance.
!
!  HISTORY: 06/07/2001 - Creation of F90 version.           Dale Barker
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in)         :: kzs                   ! Number of levels in stats.
   integer, intent(in)         :: kz                    ! Number of levels in xb.
   real, intent(in)            :: meanp_stats(:)        ! Mean pressure on stats levs.
   real, intent(inout)         :: meanp_xb(:)           ! Mean pressure on xb levs.
   real, intent(in)            :: be_evec_glo(:,:)      ! Eigenvectors of vert B.
   real, intent(in)            :: be_eval_glo(:)        ! Eigenvalues of vert B.
   real, intent(in)            :: be_lengthscale(:)     ! Correlation scale.
   real, intent(out)           :: e(:,:)                ! New eigenvectors.
   real, intent(out)           :: l(:)                  ! New eigenvalues.
   real, intent(out)           :: s(:)                  ! New lengthscales.
     
   integer                     :: k1, k2, k, ks         ! Loop counters.
   integer                     :: ktrap_min, ktrap_max  ! Max/min limits of xb levels.
   integer                     :: k1s, k2s

   real                        :: b_native(1:kzs,1:kzs)
   real                        :: b(1:kz,1:kz)
   real                        :: weight(1:kz)
   integer                     :: k_above(1:kz)

   real                        :: work(1:kz)

!---------------------------------------------------------------------------
!  [1.0] Compare stats/xb levels and compute interpolation weights:
!---------------------------------------------------------------------------
!
! For WRF 3D-Var (Y.-R. Guo):.......................
! --------------------------
! To convert the WRF meanp_xb to MM5 meanp_xb: 
   do k = 1, kz
     work(kz+1-k) = meanp_xb(k)
   enddo
   meanp_xb = work
! ..................................................

   k_above(1:kz) = 0
   weight(1:kz) = 0.0
  
   ktrap_min = 1
   do k = 1, kz
      if ( meanp_xb(k) < meanp_stats(1) ) ktrap_min = k+1
   end do

   ktrap_max = kz
   do k = kz, 1, -1
      if ( meanp_xb(k) >= meanp_stats(kzs) ) ktrap_max = k-1
   end do

   do k = ktrap_min, ktrap_max
      do ks = 1, kzs-1
         if ( meanp_xb(k) >= meanp_stats(ks) .AND. &
              meanp_xb(k) < meanp_stats(ks+1) ) then

            weight(k) = ( meanp_xb(k) - meanp_stats(ks) ) / &
                        ( meanp_stats(ks+1) - meanp_stats(ks) )
            k_above(k) = ks
         end if
      end do
   end do

! Above the STATS TOP, set weight = 0.0:
   do k = 1, ktrap_min - 1
      k_above(k) = 1
      weight(k) = 0.0
   enddo
! Below the STATS BOTTOM, set weight = 1.0
   do k = ktrap_max + 1, kz
      k_above(k) = kzs-1
      weight(k) = 1.0
   enddo

!---------------------------------------------------------------------------
!  [2.0] Interpolate lengthscales (in pressure) from original to new levels:
!---------------------------------------------------------------------------
   
!  [2.1] Levels average pressure within top/bottom of original stats:

!   do k = ktrap_min, ktrap_max
   do k = 1, kz
      ks = k_above(k)
      
      s(k) = ( 1.0 - weight(k) ) * be_lengthscale(ks) + &
             weight(k) * be_lengthscale(ks+1)
!      write(0,'(2i4,4f12.5)')k, ks, weight(k), be_lengthscale(ks), &
!                             be_lengthscale(ks+1), s(k)
   end do

!  [2.2] Scale above original stats equals scale on stats top level:

!   if ( ktrap_min > 1 ) then 
!      do k = 1, ktrap_min-1
!         s(k) = be_lengthscale(1)
!      end do
!   end if

!  [2.3] Scale below original stats equals scale on stats bottom level:

!   if ( ktrap_max < kz ) then 
!      do k = ktrap_max+1, kz
!         s(k) = be_lengthscale(kzs)
!      end do
!   end if
   
!---------------------------------------------------------------------------
!  [3.0] Recalculate native global vertical background error cov matrix:
!---------------------------------------------------------------------------

   do k1 = 1, kzs
      do k2 = k1, kzs
         b_native(k1,k2) = SUM( be_evec_glo(k1,:) * be_eval_glo(:) * &
                                be_evec_glo(k2,:) )
      end do
   end do
   
   do k1 = 2, kzs
      do k2 = 1, k1-1
         b_native(k1,k2) = b_native(k2,k1)
      end do
   end do

!---------------------------------------------------------------------------
!  [3.0] Interpolate vertical background error covariance matrix to xb domain:
!---------------------------------------------------------------------------

   b(1:kz,1:kz) = 0.0

!  [3.1] Interpolation to pressures within domain of original statistics:

   do k1 = ktrap_min, ktrap_max
      k1s = k_above(k1)
      do k2 = k1, ktrap_max
         k2s = k_above(k2)
         b(k1,k2) = (1.0-weight(k1)) * (1.0-weight(k2)) * b_native(k1s,k2s) + &
                    weight(k1) * (1.0-weight(k2)) * b_native(k1s+1,k2s) + &
                    weight(k2) * (1.0-weight(k1)) * b_native(k1s,k2s+1) + &
                    weight(k2) * weight(k1) * b_native(k1s+1,k2s+1)
      end do
   end do
      
   do k1 = 2, kz
      do k2 = 1, k1-1
         b(k1,k2) = b(k2,k1)
      end do
   end do
      
!  [3.2] Extrapolation of statistics to pressures above domain of original statistics:       

   if ( ktrap_min > 1 ) then
      do k1 = 1, ktrap_min-1
         do k2 = ktrap_min, kz
            b(k1,k2) = ( ( meanp_xb(k1) - meanp_xb(ktrap_min+1) ) * b(ktrap_min,k2) - &
                         ( meanp_xb(k1) - meanp_xb(ktrap_min  ) ) * b(ktrap_min+1,k2) ) / &
                       ( meanp_xb(ktrap_min) - meanp_xb(ktrap_min+1) )
         end do
            
         do k2 = 1, ktrap_min-1
            b(k1,k2) = b(k1,ktrap_min)
         end do
      end do
   end if

!  [3.3] Extrapolation of statistics to pressures below domain of original statistics:       

   if ( ktrap_max < kz ) then
      do k1 = 1, ktrap_max
         k1s = k_above(k1)
         do k2 = ktrap_max+1, kz
            b(k1,k2) = ( ( meanp_xb(k2) - meanp_xb(ktrap_max-1) ) * b(k1,ktrap_max  ) - &
                         ( meanp_xb(k2) - meanp_xb(ktrap_max  ) ) * b(k1,ktrap_max-1) ) / &
                       ( meanp_xb(ktrap_max) - meanp_xb(ktrap_max-1) )
         end do
      end do
         
      do k1 = ktrap_max+1, kz
         do k2 = ktrap_max+1, kz
            b(k1,k2) = b(ktrap_max,k2)
         end do
      end do
   end if
      
   do k1 = 2, kz
      do k2 = 1, k1-1
         b(k1,k2) = b(k2,k1)
      end do
   end do
       
!---------------------------------------------------------------------------
!  [4.0] Calculate new global eigenvectors/eigenvalues:
!---------------------------------------------------------------------------

   call da_1d_eigendecomposition( b, e, l )

!---------------------------------------------------------------------------
!  [5.0] Check for positive-definiteness:
!---------------------------------------------------------------------------

!   call da_array_print( 1, b_native(:,:), 'b_native' )
!   call da_array_print( 1, b(:,:), 'b_xb first' )

!  [5.1] Check for negative eigenvalues and zero if found:

   do k1 = 1, kz
      if ( l(k1) < 0.0 ) l(k1) = 0.0
   end do

!     [5.2] Calculate modified b(:,:)

   do k1 = 1, kz
      do k2 = k1, kz
         b(k1,k2) = SUM( e(k1,:) * l(:) * e(k2,:) )
      end do
   end do
   
   do k1 = 2, kz
      do k2 = 1, k1-1
         b(k1,k2) = b(k2,k1)
      end do
   end do
   
!   call da_array_print( 1, b(:,:), 'b_xb new' )

!  [5.3] Recalculate eigenvectors/eigenvalues:

   call da_1d_eigendecomposition( b, e, l )

!   do k1 = 1, kz
!      write(0,'(i3,4e15.5)')k1, l(k1)
!   end do
   
!  [5.4] Final elimination of small negative values:

   do k1 = 1, kz
      if ( l(k1) < 0.0 ) l(k1) = 0.0
   end do

! For WRF 3D-Var (Y.-R. Guo):....................................
! --------------------------

! To send back the meanp_xb in WRF k-order:
   do k = 1,kz
     meanp_xb(k) = work(kz+1-k)
   enddo

! To convert the interpolated MM5 Eigenvector to WRF Eigenvector
!   ---- the order of the components for each of eigenvectors 
!        need to be reversed.

   do k2 = 1,kz
      do k1 = 1,kz
         work(kz+1-k1) = e(k1,k2)
      enddo
      e(:,k2) = work(:)
   enddo
! ................................................................
end subroutine da_interpolate_stats








