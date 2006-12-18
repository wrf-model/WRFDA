SUBROUTINE DA_Transfer_XatoWRFTL_Adj( grid, config_flags, filnam, &
!
#include <em_dummy_args.inc>
!
                              )

!------------------------------------------------------------------------------
!  PURPOSE: Convert WRFTL variables to analysis increments
!           (inverse of the incremental part of xatowrf)
!                                                  Hans Huang, 7 March 2005
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   type(domain) ,   TARGET                   :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4 filnam

!  Definitions of dummy arguments to solve

#include <em_dummy_decl.inc>

!  Local variables

   integer :: i, j, k, n
   integer :: is, ie, js, je, ks, ke

   integer ndynopt

#ifdef WRFVAR
!------------------------------------------------------------------------------
!  [0.0] INPUT
!------------------------------------------------------------------------------

   is = xp % its
   ie = xp % ite
   js = xp % jts
   je = xp % jte
   ks = xp % kts
   ke = xp % kte

   ndynopt      = grid%dyn_opt
   grid%dyn_opt = DYN_EM_AD
   call nl_set_dyn_opt ( 1 , DYN_EM_AD )

   call med_initialdata_input_3dvar( grid , config_flags, in_filename=filnam )

   grid%dyn_opt = ndynopt
   call nl_set_dyn_opt ( 1 , DYN_EM )

#ifdef DM_PARALLEL
!--Fill the halo region for u and v.
   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)
#endif

   do k=ks,ke
   do j=js,je
   do i=is,ie
      xa%u(i,j,k)=0.5*(a_u_2(i+1,j  ,k)+a_u_2(i,j,k))
      xa%v(i,j,k)=0.5*(a_v_2(i  ,j+1,k)+a_v_2(i,j,k))
   enddo
   enddo
   enddo

   do j=js,je

   do i=is,ie
      xa%psfc(i,j)=a_mu_2(i,j)
      xa%w(i,j,ke+1)=a_w_2(i,j,ke+1)
   enddo

   do k=ks,ke
   do i=is,ie
      xa%t(i,j,k)=a_t_2(i,j,k)
      xa%q(i,j,k)=a_moist(i,j,k,P_QV)
      xa%w(i,j,k)=a_w_2(i,j,k)
   enddo
   enddo

   enddo

#if 0
   if ( size(moist,dim=4) >= 4 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         xa%qcw(i,j,k)=a_moist(i,j,k,p_qcw)
         xa%qrn(i,j,k)=a_moist(i,j,k,p_qrn)
      enddo
      enddo
      enddo
   endif

   if ( size(moist,dim=4) >= 6 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         xa%qci(i,j,k)=a_moist(i,j,k,p_qci)
         xa%qsn(i,j,k)=a_moist(i,j,k,p_qsn)
      enddo
      enddo
      enddo
   endif

   if ( size(moist,dim=4) >= 7 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         xa%qgr(i,j,k)=a_moist(i,j,k,p_qgr) 
      enddo
      enddo
      enddo
   endif
#endif
#endif

END SUBROUTINE DA_Transfer_XatoWRFTL_Adj

