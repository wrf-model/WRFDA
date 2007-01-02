SUBROUTINE DA_Transfer_WRFTLtoXa( grid, config_flags, filnam, &
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

   real    :: sdmd, s1md, qvf1, qvf2, qvf1_b, qvf2_b
   real, dimension(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme) :: p_agrid

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
   grid%dyn_opt = DYN_EM_TL
   call nl_set_dyn_opt ( 1 , DYN_EM_TL )

   call med_initialdata_input_3dvar( grid , config_flags, in_filename=filnam )

   grid%dyn_opt = ndynopt
   call nl_set_dyn_opt ( 1 , DYN_EM )

!------------------------------------------------------------------------------
!  [1.0] Get the specific humidity increments from mixing ratio increments
!------------------------------------------------------------------------------

   do k=ks,ke
   do j=js,je
   do i=is,ie
      xa%q(i,j,k)=g_moist(i,j,k,P_QV)*(1.0-xb%q(i,j,k))**2
   enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [2.0] COMPUTE psfc increments from mu-increments
!------------------------------------------------------------------------------

   do j=js,je
   do i=is,ie
      sdmd=0.0
      s1md=0.0
      do k=ks,ke
         sdmd=sdmd+g_moist(i,j,k,P_QV)*dnw(k)
         s1md=s1md+(1.0+moist(i,j,k,P_QV))*dnw(k)
      enddo
      xa%psfc(i,j)=-xb%psac(i,j)*sdmd-g_mu_2(i,j)*s1md
   enddo
   enddo

!------------------------------------------------------------------------------
!  [3.0] COMPUTE pressure increments 
!------------------------------------------------------------------------------

   do j=js,je
   do i=is,ie

     k=ke

     qvf1  =g_moist(i,j,k,P_QV)
     qvf1_b=moist(i,j,k,P_QV)
     qvf2  =-qvf1/ ((1.+qvf1_b)*(1.+qvf1_b))
     qvf2_b=1./(1.+qvf1)
     qvf1  =qvf1*qvf2_b + qvf1_b*qvf2
     qvf1_b=qvf1_b*qvf2_b

     p_agrid(i,j,k)=(-0.5/rdnw(k))*                                   &
                    ((g_mu_2(i,j)+qvf1*mub(i,j))/qvf2_b               &
                    -(mu_2(i,j)+qvf1_b*mub(i,j))*qvf2/(qvf2_b*qvf2_b))

     do k = ke-1,1,-1

       qvf1  =0.5*(g_moist(i,j,k,P_QV)+g_moist(i,j,k+1,P_QV))
       qvf1_b=0.5*(  moist(i,j,k,P_QV)+  moist(i,j,k+1,P_QV))
       qvf2  =-qvf1/((1.+qvf1_b)*(1.+qvf1_b))
       qvf2_b=1./(1.+qvf1_b)
       qvf1  =qvf1*qvf2_b + qvf1_b*qvf2
       qvf1_b=qvf1_b*qvf2_b

       p_agrid(i,j,k)=p_agrid(i,j,k+1)-(1./rdn(k+1)) *                 &
                      ((g_mu_2(i,j)+qvf1*mub(i,j))/qvf2_b              &
                      -(mu_2(i,j)+qvf1_b*mub(i,j))*qvf2/(qvf2_b*qvf2_b))
     enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [4.0] CONVERT THETA INCREMENTS TO T INCREMENTS
!------------------------------------------------------------------------------

   do k=ks,ke
   do j=js,je
   do i=is,ie
      xa%t(i,j,k)=(g_t_2(i,j,k)+t_2(i,j,k)+ts0)/ &
                  (ps0/(p_agrid(i,j,k)+xb%p(i,j,k)))**kappa &
                  - xb%t(i,j,k)
   enddo
   enddo
   enddo

! ----------------------------------------------------------------------------
![5.0] CONVERT FROM C-GRID TO A-GRID
! ----------------------------------------------------------------------------

#ifdef DM_PARALLEL
!--Fill the halo region for u and v.
   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)
#endif

   do k=ks,ke
   do j=js,je
   do i=is,ie
      xa%u(i,j,k)=0.5*(g_u_2(i+1,j  ,k)+g_u_2(i,j,k))
      xa%v(i,j,k)=0.5*(g_v_2(i  ,j+1,k)+g_v_2(i,j,k))
   enddo
   enddo
   enddo

!------------------------------------------------------------------------------
!  [6.0] ALL THE SIMPLE ONES
!------------------------------------------------------------------------------

   do j=js,je
   do k=ks,ke+1
   do i=is,ie
      xa%w(i,j,k)=g_w_2(i,j,k)
   enddo
   enddo
   enddo

#if 0
   if ( size(moist,dim=4) >= 4 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         xa%qcw(i,j,k)=g_moist(i,j,k,p_qcw)
         xa%qrn(i,j,k)=g_moist(i,j,k,p_qrn)
      enddo
      enddo
      enddo
   endif

   if ( size(moist,dim=4) >= 6 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         xa%qci(i,j,k)=g_moist(i,j,k,p_qci)
         xa%qsn(i,j,k)=g_moist(i,j,k,p_qsn)
      enddo
      enddo
      enddo
   endif

   if ( size(moist,dim=4) >= 7 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         xa%qgr(i,j,k)=g_moist(i,j,k,p_qgr) 
      enddo
      enddo
      enddo
   endif
#endif

#endif

END SUBROUTINE DA_Transfer_WRFTLtoXa

