SUBROUTINE DA_Transfer_WRFTLtoXa_Adj( grid, config_flags, filnam, &
!
#include <em_dummy_args.inc>
!
                              )

!------------------------------------------------------------------------------
!  PURPOSE: Convert analysis increments into WRFAD increments 
!           (following xatowrf, but only keep the increments)
!                                                Hans Huang, 8 March 2005
!           
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   type(domain) ,   TARGET                   :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4 filnam

!  Definitions of dummy arguments to solve

#include <em_dummy_decl.inc>

   integer :: i, j, k, n, ndynopt
   integer :: is, ie, js, je, ks, ke

#ifdef WRFVAR

!---------------------------------------------------------------------------

   is = xp % its
   ie = xp % ite
   js = xp % jts
   je = xp % jte
   ks = xp % kts
   ke = xp % kte

!------------------------------------------------------------------------------
#ifdef DM_PARALLEL
!--Fill the halo region for u and v.

   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)

   do j=js,je
   do k=ks,ke
      do i=is,ie
         g_u_2(i,j,k)=0.5*(xa%u(i-1,j  ,k)+xa%u(i,j,k))
         g_v_2(i,j,k)=0.5*(xa%v(i  ,j-1,k)+xa%v(i,j,k))
      enddo
   enddo
   enddo

!--Fill the boundary

!--The southern boundary
   if(js == xp%jds) then
      g_v_2(is:ie,js,ks:ke)=2.0*xa%v(is:ie,js  ,ks:ke) &
                           -    xa%v(is:ie,js+1,ks:ke)
   endif

!--The northern boundary
   if(je == xp%jde) then
      g_v_2(is:ie,je+1,ks:ke)=2.0*xa%v(is:ie,je  ,ks:ke) &
                             -    xa%v(is:ie,je-1,ks:ke)
   endif

!--The western boundary
   if(is == xp%ids) then
      g_u_2(is,js:je,ks:ke)=2.0*xa%u(is  ,js:je,ks:ke) &
                           -    xa%u(is+1,js:je,ks:ke)
   endif

!--The eastern boundary
   if(ie == xp%ide) then
      g_u_2(ie+1,js:je,ks:ke)=2.0*xa%u(ie  ,js:je,ks:ke) &
                             -    xa%u(ie-1,js:je,ks:ke)
   endif

#else

!--The eastern boundary
   g_u_2(ie+1,js:je,ks:ke)=2.0*xa%u(ie  ,js:je,ks:ke) &
                          -    xa%u(ie-1,js:je,ks:ke)

!--The northern boundary
   g_v_2(is:ie,je+1,ks:ke)=2.0*xa%v(is:ie,je  ,ks:ke) &
                          -    xa%v(is:ie,je-1,ks:ke)

   do k=ks,ke
      do j=js,je
      do i=is+1,ie
         g_u_2(i,j,k)=0.5*(xa%u(i-1,j,k)+xa%u(i,j,k))
      enddo
      enddo

      do j=js+1,je
      do i=is,ie
         g_v_2(i,j,k)=0.5*(xa%v(i,j-1,k)+xa%v(i,j,k))
      enddo
      enddo
   enddo

!--The western boundary
   g_u_2(is,js:je,ks:ke)=2.0*xa%u(is  ,js:je,ks:ke) &
                        -    xa%u(is+1,js:je,ks:ke)

!--The southern boundary
   g_v_2(is:ie,js,ks:ke)=2.0*xa%v(is:ie,js  ,ks:ke) &
                        -    xa%v(is:ie,js+1,ks:ke)
#endif

!------------------------------------------------------------------------------

      do j=js,je

      do i=is,ie
         g_mu_2(i,j)     = xa%psfc(i,j)
         g_w_2(i,j,ke+1) = xa%w(i,j,ke+1)
         g_ph_2(i,j,ke+1)= 0.0
      enddo

      do k=ks,ke
      do i=is,ie
         g_t_2(i,j,k) = xa%t(i,j,k)
         g_ph_2(i,j,k)= 0.0
         g_w_2(i,j,k) = xa%w(i,j,k)
         g_moist(i,j,k,P_QV) =  xa%q(i,j,k)
      enddo
      enddo

      enddo

#if 0
      if ( size(moist,dim=4) >= 4 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         g_moist(i,j,k,p_qcw) =  xa%qcw(i,j,k)
         g_moist(i,j,k,p_qrn) =  xa%qrn(i,j,k)
      enddo
      enddo
      enddo

      if ( size(moist,dim=4) >= 6 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         g_moist(i,j,k,p_qci) =  xa%qci(i,j,k)
         g_moist(i,j,k,p_qsn) =  xa%qsn(i,j,k)
      enddo
      enddo
      enddo

      if ( size(moist,dim=4) >= 7 ) then
      do k=ks,ke
      do j=js,je
      do i=is,ie
         g_moist(i,j,k,p_qgr) =  xa%qgr(i,j,k)
      enddo
      enddo
      enddo
      endif
      endif
      endif
#endif

!------------------------------------------------------------------------------

   ndynopt      = grid%dyn_opt
   grid%dyn_opt = DYN_EM_TL
   call nl_set_dyn_opt ( 1 , DYN_EM_TL )

   call med_initialdata_output_3dvar( grid , config_flags, filnam )

   grid%dyn_opt = ndynopt
   call nl_set_dyn_opt ( 1 , DYN_EM )

#endif

END SUBROUTINE DA_Transfer_WRFTLtoXa_Adj

