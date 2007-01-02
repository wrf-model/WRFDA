SUBROUTINE DA_Write_KMA_Increments(xp, xb, xa)

!------------------------------------------------------------------------------
!  PURPOSE: Gathers KMA analysis increments and writes 
!           on i"anl_inc_unit" unit  
!
!  HISTORY: 11/10/2004   Syed RH Rizvi
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE(xb_type)                               :: xb
   TYPE(x_type)                                :: xa
   TYPE(xpose_type)                            :: xp

! .. Arrays for write out increments:
   integer                                     :: ix, jy, kz
   real, dimension(1:xb%mix,1:xb%mjy)          :: gbuf_2d
   real, dimension(1:xb%mix,1:xb%mjy,1:xb%mkz) :: gbuf

   real, dimension(:,:)  , allocatable :: psfc_g
   real, dimension(:,:,:), allocatable :: u_g, v_g, t_g, q_g, p_g

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   integer                                     :: i, j, k
! ---------------------------------------------------------------------------

! .. Dimension of the domain:
      ix = xb%mix
      jy = xb%mjy
      kz = xb%mkz

#ifdef DM_PARALLEL
 
! .. 3-d and 2-d increments:

      ALLOCATE ( psfc_g (1:ix,1:jy) )
      ALLOCATE (    u_g (1:ix,1:jy,1:kz) )
      ALLOCATE (    v_g (1:ix,1:jy,1:kz) )
      ALLOCATE (    t_g (1:ix,1:jy,1:kz) )
      ALLOCATE (    q_g (1:ix,1:jy,1:kz) )
      ALLOCATE (    p_g (1:ix,1:jy,1:kz) )

      call local_to_global( xp, xa % psfc, gbuf_2d, 2 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        psfc_g(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
      END IF 

      call local_to_global( xp, xa % u, gbuf, 3 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        u_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
      END IF 

      call local_to_global( xp, xa % v, gbuf, 3 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        v_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
      END IF 

      call local_to_global( xp, xa % t, gbuf, 3 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        t_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
      END IF 

      call local_to_global( xp, xa % q, gbuf, 3 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        q_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
      END IF 

      call local_to_global( xp, xa % p, gbuf, 3 ) 
      IF ( wrf_dm_on_monitor() ) THEN 
        p_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
      END IF 
#endif

 if ( wrf_dm_on_monitor() ) then

#ifdef DM_PARALLEL

!  3d- and 2d-increments:
!
   write(anl_inc_unit) &
   ((psfc_g(i,j),i=xb%ids,xb%ide),j=xb%jds,xb%jde)
   write(anl_inc_unit) &
   (((u_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)
   write(anl_inc_unit) &
   (((v_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)
   write(anl_inc_unit) &
   (((t_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)
   write(anl_inc_unit) &
   (((q_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)
   write(anl_inc_unit) &
   (((p_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)

!
   write(710,'(10e15.7)') & 
   ((psfc_g(i,j),i=xb%ids,xb%ide),j=xb%jds,xb%jde)
   write(710,'(10e15.7)') &
   (((u_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)
   write(710,'(10e15.7)') &
   (((v_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)
   write(710,'(10e15.7)') &
   (((t_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)
   write(710,'(10e15.7)') &
   (((q_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)
   write(710,'(10e15.7)') &
   (((p_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)

#else

!  3d- and 2d-increments:
!
   write(anl_inc_unit) &
   ((xa%psfc(i,j),i=xb%ids,xb%ide),j=xb%jds,xb%jde)
   write(anl_inc_unit) &
   (((xa%u(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
   write(anl_inc_unit) &
   (((xa%v(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
   write(anl_inc_unit) &
   (((xa%t(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
   write(anl_inc_unit) &
   (((xa%q(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
   write(anl_inc_unit) &
   (((xa%p(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
!
   write(710,'(10e15.7)') &
   ((xa%psfc(i,j),i=xb%ids,xb%ide),j=xb%jds,xb%jde)
   write(710,'(10e15.7)') &
   (((xa%u(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
   write(710,'(10e15.7)') &
   (((xa%v(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
   write(710,'(10e15.7)') &
   (((xa%t(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
   write(710,'(10e15.7)') &
   (((xa%q(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
   write(710,'(10e15.7)') &
   (((xa%p(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
#endif

 end if

END SUBROUTINE DA_Write_KMA_Increments 
