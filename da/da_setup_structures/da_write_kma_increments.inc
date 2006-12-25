subroutine da_write_kma_increments(xp, xb, xa)

   !---------------------------------------------------------------------------
   ! Purpose: Gathers KMA analysis increments and writes 
   !           on i"anl_inc_unit" unit 
   !---------------------------------------------------------------------------

   implicit none

   type(xb_type),intent(in)                     :: xb
   type(x_type),intent(in)                      :: xa
   type(xpose_type),intent(in)                  :: xp

   ! Arrays for write out increments:
   integer                                     :: ix, jy, kz
   real, dimension(1:xb%mix,1:xb%mjy)          :: gbuf_2d
   real, dimension(1:xb%mix,1:xb%mjy,1:xb%mkz) :: gbuf

   real, dimension(:,:)  , allocatable :: psfc_g
   real, dimension(:,:,:), allocatable :: u_g, v_g, t_g, q_g, p_g

   integer                                     :: i, j, k,anl_inc_unit

   if (trace_use) call da_trace_entry("da_write_kma_increments")

   ! Dimension of the domain:
   ix = xb%mix
   jy = xb%mjy
   kz = xb%mkz

#ifdef DM_PARALLEL
 
   ! 3-d and 2-d increments:

   allocate (psfc_g (1:ix,1:jy))
   allocate (   u_g (1:ix,1:jy,1:kz))
   allocate (   v_g (1:ix,1:jy,1:kz))
   allocate (   t_g (1:ix,1:jy,1:kz))
   allocate (   q_g (1:ix,1:jy,1:kz))
   allocate (   p_g (1:ix,1:jy,1:kz))

   call da_local_to_global(xp, xa % psfc, gbuf_2d, 2) 
   if (rootproc) then 
      psfc_g(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if 

   call da_local_to_global(xp, xa % u, gbuf, 3) 
   if (rootproc) then 
      u_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 

   call da_local_to_global(xp, xa % v, gbuf, 3) 
   if (rootproc) then 
      v_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 

   call da_local_to_global(xp, xa % t, gbuf, 3) 
   if (rootproc) then 
      t_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 

   call da_local_to_global(xp, xa % q, gbuf, 3) 
   if (rootproc) then 
      q_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 

   call da_local_to_global(xp, xa % p, gbuf, 3) 
   if (rootproc) then 
      p_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 
#endif

   if (rootproc) then

#ifdef DM_PARALLEL

      ! 3d- and 2d-increments:

      call da_get_unit(anl_inc_unit)
      open(unit=anl_inc_unit,file="ANALYSIS_INCREMEMTS_KMA",status="replace")

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

      write(unit=stdout,fmt='(10e15.7)') & 
         ((psfc_g(i,j),i=xb%ids,xb%ide),j=xb%jds,xb%jde)
      write(unit=stdout,fmt='(10e15.7)') &
         (((u_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)
      write(unit=stdout,fmt='(10e15.7)') &
         (((v_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)
      write(unit=stdout,fmt='(10e15.7)') &
         (((t_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)
      write(unit=stdout,fmt='(10e15.7)') &
         (((q_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)
      write(unit=stdout,fmt='(10e15.7)') &
         (((p_g(i,j,k),i=xb%ids,xb%ide),j=xb%ids,xb%jde),k=xb%kds,xb%kde)

#else

      ! 3d- and 2d-increments:

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

      write(unit=stdout,fmt='(10e15.7)') &
         ((xa%psfc(i,j),i=xb%ids,xb%ide),j=xb%jds,xb%jde)
      write(unit=stdout,fmt='(10e15.7)') &
         (((xa%u(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
      write(unit=stdout,fmt='(10e15.7)') &
         (((xa%v(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
      write(unit=stdout,fmt='(10e15.7)') &
         (((xa%t(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
      write(unit=stdout,fmt='(10e15.7)') &
         (((xa%q(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
      write(unit=stdout,fmt='(10e15.7)') &
         (((xa%p(i,j,k),i=xb%ids,xb%ide),j=xb%jds,xb%jde),k=xb%kds,xb%kde)
#endif
      close(anl_inc_unit)
      call da_free_unit(anl_inc_unit)

   end if

   if (trace_use) call da_trace_exit("da_write_kma_increments")

end subroutine da_write_kma_increments 


