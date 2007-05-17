subroutine da_write_increments (xp, xb, xa, q_cgrid, mu_cgrid, ph_cgrid)

   !----------------------------------------------------------------------
   ! Purpose: Write analysis increments
   !----------------------------------------------------------------------

   implicit none

   type(xb_type), intent(in)                            :: xb
   type(x_type), intent(in)                             :: xa
   type(xpose_type), intent(inout)                      :: xp
   real,intent(in) :: q_cgrid(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme)
   real,intent(in) :: ph_cgrid(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme)
   real,intent(in) :: mu_cgrid(xp%ims:xp%ime,xp%jms:xp%jme)

   ! Arrays for write out increments:
   integer                                          :: ix, jy, kz
#ifdef DM_PARALLEL
   real, dimension(1:xb%mix,1:xb%mjy)               ::     gbuf_2d
   real, dimension(1:xb%mix+1,1:xb%mjy+1)           ::     gbuf_2dd
   real, dimension(1:xb%mix,1:xb%mjy,1:xb%mkz)      ::     gbuf

   real, dimension(1:xb%mix,1:xb%mjy,1:xb%mkz+1)    ::    wgbuf
   real, dimension(:,:,:), allocatable :: u_global, v_global, w_global, &
                                          p_global, t_global, q_global, &
                                         ph_global
   real, dimension(:,:)  , allocatable :: mu_global, psfc_global, &
                       psac_global, tgrn_global, terr_global, snow_global,&
                        lat_global,  lon_global, lanu_global,             &
                 map_factor_global, cori_global, landmask_global
#endif

   integer :: anl_inc_unit

   if (trace_use) call da_trace_entry("da_write_increments")


   ! Dimension of the domain:
   ix = xb%mix
   jy = xb%mjy
   kz = xb%mkz

#ifdef DM_PARALLEL
 
   ! 3-d and 2-d increments:

   allocate (   p_global (1:ix+1,1:jy+1,1:kz+1))
   allocate (   t_global (1:ix+1,1:jy+1,1:kz+1))
   allocate (   q_global (1:ix+1,1:jy+1,1:kz+1))
   allocate (   u_global (1:ix+1,1:jy+1,1:kz+1))
   allocate (   v_global (1:ix+1,1:jy+1,1:kz+1))
   allocate (   w_global (1:ix+1,1:jy+1,1:kz+1))
   allocate (  ph_global (1:ix+1,1:jy+1,1:kz+1))
   allocate (psfc_global (1:ix+1,1:jy+1))
   allocate (  mu_global (1:ix+1,1:jy+1))
   call da_local_to_global(xp, xa % p, gbuf, 3) 
   if (rootproc) then 
      p_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 
   call da_local_to_global(xp, xa % t, gbuf, 3) 
   if (rootproc) then 
      t_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 
   call da_local_to_global(xp, q_cgrid, gbuf, 3) 
   if (rootproc) then 
      q_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 
   call da_local_to_global(xp, xa % u, gbuf, 3) 
   if (rootproc) then 
      u_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 
   call da_local_to_global(xp, xa % v, gbuf, 3) 
   if (rootproc) then 
      v_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if

   ! One more level for w and ph:
   xp%kde = xp%kde+1
   call da_local_to_global(xp, xa % w, wgbuf, 3) 
   if (rootproc) then 
      w_global(1:ix,1:jy,1:kz+1) = wgbuf(1:ix,1:jy,1:kz+1) 
   end if 
   call da_local_to_global(xp, ph_cgrid, wgbuf, 3) 
   if (rootproc) then 
      ph_global(1:ix,1:jy,1:kz+1) = wgbuf(1:ix,1:jy,1:kz+1) 
   end if 
   xp%kde = xp%kde-1
 
   call da_local_to_global(xp, xa % psfc, gbuf_2d, 2) 
   if (rootproc) then 
      psfc_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if 
   call da_local_to_global(xp, mu_cgrid, gbuf_2d, 2) 
   if (rootproc) then 
      mu_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if 

   ! 2d constant fields:

   allocate (      psac_global (1:ix+1,1:jy+1))
   allocate (      tgrn_global (1:ix+1,1:jy+1))
   allocate (      terr_global (1:ix+1,1:jy+1))
   allocate (      snow_global (1:ix+1,1:jy+1))
   allocate (       lat_global (1:ix+1,1:jy+1))
   allocate (       lon_global (1:ix+1,1:jy+1))
   allocate (      lanu_global (1:ix+1,1:jy+1))
   allocate (map_factor_global (1:ix+1,1:jy+1))
   allocate (      cori_global (1:ix+1,1:jy+1))
   allocate (  landmask_global (1:ix+1,1:jy+1))

   call da_local_to_global(xp, xb%psac, gbuf_2d, 2) 
   if (rootproc) then 
      psac_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_local_to_global(xp, xb%tgrn, gbuf_2d, 2) 
   if (rootproc) then 
      tgrn_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_local_to_global(xp, xb%terr, gbuf_2d, 2) 
   if (rootproc) then 
      terr_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_local_to_global(xp, xb%snow, gbuf_2d, 2) 
   if (rootproc) then 
      snow_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_local_to_global(xp, xb%lat , gbuf_2d, 2) 
   if (rootproc) then 
      lat_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_local_to_global(xp, xb%lon , gbuf_2d, 2) 
   if (rootproc) then 
      lon_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_local_to_global(xp, xb%lanu, gbuf_2d, 2) 
   if (rootproc) then 
      lanu_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_local_to_global(xp, xb%map_factor, gbuf_2d, 2) 
   if (rootproc) then 
      map_factor_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_local_to_global(xp, xb%cori, gbuf_2dd, 2) 
   if (rootproc) then 
      cori_global(1:ix+1,1:jy+1) = gbuf_2dd(1:ix+1,1:jy+1) 
   end if
   call da_local_to_global(xp, xb%landmask, gbuf_2d, 2) 
   if (rootproc) then 
      landmask_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if

#endif

   if (rootproc) then
      call da_get_unit(anl_inc_unit)
      open(unit=anl_inc_unit, file='analysis_increment', form='unformatted')

      write (unit=anl_inc_unit) ANALYSIS_DATE

      write (unit=anl_inc_unit) 1, ix, 1, jy, 1, kz 

      ! Map projection information:
      write (unit=anl_inc_unit) map_projection, coarse_ix, coarse_jy
      write (unit=anl_inc_unit) &
         coarse_ds, start_x, start_y, &
         phic, xlonc, cone_factor, truelat1_3dv, truelat2_3dv, pole, dsm,   &
         psi1, c2, ptop, base_pres, t0, base_lapse, base_temp

      ! 1d constant fields:

      write (unit=anl_inc_unit) xb%sigmah, xb%sigmaf

#ifdef DM_PARALLEL

      ! 3d- and 2d-increments:
      write (unit=anl_inc_unit) u_global, v_global, w_global, p_global, &
         t_global, q_global, ph_global, mu_global, psfc_global

      ! 2d-constant fields:
      write (unit=anl_inc_unit) psac_global, tgrn_global, terr_global, &
         snow_global, lat_global, lon_global, lanu_global, map_factor_global, &
         cori_global, landmask_global
      close(anl_inc_unit)
      call da_free_unit(anl_inc_unit)
#else

      ! 3d- and 2d-increments:
      write (unit=anl_inc_unit) xa%u(1:ix+1,1:jy+1,1:kz+1), &
                    xa%v(1:ix+1,1:jy+1,1:kz+1), &
                    xa%w(1:ix+1,1:jy+1,1:kz+1), &
                    xa%p(1:ix+1,1:jy+1,1:kz+1), &
                    xa%t(1:ix+1,1:jy+1,1:kz+1), &
                    q_cgrid(1:ix+1,1:jy+1,1:kz+1), &
                    ph_cgrid(1:ix+1,1:jy+1,1:kz+1), &
                    mu_cgrid(1:ix+1,1:jy+1), &
                    xa%psfc(1:ix+1,1:jy+1)

      !    .. 2d-constant fields:
      write (unit=anl_inc_unit) xb%psac(1:ix+1,1:jy+1), &
                    xb%tgrn(1:ix+1,1:jy+1), &
                    xb%terr(1:ix+1,1:jy+1), &
                    xb%snow(1:ix+1,1:jy+1), &
                    xb%lat(1:ix+1,1:jy+1), &
                    xb%lon(1:ix+1,1:jy+1), &
                    xb%lanu(1:ix+1,1:jy+1), &
                    xb%map_factor(1:ix+1,1:jy+1), &
                    xb%cori(1:ix+1,1:jy+1), &
                    xb%landmask(1:ix+1,1:jy+1)
      close(anl_inc_unit)
      call da_free_unit(anl_inc_unit)
#endif

   end if

   if (trace_use) call da_trace_exit("da_write_increments")

end subroutine da_write_increments


