subroutine da_setup_firstguess_kma(xbx, grid)

   !---------------------------------------------------------------------------
   ! Purpose: Define/allocate components of WRF model state.
   !---------------------------------------------------------------------------

   implicit none

   type (xbx_type),intent(out)         :: xbx    ! Header & non-gridded vars.

   type (domain), intent(inout)        :: grid

   integer           :: i, j
   integer           :: is, ie, js, je, ks, ke
   integer           :: max_wavenumber

   if (trace_use) call da_trace_entry("da_setup_firstguess_kma")

   is = grid%xp % its
   ie = grid%xp % ite
   js = grid%xp % jts
   je = grid%xp % jte
   ks = grid%xp % kts
   ke = grid%xp % kte

   !------------------------------------------------------------------------
   ! [2.0] Copy header info:
   !------------------------------------------------------------------------

   ! rizvi set it to 1 . Actually it should be decided by KMA 
   grid%map_proj = 0
   map_projection = grid%map_proj
   coarse_ix = grid%e_we - grid%s_we + 1
   coarse_jy = grid%e_sn - grid%s_sn + 1

   grid%xb % mix = grid%xp%ide - grid%xp%ids + 1
   grid%xb % mjy = grid%xp%jde - grid%xp%jds + 1
   grid%xb % mkz = grid%xp%kde - grid%xp%kds + 1

   mix = grid%xb % mix
   mjy = grid%xb % mjy
   mkz = grid%xb % mkz

   grid%xb % ds  = 0.001 * grid%dx

   start_x = 1.0
   start_y = 1.0
   start_lat = -90.0
   start_lon = -180.0
   delt_lat = 180.0/real(grid%e_sn - grid%s_sn - 1)
   delt_lon = 360.0/real(grid%e_we - grid%s_we)

   phic        = 0.0
   xlonc       = 0.0
   cone_factor = 0.0


   do j = js,je
      do i = is,ie
         grid%xlat(i,j) = start_lat + real(j-1)*delt_lat
         grid%xlong(i,j) = start_lon + real(i-1)*delt_lon
      end do
   end do

   ! Avoid assigning -90,90 value                                           
   if (grid%xb%jts == grid%xb%jds) then
      grid%xlat(is:ie,j) = -89.9                               
   end if

   if (grid%xb%jte == grid%xb%jde) then
      grid%xlat(is:ie,j) = 89.9                               
   end if

   ! fix map factor and coriolis parameter
      
   grid%f(is:ie,js:je) = 2.0 *earth_omega*sin(pi*grid%xlat(is:ie,js:je)/180.0)

   xbx%inc = 1
   xbx%ni  = grid%e_we - grid%s_we
   xbx%nj  = grid%e_sn - grid%s_sn
   xbx%nk  = grid%e_vert - 1
   xbx% lenwrk    = xbx%ni
   xbx% lenr           = xbx%inc * (xbx%ni - 1) + 1
   max_wavenumber = xbx%ni/2-1
   xbx % alp_size = (xbx%nj+1)*(max_wavenumber+1)*(max_wavenumber+2)/4

   call da_transfer_kmatoxb(xbx, grid)

   if (trace_use) call da_trace_exit("da_setup_firstguess_kma")

end subroutine da_setup_firstguess_kma



   if (grid%xp%kpex > kde) grid%xp%kpex = kde

   ! Indices for xz decomposition

   if (grid%xp%itey > ide) grid%xp%itey = ide
   if (grid%xp%jtey > jde) grid%xp%jtey = jde
   if (grid%xp%ktey > kde) grid%xp%ktey = kde

   if (grid%xp%ipey > ide) grid%xp%ipey = ide
   if (grid%xp%jpey > jde) grid%xp%jpey = jde
   if (grid%xp%kpey > kde) grid%xp%kpey = kde

   ! Copy grid%xpose dimensions from grid structure to grid%xp structure.

   ! Indices for xy decomposition

   grid%xp%ids = ids
   grid%xp%ide = ide
   grid%xp%jds = jds
   grid%xp%jde = jde
   grid%xp%kds = kds
   grid%xp%kde = kde

   grid%xp%ims = ims
   grid%xp%ime = ime
   grid%xp%jms = jms
   grid%xp%jme = jme
   grid%xp%kms = kms
   grid%xp%kme = kme

   grid%xp%ips = ips
   grid%xp%ipe = ipe
   grid%xp%jps = jps
   grid%xp%jpe = jpe
   grid%xp%kps = kps
   grid%xp%kpe = kpe

   if (print_detail_parallel) then
      write(unit=stdout, fmt='(2(a, i4, 5x))') &
           'grid%xp%ids =', grid%xp%ids , 'grid%xp%ide =', grid%xp%ide , &
           'grid%xp%jds =', grid%xp%jds , 'grid%xp%jde =', grid%xp%jde , &
           'grid%xp%kds =', grid%xp%kds , 'grid%xp%kde =', grid%xp%kde
      write(unit=stdout, fmt='(//)')

      write(unit=stdout, fmt='(2(a, i4, 5x))') &
           'grid%xp%ims =', grid%xp%ims , 'grid%xp%ime =', grid%xp%ime , &
           'grid%xp%jms =', grid%xp%jms , 'grid%xp%jme =', grid%xp%jme , &
           'grid%xp%kms =', grid%xp%kms , 'grid%xp%kme =', grid%xp%kme
      write(unit=stdout, fmt='(//)')

      write(unit=stdout, fmt='(2(a, i4, 5x))') &
           'grid%xp%ips =', grid%xp%ips , 'grid%xp%ipe =', grid%xp%ipe , &
           'grid%xp%jps =', grid%xp%jps , 'grid%xp%jpe =', grid%xp%jpe , &
           'grid%xp%kps =', grid%xp%kps , 'grid%xp%kpe =', grid%xp%kpe
      write(unit=stdout, fmt='(//)')

      write(unit=stdout, fmt='(2(a, i4, 5x))') &
           'grid%xp%imsx =', grid%xp%imsx, 'grid%xp%imex=', grid%xp%imex, &
           'grid%xp%jmsx =', grid%xp%jmsx, 'grid%xp%jmex=', grid%xp%jmex, &
           'grid%xp%kmsx =', grid%xp%kmsx, 'grid%xp%kmex=', grid%xp%kmex
      write(unit=stdout, fmt='(//)')

      write(unit=stdout, fmt='(2(a, i4, 5x))') &
           'grid%xp%ipsx =', grid%xp%ipsx, 'grid%xp%ipex=', grid%xp%ipex, &
           'grid%xp%jpsx =', grid%xp%jpsx, 'grid%xp%jpex=', grid%xp%jpex, &
           'grid%xp%kpsx =', grid%xp%kpsx, 'grid%xp%kpex=', grid%xp%kpex
      write(unit=stdout, fmt='(//)')

      write(unit=stdout, fmt='(2(a, i4, 5x))') &
           'grid%xp%imsy =', grid%xp%imsy, 'grid%xp%imey=', grid%xp%imey, &
           'grid%xp%jmsy =', grid%xp%jmsy, 'grid%xp%jmey=', grid%xp%jmey, &
           'grid%xp%kmsy =', grid%xp%kmsy, 'grid%xp%kmey=', grid%xp%kmey
      write(unit=stdout, fmt='(//)')

      write(unit=stdout, fmt='(2(a, i4, 5x))') &
           'grid%xp%ipsy =', grid%xp%ipsy, 'grid%xp%ipey=', grid%xp%ipey, &
           'grid%xp%jpsy =', grid%xp%jpsy, 'grid%xp%jpey=', grid%xp%jpey, &
           'grid%xp%kpsy =', grid%xp%kpsy, 'grid%xp%kpey=', grid%xp%kpey
   end if

   if (trace_use_dull) call da_trace_exit("da_copy_dims")

end subroutine da_copy_dims


