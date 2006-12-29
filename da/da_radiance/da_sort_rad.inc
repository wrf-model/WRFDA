subroutine da_sort_rad (iv)

   !---------------------------------------------------------------------------
   ! Purpose: sorting radiance innovation to FGAT time bins
   !---------------------------------------------------------------------------

   implicit none

   type (ob_type), intent (inout) :: iv

   integer                   :: i,j, n,t, error
   type(info_type), pointer  :: info(:)
   integer, allocatable      :: ifgat(:),landsea_mask(:)
   integer, allocatable      :: loc_i(:), loc_j(:),loc_k(:,:)
   real, allocatable         :: loc_dx(:),loc_dy(:),loc_dz(:,:)
   real, allocatable         :: loc_dxm(:),loc_dym(:),loc_dzm(:,:)
   real, allocatable         :: scanpos(:), satzen(:), satazi(:), solzen(:)
   real, allocatable         :: tb_inv(:,:)

   if (trace_use) call da_trace_entry("da_sort_rad")

   if (num_fgat_time == 1) then
      do i=1,rtminit_nsensor
         iv%ob_numb(:)%radiance(i) = 0
         iv%ob_numb(num_fgat_time)%radiance(i) = iv%instid(i)%num_rad
      end do
      if (trace_use) call da_trace_exit("da_sort_rad")
      return
   end if

   do i=1,rtminit_nsensor
      iv%ob_numb(:)%radiance(i) = 0
      if (iv%instid(i)%num_rad < 1) cycle

      allocate (info        (iv%instid(i)%num_rad))
      allocate (ifgat       (iv%instid(i)%num_rad)) 
      allocate (landsea_mask(iv%instid(i)%num_rad))
      allocate (loc_i       (iv%instid(i)%num_rad))
      allocate (loc_j       (iv%instid(i)%num_rad))
      allocate (loc_k       (iv%instid(i)%nchan,iv%instid(i)%num_rad))
      allocate (loc_dx      (iv%instid(i)%num_rad))
      allocate (loc_dy      (iv%instid(i)%num_rad))
      allocate (loc_dz      (iv%instid(i)%nchan,iv%instid(i)%num_rad))
      allocate (loc_dxm     (iv%instid(i)%num_rad))
      allocate (loc_dym     (iv%instid(i)%num_rad))
      allocate (loc_dzm     (iv%instid(i)%nchan,iv%instid(i)%num_rad))
      allocate (scanpos     (iv%instid(i)%num_rad))
      allocate (satzen      (iv%instid(i)%num_rad))
      allocate (satazi      (iv%instid(i)%num_rad))
      allocate (solzen      (iv%instid(i)%num_rad))
      allocate (tb_inv      (iv%instid(i)%nchan,iv%instid(i)%num_rad))

      j = 0
      do t = 1,num_fgat_time
         do n = 1,iv%instid(i)%num_rad
            if (iv%instid(i)%ifgat(n) /= i) cycle
            j = j + 1
            ifgat(j)        = iv%instid(i)%ifgat(n)
            info(j)         = iv%instid(i)%info(n)
            loc_i(j)        = iv%instid(i)%loc_i(n)
            loc_j(j)        = iv%instid(i)%loc_j(n)
            loc_k(:,j)      = iv%instid(i)%loc_k(:,n)
            loc_dx(j)       = iv%instid(i)%loc_dx(n)
            loc_dy(j)       = iv%instid(i)%loc_dy(n)
            loc_dz(:,j)     = iv%instid(i)%loc_dz(:,n)
            loc_dxm(j)      = iv%instid(i)%loc_dxm(n)
            loc_dym(j)      = iv%instid(i)%loc_dym(n)
            loc_dzm(:,j)    = iv%instid(i)%loc_dzm(:,n)
            landsea_mask(j) = iv%instid(i)%landsea_mask(n)
            scanpos(j)      = iv%instid(i)%scanpos(n)
            satzen(j)       = iv%instid(i)%satzen(n)
            satazi(j)       = iv%instid(i)%satazi(n)
            solzen(j)       = iv%instid(i)%solzen(n)

            tb_inv(1:iv%instid(i)%nchan,j) = iv%instid(i)%tb_inv(1:iv%instid(i)%nchan,n) 
         end do
         iv%ob_numb(t)%radiance(i) = j
      end do

      write(unit=stdout,fmt='(a,2x,a,2x,10i7)') &
         ' FGAT: ',iv%instid(i)%rttovid_string, iv%ob_numb(1:num_fgat_time)%radiance(i)

      do n = 1,iv%instid(i)%num_rad
         iv%instid(i)%ifgat(n)        = ifgat(n)
         iv%instid(i)%info(n)         = info(n)
         iv%instid(i)%loc_i(j)        = loc_i(n)
         iv%instid(i)%loc_j(j)        = loc_j(n)
         iv%instid(i)%loc_k(:,j)      = loc_k(:,n)
         iv%instid(i)%loc_dx(j)       = loc_dx(n)
         iv%instid(i)%loc_dy(j)       = loc_dy(n)
         iv%instid(i)%loc_dz(:,j)     = loc_dz(:,n)
         iv%instid(i)%loc_dxm(j)      = loc_dxm(n)
         iv%instid(i)%loc_dym(j)      = loc_dym(n)
         iv%instid(i)%loc_dzm(:,j)    = loc_dzm(:,n)
         iv%instid(i)%landsea_mask(n) = landsea_mask(n)
         iv%instid(i)%scanpos(n)      = scanpos(n)
         iv%instid(i)%satzen(n)       = satzen(n)
         iv%instid(i)%satazi(n)       = satazi(n)
         iv%instid(i)%solzen(n)       = solzen(n)

         iv%instid(i)%tb_inv(1:iv%instid(i)%nchan,n) = &
                        tb_inv(1:iv%instid(i)%nchan,n)
      end do

      deallocate (info)
      deallocate (ifgat) 
      deallocate (landsea_mask)
      deallocate (loc_i)
      deallocate (loc_j)
      deallocate (loc_k)
      deallocate (loc_dx)
      deallocate (loc_dy)
      deallocate (loc_dz)
      deallocate (loc_dxm)
      deallocate (loc_dym)
      deallocate (loc_dzm)
      deallocate (scanpos)
      deallocate (satzen)
      deallocate (satazi)
      deallocate (solzen)
      deallocate (tb_inv)
   end do

   if (trace_use) call da_trace_exit("da_sort_rad")

end subroutine da_sort_rad


