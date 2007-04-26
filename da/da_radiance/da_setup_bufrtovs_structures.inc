subroutine da_setup_bufrtovs_structures( grid, ob, iv )

   !---------------------------------------------------------------------------
   ! Purpose: Define, allocate and read of tovs raidance observation structure.
   !---------------------------------------------------------------------------

   implicit none

   type (domain) ,  intent (in)    :: grid       ! model data
   type ( y_type), intent(inout)   :: ob         ! Observation structure.
   type (ob_type), intent(inout)   :: iv         ! O-B structure.

   character(len=200)          :: filename
   integer                     :: i, j, n, ios
   logical                     :: lprinttovs 

   ! thinning variables
   integer  :: istart,iend,jstart,jend
   
   if (trace_use) call da_trace_entry("da_setup_bufrtovs_structures")

   !-------------------------------------------------------------------
   ! [1.0] Initialize RTTOV coefs and innovations vector for radiance
   !-------------------------------------------------------------------

    if (rtm_option == rtm_option_rttov) then
       call da_rttov_init(iv,ob)
    end if

    if (rtm_option == rtm_option_crtm) then
#ifdef CRTM
       call da_crtm_init(iv,ob)
#endif
    end if

   !-------------------------------
   ! 1.1 Make thinning grids
   !------------------------------
   call init_constants_derived

   if (thinning) then
      rlat_min =  r999
      rlat_max = -r999
      rlon_min =  r999
      rlon_max = -r999

      istart=grid%i_start(1)
      iend  =grid%i_end  (1)
      jstart=grid%j_start(1)
      jend  =grid%j_end  (1)

      do i = istart, iend
         do j = jstart, jend
            rlat_min=min(rlat_min, grid%xb%lat(i,j))
            rlat_max=max(rlat_max, grid%xb%lat(i,j))
            if( grid%xb%lon(i,j) < zero) then
              rlon_min=min(rlon_min, (r360+grid%xb%lon(i,j)))
              rlon_max=max(rlon_max, (r360+grid%xb%lon(i,j)))
            else
              rlon_min=min(rlon_min, grid%xb%lon(i,j))
              rlon_max=max(rlon_max, grid%xb%lon(i,j))
            endif
         enddo
      enddo

      dlat_grid = rlat_max - rlat_min
      dlon_grid = rlon_max - rlon_min

      allocate(thinning_grid(iv%num_inst))
      do n=1,iv%num_inst
          call makegrids (n,thinning_mesh(n))
      end do
   end if

   !-------------------------------------------------------------------
   ! [2.0] Read NCEP bufr tovs data in radiance innovations vector
   !-------------------------------------------------------------------

   if (.not. use_filtered_rad) then

      if (use_hirs2obs) then
         write(unit=stdout,fmt='(a)') 'Reading radiance 1b data from hirs2.bufr'
         filename = ' '
         write(filename(1:10), fmt='(a)') 'hirs2.bufr'
         call da_read_bufrtovs ('hirs2', iv, grid%xp, filename)
      end if

      if (use_msuobs) then
         filename = ' '
         write(unit=stdout,fmt='(a)') 'Reading radiance 1b data from msu.bufr'
         write(filename(1:8), fmt='(a)') 'msu.bufr'
         call da_read_bufrtovs ('msu  ', iv, grid%xp, filename)
      end if

      if (use_hirs3obs) then
         write(unit=stdout,fmt='(a)') 'Reading radiance 1b data from hirs3.bufr'
         filename = ' '
         write(filename(1:10), fmt='(a)') 'hirs3.bufr'
         call da_read_bufrtovs('hirs3', iv, grid%xp, filename)
      end if

      if (use_amsuaobs) then
         write(unit=stdout,fmt='(a)') 'Reading radiance 1b data from amsua.bufr'
         filename = ' '
         write(filename(1:10), fmt='(a)') 'amsua.bufr'
         call da_read_bufrtovs ('amsua', iv, grid%xp, filename)
      end if

      if (use_amsubobs) then
         write(unit=stdout,fmt='(a)') 'Reading radiance 1b data from amsub.bufr'
         filename = ' '
         write(filename(1:10), fmt='(a)') 'amsub.bufr'
         call da_read_bufrtovs ('amsub', iv, grid%xp, filename)
      end if

      if (use_hirs4obs) then
         write(unit=stdout,fmt='(a)') 'Reading radiance 1b data from hirs4.bufr'
         filename = ' '
         write(filename(1:10), fmt='(a)') 'hirs4.bufr'
         call da_read_bufrtovs('hirs4', iv, grid%xp, filename)
      end if

      if (use_mhsobs) then
         write(unit=stdout,fmt='(a)') 'Reading radiance 1b data from mhs.bufr'
         filename = ' '
         write(filename(1:8), fmt='(a)') 'mhs.bufr'
         call da_read_bufrtovs('mhs  ', iv, grid%xp, filename)
      end if

      if (use_airsobs) then
         write(unit=stdout,fmt='(a)') 'Reading airs 1b data from airs.bufr'
         filename = ' '
         write(filename(1:9), fmt='(a)') 'airs.bufr'
         call da_read_bufrairs ('airs     ',iv, grid%xp, filename)
      end if

      if (use_eos_amsuaobs) then
         write(unit=stdout,fmt='(a)') 'Reading eos_amsua 1b data from airs.bufr'
         filename = ' '
         write(filename(1:9), fmt='(a)') 'airs.bufr'
         call da_read_bufrairs ('eos_amsua',iv, grid%xp, filename)
      end if

      if (use_hsbobs) then
         write(unit=stdout,fmt='(a)') 'Reading hsb 1b data from airs.bufr'
         filename = ' '
         write(filename(1:9), fmt='(a)') 'airs.bufr'
         call da_read_bufrairs ('hsb      ',iv, grid%xp, filename)
      end if

      if (use_ssmisobs) then
         write(unit=stdout,fmt='(a)') 'Reading ssmis data from ssmis.bufr'
         filename = ' '
         write(filename(1:10), fmt='(a)') 'ssmis.bufr'
         call da_read_bufrssmis ('ssmis',iv, grid%xp, filename)
      end if

   else

      call da_read_filtered_rad (grid%xp, iv)

   end if

   if (use_kma1dvar) then
      do i=1,rtminit_nsensor
         filename = ' '
         filename='kma1dvar-'//trim(iv%instid(i)%rttovid_string)//'.inv'
         write(unit=stdout,fmt='(a,a)')  ' Reading KMA 1dvar innovation from  ', filename
         call da_read_kma1dvar (i,iv, ob, grid%xp, filename)
      end do
   end if

   if (thinning) then
      do n=1,iv%num_inst
         call destroygrids (n)
      end do
      deallocate(thinning_grid)
   end if

   ! sorting obs into FGAT time bins
   call da_sort_rad(iv)

   ! allocate cloud_flag in iv structure
   !   do i = 1,  iv % num_inst
   !      if (iv % instid(i) % num_rad < 1) cycle
   !         iv%instid(i)%cloud_flag(:,:) = 1  ! no cloud
   !   end do

   !-----------------------------------------------------------------------------
   ! [3.0] create (smaller) ob structure:
   !-----------------------------------------------------------------------------

   if (.not. use_kma1dvar) then
      do i = 1,  ob % num_inst
         ob % instid(i) % num_rad = iv % instid(i) % num_rad
         if (ob % instid(i) % num_rad < 1) cycle
         allocate (ob % instid(i) % tb(ob % instid(i) % nchan,ob % instid(i)%num_rad))
         ob % instid(i) % tb(:,:) = iv % instid(i) % tb_inv(:,:)
      end do
   end if

   if (trace_use) call da_trace_exit("da_setup_bufrtovs_structures")

end subroutine da_setup_bufrtovs_structures

