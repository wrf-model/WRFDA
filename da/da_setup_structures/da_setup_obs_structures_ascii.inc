subroutine da_setup_obs_structures_ascii( xp, ob, iv, xb )

   !---------------------------------------------------------------------------
   ! Purpose: Define, allocate and read of observation structure.
   !---------------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in)    :: xp  ! Domain decomposition vars.
   type (y_type),     intent(out)   :: ob  ! Observation structure.
   type (ob_type),    intent(out)   :: iv  ! O-B structure.
   type (xb_type),    intent(inout) :: xb  ! First guess structure

   character(len=120)           :: filename
   integer                      :: n
   integer                      :: i, j, k

   if (trace_use) call da_trace_entry("da_setup_obs_structures_ascii")

   !--------------------------------------------------------------------------
   ! [1.0] Scan GTS observation header and get idea of number of obs:
   !--------------------------------------------------------------------------
  
   if (num_fgat_time > 1) then
      filename = ' '

      do n=1, num_fgat_time
         iv%current_ob_time = n

         write(filename(1:10), fmt='(a, i2.2,a)') 'ob', n,'.ascii'

         ! scan main body of gts observation file
         call da_scan_obs (iv, xp, filename)

         if (use_ssmiretrievalobs .or. use_ssmitbobs) then
            ! scan SSMI observation file
            write(filename(1:10), fmt='(a, i2.2,a)') 'ssmi', n,'.dat'
            call da_scan_ssmi (iv, xp, filename)
         end if

         if (use_radarobs) then
            ! scan radar observation file
            write(filename(1:10), fmt='(a, i2.2,a)') 'radar', n,'dat'
            call da_scan_radar(iv, xp, filename)
         end if
      end do
   else
      iv%current_ob_time = 1

      call da_scan_obs(iv, xp,'ob01.ascii')

      !-----------------------------------------------------------------------
      ! read header of ssmi observation file
      !-----------------------------------------------------------------------
      if (use_ssmiretrievalobs .or. use_ssmitbobs) then
         call da_scan_ssmi(iv, xp,'ssmi01.dat')
      end if

      ! scan main body of radar observation file
      if (use_radarobs) then
         call da_scan_radar(iv, xp,'radar01.dat')
      end if
   end if

   !--------------------------------------------------------------------------
   ! Allocate the ob based on input number of obs:
   !--------------------------------------------------------------------------

   call da_allocate_observations (iv)

   if (num_fgat_time > 1) then
      do n=1, num_fgat_time
         iv%current_ob_time = n

         write(filename(1:10), fmt='(a, i2.2,a)') 'ob', n,'.ascii'

         ! Read gts observation file
         call da_read_obs (iv, xp, filename)

         if (use_ssmiretrievalobs .or. use_ssmitbobs) then
            ! read ssmi observation file
            write(filename(1:10), fmt='(a, i2.2)') 'ssmi', n,'.dat'
            call da_read_ssmi (iv, xp, filename)
         end if

         if (use_radarobs) then
            ! read radar observation file
            write(filename(1:11), fmt='(a, i2.2)') 'radar', n,'.dat'
            call da_read_radar(iv, xp, filename)
         end if
      end do
   else
      iv%current_ob_time = 1

      call da_read_obs(iv, xp, 'ob01.ascii')

      if (use_ssmiretrievalobs .or. use_ssmitbobs) then
         ! read ssmi observation file
         call da_read_ssmi (iv, xp,'ssmi01.dat')
      end if

      if (use_radarobs) then
         ! read radar observation file
         call da_read_radar(iv, xp,'radar01.dat')
      end if
   end if

   ! Calculate DT for RF DA

   if (use_radarobs .and. use_radar_rf) then
      if (.not. DT_cloud_model) then
         do j = xp%jts, xp%jte
            do i = xp%its, xp%ite
               do k = xp%kts, xp%kte
                   xb%delt(i,j,k) = 0.0
               end do
            end do
        end do

        do n = 1, iv % num_radar
           do i=int(iv%radar(n)%loc%i), int(iv%radar(n)%loc%i)+1
              do j=int(iv%radar(n)%loc%j), int(iv%radar(n)%loc%j)+1
                 do k=xp%kts, xp%kte
                    xb%delt(i,j,k) = 1800.0
                    xb%qrn(i,j,k) = amax1(5.0E-5, xb%qrn(i,j,k))
                    xb%qcw(i,j,k) = amax1(5.0E-12, xb%qcw(i,j,k))
                  end do
                  ! do k=xp%kts, xp%kte
                  !    v_h(k)=xb%h(i,j  ,k)
                  ! end do
                  ! do k = 1, iv % radar(n) % info % levels
                  !    call zk(iv%radar(n)%height(k), v_h, xp, zkk)
                  !    xb%delt(i,j,int(zkk))=1800.0
                  !    xb%delt(i,j,int(zkk)+1)=1800.0
                  ! end do
               end do
            end do
         end do
      end if
   end if

   !--------------------------------------------------------------------------
   ! [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
   !--------------------------------------------------------------------------

   call da_fill_obs_structures(xp, iv, ob)

   iv%current_ob_time = 1

   if (trace_use) call da_trace_exit("da_setup_obs_structures_ascii")

end subroutine da_setup_obs_structures_ascii


