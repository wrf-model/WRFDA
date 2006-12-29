subroutine da_read_iv_rad_binary (xp, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: write out innovation vector structure for radiance data.
   !
   ! METHOD: write out for separated PEs 
   !---------------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in)     :: xp       ! Domain decomposition vars.
   type (y_type),     intent(inout)  :: ob       ! Observation structure.
   type (ob_type),    intent(inout)  :: iv       ! O-B structure.

   integer                        :: n        ! Loop counter.
   integer                        :: i, j, k  ! Index dimension.
   integer                        :: num_levs ! Number of obs levels.

   integer            :: iv_rad_binary_unit, ios
   character(len=30)  :: filename
   integer            :: ndomain

   if (trace_use) call da_trace_entry("da_read_iv_rad_binary")

   do i = 1, iv%num_inst

#ifdef DM_PARALLEL
      write(filename, '(a,i2.2)') trim(iv%instid(i)%rttovid_string)//'.inv_bin_', myproc
#else
      write(filename, '(a)') trim(iv%instid(i)%rttovid_string)//'.inv_bin'
#endif
      call da_get_unit(iv_rad_binary_unit)
      open(iv_rad_binary_unit,file=trim(filename),form='unformatted',iostat=ios)
      if (ios /= 0 ) Then
         call da_warning(__FILE__,__LINE__,(/"cannot open the file "//trim(filename)/))
         iv%instid(i)%num_rad = 0
         cycle
      Endif

      read(iv_rad_binary_unit) iv%instid(i)%num_rad, iv%instid(i)%nchan,iv%instid(i)%ichan
      do n =1,iv%instid(i)%num_rad
            read(iv_rad_binary_unit) ndomain, &
                                iv%instid(i)%rad(n)%info%date_char, &
                                iv%instid(i)%rad(n)%scanpos,   &
                                iv%instid(i)%rad(n)%landsea_mask, &
                                iv%instid(i)%rad(n)%info%elv,  &
                                iv%instid(i)%rad(n)%info%lat,  &
                                iv%instid(i)%rad(n)%info%lon, &
                                iv%instid(i)%rad(n)%satzen,    &
                                iv%instid(i)%rad(n)%satazi
            read(iv_rad_binary_unit) &
                                iv%instid(i)%rad(n)%t2m, &
                                iv%instid(i)%rad(n)%mr2m,   &
                                iv%instid(i)%rad(n)%u10, &
                                iv%instid(i)%rad(n)%v10,  &
                                iv%instid(i)%rad(n)%ps,  &
                                iv%instid(i)%rad(n)%ts,  &
                                iv%instid(i)%rad(n)%smois,  &
                                iv%instid(i)%rad(n)%tslb,  &
                                iv%instid(i)%rad(n)%snowh, &
                                iv%instid(i)%rad(n)%isflg, &
                                nint(iv%instid(i)%rad(n)%soiltyp), &
                                nint(iv%instid(i)%rad(n)%vegtyp), &
                                iv%instid(i)%rad(n)%vegfra, &
                                iv%instid(i)%rad(n)%elevation, &
                                iv%instid(i)%rad(n)%clwp

            read(iv_rad_binary_unit)  ob%instid(i)%rad(n)%tb(:)
            read(iv_rad_binary_unit)  iv%instid(i)%rad(n)%tb(:)%inv
            read(iv_rad_binary_unit)  iv%instid(i)%rad(n)%emiss(1:iv%instid(i)%nchan)

            read(iv_rad_binary_unit)  iv%instid(i)%rad(n)%tb(:)%error
            read(iv_rad_binary_unit)  iv%instid(i)%rad(n)%tb(:)%qc

            if (write_profile) then
               read(iv_rad_binary_unit) num_levs, &
                  coefs(i) % ref_prfl_p(1:num_levs) , &
                  iv%instid(i)%rad(n)%t(1:num_levs) , &
                  iv%instid(i)%rad(n)%mr(1:num_levs)

               read(iv_rad_binary_unit) num_model_levs, &
                  iv%instid(i)%rad(n)%pm(xp%kts:xp%kte) , &
                  iv%instid(i)%rad(n)%tm(xp%kts:xp%kte) , &
                  iv%instid(i)%rad(n)%qm(xp%kts:xp%kte) , &
                  iv%instid(i)%rad(n)%qcw(xp%kts:xp%kte), &
                  iv%instid(i)%rad(n)%qrn(xp%kts:xp%kte)
            end if   ! end if write_profile
         end if    ! end if proc_domain
      end do     ! end do pixels
      close(iv_rad_binary_unit)
      call da_free_unit(iv_rad_binary_unit)
   end do    !! end do instruments

   if (trace_use) call da_trace_exit("da_read_iv_rad_binary")
 
end subroutine da_read_iv_rad_binary


