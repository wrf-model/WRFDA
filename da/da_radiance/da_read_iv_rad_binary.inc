subroutine da_read_iv_rad_binary (xp, ob, iv)

   !---------------------------------------------------------------------------
   ! PURPOSE: write out innovation vector structure for radiance data.
   !
   ! METHOD: write out for separated PEs 
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(IN)     :: xp       ! Domain decomposition vars.
   TYPE (y_type),     INTENT(INOUT)  :: ob       ! Observation structure.
   TYPE (ob_type),    INTENT(INOUT)  :: iv       ! O-B structure.

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.

   INTEGER            :: iv_rad_binary_unit, ios
   character(len=30)  :: filename
   integer            :: ndomain

   if (trace_use) call da_trace_entry("da_read_iv_rad_binary")

   do n = 1, iv%num_inst

#ifdef DM_PARALLEL
      write(filename, '(a,i2.2)') trim(iv%instid(n)%rttovid_string)//'.inv_bin_', myproc
#else
      write(filename, '(a)') trim(iv%instid(n)%rttovid_string)//'.inv_bin'
#endif
      open(iv_rad_binary_unit,file=trim(filename),form='unformatted',iostat=ios)
      if (ios /= 0 ) Then
         call da_warning(__FILE__,__LINE__,(/"cannot open the file "//TRIM(filename)/))
         iv%instid(n)%num_rad = 0
         cycle
      Endif

      read(iv_rad_binary_unit) iv%instid(n)%num_rad, iv%instid(n)%nchan,iv%instid(n)%ichan
      do i =1,iv%instid(n)%num_rad
            read(iv_rad_binary_unit) ndomain, &
                                iv%instid(n)%rad(i)%info%date_char, &
                                iv%instid(n)%rad(i)%scanpos,   &
                                iv%instid(n)%rad(i)%landsea_mask, &
                                iv%instid(n)%rad(i)%info%elv,  &
                                iv%instid(n)%rad(i)%info%lat,  &
                                iv%instid(n)%rad(i)%info%lon, &
                                iv%instid(n)%rad(i)%satzen,    &
                                iv%instid(n)%rad(i)%satazi
            read(iv_rad_binary_unit) &
                                iv%instid(n)%rad(i)%t2m, &
                                iv%instid(n)%rad(i)%mr2m,   &
                                iv%instid(n)%rad(i)%u10, &
                                iv%instid(n)%rad(i)%v10,  &
                                iv%instid(n)%rad(i)%ps,  &
                                iv%instid(n)%rad(i)%ts,  &
                                iv%instid(n)%rad(i)%smois,  &
                                iv%instid(n)%rad(i)%tslb,  &
                                iv%instid(n)%rad(i)%snowh, &
                                iv%instid(n)%rad(i)%isflg, &
                                nint(iv%instid(n)%rad(i)%soiltyp), &
                                nint(iv%instid(n)%rad(i)%vegtyp), &
                                iv%instid(n)%rad(i)%vegfra, &
                                iv%instid(n)%rad(i)%elevation, &
                                iv%instid(n)%rad(i)%clwp

            read(iv_rad_binary_unit)  ob%instid(n)%rad(i)%tb(:)
            read(iv_rad_binary_unit)  iv%instid(n)%rad(i)%tb(:)%inv
            read(iv_rad_binary_unit)  iv%instid(n)%rad(i)%emiss(1:iv%instid(n)%nchan)

            read(iv_rad_binary_unit)  iv%instid(n)%rad(i)%tb(:)%error
            read(iv_rad_binary_unit)  iv%instid(n)%rad(i)%tb(:)%qc

            if (write_profile) then
               read(iv_rad_binary_unit) num_levs, &
                  coefs(n) % ref_prfl_p(1:num_levs) , &
                  iv%instid(n)%rad(i)%t(1:num_levs) , &
                  iv%instid(n)%rad(i)%mr(1:num_levs)

               read(iv_rad_binary_unit) num_model_levs, &
                  iv%instid(n)%rad(i)%pm(xp%kts:xp%kte) , &
                  iv%instid(n)%rad(i)%tm(xp%kts:xp%kte) , &
                  iv%instid(n)%rad(i)%qm(xp%kts:xp%kte) , &
                  iv%instid(n)%rad(i)%qcw(xp%kts:xp%kte), &
                  iv%instid(n)%rad(i)%qrn(xp%kts:xp%kte)
            end if   ! end if write_profile
         end if    ! end if proc_domain
      end do     ! end do pixels
      close(iv_rad_binary_unit)
   end do    !! end do instruments

   if (trace_use) call da_trace_exit("da_read_iv_rad_binary")
 
END SUBROUTINE da_read_iv_rad_binary


