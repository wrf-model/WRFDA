subroutine da_write_oa_rad_ascii ( xp, ob, iv, re )

   !---------------------------------------------------------------------------
   ! Purpose: write out OMB and OMA vector structure for radiance data.
   !---------------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in)  :: xp       ! Domain decomposition vars.
   type (y_type),     intent(in)  :: ob       ! Observation structure.
   type (ob_type),    intent(in)  :: iv       ! O-B structure.
   type (y_type),     intent(in)  :: re       ! O-A structure.

#ifdef RTTOV
   integer                        :: n        ! Loop counter.
   integer                        :: i, k     ! Index dimension.
   integer                        :: nlevelss ! Number of obs levels.

   integer            :: ios, oma_rad_unit
   character(len=filename_len)  :: filename
   integer            :: ndomain

   if (trace_use) call da_trace_entry("da_write_oa_rad_ascii")

   do i = 1, iv%num_inst
      if (iv%instid(i)%num_rad < 1) cycle

      ! count number of obs within the loc%proc_domain
      !---------------------------------------------
      ndomain = 0
      do n =1,iv%instid(i)%num_rad
         if (iv%instid(i)%proc_domain(n)) then
            ndomain = ndomain + 1
         end if
      end do

#ifdef DM_PARALLEL
      write(unit=filename, fmt='(a,i2.2)') 'oma_'//trim(iv%instid(i)%rttovid_string)//'.', myproc
#else
      write(unit=filename, fmt='(a)') 'oma_'//trim(iv%instid(i)%rttovid_string)
#endif
      call da_get_unit(oma_rad_unit)
      open(unit=oma_rad_unit,file=trim(filename),form='formatted',iostat=ios)
      if (ios /= 0) Then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open oma radiance file"//filename/))
      endif
      write(unit=oma_rad_unit,fmt='(a,a,i7,a,i5,a)') trim(iv%instid(i)%rttovid_string), &
                           ' number-of-pixels : ', ndomain, &
                           ' channel-number-of-each-pixel : ', iv%instid(i)%nchan, &
                           ' index-of-channels : '
      write(unit=oma_rad_unit,fmt='(10i5)') iv%instid(i)%ichan
      write(unit=oma_rad_unit,fmt= *) ' pixel-info : i date scanpos landsea_mask  elv lat lon  satzen satazi '
      write(unit=oma_rad_unit,fmt= *) ' xb-surf-info : i t2m mr2m(ppmv) u10 v10 ps ts smois tslb snowh isflg &
                       & soiltyp vegtyp vegfra elev clwp'
      ndomain = 0
      do n=1,iv%instid(i)%num_rad
         if (iv%instid(i)%proc_domain(n)) then
            ndomain=ndomain+1
            write(unit=oma_rad_unit,fmt='(a,i7,2x,a,2i3,f6.0,4f8.2)') 'INFO : ', ndomain, &
                                   iv%instid(i)%info(n)%date_char, &
                                   iv%instid(i)%scanpos(n),   &
                                   iv%instid(i)%landsea_mask(n), &
                                   iv%instid(i)%info(n)%elv,  &
                                   iv%instid(i)%info(n)%lat,  &
                                   iv%instid(i)%info(n)%lon, &
                                   iv%instid(i)%satzen(n),    &
                                   iv%instid(i)%satazi(n)
             if (iv%instid(i)%isflg(n)==0) then
                write(unit=oma_rad_unit,fmt='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') ' SEA : ', n, &
                                   iv%instid(i)%t2m(n), &
                                   iv%instid(i)%mr2m(n),   &
                                   iv%instid(i)%u10(n), &
                                   iv%instid(i)%v10(n),  &
                                   iv%instid(i)%ps(n),  &
                                   iv%instid(i)%ts(n),  &
                                   iv%instid(i)%smois(n),  &
                                   iv%instid(i)%tslb(n),  &
                                   iv%instid(i)%snowh(n), &
                                   iv%instid(i)%isflg(n), &
                                   nint(iv%instid(i)%soiltyp(n)), &
                                   nint(iv%instid(i)%vegtyp(n)), &
                                   iv%instid(i)%vegfra(n), &
                                   iv%instid(i)%elevation(n), &
                                   iv%instid(i)%clwp(n)
            else if (iv%instid(i)%isflg(n)==1) then
               write(unit=oma_rad_unit,fmt='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') ' ICE : ', n, &
                                   iv%instid(i)%t2m(n), &
                                   iv%instid(i)%mr2m(n),   &
                                   iv%instid(i)%u10(n), &
                                   iv%instid(i)%v10(n),  &
                                   iv%instid(i)%ps(n),  &
                                   iv%instid(i)%ts(n),  &
                                   iv%instid(i)%smois(n),  &
                                   iv%instid(i)%tslb(n),  &
                                   iv%instid(i)%snowh(n), &
                                   iv%instid(i)%isflg(n), &
                                   nint(iv%instid(i)%soiltyp(n)), &
                                   nint(iv%instid(i)%vegtyp(n)), &
                                   iv%instid(i)%vegfra(n), &
                                   iv%instid(i)%elevation(n), &
                                   iv%instid(i)%clwp(n)
            else if (iv%instid(i)%isflg(n)==2) then
               write(unit=oma_rad_unit,fmt='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') 'LAND : ', n, &
                                   iv%instid(i)%t2m(n), &
                                   iv%instid(i)%mr2m(n),   &
                                   iv%instid(i)%u10(n), &
                                   iv%instid(i)%v10(n),  &
                                   iv%instid(i)%ps(n),  &
                                   iv%instid(i)%ts(n),  &
                                   iv%instid(i)%smois(n),  &
                                   iv%instid(i)%tslb(n),  &
                                   iv%instid(i)%snowh(n), &
                                   iv%instid(i)%isflg(n), &
                                   nint(iv%instid(i)%soiltyp(n)), &
                                   nint(iv%instid(i)%vegtyp(n)), &
                                   iv%instid(i)%vegfra(n), &
                                   iv%instid(i)%elevation(n), &
                                   iv%instid(i)%clwp(n)
            else if (iv%instid(i)%isflg(n)==3) then
               write(unit=oma_rad_unit,fmt='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') 'SNOW : ', n, &
                                   iv%instid(i)%t2m(n), &
                                   iv%instid(i)%mr2m(n),   &
                                   iv%instid(i)%u10(n), &
                                   iv%instid(i)%v10(n),  &
                                   iv%instid(i)%ps(n),  &
                                   iv%instid(i)%ts(n),  &
                                   iv%instid(i)%smois(n),  &
                                   iv%instid(i)%tslb(n),  &
                                   iv%instid(i)%snowh(n), &
                                   iv%instid(i)%isflg(n), &
                                   nint(iv%instid(i)%soiltyp(n)), &
                                   nint(iv%instid(i)%vegtyp(n)), &
                                   iv%instid(i)%vegfra(n), &
                                   iv%instid(i)%elevation(n), &
                                   iv%instid(i)%clwp(n)
            else if (iv%instid(i)%isflg(n)==4) then
               write(unit=oma_rad_unit,fmt='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') 'MSEA : ', n, &
                                   iv%instid(i)%t2m(n), &
                                   iv%instid(i)%mr2m(n),   &
                                   iv%instid(i)%u10(n), &
                                   iv%instid(i)%v10(n),  &
                                   iv%instid(i)%ps(n),  &
                                   iv%instid(i)%ts(n),  &
                                   iv%instid(i)%smois(n),  &
                                   iv%instid(i)%tslb(n),  &
                                   iv%instid(i)%snowh(n), &
                                   iv%instid(i)%isflg(n), &
                                   nint(iv%instid(i)%soiltyp(n)), &
                                   nint(iv%instid(i)%vegtyp(n)), &
                                   iv%instid(i)%vegfra(n), &
                                   iv%instid(i)%elevation(n), &
                                   iv%instid(i)%clwp(n)
            else if (iv%instid(i)%isflg(n)==5) then
               write(unit=oma_rad_unit,fmt='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') 'MICE : ', n, &
                                   iv%instid(i)%t2m(n), &
                                   iv%instid(i)%mr2m(n),   &
                                   iv%instid(i)%u10(n), &
                                   iv%instid(i)%v10(n),  &
                                   iv%instid(i)%ps(n),  &
                                   iv%instid(i)%ts(n),  &
                                   iv%instid(i)%smois(n),  &
                                   iv%instid(i)%tslb(n),  &
                                   iv%instid(i)%snowh(n), &
                                   iv%instid(i)%isflg(n), &
                                   nint(iv%instid(i)%soiltyp(n)), &
                                   nint(iv%instid(i)%vegtyp(n)), &
                                   iv%instid(i)%vegfra(n), &
                                   iv%instid(i)%elevation(n), &
                                   iv%instid(i)%clwp(n)
            else if (iv%instid(i)%isflg(n)==6) then
               write(unit=oma_rad_unit,fmt='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') 'MLND : ', n, &
                                   iv%instid(i)%t2m(n), &
                                   iv%instid(i)%mr2m(n),   &
                                   iv%instid(i)%u10(n), &
                                   iv%instid(i)%v10(n),  &
                                   iv%instid(i)%ps(n),  &
                                   iv%instid(i)%ts(n),  &
                                   iv%instid(i)%smois(n),  &
                                   iv%instid(i)%tslb(n),  &
                                   iv%instid(i)%snowh(n), &
                                   iv%instid(i)%isflg(n), &
                                   nint(iv%instid(i)%soiltyp(n)), &
                                   nint(iv%instid(i)%vegtyp(n)), &
                                   iv%instid(i)%vegfra(n), &
                                   iv%instid(i)%elevation(n), &
                                   iv%instid(i)%clwp(n)
            else if (iv%instid(i)%isflg(n)==7) then
               write(unit=oma_rad_unit,fmt='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') 'MSNO : ', n, &
                                   iv%instid(i)%t2m(n), &
                                   iv%instid(i)%mr2m(n),   &
                                   iv%instid(i)%u10(n), &
                                   iv%instid(i)%v10(n),  &
                                   iv%instid(i)%ps(n),  &
                                   iv%instid(i)%ts(n),  &
                                   iv%instid(i)%smois(n),  &
                                   iv%instid(i)%tslb(n),  &
                                   iv%instid(i)%snowh(n), &
                                   iv%instid(i)%isflg(n), &
                                   nint(iv%instid(i)%soiltyp(n)), &
                                   nint(iv%instid(i)%vegtyp(n)), &
                                   iv%instid(i)%vegfra(n), &
                                   iv%instid(i)%elevation(n), &
                                   iv%instid(i)%clwp(n)
            end if

            write(unit=oma_rad_unit,fmt='(a)') 'OBS  : '
            write(unit=oma_rad_unit,fmt='(10f11.2)') ob%instid(i)%tb(:,n)
            write(unit=oma_rad_unit,fmt='(a)') 'BAK  : '
            write(unit=oma_rad_unit,fmt='(10f11.2)') iv%instid(i)%tb_xb(:,n)
            write(unit=oma_rad_unit,fmt='(a)') 'IVBC : '
            write(unit=oma_rad_unit,fmt='(10f11.2)')  iv%instid(i)%tb_inv(:,n)
            write(unit=oma_rad_unit,fmt='(a)') 'OMA  : '
            write(unit=oma_rad_unit,fmt='(10f11.2)')  re%instid(i)%tb(:,n)
            write(unit=oma_rad_unit,fmt='(a)') 'EMS  : '
            write(unit=oma_rad_unit,fmt='(10f11.2)')  iv%instid(i)%emiss(1:iv%instid(i)%nchan,n)
            write(unit=oma_rad_unit,fmt='(a)') 'ERR  : '
            write(unit=oma_rad_unit,fmt='(10f11.2)') iv%instid(i)%tb_error(:,n)
            write(unit=oma_rad_unit,fmt='(a)') 'QC   : '
            write(unit=oma_rad_unit,fmt='(10i11)') iv%instid(i)%tb_qc(:,n)

            if (write_profile) then
               nlevelss  = iv%instid(i)%nlevels
               write(unit=oma_rad_unit,fmt=*) &
                  'RTM_level pres(mb) T(k) Q(ppmv) WRF_level pres(mb) T(k) q(g/kg) clw(g/kg) rain(g/kg)'
               do k=xp%kts,xp%kte
                  if (k <= nlevelss) then
                     write(unit=oma_rad_unit,fmt='(i3,f10.2,f8.2,e11.4,i3,f10.2,f8.2,3e11.4)') &
                        k, &                             ! RTTOV levels
                        coefs(i) % ref_prfl_p(k) , &
                        iv%instid(i)%t(k,n) , &
                        iv%instid(i)%mr(k,n), &
                        k,  &                     ! WRF model levels
                        iv%instid(i)%pm(k,n) , &
                        iv%instid(i)%tm(k,n) , &
                        iv%instid(i)%qm(k,n)*1000 , &    
                        iv%instid(i)%qcw(k,n)*1000.0, &
                        iv%instid(i)%qrn(k,n)*1000.0

                  else
                     write(unit=oma_rad_unit,fmt='(32x,i3,f10.2,f8.2,3e11.4)') k, &
                        iv%instid(i)%pm(k,n) , &
                        iv%instid(i)%tm(k,n) , &
                        iv%instid(i)%qm(k,n)*1000 , &
                        iv%instid(i)%qcw(k,n)*1000.0, &
                        iv%instid(i)%qrn(k,n)*1000.0
                     ! iv%instid(i)%qci(k,n)*1000.0, &
                     ! iv%instid(i)%qsn(k,n)*1000.0, &
                     ! iv%instid(i)%qgr(k,n)*10000.0
                  end if  
               end do   ! end loop profile
            end if   ! end if write_profile
         end if    ! end if proc_domain
      end do     ! end do pixels
      close(unit=oma_rad_unit)
      call da_free_unit(oma_rad_unit)
   end do    !! end do instruments

   if (trace_use) call da_trace_exit("da_write_oa_rad_ascii")
#endif

end subroutine da_write_oa_rad_ascii


