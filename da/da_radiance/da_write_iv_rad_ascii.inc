subroutine da_write_iv_rad_ascii ( xp, ob, iv )

   !---------------------------------------------------------------------------
   ! PURPOSE: write out innovation vector structure for radiance data.
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (y_type),     INTENT(IN)  :: ob       ! Observation structure.
   TYPE (ob_type),    INTENT(IN)  :: iv       ! O-B structure.

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.

   INTEGER            :: ios, innov_rad_unit
   character(len=30)  :: filename
   integer            :: ndomain

#ifdef RTTOV
   if (trace_use) call da_trace_entry("da_write_iv_rad_ascii")

   do n = 1, iv%num_inst
      if (iv%instid(n)%num_rad < 1) cycle

      ! count number of obs within the loc%proc_domain
      ! ---------------------------------------------
      ndomain = 0
      do i =1,iv%instid(n)%num_rad
         if (iv%instid(n)%rad(i)%loc%proc_domain) then
            ndomain = ndomain + 1
         end if
      end do
#ifdef DM_PARALLEL
      write(UNIT=stdout,FMT='(a,i3,a,i7)') 'number of obs in proc # ', myproc,' : ', ndomain
#else
      write(UNIT=stdout,FMT='(a,i7)') 'number of obs ', ndomain
#endif

#ifdef DM_PARALLEL
      write(UNIT=filename, FMT='(a,i2.2)') trim(iv%instid(n)%rttovid_string)//'.inv_', myproc
#else
      write(UNIT=filename, FMT='(a)') trim(iv%instid(n)%rttovid_string)//'.inv'
#endif

      call da_get_unit(innov_rad_unit)
      open(UNIT=innov_rad_unit,file=trim(filename),form='formatted',iostat=ios)
      if (ios /= 0 ) Then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open innovation radiance file"//filename/))
      Endif
      write(UNIT=innov_rad_unit,FMT='(a,a,i7,a,i5,a)') trim(iv%instid(n)%rttovid_string), &
                        ' number-of-pixels : ', ndomain, &
                        ' channel-number-of-each-pixel : ', iv%instid(n)%nchan, &
                        ' index-of-channels : '
      write(UNIT=innov_rad_unit,FMT='(10i5)') iv%instid(n)%ichan
      write(UNIT=innov_rad_unit,FMT=*) ' pixel-info : i date scanpos landsea_mask  elv lat lon  satzen satazi '
      write(UNIT=innov_rad_unit,FMT=*) ' xb-surf-info : i t2m mr2m(ppmv) u10 v10 ps ts smois tslb snowh isflg &
                    & soiltyp vegtyp vegfra elev clwp'
      ndomain = 0
      do i =1,iv%instid(n)%num_rad
         if (iv%instid(n)%rad(i)%loc%proc_domain) then
            ndomain=ndomain+1
            write(UNIT=innov_rad_unit,FMT='(a,i7,2x,a,2i3,f6.0,4f8.2)') 'INFO : ', ndomain, &
                                iv%instid(n)%rad(i)%info%date_char, &
                                iv%instid(n)%rad(i)%scanpos,   &
                                iv%instid(n)%rad(i)%landsea_mask, &
                                iv%instid(n)%rad(i)%info%elv,  &
                                iv%instid(n)%rad(i)%info%lat,  &
                                iv%instid(n)%rad(i)%info%lon, &
                                iv%instid(n)%rad(i)%satzen,    &
                                iv%instid(n)%rad(i)%satazi
            if (iv%instid(n)%rad(i)%isflg==0) then
               write(UNIT=innov_rad_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') ' SEA : ', i, &
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
            else if (iv%instid(n)%rad(i)%isflg==1) then
               write(UNIT=innov_rad_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') ' ICE : ', i, &
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
            else if (iv%instid(n)%rad(i)%isflg==2) then
               write(UNIT=innov_rad_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') 'LAND : ', i, &
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
            else if (iv%instid(n)%rad(i)%isflg==3) then
               write(UNIT=innov_rad_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') 'SNOW : ', i, &
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
            else if (iv%instid(n)%rad(i)%isflg==4) then
               write(UNIT=innov_rad_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') 'MSEA : ', i, &
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
            else if (iv%instid(n)%rad(i)%isflg==5) then
               write(UNIT=innov_rad_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') 'MICE : ', i, &
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
            else if (iv%instid(n)%rad(i)%isflg==6) then
               write(UNIT=innov_rad_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') 'MLND : ', i, &
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
            else if (iv%instid(n)%rad(i)%isflg==7) then
               write(UNIT=innov_rad_unit,FMT='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') 'MSNO : ', i, &
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
            end if

            write(UNIT=innov_rad_unit,FMT='(a)') 'OBS  : '
            write(UNIT=innov_rad_unit,FMT='(10f11.2)') ob%instid(n)%rad(i)%tb(:)
            write(UNIT=innov_rad_unit,FMT='(a)') 'BAK  : '
            write(UNIT=innov_rad_unit,FMT='(10f11.2)') iv%instid(n)%rad(i)%tb_xb(:)
            write(UNIT=innov_rad_unit,FMT='(a)') 'IVBC : '
            write(UNIT=innov_rad_unit,FMT='(10f11.2)')  iv%instid(n)%rad(i)%tb(:)%inv
            write(UNIT=innov_rad_unit,FMT='(a)') 'EMS  : '
            write(UNIT=innov_rad_unit,FMT='(10f11.2)')  iv%instid(n)%rad(i)%emiss(1:iv%instid(n)%nchan)
            write(UNIT=innov_rad_unit,FMT='(a)') 'ERR  : '
            write(UNIT=innov_rad_unit,FMT='(10f11.2)') iv%instid(n)%rad(i)%tb(:)%error
            write(UNIT=innov_rad_unit,FMT='(a)') 'QC   : '
            write(UNIT=innov_rad_unit,FMT='(10i11)') iv%instid(n)%rad(i)%tb(:)%qc


            if (write_profile) then
               num_levs  = iv%instid(n)%nlevels
               write(UNIT=innov_rad_unit,FMT=*) &
                  'RTM_level pres(mb) T(k) Q(ppmv) WRF_level pres(mb) T(k) q(g/kg) clw(g/kg) rain(g/kg)'
               do k=xp%kts,xp%kte
                  if (k <= num_levs) then
                     write(UNIT=innov_rad_unit,FMT='(i3,f10.2,f8.2,e11.4,i3,f10.2,f8.2,3e11.4)') &
                        k, &                             ! RTTOV levels
                        coefs(n) % ref_prfl_p(k) , &
                        iv%instid(n)%rad(i)%t(k) , &
                        iv%instid(n)%rad(i)%mr(k), &
                        k,  &                            ! WRF model levels
                        iv%instid(n)%rad(i)%pm(k) , &
                        iv%instid(n)%rad(i)%tm(k) , &
                        iv%instid(n)%rad(i)%qm(k)*1000 , &    
                        iv%instid(n)%rad(i)%qcw(k)*1000., &
                        iv%instid(n)%rad(i)%qrn(k)*1000.
                  else
                     write(UNIT=innov_rad_unit,FMT='(32x,i3,f10.2,f8.2,3e11.4)') k, &
                        iv%instid(n)%rad(i)%pm(k) , &
                        iv%instid(n)%rad(i)%tm(k) , &
                        iv%instid(n)%rad(i)%qm(k)*1000 , &
                        iv%instid(n)%rad(i)%qcw(k)*1000., &
                        iv%instid(n)%rad(i)%qrn(k)*1000.
                        ! iv%instid(n)%rad(i)%qci(k)*1000., &
                        ! iv%instid(n)%rad(i)%qsn(k)*1000., &
                        ! iv%instid(n)%rad(i)%qgr(k)*1000.
                  end if  
               end do ! end loop profile
            end if  ! end if write_profile
         end if ! end if proc_domain
      end do ! end do pixels
      close(UNIT=innov_rad_unit)
      call da_free_unit(innov_rad_unit)
   end do ! end do instruments

   if (trace_use) call da_trace_exit("da_write_iv_rad_ascii")

#endif

end subroutine da_write_iv_rad_ascii

