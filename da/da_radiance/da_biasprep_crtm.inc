subroutine da_biasprep_crtm(inst,ob,iv)

   !-----------------------------------------------------------------------
   ! Purpose: Output information files for bias correction progs
   !-----------------------------------------------------------------------

   implicit none

   integer       ,  intent(in)      :: inst
   type (y_type),  intent(in)       :: ob         ! O structure.
   type (ob_type),  intent(inout)   :: iv         ! O-B structure.

   integer  :: n,jx,npred,nchan,num_rad,nlevels
   character(len=80)  :: filename
   character(len=1)   :: s1
   real               :: pred(6)
   type (bias_type)   :: radbias

   if (trace_use) call da_trace_entry("da_biasprep_crtm")

   num_rad         = iv%ob_numb(iv%current_ob_time)%radiance(inst)- &
                      iv%ob_numb(iv%current_ob_time-1)%radiance(inst)

   if (num_rad < 1) return

#ifdef DM_PARALLEL
   write(filename, '(a,i2.2)') 'biasprep_'//trim(iv%instid(inst)%rttovid_string)//'.', myproc
#else
   write(filename, '(a)') 'biasprep_'//trim(iv%instid(inst)%rttovid_string)//'.00'
#endif

   call da_get_unit(biasprep_unit)
   open(unit=biasprep_unit,FILE=filename,FORM='unformatted')

   !---------------------------------------------------------------------------
   npred = 4
   nchan = iv%instid(inst)%nchan 
   nlevels = iv%instid(inst)%nlevels-1

   allocate(radbias%tb(nchan))
   allocate(radbias%omb(nchan))
   allocate(radbias%bias(nchan))
   allocate(radbias%qc_flag(nchan))
   allocate(radbias%cloud_flag(nchan))
   allocate(radbias%pred(npred))

   do n= iv%ob_numb(iv%current_ob_time-1)%radiance(inst)+1, &
      iv%ob_numb(iv%current_ob_time)%radiance(inst)
      if (iv%instid(inst)%proc_domain(n)) then 
         !q(1:43) = iv%instid(inst)%mr(1:43,n)/q2ppmv ! to kg/kg

         if (n==10) then
          write(*,'(2f10.2)') iv%instid(inst)%ts(n),iv%instid(inst)%pf(0,n)
          do jx=1,nlevels
           write(*,'(i3,2f10.2,f8.2,f10.4)') jx, &
                            iv%instid(inst)%pm(jx,n),iv%instid(inst)%pf(jx,n), &
                            iv%instid(inst)%tm(jx,n),iv%instid(inst)%qm(jx,n)
          end do
         end if

         call da_predictor_crtm(pred(1:npred), npred, nlevels,iv%instid(inst)%tm(1:nlevels,n), &
            iv%instid(inst)%qm(1:nlevels,n), iv%instid(inst)%ts(n), &
            iv%instid(inst)%pm(1:nlevels,n),iv%instid(inst)%pf(0:nlevels,n))

         ! iv%instid(inst)%pred(1:npred,n)=pred(1:npred)

         ! transfer information to bias structure
         radbias%platform_id  = iv%instid(inst)%platform_id
         radbias%satellite_id = iv%instid(inst)%satellite_id
         radbias%sensor_id    = iv%instid(inst)%sensor_id

         read(iv%instid(inst)%info(n)%date_char,'(i4,5(a1,i2))') &
                                   radbias%year,s1, radbias%month,s1, radbias%day, &
                                   s1,radbias%hour, s1,radbias%min, s1,radbias%sec
         !if (n < 10 .and. print_detail_radiance) then
         !   write(unit=stdout,fmt=*) iv%instid(inst)%info(n)%date_char
         !   write(unit=stdout,fmt=*) radbias%year, radbias%month, radbias%day, &
         !      radbias%hour, radbias%min, radbias%sec
         !endif

         radbias%scanline     = iv%instid(inst)%scanline(n)    ! not available
         radbias%scanpos      = iv%instid(inst)%scanpos(n)
         radbias%landmask     = iv%instid(inst)%landsea_mask(n)
         radbias%elevation    = iv%instid(inst)%info(n)%elv
         radbias%lat          = iv%instid(inst)%info(n)%lat
         radbias%lon          = iv%instid(inst)%info(n)%lon
         radbias%surf_flag    = iv%instid(inst)%isflg(n)
         radbias%ps           = iv%instid(inst)%ps(n)
         radbias%t2m          = iv%instid(inst)%t2m(n)
         radbias%q2m          = iv%instid(inst)%mr2m(n)/q2ppmv
         radbias%tsk          = iv%instid(inst)%ts(n)
         radbias%clwp         = iv%instid(inst)%clwp(n)  ! in mm

         radbias%nchan        = nchan 
         radbias%tb(1:nchan)  = ob%instid(inst)%tb(1:nchan,n)
         radbias%omb(1:nchan) = iv%instid(inst)%tb_inv(1:nchan,n)
         radbias%bias(1:nchan) = 0.

         radbias%npred         = npred
         radbias%pred(1:npred) = pred(1:npred)

         radbias%qc_flag(1:nchan)= iv%instid(inst)%tb_qc(1:nchan,n)
         radbias%cloud_flag(1:nchan)= iv%instid(inst)%cloud_flag(1:nchan,n)

         ! set missing data and bad data to missing
         do jx=1,nchan   
            if (radbias%tb(jx) < 150. .or. radbias%tb(jx) > 400. ) then
               radbias%tb(jx)   = missing_r
               radbias%omb(jx)  = missing_r 
            end if
         end do

         !write(unit=biasprep_unit) radbias ! can not compiled with pointer

         call da_write_biasprep(radbias)

      end if
   end do
  
   close(unit=biasprep_unit)
   call da_free_unit(biasprep_unit)

   deallocate(radbias%tb)
   deallocate(radbias%omb)
   deallocate(radbias%bias)
   deallocate(radbias%qc_flag)
   deallocate(radbias%cloud_flag)
   deallocate(radbias%pred)

   if (trace_use) call da_trace_exit("da_biasprep_crtm")

end subroutine da_biasprep_crtm

