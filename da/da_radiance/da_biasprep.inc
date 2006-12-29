subroutine da_biasprep(inst,ob,iv)

   implicit none

   !-----------------------------------------------------------------------
   ! Output information files for bias correction progs
   !-----------------------------------------------------------------------

   integer       ,  intent(in)      :: inst
   type (y_type),  intent(in)       :: ob         ! o structure.
   type (ob_type),  intent(inout)   :: iv         ! o-b structure.

   ! Initialize constants and variables.

   integer  :: j,jx,npred,nchan,num_rad
   character(len=80)  :: filename
   character(len=1)   :: s1
   real        :: pred(6), q(43)
   type(bias)  :: radbias

   if (trace_use) call da_trace_entry("da_biasprep")

   num_rad         = iv%ob_numb(iv%current_ob_time)%radiance(inst)- &
                      iv%ob_numb(iv%current_ob_time-1)%radiance(inst)

   if (num_rad < 1) return

#ifdef DM_PARALLEL
   write(filename, '(a,i2.2)') 'biasprep_'//trim(iv%instid(inst)%rttovid_string)//'.', myproc
#else
    write(filename, '(a)') 'biasprep_'//trim(iv%instid(inst)%rttovid_string)//'.00'
#endif

   call da_get_unit(biasprep_unit)
   open (unit=biasprep_unit,FILE=filename,FORM='unformatted')

   npred = 4
   nchan = iv%instid(inst)%nchan 

   allocate (radbias%tb(nchan))
   allocate (radbias%omb(nchan))
   allocate (radbias%bias(nchan))
   allocate (radbias%qc_flag(nchan))
   allocate (radbias%cloud_flag(nchan))
   allocate (radbias%pred(npred))

   do j= iv%ob_numb(iv%current_ob_time-1)%radiance(inst)+1, &
      iv%ob_numb(iv%current_ob_time)%radiance(inst)
      if (iv%instid(inst)%loc(j)%proc_domain) then 
         q(1:43) = iv%instid(inst)%mr(1:43,j)/q2ppmv

         call da_predictor(pred(1:npred), npred, &
            iv%instid(inst)%t(1:43,j), q(1:43), iv%instid(inst)%ts(j))

         ! iv%instid(inst)%pred(1:npred,j)=pred(1:npred)

         ! transfer information to bias structure
         radbias%platform_id  = iv%instid(inst)%platform_id
         radbias%satellite_id = iv%instid(inst)%satellite_id
         radbias%sensor_id    = iv%instid(inst)%sensor_id

         read (iv%instid(inst)%info(j)%date_char,'(i4,5(a1,i2))') &
                                   radbias%year,s1, radbias%month,s1, radbias%day, &
                                   s1,radbias%hour, s1,radbias%min, s1,radbias%sec
         !if (j < 10 .and. print_detail_radiance) then
         !   write(UNIT=stdout,FMT=*) iv%instid(inst)%info(j)%date_char
         !   write(UNIT=stdout,FMT=*) radbias%year, radbias%month, radbias%day, &
         !      radbias%hour, radbias%min, radbias%sec
         !end if

         radbias%scanline     = iv%instid(inst)%scanline(j)    ! not available
         radbias%scanpos      = iv%instid(inst)%scanpos(j)
         radbias%landmask     = iv%instid(inst)%landsea_mask(j)
         radbias%elevation    = iv%instid(inst)%info(j)%elv
         radbias%lat          = iv%instid(inst)%info(j)%lat
         radbias%lon          = iv%instid(inst)%info(j)%lon
         radbias%surf_flag    = iv%instid(inst)%isflg(j)
         radbias%ps           = iv%instid(inst)%ps(j)
         radbias%t2m          = iv%instid(inst)%t2m(j)
         radbias%q2m          = iv%instid(inst)%mr2m(j)/q2ppmv
         radbias%tsk          = iv%instid(inst)%ts(j)
         radbias%clwp         = iv%instid(inst)%clwp(j)  ! in mm

         radbias%nchan        = nchan 
         radbias%tb(1:nchan)  = ob%instid(inst)%tb(1:nchan,j)
         radbias%omb(1:nchan) = iv%instid(inst)%tb_inv(1:nchan,j)
         radbias%bias(1:nchan) = 0.

         radbias%npred         = npred
         radbias%pred(1:npred) = pred(1:npred)

         radbias%qc_flag(1:nchan)= iv%instid(inst)%tb_qc(1:nchan,j)
         radbias%cloud_flag(1:nchan)= iv%instid(inst)%cloud_flag(1:nchan,j)

         ! set missing data and bad data to missing
         do jx=1,nchan   
            if (radbias%tb(jx) < 150.0 .or. radbias%tb(jx) > 400.0) then
               radbias%tb(jx)   = missing_r
               radbias%omb(jx)  = missing_r 
            end if
         end do

         ! write(unit=biasprep_unit) radbias ! can not compiled with pointer

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

   if (trace_use) call da_trace_exit("da_biasprep")

end subroutine da_biasprep


