subroutine da_biasprep(inst,ob,iv)

   implicit none

   !-----------------------------------------------------------------------
   ! Output information files for bias correction progs
   !-----------------------------------------------------------------------

   integer       ,  intent(in)      :: inst
   type (y_type),  intent(in)       :: ob         ! o structure.
   type (ob_type),  intent(inout)   :: iv         ! o-b structure.

   ! Initialize constants and variables.

   integer  :: j,jx,npred,nchan,myproc,ierr,num_rad
   character(len=80)  :: filename
   character(len=1)   :: s1
   real        :: pred(6), q(43)
   type(bias)  :: radbias

   if (trace_use) call da_trace_entry("da_biasprep")

   num_rad         = iv%ob_numb(iv%current_ob_time)%radiance(inst)- &
                      iv%ob_numb(iv%current_ob_time-1)%radiance(inst)

   if (num_rad < 1 ) return

#ifdef DM_PARALLEL
   call mpi_comm_rank( mpi_comm_world, myproc, ierr )
   write(filename, '(a,i2.2)') 'biasprep_'//trim(iv%instid(inst)%rttovid_string)//'.', myproc
#else
   write(filename, '(a)') 'biasprep_'//trim(iv%instid(inst)%rttovid_string)//'.00'
#endif

   call da_get_unit(biasprep_unit)
   open (unit=biasprep_unit,FILE=filename,FORM='unformatted')

   !---------------------------------------------------------------------------
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
      if (iv%instid(inst)%rad(j)%loc%proc_domain) then 
         q(1:43) = iv%instid(inst)%rad(j)%mr(1:43)/q2ppmv

         call da_predictor(pred(1:npred), npred, &
            iv%instid(inst)%rad(j)%t(1:43), q(1:43), iv%instid(inst)%rad(j)%ts)

         ! iv%instid(inst)%rad(j)%pred(1:npred)=pred(1:npred)

         ! transfer information to bias structure
         radbias%platform_id  = iv%instid(inst)%platform_id
         radbias%satellite_id = iv%instid(inst)%satellite_id
         radbias%sensor_id    = iv%instid(inst)%sensor_id

         read (iv%instid(inst)%rad(j)%info%date_char,'(i4,5(a1,i2))') &
                                   radbias%year,s1, radbias%month,s1, radbias%day, &
                                   s1,radbias%hour, s1,radbias%min, s1,radbias%sec
         !if (j < 10 .and. print_detail_radiance) then
         !   write(unit=stdout,fmt=*) iv%instid(inst)%rad(j)%info%date_char
         !   write(unit=stdout,fmt=*) radbias%year, radbias%month, radbias%day, &
         !      radbias%hour, radbias%min, radbias%sec
         !endif

         radbias%scanline     = iv%instid(inst)%rad(j)%scanline    ! not available
         radbias%scanpos      = iv%instid(inst)%rad(j)%scanpos
         radbias%landmask     = iv%instid(inst)%rad(j)%landsea_mask
         radbias%elevation    = iv%instid(inst)%rad(j)%info%elv
         radbias%lat          = iv%instid(inst)%rad(j)%info%lat
         radbias%lon          = iv%instid(inst)%rad(j)%info%lon
         radbias%surf_flag    = iv%instid(inst)%rad(j)%isflg
         radbias%ps           = iv%instid(inst)%rad(j)%ps
         radbias%t2m          = iv%instid(inst)%rad(j)%t2m
         radbias%q2m          = iv%instid(inst)%rad(j)%mr2m/q2ppmv
         radbias%tsk          = iv%instid(inst)%rad(j)%ts
         radbias%clwp         = iv%instid(inst)%rad(j)%clwp  ! in mm

         radbias%nchan        = nchan 
         radbias%tb(1:nchan)  = ob%instid(inst)%rad(j)%tb(1:nchan)
         radbias%omb(1:nchan) = iv%instid(inst)%rad(j)%tb(1:nchan)%inv
         radbias%bias(1:nchan) = 0.

         radbias%npred         = npred
         radbias%pred(1:npred) = pred(1:npred)

         radbias%qc_flag(1:nchan)= iv%instid(inst)%rad(j)%tb(1:nchan)%qc
         radbias%cloud_flag(1:nchan)= iv%instid(inst)%rad(j)%cloud_flag(1:nchan)

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


