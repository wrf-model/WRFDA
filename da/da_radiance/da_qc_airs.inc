subroutine da_qc_airs (i, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for AQUA/EOS-2-AIRS data.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)             :: i          ! sensor index.
   integer, intent(in)             :: nchan      ! number of channel
   type (y_type),  intent(in)      :: ob         ! Observation structure.
   type (ob_type), intent(inout)   :: iv         ! O-B structure.


   ! local variables
   integer   :: n,scanpos,k,isflg,ios,fgat_rad_unit
   logical   :: lmix
   real      :: si
   ! real    :: satzen
   integer   :: ngood(nchan),nrej(nchan),nrej_omb_abs(nchan), &
                nrej_omb_std(nchan),nrej_omb_rms(nchan),      &
                nrej_mixsurface,nrej_windowchanl, nrej_si,    &
                nrej_clw,nrej_topo,num_rad, num_proc_domain,  &
                nrej_limb

   character(len=30)  :: filename

   if (trace_use) call da_trace_entry("da_qc_airs")

   ngood(:)        = 0
   nrej(:)         = 0
   nrej_omb_abs(:) = 0
   nrej_omb_std(:) = 0
   nrej_omb_rms(:) = 0
   nrej_mixsurface = 0
   nrej_windowchanl= 0
   nrej_si         = 0
   nrej_clw        = 0
   nrej_topo       = 0
   nrej_limb       = 0
   num_rad         = iv%ob_numb(iv%current_ob_time)%radiance(i)- &
                      iv%ob_numb(iv%current_ob_time-1)%radiance(i)
   ! num_rad        = iv%instid(i)%num_rad
   num_proc_domain = 0

   if (num_rad > 0) then

      ! do n= 1, iv%instid(i)%num_rad           ! loop for pixel
      do n= iv%ob_numb(iv%current_ob_time-1)%radiance(i)+1, iv%ob_numb(iv%current_ob_time)%radiance(i)

         if (iv%instid(i)%proc_domain(n)) &
               num_proc_domain = num_proc_domain + 1

         !  0.0  initialise QC by flags assuming good obs
         !---------------------------------------------
         iv%instid(i)%tb_qc(:,n) = qc_good

         !  a.  reject all channels over mixture surface type
         !------------------------------------------------------
         isflg = iv%instid(i)%isflg(n)
         lmix  = (isflg==4) .or. (isflg==5) .or. (isflg==6) .or. (isflg==7)
         if (lmix) then
            iv%instid(i)%tb_qc(:,n)  =  qc_bad
            if (iv%instid(i)%proc_domain(n)) &
               nrej_mixsurface = nrej_mixsurface + 1
         end if

         !  b.  reject all channels over land/sea-ice/snow
         !------------------------------------------------------
         if (isflg > 0) then 
            iv%instid(i)%tb_qc(:,n)  = qc_bad
            if (iv%instid(i)%proc_domain(n)) &
               nrej_windowchanl = nrej_windowchanl + 1
         end if

         !  c.  reject channels 13,14(above top model 10mb),15 
         !------------------------------------------------------
         !iv%instid(i)%tb_qc(13:15,n)  = qc_bad

         !    reject limb obs 
         !------------------------------------------------------
         scanpos = iv%instid(i)%scanpos(n)
         if (scanpos <= 3 .or. scanpos >= 88) then
            iv%instid(i)%tb_qc(:,n)  =  qc_bad
            if (iv%instid(i)%proc_domain(n)) &
                  nrej_limb = nrej_limb + 1
         end if

         !  d. cloud detection to be added
         !-----------------------------------------------------------
         if (iv%instid(i)%clwp(n) >= 0.2) then
            iv%instid(i)%tb_qc(:,n) = qc_bad
            iv%instid(i)%cloud_flag(:,n) = qc_bad
            if (iv%instid(i)%proc_domain(n)) &
               nrej_clw = nrej_clw + 1
         end if

         !  e. check surface height/pressure
         !-----------------------------------------------------------
         ! sfchgt = ivrad%info(n)%elv
         ! if (sfchgt >=) then
         ! else 
         ! end if

         !if ((isflg .ne. 0) .and. (iv%instid(i)%ps(n) < 850.)) then
         !   iv%instid(i)%tb_qc(5,n)  = qc_bad
         !   if (iv%instid(i)%proc_domain(n)) &
         !      nrej_topo = nrej_topo + 1
         !end if

         !  g. check iuse from information file (channel selection)
         !-----------------------------------------------------------
         do k = 1, nchan
            if (satinfo(i)%iuse(k) .eq. -1) &
               iv%instid(i)%tb_qc(k,n)  = qc_bad
         end do

         !  f. check innovation
         !-----------------------------------------------------------
         do k = 1, nchan

         !  1. check absolute value of innovation
         !------------------------------------------------
            if (abs(iv%instid(i)%tb_inv(k,n)) > 15.) then
               iv%instid(i)%tb_qc(k,n)  = qc_bad
               if (iv%instid(i)%proc_domain(n)) &
                  nrej_omb_abs(k) = nrej_omb_abs(k) + 1
            end if

         !  2. check relative value of innovation
         !      and assign of the observation error (standard deviation)
         !------------------------------------------------------------------------
            if (abs(iv%instid(i)%tb_inv(k,n)) > 3.0*satinfo(i)%error(k)) then
               iv%instid(i)%tb_qc(k,n)  = qc_bad
               if (iv%instid(i)%proc_domain(n)) &
                  nrej_omb_std(k) = nrej_omb_std(k) + 1
            end if

           ! 3. Final QC decision
           !---------------------------------------------
            if (iv%instid(i)%tb_qc(k,n) == qc_bad) then  ! bad obs
               iv%instid(i)%tb_error(k,n) = 500.0
               if (iv%instid(i)%proc_domain(n)) &
                  nrej(k) = nrej(k) + 1
            else                                         ! good obs
               if (iv%instid(i)%proc_domain(n)) &
                  ngood(k) = ngood(k) + 1
               if (use_error_factor_rad) then         ! if use error tuning factor
                  iv%instid(i)%tb_error(k,n) = &
                     satinfo(i)%error(k)*satinfo(i)%error_factor(k)
               else
                  iv%instid(i)%tb_error(k,n) = satinfo(i)%error(k)
               end if
            end if
         end do ! chan
      end do ! end loop pixel
   end if
 
   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (num_proc_domain)
   call da_proc_sum_int (nrej_mixsurface)
   call da_proc_sum_int (nrej_windowchanl)
   call da_proc_sum_int (nrej_si )
   call da_proc_sum_int (nrej_clw)
   call da_proc_sum_int (nrej_topo)
   call da_proc_sum_int (nrej_limb)
   call da_proc_sum_ints (nrej_omb_abs(:))
   call da_proc_sum_ints (nrej_omb_std(:))
   call da_proc_sum_ints (nrej_omb_rms(:))
   call da_proc_sum_ints (nrej(:))
   call da_proc_sum_ints (ngood(:))

   if (rootproc) then
      if (num_fgat_time > 1) then
         write(filename,'(a,i2.2)') trim(iv%instid(i)%rttovid_string)//'.qcstat_',iv%current_ob_time
      else
         filename = trim(iv%instid(i)%rttovid_string)//'.qcstat'
      end if

      call da_get_unit(fgat_rad_unit)
      open(fgat_rad_unit,file=trim(filename),form='formatted',iostat=ios)
      if (ios /= 0) Then
         write(unit=message(1),fmt='(A,A)') 'error opening the output file ', filename
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      write(fgat_rad_unit, fmt='(/a/)') ' Quality Control Statistics for '//iv%instid(i)%rttovid_string
      write(fgat_rad_unit,'(a20,i7)') ' num_proc_domain  = ', num_proc_domain
      write(fgat_rad_unit,'(a20,i7)') ' nrej_mixsurface  = ', nrej_mixsurface
      write(fgat_rad_unit,'(a20,i7)') ' nrej_windowchanl = ', nrej_windowchanl
      write(fgat_rad_unit,'(a20,i7)') ' nrej_si          = ', nrej_si
      write(fgat_rad_unit,'(a20,i7)') ' nrej_clw         = ', nrej_clw
      write(fgat_rad_unit,'(a20,i7)') ' nrej_topo        = ', nrej_topo
      write(fgat_rad_unit,'(a20,i7)') ' nrej_limb        = ', nrej_limb
      write(fgat_rad_unit,'(a20)')    ' nrej_omb_abs(:)  = '
      write(fgat_rad_unit,'(10i7)')     nrej_omb_abs(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_omb_std(:)  = '
      write(fgat_rad_unit,'(10i7)')     nrej_omb_std(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_omb_rms(:)  = ' 
      write(fgat_rad_unit,'(10i7)')     nrej_omb_rms(:)
      write(fgat_rad_unit,'(a20)')    ' nrej(:)          = '
      write(fgat_rad_unit,'(10i7)')     nrej(:)
      write(fgat_rad_unit,'(a20)')    ' ngood(:)         = '
      write(fgat_rad_unit,'(10i7)')     ngood(:)

      close(fgat_rad_unit)
      call da_free_unit(fgat_rad_unit)
   end if

   if (trace_use) call da_trace_exit("da_qc_airs")

end subroutine da_qc_airs

