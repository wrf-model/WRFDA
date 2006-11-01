subroutine da_qc_amsub ( inst, nchan, ob, iv )

   !---------------------------------------------------------------------------
   ! PURPOSE: perform quality control for amsub data.
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN)             :: inst       ! sensor index.
   INTEGER, INTENT(IN)             :: nchan      ! number of channel
   TYPE (y_type),  INTENT(IN)      :: ob         ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)   :: iv         ! O-B structure.

   ! local variables
   integer   :: n,scanpos,k,isflg,ios
   logical   :: lmix
   real      :: satzen,si
   integer   :: ngood(nchan),nrej(nchan),nrej_omb_abs(nchan), &
                nrej_omb_std(nchan),nrej_omb_rms(nchan),      &
                nrej_mixsurface,nrej_windowchanl, nrej_si,    &
                nrej_clw,nrej_topo,num_rad

   character(len=30)  :: filename

   if (trace_use) call da_trace_entry("da_qc_amsub")

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
   num_rad         = iv%ob_numb(iv%current_ob_time)%radiance(inst)- &
                         iv%ob_numb(iv%current_ob_time-1)%radiance(inst)
   ! num_rad        = iv%instid(inst)%num_rad 

   if (num_rad > 0) then
      ! do n= 1, iv%instid(inst)%num_rad           ! loop for pixel
      do n= iv%ob_numb(iv%current_ob_time-1)%radiance(inst)+1, iv%ob_numb(iv%current_ob_time)%radiance(inst)


            ! 0.0  initialise QC flags by assuming good obs
            !---------------------------------------------
            iv%instid(inst)%rad(n)%tb(:)%qc = 1

            ! a.  reject all channels over mixture surface type
            !------------------------------------------------------
            isflg = iv%instid(inst)%rad(n)%isflg 
            lmix  = (isflg==4) .or. (isflg==5) .or. (isflg==6) .or. (isflg==7)
            if (lmix) then 
               iv%instid(inst)%rad(n)%tb(:)%qc  =  -1
               if (iv%instid(inst)%rad(n)%loc%proc_domain) &
                  nrej_mixsurface = nrej_mixsurface + 1
            end if

            !  b.  reject channels 1~2 over land/sea-ice/snow
            !------------------------------------------------------
            if (isflg > 0) then
               iv%instid(inst)%rad(n)%tb(1:2)%qc    = -1
               if (iv%instid(inst)%rad(n)%loc%proc_domain) &
                  nrej_windowchanl = nrej_windowchanl + 1
            end if

            ! reject limb obs 
            !------------------------------------------------------
            scanpos = iv%instid(inst)%rad(n)%scanpos
            ! if (scanpos <= 8 .or. scanpos >= 83) &
            !    iv%instid(inst)%rad(n)%tb(:)%qc  =  -1

            ! satzen  = rad%satzen
            ! if (abs(satzen) > 45.) &
            !    iv%instid(inst)%rad(n)%tb(:)%qc  =  -1

            !  d. check cloud/precipitation 
            !-----------------------------------------------------------
            if ( ob%instid(inst)%rad(n)%tb(1) > 0. .and. &
                 ob%instid(inst)%rad(n)%tb(2) > 0.) then
               si = ob%instid(inst)%rad(n)%tb(1) - &
                   ob%instid(inst)%rad(n)%tb(2)
               if (si >= 3.) then
                  iv%instid(inst)%rad(n)%tb(:)%qc = -1 
                  iv%instid(inst)%rad(n)%cloud_flag(:) = -1
                  if (iv%instid(inst)%rad(n)%loc%proc_domain) &
                     nrej_si = nrej_si + 1
               end if
            end if

            if (iv%instid(inst)%rad(n)%clwp >= 0.2) then
               iv%instid(inst)%rad(n)%tb(:)%qc = -1
               iv%instid(inst)%rad(n)%cloud_flag(:) = -1
               if (iv%instid(inst)%rad(n)%loc%proc_domain) &
                  nrej_clw = nrej_clw + 1
            end if

            !  e. check surface height/pressure
            !------------------------------------------------------
            ! sfchgt = ivrad%rad(i)%info%elv
            ! if ( sfchgt >= ) then
            !
            ! else 
            ! end if

            if ((isflg .ne. 0) .and. &
                (iv%instid(inst)%rad(n)%ps < 800.)) then
               iv%instid(inst)%rad(n)%tb(5)%qc  = -1
               if (iv%instid(inst)%rad(n)%loc%proc_domain) &
                  nrej_topo = nrej_topo + 1
            end if

            !  f. check innovation
            !-----------------------------------------------------------
            do k = 1, nchan
               if ( abs(iv%instid(inst)%rad(n)%tb(k)%inv) > 15. ) then
                  iv%instid(inst)%rad(n)%tb(k)%qc  = -1
                  if (iv%instid(inst)%rad(n)%loc%proc_domain) &
                     nrej_omb_abs(k) = nrej_omb_abs(k) + 1
               end if

               if (biascorr) then
                  ! if (abs(iv%instid(inst)%rad(n)%tb(k)%inv) > 3.*satinfo(inst)%std(scanpos+1,k)) then
                  if (abs(iv%instid(inst)%rad(n)%tb(k)%inv) > 3.*satinfo(inst)%std(1,k)) then
                     iv%instid(inst)%rad(n)%tb(k)%qc  = -1
                     if (iv%instid(inst)%rad(n)%loc%proc_domain) &
                        nrej_omb_std(k) = nrej_omb_std(k) + 1
                  end if
               else
                  ! if (abs(iv%instid(inst)%rad(n)%tb(k)%inv) > 3.*satinfo(inst)%rms(scanpos+1,k) ) then
                  if (abs(iv%instid(inst)%rad(n)%tb(k)%inv) > 3.*satinfo(inst)%rms(1,k)) then
                     iv%instid(inst)%rad(n)%tb(k)%qc  = -1
                     if (iv%instid(inst)%rad(n)%loc%proc_domain) &
                        nrej_omb_rms(k) = nrej_omb_rms(k) + 1
                  end if
               end if

               if (iv%instid(inst)%rad(n)%tb(k)%qc == -1) then
                  iv%instid(inst)%rad(n)%tb(k)%error = 500.0
                  if (iv%instid(inst)%rad(n)%loc%proc_domain) &
                     nrej(k) = nrej(k) + 1
               else
                  if (iv%instid(inst)%rad(n)%loc%proc_domain) &
                     ngood(k) = ngood(k) + 1
                  if (biascorr) then
                     ! iv%instid(inst)%rad(n)%tb(k)%error = satinfo(inst)%std(scanpos+1,k)
                     if (use_error_factor_rad) then
                        iv%instid(inst)%rad(n)%tb(k)%error =  &
                           satinfo(inst)%std(1,k)*satinfo(inst)%error_factor(k)
                     else
                        iv%instid(inst)%rad(n)%tb(k)%error = satinfo(inst)%std(1,k)
                     end if
                  else
                     ! iv%instid(inst)%rad(n)%tb(k)%error = satinfo(inst)%rms(scanpos+1,k)
                     if (use_error_factor_rad) then
                        iv%instid(inst)%rad(n)%tb(k)%error =  &
                           satinfo(inst)%rms(1,k)*satinfo(inst)%error_factor(k)
                     else
                        iv%instid(inst)%rad(n)%tb(k)%error = satinfo(inst)%rms(1,k)
                     end if
                  end if
               end if
            end do ! chan
      end do ! end loop pixel
   end if

   ! Do inter-processor communication to gather statistics.

   call da_proc_sum_int ( num_rad )
   call da_proc_sum_int ( nrej_mixsurface )
   call da_proc_sum_int ( nrej_windowchanl)
   call da_proc_sum_int ( nrej_si   )
   call da_proc_sum_int ( nrej_clw  )
   call da_proc_sum_int ( nrej_topo )
   do k = 1, nchan
      call da_proc_sum_int ( nrej_omb_abs(k) )
      call da_proc_sum_int ( nrej_omb_std(k) )
      call da_proc_sum_int ( nrej_omb_rms(k) )
      call da_proc_sum_int ( nrej(k)  )
      call da_proc_sum_int ( ngood(k) )
   end do

   IF ( rootproc ) THEN
      if (num_fgat_time > 1) then
        write(filename,'(a,i2.2)') trim(iv%instid(inst)%rttovid_string)//'.QCSTAT',iv%current_ob_time
      else
        filename = trim(iv%instid(inst)%rttovid_string)//'.QCSTAT'
      endif
      open(fgat_rad_unit,file=trim(filename),form='formatted',iostat=ios)
      if (ios /= 0) Then
         Write(unit=errmsg(1),FMT='(A,A)') 'error opening the output file ', filename
         call da_error(__FILE__,__LINE__,errmsg(1:1))
      Endif

      write(fgat_rad_unit, fmt='(/a/)') &
         ' Quality Control Statistics for '//iv%instid(inst)%rttovid_string
      write(fgat_rad_unit,'(a20,i7)') ' num_rad          = ', num_rad
      write(fgat_rad_unit,'(a20,i7)') ' nrej_mixsurface  = ', nrej_mixsurface
      write(fgat_rad_unit,'(a20,i7)') ' nrej_windowchanl = ', nrej_windowchanl
      write(fgat_rad_unit,'(a20,i7)') ' nrej_si          = ', nrej_si
      write(fgat_rad_unit,'(a20,i7)') ' nrej_clw         = ', nrej_clw
      write(fgat_rad_unit,'(a20,i7)') ' nrej_topo        = ', nrej_topo
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
   End if

   if (trace_use) call da_trace_exit("da_qc_amsub")

end subroutine da_qc_amsub
