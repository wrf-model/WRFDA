!
subroutine da_qc_amsua ( inst, nchan, ob, iv )
!------------------------------------------------------------------------------
!  PURPOSE: perform quality control for amsua data.
!
!  METHOD:  
!
!  HISTORY: 2005/12/14 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN)             :: inst       ! sensor index.
   INTEGER, INTENT(IN)             :: nchan      ! number of channel
   TYPE (y_type),  INTENT(IN)      :: ob         ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)   :: iv         ! O-B structure.


! local variables
   integer   :: n,scanpos,k,isflg,unit,ios
   logical   :: lmix
   real      :: satzen,si
   integer   :: ngood(nchan),nrej(nchan),nrej_omb_abs(nchan), &
                nrej_omb_std(nchan),nrej_omb_rms(nchan),      &
                nrej_mixsurface,nrej_windowchanl, nrej_si,    &
                nrej_clw,nrej_topo,num_rad

   character(len=30)  :: filename

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
!    num_rad        = iv%instid(inst)%num_rad

  if ( num_rad > 0 ) then

!    do n= 1, iv%instid(inst)%num_rad           ! loop for pixel
    do n= iv%ob_numb(iv%current_ob_time-1)%radiance(inst)+1, iv%ob_numb(iv%current_ob_time)%radiance(inst)

!  0.0  initialise QC by flags assuming good obs
!---------------------------------------------
      iv%instid(inst)%rad(n)%tb(:)%qc = 1

!  a.  reject all channels over mixture surface type
!------------------------------------------------------
      isflg = iv%instid(inst)%rad(n)%isflg 
      lmix  = (isflg==4) .or. (isflg==5) .or. (isflg==6) .or. (isflg==7)
      if (lmix) then
         iv%instid(inst)%rad(n)%tb(:)%qc  =  -1
         if(iv%instid(inst)%rad(n)%loc%proc_domain) nrej_mixsurface = nrej_mixsurface + 1
      end if

!  b.  reject channels 1~4 over land/sea-ice/snow
!------------------------------------------------------
      if ( isflg > 0 ) then 
          iv%instid(inst)%rad(n)%tb(1:4)%qc  = -1 
          if(iv%instid(inst)%rad(n)%loc%proc_domain) nrej_windowchanl = nrej_windowchanl + 1
      end if

!  c.  reject channels 13,14(above top model 10mb),15 
!------------------------------------------------------
        iv%instid(inst)%rad(n)%tb(13:15)%qc  = -1

!    reject limb obs 
!------------------------------------------------------
       scanpos = iv%instid(inst)%rad(n)%scanpos
       if ( scanpos <= 3 .or. scanpos >= 28 ) iv%instid(inst)%rad(n)%tb(:)%qc  =  -1

!       satzen  = rad%satzen
!       if ( abs(satzen) > 45. ) iv%instid(inst)%rad(n)%tb(:)%qc  =  -1

!  d. check precipitation 
!-----------------------------------------------------------
     if ( ob%instid(inst)%rad(n)%tb(1) > 0. .and. ob%instid(inst)%rad(n)%tb(15) > 0.) then
       si = ob%instid(inst)%rad(n)%tb(1) - ob%instid(inst)%rad(n)%tb(15)
       if ( si >= 3. ) then
         iv%instid(inst)%rad(n)%tb(:)%qc = -1 
         if(iv%instid(inst)%rad(n)%loc%proc_domain) nrej_si = nrej_si + 1
       end if
     end if

     if ( iv%instid(inst)%rad(n)%clwp >= 0.2 ) then
       iv%instid(inst)%rad(n)%tb(:)%qc = -1
       if(iv%instid(inst)%rad(n)%loc%proc_domain) nrej_clw = nrej_clw + 1
     end if

!   3.1 Estimate Cloud Liquid Water (CLW) in mm over sea
!       ( Grody etal. 2001, JGR, Equation 5b,7c,7d,9 )
!---------------------------------------------------------
! if (isflg == 0) then
!    coszen =  cos(iv%instid(inst)%rad(n)%satzen)
!    d0     =  8.24-(2.622-1.846*coszen)*coszen
!    d1     =  0.754
!    d2     =  -2.265
!    ts     =  iv%instid(inst)%rad(n)%ts
!    tb1    =  ob%instid(inst)%rad(n)%tb(1)
!    tb2    =  ob%instid(inst)%rad(n)%tb(2)
!    clw    =  coszen*(d0+d1*log(ts-tb1)+d2*log(ts-tb2))
!    clw    =  clw - 0.03
! end if


!  e. check surface height/pressure
!-----------------------------------------------------------
!      sfchgt = ivrad%rad(i)%info%elv
!      if ( sfchgt >= ) then
!
!      else 

!      end if

       if ( (isflg .ne. 0) .and. (iv%instid(inst)%rad(n)%ps < 850.) ) then
           iv%instid(inst)%rad(n)%tb(5)%qc  = -1
           if(iv%instid(inst)%rad(n)%loc%proc_domain) nrej_topo = nrej_topo + 1
       end if

!  g. check iuse
!-----------------------------------------------------------
       do k = 1, nchan
         if ( satinfo(inst)%iuse(k) .eq. -1 ) &
           iv%instid(inst)%rad(n)%tb(k)%qc  = -1
       end do

!  f. check innovation
!-----------------------------------------------------------
       do k = 1, nchan
        if ( abs(iv%instid(inst)%rad(n)%tb(k)%inv) > 15. ) then
            iv%instid(inst)%rad(n)%tb(k)%qc  = -1
            if(iv%instid(inst)%rad(n)%loc%proc_domain) nrej_omb_abs(k) = nrej_omb_abs(k) + 1
        end if

        if (lbiascorr) then
!          if ( abs(iv%instid(inst)%rad(n)%tb(k)%inv) > 3.*satinfo(inst)%std(scanpos+1,k) ) then
          if ( abs(iv%instid(inst)%rad(n)%tb(k)%inv) > 3.*satinfo(inst)%std(1,k) ) then
            iv%instid(inst)%rad(n)%tb(k)%qc  = -1
            if(iv%instid(inst)%rad(n)%loc%proc_domain) nrej_omb_std(k) = nrej_omb_std(k) + 1
          end if
        else
!          if ( abs(iv%instid(inst)%rad(n)%tb(k)%inv) > 3.*satinfo(inst)%rms(scanpos+1,k) ) then
          if ( abs(iv%instid(inst)%rad(n)%tb(k)%inv) > 3.*satinfo(inst)%rms(1,k) ) then
            iv%instid(inst)%rad(n)%tb(k)%qc  = -1
            if(iv%instid(inst)%rad(n)%loc%proc_domain) nrej_omb_rms(k) = nrej_omb_rms(k) + 1
          end if
        end if

        if (iv%instid(inst)%rad(n)%tb(k)%qc == -1) then
            iv%instid(inst)%rad(n)%tb(k)%error = 500.0
            if(iv%instid(inst)%rad(n)%loc%proc_domain) nrej(k) = nrej(k) + 1
        else
            if(iv%instid(inst)%rad(n)%loc%proc_domain) ngood(k) = ngood(k) + 1
          if (lbiascorr) then
!             iv%instid(inst)%rad(n)%tb(k)%error = satinfo(inst)%std(scanpos+1,k)
            if (use_error_factor_rad) then
               iv%instid(inst)%rad(n)%tb(k)%error = &
                   satinfo(inst)%std(1,k)*satinfo(inst)%error_factor(k)
            else
               iv%instid(inst)%rad(n)%tb(k)%error = satinfo(inst)%std(1,k)
            end if
          else
!             iv%instid(inst)%rad(n)%tb(k)%error = satinfo(inst)%rms(scanpos+1,k)
            if (use_error_factor_rad) then
               iv%instid(inst)%rad(n)%tb(k)%error =  &
                   satinfo(inst)%rms(1,k)*satinfo(inst)%error_factor(k)
            else
               iv%instid(inst)%rad(n)%tb(k)%error = satinfo(inst)%rms(1,k)
            end if
          end if
        end if
       end do
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
     unit = 22
     if (num_fgat_time > 1) then
       write(filename,'(a,i2.2)') trim(iv%instid(inst)%rttovid_string)//'.QCSTAT_',iv%current_ob_time
     else
       filename = trim(iv%instid(inst)%rttovid_string)//'.QCSTAT'
     endif
     open(unit,file=trim(filename),form='formatted',iostat=ios)
     if( ios /= 0 ) Then
        Write(UNIT=errmsg(1),FMT='(A,A)') 'error opening the output file ', filename
        call da_error(__FILE__,__LINE__,errmsg(1:1))
     Endif

     write(unit, fmt='(/a/)') ' Quality Control Statistics for '//iv%instid(inst)%rttovid_string
     write(unit,'(a20,i7)') ' num_rad          = ', num_rad
     write(unit,'(a20,i7)') ' nrej_mixsurface  = ', nrej_mixsurface
     write(unit,'(a20,i7)') ' nrej_windowchanl = ', nrej_windowchanl
     write(unit,'(a20,i7)') ' nrej_si          = ', nrej_si
     write(unit,'(a20,i7)') ' nrej_clw         = ', nrej_clw
     write(unit,'(a20,i7)') ' nrej_topo        = ', nrej_topo
     write(unit,'(a20)')    ' nrej_omb_abs(:)  = '
     write(unit,'(10i7)')     nrej_omb_abs(:)
     write(unit,'(a20)')    ' nrej_omb_std(:)  = '
     write(unit,'(10i7)')     nrej_omb_std(:)
     write(unit,'(a20)')    ' nrej_omb_rms(:)  = ' 
     write(unit,'(10i7)')     nrej_omb_rms(:)
     write(unit,'(a20)')    ' nrej(:)          = '
     write(unit,'(10i7)')     nrej(:)
     write(unit,'(a20)')    ' ngood(:)         = '
     write(unit,'(10i7)')     ngood(:)

     close(unit)
   End if

   return

end subroutine da_qc_amsua
