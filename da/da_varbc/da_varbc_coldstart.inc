  subroutine da_varbc_coldstart (iv)

   !---------------------------------------------------------------------------
   !  PURPOSE: [1]: Calculate mode of histogram of (uncorrected) innovations 
   !                for each channel if a cold-start is needed
   !
   !           [2]: Compute bias predictors
   !
   !           [3]: Calculate statistics (mean/std) for cold-started predictors
   !
   !           [4]: Normalize predictors
   !
   !  Called from da_get_innov_vector_radiance
   !
   !  HISTORY: 10/26/2007 - Creation                     Tom Auligne
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent (inout)   :: iv             ! Innovation

   integer                          :: inst, k, n, i, n1, n2
   integer                          :: npredmax, npred, num_rad, num_rad_tot

   integer, parameter               :: nbins   = 200                       ! Number of Hist bins.
   real,    parameter               :: maxhist = 10.                       ! Maximum bin value.
   real,    parameter               :: zbin    = 2 * maxhist / real(nbins) ! Hist bin width.
   integer                          :: ibin                                ! Hist bin number.
   real                             :: modetmp(1), mode
   integer, allocatable             :: hist(:), hist_tot(:)
   
   logical, allocatable             :: lcs(:)
   real, allocatable                :: mean(:), rms(:), mean_tot(:), rms_tot(:)
   integer, allocatable             :: ipred(:)
   
   if ( iv%num_inst < 1 ) RETURN

   if (trace_use) call da_trace_entry("da_varbc_coldstart")
   

   do inst = 1, iv%num_inst                                               !! loop for sensors

      npredmax = iv%instid(inst)%varbc_info%npredmax
      num_rad  = iv%instid(inst)%num_rad
      n1       = iv%instid(inst)%info%n1
      n2       = iv%instid(inst)%info%n2
       
      allocate( lcs(npredmax), ipred(npredmax) )
      lcs(:) = .false.
    
    !---------------------------------------------------------------------------
    ! [1]: Calculate mode of histogram of (uncorrected) innovations 
    !---------------------------------------------------------------------------
      do k = 1, iv%instid(inst)%nchan                                  !! loop for channels
         npred          = iv%instid(inst)%varbc(k)%npred
	 ipred(1:npred) = iv%instid(inst)%varbc(k)%ipred(1:npred)
	 if (npred <= 0) cycle                                         !! VarBC channels only
	 if (ALL(iv%instid(inst)%varbc(k)%pred_use /= 0)) cycle        !! Coldstart channels only
	 
         write(unit=stdout,fmt='(A,A,I4)') 'VARBC: Cold-starting ', &
               trim(adjustl(iv%instid(inst)%rttovid_string)),k

         lcs(ipred(1:npred)) = (lcs(ipred(1:npred)) .or. &
	                       (iv%instid(inst)%varbc(k)%pred_use(ipred(1:npred)) == 0))
			       	
	 where (iv%instid(inst)%varbc(k)%pred_use(ipred(1:npred)) == 0) &
	        iv%instid(inst)%varbc(k)%param(1:npred) = 0.0

         if (iv%instid(inst)%varbc(k)%pred_use(1) == 0) then
	    Allocate ( hist(nbins),  hist_tot(nbins))
            hist(:) = 0  
            mode    = 0.0
	 
           ! Accumulate statistics for histogram
           ! -----------------------------------
            do n = n1,n2      !! loop for pixel      
               if (iv%instid(inst)%info%proc_domain(1,n)) then ! do not count HALO
                  ibin = NINT( (iv%instid(inst)%tb_inv(k,n)+maxhist)/zbin )
 	          if ((ibin>0).AND.(ibin<=nbins)) &
	             hist(ibin) = hist(ibin) + 1
               end if          
            end do             ! end loop for pixels
	       
           ! Do inter-processor communication to gather statistics
           ! ------------------------------------------------------
	    do ibin = 1, nbins
	       hist_tot(ibin) = wrf_dm_sum_integer(hist(ibin))
	    end do
		   
           ! Determine mode of Histogram
           !----------------------------
            if ( SUM(hist_tot(:)) > 0 ) then
	      modetmp(1:1) = MAXLOC(hist_tot(:))*zbin - maxhist
              mode = modetmp(1)
	    end if
	 
	   ! Use mode to initialize VarBC 
	   !-----------------------------
	    iv%instid(inst)%varbc(k)%param(1) = mode

	    Deallocate ( hist, hist_tot )
	 end if
      end do                                                              !  end loop for channels

    !---------------------------------------------------------------------------
    ! [2]: Get bias predictors:
    !---------------------------------------------------------------------------
      call da_varbc_pred(inst,iv)   !! bias predictors will be used for VarBC	              
      
    !---------------------------------------------------------------------------
    !  [3]: Calculate statistics for cold-started predictors 
    !---------------------------------------------------------------------------
      if (ANY(lcs)) then      !! coldstart instruments only
      
         write(unit=stdout,fmt='(A,A)') 'VARBC: Computing predictor statistics for ', &
 	         trim(adjustl(iv%instid(inst)%rttovid_string))
         allocate (mean(npredmax), rms(npredmax), mean_tot(npredmax), rms_tot(npredmax))

        ! Accumulate statistics for predictor mean/std
        ! ---------------------------------------------
         do i = 1, npredmax
            mean(i) = SUM(iv%instid(inst)%varbc_info%pred(i,n1:n2))    
            rms(i)  = SUM(iv%instid(inst)%varbc_info%pred(i,n1:n2)**2)
         end do
  
        ! Do inter-processor communication to gather statistics
        ! ------------------------------------------------------
         num_rad_tot = wrf_dm_sum_integer(num_rad)
	 call wrf_dm_sum_reals(mean, mean_tot)
	 call wrf_dm_sum_reals(rms,  rms_tot)
	 
         if (num_rad_tot >0) then
	    mean_tot = mean_tot / num_rad_tot
            rms_tot  = rms_tot  / num_rad_tot
         end if
	 
        ! Store statistics
        !------------------
	 where (lcs)
	    iv%instid(inst)%varbc_info%pred_mean = mean_tot
	    iv%instid(inst)%varbc_info%pred_std  = sqrt(rms_tot - mean_tot**2)
	 end where   
      
         deallocate(mean, rms, mean_tot, rms_tot)

      end if
      deallocate(lcs, ipred)  	           	     	

    !---------------------------------------------------------------------------
    !  [4]: Normalize predictors
    !---------------------------------------------------------------------------
      do i = 1,  npredmax
         if ( iv%instid(inst)%varbc_info%pred_std(i) <= 0 ) cycle
         do n = n1, n2      
            iv%instid(inst)%varbc_info%pred(i,n) = &
          ( iv%instid(inst)%varbc_info%pred(i,n) - &
            iv%instid(inst)%varbc_info%pred_mean(i) ) / &
            iv%instid(inst)%varbc_info%pred_std(i)
	 end do     
      end do
   end do                           !  end loop for sensor
   
   if (trace_use) call da_trace_exit("da_varbc_coldstart")

 end subroutine da_varbc_coldstart
