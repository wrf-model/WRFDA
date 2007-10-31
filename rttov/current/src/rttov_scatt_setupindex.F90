
    subroutine rttov_scatt_setupindex (nprofiles, n_chan, coef, nchannels, &
				     & lsprofiles,lsprofiles2, frequencies, nbtout)
                            
!**** Set indices for frequencies, lsprofiles for RTTOV-8 (SCATT)

!     P. Bauer, P. Lopez, E. Moreau, D. Salmond   ECMWF    May 2004

!     1. Set up indices for profiles/channels

!     RTTOV_SCATT_SETUPINDEX is called from ONEDVAR_OBSOP_RTTOV, ONEDVAR_OBSOP_RTTOV_GRAD

!     Modifications:
!

!* KIND     
    use parkind1     , only: jpim, jprb
    
    use rttov_const, only : npolar_return, npolar_compute, &
                          & inst_id_ssmi
    use rttov_types, only : rttov_coef   
        
    implicit none
    
    integer (kind=jpim), intent ( in) :: nprofiles
    integer (kind=jpim), intent ( in) :: nchannels
    integer (kind=jpim), intent ( in) :: nbtout
    integer (kind=jpim), intent ( in) :: n_chan (nprofiles)
    
    type   (rttov_coef), intent ( in) :: coef   
    
    integer (kind=jpim), intent (out), dimension (nchannels)    :: lsprofiles   
    integer (kind=jpim), intent (out), dimension (nbtout)       :: lsprofiles2  
    integer (kind=jpim), intent (out), dimension (nchannels)    :: frequencies

    integer (kind=jpim) :: i_prof, i_chan, j_chan, i_freq, i_polid, i_pol, nch

!- End of header ------------------------------------------------------

!* Set index arrays
    j_chan = 0
    
    do i_prof = 1, nprofiles    
       do i_chan = 1, n_chan (i_prof)
          i_polid = coef % fastem_polar (i_chan) + 1
	  
	  do i_pol = 1, npolar_compute (i_polid)
	     frequencies (j_chan + i_pol) = i_chan
	     lsprofiles  (j_chan + i_pol) = i_prof
	     	  
             if (coef % id_comp_lvl < 8 .and. coef % id_inst == inst_id_ssmi) then
 	         if (i_chan == 1 .or. i_chan == 2) frequencies (j_chan + i_pol) = 1
	         if (i_chan == 3                 ) frequencies (j_chan + i_pol) = 2
	         if (i_chan == 4 .or. i_chan == 5) frequencies (j_chan + i_pol) = 3
	         if (i_chan == 6 .or. i_chan == 7) frequencies (j_chan + i_pol) = 4         
             endif
 	  end do
	  j_chan = j_chan + npolar_compute (i_polid)
      end do
    end do
    
!* Set index arrays for output channels
    
    nch=0
    do i_prof = 1, nprofiles  
       do i_chan = 1, nbtout/nprofiles
    nch=nch+1  
 	     lsprofiles2  (nch) = i_prof
 	  end do
    end do

    end subroutine rttov_scatt_setupindex

