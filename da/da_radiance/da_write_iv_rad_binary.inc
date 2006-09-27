 subroutine da_write_iv_rad_binary ( xp, ob, iv )
!------------------------------------------------------------------------------
!  PURPOSE: write out innovation vector structure for radiance data.
!
!  METHOD: write out for separated PEs 
!
!  HISTORY: 27/07/2005 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
#ifdef DM_PARALLEL
   INCLUDE 'mpif.h'
#endif

   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (y_type),     INTENT(IN)  :: ob       ! Observation structure.
   TYPE (ob_type),    INTENT(IN)  :: iv       ! O-B structure.

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.

   INTEGER            :: unit, ios
   character(len=30)  :: filename
   integer            :: myproc, ierr, ndomain

#ifdef DM_PARALLEL
 call MPI_COMM_RANK( MPI_COMM_WORLD, myproc, ierr )
#endif

 do n = 1, iv%num_inst
  if (iv%instid(n)%num_rad < 1) cycle

!! count number of obs within the loc%proc_domain
!!---------------------------------------------
  ndomain = 0
  do i =1,iv%instid(n)%num_rad
    if(iv%instid(n)%rad(i)%loc%proc_domain_with_halo) then
      ndomain = ndomain + 1
    end if
  end do
#ifdef DM_PARALLEL
  write(6,'(a,i3,a,i7)') 'number of obs in proc # ', myproc,' : ', ndomain
#else
  write(6,'(a,i7)') 'number of obs ', ndomain
#endif

  unit = 14
#ifdef DM_PARALLEL
  write(filename, '(a,i2.2)') trim(iv%instid(n)%rttovid_string)//'.inv_bin_', myproc
#else
  write(filename, '(a)') trim(iv%instid(n)%rttovid_string)//'.inv_bin'
#endif
  open(unit,file=trim(filename),form='unformatted',iostat=ios)
  if( ios /= 0 ) Then
      Write(*,*) 'error opening the output file ', filename
      Stop ' in da_write_iv_rad_binary '
  Endif
    write(unit) ndomain, iv%instid(n)%nchan,iv%instid(n)%ichan
   ndomain = 0
  do i =1,iv%instid(n)%num_rad
   if(iv%instid(n)%rad(i)%loc%proc_domain_with_halo) then
    ndomain=ndomain+1
    write(unit) ndomain, &
                             iv%instid(n)%rad(i)%info%date_char, &
                             iv%instid(n)%rad(i)%scanpos,   &
                             iv%instid(n)%rad(i)%landsea_mask, &
                             iv%instid(n)%rad(i)%info%elv,  &
                             iv%instid(n)%rad(i)%info%lat,  &
                             iv%instid(n)%rad(i)%info%lon, &
                             iv%instid(n)%rad(i)%satzen,    &
                             iv%instid(n)%rad(i)%satazi
    write(unit) &
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

    write(unit)  ob%instid(n)%rad(i)%tb(:)
    write(unit)  iv%instid(n)%rad(i)%tb(:)%inv
    write(unit)  iv%instid(n)%rad(i)%emiss(1:iv%instid(n)%nchan)

    write(unit)  iv%instid(n)%rad(i)%tb(:)%error
    write(unit)  iv%instid(n)%rad(i)%tb(:)%qc


  if (lwrite_profile) then
    num_levs  = iv%instid(n)%nlevels
!    write(unit,*)'RTM_level pres(mb) T(k) Q(ppmv) WRF_level pres(mb) T(k) q(g/kg) clw(g/kg) rain(g/kg)'
!    do k=xp%kts,xp%kte
!     if (k <= num_levs) then
      write(unit) num_levs, &
            coefs(n) % ref_prfl_p(1:num_levs) , &
            iv%instid(n)%rad(i)%t(1:num_levs) , &
            iv%instid(n)%rad(i)%mr(1:num_levs)

      write(unit) xp%kte, &
            iv%instid(n)%rad(i)%pm(xp%kts:xp%kte) , &
            iv%instid(n)%rad(i)%tm(xp%kts:xp%kte) , &
            iv%instid(n)%rad(i)%qm(xp%kts:xp%kte) , &
            iv%instid(n)%rad(i)%qcw(xp%kts:xp%kte), &
            iv%instid(n)%rad(i)%qrn(xp%kts:xp%kte)
!            iv%instid(n)%rad(i)%qci(xp%kts:xp%kte), &
!            iv%instid(n)%rad(i)%qsn(xp%kts:xp%kte), &
!            iv%instid(n)%rad(i)%qgr(xp%kts:xp%kte)
!     end if  
!    end do   ! end loop profile
    end if   ! end if write_profile

   end if    ! end if proc_domain
  end do     ! end do pixels
  close(unit)
 end do    !! end do instruments

 return
 
END SUBROUTINE da_write_iv_rad_binary

