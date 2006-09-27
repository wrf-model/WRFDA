 subroutine da_read_iv_rad_binary ( xp, ob, iv )
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

   TYPE (xpose_type), INTENT(IN)     :: xp       ! Domain decomposition vars.
   TYPE (y_type),     INTENT(INOUT)  :: ob       ! Observation structure.
   TYPE (ob_type),    INTENT(INOUT)  :: iv       ! O-B structure.

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.

   INTEGER            :: unit, ios
   character(len=30)  :: filename
   integer            :: myproc, ierr, ndomain

#ifdef DM_PARALLEL
 call MPI_COMM_RANK( MPI_COMM_WORLD, myproc, ierr )
 print *, ' ierr= ', ierr
 print *, ' myproc = ', myproc
#endif

 do n = 1, iv%num_inst

  unit = 14
#ifdef DM_PARALLEL
  write(filename, '(a,i2.2)') trim(iv%instid(n)%rttovid_string)//'.inv_bin_', myproc
#else
  write(filename, '(a)') trim(iv%instid(n)%rttovid_string)//'.inv_bin'
#endif
  open(unit,file=trim(filename),form='unformatted',iostat=ios)
  if( ios /= 0 ) Then
      Write(*,*) 'can not open the file ', filename
      iv%instid(n)%num_rad = 0
      cycle
  Endif

    read(unit) iv%instid(n)%num_rad, iv%instid(n)%nchan,iv%instid(n)%ichan
  do i =1,iv%instid(n)%num_rad
    read(unit) ndomain, &
                             iv%instid(n)%rad(i)%info%date_char, &
                             iv%instid(n)%rad(i)%scanpos,   &
                             iv%instid(n)%rad(i)%landsea_mask, &
                             iv%instid(n)%rad(i)%info%elv,  &
                             iv%instid(n)%rad(i)%info%lat,  &
                             iv%instid(n)%rad(i)%info%lon, &
                             iv%instid(n)%rad(i)%satzen,    &
                             iv%instid(n)%rad(i)%satazi
    read(unit) &
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

    read(unit)  ob%instid(n)%rad(i)%tb(:)
    read(unit)  iv%instid(n)%rad(i)%tb(:)%inv
    read(unit)  iv%instid(n)%rad(i)%emiss(1:iv%instid(n)%nchan)

    read(unit)  iv%instid(n)%rad(i)%tb(:)%error
    read(unit)  iv%instid(n)%rad(i)%tb(:)%qc


    if (lwrite_profile) then
      read(unit) num_levs, &
            coefs(n) % ref_prfl_p(1:num_levs) , &
            iv%instid(n)%rad(i)%t(1:num_levs) , &
            iv%instid(n)%rad(i)%mr(1:num_levs)

      read(unit) num_model_levs, &
            iv%instid(n)%rad(i)%pm(xp%kts:xp%kte) , &
            iv%instid(n)%rad(i)%tm(xp%kts:xp%kte) , &
            iv%instid(n)%rad(i)%qm(xp%kts:xp%kte) , &
            iv%instid(n)%rad(i)%qcw(xp%kts:xp%kte), &
            iv%instid(n)%rad(i)%qrn(xp%kts:xp%kte)
    end if   ! end if write_profile

   end if    ! end if proc_domain
  end do     ! end do pixels
  close(unit)
 end do    !! end do instruments

 return
 
END SUBROUTINE da_read_iv_rad_binary

