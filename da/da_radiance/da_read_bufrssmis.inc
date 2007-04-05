subroutine da_read_bufrssmis (obstype,iv,xp,infile)

   !---------------------------------------------------------------------------
   !  Purpose: read in NCEP bufr SSM/IS data to innovation structure
   !
   !   METHOD: use F90 sequential data structure to avoid reading file twice  
   !            1. read file radiance data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !               and deallocate sequential data structure
   !---------------------------------------------------------------------------

   use da_control

   implicit none

   character(5) ,     intent (in)    :: obstype    ! ssmis
   character(20),     intent (in)    :: infile     ! ssmis.bufr
   type (xpose_type), intent (in)    :: xp
   type (ob_type),    intent (inout) :: iv

#ifdef BUFR

   integer(i_kind), parameter :: bufsat_dmsp16 = 249  ! DMSP16 BUFR identifier
   integer(i_kind), parameter :: bufsat_dmsp17 = 285  ! DMSP17 BUFR identifier
   integer(i_kind), parameter :: nsubinst = 4   ! number of ssmis sub-instruments
   integer(i_kind), parameter :: n1bhdr = 14
   integer(i_kind), parameter :: maxchanl = 24
   integer(i_kind), parameter :: mxscen_img = 180
   integer(i_kind), parameter :: mxscen_env = 90
   integer(i_kind), parameter :: mxscen_las = 60
   integer(i_kind), parameter :: mxscen_uas = 30
   integer(i_kind), parameter :: ssmis_img = 1    ! sub-instrument index
   integer(i_kind), parameter :: ssmis_env = 2    ! sub-instrument index
   integer(i_kind), parameter :: ssmis_las = 3    ! sub-instrument index
   integer(i_kind), parameter :: ssmis_uas = 4    ! sub-instrument index
   real(r_kind),    parameter :: tbmin = 70.0_r_kind
   real(r_kind),    parameter :: tbmax = 320.0_r_kind

   character(3), dimension(nsubinst) :: subinst = (/ 'img', 'env', 'las', 'uas' /)
   character(80) :: hdr1b
   data hdr1b /'SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON SLNM RSURF RAINF ORBN'/
   character(10) :: date
   character(8)  :: subset, subfgn

   logical :: outside, outside_all

   integer(i_kind) :: iost, inst, lnbufr, ifgat, ifovoff
   integer(i_kind) :: ihh, i, n, k, slnm, ifov, idd, ireadmg, ireadsb
   integer(i_kind) :: iret, idate, im, iy, nchan
   integer(i_kind) :: nscan, jc, incangl, jch, bch
   integer(i_kind) :: platform_id, satellite_id, sensor_id
   integer(i_kind) :: num_ssmis_local, num_ssmis_global, num_ssmis_used

   integer(i_kind), dimension(maxchanl)  :: indxchan   ! channel index
   real(r_double), dimension(2,maxchanl) :: bufrtbb
   real(r_double), dimension(n1bhdr)     :: bfr1bhdr

   ! pixel information
   integer(i_kind)   ::  year,month,day,hour,minute,second  ! observation time
   real(r_double)    ::  obs_time
   real(r_double), allocatable ::  tb_inv(:)          !  bright temperatures

   type (datalink_type), pointer  :: head, p, current
   type(info_type)                :: info
   type(model_loc_type)           :: loc

   integer(i_kind) :: iread   !hcl temporary

   call da_trace_entry("da_read_bufrssmis")

   ! Initialize variables

   indxchan(:) = 0

   ! Set various variables depending on type of data to be read

   !call init_constants_derived

   platform_id  = 2                 ! for DMSP series
   sensor_id    = 10                ! for SSMIS

   ! 0.0  Open unit to satellite bufr file and read file header
   !--------------------------------------------------------------

   call da_get_unit(lnbufr)
   open(unit=lnbufr,file=trim(infile),form='unformatted', &
      iostat = iost, status = 'old')
   if (iost /= 0) then
      call da_warning(__FILE__,__LINE__, &
         (/"Cannot open file "//trim(infile)/))
      return
   end if

   call openbf(lnbufr,'IN',lnbufr)
   call datelen(10)
   call readmg(lnbufr,subset,idate,iret)

   iy=0
   im=0
   idd=0
   ihh=0
   write(unit=date,fmt='( i10)') idate
   read(unit=date,fmt='(i4,3i2)') iy,im,idd,ihh
   write(unit=stdout,fmt=*) &
      'Bufr file date is ',iy,im,idd,ihh,infile

   ! Loop to read bufr file and assign information to a sequential structure
   !-------------------------------------------------------------------------

   allocate (head)
   nullify  ( head % next )
   p => head

   num_ssmis_local  = 0
   num_ssmis_global = 0
   num_ssmis_used = 0

   allocate (tb_inv(maxchanl))

   instrument_loop: do k = 1, nsubinst

     iread = 0  !hcl for testing

! Set various variables depending on type of data to be read

      select case ( k )
! Humidity imager:180
         case ( ssmis_img )
            nscan  = mxscen_img
            ifovoff = 0
            nchan = 6
            indxchan(1:nchan)=(/8,9,10,11,17,18/)
            subfgn = 'NC021201'
            incangl = 53.0_r_kind
! env:90
         case ( ssmis_env )
            nscan  = mxscen_env
            ifovoff = 180
            nchan = 5
            indxchan(1:nchan)=(/12,13,14,15,16/)
            subfgn = 'NC021202'
            incangl = 53.1_r_kind
! las:60
         case ( ssmis_las )
            nscan  = mxscen_las
            ifovoff = 270
            nchan = 8
            indxchan(1:nchan)=(/1,2,3,4,5,6,7,24/)
            subfgn = 'NC021203'
            incangl = 53.0_r_kind
! uas:30
         case ( ssmis_uas )
            nscan  = mxscen_uas
            ifovoff = 330
            nchan = 5
            indxchan(1:nchan)=(/19,20,21,22,23/)
            subfgn = 'NC021204'
            incangl = 52.4_r_kind
      end select

      subset_loop: do while (ireadmg(lnbufr,subset,idate)==0)

         read_loop: do while (ireadsb(lnbufr)==0 .and. subset==subfgn)

            iread = iread + 1   !hcl for testing
            ! 1.0     Read header record and data record

            call ufbint(lnbufr,bfr1bhdr,n1bhdr,1,iret,hdr1b)
            call ufbrep(lnbufr,bufrtbb,2,maxchanl,iret,"CHNM TMBR" )

            ! check if observation outside range

            ! 2.0     Extract observation location and other required information
            !     QC1:  judge if data is in the domain, read next record if not
            !------------------------------------------------------------------------

            info%lat  =  bfr1bhdr(bufr_lat)
            info%lon  =  bfr1bhdr(bufr_lon)
            call da_ll_to_xy (info, loc, xp, outside, outside_all)

            if (outside_all) cycle

            !  3.0     Extract other information
            !------------------------------------------------------
            !  3.1     Extract satellite id and scan position. 
   
            if (nint(bfr1bhdr(bufr_satellite_id)) == bufsat_dmsp16) then
               satellite_id = 16
            else if (nint(bfr1bhdr(bufr_satellite_id)) == bufsat_dmsp17) then
               satellite_id = 17
            end if

            ! 3.3 Find wrfvar instrument index from RTTOV instrument triplet
            !     go to next data if id is not in the lists

            inst = 0
            do i = 1, rtminit_nsensor
               if (platform_id  == rtminit_platform(i)      &
                  .and. satellite_id == rtminit_satid(i)    &
                  .and. sensor_id    == rtminit_sensor(i)) then
                  inst = i
                  exit
               end if
            end do
            if (inst == 0) cycle read_loop

            !  3.1     Extract scan number and scan position. 

            slnm = nint(bfr1bhdr(11))

            ifov = nint(bfr1bhdr(bufr_ifov)) - ifovoff
            if (ifov > nscan) then
               write(unit=message(1),fmt=*)  &
                    'READ_SSMIS(',subinst(k),'): unreliable FOV number =',  &
                    bfr1bhdr(bufr_ifov), ' ifov=',ifov
               call da_warning(__FILE__,__LINE__,message(1:1))
               cycle read_loop
            end if

            !  3.2     Extract date information.
    
            year   = bfr1bhdr(bufr_year)   
            month  = bfr1bhdr(bufr_month)  
            day    = bfr1bhdr(bufr_day)    
            hour   = bfr1bhdr(bufr_hour)   
            minute = bfr1bhdr(bufr_minute) 
            second = bfr1bhdr(bufr_second) 

            write(unit=info%date_char, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
               year, '-', month, '-', day, '_', hour, ':', minute, ':', second

            !  QC3: time consistency check with the background date

            if (year <= 99) then
               if (year < 78) then
                  year = year + 2000
               else
                  year = year + 1900
               end if
            end if

            call da_get_julian_time(year,month,day,hour,minute,obs_time)

            if (obs_time < time_slots(0) .or.  &
               obs_time >= time_slots(num_fgat_time)) cycle read_loop

            ! 3.2.1 determine FGAT index ifgat
   
            do ifgat=1,num_fgat_time
               if (obs_time >= time_slots(ifgat-1) .and.  &
                   obs_time  < time_slots(ifgat)) exit
            end do

            num_ssmis_global = num_ssmis_global + 1

            num_ssmis_used = num_ssmis_used + 1

            if (num_ssmis_used > max_ssmis_input) then
               write(unit=message(1),fmt='(A,I10,A)') &
                  "Max number of ssmis",max_ssmis_input," reached"
               call da_warning(__FILE__,__LINE__,message(1:1))
               num_ssmis_used = num_ssmis_used - 1
               exit read_loop
            end if
            if (outside) cycle ! No good for this PE

            num_ssmis_local = num_ssmis_local + 1

            ! 3.4 extract satellite and solar angle
   
            ! 3.5 extract surface information

            ! 3.6 extract channel bright temperature
   
            tb_inv(1:maxchanl) = missing_r
            do jc = 1, nchan
               jch = indxchan(jc)             !ch index specified in this code
               bch = nint(bufrtbb(1,jch))     !ch index from bufr
               tb_inv(jch) = bufrtbb(2,jch)
               if (tb_inv(jch) < tbmin .or. tb_inv(jch) > tbmax .or. bch /= jch) then
                   tb_inv(jch) = missing_r
               end if
            end do

            if ( maxval(tb_inv(:)) > missing_r ) then

               !  4.0   assign information to sequential radiance structure
               !--------------------------------------------------------------------------
               allocate (p % tb_inv (1:maxchanl))
               p%info                = info
               p%loc                 = loc
               p%ssmis_subinst       = k
!               p%landsea_mask        = landsea_mask
               p%scanline            = slnm
               p%scanpos             = ifov
               p%satzen              = incangl
!               p%satazi              = satazi
!               p%solzen              = solzen
               p%tb_inv(1:maxchanl)  = tb_inv(1:maxchanl)
               p%sensor_index        = inst
               p%ifgat               = ifgat

               allocate (p%next)   ! add next data
               p => p%next
               nullify (p%next)

            else

               num_ssmis_local = num_ssmis_local - 1

            end if

         end do read_loop

      end do subset_loop

      iv%total_obs         = iv%total_obs + num_ssmis_used
      iv%total_rad_pixel   = iv%total_rad_pixel + num_ssmis_used
      iv%total_rad_channel = iv%total_rad_channel + num_ssmis_used*maxchanl

      rewind(lnbufr)
      write(stdout,*) 'ssmis: ', subfgn, iread, num_ssmis_local    !hcl for testing

   end do instrument_loop

   deallocate(tb_inv)  

   !  5.0 allocate innovation radiance structure
   !----------------------------------------------------------------  
   
   if ( num_ssmis_local > 0 ) then
      iv%instid(inst)%num_rad = num_ssmis_local
      write(UNIT=stdout,FMT='(a,i3,2x,a,3x,i10)') &
        'Allocating space for radiance innov structure', &
         inst, iv%instid(inst)%rttovid_string, iv%instid(inst)%num_rad
      if (rtm_option == rtm_option_crtm) then
         iv%instid(inst)%nlevels = xp%kme-xp%kms+1
         iv%instid(inst)%nchannels = maxchanl
      end if

#ifdef RTTOV
      if (rtm_option == rtm_option_rttov) then
         call rttov_setupchan(1, maxchanl, coefs(inst),             &   ! in
            iv%instid(inst)%nfrequencies,iv%instid(inst)%nchannels, &
            iv%instid(inst)%nbtout)                                     ! out
      end if
#endif

      allocate (iv%instid(inst)%ssmis_subinst(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%info(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%loc(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%loc_i(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%loc_j(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%loc_k(iv%instid(inst)%nlevels,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%loc_dz(iv%instid(inst)%nlevels,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%loc_dzm(iv%instid(inst)%nlevels,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%zk(iv%instid(inst)%nlevels,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%t(iv%instid(inst)%nlevels,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%mr(iv%instid(inst)%nlevels,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%loc_dx(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%loc_dy(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%loc_dxm(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%loc_dym(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%tm(xp%kms:xp%kme,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%qm(xp%kms:xp%kme,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%qrn(xp%kms:xp%kme,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%qcw(xp%kms:xp%kme,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%qci(xp%kms:xp%kme,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%qsn(xp%kms:xp%kme,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%qgr(xp%kms:xp%kme,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%pm(xp%kms:xp%kme,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%pf(0:xp%kme,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%u10(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%v10(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%t2m(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%q2m(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%mr2m(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%psfc(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%ts(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%smois(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%tslb(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%snowh(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%isflg(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%landsea_mask(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%elevation(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%vegfra(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%vegtyp(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%soiltyp(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%clwp(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%ps(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%tb_xb(maxchanl,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%tb_qc(maxchanl,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%tb_inv(maxchanl,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%tb_error(maxchanl,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%emiss(maxchanl,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%scanpos(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%scanline(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%ifgat(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%cloud_flag(maxchanl,iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%satzen(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%satazi(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%solzen(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%solazi(iv%instid(inst)%num_rad))
      allocate (iv%instid(inst)%proc_domain(iv%instid(inst)%num_rad))
      if (rtm_option == rtm_option_crtm) then
         allocate (iv%instid(inst)%water_coverage(iv%instid(inst)%num_rad))
         allocate (iv%instid(inst)%land_coverage(iv%instid(inst)%num_rad))
         allocate (iv%instid(inst)%ice_coverage(iv%instid(inst)%num_rad))
         allocate (iv%instid(inst)%snow_coverage(iv%instid(inst)%num_rad))
         allocate (iv%instid(inst)%ps_jacobian(maxchanl,iv%instid(inst)%num_rad))
         allocate (iv%instid(inst)%t_jacobian(maxchanl,xp%kte,iv%instid(inst)%num_rad))
         allocate (iv%instid(inst)%q_jacobian(maxchanl,xp%kte,iv%instid(inst)%num_rad))
      end if
   
   end if   ! end if num_ssmis_local

   !  6.0 assign sequential structure to innovation structure
   !-------------------------------------------------------------
   p => head

   do n = 1, num_ssmis_local
      iv%instid(inst)%ssmis_subinst(n)= p%ssmis_subinst
      iv%instid(inst)%info(n)         = p%info
      iv%instid(inst)%loc(n)          = p%loc
      iv%instid(inst)%loc_i(n)        = p%loc%i
      iv%instid(inst)%loc_j(n)        = p%loc%j
      iv%instid(inst)%loc_k(:,n)      = 0
      iv%instid(inst)%loc_dx(n)       = p%loc%dx
      iv%instid(inst)%loc_dy(n)       = p%loc%dy
      iv%instid(inst)%loc_dz(:,n)     = 0.0
      iv%instid(inst)%loc_dxm(n)      = p%loc%dxm
      iv%instid(inst)%loc_dym(n)      = p%loc%dym
      iv%instid(inst)%loc_dzm(:,n)    = 0.0
      ! z done in da_get_innov_vector_rad
      iv%instid(inst)%t(:,n)          = 0.0
      iv%instid(inst)%mr(:,n)         = 0.0
      iv%instid(inst)%tm(:,n)         = 0.0
      iv%instid(inst)%qm(:,n)         = 0.0
      iv%instid(inst)%qrn(:,n)        = 0.0
      iv%instid(inst)%qcw(:,n)        = 0.0
      iv%instid(inst)%qci(:,n)        = 0.0
      iv%instid(inst)%qsn(:,n)        = 0.0
      iv%instid(inst)%qgr(:,n)        = 0.0
      iv%instid(inst)%pm(:,n)         = 0.0
      iv%instid(inst)%u10(n)          = 0.0
      iv%instid(inst)%v10(n)          = 0.0
      iv%instid(inst)%t2m(n)          = 0.0
      iv%instid(inst)%q2m(n)          = 0.0
      iv%instid(inst)%mr2m(n)         = 0.0
      iv%instid(inst)%psfc(n)         = 0.0
      iv%instid(inst)%ts(n)           = 0.0
      iv%instid(inst)%smois(n)        = 0.0
      iv%instid(inst)%tslb(n)         = 0.0
      iv%instid(inst)%snowh(n)        = 0.0
      iv%instid(inst)%isflg(n)        = 0
      iv%instid(inst)%soiltyp(n)      = 0.0
      iv%instid(inst)%landsea_mask(n) = 0.0
      iv%instid(inst)%elevation(n)    = 0.0
      iv%instid(inst)%vegfra(n)       = 0.0
      iv%instid(inst)%vegtyp(n)       = 0.0
      iv%instid(inst)%clwp(n)         = 0.0
      iv%instid(inst)%ps(n)           = 0.0
      iv%instid(inst)%tb_xb(:,n)      = 0
      iv%instid(inst)%tb_inv(:,n)     = p%tb_inv(:) 
      iv%instid(inst)%tb_qc(:,n)      = 0
      iv%instid(inst)%tb_error(:,n)   = 500.
      iv%instid(inst)%emiss(:,n)      = 0.0
      iv%instid(inst)%scanpos(n)      = p%scanpos 
      iv%instid(inst)%scanline(n)     = p%scanline
      iv%instid(inst)%ifgat(n)        = p%ifgat
      iv%instid(inst)%cloud_flag(:,n) = 1        ! no cloud
      iv%instid(inst)%satzen(n)       = p%satzen  
      iv%instid(inst)%satazi(n)       = 0.0
      iv%instid(inst)%solzen(n)       = 0.0
      iv%instid(inst)%solazi(n)       = 0.0
      iv%instid(inst)%proc_domain(n)  = .false.

      current => p
      p => p%next

      ! free current data
      deallocate ( current % tb_inv )
      deallocate ( current )
   end do

   deallocate ( p )

   call closbf(lnbufr)
   close(lnbufr)
   call da_free_unit(lnbufr)

   call da_trace_exit("da_read_bufrssmis")
#endif

end subroutine da_read_bufrssmis


