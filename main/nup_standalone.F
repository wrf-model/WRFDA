      program nup_standalone
!  Program to read/write wrf output. 
!  OPTION -thin  : It will nestup data to the ratio given
!   Cindy Bruyere - March 2006
!   Some code borrowed from Jim Bresch
!   Modyfied for multi-incremental wrf4dvar by Xin Zhang - Sept. 2006
!   Modyfied for more improvement by Xin Zhang - Jan. 2008
!=================================Run Program================================
!  To thin your input file (by picking up corresponding points)
!     nup_standalone wrfout_file -thin 3 [-debug]
!
!  To see more options
!     nup_standalone -help         
!
!  The output will be written to a file with original file name + -thin
!
!=================================Make Executable============================
!  Make executable:
!    DEC Alpha
!      f90 nup_standalone.f -L/usr/local/netcdf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -free  -o nup_standalone
!
!   linux flags
!      pgf90 nup_standalone.f -L/usr/local/netcdf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -Mfree  -o nup_standalone
!
!   Sun flags
!      f90 nup_standalone.f -L/usr/local/netcdf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -free  -o nup_standalone
!
!   SGI flags
!      f90 nup_standalone.f -L/usr/local/netcdf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -freeform  -o nup_standalone
!
!   IBM flags (NCAR bluesky - 32bit)
!      xlf nup_standalone.f -L/usr/local/lib32/r4i4 -lnetcdf -lm  \
!      -I/usr/local/include  -qfree=f90  -o nup_standalone
!
!   IBM flags (NCAR bluesky - 64bit)
!      xlf nup_standalone.f -L/usr/local/lib64/r4i4 -lnetcdf -lm  \
!      -I/usr/local/include  -qfree=f90  -o nup_standalone
!
!   IBM flags (NCAR bluevista)   
!      xlf nup_standalone.f -L/usr/local/netcdf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -qfree=f90  -o nup_standalone
!
!   Mac flags (with xlf compiler)
!      xlf -O3 nup_standalone.f -L/usr/local/netcdf-3.5.1-xlf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf-3.5.1-xlf/include  -qfree=f90  -o nup_standalone
!
!============================================================================

      use feedback
      INCLUDE 'netcdf.inc'
      parameter (jdim=6)
      integer ncid, status
      integer ishape(jdim)
      integer ishape2(jdim)
      character cval*50
      character name*31
      character (len=31),allocatable, dimension(:)      :: dname
      integer,           allocatable, dimension(:)       :: dval, dval2
      real,              allocatable, dimension(:,:,:,:) :: data, data2
      double precision,  allocatable, dimension(:,:,:,:) :: ddata, ddata2
      integer,           allocatable, dimension(:,:,:,:) :: idata, idata2
      character,         allocatable, dimension(:,:,:,:) :: text
      character omit(10)*80
      integer             :: dims_in(4), dims_out(4), box_start(3), box_end(3)
      real                :: new_cen
      character (len=80)  :: input_file, output_file
      character (len=10)  :: option
      logical             :: debug=.FALSE.
      logical             :: x_ave=.FALSE.
      logical             :: y_ave=.FALSE.
      real                :: okm
      double precision    :: dbval
      integer             :: i, j, ii, jj

      call read_args(input_file,option,ratio,box_start,box_end,debug)
      output_file  = trim(input_file)//option

      write(6,*) 
      write(6,*) "#########################################"
      write(6,*) " Running NUP_STANDALONE with option ",option
      write(6,*) 
      IF ( .not. debug ) write(6,*) "   Ouput will be written to ",output_file

      IF (debug) THEN
        write(6,*) "INPUT FILE:         ",input_file
        write(6,*) "OUTPUT FILE:        ",output_file
        write(6,*) "OPTION:             ",option    
        if ( option(1:5) == '-thin' ) then           ! used for -thin
          write(6,*) "RATIO:             ",int(ratio)    
        endif
      ENDIF


! OPEN INPUT AND OUTPUT FILE
! output_file is input_file_new
      status = nf_open(input_file, 0, ncid)
      if (status .ne. nf_noerr) call handle_err(status)
      status = nf_create(output_file, 0, mcid)
      if (status .ne. nf_noerr) call handle_err(status)

! GET BASIC INFORMTION ABOUT THE FILE
! most important 
!   ndims:  number of dimensions
!   nvars:  number of variables
!   ngatts: number of global attributes
      status = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)
      if (status .ne. nf_noerr) call handle_err(status)
      IF (debug) THEN
        write(6,*) 'ndims = ',ndims,' nvars = ',nvars,' ngatts = ',ngatts, &
                   ' nunlimdimid =',nunlimdimid
      ENDIF

! ALLOCATE SOME VARIABLES
      allocate (dval(ndims))
      allocate(dval2(ndims))
      allocate(dname(ndims))

! GET SOME BASIC DIMS FROM INPUT_FILE
      dx = -99.
      status = nf_get_att_real (ncid, nf_global, 'DX', dx)
      status = nf_get_att_int (ncid, nf_global, 'WEST-EAST_GRID_DIMENSION', iweg)
      status = nf_get_att_int (ncid, nf_global, 'SOUTH-NORTH_GRID_DIMENSION', isng)
      status = nf_get_att_int (ncid, nf_global, 'BOTTOM-TOP_GRID_DIMENSION', ibtg)
      IF (debug) THEN
        write(6,*) "BASICS from input file:"
        write(6,*) "       DX= ", dx
        write(6,*) "       DT= ", dt
        write(6,*) "        X= ", iweg
        write(6,*) "        Y= ", isng
        write(6,*) "        Z= ", ibtg
      ENDIF
      if (dx .lt. 0.) stop 'dx is bad'

! CALCULATE DIMS FOR OUTPUT FILE
      IF ( option(1:5) == '-thin' ) THEN           ! used for -thin
        okm = dx*ratio
        jweg = int((iweg-1)/ratio) + 1
        jsng = int((isng-1)/ratio) + 1
        jbtg = ibtg
      ENDIF
      IF (debug) THEN
        write(6,*) "BASICS for output file:"
        write(6,*) "       DX= ", okm
        write(6,*) "       DT= ", dt
        write(6,*) "        X= ", jweg
        write(6,*) "        Y= ", jsng
        write(6,*) "        Z= ", jbtg
      ENDIF
      !! We also need to fix the CEN_LAT and CEN_LON later, so get
      !! the middle of the new domain
      ix = int((jweg-1)/2.)
      iy = int((jsng-1)/2.)
      if ( ix .eq. int(jweg/2.) ) x_ave = .TRUE.
      if ( iy .eq. int(jsng/2.) ) y_ave = .TRUE.
      ix = int(jweg/2.)
      iy = int(jsng/2.)

! READ ALL DIMS FROM INPUT FILE AND CREATE DIMS FOR OUTPUT FILE
      IF (debug) THEN
        write(6,*) 
        write(6,*) "FILE dimensions:"
      ENDIF
      do i = 1, ndims
        status = nf_inq_dim(ncid, i, dname(i), dval(i))
	dval2(i) = dval(i)
!       CAUTION -- this stuff is hard-wired
	if (dname(i) .eq. 'west_east_stag') then
	  dval2(i) = jweg
	else if (dname(i) .eq. 'west_east') then
	  dval2(i) = jweg-1
	else if (dname(i) .eq. 'south_north_stag') then
	  dval2(i) = jsng
	else if (dname(i) .eq. 'south_north') then
	  dval2(i) = jsng-1
	else if (dname(i) .eq. 'bottom_top_stag') then
	  dval2(i) = jbtg
	else if (dname(i) .eq. 'bottom_top') then
	  dval2(i) = jbtg-1
        endif
        status = nf_def_dim(mcid, dname(i), dval2(i), i)
        IF (debug) THEN
	  write(6,'(" i = ",i2," : ",A," in = ",i4," (out = ",i4,")")') &
                i,dname(i),dval(i),dval2(i)
        ENDIF
      enddo

! DEALING WITH THE GLOBAL ATTRIBUTES
      IF (debug) THEN
        write(6,*) 
        write(6,*) "FILE attributes:"
      ENDIF
      do i = 1, ngatts
        status = nf_inq_attname(ncid, nf_global, i, name)
        status = nf_inq_atttype(ncid, nf_global, name, itype)
        status = nf_inq_attlen(ncid, nf_global, name, ilen)

	if ( itype .eq. 2 ) then        ! characters
	  status = nf_get_att_text (ncid, nf_global, name, cval)
          IF (debug) THEN
	    write(6,'(" i = ",i2," : ",A," in = ",A," (out = ",$)') &
                  i,name,cval(1:ilen)
          ENDIF
	  if(name(1:5) .eq. 'TITLE') then
             cval = cval(1:ilen)//" : nup_standalone"//option
             ilen = len_trim(cval)
          endif
          IF (debug) write(6,'(A,")")') cval(1:ilen)
	  status = nf_put_att_text(mcid, nf_global, name, ilen,&
                    cval(1:ilen))

	elseif ( itype .eq. 4 ) then     ! integers
	  status = nf_get_att_int (ncid, nf_global, name, ival)
          IF (debug) THEN
	    write(6,'(" i = ",i2," : ",A," in = ",i7," (out = ",$)') &
                  i,name,ival        
          ENDIF
	  if(name .eq. 'WEST-EAST_GRID_DIMENSION') ival = jweg
	  if(name .eq. 'SOUTH-NORTH_GRID_DIMENSION') ival = jsng
	  if(name .eq. 'BOTTOM-TOP_GRID_DIMENSION') ival = jbtg
	  if(name .eq. 'WEST-EAST_PATCH_END_UNSTAG') ival = jweg-1
	  if(name .eq. 'WEST-EAST_PATCH_END_STAG') ival = jweg
	  if(name .eq. 'SOUTH-NORTH_PATCH_END_UNSTAG') ival = jsng-1
	  if(name .eq. 'SOUTH-NORTH_PATCH_END_STAG') ival = jsng
          iF (debug) write(6,'(i7,")")') ival
          status = nf_put_att_int(mcid, nf_global, name, itype,&
                    ilen, ival)

	elseif ( itype .eq. 5 ) then    ! real
	  status = nf_get_att_real (ncid, nf_global, name, rval)
          IF (debug) THEN
	    write(6,'(" i = ",i2," : ",A," in = ",G18.10E2," (out = ",$)') &
                  i,name,rval        
          ENDIF
          if(name(1:2) .eq. 'DX' .or. name(1:2) .eq. 'DY') rval = okm
          IF (debug) write(6,'(G18.10E2,")")') rval
	  status = nf_put_att_real(mcid, nf_global, name, itype,&
                    ilen, rval)
	elseif ( itype .eq. 6 ) then    ! double
	  status = nf_get_att_double (ncid, nf_global, name, dbval)
          IF (debug) THEN
	    write(6,'(" i = ",i2," : ",A," in = ",G18.10E2," (out = ",$)') &
                  i,name,dbval        
          ENDIF
          if(name(1:2) .eq. 'DX' .or. name(1:2) .eq. 'DY') dbval = okm
          IF (debug) write(6,'(G18.10E2,")")') dbval
	  status = nf_put_att_double(mcid, nf_global, name, itype,&
                    ilen, dbval)
	endif
      enddo


      IF (debug) THEN
        write(6,*) 
        write(6,*) 
        write(6,*) "FILE variables:"
      ENDIF
      do i = 1, nvars
        status = nf_inq_var(ncid, i, cval, itype, idm, ishape, natt)
	if (idm .gt. jdim) stop
	status = nf_def_var(mcid, cval, itype, idm, ishape, j)
	do na = 1, natt
          status = nf_inq_attname(ncid, i, na, name)
          status = nf_copy_att(ncid, i, name, mcid, i)
	enddo
      enddo
      status = nf_enddef(mcid)

! ########## LOOP THROUGH THE DATA 
      do i = 1, nvars
        status = nf_inq_var(ncid, i, cval, itype, idm, ishape, natt)
        IF (debug) THEN
          write(6,*) 
	  write(6,*) 'VARIABLE: ',trim(cval), '    itype: ',itype
        ENDIF

! GET THE DIMS FOR INPUT AND OUTPUT FROM THE SHAPE
        dims_in  = 1
        dims_out = 1
        do ii = 1,idm
          dims_in(ii)  = dval(ishape(ii))
          dims_out(ii) = dval2(ishape(ii))
        enddo
        IF (debug) THEN
          write(6,*) '   DIMS  IN: ',dims_in
          write(6,*) '   DIMS OUT: ',dims_out
        ENDIF


! ALLOCATE THE INPUT AND OUTPUT ARRAYS
! READ THE DATA FROM INPUT FILE
! THIN THE GRID IF NEEDED, OR GET THE CORRECT BOX

	IF     (itype .eq. 2) THEN          ! character
          allocate (text(dims_in(1), dims_in(2), dims_in(3), &
                         dims_in(4)))
	  status = nf_get_var_text(ncid, i, text)
          IF (debug) write(6,*) '   SAMPLE VALUE = ',text(:,1,1,1)
	  status = nf_put_var_text (mcid, i, text)
          deallocate (text)

	ELSEIF (itype .eq. 4) THEN          ! integer
          allocate (idata(dims_in(1), dims_in(2), dims_in(3), &
                          dims_in(4)))
          allocate(idata2(dims_out(1),dims_out(2),dims_out(3),&
                          dims_out(4)))
	  status = nf_get_var_int(ncid, i, idata)
          IF (debug) write(6,*) '   SAMPLE VALUE = ',idata(1,1,1,1)
          if (idm .ge. 3) then
            IF (debug) write(6,*) '   Grid is thinned with a ratio of ',int(ratio), 'copy_fcni'
            call copy_fcni(idata2,                                           &  ! CD field
                           1, dims_out(1), 1, dims_out(2), 1, dims_out(3),   &
                           idata,                                            &  ! ND field
                           1, dims_in(1), 1, dims_in(2), 1, dims_in(3),      &
                           .false., .false.,                                 &  ! staggering of field
                           1, 1,                                             &  ! Position of lower left of nest in CD
                           int(ratio), int(ratio)               )               ! nest ratios
	  else
	    idata2 = idata
	  endif
	  status = nf_put_var_int (mcid, i, idata2)
          deallocate (idata)
          deallocate (idata2)

	ELSEIF (itype .eq. 5) THEN          ! real
          allocate (data(dims_in(1), dims_in(2), dims_in(3), &
                         dims_in(4)))
          allocate(data2(dims_out(1),dims_out(2),dims_out(3), &
                         dims_out(4)))
	  status = nf_get_var_real(ncid, i, data)
          IF (debug) write(6,*) '   SAMPLE VALUE = ',data(1,1,1,1)
            if (idm .ge. 3) then
            if ( trim(cval) == 'MAPFAC_UX' .or. trim(cval) == 'MAPFAC_U' .or.  &
                 trim(cval) == 'MAPFAC_UY' .or.  &
                 trim(cval) == 'XLAT_U' .or. trim(cval) == 'XLONG_U' ) then
                 IF (debug) write(6,*) '   Grid is thinned with a ratio of ',int(ratio),'copy_fcnm'
                 call copy_fcnm(data2,                                            &  ! CD field
                                1, dims_out(1), 1, dims_out(2), 1, dims_out(3),   &
                                data,                                             &  ! ND field
                                1, dims_in(1), 1, dims_in(2), 1, dims_in(3),      &
                                .true., .false.,                                  &  ! staggering of field
                                1, 1,                                             &  ! Position of lower left of nest in CD
                                int(ratio), int(ratio)               )               ! nest ratios
            elseif ( trim(cval) == 'MAPFAC_VX' .or. trim(cval) == 'MAPFAC_V' .or.  &
                 trim(cval) == 'MAPFAC_VY' .or. trim(cval) == 'MF_VX_INV' .or. &
                 trim(cval) == 'XLAT_V' .or. trim(cval) == 'XLONG_V' ) then
                 IF (debug) write(6,*) '   Grid is thinned with a ratio of ',int(ratio),'copy_fcnm'
                 call copy_fcnm(data2,                                            &  ! CD field
                                1, dims_out(1), 1, dims_out(2), 1, dims_out(3),   &
                                data,                                             &  ! ND field
                                1, dims_in(1), 1, dims_in(2), 1, dims_in(3),      &
                                .false., .true.,                                  &  ! staggering of field
                                1, 1,                                             &  ! Position of lower left of nest in CD
                                int(ratio), int(ratio)               )               ! nest ratios
            elseif ( trim(cval) == 'U' ) then
                 IF (debug) write(6,*) '   Grid is thinned with a ratio of ',int(ratio),'copy_fcn'
                 call copy_fcn (data2,                                            &  ! CD field
                                1, dims_out(1), 1, dims_out(2), 1, dims_out(3),   &
                                data,                                             &  ! ND field
                                1, dims_in(1), 1, dims_in(2), 1, dims_in(3),      &
                                .true., .false.,                                  &  ! staggering of field
                                1, 1,                                             &  ! Position of lower left of nest in CD
                                int(ratio), int(ratio)               )               ! nest ratios
                 CALL smoother (                                                               &
                                data2,                                            &         ! CD field
                                1, dims_out(1), 1, dims_out(2), 1, dims_out(3),   &
                                1, dims_in(1), 1, dims_in(2), 1, dims_in(3),      &
                                .TRUE., .FALSE.,                                                &         ! xstag, ystag
                                1, 1,                                             &  ! Position of lower left of nest in CD
                                int(ratio), int(ratio)               )               ! nest ratios
            elseif ( trim(cval) == 'V' ) then
                 IF (debug) write(6,*) '   Grid is thinned with a ratio of ',int(ratio),'copy_fcn'
                 call copy_fcn (data2,                                            &  ! CD field
                                1, dims_out(1), 1, dims_out(2), 1, dims_out(3),   &
                                data,                                             &  ! ND field
                                1, dims_in(1), 1, dims_in(2), 1, dims_in(3),      &
                                .false., .true.,                                  &  ! staggering of field
                                1, 1,                                             &  ! Position of lower left of nest in CD
                                int(ratio), int(ratio)               )               ! nest ratios
                 CALL smoother (                                                               &
                                data2,                                            &         ! CD field
                                1, dims_out(1), 1, dims_out(2), 1, dims_out(3),   &
                                1, dims_in(1), 1, dims_in(2), 1, dims_in(3),      &
                                .FALSE., .TRUE.,                                                &         ! xstag, ystag
                                1, 1,                                             &  ! Position of lower left of nest in CD
                                int(ratio), int(ratio)               )               ! nest ratios
            elseif ( trim(cval) == 'W' .or. trim(cval) == 'PH' .or. trim(cval) == 'PHB' .or. &
                     trim(cval) == 'T' .or. trim(cval) == 'T_INIT' .or. trim(cval) == 'MU' .or. &
                     trim(cval) == 'MUB' .or. trim(cval) == 'MU0' .or. trim(cval) == 'Q2' .or. &
                     trim(cval) == 'T2' .or. trim(cval) == 'TH2' .or. trim(cval) == 'PSFC' .or. &
                     trim(cval) == 'U10' .or. trim(cval) == 'V10' .or. trim(cval) == 'TH10' .or. &
                     trim(cval) == 'Q10' .or. trim(cval) == 'QVAPOR' .or. trim(cval) == 'QCLOUD' .or. &
                     trim(cval) == 'QRAIN' .or. trim(cval) == 'QICE' .or. trim(cval) == 'QGRAUP' .or. &
                     trim(cval) == 'HGT' .or. trim(cval) == 'SNOWNC' .or. trim(cval) == 'GRAUPELNCV' &
                      ) then
                 IF (debug) write(6,*) '   Grid is thinned with a ratio of ',int(ratio),'copy_fcn'
                 call copy_fcn (data2,                                            &  ! CD field
                                1, dims_out(1), 1, dims_out(2), 1, dims_out(3),   &
                                data,                                             &  ! ND field
                                1, dims_in(1), 1, dims_in(2), 1, dims_in(3),      &
                                .false., .false.,                                  &  ! staggering of field
                                1, 1,                                             &  ! Position of lower left of nest in CD
                                int(ratio), int(ratio)               )               ! nest ratios
                 CALL smoother (                                                               &
                                data2,                                            &         ! CD field
                                1, dims_out(1), 1, dims_out(2), 1, dims_out(3),   &
                                1, dims_in(1), 1, dims_in(2), 1, dims_in(3),      &
                                .FALSE., .FALSE.,                                                &         ! xstag, ystag
                                1, 1,                                             &  ! Position of lower left of nest in CD
                                int(ratio), int(ratio)               )               ! nest ratios
            else
                 IF (debug) write(6,*) '   Grid is thinned with a ratio of ',int(ratio),'copy_fcnm'
                 call copy_fcnm(data2,                                            &  ! CD field
                                1, dims_out(1), 1, dims_out(2), 1, dims_out(3),   &
                                data,                                             &  ! ND field
                                1, dims_in(1), 1, dims_in(2), 1, dims_in(3),      &
                                .false., .false.,                                 &  ! staggering of field
                                1, 1,                                             &  ! Position of lower left of nest in CD
                                int(ratio), int(ratio)               )               ! nest ratios
            endif
	  else
	    data2 = data
	  endif
	  status = nf_put_var_real (mcid, i, data2)
          IF ( cval == 'XLAT' .or. cval == 'XLONG' ) THEN
!            We need fix the box's center long and lat
             new_cen = data2(ix,iy,1,1)
             if ( x_ave .and. y_ave ) then
               new_cen = (data2(ix,  iy,1,1)+data2(ix  ,iy+1,1,1)+  &
                          data2(ix+1,iy,1,1)+data2(ix+1,iy+1,1,1))/4.
             elseif ( x_ave .and. .not. y_ave ) then
               new_cen = (data2(ix,  iy,1,1)+data2(ix+1,iy  ,1,1))/2.
             elseif ( .not. x_ave .and. y_ave ) then
               new_cen = (data2(ix,  iy,1,1)+data2(ix  ,iy+1,1,1))/2.
             endif
          ENDIF
          IF ( cval == 'XLAT' ) THEN
             IF (debug) write(6,*) '   Fix global attribute CEN_LAT: now = ', new_cen
             status = nf_inq_atttype(ncid, nf_global, 'CEN_LAT', itype)
             status = nf_inq_attlen(ncid, nf_global, 'CEN_LAT', ilen)
! 	     status = nf_put_att_real(mcid, nf_global, 'CEN_LAT', itype,&
!                      ilen, new_cen)
          ELSEIF ( cval == 'XLONG' ) THEN
             IF (debug) write(6,*) '   Fix global attribute CEN_LON: now = ', new_cen
             status = nf_inq_atttype(ncid, nf_global, 'CEN_LON', itype)
             status = nf_inq_attlen(ncid, nf_global, 'CEN_LON', ilen)
!	     status = nf_put_att_real(mcid, nf_global, 'CEN_LON', itype,&
!                      ilen, new_cen)
          ENDIF
          deallocate (data)
          deallocate (data2)

	ELSEIF (itype .eq. 6) THEN          ! double
          allocate (ddata(dims_in(1), dims_in(2), dims_in(3), &
                          dims_in(4)))
          allocate(ddata2(dims_out(1),dims_out(2),dims_out(3),&
                          dims_out(4)))
	  status = nf_get_var_double(ncid, i, ddata)
          IF (debug) write(6,*) '   SAMPLE VALUE = ',ddata(1,1,1,1)
            if (idm .ge. 3) then
            IF (debug) write(6,*) '   Grid is thinned with a ratio of ',int(ratio)
            if ( trim(cval) == 'U' .or. trim(cval) == 'MAPFAC_U' .or.  &
                 trim(cval) == 'XLAT_U' .or. trim(cval) == 'XLONG_U' ) then
                 IF (debug) write(6,*) '   Attention'
                 ddata2 = ddata(1:dims_in(1):3,2:dims_in(2):3,:,:)
            elseif ( trim(cval) == 'V' .or. trim(cval) == 'MAPFAC_V' .or.  &
                 trim(cval) == 'XLAT_V' .or. trim(cval) == 'XLONG_V' ) then
                 IF (debug) write(6,*) '   Attention'
                 ddata2 = ddata(2:dims_in(1):3,1:dims_in(2):3,:,:)
            else
                 ddata2 = ddata(2:dims_in(1):3,2:dims_in(2):3,:,:)
            endif
	  else
	    ddata2 = ddata
	  endif
	  status = nf_put_var_double (mcid, i, ddata2)
          deallocate (ddata)
          deallocate (ddata2)
        ELSE
	    stop 'trouble - do not know the variable type'
        ENDIF

      ENDDO     ! END OF VARIABLE LOOP
      status = nf_close(mcid)

      write(6,*) 
      write(6,*) " Success - we are out of here"      
      write(6,*) "#########################################"

      end program nup_standalone
!---------------------------------------------------------------------
      subroutine handle_err(status)
      integer status
      write(6,*) 'Error number ',status
      stop
      end subroutine
!---------------------------------------------------------------------

  subroutine read_args(input_file,option,ratio,box_start,box_end,debug)

  implicit none
  character (len=80)    :: input_file
  character (len=10)    :: option

  integer               :: iratio
  real                  :: ratio
  integer               :: box_start(3), box_end(3)
  logical               :: debug

  integer               :: numarg, i, idummy, idummy1, idummy2
  real                  :: rdummy
  integer, external     :: iargc
  character (len=80)    :: dummy, dir

! set up some defaults first
  input_file = " "
  option = " "
  ratio = 0.0
  idummy1 = 0
  idummy2 = 0
  box_start = 0
  box_end   = 0
  numarg = iargc()
  i = 1

  if (numarg .lt. 1) call help_info

  do while (i <= numarg)
    call getarg(i,dummy)

    if (dummy(1:1) == "-") then    ! We have an option, else it is the filename

      SELECTCASE (trim(dummy))
          CASE ("-help")
               call help_info
          CASE ("-h")
               call help_info
          CASE ("-debug")
               debug = .TRUE.     
          CASE ("-thin")
               option = dummy
               i = i+1
               call getarg(i,dummy)
               read(dummy,'(i3)')idummy
               iratio = idummy
               if ( iratio .lt. 2 ) STOP ' Must supply a ratio of 2 or more ' 
               ratio = real(iratio)
          CASE DEFAULT
               call help_info
      END SELECT
    else
      input_file = dummy
    endif

      i = i+1

  enddo

  if (input_file == " ") call help_info


  end subroutine read_args
!------------------------------------------------------------------------------

  subroutine help_info

  print*," "
  print*," nup_standalone   wrf_data_file_name  [-options] "
  print*," "
  print*," Current options available are:"
  print*," -help     : Print this information"                           
  print*," -h        : Print this information"                           
  print*," "
  print*," -thin x   : Thin the input grid, with a ration of x"  
  print*,"             Example:"
  print*,"                -thin 3 " 
  print*,"             Will thin the grid with a ratio of 3, i.e.,   "
  print*,"             a 12km grid will be upscaled to a 36km grid" 
  print*,"             The new grid point will be the feedback value from the input domain"
  print*," "
  print*," -debug    : Print information about dimensions/attributes and variables"
  print*," "

  STOP

  end subroutine help_info

!-----------------------------------------------------------------------------

MODULE feedback

   implicit none

   interface copy_fcn
     module procedure copy_fcn_real, copy_fcn_double
   end interface

   interface copy_fcnm
     module procedure copy_fcnm_real, copy_fcnm_double
   end interface

   contains

   SUBROUTINE copy_fcn_real ( cfld,                                 &  ! CD field
                           cids, cide, cjds, cjde, ckds, ckde,   &
                           nfld,                                 &  ! ND field
                           nids, nide, njds, njde, nkds, nkde,   &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nids:nide, njds:njde, nkds:nkde ),INTENT(IN)  :: nfld

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints

     !  Loop over the coarse grid in the area of the fine mesh.  Do not
     !  process the coarse grid values that are along the lateral BC
     !  provided to the fine grid.  Since that is in the specified zone
     !  for the fine grid, it should not be used in any feedback to the
     !  coarse grid as it should not have changed.

     !  Due to peculiarities of staggering, it is simpler to handle the feedback
     !  for the staggerings based upon whether it is a even ratio (2::1, 4::1, etc.) or
     !  an odd staggering ratio (3::1, 5::1, etc.). 

     !  Though there are separate grid ratios for the i and j directions, this code
     !  is not general enough to handle aspect ratios .NE. 1 for the fine grid cell.
 
     !  These are local integer increments in the looping.  Basically, istag=1 means
     !  that we will assume one less point in the i direction.  Note that ci and cj
     !  have a maximum value that is decreased by istag and jstag, respectively.  

     !  Horizontal momentum feedback is along the face, not within the cell.  For a
     !  3::1 ratio, temperature would use 9 pts for feedback, while u and v use
     !  only 3 points for feedback from the nest to the parent.

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        IF      ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + jstag + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, cj, ck ) = 0.
                    DO ijpoints = 1 , nri * nrj
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, cj, ck ) =  cfld( ci, cj, ck ) + &
                                             1./REAL(nri*nrj) * nfld( ni+ipoints , nj+jpoints , nk )
                    END DO
                 ENDDO
              ENDDO
           ENDDO

        ELSE IF ( (       xstag ) .AND. ( .NOT. ystag ) ) THEN
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + jstag + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, cj, ck ) = 0.
                    DO ijpoints = (nri+1)/2 , (nri+1)/2 + nri*(nri-1) , nri
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, cj, ck ) =  cfld( ci, cj, ck ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nj+jpoints , nk )
                    END DO
                 ENDDO
              ENDDO
           ENDDO

        ELSE IF ( ( .NOT. xstag ) .AND. (       ystag ) ) THEN
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + jstag + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, cj, ck ) = 0.
                    DO ijpoints = ( nrj*nrj +1 )/2 - nrj/2 , ( nrj*nrj +1 )/2 - nrj/2 + nrj-1
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, cj, ck ) =  cfld( ci, cj, ck ) + &
                                             1./REAL(    nrj) * nfld( ni+ipoints , nj+jpoints , nk )
                    END DO
                 ENDDO
              ENDDO
           ENDDO

        END IF

     !  Even refinement ratio

     ELSE IF ( MOD(nrj,2) .EQ. 0) THEN
        IF ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN

        !  This is a simple schematic of the feedback indexing used in the even
        !  ratio nests.  For simplicity, a 2::1 ratio is depicted.  Only the 
        !  mass variable staggering is shown. 
        !                                                                  Each of
        !  the boxes with a "T" and four small "t" represents a coarse grid (CG)
        !  cell, that is composed of four (2::1 ratio) fine grid (FG) cells.
   
        !  Shown below is the area of the CG that is in the area of the FG.   The
        !  first grid point of the depicted CG is the starting location of the nest
        !  in the parent domain (ipos,jpos - i_parent_start and j_parent_start from
        !  the namelist).  
   
        !  For each of the CG points, the feedback loop is over each of the FG points
        !  within the CG cell.  For a 2::1 ratio, there are four total points (this is 
        !  the ijpoints loop).  The feedback value to the CG is the arithmetic mean of 
        !  all of the FG values within each CG cell.

!              |-------------||-------------|                          |-------------||-------------|
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
! jpos+        |             ||             |                          |             ||             |
! (njde-njds)- |      T      ||      T      |                          |      T      ||      T      |
! jstag        |             ||             |                          |             ||             |
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |-------------||-------------|                          |-------------||-------------|
!              |-------------||-------------|                          |-------------||-------------|
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |             ||             |                          |             ||             |
!              |      T      ||      T      |                          |      T      ||      T      |
!              |             ||             |                          |             ||             |
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |-------------||-------------|                          |-------------||-------------|
!
!                   ...
!                   ...
!                   ...
!                   ...
!                   ...

!              |-------------||-------------|                          |-------------||-------------|
! jpoints = 1  |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |             ||             |                          |             ||             |
!              |      T      ||      T      |                          |      T      ||      T      |
!              |             ||             |                          |             ||             |
! jpoints = 0, |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!  nj=3        |-------------||-------------|                          |-------------||-------------|
!              |-------------||-------------|                          |-------------||-------------|
! jpoints = 1  |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |             ||             |                          |             ||             |
!    jpos      |      T      ||      T      |          ...             |      T      ||      T      |
!              |             ||             |          ...             |             ||             |
! jpoints = 0, |  t      t   ||  t      t   |          ...             |  t      t   ||  t      t   |
!  nj=1        |-------------||-------------|                          |-------------||-------------|
!                     ^                                                                      ^
!                     |                                                                      |
!                     |                                                                      |
!                   ipos                                                                   ipos+
!     ni =        1              3                                                         (nide-nids)/nri
! ipoints=        0      1       0      1                                                  -istag
!

           !  For performance benefits, users can comment out the inner most loop (and cfld=0) and
           !  hardcode the loop feedback.  For example, it is set up to run a 2::1 ratio
           !  if uncommented.  This lacks generality, but is likely to gain timing benefits
           !  with compilers unable to unroll inner loops that do not have parameterized sizes.
   
           !  The extra +1 ---------/ and the extra -1 ----\  (both for ci and cj) 
           !                       /                        \   keeps the feedback out of the 
           !                      /                          \  outer row/col, since that CG data
           !                     /                            \ specified the nest boundary originally
           !                    /                              \   This
           !                   /                                \    is just
           !                  /                                  \   a sentence to not end a line
           !                 /                                    \   with a stupid backslash
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + jstag
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + istag
                    cfld( ci, cj, ck ) = 0.
                    DO ijpoints = 1 , nri * nrj
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, cj, ck ) =  cfld( ci, cj, ck ) + &
                                             1./REAL(nri*nrj) * nfld( ni+ipoints , nj+jpoints , nk )
                    END DO
                 END DO
              END DO
           END DO

        !  U

        ELSE IF ( (       xstag ) .AND. ( .NOT. ystag ) ) THEN
!              |---------------|
!              |               |
! jpoints = 1  u       u       |
!              |               |
!              U               |
!              |               |
! jpoints = 0, u       u       |
!  nj=3        |               |
!              |---------------|
!              |---------------|
!              |               |
! jpoints = 1  u       u       |
!              |               |
!    jpos      U               |
!              |               |
! jpoints = 0, u       u       |
! nj=1         |               |
!              |---------------|
! 
!              ^               
!              |              
!              |             
!            ipos           
!     ni =     1               3
! ipoints=     0       1       0 
!

           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + 1
                    cfld( ci, cj, ck ) = 0.
                    DO ijpoints = 1 , nri*nrj , nri
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, cj, ck ) =  cfld( ci, cj, ck ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nj+jpoints , nk )
                    END DO
                 ENDDO
              ENDDO
           ENDDO

        !  V 

        ELSE IF ( ( .NOT. xstag ) .AND. (       ystag ) ) THEN
           DO ck = ckds, ckde
              nk = ck
              DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
                 nj = (cj-jpos)*nrj + 1
                 DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                    ni = (ci-ipos)*nri + 1
                    cfld( ci, cj, ck ) = 0.
                    DO ijpoints = 1 , nri
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, cj, ck ) =  cfld( ci, cj, ck ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nj+jpoints , nk )
                    END DO
                 ENDDO
              ENDDO
           ENDDO
        END IF
     END IF

     RETURN

   END SUBROUTINE copy_fcn_real

!==================================
! this is the 1pt function used in feedback.

   SUBROUTINE copy_fcnm_double (  cfld,                                 &  ! CD field
                           cids, cide, cjds, cjde, ckds, ckde,   &
                           nfld,                                 &  ! ND field
                           nids, nide, njds, njde, nkds, nkde,   &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     DOUBLE PRECISION, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde ), INTENT(OUT) :: cfld
     DOUBLE PRECISION, DIMENSION ( nids:nide, njds:njde, nkds:nkde ), INTENT(IN) :: nfld

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        DO ck = ckds, ckde
           nk = ck
           DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, cj, ck ) =  nfld( ni  , nj , nk  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  ! even refinement ratio, pick nearest neighbor on SW corner
        DO ck = ckds, ckde
           nk = ck
           DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
              nj = (cj-jpos)*nrj + 1
              DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                 ni = (ci-ipos)*nri + 1
                 ipoints = nri/2 -1
                 jpoints = nrj/2 -1
                 cfld( ci, cj, ck ) =  nfld( ni+ipoints , nj+jpoints , nk )
              END DO
           END DO
        END DO

     END IF

     RETURN

   END SUBROUTINE copy_fcnm_double

   SUBROUTINE copy_fcnm_real (  cfld,                                 &  ! CD field
                           cids, cide, cjds, cjde, ckds, ckde,   &
                           nfld,                                 &  ! ND field
                           nids, nide, njds, njde, nkds, nkde,   &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nids:nide, njds:njde, nkds:nkde ), INTENT(IN) :: nfld

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        DO ck = ckds, ckde
           nk = ck
           DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, cj, ck ) =  nfld( ni  , nj , nk  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  ! even refinement ratio, pick nearest neighbor on SW corner
        DO ck = ckds, ckde
           nk = ck
           DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
              nj = (cj-jpos)*nrj + 1
              DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                 ni = (ci-ipos)*nri + 1
                 ipoints = nri/2 -1
                 jpoints = nrj/2 -1
                 cfld( ci, cj, ck ) =  nfld( ni+ipoints , nj+jpoints , nk )
              END DO
           END DO
        END DO

     END IF

     RETURN

   END SUBROUTINE copy_fcnm_real

   SUBROUTINE copy_fcnm_double (  cfld,                                 &  ! CD field
                           cids, cide, cjds, cjde, ckds, ckde,   &
                           nfld,                                 &  ! ND field
                           nids, nide, njds, njde, nkds, nkde,   &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     DOUBLE PRECISION, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde ), INTENT(OUT) :: cfld
     DOUBLE PRECISION, DIMENSION ( nids:nide, njds:njde, nkds:nkde ), INTENT(IN) :: nfld

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        DO ck = ckds, ckde
           nk = ck
           DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, cj, ck ) =  nfld( ni  , nj , nk  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  ! even refinement ratio, pick nearest neighbor on SW corner
        DO ck = ckds, ckde
           nk = ck
           DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
              nj = (cj-jpos)*nrj + 1
              DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                 ni = (ci-ipos)*nri + 1
                 ipoints = nri/2 -1
                 jpoints = nrj/2 -1
                 cfld( ci, cj, ck ) =  nfld( ni+ipoints , nj+jpoints , nk )
              END DO
           END DO
        END DO

     END IF

     RETURN

   END SUBROUTINE copy_fcnm_real
!==================================
! this is the 1pt function used in feedback for integers

   SUBROUTINE copy_fcni ( cfld,                                 &  ! CD field
                           cids, cide, cjds, cjde, ckds, ckde,   &
                           nfld,                                 &  ! ND field
                           nids, nide, njds, njde, nkds, nkde,   &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     INTEGER, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde ), INTENT(OUT) :: cfld
     INTEGER, DIMENSION ( nids:nide, njds:njde, nkds:nkde ), INTENT(IN) :: nfld

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        DO ck = ckds, ckde
           nk = ck
           DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, cj, ck ) =  nfld( ni  , nj , nk  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  ! even refinement ratio
        DO ck = ckds, ckde
           nk = ck
           DO cj = MAX(jpos,cjds),MIN(jpos+(njde-njds)/nrj-jstag,cjde)
              nj = (cj-jpos)*nrj + 1
              DO ci = MAX(ipos,cids),MIN(ipos+(nide-nids)/nri-istag,cide)
                 ni = (ci-ipos)*nri + 1
                 ipoints = nri/2 -1
                 jpoints = nrj/2 -1
                 cfld( ci, cj, ck ) =  nfld( ni+ipoints , nj+jpoints , nk )
              END DO
           END DO
        END DO

     END IF

     RETURN

   END SUBROUTINE copy_fcni

!==================================
   SUBROUTINE smoother ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      nids, nide, nkds, nkde, njds, njde,   &
                      xstag, ystag,                         &  ! staggering of field
                      ipos, jpos,                           &  ! Position of lower left of nest in
                      nri, nrj                              &
                      )
 
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
      INTEGER             :: smooth_option, feedback 
   
      REAL, DIMENSION ( cids:cide, ckds:ckde, cjds:cjde ) :: cfld

      !  These are the 2d smoothers used on the fedback data.  These filters
      !  are run on the coarse grid data (after the nested info has been
      !  fedback).  Only the area of the nest in the coarse grid is filtered.

      smooth_option = 0

      IF      ( smooth_option == 0 ) THEN
! no op
      ELSE IF ( smooth_option == 1 ) THEN
         CALL sm121 ( cfld , &
                      cids, cide, cjds, cjde, ckds, ckde,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, njds, njde, nkds, nkde,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
      ELSE IF ( smooth_option == 2 ) THEN
         CALL smdsm ( cfld , &
                      cids, cide, cjds, cjde, ckds, ckde,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, njds, njde, nkds, nkde,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
      END IF

   END SUBROUTINE smoother 

   SUBROUTINE sm121 ( cfld , &
                      cids, cide, cjds, cjde, ckds, ckde,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, njds, njde, nkds, nkde,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
   
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde ) :: cfld
      REAL, DIMENSION ( cids:cide,            cjds:cjde ) :: cfldnew
   
      INTEGER                        :: i , j , k , loop
      INTEGER :: istag,jstag

      INTEGER, PARAMETER  :: smooth_passes = 1 ! More passes requires a larger stencil (currently 48 pt)

      istag = 1 ; jstag = 1
      IF ( xstag ) istag = 0
      IF ( ystag ) jstag = 0
   
      !  Simple 1-2-1 smoother.
   
      smoothing_passes : DO loop = 1 , smooth_passes
   
         DO k = ckds , ckde
   
            !  Initialize dummy cfldnew

            DO i = MAX(ipos,cids-3) , MIN(ipos+(nide-nids)/nri,cide+3)
               DO j = MAX(jpos,cjds-3) , MIN(jpos+(njde-njds)/nrj,cjde+3)
                  cfldnew(i,j) = cfld(i,j,k) 
               END DO
            END DO

            !  1-2-1 smoothing in the j direction first, 
   
            DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
            DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
                  cfldnew(i,j) = 0.25 * ( cfld(i,j+1,k) + 2.*cfld(i,j,k) + cfld(i,j-1,k) )
               END DO
            END DO

            !  then 1-2-1 smoothing in the i direction last
       
            DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
            DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
                  cfld(i,j,k) =  0.25 * ( cfldnew(i+1,j) + 2.*cfldnew(i,j) + cfldnew(i-1,j) )
               END DO
            END DO
       
         END DO
    
      END DO smoothing_passes
   
   END SUBROUTINE sm121

   SUBROUTINE smdsm ( cfld , &
                      cids, cide, cjds, cjde, ckds, ckde,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, njds, njde, nkds, nkde,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
   
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cids:cide, cjds:cjde, ckds:ckde ) :: cfld
      REAL, DIMENSION ( cids:cide,            cjds:cjde ) :: cfldnew
   
      REAL , DIMENSION ( 2 )         :: xnu
      INTEGER                        :: i , j , k , loop , n 
      INTEGER :: istag,jstag

      INTEGER, PARAMETER  :: smooth_passes = 1 ! More passes requires a larger stencil (currently 48 pt)

      xnu  =  (/ 0.50 , -0.52 /)
    
      istag = 1 ; jstag = 1
      IF ( xstag ) istag = 0
      IF ( ystag ) jstag = 0
   
      !  The odd number passes of this are the "smoother", the even
      !  number passes are the "de-smoother" (note the different signs on xnu).
   
      smoothing_passes : DO loop = 1 , smooth_passes * 2
   
         n  =  2 - MOD ( loop , 2 )
    
         DO k = ckds , ckde
   
            DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
               DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
                  cfldnew(i,j) = cfld(i,j,k) + xnu(n) * ((cfld(i,j+1,k) + cfld(i,j-1,k)) * 0.5-cfld(i,j,k))
               END DO
            END DO
       
            DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
               DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
                  cfld(i,j,k) = cfldnew(i,j)
               END DO
            END DO
       
            DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
               DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
                  cfldnew(i,j) = cfld(i,j,k) + xnu(n) * ((cfld(i+1,j,k) + cfld(i-1,j,k)) * 0.5-cfld(i,j,k))
               END DO
            END DO
       
            DO j = MAX(jpos+2,cjds-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjde+2)
               DO i = MAX(ipos+2,cids-2) , MIN(ipos+(nide-nids)/nri-2-istag,cide+2)
                  cfld(i,j,k) = cfldnew(i,j)
               END DO
            END DO
   
         END DO
    
      END DO smoothing_passes
   
   END SUBROUTINE smdsm

END MODULE feedback

!==================================

