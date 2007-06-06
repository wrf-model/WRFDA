      program nupdown
!  Program to read/write wrf output. 
!  OPTION -thin  : It will nestup data to the ratio given
!  OPTION -down  : It will nestdown data to the ratio given
!   Cindy Bruyere - March 2006
!   Some code borrowed from Jim Bresch
!   Modyfied for multi-incremental wrf4dvar by Xin Zhang - Sept. 2006
!=================================Run Program================================
!  To thin your input file down (by picking up corresponding points)
!     nupdown wrfout_file -thin/down 3 [-debug]
!
!  To see more options
!     nupdown -help         
!
!  The output will be written to a file with original file name + -thin/down
!
!=================================Make Executable============================
!  Make executable:
!    DEC Alpha
!      f90 nupdown.f -L/usr/local/netcdf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -free  -o nupdown
!
!   linux flags
!      pgf90 nupdown.f -L/usr/local/netcdf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -Mfree  -o nupdown
!
!   Sun flags
!      f90 nupdown.f -L/usr/local/netcdf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -free  -o nupdown
!
!   SGI flags
!      f90 nupdown.f -L/usr/local/netcdf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -freeform  -o nupdown
!
!   IBM flags (NCAR bluesky - 32bit)
!      xlf nupdown.f -L/usr/local/lib32/r4i4 -lnetcdf -lm  \
!      -I/usr/local/include  -qfree=f90  -o nupdown
!
!   IBM flags (NCAR bluesky - 64bit)
!      xlf nupdown.f -L/usr/local/lib64/r4i4 -lnetcdf -lm  \
!      -I/usr/local/include  -qfree=f90  -o nupdown
!
!   IBM flags (NCAR bluevista)   
!      xlf nupdown.f -L/usr/local/netcdf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -qfree=f90  -o nupdown
!
!   Mac flags (with xlf compiler)
!      xlf -O3 nupdown.f -L/usr/local/netcdf-3.5.1-xlf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf-3.5.1-xlf/include  -qfree=f90  -o nupdown
!
!============================================================================

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
      write(6,*) " Running NUPDOWN with option ",option
      write(6,*) 
      IF ( .not. debug ) write(6,*) "   Ouput will be written to ",output_file

      IF (debug) THEN
        write(6,*) "INPUT FILE:         ",input_file
        write(6,*) "OUTPUT FILE:        ",output_file
        write(6,*) "OPTION:             ",option    
        if ( option(1:5) == '-thin' .or. option(1:5) == '-down' ) then           ! used for -thin or -down
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
      ELSEIF ( option(1:5) == '-down' ) THEN           ! used for -down
        okm = dx/ratio
        jweg = int((iweg-1)*ratio) + 1
        jsng = int((isng-1)*ratio) + 1
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
             cval = cval(1:ilen)//" : nupdown"//option
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
          IF ( option == '-thin' ) THEN   
            if (idm .ge. 3) then
              IF (debug) write(6,*) '   Grid is thinned with a ratio of ',int(ratio)
              idata2 = idata(2:dims_in(1):3,2:dims_in(2):3,:,:)
	    else
	      idata2 = idata
	    endif
          ELSEIF ( option == '-down' ) THEN   
            if (idm .ge. 3) then
              IF (debug) write(6,*) '   Grid is nestdowned with a ratio of ',int(ratio)
                   idata2(2:dims_out(1):3,2:dims_out(2):3,:,:) = idata
                   do jj = 3 , dims_out(2) , 3
                      idata2(2:dims_out(1):3,jj,:,:) = ( idata2(2:dims_out(1):3,jj-1,:,:) &
                              * 2.0 + idata2(2:dims_out(1):3,jj+2,:,:) * 1.0 ) / 3.0
                   enddo
                   do jj = 4 , dims_out(2) , 3
                      idata2(2:dims_out(1):3,jj,:,:) = ( idata2(2:dims_out(1):3,jj+1,:,:) &
                              * 2.0 + idata2(2:dims_out(1):3,jj-2,:,:) * 1.0 ) / 3.0
                   enddo
                   do ii = 3 , dims_out(1) , 3
                      idata2(ii,2:dims_out(2):3,:,:) = ( idata2(ii-1,2:dims_out(2):3,:,:) &
                              * 2.0 + idata2(ii+2,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   enddo
                   do ii = 4 , dims_out(1) , 3
                      idata2(ii,2:dims_out(2):3,:,:) = ( idata2(ii+1,2:dims_out(2):3,:,:) &
                              * 2.0 + idata2(ii-2,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   enddo
                   idata2(1,2:dims_out(2):3,:,:) = ( 2.0*idata2(2,2:dims_out(2):3,:,:)  &
                              - idata2(3,2:dims_out(2):3,:,:)  )
                   idata2(dims_out(1),2:dims_out(2):3,:,:) = ( 2.0*idata2(dims_out(1)-1,2:dims_out(2):3,:,:)  &
                              - idata2(dims_out(1)-2,2:dims_out(2):3,:,:)  )
                   idata2(2:dims_out(1):3,1,:,:) = ( 2.0*idata2(2:dims_out(1):3,2,:,:)  &
                              - idata2(2:dims_out(1):3,3,:,:)  )
                   idata2(2:dims_out(1):3,dims_out(2),:,:) = ( 2.0*idata2(2:dims_out(1):3,dims_out(2)-1,:,:)  &
                              - idata2(2:dims_out(1):3,dims_out(2)-2,:,:)  )
                   idata2(1:dims_out(1):3,3:dims_out(2):3,:,:) = &
                               ( idata2(1:dims_out(1):3,2:dims_out(2):3,:,:) * 2.0 &
                               + idata2(1:dims_out(1):3,5:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   idata2(1:dims_out(1):3,4:dims_out(2):3,:,:) = &
                               ( idata2(1:dims_out(1):3,5:dims_out(2):3,:,:) * 2.0 &
                               + idata2(1:dims_out(1):3,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   idata2(3:dims_out(1):3,3:dims_out(2):3,:,:) = &
                               ( idata2(3:dims_out(1):3,2:dims_out(2):3,:,:) * 2.0 &
                               + idata2(3:dims_out(1):3,5:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   idata2(3:dims_out(1):3,4:dims_out(2):3,:,:) = &
                               ( idata2(3:dims_out(1):3,5:dims_out(2):3,:,:) * 2.0 &
                               + idata2(3:dims_out(1):3,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   idata2(3:dims_out(1):3,1,:,:) = &
                               ( idata2(2:dims_out(1):3,1,:,:) * 2.0 &
                               + idata2(5:dims_out(1):3,1,:,:) * 1.0 ) / 3.0
                   idata2(4:dims_out(1):3,1,:,:) = &
                               ( idata2(5:dims_out(1):3,1,:,:) * 2.0 &
                               + idata2(2:dims_out(1):3,1,:,:) * 1.0 ) / 3.0
                   idata2(3:dims_out(1):3,dims_out(2),:,:) = &
                               ( idata2(2:dims_out(1):3,dims_out(2),:,:) * 2.0 &
                               + idata2(5:dims_out(1):3,dims_out(2),:,:) * 1.0 ) / 3.0
                   idata2(4:dims_out(1):3,dims_out(2),:,:) = &
                               ( idata2(5:dims_out(1):3,dims_out(2),:,:) * 2.0 &
                               + idata2(2:dims_out(1):3,dims_out(2),:,:) * 1.0 ) / 3.0
                   idata2(1,1,:,:) = ( 2.0*idata2(2,1,:,:) - idata2(3,1,:,:)  )
                   idata2(dims_out(1),1,:,:) = ( 2.0*idata2(dims_out(1)-1,1,:,:) - idata2(dims_out(1)-2,1,:,:)  )
                   idata2(1,dims_out(2),:,:) = ( 2.0*idata2(1,dims_out(2)-1,:,:) - idata2(1,dims_out(2)-2,:,:)  )
                   idata2(dims_out(1),dims_out(2),:,:) = ( 2.0*idata2(dims_out(1)-1,dims_out(2),:,:) - idata2(dims_out(1)-2,dims_out(2),:,:)  )
                   if ( trim(cval) .eq. 'IVGTYP'  ) then 
                        where ( idata2 .lt. 1 )  idata2 = 1
                        where ( idata2 .gt. 19 ) idata2 = 19
                   endif
                   if ( trim(cval) .eq. 'ISLTYP'  ) then 
                        where ( idata2 .lt. 3 )  idata2 = 3
                        where ( idata2 .gt. 14 ) idata2 = 14
                   endif
	    else
	      idata2 = idata
	    endif
          ENDIF
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
          IF ( option == '-thin' ) THEN    
            if (idm .ge. 3) then
              IF (debug) write(6,*) '   Grid is thinned with a ratio of ',int(ratio)
              if ( trim(cval) == 'U' .or. trim(cval) == 'MAPFAC_U' .or.  &
                   trim(cval) == 'XLAT_U' .or. trim(cval) == 'XLONG_U' ) then
                   IF (debug) write(6,*) '   Attention'
                   data2 = data(1:dims_in(1):3,2:dims_in(2):3,:,:)
              elseif ( trim(cval) == 'V' .or. trim(cval) == 'MAPFAC_V' .or.  &
                   trim(cval) == 'XLAT_V' .or. trim(cval) == 'XLONG_V' ) then
                   IF (debug) write(6,*) '   Attention'
                   data2 = data(2:dims_in(1):3,1:dims_in(2):3,:,:)
              else
                   data2 = data(2:dims_in(1):3,2:dims_in(2):3,:,:)
              endif
	    else
	      data2 = data
	    endif
          ELSEIF ( option == '-down' ) THEN    
            if (idm .ge. 3) then
              IF (debug) write(6,*) '   Grid is nestdowned with a ratio of ',int(ratio)
              if ( trim(cval) == 'U' .or. trim(cval) == 'MAPFAC_U' .or.  &
                   trim(cval) == 'XLAT_U' .or. trim(cval) == 'XLONG_U' ) then
                   data2(1:dims_out(1):3,2:dims_out(2):3,:,:) = data
                   do ii = 2 , dims_out(1) , 3
                      data2(ii,2:dims_out(2):3,:,:) = ( data2(ii-1,2:dims_out(2):3,:,:) &
                              * 2.0 + data2(ii+2,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   enddo
                   do ii = 3 , dims_out(1) , 3
                      data2(ii,2:dims_out(2):3,:,:) = ( data2(ii+1,2:dims_out(2):3,:,:) &
                              * 2.0 + data2(ii-2,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   enddo
                   do jj = 3 , dims_out(2) , 3
                      data2(1:dims_out(1):3,jj,:,:) = ( data2(1:dims_out(1):3,jj-1,:,:) &
                              * 2.0 + data2(1:dims_out(1):3,jj+2,:,:) * 1.0 ) / 3.0
                   enddo
                   do jj = 4 , dims_out(2) , 3
                      data2(1:dims_out(1):3,jj,:,:) = ( data2(1:dims_out(1):3,jj+1,:,:) &
                              * 2.0 + data2(1:dims_out(1):3,jj-2,:,:) * 1.0 ) / 3.0
                   enddo
                   data2(1:dims_out(1):3,1,:,:) = ( 2.0*data2(1:dims_out(1):3,2,:,:)  &
                              - data2(1:dims_out(1):3,3,:,:)  )
                   data2(1:dims_out(1):3,dims_out(2),:,:) = ( 2.0*data2(1:dims_out(1):3,dims_out(2)-1,:,:) &
                              - data2(1:dims_out(1):3,dims_out(2)-2,:,:)  )
                   data2(2:dims_out(1):3,3:dims_out(2):3,:,:) = &
                               ( data2(1:dims_out(1):3,3:dims_out(2):3,:,:) * 2.0 &
                               + data2(4:dims_out(1):3,3:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   data2(3:dims_out(1):3,3:dims_out(2):3,:,:) = &
                               ( data2(4:dims_out(1):3,3:dims_out(2):3,:,:) * 2.0 &
                               + data2(1:dims_out(1):3,3:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   data2(2:dims_out(1):3,1:dims_out(2):3,:,:) = &
                               ( data2(1:dims_out(1):3,1:dims_out(2):3,:,:) * 2.0 &
                               + data2(4:dims_out(1):3,1:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   data2(3:dims_out(1):3,1:dims_out(2):3,:,:) = &
                               ( data2(4:dims_out(1):3,1:dims_out(2):3,:,:) * 2.0 &
                               + data2(1:dims_out(1):3,1:dims_out(2):3,:,:) * 1.0 ) / 3.0
              elseif ( trim(cval) == 'V' .or. trim(cval) == 'MAPFAC_V' .or.  &
                   trim(cval) == 'XLAT_V' .or. trim(cval) == 'XLONG_V' ) then
                   data2(2:dims_out(1):3,1:dims_out(2):3,:,:) = data
                   do jj = 2 , dims_out(2) , 3
                      data2(2:dims_out(1):3,jj,:,:) = ( data2(2:dims_out(1):3,jj-1,:,:) &
                              * 2.0 + data2(2:dims_out(1):3,jj+2,:,:) * 1.0 ) / 3.0
                   enddo
                   do jj = 3 , dims_out(2) , 3
                      data2(2:dims_out(1):3,jj,:,:) = ( data2(2:dims_out(1):3,jj+1,:,:) &
                              * 2.0 + data2(2:dims_out(1):3,jj-2,:,:) * 1.0 ) / 3.0
                   enddo
                   do ii = 3 , dims_out(1) , 3
                      data2(ii,1:dims_out(2):3,:,:) = ( data2(ii-1,1:dims_out(2):3,:,:) &
                              * 2.0 + data2(ii+2,1:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   enddo
                   do ii = 4 , dims_out(1) , 3
                      data2(ii,1:dims_out(2):3,:,:) = ( data2(ii+1,1:dims_out(2):3,:,:) &
                              * 2.0 + data2(ii-2,1:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   enddo
                   data2(1,1:dims_out(2):3,:,:) = ( 2.0*data2(2,1:dims_out(2):3,:,:) & 
                              - data2(3,1:dims_out(2):3,:,:)  )
                   data2(dims_out(1),1:dims_out(2):3,:,:) = ( 2.0*data2(dims_out(1)-1,1:dims_out(2):3,:,:)  &
                              - data2(dims_out(1)-2,1:dims_out(2):3,:,:)  )
                   data2(3:dims_out(1):3,2:dims_out(2):3,:,:) = &
                               ( data2(3:dims_out(1):3,1:dims_out(2):3,:,:) * 2.0 &
                               + data2(3:dims_out(1):3,4:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   data2(3:dims_out(1):3,3:dims_out(2):3,:,:) = &
                               ( data2(3:dims_out(1):3,4:dims_out(2):3,:,:) * 2.0 &
                               + data2(3:dims_out(1):3,1:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   data2(1:dims_out(1):3,2:dims_out(2):3,:,:) = &
                               ( data2(1:dims_out(1):3,1:dims_out(2):3,:,:) * 2.0 &
                               + data2(1:dims_out(1):3,4:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   data2(1:dims_out(1):3,3:dims_out(2):3,:,:) = &
                               ( data2(1:dims_out(1):3,4:dims_out(2):3,:,:) * 2.0 &
                               + data2(1:dims_out(1):3,1:dims_out(2):3,:,:) * 1.0 ) / 3.0
              else
                   data2(2:dims_out(1):3,2:dims_out(2):3,:,:) = data
                   do jj = 3 , dims_out(2) , 3
                      data2(2:dims_out(1):3,jj,:,:) = ( data2(2:dims_out(1):3,jj-1,:,:) &
                              * 2.0 + data2(2:dims_out(1):3,jj+2,:,:) * 1.0 ) / 3.0
                   enddo
                   do jj = 4 , dims_out(2) , 3
                      data2(2:dims_out(1):3,jj,:,:) = ( data2(2:dims_out(1):3,jj+1,:,:) &
                              * 2.0 + data2(2:dims_out(1):3,jj-2,:,:) * 1.0 ) / 3.0
                   enddo
                   do ii = 3 , dims_out(1) , 3
                      data2(ii,2:dims_out(2):3,:,:) = ( data2(ii-1,2:dims_out(2):3,:,:) &
                              * 2.0 + data2(ii+2,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   enddo
                   do ii = 4 , dims_out(1) , 3
                      data2(ii,2:dims_out(2):3,:,:) = ( data2(ii+1,2:dims_out(2):3,:,:) &
                              * 2.0 + data2(ii-2,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   enddo
                   data2(1,2:dims_out(2):3,:,:) = ( 2.0*data2(2,2:dims_out(2):3,:,:)  &
                              - data2(3,2:dims_out(2):3,:,:)  )
                   data2(dims_out(1),2:dims_out(2):3,:,:) = ( 2.0*data2(dims_out(1)-1,2:dims_out(2):3,:,:)  &
                              - data2(dims_out(1)-2,2:dims_out(2):3,:,:)  )
                   data2(2:dims_out(1):3,1,:,:) = ( 2.0*data2(2:dims_out(1):3,2,:,:)  &
                              - data2(2:dims_out(1):3,3,:,:)  )
                   data2(2:dims_out(1):3,dims_out(2),:,:) = ( 2.0*data2(2:dims_out(1):3,dims_out(2)-1,:,:)  &
                              - data2(2:dims_out(1):3,dims_out(2)-2,:,:)  )
                   data2(1:dims_out(1):3,3:dims_out(2):3,:,:) = &
                               ( data2(1:dims_out(1):3,2:dims_out(2):3,:,:) * 2.0 &
                               + data2(1:dims_out(1):3,5:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   data2(1:dims_out(1):3,4:dims_out(2):3,:,:) = &
                               ( data2(1:dims_out(1):3,5:dims_out(2):3,:,:) * 2.0 &
                               + data2(1:dims_out(1):3,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   data2(3:dims_out(1):3,3:dims_out(2):3,:,:) = &
                               ( data2(3:dims_out(1):3,2:dims_out(2):3,:,:) * 2.0 &
                               + data2(3:dims_out(1):3,5:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   data2(3:dims_out(1):3,4:dims_out(2):3,:,:) = &
                               ( data2(3:dims_out(1):3,5:dims_out(2):3,:,:) * 2.0 &
                               + data2(3:dims_out(1):3,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   data2(3:dims_out(1):3,1,:,:) = &
                               ( data2(2:dims_out(1):3,1,:,:) * 2.0 &
                               + data2(5:dims_out(1):3,1,:,:) * 1.0 ) / 3.0
                   data2(4:dims_out(1):3,1,:,:) = &
                               ( data2(5:dims_out(1):3,1,:,:) * 2.0 &
                               + data2(2:dims_out(1):3,1,:,:) * 1.0 ) / 3.0
                   data2(3:dims_out(1):3,dims_out(2),:,:) = &
                               ( data2(2:dims_out(1):3,dims_out(2),:,:) * 2.0 &
                               + data2(5:dims_out(1):3,dims_out(2),:,:) * 1.0 ) / 3.0
                   data2(4:dims_out(1):3,dims_out(2),:,:) = &
                               ( data2(5:dims_out(1):3,dims_out(2),:,:) * 2.0 &
                               + data2(2:dims_out(1):3,dims_out(2),:,:) * 1.0 ) / 3.0
                   data2(1,1,:,:) = ( 2.0*data2(2,1,:,:) - data2(3,1,:,:)  )
                   data2(dims_out(1),1,:,:) = ( 2.0*data2(dims_out(1)-1,1,:,:) - data2(dims_out(1)-2,1,:,:)  )
                   data2(1,dims_out(2),:,:) = ( 2.0*data2(1,dims_out(2)-1,:,:) - data2(1,dims_out(2)-2,:,:)  )
                   data2(dims_out(1),dims_out(2),:,:) = ( 2.0*data2(dims_out(1)-1,dims_out(2),:,:) - data2(dims_out(1)-2,dims_out(2),:,:)  )
                   if ( trim(cval) .eq. 'PHB' .or. trim(cval) .eq. 'SNOALB') where ( data2 .lt. 0.0 ) data2 = 0.0
                   if ( trim(cval) .eq. 'SMOIS' .or. trim(cval) .eq. 'SH2O') then 
                        where ( data2 .lt. 0.0 ) data2 = 0.0
                        where ( data2 .gt. 1.0 ) data2 = 1.0
                   endif
                   if ( trim(cval) .eq. 'SHDMAX' .or. trim(cval) .eq. 'VEGFRA' ) then 
                        where ( data2 .lt. 0.0 ) data2 = 0.0
                        where ( data2 .gt. 100.0 ) data2 = 1.0
                   endif
                   if ( trim(cval) .eq. 'LANDMASK'  ) then 
                        data2 = INT (data2+0.5)
                        where ( data2 .lt. 0.0 ) data2 = 0.0
                        where ( data2 .gt. 1.0 ) data2 = 1.0
                   endif
                   if ( trim(cval) .eq. 'XLAND'  ) then 
                        data2 = INT (data2+0.5)
                        where ( data2 .lt. 1.0 ) data2 = 1.0
                        where ( data2 .gt. 2.0 ) data2 = 2.0
                   endif
                   if ( trim(cval) .eq. 'LU_INDEX'  ) then 
                        data2 = INT (data2+0.5)
                        where ( data2 .lt. 1.0 ) data2 = 1.0
                        where ( data2 .gt. 19.0 ) data2 = 19.0
                   endif
              endif
	    else
	      data2 = data
	    endif
          ENDIF
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
          IF ( option == '-thin' ) THEN 
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
          ELSEIF ( option == '-down' ) THEN 
            if (idm .ge. 3) then
              IF (debug) write(6,*) '   Grid is nestdowned with a ratio of ',int(ratio)
              if ( trim(cval) == 'U' .or. trim(cval) == 'MAPFAC_U' .or.  &
                   trim(cval) == 'XLAT_U' .or. trim(cval) == 'XLONG_U' ) then
                   ddata2(1:dims_out(1):3,2:dims_out(2):3,:,:) = ddata
                   do ii = 2 , dims_out(1) , 3
                      ddata2(ii,2:dims_out(2):3,:,:) = ( ddata2(ii-1,2:dims_out(2):3,:,:) &
                              * 2.0 + ddata2(ii+2,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   enddo
                   do ii = 3 , dims_out(1) , 3
                      ddata2(ii,2:dims_out(2):3,:,:) = ( ddata2(ii+1,2:dims_out(2):3,:,:) &
                              * 2.0 + ddata2(ii-2,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   enddo
                   do jj = 3 , dims_out(2) , 3
                      ddata2(1:dims_out(1):3,jj,:,:) = ( ddata2(1:dims_out(1):3,jj-1,:,:) &
                              * 2.0 + ddata2(1:dims_out(1):3,jj+2,:,:) * 1.0 ) / 3.0
                   enddo
                   do jj = 4 , dims_out(2) , 3
                      ddata2(1:dims_out(1):3,jj,:,:) = ( ddata2(1:dims_out(1):3,jj+1,:,:) &
                              * 2.0 + ddata2(1:dims_out(1):3,jj-2,:,:) * 1.0 ) / 3.0
                   enddo
                   ddata2(1:dims_out(1):3,1,:,:) = ( 2.0*ddata2(1:dims_out(1):3,2,:,:)  &
                              - ddata2(1:dims_out(1):3,3,:,:)  )
                   ddata2(1:dims_out(1):3,dims_out(2),:,:) = ( 2.0*ddata2(1:dims_out(1):3,dims_out(2)-1,:,:) &
                              - ddata2(1:dims_out(1):3,dims_out(2)-2,:,:)  )
                   ddata2(2:dims_out(1):3,3:dims_out(2):3,:,:) = &
                               ( ddata2(1:dims_out(1):3,3:dims_out(2):3,:,:) * 2.0 &
                               + ddata2(4:dims_out(1):3,3:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   ddata2(3:dims_out(1):3,3:dims_out(2):3,:,:) = &
                               ( ddata2(4:dims_out(1):3,3:dims_out(2):3,:,:) * 2.0 &
                               + ddata2(1:dims_out(1):3,3:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   ddata2(2:dims_out(1):3,1:dims_out(2):3,:,:) = &
                               ( ddata2(1:dims_out(1):3,1:dims_out(2):3,:,:) * 2.0 &
                               + ddata2(4:dims_out(1):3,1:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   ddata2(3:dims_out(1):3,1:dims_out(2):3,:,:) = &
                               ( ddata2(4:dims_out(1):3,1:dims_out(2):3,:,:) * 2.0 &
                               + ddata2(1:dims_out(1):3,1:dims_out(2):3,:,:) * 1.0 ) / 3.0
              elseif ( trim(cval) == 'V' .or. trim(cval) == 'MAPFAC_V' .or.  &
                   trim(cval) == 'XLAT_V' .or. trim(cval) == 'XLONG_V' ) then
                   ddata2(2:dims_out(1):3,1:dims_out(2):3,:,:) = data
                   do jj = 2 , dims_out(2) , 3
                      ddata2(2:dims_out(1):3,jj,:,:) = ( ddata2(2:dims_out(1):3,jj-1,:,:) &
                              * 2.0 + ddata2(2:dims_out(1):3,jj+2,:,:) * 1.0 ) / 3.0
                   enddo
                   do jj = 3 , dims_out(2) , 3
                      ddata2(2:dims_out(1):3,jj,:,:) = ( ddata2(2:dims_out(1):3,jj+1,:,:) &
                              * 2.0 + ddata2(2:dims_out(1):3,jj-2,:,:) * 1.0 ) / 3.0
                   enddo
                   do ii = 3 , dims_out(1) , 3
                      ddata2(ii,1:dims_out(2):3,:,:) = ( ddata2(ii-1,1:dims_out(2):3,:,:) &
                              * 2.0 + ddata2(ii+2,1:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   enddo
                   do ii = 4 , dims_out(1) , 3
                      ddata2(ii,1:dims_out(2):3,:,:) = ( ddata2(ii+1,1:dims_out(2):3,:,:) &
                              * 2.0 + ddata2(ii-2,1:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   enddo
                   ddata2(1,1:dims_out(2):3,:,:) = ( 2.0*ddata2(2,1:dims_out(2):3,:,:) & 
                              - ddata2(3,1:dims_out(2):3,:,:)  )
                   ddata2(dims_out(1),1:dims_out(2):3,:,:) = ( 2.0*ddata2(dims_out(1)-1,1:dims_out(2):3,:,:)  &
                              - ddata2(dims_out(1)-2,1:dims_out(2):3,:,:)  )
                   ddata2(3:dims_out(1):3,2:dims_out(2):3,:,:) = &
                               ( ddata2(3:dims_out(1):3,1:dims_out(2):3,:,:) * 2.0 &
                               + ddata2(3:dims_out(1):3,4:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   ddata2(3:dims_out(1):3,3:dims_out(2):3,:,:) = &
                               ( ddata2(3:dims_out(1):3,4:dims_out(2):3,:,:) * 2.0 &
                               + ddata2(3:dims_out(1):3,1:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   ddata2(1:dims_out(1):3,2:dims_out(2):3,:,:) = &
                               ( ddata2(1:dims_out(1):3,1:dims_out(2):3,:,:) * 2.0 &
                               + ddata2(1:dims_out(1):3,4:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   ddata2(1:dims_out(1):3,3:dims_out(2):3,:,:) = &
                               ( ddata2(1:dims_out(1):3,4:dims_out(2):3,:,:) * 2.0 &
                               + ddata2(1:dims_out(1):3,1:dims_out(2):3,:,:) * 1.0 ) / 3.0
              else
                   ddata2(2:dims_out(1):3,2:dims_out(2):3,:,:) = data
                   do jj = 3 , dims_out(2) , 3
                      ddata2(2:dims_out(1):3,jj,:,:) = ( ddata2(2:dims_out(1):3,jj-1,:,:) &
                              * 2.0 + ddata2(2:dims_out(1):3,jj+2,:,:) * 1.0 ) / 3.0
                   enddo
                   do jj = 4 , dims_out(2) , 3
                      ddata2(2:dims_out(1):3,jj,:,:) = ( ddata2(2:dims_out(1):3,jj+1,:,:) &
                              * 2.0 + ddata2(2:dims_out(1):3,jj-2,:,:) * 1.0 ) / 3.0
                   enddo
                   do ii = 3 , dims_out(1) , 3
                      ddata2(ii,2:dims_out(2):3,:,:) = ( ddata2(ii-1,2:dims_out(2):3,:,:) &
                              * 2.0 + ddata2(ii+2,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   enddo
                   do ii = 4 , dims_out(1) , 3
                      ddata2(ii,2:dims_out(2):3,:,:) = ( ddata2(ii+1,2:dims_out(2):3,:,:) &
                              * 2.0 + ddata2(ii-2,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   enddo
                   ddata2(1,2:dims_out(2):3,:,:) = ( 2.0*ddata2(2,2:dims_out(2):3,:,:)  &
                              - ddata2(3,2:dims_out(2):3,:,:)  )
                   ddata2(dims_out(1),2:dims_out(2):3,:,:) = ( 2.0*ddata2(dims_out(1)-1,2:dims_out(2):3,:,:)  &
                              - ddata2(dims_out(1)-2,2:dims_out(2):3,:,:)  )
                   ddata2(2:dims_out(1):3,1,:,:) = ( 2.0*ddata2(2:dims_out(1):3,2,:,:)  &
                              - ddata2(2:dims_out(1):3,3,:,:)  )
                   ddata2(2:dims_out(1):3,dims_out(2),:,:) = ( 2.0*ddata2(2:dims_out(1):3,dims_out(2)-1,:,:)  &
                              - ddata2(2:dims_out(1):3,dims_out(2)-2,:,:)  )
                   ddata2(1:dims_out(1):3,3:dims_out(2):3,:,:) = &
                               ( ddata2(1:dims_out(1):3,2:dims_out(2):3,:,:) * 2.0 &
                               + ddata2(1:dims_out(1):3,5:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   ddata2(1:dims_out(1):3,4:dims_out(2):3,:,:) = &
                               ( ddata2(1:dims_out(1):3,5:dims_out(2):3,:,:) * 2.0 &
                               + ddata2(1:dims_out(1):3,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   ddata2(3:dims_out(1):3,3:dims_out(2):3,:,:) = &
                               ( ddata2(3:dims_out(1):3,2:dims_out(2):3,:,:) * 2.0 &
                               + ddata2(3:dims_out(1):3,5:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   ddata2(3:dims_out(1):3,4:dims_out(2):3,:,:) = &
                               ( ddata2(3:dims_out(1):3,5:dims_out(2):3,:,:) * 2.0 &
                               + ddata2(3:dims_out(1):3,2:dims_out(2):3,:,:) * 1.0 ) / 3.0
                   ddata2(3:dims_out(1):3,1,:,:) = &
                               ( ddata2(2:dims_out(1):3,1,:,:) * 2.0 &
                               + ddata2(5:dims_out(1):3,1,:,:) * 1.0 ) / 3.0
                   ddata2(4:dims_out(1):3,1,:,:) = &
                               ( ddata2(5:dims_out(1):3,1,:,:) * 2.0 &
                               + ddata2(2:dims_out(1):3,1,:,:) * 1.0 ) / 3.0
                   ddata2(3:dims_out(1):3,dims_out(2),:,:) = &
                               ( ddata2(2:dims_out(1):3,dims_out(2),:,:) * 2.0 &
                               + ddata2(5:dims_out(1):3,dims_out(2),:,:) * 1.0 ) / 3.0
                   ddata2(4:dims_out(1):3,dims_out(2),:,:) = &
                               ( ddata2(5:dims_out(1):3,dims_out(2),:,:) * 2.0 &
                               + ddata2(2:dims_out(1):3,dims_out(2),:,:) * 1.0 ) / 3.0
                   ddata2(1,1,:,:) = ( 2.0*ddata2(2,1,:,:) - ddata2(3,1,:,:)  )
                   ddata2(dims_out(1),1,:,:) = ( 2.0*ddata2(dims_out(1)-1,1,:,:) - ddata2(dims_out(1)-2,1,:,:)  )
                   ddata2(1,dims_out(2),:,:) = ( 2.0*ddata2(1,dims_out(2)-1,:,:) - ddata2(1,dims_out(2)-2,:,:)  )
                   ddata2(dims_out(1),dims_out(2),:,:) = ( 2.0*ddata2(dims_out(1)-1,dims_out(2),:,:) - ddata2(dims_out(1)-2,dims_out(2),:,:)  )
              endif
	    else
	      ddata2 = ddata
	    endif
          ENDIF
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

      end program nupdown
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
          CASE ("-down")
               option = dummy
               i = i+1
               call getarg(i,dummy)
               read(dummy,'(i3)')idummy
               iratio = idummy
               if ( iratio .lt. 2 ) STOP ' Must supply a ratio of 2 or more ' 
               ratio = real(iratio)
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
  print*," nupdown   wrf_data_file_name  [-options] "
  print*," "
  print*," Current options available are:"
  print*," -help     : Print this information"                           
  print*," -h        : Print this information"                           
  print*," "
  print*," -down x  : Nestdown the input grid, with a ration of x"  
  print*,"             Example:"
  print*,"                -down 3 " 
  print*,"             Will nestdown the grid with a ratio of 3, i.e.,   "
  print*,"             a 36km grid will be downscaled to a 12km grid" 
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

!------------------------------------------------------------------------------

