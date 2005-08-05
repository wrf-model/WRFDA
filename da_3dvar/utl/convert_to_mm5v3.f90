program convert_to_mm5v3

! ==================================================================== !
! This reading program can be used to convert the wrf3dvar increment   !
! output to the MM5V3 output for plotting with MM5/GRAPH.              !
!                                                                      !
! Usage:  convert_to_mm5v3  ANALYSIS_INCREMENT                         !
!                                                                      !
!  input files required: 1) ANALYSIS_INCREMENT                         !
!                                                                      !
!                           This file is from wrf3dvar run by setting  !
!                           WRITE_INCREMENTS = .TRUE.                  !
!                                                                      !
!                           This file contains the 3-D increment       !
!                           fields: u, v, w, p, t, q, and 2-D increment!
!                           fields: mu. psfc at C-grid with the 8-byte !
!                           REAL number. The necessary information,    !
!                           such as map background, domain definition, !
!                           and 2-D constant fields are also contained.!
!                                                                      !
!                        2) MMINPUT_template                           !
!                                                                      !
!                           This is any of MM5V3 INPUT files, not need !
!                           to have the same date, dimensions, etc.,   !
!                           this is just used to provides some of the  !
!                           text description in MM5V3 INPUT file.      !
!                                                                      !
!  output file         : MMINPUT_ANALYSIS_INCREMENT                    !
!                                                                      !   
!                        This is the standard MM5V3 format file being  !
!                        used by MM5/GRAPH for plotting.               !
!                                                                      !
!                        For 3-D increment fields, they are stored in  !
!                        the same variable names, but the psfc         !
!                        increment has the name 'GROUND T' and the mu  !
!                        increment has the name 'TSEASFC'. So when     !
!                        using GRAPH, user should use 'TGD' psfc and   !
!                        'TSEASFC' for mu in g_plots.tbl.              !
!                                                                      !
!                                  Yong-Run Guo  08/03/2004            !
! ==================================================================== !
!                                                                      !
! This utility program is written in free-format Fortran 90.           !
!   It requires a Fortran 90 compiler to compile. On a DEC_Alpha       ! 
!   machine, type the following to compile:                            !
!                                                                      !
!   f90 -free -convert big_endian read_write_v3.f90 -o read_write_v3   !
! ---------------------------------------------------------------------!
  implicit none
  integer, dimension(50,20) :: bhi
  real, dimension(20,20) :: bhr
  character(len=80), dimension(50,20) :: bhic
  character(len=80), dimension(20,20) :: bhrc
  character(len=120) :: flnm, new_flnm
  integer :: iunit = 10, iunit1 = 11, iunit2 = 12

  integer :: flag

  integer :: ndim

  integer, dimension(4) :: start_index, end_index
  character (len= 4) :: staggering
  character (len= 4) :: ordering
  character (len=24) :: start_date
  character (len=24) :: current_date
  character (len= 9) :: name
  character (len=25) :: units
  character (len=46) :: description

  real               :: time, sample

  integer :: l, k, i, j, ii, jj, kk

  real, allocatable, dimension(:,:,:,:) :: data, data1

  integer :: ierr, ier

  logical :: newtime = .TRUE.
  logical :: lmore, first_3d, first_2d
!
! wrf3dvar increment data:

 CHARACTER(LEN=24) :: ANALYSIS_DATE ! Analysis time, "CCYY-MM-DD_HH:MM:SS.ssss"

 integer           :: yy,mm,dd,hh,mn,sc
!
   INTEGER            :: map_projection  ! 1=LamConf/2=PolarSte/3=Mercator
   REAL (kind=8)      :: ycntr
   INTEGER            :: coarse_ix       ! COARSE DOMAIN DIM IN I DIRECTION.
   INTEGER            :: coarse_jy       ! COARSE DOMAIN DIM IN Y DIRECTION.
   REAL (kind=8)      :: coarse_ds       ! Coarse domain gridlength (km)
   REAL (kind=8)      :: start_x         ! i posn. of (1,1) in coarse domain.
   REAL (kind=8)      :: start_y         ! j posn. of (1,1) in coarse domain.

   REAL (kind=8)      :: phic            ! COARSE DOMAIN CENTRAL LAT(DEGREE)
   REAL (kind=8)      :: xlonc           ! COARSE DOMAIN CENTRAL LON(DEGREE)
   REAL (kind=8)      :: cone_factor     ! Cone Factor
   REAL (kind=8)      :: truelat1_3dv    ! True latitude 1 (degrees)
   REAL (kind=8)      :: truelat2_3dv    ! True latitude 2 (degrees)
   REAL (kind=8)      :: pole            ! Pole latitude (degrees)
   REAL (kind=8)      :: dsm             ! Current domain gridlength (km)
   REAL (kind=8)      :: psi1            ! ?
   REAL (kind=8)      :: c2              ! earth_radius * COS(psi1)

   REAL (kind=8)      :: ptop
   REAL (kind=8)      :: ps0
   REAL (kind=8)      :: ts0
   REAL (kind=8)      :: tlp
   REAL (kind=8)      :: tis0

   integer            :: is, ie, js, je, ks, ke

   real(kind=8), allocatable, dimension(:,:,:) :: ua, va, u, v, w, t, p, qv, ph 
   real(kind=8), allocatable, dimension(:,:)   :: mu, terr, snow, lat, lon, &
                                                  lanu, msft, cori, xland,  &
                                                  tgrn, psac, psfc
   real(kind=8), allocatable, dimension(:)     :: sigma

! ...........................................................................

  first_3d = .true.
  first_2d = .true.

  call arguments(flnm, lmore)

! Output file in MM5V3 format:

    write(*,'(/"wrf3dvar_increment_file = ",a)') flnm
    new_flnm = 'MMINPUT_'//trim(flnm)
    open(iunit1, file=new_flnm, form='unformatted')

! wrf3dvar increment file (binary):

  open(iunit2, file=trim(flnm), form='unformatted', &
                                status='old', action='read')
      read (iunit2) ANALYSIS_DATE
      read (iunit2) is, ie, js, je, ks, ke
      read (iunit2) &
        map_projection, coarse_ix, coarse_jy, coarse_ds, start_x, start_y, &
        phic, xlonc, cone_factor, truelat1_3dv, truelat2_3dv, pole, dsm,   &
        psi1, c2, ptop, ps0, ts0, tlp, tis0
      
      print '(a,a)', "ANALYSIS_DATE =",ANALYSIS_DATE
      print '(a,6i4)',"is, ie, js, je, ks, ke:", is, ie, js, je, ks, ke
      print '(a,i2,1x,2i4,f10.2,1x,2f7.2,1x,2f10.3)', &
       "map_proj,coarse_ix,coarse_jy,coarse_ds,start_x,start_y,phic,xlonc:", &
        map_projection, coarse_ix, coarse_jy, coarse_ds, start_x, start_y, &
        phic, xlonc
      print '(a,7f12.3)', &
        "cone_factor,truelat1,truelat2,pole,dsm,psi1,c2:",     &
         cone_factor, truelat1_3dv, truelat2_3dv, pole, dsm,   &
         psi1, c2
      print '(a,5f12.2)', "ptop, ps0, ts0, tlp, tis0:", &
         ptop,ps0,ts0,tlp,tis0

      print *,'ie+1,je+1,ke:', ie+1,je+1,ke
              allocate(ua(ie+1,je+1,ke+1))
              allocate(va(ie+1,je+1,ke+1))
              allocate(u (ie+1,je+1,ke+1))
              allocate(v (ie+1,je+1,ke+1))
              allocate(w (ie+1,je+1,ke+1))
              allocate(t (ie+1,je+1,ke+1))
              allocate(p (ie+1,je+1,ke+1))
              allocate(qv(ie+1,je+1,ke+1))
              allocate(ph(ie+1,je+1,ke+1))
              allocate(mu   (ie+1,je+1))
              allocate(psfc (ie+1,je+1))

              allocate(psac (ie+1,je+1))
              allocate(cori (ie+1,je+1))
              allocate(terr (ie+1,je+1))
              allocate(snow (ie+1,je+1))
              allocate(tgrn (ie+1,je+1))
              allocate(lat  (ie+1,je+1))
              allocate(lon  (ie+1,je+1))
              allocate(msft (ie+1,je+1))
              allocate(lanu (ie+1,je+1))
              allocate(xland(ie+1,je+1))

              allocate(sigma(ke))

      read (iunit2) sigma
      print '("k=",i3," sigma=",f8.5)', (k,sigma(k),k=ks,ke)

      read (iunit2) ua, va, w, p, t, qv, ph, mu, psfc

      print '(/"MAXVAL:: ua, va, w, p, t, qv, ph, mu, psfc:",9e13.5)', &
         maxval(ua),maxval(va),maxval(w ),maxval(p ), &
         maxval(t ),maxval(qv),maxval(ph),maxval(mu),maxval(psfc)
      print '(/"MINVAL:: ua, va, w, p, t, qv, ph, mu, psfc:",9e13.5)', &
         minval(ua),minval(va),minval(w ),minval(p ), &
         minval(t ),minval(qv),minval(ph),minval(mu),minval(psfc)

      do k = ks, ke+1
         print '("k=",i3," Max ph=",e13.5," Min ph=",e13.5)',k, maxval(ph(:,:,k)), minval(ph(:,:,k))
      enddo
     
      read (iunit2) psac, tgrn, terr, snow, lat, lon, &
                    lanu, msft, cori, xland

     print '(/"MAXVAL:: psac,tgrn,terr,snow,lat,lon,lanu,msft,cori,xland:",(5e13.5/))',&
               maxval(psac),maxval(tgrn),maxval(terr),maxval(snow),maxval(lat),   &
               maxval(lon),maxval(lanu),maxval(msft),maxval(cori),maxval(xland)
     print '(/"MINVAL:: psac,tgrn,terr,snow,lat,lon,lanu,msft,cori,xland:",(5e13.5/))',&
               minval(psac),minval(tgrn),minval(terr),minval(snow),minval(lat),   &
               minval(lon),minval(lanu),minval(msft),minval(cori),minval(xland)

! A-grid to B-grid:
      
      do k = ks, ke
        do i = is+1, ie
        do j = js+1, je
          u (i,j,k) = 0.25 * (ua(i-1,j-1,k) + ua(i  ,j-1,k) + &
                              ua(i  ,j  ,k) + ua(i-1,  j,k))
          v (i,j,k) = 0.25 * (va(i-1,j-1,k) + va(i  ,j-1,k) + &
                              va(i  ,j  ,k) + va(i-1,  j,k))
        enddo
        enddo
      enddo
  
! MMINPUT template:

  open(iunit, file='MMINPUT_template', form='unformatted', &
                                status='old', action='read')



  read(iunit, iostat=ierr) flag
  write(iunit1) flag

  do while (ierr == 0)

     if (flag == 0) then
        read(iunit,iostat=ier) bhi, bhr, bhic, bhrc
        if(ier/=0) then
           write(*,'("Error reading big header")')
           call abort()
        endif

! Modify the big_header:

        bhi( 5,1) = coarse_jy
        bhi( 6,1) = coarse_ix
        bhi( 7,1) = map_projection
        bhi( 8,1) = 0
        bhi( 9,1) = coarse_jy
        bhi(10,1) = coarse_ix
        bhi(11,1) = 0
        bhi(12,1) = 0

        if (start_y==1.0 .and. start_x==1.0) then
          bhi(13,1) = 1
          bhi(15,1) = 0
        else
          bhi(13,1) = 2
          bhi(15,1) = 1
        endif

        bhi(14,1) = 1
        bhi(16,1) = je+1
        bhi(17,1) = ie+1
        bhi(18,1) = int(start_y)
        bhi(19,1) = int(start_x)
        bhi(20,1) = 1
        bhi(21,1) = 1

        bhr( 1,1) = coarse_ds * 1000.
        bhr( 2,1) = phic
        bhr( 3,1) = xlonc
        bhr( 4,1) = cone_factor
        bhr( 5,1) = truelat1_3dv
        bhr( 6,1) = truelat2_3dv
        bhr( 7,1) = pole
        bhr( 8,1) = 0.0
        bhr( 9,1) = dsm * 1000.
        bhr(10,1) = start_y
        bhr(11,1) = start_x
        bhr(12,1) = start_y + je
        bhr(13,1) = start_x + ie
   
        read(ANALYSIS_DATE,'(i4,1x,5(i2,1x))') yy,mm,dd,hh,mn,sc

        bhi( 5,2) = yy
        bhi( 6,2) = mm
        bhi( 7,2) = dd
        bhi( 8,2) = hh
        bhi( 9,2) = mn
        bhi(10,2) = sc
        bhi(11,2) = 0000

        bhr( 2,2) = ptop

        bhi( 5,5) = yy
        bhi( 6,5) = mm
        bhi( 7,5) = dd
        bhi( 8,5) = hh
        bhi( 9,5) = mn
        bhi(10,5) = sc
        bhi(11,5) = 0000
        bhi(12,5) = ke

        bhr( 2,5) = ps0
        bhr( 3,5) = ts0
        bhr( 4,5) = 50.0
        bhr( 5,5) = tis0

        call printout_big_header(bhi, bhr, bhic, bhrc)
        write(iunit1) bhi, bhr, bhic, bhrc

     elseif (flag == 1) then

        READ (iunit,iostat=ier) ndim, start_index, end_index, time, staggering, ordering,&
             current_date, name, units, description

        if(ier/=0) then
           write(*,'("Error reading subheader")')
           call abort()
        endif

        if (lmore) then
           print*, 'ndim: ', ndim
           print*, 'start_index: ', start_index
           print*, 'end_index: ', end_index
           print*, 'time: ', time
           print*, 'staggering: #'//staggering//'#'
           print*, 'ordering: #'//ordering//'#'
           print*, 'date/time: #'//current_date//'#'
           print*, 'name: #'//name//'#'
           print*, 'units: #'//units//'#'
           print*, 'description: #'//description//'#'
        endif

        if (newtime) then
           write(*,'(/,A,2x, F15.5," Hours"/)') current_date, time/60.
           newtime = .FALSE.
        endif

        if (ndim == 1) then
           allocate(data(end_index(1), 1, 1, 1))
        elseif (ndim == 2) then
           allocate(data(end_index(1), end_index(2), 1, 1))
        elseif (ndim == 3) then
           allocate(data(end_index(1), end_index(2), end_index(3), 1))
        endif

        read(iunit) data
         write(*,'("===> WRITE data of name=",a)') name

        IF ( name == 'U        ' .OR. name == 'V        ' .OR. &
             name == 'T        ' .OR. name == 'W        ' .OR. &
             name == 'Q        ' .OR. name == 'PP       ' .OR. &
             name == 'TERRAIN  ' .OR. name == 'LATITCRS ' .OR. &
             name == 'LONGICRS ' .OR. name == 'MAPFACCR ' .OR. &
             name == 'LAND USE ' .OR. name == 'GROUND T ' .OR. &
             name == 'SNOWCOVR ' .OR. name == 'LANDMASK ' .OR. &
             name == 'PSTARCRS ' .OR. name == 'CORIOLIS ' .OR. &
             name == 'LATITDOT ' .OR. name == 'LONGIDOT ' .OR. &
             name == 'TSEASFC  ' .OR. name == 'SIGMAH   ' ) THEN
 
        current_date = ANALYSIS_DATE

        if (ndim == 1) then
           end_index(1) = ke
           allocate(data1(end_index(1), 1, 1, 1))
        elseif (ndim == 2) then
           end_index(1) = je+1
           end_index(2) = ie+1
           allocate(data1(end_index(1), end_index(2), 1, 1))
        elseif (ndim == 3) then
           end_index(1) = je+1
           end_index(2) = ie+1
           end_index(3) = ke
           if ( name == 'W        ' ) end_index(3) = end_index(3) + 1
           allocate(data1(end_index(1), end_index(2), end_index(3), 1))
        endif

         write(*,'("===> WRITE flag=",i2)') flag
         write(iunit1) flag
         write(*,'("===> WRITE sub_head at time=",f10.2)') time
         WRITE (iunit1) ndim, start_index, end_index, time, &
                        staggering, ordering, current_date, name, &
                        units, description

        if (name == 'U        ') then
           data1 = 0.0
           do k = 1, end_index(3)
           do i = 1, end_index(1)
           do j = 1, end_index(2)
           data1(i,j,k,1) = U(j,i,end_index(3)+1-k)
           enddo
           enddo
           enddo
        else if (name == 'V        ') then
           data1 = 0.0
           do k = 1, end_index(3)
           do i = 1, end_index(1)
           do j = 1, end_index(2)
           data1(i,j,k,1) = V(j,i,end_index(3)+1-k)
           enddo
           enddo
           enddo
        else if (name == 'W        ') then
           data1 = 0.0
           do k = 1, end_index(3)
           do i = 1, end_index(1)
           do j = 1, end_index(2)
           data1(i,j,k,1) = ph(j,i,end_index(3)+1-k)
           enddo
           enddo
           enddo
        else if (name == 'T        ') then
           data1 = 0.0
           do k = 1, end_index(3)
           do i = 1, end_index(1)
           do j = 1, end_index(2)
           data1(i,j,k,1) = T(j,i,end_index(3)+1-k)
           enddo
           enddo
           enddo
        else if (name == 'Q        ') then
           data1 = 0.0
           do k = 1, end_index(3)
           do i = 1, end_index(1)
           do j = 1, end_index(2)
           data1(i,j,k,1) = QV(j,i,end_index(3)+1-k)
           enddo
           enddo
           enddo
        else if (name == 'PP       ') then
           data1 = 0.0
           do k = 1, end_index(3)
           do i = 1, end_index(1)
           do j = 1, end_index(2)
           data1(i,j,k,1) = P(j,i,end_index(3)+1-k)
           enddo
           enddo
           enddo
! xa%psfc: increment of surface pressure:
        else if (name == 'GROUND T ') then
           data1 = 0.0
           do i = 1, end_index(1)-1
           do j = 1, end_index(2)-1
           data1(i,j,1,1) = PSFC(j,i)
           enddo
           enddo
! increment of MU (total dry mass in column):
        else if (name == 'TSEASFC  ') then
           data1 = 0.0
           do i = 1, end_index(1)-1
           do j = 1, end_index(2)-1
           data1(i,j,1,1) = MU  (j,i)
           enddo
           enddo
! Constant 2-D fileds:
        else if (name == 'PSTARCRS ') then
           data1 = 0.0
           do i = 1, end_index(1)-1
           do j = 1, end_index(2)-1
           data1(i,j,1,1) = PSAC(j,i) + mu(j,i)
           enddo
           enddo
        else if (name == 'TERRAIN  ') then
           data1 = 0.0
           do i = 1, end_index(1)-1
           do j = 1, end_index(2)-1
           data1(i,j,1,1) = TERR(j,i)
           enddo
           enddo
        else if (name == 'CORIOLIS ') then
           data1 = 0.0
           do i = 1, end_index(1)
           do j = 1, end_index(2)
           data1(i,j,1,1) = CORI(j,i)
           enddo
           enddo
        else if (name == 'LATITCRS ') then
           data1 = 0.0
           do i = 1, end_index(1)-1
           do j = 1, end_index(2)-1
           data1(i,j,1,1) = LAT (j,i)
           enddo
           enddo
        else if (name == 'LONGICRS ') then
           data1 = 0.0
           do i = 1, end_index(1)-1
           do j = 1, end_index(2)-1
           data1(i,j,1,1) = LON (j,i)
           enddo
           enddo
        else if (name == 'LATITDOT ') then
           data1 = 0.0
           do i = 2, end_index(1)-1
           do j = 2, end_index(2)-1
           data1(i,j,1,1) = 0.25 * (LAT (j  ,i) + LAT(j-1,i-1) + &
                                    LAT (j-1,i) + LAT(j  ,i-1))
           enddo
           enddo
!     .....west, east boundaries:
           do i = 2, end_index(1)-1
             data1(i,1,1,1) = 0.5 * (lat(1,i) + lat(1,i-1))
             data1(i, end_index(2),1,1) = 0.5 * &
                (lat(end_index(2)-1,i) + lat(end_index(2)-1,i-1))
           enddo
!     .....south, north boundaries: 
           do j = 2, end_index(2)-1
               data1(1,j,1,1) = 0.5 * (lat(j,1) + lat(j-1,1))
               data1(end_index(1),j,1,1) = 0.5 * &
                (lat(j,end_index(1)-1) + lat(j-1,end_index(1)-1))
           enddo

!      ... low-left corner:
               data1(1,1,1,1) = lat(1,1)
!      ... upper-left corner:
               data1(end_index(1),1,1,1) = lat(1,end_index(1)-1)
!      ... low-right corner:
               data1(1,end_index(2),1,1) = lat(end_index(2)-1,1)
!      ... upper-right corner:
               data1(end_index(1),end_index(2),1,1) = &
                              lat(end_index(2)-1,end_index(1)-1)
        else if (name == 'LONGIDOT ') then
           data1 = 0.0
           do i = 2, end_index(1)-1
           do j = 2, end_index(2)-1
           data1(i,j,1,1) = 0.25 * (LON (j  ,i) + LON(j-1,i-1) + &
                                    LON (j-1,i) + LON(j  ,i-1))
           enddo
           enddo
!    ..... south, north boundaries:
           do j = 2, end_index(2)-1
             data1(1,j,1,1) = 0.5 * (lon(j,1) + lon(j-1,1))
             data1(end_index(1),j,1,1) = 0.5 * &
              (lon(j,end_index(1)-1) + lon(j-1,end_index(1)-1))
           enddo
!    ..... west, east boundaries:
           do i = 2, end_index(1)-1
             data1(i,1,1,1) = 0.5 * (lon(1,i) + lon(1,i-1))
             data1(i, end_index(2),1,1) = 0.5 * &
              (lon(end_index(2)-1,i) + lon(end_index(2)-1,i-1))
           enddo
!      ... low-left corner:
               data1(1,1,1,1) = lon(1,1)
!      ... upper-left corner:
               data1(end_index(1),1,1,1) = lon(1,end_index(1)-1)
!      ... low-right corner:
               data1(1,end_index(2),1,1) = lon(end_index(2)-1,1)
!      ... upper-right corner:
               data1(end_index(1),end_index(2),1,1) = &
                              lon(end_index(2)-1,end_index(1)-1)
        else if (name == 'MAPFACCR ') then
           data1 = 0.0
           do i = 1, end_index(1)-1
           do j = 1, end_index(2)-1
           data1(i,j,1,1) = MSFT(j,i)
           enddo
           enddo
        else if (name == 'SNOWCOVR ') then
           data1 = 0.0
           do i = 1, end_index(1)-1
           do j = 1, end_index(2)-1
           data1(i,j,1,1) = SNOW(j,i)
           enddo
           enddo
        else if (name == 'LAND USE ') then
           data1 = 0.0
           do i = 1, end_index(1)-1
           do j = 1, end_index(2)-1
           data1(i,j,1,1) = LANU(j,i)
           enddo
           enddo
        else if (name == 'LANDMASK ') then
           data1 = 0.0
           do i = 1, end_index(1)-1
           do j = 1, end_index(2)-1
           data1(i,j,1,1) = XLAND(j,i)
           enddo
           enddo
        else if (name == 'SIGMAH   ') then
           data1 = 0.0
           do i = 1, end_index(1)
           data1(i,1,1,1) = sigma(end_index(1)+1-i)
           enddo
        endif

        write(iunit1) data1

        if (ndim == 3) then
            sample = data1( end_index(1)/2,end_index(2)/2,end_index(3)/2,1 )
        else if (ndim == 2) then
            sample = data1( end_index(1)/2,end_index(2)/2,1,1)
        else if (ndim == 1) then
            sample = data1( end_index(1)/2,1,1,1)
            write(*,'(10f10.3)') (data(l,1,1,1),l=1,end_index(1))
        end if

        write(*,'(A8,1x,I1,4(1x,I4),1x,A,1x,A," : ", E12.4,1x,A,2x,A)')&
             name, ndim, end_index(1), end_index(2), end_index(3), end_index(4),&
             staggering, ordering, sample, trim(units), trim(description)

        deallocate(data1)
        ENDIF

        deallocate(data)

     elseif (flag == 2) then
        newtime = .TRUE.
        write(*,'("===> WRITE flag=",i2," at ",f12.2)') flag, time
        write(*,'(/" ..newtime=",L)')  newtime
        write(iunit1) flag
        EXIT
     else
        stop
     endif

     read(iunit, iostat=ier) flag
 
     if(ier/=0) then
        exit
     endif
  enddo

  write(*,'(/,"Hit the end of file of unit ", I4)') iunit

end program convert_to_mm5v3

   subroutine printout_big_header(bhi, bhr, bhic, bhrc)

     implicit none
     integer, dimension(50,20) :: bhi
     real, dimension(20,20) :: bhr
     character(len=80), dimension(50,20) :: bhic
     character(len=80), dimension(20,20) :: bhrc
     integer :: i, j, v3j
   
     write(*,'(/)')
     v3j = bhi(1,1)
     if (bhi(1,1) == 11) v3j = v3j+5
     do j = 1, v3j
        if (j ==  1) write(*, '(/,"TERRAIN Portion of big header:")')
        if (j ==  2) write(*, '(/,"REGRID Portion of big header:")')
        if (j ==  3) write(*, '(/,"RAWINS Portion of big header:")')
        if (j ==  4) write(*, '(/,"SFC RAWINS Portion of big header:")')
        if (j ==  5) write(*, '(/,"INTERP Portion of big header:")')
        if (j ==  6) write(*, '(/,"MM5 Substrate Temp File big header:")')
        if (j ==  7) write(*, '(/,"MM5 Boundary File big header:")')
        if (j ==  8) write(*, '(/,"Interpolated MM5 Portion of big header:")')
        if (j == 10) write(*, '(/,"MM5 Statistics big header:")')
        if (j == 11) write(*, '(/,"MM5 Portion of big header:")')

        write(*,'(/,"***Integers:"/)')

        do i = 1, size(bhi,1)
           if (bhi(i,j) /= -999) then
              write(*,'("BHI(",I4,",",I2,"):",I8," : ",A)')&
                   i, j, bhi(i,j),trim(bhic(i,j))
           endif
        enddo
   
        write(*,'(/,"***Floats:"/)')

        do i = 1, size(bhr,1)
           if (bhr(i,j) /= -999.) then
              write(*,'("BHR(",I4,",",I2,"):",F9.2," : ",A)')&
                   i, j, bhr(i,j),trim(bhrc(i,j))
           endif
        enddo

        write(*,'(/)')
     enddo
   end subroutine printout_big_header
   
   subroutine arguments(wrf3dvar_file, lmore)
     implicit none
     character(len=*) :: wrf3dvar_file
     character(len=120) :: harg
     logical :: lmore
   
     integer :: ierr, i, numarg
     integer, external :: iargc
   
     numarg = iargc()
   
     i = 1
     lmore = .false.
   
     do while ( i < numarg) 
        call getarg(i, harg)
!        print*, 'harg = ', trim(harg)
   
        if (harg == "-v") then
           i = i + 1
           lmore = .true.
        elseif (harg == "-h") then
           call help
        endif
   
     enddo
   
     call getarg(i,harg)
     wrf3dvar_file = harg
   end subroutine arguments
   
   subroutine help
     implicit none
     character(len=120) :: cmd
     call getarg(0, cmd)
   
     write(*,'(/,"Usage: ", A, " [-v] v2file ")') trim(cmd)
     write(*,'(8x, "-v     : Print extra info")')
     write(*,'(8x, "v3file : MM5v3 file name to read.")')
     write(*,'(8x, "-h     : print this help message and exit.",/)')
     stop
   end subroutine help

