 program da_stats_add
!------------------------------------------------------------------------------
!  PURPOSE: add current date's data to previous date' data
!
!  HISTORY: 2005/11/16 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   integer, parameter        :: ounit = 77     ! output ascii stat file unit
   integer, parameter        :: namelist_unit = 16
   real, parameter           :: missing_r=-99.0 ! Missing data indicator.

   character (len=200)       :: sdate          ! Start Date to process
   character (len=200)       :: cdate          ! End   Date to process
   INTEGER                   :: cycle_hour     ! cycle hours
   character (len=200)       :: filename       ! Data filename.
   character (len=200)       :: file_in        ! Data filename.
   character (len=200)       :: file_in1       ! Data filename.
   character (len=200)       :: file_out       ! Data filename.
   character (len=40)        :: platform       ! Satellite platform.
   character (len=40)        :: satid          ! Satellite id.
   character (len=40)        :: instrument     ! Instrument.
   character (len=45)       :: string

   integer                   :: i, n        ! Loop counters.
   integer                   :: ndomain        ! Number of obs. on processor.
   integer                   :: num_rad        ! current observation count.
   integer                   :: t_num_rad      ! Total observation count.
   integer                   :: nchan          ! Number of channels.
   integer                   :: nscan          ! Number of scan angles

   INTEGER                   :: j, k, kk, jj  ! Index dimension.

   REAL              :: delta
   INTEGER, pointer  :: ninv(:), nerr(:), ngood(:)
   INTEGER           :: nscat, nclwp, nsurf,nrej_chw_land,nrej_ch5_topo
   INTEGER, pointer  :: npos(:,:), nc(:)
   REAL   , pointer  :: avescan(:,:), rmsscan(:,:), stdscan(:,:)
   REAL   , pointer  :: ave(:), rms(:), std(:)
   REAL   , pointer  :: sumx(:),sumy(:),sumxx(:),sumxy(:),a(:),b(:)
   REAL   , pointer  :: sumxscan(:,:),sumyscan(:,:), &
                        sumxxscan(:,:),sumxyscan(:,:), &
                        ascan(:,:),bscan(:,:), sumiv(:), &
                        sumiv2(:), sumivscan(:,:),sumiv2scan(:,:)

   INTEGER, pointer  :: t_ninv(:), t_nerr(:), t_ngood(:)
   INTEGER           :: t_nscat, t_nclwp, t_nsurf,t_nrej_chw_land,t_nrej_ch5_topo
   INTEGER, pointer  :: t_npos(:,:), t_nc(:)
   REAL   , pointer  :: t_avescan(:,:), t_rmsscan(:,:), t_stdscan(:,:)
   REAL   , pointer  :: t_ave(:), t_rms(:), t_std(:)
   REAL   , pointer  :: t_sumx(:),t_sumy(:),t_sumxx(:),t_sumxy(:),t_a(:),t_b(:)
   REAL   , pointer  :: t_sumxscan(:,:),t_sumyscan(:,:), &
                        t_sumxxscan(:,:),t_sumxyscan(:,:), &
                        t_ascan(:,:),t_bscan(:,:), t_sumiv(:), &
                        t_sumiv2(:), t_sumivscan(:,:),t_sumiv2scan(:,:)

   INTEGER    :: IOST
   
   NAMELIST /nam_stat/ sdate, cdate, cycle_hour, platform, satid, instrument, &
                       nchan, nscan

!
!  0.0 read namelist 
!---------------------------------------------------------------
   OPEN ( FILE   = 'namelist.add', UNIT   = namelist_unit, &
          STATUS = 'OLD' , ACCESS = 'SEQUENTIAL', &
          FORM   = 'FORMATTED', ACTION = 'READ', &
          IOSTAT = IOST )

   READ  ( UNIT = namelist_unit, NML = nam_stat , IOSTAT = IOST)

   CLOSE ( UNIT = namelist_unit )

   write(unit=*, fmt='(a)') ' ---------------------------------------------------'
   write(unit=*, fmt='(a,a10)') ' [2.0] Add statistics from  ', trim(cdate)
   write(unit=*, fmt='(a)') ' ---------------------------------------------------'

!
!  0.1 allocate space
!----------------------------------------------------------
   allocate ( ninv (nchan) )
   allocate ( nerr (nchan) )
   allocate ( ngood(nchan) )
   allocate ( ave  (nchan) )
   allocate ( std  (nchan) )
   allocate ( rms  (nchan) )
   allocate ( sumiv(nchan) )
   allocate ( sumiv2(nchan) )
   allocate ( nc   (nchan) )
   allocate ( sumx (nchan) )
   allocate ( sumy (nchan) )
   allocate ( sumxx(nchan) )
   allocate ( sumxy(nchan) )
   allocate ( a    (nchan) )
   allocate ( b    (nchan) )
   allocate ( npos (nscan,nchan) )
   allocate ( avescan(nscan,nchan) )
   allocate ( stdscan(nscan,nchan) )
   allocate ( rmsscan(nscan,nchan) )
   allocate ( sumivscan(nscan,nchan) )
   allocate ( sumiv2scan(nscan,nchan) )
   allocate ( sumxscan(nscan,nchan) )
   allocate ( sumyscan(nscan,nchan) )
   allocate ( sumxxscan(nscan,nchan) )
   allocate ( sumxyscan(nscan,nchan) )
   allocate ( ascan(nscan,nchan) )
   allocate ( bscan(nscan,nchan) )

   allocate ( t_ninv (nchan) )
   allocate ( t_nerr (nchan) )
   allocate ( t_ngood(nchan) )
   allocate ( t_ave  (nchan) )
   allocate ( t_std  (nchan) )
   allocate ( t_rms  (nchan) )
   allocate ( t_nc   (nchan) )
   allocate ( t_sumiv (nchan) )
   allocate ( t_sumiv2 (nchan) )
   allocate ( t_sumx (nchan) )
   allocate ( t_sumy (nchan) )
   allocate ( t_sumxx(nchan) )
   allocate ( t_sumxy(nchan) )
   allocate ( t_a    (nchan) )
   allocate ( t_b    (nchan) )
   allocate ( t_npos (nscan,nchan) )
   allocate ( t_avescan(nscan,nchan) )
   allocate ( t_stdscan(nscan,nchan) )
   allocate ( t_rmsscan(nscan,nchan) )
   allocate ( t_sumivscan(nscan,nchan) )
   allocate ( t_sumiv2scan(nscan,nchan) )
   allocate ( t_sumxscan(nscan,nchan) )
   allocate ( t_sumyscan(nscan,nchan) )
   allocate ( t_sumxxscan(nscan,nchan) )
   allocate ( t_sumxyscan(nscan,nchan) )
   allocate ( t_ascan(nscan,nchan) )
   allocate ( t_bscan(nscan,nchan) )

!  1.0 read total stat file
!------------------------------------------------------------------------------
   read(99) t_num_rad,t_nscat,t_nclwp, &
            t_nsurf,t_ninv,t_nerr,t_ngood,t_nrej_chw_land,t_nrej_ch5_topo

!   write(6,'(19i8)') t_num_rad,t_nscat,t_nclwp,t_nsurf,t_ninv,t_nerr,t_ngood
   write(6,'(19i8)') t_num_rad,t_ngood

   read(99) t_nc,t_sumx,t_sumy, &
               t_sumxx, t_sumxy, &
               t_sumiv, t_sumiv2 

   read(99) t_npos,t_sumxscan, t_sumyscan, &
               t_sumxxscan, t_sumxyscan, &
               t_sumivscan, t_sumiv2scan

!
!  2.0 read current stat file
!-----------------------------------------------------------------
   read(88) num_rad,nscat,nclwp, &
            nsurf,ninv,nerr,ngood,nrej_chw_land,nrej_ch5_topo

!   write(6,'(19i8)') num_rad,nscat,nclwp,nsurf,ninv,nerr,ngood
   write(6,'(19i8)') num_rad,ngood

   read(88) nc,sumx,sumy, &
               sumxx, sumxy, &
               sumiv, sumiv2

   read(88) npos,sumxscan, sumyscan, &
               sumxxscan, sumxyscan, &
               sumivscan, sumiv2scan

!
!   3.0 Add current to total stat
!---------------------------------------------
    t_num_rad  = t_num_rad + num_rad
    t_nscat    = t_nscat   + nscat
    t_nclwp    = t_nclwp   + nclwp
    t_nsurf    = t_nsurf   + nsurf
    t_nrej_chw_land  = t_nrej_chw_land + nrej_chw_land
    t_nrej_ch5_topo  = t_nrej_ch5_topo + nrej_ch5_topo
    t_ninv(:)  = t_ninv(:) + ninv(:)
    t_nerr(:)  = t_nerr(:) + nerr(:)
    t_ngood(:) = t_ngood(:)+ ngood(:)

!    write(6,'(19i8)') t_num_rad,t_nscat,t_nclwp,t_nsurf,t_ninv,t_nerr,t_ngood
    write(6,'(19i8)') t_num_rad,t_ngood

     t_nc     =   t_nc   + nc
     t_sumx   = t_sumx   + sumx
     t_sumy   = t_sumy   + sumy
     t_sumxx  = t_sumxx  + sumxx
     t_sumxy  = t_sumxy  + sumxy
     t_sumiv  = t_sumiv  + sumiv
     t_sumiv2 = t_sumiv2 + sumiv2

     t_npos       = t_npos + npos
     t_sumxscan   = t_sumxscan + sumxscan
     t_sumyscan   = t_sumyscan + sumyscan
     t_sumxxscan  = t_sumxxscan+ sumxxscan
     t_sumxyscan  = t_sumxyscan+ sumxyscan
     t_sumivscan  = t_sumivscan+ sumivscan
     t_sumiv2scan = t_sumiv2scan+ sumiv2scan


   do j = 1,nchan
     if ( t_nc(j)>= 10 ) then
         t_ave(j) = t_sumiv(j)/t_nc(j)
         t_rms(j) = t_sumiv2(j)/t_nc(j)
         t_std(j) = t_rms(j)-t_ave(j)*t_ave(j)
         t_rms(j) = sqrt(t_rms(j))
         t_std(j) = sqrt(t_std(j))
         delta    = t_nc(j)*t_sumxx(j) - t_sumx(j)*t_sumx(j)
         t_a(j)   = (t_sumxx(j)*t_sumy(j)-t_sumx(j)*t_sumxy(j))/delta
         t_b(j)   = (  t_nc(j)*t_sumxy(j)-t_sumx(j)*t_sumy (j))/delta
     else
         t_ave(j) = missing_r
         t_std(j) = missing_r
         t_a(j)   = missing_r
         t_b(j)   = missing_r
     end if

   do k = 1,nscan
    if ( t_npos(k,j) >= 10 ) then
         t_avescan(k,j) = t_sumivscan(k,j)/t_npos(k,j)
         t_rmsscan(k,j) = t_sumiv2scan(k,j)/t_npos(k,j)
         t_stdscan(k,j) = t_rmsscan(k,j)-t_avescan(k,j)*t_avescan(k,j)
         t_rmsscan(k,j) = sqrt(t_rmsscan(k,j))
         t_stdscan(k,j) = sqrt(t_stdscan(k,j))
          delta     = t_npos(k,j)*t_sumxxscan(k,j) - t_sumxscan(k,j)*t_sumxscan(k,j)
       t_ascan(k,j) = (t_sumxxscan(k,j)*t_sumyscan (k,j)-t_sumxscan(k,j)*t_sumxyscan(k,j))/delta
       t_bscan(k,j) = (     t_npos(k,j)*t_sumxyscan(k,j)-t_sumxscan(k,j)*t_sumyscan (k,j))/delta
    else
       t_avescan(k,j) = missing_r
       t_stdscan(k,j) = missing_r
       t_ascan(k,j)   = missing_r
       t_bscan(k,j)   = missing_r
    end if
   end do

   end do

!
!   4.0 write back total stat file
!---------------------------------------------------------------------------
   
   write(ounit,'(2x,5a)')  trim(platform),'-',trim(satid),'-',trim(instrument)
   write(ounit,'(2x,a10,2x,a10,i4)') trim(sdate), trim(cdate), cycle_hour
   write(ounit,'(2x,2i5)') nchan, nscan

   write(ounit,'(a,i7)')   ' Number of    total obs                    = ', t_num_rad
   write(ounit,'(a,i7)')   ' Number of rejected window chanl over land = ', t_nrej_chw_land
   write(ounit,'(a,i7)')   ' Number of rejected ch5 over mountain      = ', t_nrej_ch5_topo
   write(ounit,'(a,i7)')   ' Number of rejected obs by SI>3K           = ', t_nscat
   write(ounit,'(a,i7)')   ' Number of rejected obs by CLWP>0.2mm      = ', t_nclwp
   write(ounit,'(a,i7)')   ' Number of rejected obs by Mixture surface = ', t_nsurf
   write(ounit,'(a,19i7)') ' Number of rejected obs by abs(IV)>15K     = ', t_ninv(:)
   write(ounit,'(a,19i7)') ' Number of rejected obs by abs(IV)>3*sigma = ', t_nerr(:)
   write(ounit,'(a,19i7)') ' Number of     good obs                    = ', t_ngood(:)

   write(ounit,'(7a8)') 'scan  ', 'chan ', 'number ', 'average ', 'rms  std ','a ','b '
   do j = 1,nchan
     write(ounit,'(i3,i5,i8,3(2x,f8.3),2(2x,f11.3))') &
          0, j, t_nc(j), t_ave(j), t_rms(j),t_std(j),t_a(j),t_b(j)
   end do
   do k = 1,nscan
   do j = 1,nchan
      write(ounit,'(i3,i5,i8,3(2x,f8.3),2(2x,f11.3))') &
                 k, j, t_npos(k,j),t_avescan(k,j),t_rmsscan(k,j),t_stdscan(k,j), &
                 t_ascan(k,j),t_bscan(k,j)
   end do
   end do

   write(98) t_num_rad,t_nscat,t_nclwp, &
            t_nsurf,t_ninv,t_nerr,t_ngood, &
            t_nrej_chw_land, t_nrej_ch5_topo

   write(98) t_nc,t_sumx,t_sumy, &
               t_sumxx, t_sumxy, &
               t_sumiv, t_sumiv2

   write(98) t_npos,t_sumxscan, t_sumyscan, &
               t_sumxxscan, t_sumxyscan, &
               t_sumivscan, t_sumiv2scan

  stop
 
END program da_stats_add
