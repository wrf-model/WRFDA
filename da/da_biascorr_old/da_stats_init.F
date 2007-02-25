 program da_stats_init
!------------------------------------------------------------------------------
!  PURPOSE: write a null statistics file with 0 values
!
!  HISTORY: 2005/11/16 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   integer, parameter        :: namelist_unit = 16

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

   integer                   :: p, i, n        ! Loop counters.
   integer                   :: ndomain        ! Number of obs. on processor.
   integer                   :: num_rad        ! Total observation count.
   integer                   :: nchan          ! Number of channels.
   integer                   :: nscan          ! Number of scan angles

   INTEGER                   :: j, k  ! Index dimension.

   INTEGER    :: sensor_id, sensor_type
   INTEGER, pointer  :: ninv(:), nerr(:), ngood(:), ichan(:)
   INTEGER    :: nscat, nclwp, nsurf,nrej_chw_land,nrej_ch5_topo
   INTEGER, pointer  :: npos(:,:), nc(:)
   REAL   , pointer  :: avescan(:,:), rmsscan(:,:), stdscan(:,:)
   REAL   , pointer  :: ave(:), rms(:), std(:)
   REAL   , pointer  :: sumx(:),sumy(:),sumxx(:),sumxy(:),a(:),b(:)
   REAL   , pointer  :: sumxscan(:,:),sumyscan(:,:), &
                        sumxxscan(:,:),sumxyscan(:,:), &
                        ascan(:,:),bscan(:,:), sumiv(:), &
                        sumiv2(:), sumivscan(:,:),sumiv2scan(:,:)
   INTEGER    :: IOST
   
   NAMELIST /nam_stat/ sdate, cdate, cycle_hour, platform, satid, instrument, &
                       nchan, nscan

!
!  0.0 read namelist 
!---------------------------------------------------------------
   write(unit=*, fmt='(a)') ' -------------------------'
   write(unit=*, fmt='(a)') ' Initialise fort.99 '
   write(unit=*, fmt='(a)') ' -------------------------'

   OPEN ( FILE   = 'namelist.add', UNIT   = namelist_unit, &
          STATUS = 'OLD' , ACCESS = 'SEQUENTIAL', &
          FORM   = 'FORMATTED', ACTION = 'READ', &
          IOSTAT = IOST )

   READ  ( UNIT = namelist_unit, NML = nam_stat , IOSTAT = IOST)

   CLOSE ( UNIT = namelist_unit )

!
!  0.1 allocate space
!----------------------------------------------------------
   allocate ( ninv (nchan) )
   allocate ( nerr (nchan) )
   allocate ( ngood(nchan) )
   allocate ( nc   (nchan) )
   allocate ( sumx (nchan) )
   allocate ( sumy (nchan) )
   allocate ( sumxx(nchan) )
   allocate ( sumxy(nchan) )
   allocate ( sumiv(nchan) )
   allocate ( sumiv2(nchan) )
   allocate ( npos (nscan,nchan) )
   allocate ( sumxscan(nscan,nchan) )
   allocate ( sumyscan(nscan,nchan) )
   allocate ( sumxxscan(nscan,nchan) )
   allocate ( sumxyscan(nscan,nchan) )
   allocate ( sumivscan(nscan,nchan) )
   allocate ( sumiv2scan(nscan,nchan) )

   num_rad = 0
   nscat   = 0
   nclwp   = 0
   nsurf   = 0
   ninv(:) = 0
   nerr(:) = 0
   ngood(:)= 0
   nrej_chw_land = 0
   nrej_ch5_topo = 0

   npos(:,:)      = 0
   nc  (:)        = 0
   sumiv (:)      = 0.
   sumiv2(:)      = 0.
   sumivscan (:,:)= 0.
   sumiv2scan(:,:)= 0.
   sumx(:)        = 0.
   sumy(:)        = 0.
   sumxx(:)       = 0.
   sumxy(:)       = 0.
   sumxscan(:,:)  = 0.
   sumyscan(:,:)  = 0.
   sumxxscan(:,:) = 0.
   sumxyscan(:,:) = 0.

     write(99) num_rad,nscat,nclwp, &
               nsurf,ninv,nerr,ngood,nrej_chw_land,nrej_ch5_topo

     write(99) nc,sumx,sumy, &
               sumxx, sumxy, &
               sumiv, sumiv2

     write(99) npos,sumxscan, sumyscan, &
               sumxxscan, sumxyscan, &
               sumivscan, sumiv2scan

  stop
 
END program da_stats_init
