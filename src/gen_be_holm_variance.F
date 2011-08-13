program gen_be_holm_variance
!-----------------------------------------------------------------------
! Purpose: Creating WRF-ARW BE statistics for GSI                        
! 
! Author: Syed RH Rizvi,  NCAR/ESSL/MMM/DAG  08/06/2009
! Please acknowledge author/institute in work that uses this code.
!-----------------------------------------------------------------------
!
   implicit none

#ifdef DM_PARALLEL
  include 'mpif.h'
#endif

   integer, parameter   :: percent_bins=20    
!
   real, allocatable    :: xlat(:,:),xlon(:,:),rlat(:),mapfac_x(:,:),mapfac_y(:,:)
   integer, allocatable :: lat_bin(:,:)
!
   integer              :: nx,ny,nz,ni,nj,nk,dim1,dim2,dim3,ncat
   integer              :: nml_unit,ncases,member
   real                 :: min_xlat,lat_bins_in_deg
   integer              :: less_levels_from_top, debug

   character(len=120),allocatable  :: filen(:)       
   character(len=120)   :: stage1_gsi_dir, filename
   character(len=10)    :: date                    
   character(len=3)     :: ce                     

   integer              :: i,j,n, ierror, npes, mype
!
   namelist /gen_be_holm_variance_nl/debug,stage1_gsi_dir,nx,ny,nz, &
             less_levels_from_top, lat_bins_in_deg
!---------------------------------------------------------------------------------
!--------
! MPI setup
#ifdef DM_PARALLEL
      call mpi_init(ierror)

      call mpi_comm_size(mpi_comm_world,npes,ierror)

      call mpi_comm_rank(mpi_comm_world,mype,ierror)
#endif
!------------------------------------------------------------
!  Set default valuse for namelist parameters
!   
!------------------------------------------------------------

    stage1_gsi_dir = '/ptmp/rizvi/data/conus_3Km_cloud_case/run_gen_be_gsi/working/stage1_gsi'
    nx = 350; ny = 450; nz = 34 ; lat_bins_in_deg = 1.0
    less_levels_from_top = 0 ; debug = 0
!

   nml_unit=5
   open(unit=nml_unit, file='gen_be_holm_variance_nl.nl',form='formatted', status='old', action='read')
   read(nml_unit, gen_be_holm_variance_nl)
   close ( nml_unit)
   if(mype==0) write(6,nml= gen_be_holm_variance_nl)

   filename=trim(stage1_gsi_dir)//'/fixed_field'


    open(unit=10,file=trim(filename),form='unformatted', &
             status='old', action='read')

    read(10)date, dim1, dim2, dim3, ncases
    if(mype==0) write(6,*)' lat_bins (deg) = ',lat_bins_in_deg
    close (10)
    if(mype==0) write(6,*)' read dims: ',dim1,dim2,dim3,' ncases= ',ncases

    if( nx /= dim1 .or. ny /= dim2 .or. nz /= dim3) then
    write(6,*)' mismatch dims in fixed_field= ',dim1,dim2,dim3,' in gen_be_holm_variance_nl.nl ',nx,ny,nz
    stop
    end if

    allocate( mapfac_x(1:nx,1:ny) ,mapfac_y(1:nx,1:ny))
    allocate( xlat(1:nx,1:ny) ,xlon(1:nx,1:ny))
    allocate( lat_bin(1:nx,1:ny) )

    if( debug > 0  .and. mype==0)write(6,*)'calling get_fixed_fields ' 

    call get_fixed_fields(filename,nx,ny,nz, mype,ncat,mapfac_x,mapfac_y,&
                      lat_bins_in_deg,xlat,xlon,min_xlat)

!------------------------------------------
    allocate( rlat(1:ncat))
     rlat(1) = min_xlat + lat_bins_in_deg/2.
     do i=2,ncat
     rlat(i) = min_xlat + (i-1)*lat_bins_in_deg
     end do
  if( mype == 0 .and. debug > 0) then
     do i=1,ncat
     write(6,*)' Stats latitude = ',i,rlat(i)
     end do
  end if

!------------------------------------------
!  Fix lat_bins               
!------------------------------------------
   do j=1,ny
     do i=1,nx
      call get_cat_bins(rlat, xlat(i,j),ncat, lat_bin(i,j))
     end do
   end do
    filename=trim(stage1_gsi_dir)//'/pert_files'

    open(unit=10,file=trim(filename),form='formatted', &
             status='old', action='read')

    allocate(filen(1:ncases))
    do i = 1, ncases
     read(10,'(a)')filen(i)
    end do
    close (10)

    if( debug > 0  .and. mype==0)write(6,*)'calling compute_variance'
     call compute_variance(nx,ny,nz,ncat, npes,mype,debug, ncases, &
       less_levels_from_top,filen, rlat, xlat,xlon,lat_bin)

#ifdef DM_PARALLEL
    call mpi_finalize(ierror)
#endif

    deallocate(filen,xlat,xlon,mapfac_x,mapfac_y,rlat,lat_bin)

    stop
    end

  subroutine compute_variance(nx,ny,nz,ncat, &
              npes,mype,debug, ncases, less_levels_from_top, filen, &
              rlat, xlat,xlon,lat_bin)
!
   implicit none

#ifdef DM_PARALLEL
   include 'mpif.h'
 
     integer       :: mpi_rtype
    parameter(mpi_rtype=mpi_real8)

!   real, parameter           :: mpi_rtype=mpi_real8
#endif


   integer, parameter        :: len_vars_out = 10
   integer, parameter        :: nvars = 5
   real   , parameter    :: rsmall = 10.0e-30          ! small number to assign with hydrometeor

   integer, intent(in)       :: nx,ny,nz,ncat
   integer, intent(in)       :: npes,mype,debug,ncases,less_levels_from_top
   character(120), intent(in):: filen(ncases)
   real,    intent(in)       :: xlat(1:nx,1:ny),xlon(1:nx,1:ny)
   real, intent(in)          :: rlat(1:ncat)
   integer, intent(in)       :: lat_bin(1:nx,1:ny)

   real                      :: p(1:nx,1:ny)
   real                      :: sf(1:nx,1:ny,1:nz),vp(1:nx,1:ny,1:nz),t(1:nx,1:ny,1:nz)
   real                      :: rh(1:nx,1:ny,1:nz), rhm(1:nx,1:ny,1:nz)
   real                      :: qcloud(1:nx,1:ny,1:nz), qcloudm(1:nx,1:ny,1:nz)
   real                      :: qrain(1:nx,1:ny,1:nz), qrainm(1:nx,1:ny,1:nz)
   real                      :: qice(1:nx,1:ny,1:nz), qicem(1:nx,1:ny,1:nz)
   real                      :: qsnow(1:nx,1:ny,1:nz), qsnowm(1:nx,1:ny,1:nz)

   real                      :: rms(20,1:ncat,1:nz*nvars), rmst(20,1:ncat,1:nz*nvars) 

   real                      :: count(20,1:ncat,1:nz*nvars),countt(20,1:ncat,1:nz*nvars)

   integer                   :: ierror, npeloop, moop, loop 
   integer                   :: l_rh, l_qcloud, l_qrain, l_qice, l_qsnow

   integer                   :: i,j,l,ll,m,n


!----------------------------------------------------------------------------
!  Single precission variables
!-----------------------------------------------------------------------------
!----------------------------------------------------------------------------
   integer, allocatable :: nsig(:) 
   character(len=len_vars_out), allocatable :: vars(:) 
!DALE   integer              :: lat_bin 
   

!----------------------------------------------------------------------------


   allocate(vars(1:nvars) )  
   vars(1)='rh' ; vars(2)='qcloud'; vars(3)='qrain'; vars(4)='qice'; vars(5)='qsnow'
   
!----------------------------------------------------------------------------

    count=0. 
    countt=0. 

    npeloop=ncases/npes
    if(npes*npeloop .ne. ncases) npeloop=npeloop+1

      do moop=1,npeloop
      loop=npes*(moop-1)+mype+1

       if (loop.le.ncases)then
          call read_wrf_arw(trim(filen(loop)),nx,ny,nz, mype, sf,vp,t,rh,rhm,p,&
           qcloud,qcloudm,qrain,qrainm,qice,qicem,qsnow,qsnowm)
     
!   qcloud    
   do l=1,nz- less_levels_from_top
      l_rh     =        l
      l_qcloud =   nz + l
      l_qrain  = 2*nz + l
      l_qice   = 3*nz + l
      l_qsnow  = 4*nz + l
      do j=1,ny
      do i=1,nx

      n=rhm(i,j,l)*20.+1
!  rizvifix
      n = max( min(20,n), 1) 
!  rizvifix
      if(n <=20 .and. n >= 1 )then
            rms(n,lat_bin(i,j),l_rh)= rms(n,lat_bin(i,j),l_rh) + &
                         rh(i,j,l)*rh(i,j,l)
            count(n,lat_bin(i,j),l_rh)=count(n,lat_bin(i,j),l_rh)+1.
      else
      write(6,*)' got n= ',n,' bad rhm  at i,j,k ',i,j,l,' rhm = ',rhm(i,j,l)
      endif

      n=qcloudm(i,j,l)*20.+1
!  rizvifix
      n = max( min(20,n), 1) 
!  rizvifix
      if(n <=20 .and. n >= 1 )then
            rms(n,lat_bin(i,j),l_qcloud)= rms(n,lat_bin(i,j),l_qcloud) + &
                         qcloud(i,j,l)*qcloud(i,j,l)
            count(n,lat_bin(i,j),l_qcloud)=count(n,lat_bin(i,j),l_qcloud)+1.
      else
      write(6,*)' got n= ',n,' bad qcloudm  at i,j,k ',i,j,l,' qcloudm = ',qcloudm(i,j,l)
      endif

      n=qrainm(i,j,l)*20.+1
!  rizvifix
      n = max( min(20,n), 1) 
!  rizvifix
      if(n <=20 .and. n >= 1 )then
            rms(n,lat_bin(i,j),l_qrain)= rms(n,lat_bin(i,j),l_qrain) + &
                         qrain(i,j,l)*qrain(i,j,l)
            count(n,lat_bin(i,j),l_qrain)=count(n,lat_bin(i,j),l_qrain)+1.
      else
      write(6,*)' got n= ',n,' bad qrainm  at i,j,k ',i,j,l,' qrainm = ',qrainm(i,j,l)
      endif

      n=qicem(i,j,l)*20.+1
!  rizvifix
      n = max( min(20,n), 1) 
!  rizvifix
      if(n <=20 .and. n >= 1 )then
            rms(n,lat_bin(i,j),l_qice)= rms(n,lat_bin(i,j),l_qice) + &
                         qice(i,j,l)*qice(i,j,l)
            count(n,lat_bin(i,j),l_qice)=count(n,lat_bin(i,j),l_qice)+1.
      else
      write(6,*)' got n= ',n,' bad qicem at i,j,k ',i,j,l,' qicem = ',qicem(i,j,l)
      endif

      n=qsnowm(i,j,l)*20.+1
!  rizvifix
      n = max( min(20,n), 1) 
!  rizvifix
      if(n <=20 .and. n >= 1 )then
            rms(n,lat_bin(i,j),l_qsnow)= rms(n,lat_bin(i,j),l_qsnow) + &
                         qsnow(i,j,l)*qsnow(i,j,l)
            count(n,lat_bin(i,j),l_qsnow)=count(n,lat_bin(i,j),l_qsnow)+1.
      else
      write(6,*)' got n= ',n,' bad qsnowm  at i,j,k ',i,j,l,' qsnowm = ',qsnowm(i,j,l)
      endif

      end do
      end do

   end do

  endif !loop<ncases
ENDDO   ! LOOP 

#ifdef DM_PARALLEL
      call mpi_allreduce(rms,rmst,20*ncat*nz*nvars,mpi_rtype,mpi_sum, &
         mpi_comm_world,ierror)
      call mpi_allreduce(count,countt,20*ncat*nz*nvars,mpi_rtype,mpi_sum, &
         mpi_comm_world,ierror)
#endif
   
   do l= nz - less_levels_from_top +1 , nz

      l_rh     =        l
      l_qcloud =   nz + l
      l_qrain  = 2*nz + l
      l_qice   = 3*nz + l
      l_qsnow  = 4*nz + l

       rmst(:,1:ncat,l_rh    )   = rsmall                       
       rmst(:,1:ncat,l_qcloud)   = rsmall                       
       rmst(:,1:ncat,l_qrain )   = rsmall                       
       rmst(:,1:ncat,l_qice  )   = rsmall                       
       rmst(:,1:ncat,l_qsnow )   = rsmall                       
   end do

!!!! normalize !!!!!!!!!!!

   if ( mype == 0) then
   do i=1,20   
    do ll=1,ncat
     do l=1,nz

      l_rh     =        l
      l_qcloud =   nz + l
      l_qrain  = 2*nz + l
      l_qice   = 3*nz + l
      l_qsnow  = 4*nz + l

     if(countt(i,ll,l_rh) > 0) & 
     rmst(i,ll,l_rh)=rmst(i,ll,l_rh)/countt(i,ll,l_rh)
     if(countt(i,ll,l_qcloud) > 0) & 
     rmst(i,ll,l_qcloud)=rmst(i,ll,l_qcloud)/countt(i,ll,l_qcloud)
     if(countt(i,ll,l_qrain) > 0) & 
     rmst(i,ll,l_qrain)=rmst(i,ll,l_qrain)/countt(i,ll,l_qrain)
     if(countt(i,ll,l_qice) > 0) & 
     rmst(i,ll,l_qice)=rmst(i,ll,l_qice)/countt(i,ll,l_qice)
     if(countt(i,ll,l_qsnow) > 0) & 
     rmst(i,ll,l_qsnow)=rmst(i,ll,l_qsnow)/countt(i,ll,l_qsnow)

    enddo
   enddo
   enddo

    rmst=sqrt(rmst)

    open(unit=20, file='holm_variance.dat',form='unformatted', status='new', action='write')
    open(unit=30, file='holm_variance.bin',form='unformatted', status='new', action='write')
    write(20)ncat, nz
    write(30)ncat, nz
    do n = 1, nvars
    write(20)trim(vars(n)), rmst(1:20,1:ncat,(n-1)*nz + 1 : n*nz)
    write(30)               rmst(1:20,1:ncat,(n-1)*nz + 1 : n*nz)
    end do
    do n = 1, 20
!    do l = 1, nz    
    l = 10
    write(6,*)' at percent_bin ',n,' level ',l,' rh and hydro ',rmst(n,1,l),rmst(n,1,nz+l),rmst(n,1,2*nz+l),rmst(n,1,3*nz+l),rmst(n,1,4*nz+1)
!    write(6,*)' for level ',l,' maxval rh     ',maxval(rmst(n,:,     l))
!    write(6,*)' for level ',l,' maxval qcloud ',maxval(rmst(n,:,  nz+l))
!    write(6,*)' for level ',l,' maxval qrain  ',maxval(rmst(n,:,2*nz+l))
!    write(6,*)' for level ',l,' maxval qice   ',maxval(rmst(n,:,3*nz+l))
!    write(6,*)' for level ',l,' maxval qsnow  ',maxval(rmst(n,:,4*nz+l))
!    end do
    end do

   end if
  return
  end subroutine compute_variance

   subroutine statww(x,n) 
   implicit none

   integer, intent(in)   :: n 
   real,    intent(in)   :: x(1:n)
   real                  ::  xmax, xmin, xavg
   integer               :: i    
    xmax=-99999.
    xmin=99999.
    xavg=0.
    do i=1,n
    xavg=xavg+x(i)
    xmax=max(x(i),xmax)
    xmin=min(x(i),xmin)
    enddo
    xavg=xavg/float(n)
    write( 123,*)'max= ',xmax,'xmin= ',xmin,'avg= ',xavg
   return
   end subroutine statww 

   subroutine statww4(x,nx)
   implicit none
   real*4,dimension(nx):: x
   real*4 wmean,xmin,xmax
   integer i,imin,imax,nx
!
        xmax=-1e+9
        xmin=1e+9
        wmean=0.
        do i=1,nx
        wmean=wmean+x(i)
        if(x(i).gt.xmax)then
         xmax=x(i)
         imax=i
        else if (x(i).lt.xmin)then
        xmin=x(i)
         imin=i
           endif
          enddo
       wmean=wmean/float(nx)
      write(123,*)'max,min=',xmax,imax,xmin,imin,wmean
   return
   end subroutine statww4


   subroutine get_fixed_fields(filename,nx,ny,nz,mype,ncat,mapfac_x,mapfac_y,&
                      lat_bins_in_deg,xlat,xlon,min_xlat)
   implicit none

   character(len=*), intent(in)      :: filename
   integer, intent(in)               :: nx, ny, nz, mype
   real, intent(in)                  :: lat_bins_in_deg
   real, intent(out)                 :: mapfac_x(1:nx,1:ny)
   real, intent(out)                 :: mapfac_y(1:nx,1:ny)
   real, intent(out)                 :: xlat(1:nx,1:ny)
   real, intent(out)                 :: xlon(1:nx,1:ny)
   real, intent(out)                 :: min_xlat
   integer, intent(out)              :: ncat

   character (len=10)           :: date
   integer                      :: i,j,n,nrec,dim1,dim2,dim3, ncases, nmax, nmin
   real                         :: ds, xmax, xmin
   real                         :: sigl(1:nz)

   open(unit=10,file=trim(filename),form='unformatted', &
             status='old', action='read')

   read(10, err=100,end=99)date, dim1, dim2, dim3, ncases, ds

   nrec = 1
   nmin=999
   nmax=-999
   xmin=999.
   xmax=-999.

   nrec = nrec + 1
!  Halflevel sigma field
   read(10, err=100,end=99)sigl
   nrec = nrec + 1
! Lat
   read(10,err=100,end=99)xlat
   nrec=nrec+1
! Lon
   read(10,err=100,end=99)xlon
   nrec=nrec+1
! mafac_x
   read(10,err=100,end=99)mapfac_x
   nrec=nrec+1
! mafac_y
   read(10,err=100,end=99)mapfac_y
   close (10)

! Compute Category
     do j=1,ny
        do i=1,nx
        xmax=max(xmax,xlat(i,j))
        xmin=min(xmin,xlat(i,j))
         n=int(xlat(i,j))
         nmin=min0(nmin,n)
         nmax=max0(nmax,n)
        end do
     end do
   min_xlat = xmin

   ncat  =(xmax-xmin)/lat_bins_in_deg + 1
   if(mype==0) then
    write(6,*)'For this domain min/max latitude = ',xmin,xmax
    write(6,*)' nmin,nmax = ',nmin,nmax,' ncat = ',ncat
   end if
   return
99  write(6,*)'Unexpected end of file in ',trim(filename),' in get_fixed_fileds record recs: ',nrec
   stop
100 write(6,*)'read error in ',trim(filename),' in get_fixed_fileds record recs: ',nrec
   stop
   end subroutine get_fixed_fields


  subroutine read_wrf_arw(filename,nx,ny,nz, mype, sf,vp,t,rh,rhm,p,&
            qcloud,qcloudm,qrain,qrainm,qice,qicem,qsnow,qsnowm)

   implicit none

   character(len=*), intent(in)      :: filename       
   integer, intent(in)      :: nx, ny, nz, mype

   real, intent(out)        :: sf(1:nx,1:ny,1:nz)
   real, intent(out)        :: vp(1:nx,1:ny,1:nz)
   real, intent(out)        :: t(1:nx,1:ny,1:nz)
   real, intent(out)        :: rh(1:nx,1:ny,1:nz)
   real, intent(out)        :: rhm(1:nx,1:ny,1:nz)
   real, intent(out)        :: p(1:nx,1:ny)
   real, intent(out)        :: qcloud(1:nx,1:ny,1:nz), qcloudm(1:nx,1:ny,1:nz)
   real, intent(out)        :: qrain(1:nx,1:ny,1:nz), qrainm(1:nx,1:ny,1:nz)
   real, intent(out)        :: qice(1:nx,1:ny,1:nz), qicem(1:nx,1:ny,1:nz)
   real, intent(out)        :: qsnow(1:nx,1:ny,1:nz), qsnowm(1:nx,1:ny,1:nz)

   real                     :: ds          
   character (len=10) :: date 
   integer            :: nrec

   integer            :: i,j,k
!
   nrec = 1
    open(unit=24,file=trim(filename),form='unformatted', &
             status='old', action='read')

! Psi
   read(24,err=100,end=99)sf  
   nrec=nrec+1
! Chi
   read(24,err=100,end=99)vp 
   nrec=nrec+1
! Fill zero vp fileds with adjacent value
!   vp(:,1,:) =2*vp(:,2,:) - vp(:,3,:) 
!   vp(1,:,:) =2*vp(2,:,:) - vp(3,:,:) 
! Temp
   read(24,err=100,end=99)t  
   nrec=nrec+1
! Moisture
   read(24,err=100,end=99)rh  
   nrec=nrec+1
! Mean Moisturer
   read(24,err=100,end=99)rhm  
   nrec=nrec+1
! Surface pressure (in whatever units it is writtenn stage0_gsi) 
   read(24,err=100,end=99)p    
! qcloud  
   read(24,err=100,end=99)qcloud 
   nrec=nrec+1
! Mean qcloud  
   read(24,err=100,end=99)qcloudm 
   nrec=nrec+1
! qrain   
   read(24,err=100,end=99)qrain 
   nrec=nrec+1
! Mean qcloud  
   read(24,err=100,end=99)qrainm 
   nrec=nrec+1
! qice 
   read(24,err=100,end=99)qice 
   nrec=nrec+1
! Mean qice  
   read(24,err=100,end=99)qicem 
   nrec=nrec+1
! qsnow 
   read(24,err=100,end=99)qsnow 
   nrec=nrec+1
! Mean qsnow 
   read(24,err=100,end=99)qsnowm 
   nrec=nrec+1

   close (24)

   return
99  write(6,*)'Unexpected end of file ',trim(filename),' in read_wrf_arw record recs: ',nrec
   stop
100 write(6,*)'read error in ',trim(filename),' in read_wrf_arw record recs: ',nrec
   stop
   end subroutine read_wrf_arw

  subroutine get_cat_bins(rlat, lat, ncat, lat_bin)
  implicit   none
  
  integer, intent(in)  :: ncat
  real,    intent(in)  :: lat
  real,    intent(in)  :: rlat(1:ncat)
  integer, intent(out) :: lat_bin 
  integer              :: i, j, k

   if( lat < rlat(2)) then
    lat_bin = 1
    return
   else if( lat > rlat(ncat-1) ) then
    lat_bin = ncat
    return
   else
     do k = 2, ncat - 1
     if( lat >= rlat(k) .and. lat < rlat(k+1) ) then
      lat_bin = k
      return
     end if
     end do
     write( 6, *)' in get_cat_bins hit wrong lat= ',lat
     write( 6, *)' rlat(',k,') = ',rlat(k),' rlat(',k+1,')= ',rlat(k+1)
     stop
    return
   end if
  end subroutine get_cat_bins
