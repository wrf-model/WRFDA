c
c  In this example we will open an HDF file and create UTM and Polar
c  Stereographic grid structures within the file.
c

      program setupgrid

      integer            status, gddefdim
      integer            gddetach, gdclose
      integer*4          gdfid, gdid, gdid2, gdid3, gdopen, gdcreate
      integer*4          xdim, ydim
      integer*4          zonecode, spherecode
      real*8             uplft(2), lowrgt(2), projparm(16), ehconvang

      integer DFACC_CREATE
      parameter (DFACC_CREATE=4)
      integer SD_UNLIMITED
      parameter (SD_UNLIMITED=0)      
      integer GCTP_UTM
      parameter (GCTP_UTM=1)
      integer GCTP_PS
      parameter (GCTP_PS=6)      
      integer GCTP_GEO
      parameter (GCTP_GEO=0)      
      integer HDFE_GD_LR
      parameter (HDFE_GD_LR=3)      
      integer HDFE_DEG_DMS
      parameter (HDFE_DEG_DMS=3)      
      
c   
c      We first open the HDF grid file, "GridFile.hdf".  Because this
c      file does not already exist, we use the DFACC_CREATE access
c      code in the open statement.  The GDopen routine returns the grid
c      file id, gdfid, which is used to identify the file in subsequent
c      routines in the library.
c
      gdfid = gdopen('GridFile.hdf',DFACC_CREATE)

c    
c     Create UTM Grid
c     

c     Region is bounded by 54 E and 60 E longitude and 20 N and 30 N latitude.
c     UTM Zone 40
c 
c     Use default spheriod (Clarke 1866 - spherecode = 0)
c 
c     Grid into 120 bins along x-axis and 200 bins along y-axis
c                   (approx 3' by 3' bins)


      zonecode = 40
      spherecode = 0

c    Upper Left and Lower Right points in meters
c    -------------------------------------------
      uplft(1) =   210584.50041
      uplft(2) =  3322395.95445
      lowrgt(1) =  813931.10959
      lowrgt(2) = 2214162.53278

      xdim = 120
      ydim = 200
    
      gdid = gdcreate(gdfid, "UTMGrid", xdim, ydim, uplft, lowrgt)
      status = gddefproj(gdid, GCTP_UTM, zonecode, 
     1                   spherecode, projparm)
    
c    Define "Time" Dimension
      status = gddefdim(gdid, "Time", 10)



c   
c     Create polar stereographic grid
c     
c     Northern Hemisphere  (True Scale at 90 N, 0 Longitude below pole)
c     
c     Use International 1967 spheriod (spherecode = 3)
c     
c     Grid into 100 bins along x-axis and y-axis
c
      
      xdim = 100
      ydim = 100
      
      spherecode = 3

c     Define GCTP Projection Parameters
c     ---------------------------------
      do i=1,16
         projparm(i) = 0
      enddo
      
c     Set Longitude below pole & true scale in DDDMMMSSS.SSS format)
      projparm(5) = 0.0
      projparm(6) = 90000000.00

c     Use default boundaries for Polar Stereographic (hemisphere)
      uplft(1) =   0
      uplft(2) =   0
      lowrgt(1) =  0
      lowrgt(2) =  0
      
      gdid2 = gdcreate(gdfid, "PolarGrid", xdim, ydim, uplft, lowrgt)
      status = gddefproj(gdid2, GCTP_PS, 0, spherecode, projparm)
      status = gddeforigin(gdid2, HDFE_GD_LR)
      
c     Define "Bands" Dimension
      status = gddefdim(gdid2, "Bands", 3)

      
      
c
c     Create geographic (linear scale) grid
c
c     0 - 15 degrees longitude,  20 - 30 degrees latitude
c
      xdim = 60
      ydim = 40

      uplft(1) = ehconvAng(0.d0, HDFE_DEG_DMS)
      uplft(2) = ehconvAng(30.d0, HDFE_DEG_DMS)
      lowrgt(1) = ehconvAng(15.d0, HDFE_DEG_DMS)
      lowrgt(2) = ehconvAng(20.d0, HDFE_DEG_DMS)
      
      gdid3 = gdcreate(gdfid, "GEOGrid", xdim, ydim, uplft, lowrgt)
      status = gddefproj(gdid3, GCTP_GEO, 0, 0, 0)
      
      
c
c     We now close the grid interface with the GDdetach routine.  This step
c     is necessary to properly store the grid information within the file
c     AND SHOULD BE DONE BEFORE WRITING OR READING DATA TO OR FROM THE FIELD.
c

      status = gddetach(gdid)
      status = gddetach(gdid2)
      status = gddetach(gdid3)


c    
c     Finally, we close the grid file using the gdclose routine.  This will
c     release the grid file handles established by gdopen.
c     

      status = gdclose(gdfid)

      stop
      end




