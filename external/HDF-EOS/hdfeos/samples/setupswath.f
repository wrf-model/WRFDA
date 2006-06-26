c
c  In this example we will (1) open an HDF file, (2) create the swath
c  interface within the file and (3) define the swath field dimensions.
c

      program setupswath

      integer            status, swdefdim, swdefmap, swdefimap
      integer            swdetach, swclose
      integer*4          swfid, swid, swopen, swcreate
      integer*4          indx(12) /0,1,3,6,7,8,11,12,14,24,32,39/

      integer DFACC_CREATE
      parameter (DFACC_CREATE=4)
      integer SD_UNLIMITED
      parameter (SD_UNLIMITED=0)      

c   
c      We first open the HDF swath file, "SwathFile.hdf".  Because this
c      file does not already exist, we use the DFACC_CREATE access
c      code in the open statement.  The SWopen routine returns the swath
c      file id, swfid, which is used to identify the file in subsequent
c      routines in the library.
c
      swfid = swopen('SwathFile.hdf',DFACC_CREATE)

c    
c     The first of these, SWcreate, creates the swath, "Swath1", within the
c     file designated by the file id, swfid.  It returns the swath id, SWid,
c     which identifies the swath in subsequent routines.  We will show how
c     to define, write and read field swaths in later programs.
c     
      swid = swcreate(swfid, "Swath1")


c    
c     Typically, many fields within a swath share the same dimension. The
c     swath interface therefore provides a way of defining dimensions that
c     will then be used to define swath fields.  A dimension is defined with
c     a name and a size and is connected to the particular swath through the
c     swath id.  In this example, we define the geo- location track and
c     cross track dimensions with size 20 and 10 respectively and two
c     dimensions corresponding to these but with twice the resolution.
c     We also define a dimension corresponding to a number of spectral
c     bands.

      status = swdefdim(swid, "GeoTrack", 20)
      status = swdefdim(swid, "GeoXtrack", 10)

      status = swdefdim(swid, "Res2tr", 40)
      status = swdefdim(swid, "Res2xtr", 20)
      status = swdefdim(swid, "Bands", 15)
      status = swdefdim(swid, "IndxTrack", 12)
      
c     Define Unlimited (appendable) dimension
      status = swdefdim(swid, "Unlim", SD_UNLIMITED)
      
c   
c     Once the dimensions are defined, the relationship (mapping)between the
c     geolocation dimensions, such as track and cross track, and the data
c     dimensions, must be established.  This is done through the SWdefdimmap
c     routine.  It takes as input the swath id, the names of the dimensions
c     designating the geolocation and data dimensions, respectively, and the
c     offset and increment defining the relation.
c     
c     In the first example we relate the "GeoTrack" and "Res2tr" dimensions
c     with an offset of 0 and an increment of 2.  Thus the ith element of
c     "Geotrack" corresponds to the 2 * ith element of "Res2tr".
c   
c     In the second example, the ith element of "GeoXtrack" corresponds to the
c     2 * ith + 1 element of "Res2xtr".
c     

      status = swdefmap(swid, "GeoTrack", "Res2tr", 0, 2)
      status = swdefmap(swid, "GeoXtrack", "Res2xt", 1, 2)

      status = swdefimap(swid, "IndxTrack", "Res2tr", indx)

c    
c     We now close the swath interface with the SWdetach routine.  This step
c     is necessary to properly store the swath information within the file.
c     
      status = swdetach(swid)


c    
c     Finally, we close the swath file using the SWclose routine.  This will
c     release the swath file handles established by SWopen.
c     

      status = swclose(swfid)

      stop
      end




