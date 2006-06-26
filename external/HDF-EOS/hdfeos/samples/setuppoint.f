c
c  In this example we will (1) open an HDF file, and (2) create three point
c  interfaces within the file.
c

      program setuppoint

      integer*4          ptfid, ptid1, ptid2, ptid3, ptdetach, ptclose
      integer*4          ptopen, ptcreate
      
      integer DFACC_CREATE
      parameter (DFACC_CREATE=4)

c   
c      We first open the HDF swath point, "PointFile.hdf".  Because this
c      file does not already exist, we use the DFACC_CREATE access
c      code in the open statement.  The ehopen routine returns the point
c      file id, ptfid, which is used to identify the file in subsequent
c      routines in the library.
c
      ptfid = ptopen("PointFile.hdf", DFACC_CREATE)


      ptid1 = ptcreate(ptfid, "Simple Point")
      ptid2 = ptcreate(ptfid, "FixedBuoy Point")
      ptid3 = ptcreate(ptfid, "FloatBuoy Point")

c    
c     We now close the point interface with the ptdetach routine.  This step
c     is necessary to properly store the point information within the file.
c     

      status = ptdetach(ptid1)
      status = ptdetach(ptid2)
      status = ptdetach(ptid3)

c    
c     Finally, we close the point file using the ehclose routine.  This will
c     release the point file handles established by ptopen.
c     

      status = ptclose(ptfid)

      stop
      end



