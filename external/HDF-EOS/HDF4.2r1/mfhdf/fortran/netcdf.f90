!      
!     Functions in the FORTRAN interface

      integer nccre, ncopn, ncddef, ncdid, ncvdef, ncvid, nctlen
      integer ncsfil
      external nccre, ncopn, ncddef, ncdid, ncvdef, ncvid, nctlen
      external ncsfil

      integer NCRDWR,NCCREAT,NCEXCL,NCINDEF,NCNSYNC,NCHSYNC
      integer NCNDIRTY,NCHDIRTY,NCLINK,NCNOWRIT,NCWRITE
      integer NCCLOB,NCNOCLOB,NCGLOBAL,NCFILL,NCNOFILL
      integer MAXNCOP,MAXNCDIM,MAXNCATT,MAXNCVAR
      integer MAXNCNAM,MAXVDIMS,NCNOERR,NCEBADID
      integer NCENFILE,NCEEXIST,NCEINVAL,NCEPERM,NCENOTIN
      integer NCEINDEF,NCECOORD,NCEMAXDS,NCENAME
      integer NCENOATT,NCEMAXAT,NCEBADTY,NCEBADD, NCESTS
      integer NCEUNLIM,NCEMAXVS,NCENOTVR,NCEGLOB,NCENOTNC
      integer NCFOOBAR,NCSYSERR,NCFATAL,NCVERBOS, NCENTOOL
      
      integer NCBYTE,NCCHAR,NCSHORT,NCLONG,NCFLOAT,NCDOUBLE
      
      parameter(NCBYTE = 1)
      parameter(NCCHAR = 2)
      parameter(NCSHORT = 3)
      parameter(NCLONG = 4)
      parameter(NCFLOAT = 5)
      parameter(NCDOUBLE = 6)
      
!     
!     masks for the struct NC flag field; passed in as 'mode' arg to
!     nccreate and ncopen.
!     
      
!     read/write, 0 => readonly 
      parameter(NCRDWR = 1)
!     in create phase, cleared by ncendef 
      parameter(NCCREAT = 2)
!     on create destroy existing file 
      parameter(NCEXCL = 4)
!     in define mode, cleared by ncendef 
      parameter(NCINDEF = 8)
!     synchronise numrecs on change (X'10')
      parameter(NCNSYNC = 16)
!     synchronise whole header on change (X'20')
      parameter(NCHSYNC = 32)
!     numrecs has changed (X'40')
      parameter(NCNDIRTY = 64)	
!     header info has changed (X'80')
      parameter(NCHDIRTY = 128)
!     prefill vars on endef and increase of record, the default behavior
      parameter(NCFILL = 0)
!     don't fill vars on endef and increase of record (X'100')
      parameter(NCNOFILL = 256)
!     isa link (X'8000')
      parameter(NCLINK = 32768)
      
!     
!     'mode' arguments for nccreate and ncopen
!     
      
      parameter(NCNOWRIT = 0)
      parameter(NCWRITE = NCRDWR)
      parameter(NCCLOB = 11)
      parameter(NCNOCLOB = 15)
!     
!     'size' argument to ncdimdef for an unlimited dimension
!     
      integer NCUNLIM
      parameter(NCUNLIM = 0)
      
!     
!     attribute id to put/get a global attribute
!     
      parameter(NCGLOBAL  = 0)
!     
!     Advisory Maximums
!     
      parameter(MAXNCOP = 32)
      parameter(MAXNCDIM = 32)
      parameter(MAXNCATT = 512)
      parameter(MAXNCVAR = 512)
!     Not enforced 
      parameter(MAXNCNAM = 128)
      parameter(MAXVDIMS = MAXNCDIM)
      
      
!     
!     The netcdf data types
!     
      
!     
!     Global netcdf error status variable
!     Initialized in error.c
!     
      
!     No Error 
      parameter(NCNOERR = 0)
!     Not a netcdf id 
      parameter(NCEBADID = 1)
!     Too many netcdfs open 
      parameter(NCENFILE = 2)	
!     netcdf file exists && NCNOCLOB
      parameter(NCEEXIST = 3)
!     Invalid Argument 
      parameter(NCEINVAL = 4)
!     Write to read only 
      parameter(NCEPERM = 5)
!     Operation not allowed in data mode 
      parameter(NCENOTIN = 6)	
!     Operation not allowed in define mode 
      parameter(NCEINDEF = 7)	
!     Coordinates out of Domain 
      parameter(NCECOORD = 8)
!     MAXNCDIMS exceeded 
      parameter(NCEMAXDS = 9)
!     String match to name in use 
      parameter(NCENAME = 10)	
!     Attribute not found 
      parameter(NCENOATT = 11)
!     MAXNCATTRS exceeded 
      parameter(NCEMAXAT = 12)
!     Not a netcdf data type 
      parameter(NCEBADTY = 13)
!     Invalid dimension id 
      parameter(NCEBADD = 14)	
!     NCUNLIMITED in the wrong index 
      parameter(NCEUNLIM = 15)
!     MAXNCVARS exceeded 
      parameter(NCEMAXVS = 16)
!     Variable not found 
      parameter(NCENOTVR = 17)
!     Action prohibited on NCGLOBAL varid 
      parameter(NCEGLOB = 18)
!     Not a netcdf file 
      parameter(NCENOTNC = 19)
      parameter(NCESTS = 20)
      parameter (NCENTOOL = 21)	
      parameter(NCFOOBAR = 32)
      parameter(NCSYSERR = -1)
      
      
!     
!     Global options variable. Used to determine behavior of error handler.
!     Initialized in lerror.c
!     
      parameter(NCFATAL = 1)
      parameter(NCVERBOS = 2)
