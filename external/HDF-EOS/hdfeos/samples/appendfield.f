
c
c  In this example we will write to an appendable field.
c


      program appendfield

      integer            status, i, swfldinfo, swdetach, swclose
      integer            swwrfld, swrdfld
      integer*2          inarray(5), outarray(20)
      integer*4          swfid, swid, rank, dims(1), ntype
      integer*4          start(1), stride(1), count(1)
      integer*4          swopen, swattach
      character*72       dimlist
      
      integer*4          DFACC_RDWR
      parameter (DFACC_RDWR=3)
      
      
      do i=1,5
         inarray(i) = i
      enddo
      
      swfid = swopen("SwathFile.hdf", DFACC_RDWR)
      swid = swattach(swfid, "Swath1")


c     Write 5 records to field
c     ------------------------
      start(1) = 0
      stride(1) = 1
      count(1) = 5
      status = swwrfld(swid, "Count", start, stride, count, inarray)

      status = swfldinfo(swid, "Count", rank, dims, ntype, dimlist)
      write(*,*) 'Number of elements after first write:' ,dims(1)



c     Append 1 record to field
c     ------------------------
      start(1) = dims(1)
      count(1) = 1
      status = swwrfld(swid, "Count", start, stride, count, inarray)
      status = swfldinfo(swid, "Count", rank, dims, ntype, dimlist)
      write(*,*) 'Number of elements after append:' ,dims(1)

      start(1) = 0
      count(1) = dims(1)
      status = swrdfld(swid, "Count", start, stride, dims, outarray)    
      do i=1,dims(1)
         write(*,*) 'Data Element:', outarray(i)
      enddo

      status = swdetach(swid)
      status = swclose(swfid)

      stop
      end

