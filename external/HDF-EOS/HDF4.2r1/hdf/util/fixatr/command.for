	subroutine command_parse
c
	implicit	integer*4(a-z)
	integer*4	len1,index,len2
	include		'common.for/list'
c
c	call cli routines to get command line
c
	call cli$get_value('filespec',filename)
c
	istat = str$trim (filename, filename, len)
c
c	model file is currently not implemented.
c
	if (cli$present('model')) then
		mdl_flag = 1
	type *,'%FIXATR-I-NOTIMP, currently not implemented.'
		call exit
	endif
c
	if (cli$present('rfm')) then
		call cli$get_value ('rfm',rfm_val)
	else 
		rfm = 'VARIABLE'
		goto 100
	endif
c
c	parse rfm qualifier
c
		index = str$position (rfm_val, ':')
c
c	value for rfm present?
c
		if (index .ne. 0) then
		     rfm = rfm_val(1:index-1)
		     size = rfm_val(index+1:)
		else
		     rfm = rfm_val
		     size = ' '
		endif
c
		istat = str$trim (rfm,rfm,len2)
		if (.not. istat) call lib$stop (%val(istat))
c
		istat = str$trim (size,size,len1)
		if (.not. istat) call lib$stop (%val(istat))

c
	if((size(1:2).eq.'CR').or.(size(1:2).eq.'LF')) goto 50  ! cjfm
	if (size(1:2) .ne.' ') then                             ! cjfm
		  if (len1 .eq.1) then
		    isize = ichar ( size ) - 48
		  else
		    istat=lib$cvt_dtb(%val(len1),%ref(size(1:len1)),isize)
        	  endif
  	else if (size .eq. ' ') then
  		  isize = 0
        else
  		  type *,'%FIXATR-E-SYNERR, command syntax error.'
  	          call exit
  	endif
c
c	assign value for variable recl.
c
50		i = lib$matchc (%descr(rfm(1:len2)),'VARIABLE')
		if (i .eq. 1) then
			rfm = 'VARIABLE'
			if (isize .ne. 0) then
			     	mrs = isize
			     	rs  = 0
			endif		
		goto 100
		endif
c
c 	assign value for fixed recl.
c
		i =lib$matchc (%descr(rfm(1:len2)),'FIXED')
		if (i .eq. 1) then
		    rfm = 'FIXED'
		    if (isize .ne. 0) then
			mrs = isize
			rs  = isize
		    else 
			type *,'%FIXATR-E-SYNERR, command syntax error.'
			call exit
		     endif
		goto 100
		endif
c
c	assign value for stream rec type.
c
		i = lib$matchc (%descr(rfm(1:len2)),'STREAM')
		if (i .eq. 1) then
			if (size .eq. 'CR') then
		     	    rfm = 'STREAMCR'
	          	else if (size .eq. 'LF') then
	     		          rfm = 'STREAMLF'
		     	     else if (size .eq. ' ') then
				      rfm = 'STREAM'
				  else
			type *,'%FIXATR-E-SYNERR, command syntax error.'
				      call exit
			endif
		goto 100
		endif
c
c	assign value for VFC records.
c
	      	i = lib$matchc (%descr(rfm (1:len2)), 'VFC') 
	   	if (i .eq. 1) then
		       rfm = 'VFC'
		       if (isize .ne. 0) then
		           vfcsize = isize
			   goto 100
		       else
		type *,'%FIXATR-E-SYNERR, command syntax error.'
	     		   call exit
		        endif
	       	else
		type *,'%FIXATR-E-SYNERR, command syntax error.'
			call exit
		endif
c
100	if (cli$present('rat')) then
		call cli$get_value('rat',rat)
		istat = str$trim (rat,rat,len1)
		i = lib$matchc (%descr(rat(1:len1)),'FORTRANCC')
		if (i .eq. 1) then
		    rat = 'FORTRANCC'
		else
		    i = lib$matchc (%descr(rat(1:len1)),'IMPLIEDCC')
		    if (i .eq. 1) then
			rat = 'IMPLIEDCC'
		    else
		        i = lib$matchc (%descr(rat(1:len1)),'PRINTCC')
			if (i .eq. 1) then
			    rat = 'PRINTCC'
			else
			    i = lib$matchc (%descr(rat(1:len1)),'NOSPAN')
			    if (i .eq. 1) then
				rat = 'NOSPAN'
			    else
				i = lib$matchc (rat(1:len1),'NONE')
				if (i .eq. 1) then
				    rat = 'NONE'
				else
		type *,'%FIXATR-E-SYNERR, command syntax error.'
				call exit
				endif
			    endif
			endif
		     endif
		endif
	else
		rat = 'IMPLIEDCC'		! default
	endif
c
c	parse org qualifier
c
	if (cli$present('org')) then
		call cli$get_value('org',org_val)
		istat = str$trim (org_val,org_val,len1)
		index = str$position (org_val, ':')
	else
		org = 'SEQUENTIAL'
		goto 200
	endif	
c
c	value for org present?
c
	if (index .ne. 0) then
	     org = org_val(1:index-1)
	     size = org_val(index+1:)
	else
	     org = org_val
	     size = ' '
	endif
c
	istat = str$trim (org,org,len2)
	if (.not. istat) call lib$stop (%val(istat))
c
	istat = str$trim (size,size,len1)
	if (.not. istat) call lib$stop (%val(istat))

	i = lib$matchc (%descr(org(1:len2)),'SEQUENTIAL')
	if (i .eq. 1) then
	    org = 'SEQUENTIAL'
		goto 200
	endif

     	i = lib$matchc (%descr(org(1:len2)),'INDEXED')
        if (i .eq. 1) then
	    org = 'INDEXED'
	    if (size .eq. ' ') then
		type *, '%FIXATR-E-SYNERR, command syntax error'
                call exit
	    endif
            if (len1 .eq.1) then
	         isize = ichar ( size ) -48
		 bktsize = isize
     	    else
	     istat = lib$cvt_dtb (%val(len1),%ref(size(1:len1)),isize)
		 bktsize = isize
	    endif
		goto 200
	endif

        i = lib$matchc (%descr(org(1:len2)),'RELATIVE')
        if  (i .eq. 1) then
             org = 'RELATIVE'
	    if (size .eq. ' ') then
		type *, '%FIXATR-E-SYNERR, command syntax error'
                call exit
	    endif
            if (len1 .eq.1) then
	         isize = ichar ( size ) -48
		 bktsize = isize
     	    else
	     istat = lib$cvt_dtb (%val(len1),%ref(size(1:len1)),isize)
		 bktsize = isize
	    endif
	    goto 200
	endif

	i = lib$matchc (%descr(org(1:len2)),'DIRECT')
	if (i .eq. 1) then
            org = 'DIRECT'
		goto 200
        else
	     type *, '%FIXATR-E-SYNERR, command syntax error'
             call exit
        endif
c
	
200	if (cli$present('efblk')) then
		call cli$get_value('efblk',efblk)
		istat = str$trim (efblk,efblk,len1)
		istat = lib$cvt_dtb (%val(len1),%ref(efblk(1:len1)),eof_blk)
	endif
c
	if (cli$present('ffbyte')) then
		call cli$get_value('ffbyte',ffbyte)
		istat = str$trim (ffbyte,ffbyte,len1)
		istat = lib$cvt_dtb (%val(len1),%ref(ffbyte(1:len1)),ff_byte)
	endif
d	type *, 'bktsize = ',bktsize
d	type *, 'vfcsize = ',vfcsize
d	type *, 'rfm = ', rfm
d	type *, 'mrs = ', mrs
d	type *, 'rs  = ', rs
d	type *, 'rat = ', rat
d	type *, 'org = ', org
d	type *, 'efblk = ', eof_blk
d	type *, 'ffbyte= ', ff_byte

	return
	end
		
