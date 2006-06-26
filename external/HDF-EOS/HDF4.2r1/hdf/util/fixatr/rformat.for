	program recformat
c
c	This program will prompt the user for proper record attributes 
c	to be applied to the file header of interest.
c
	implicit	integer*4 (a-z)
c
	include		'($iodef)'
c
c	include storage for parsed command line.
c
	include		'common.for/list'
c
c	$fibdef
c
	parameter	(fib$m_writethru=	'80000'x)
c
c	$fatdef.
c
	parameter 	(fat$c_fixed	=	'1'x)
	parameter 	(fat$c_variable	=	'2'x)
	parameter 	(fat$c_vfc	=	'3'x)
	parameter 	(fat$c_stream	=	'4'x)
	parameter 	(fat$c_streamlf =	'5'x)
	parameter 	(fat$c_streamcr	=	'6'x)
	parameter	(fat$m_fortrancc=	'1'x)
	parameter	(fat$m_printcc	=	'4'x)
	parameter	(fat$m_impliedcc=	'2'x)
	parameter	(fat$m_nospan	=	'8'x)
	parameter	(fat$c_direct	=	'3'x)
	parameter	(fat$c_indexed	=	'2'x)
	parameter	(fat$c_relative	=	'1'x)
	parameter	(fat$c_sequential=	'0'x)
	parameter	(fat$v_rtype	=	'0'x)
	parameter	(fat$v_fileorg	=	'4'x)
	parameter	(fat$b_vfcsize	=	'f'x)
	parameter	(fat$w_maxrec	=	'10'x)
	parameter	(fat$w_rsize	=	'2'x)
	parameter	(fat$l_efblk	=	'8'x)
	parameter	(fat$w_ffbyte	=	'c'x)
	parameter	(fat$w_vfcsize	=	'f'x)
	parameter	(fat$w_bktsize	=	'e'x)
c
c	$atrdef
c
	parameter	(atr$c_recattr	=	'4'x)
	parameter	(atr$s_recattr	=	'20'x)
c
	character*255	res_str
	integer*2	fid_blk(3), did_blk(3),iosb(4)
	character*64	device_name
	integer*4	device_len
c
c	p1 for qio.
c
	integer*4	fib_desc(2)
	integer*2	fib_desc_w(4)
	equivalence	(fib_desc,fib_desc_w)
	integer*2	fib(11)
	integer*4	fib_l
	equivalence	(fib_l,fib)
c	
c	p5 for qio.
c
	integer*4	attr_desc(3)
	integer*2	attr_desc_w(6)
	equivalence	(attr_desc,attr_desc_w)
	integer*4	attr(8)
	integer*2	attr_w(16)
	byte		attr_b(32)
	equivalence	(attr,attr_b)
	equivalence	(attr_w,attr_b)
c
c	Build p1.
c
	data	fib_desc_w(1)/22/
	fib_desc(2)=%loc(fib)
c
c	Build p5.
c
	data	attr_desc_w(1)/atr$s_recattr/
	data	attr_desc_w(2)/atr$c_recattr/
	attr_desc(2)=%loc(attr)
	attr_desc(3)=0
c
c	call command_parse to get command line
c
	call command_parse	
c
c	call macro routine parse to obtain DID and FID
c
	istat = parse (filename,%ref(fid_blk),%ref(did_blk),res_str)
	if (.not.istat) call lib$stop(%val(istat))
d	type *,' fid= ',fid_blk(1),fid_blk(2),fid_blk(3)
d	type *,' did= ',did_blk(1),did_blk(2),did_blk(3)
	device_len = index(res_str,':')
	device_name = res_str(1:device_len)
d	type *,' Device= ',device_name(1:device_len)
c
c	build dib. fid and did.
c
	fib(1) = 0
	fib(2) = 0
	fib(3) = fid_blk(1)
	fib(4) = fid_blk(2)
	fib(5) = fid_blk(3)
c	fib(6) = did_blk(1)
c	fib(7) = did_blk(2)
c	fib(8) = did_blk(3)
	fib(9) = 0
	fib(10) = 0
	fib(11) = 0
	fib_l = fib$m_writethru
c	
c	assign channel
c
c	istat = sys$assign('sys$disk:',chan,,)
	istat = sys$assign(device_name(1:device_len),chan,,)
	if (.not. istat) call lib$stop (%val(istat))
c
c
c	Build record attribute block using the io$_access func and
c	then change the attributes based on the input from user.
c
	istat = sys$qiow(,%val(chan),%val(io$_access),iosb,,,fib_desc
	1		,,,,attr_desc,)
	if (.not. istat) call lib$stop (%val(istat))
	if (.not. iosb(1)) call lib$signal (%val(iosb(1)))
c
	if (rfm .eq. 'VARIABLE') then 
		attr_b(1)=fat$c_variable
		if (rs .ne. 0 .or. mrs .ne. 0) then
		     	attr_w(2) = rs
		     	attr_w(9) = mrs
		endif
	else if	(rfm .eq. 'FIXED') then 
		   attr_b(1)=fat$c_fixed
		   if (rs .ne. 0 .and. mrs .ne. 0) then
		   	attr_w(2) = rs
		   	attr_w(9) = mrs
		   endif	
	     else if (rfm .eq. 'STREAM') then
			attr_b(1)=fat$c_stream
		  else if (rfm .eq. 'VFC') then
			    attr_b(1)=fat$c_vfc
			    attr_b(16) = vfcsize
		       else if (rfm .eq. 'STREAMLF') then
			       attr_b(1)=fat$c_streamlf
			    else if (rfm .eq. 'STREAMCR') then
				     attr_b(1)=fat$c_streamcr
				 else
				     attr(1)=0
	end if
c
	if (rat .eq. 'NONE') then
		attr_b(2)= 0
	else if (rat .eq. 'PRINTCC') then
	            attr_b(2) = fat$m_printcc
	     else if (rat .eq. 'FORTRANCC') then
		        attr_b(2) = fat$m_fortrancc
		  else if (rat .eq. 'IMPLIEDCC') then
				attr_b(2) = fat$m_impliedcc
		       else if (rat .eq. 'NOSPAN') then
				     attr_b(2) = fat$m_nospan
			    else
   				     attr_b(2) = 0
	endif	
c
	if (org .eq. 'SEQUENTIAL') then
	    attr_b(1) = attr_b(1) + fat$c_sequential*2**fat$v_fileorg
	else if (org .eq. 'INDEXED') then
		 attr_b(1) = attr_b(1) + fat$c_indexed*2**fat$v_fileorg
		 attr_b(15) = bktsize
	     else if (org .eq. 'RELATIVE') then
		      attr_b(1)=attr_b(1)+fat$c_relative*2**fat$v_fileorg
		      attr_b(15) = bktsize
		  else
		      attr_b(1)=attr_b(1)+fat$c_direct*2**fat$v_fileorg
	endif
c
	if (eof_blk .ne. 0) attr(3) = eof_blk
c
	if (ff_byte .ne. 0) attr_w(7) = ff_byte
c
c	Now perform the QIOW with IO$_MODIFY as function code.
c
	istat = sys$qiow(,%val(chan),%val(io$_modify),iosb,,,fib_desc
	1		,,,,attr_desc,)
	if (.not. istat) call lib$stop (%val(istat))
	if (.not. iosb(1)) call lib$signal (%val(iosb(1)))
c
	istat = sys$dassgn(%val(chan))
	if (.not. istat) call lib$stop (%val(istat))
	call exit
999	type *,' iostat = ', iso
	end
