	character	org*12,org_val*12,rfm_val*20,rfm*13,size*5
	character	rat*9,efblk*10,ffbyte*3,filename*80,model*20
	integer*2	mrs,rs,ff_byte,isize
	integer*4	eof_blk
	byte		vfcsize,mdl_flag,bktsize
	common/clibuf/eof_blk,mrs,rs,ff_byte,isize,bktsize,
	1	      vfcsize,
	1	      mdl_flag,rfm_val,
	1	      rfm,filename,model,size,ffbyte,org,efblk,rat,
	1	      org_val
