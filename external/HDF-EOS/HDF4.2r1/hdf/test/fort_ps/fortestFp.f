C $Id: fortestFp.f,v 1.2 2001/09/26 19:58:31 epourmal Exp $
C Interface to invoke tests for HDF Fortran interfaces.

	Program main
	implicit none
	include 'fortest.inc'

	integer nerror, retcode
	character cmd*15, test*30

	print *, '==========================================='
	print *, 'HDF Library Fortran Interface Tests Started'
	print *, '==========================================='

C Default to cleanup *.hdf files and set verbosity to default value
	CleanUp = .TRUE.
	CleanUpCMD = 'rm -f *.hdf'
	Verbosity = VERBO_DEF
	nerror = 0

C Open command file
        call opencmdf(retcode)
	if (retcode .ne. 0) then
	    nerror = nerror + 1
	else
C
C read and run one test command at a time
	    call getcmd(cmd, test, retcode)
	    do while (retcode .eq. 0)
		call runcmd(cmd, test, retcode)
		if (retcode .ne. 0) nerror = nerror + 1
		call getcmd(cmd, test, retcode)
	    end do
	end if

	print *, '====================================='
	if (nerror .ne. 0) then
	    print*, nerror, ' Fortran test(s) failed'
	else
	    print*, 'All Fortran Interface Tests Passed'
	endif
	print *, '====================================='

	if (CleanUp) call Hsystem(CleanUpCMD)

	stop
	end


C
C Open the test command file
C This is more general than reading from standard input file
C which is often system dependent.
C Retcode: 0 if everything is fine, else -1
C
	subroutine opencmdf(retcode)
	implicit none
	include 'fortest.inc'

	integer retcode

	retcode = 0
	open(cmdf, FILE=cmdfilename, err=100)
	return

100	call MESSAGE(VERBO_NONE, 'failed to open command file')
	retcode = -1
	return
	end

C Get a test command.
C Currently taking it from standard input.
C If EOF encounters, set retcode = 1.  Else retcode = 0.
C
	subroutine getcmd(cmd, test, retcode)
	implicit none
	include 'fortest.inc'
	
	character*(*) cmd, test
	integer retcode

	character*120 inline
	integer linelen, i

	retcode = 0

C For VMS, un-comment the next line 
C        read(5,11,END=100,err=100) inline
	read(cmdf,11,END=100,err=100) inline
C	print *, 'inline=', inline
	linelen = len(inline)
	i = index(inline, ' ')
	if (i .le. 0) i = linelen+1
	cmd = inline(1 : i - 1)

	do while (i .le. linelen)
	    if (inline(i:i) .eq. ' ') then
		i = i + 1
	    else
		goto 50
	    endif
	end do

50	test = inline(i:linelen)

	if (Verbosity .ge. VERBO_HI) then
	    print * , 'cmd=', cmd, ', test=', test
	endif

	retcode = 0
	return

C For VMS use the next three lines 
C100	continue
C        close(5)
C        retcode =1
C For VMS comment out next line
C End of file or read error on cmdfile.  Close it either way.
100     close(cmdf)
	retcode = 1
	return
C
11	format(A120)
	end
	    

C Run the Fortran test command.
C
	subroutine runcmd(cmd, param, retcode)
	implicit none
	include 'fortest.inc'
	
	character*(*) cmd, param

	integer retcode
C
	retcode = 0

C Parse command types
C
C Verbosity level command
	if (cmd .EQ. 'Verbosity' .OR. cmd .EQ. 'verbosity') then
	    Verbosity = index('0123456789', param(1:1)) - 1
	    return
	endif
	    

C Cleanup command
C If param is Yes/No, it directs to delete the *.hdf or not.
C If param is not Yes/No, it represents the system command to delete
C    the *.hdf.  (NB: specifying system command does not imply
C    to Cleanup, i.e., it does not current CleanUp setting.
C
	if (cmd .EQ. 'Cleanup' .OR. cmd .EQ. 'cleanup') then
	    if (param .EQ. 'No' .OR. param .EQ. 'no') then
		CleanUp = .FALSE.
	    else if (param .EQ. 'Yes' .OR. param .EQ. 'yes') then
		CleanUp = .TRUE.
	    else
		CleanUpCMD = param
	    endif
	    return
	endif
	    
C	print *, '====================================='
C	print *, cmd, param
C	print *, '====================================='

C Skip command
	if (cmd .EQ. 'Skip' .OR. cmd .EQ. 'skip') then
	    call ptestban('Skipping', param)
	    return
	endif

	if (cmd .NE. 'Test' .AND. cmd .NE. 'test') then
	    print *, 'Unknown Command: ', cmd, param
	    print *, 'Try one of "Skip", "Test", "Verbosity" or "Cleanup"'
	    retcode = -1
	    return
	endif

C Test command
C	if (param .EQ. 'slab') then
C	    call slabwf(retcode)
C	    return
C	endif

C	if (param .EQ. 'r24') then
C	    call t24f(retcode)
C	    return
C	endif

C	if (param .EQ. 'an') then
C	    call tanf(retcode)
C	    return
C	endif

C	if (param .EQ. 'anfile') then
C	    call tanfilef(retcode)
C	    return
C	endif

 	if (param .EQ. 'manf') then
 	    call manf(retcode)
 	    return
	endif

	if (param .EQ. 'mgrf') then
	    call mgrf(retcode)
	    return
	endif
	
	if (param .EQ. 'grcompress')  then
            call test_grcompress(retcode)
            return
        endif  

C	if (param .EQ. 'p') then
C	    call tpf(retcode)
C	    return
C	endif
C
C	if (param .EQ. 'r8') then
C	    call tr8f(retcode)
C	    return
C	endif
C
C	if (param .EQ. 'sdmms') then
C	    call tsdmmsf(retcode)
C	    return
C	endif

C	if (param .EQ. 'sdnmms') then
C	    call tsdnmmsf(retcode)
C	    return
C	endif
 
C	if (param .EQ. 'sdnnt') then
C	    call tsdnntf(retcode)
C	    return
C	endif

C	if (param .EQ. 'sdnt') then
C	    call tsdntf(retcode)
C	    return
C	endif

C	if (param .EQ. 'sdstr') then
C	    call tsdstrf(retcode)
C	    return
C	endif
 
	if (param .EQ. 'vsetf') then
	    call tvsetf(retcode)
	    return
	endif

	if (param .EQ. 'vsblock') then
	    call tvsetblock(retcode)
	    return
	endif

	if (param .EQ. 'vattrf') then
	    call tvattrf(retcode)
	    return
	endif

c	if (param .EQ. 'stubs') then
C	    call tstubsf(retcode)
C	    return
C	endif

C
	print *, 'Unknown Command: ', cmd, param
	retcode = -1
	return
	end

