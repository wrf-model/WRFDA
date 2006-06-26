C $Id: fortest.inc,v 1.5 1997/02/17 22:28:18 acheng Exp $
C Common block definiton to be included by fortestF.f modules.  If your
C fortran compile cannot do include, you need to replace each include
C statement with this file.

	common CleanUp, Verbosity, CleanUpCMD
	logical CleanUp
	character*80	CleanUpCMD
	integer Verbosity
C
C Predefined verbosity levels.
C
C Convention:
C 
C The higher the verbosity value, the more information printed.
C So, output for higher verbosity also include output of all lower
C verbosity.
C 
C     Value	Description
C     0		None:	No informational message.
C     1			"All tests passed"
C     2			Header of overall test
C     3		Default: header and results of individual test
C     4		
C     5		Low:	Major category of tests.
C     6
C     7		Medium:	Minor category of tests such as functions called.
C     8
C     9		High:	Highest level.  All information.

	integer VERBO_NONE, VERBO_DEF, VERBO_LO, VERBO_MED, VERBO_HI
	parameter (
     +		VERBO_NONE = 0,
     +		VERBO_DEF  = 3,
     +		VERBO_LO   = 5,
     +		VERBO_MED  = 7,
     +		VERBO_HI   = 9
     +		)

	integer FALSE, TRUE
	parameter (
     +		FALSE = 0,
     +		TRUE = 1
     +		)

C Tests command file parameters.
C cmdf: file unit number
      integer      cmdf
      character*30 cmdfilename
      parameter (cmdf = 1)
      parameter (cmdfilename = 'fortest.arg')
