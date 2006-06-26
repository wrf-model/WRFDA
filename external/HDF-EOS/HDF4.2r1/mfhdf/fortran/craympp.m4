divert(-1)

# Name of system platform (for use in comments)
define(`M4__SYSTEM', UNICOS)

# Includes needed at the top of a file of C to be called from FORTRAN
define(`M4__STRING_DESCRIPTOR_INCLUDES',`
#include <fortran.h>	/* for _fcd functions */
')

# Special #defines needed for this FORTRAN, e.g. FORTRAN_HAS_NO_SHORT 
define(`M4__FORTRAN_DEFINES',`
#define FORTRAN_HAS_NO_BYTE
#define FORTRAN_HAS_NO_SHORT
')

# transformation from fortran name to name of C module
# for unicos, just convert to uppercase
define(`NAMEF',
       `translit($1,abcdefghijklmnopqrstuvwxyz,ABCDEFGHIJKLMNOPQRSTUVWXYZ)')

# transformation from string name to corresponding argument name
define(`STRINGF',`$1d')

# extra arguments, if any, for string length
define(`STRINGX',`')

# declaration to be used for argument name descriptor
define(`STRINGD',`
    _fcd	$1d;	`$2'') # declare string parameter as type _fcd

# declarations and initializations of canonical local variables
define(`STRINGL',`
    char	*$1	= _fcdtocp ($1d);
    unsigned	$1len	= _fcdlen ($1d);')	# use _fcd functions

# C integral type equivalent to a FORTRAN INTEGER
define(`F_INTEGER',`int') 

# FORTRAN declaration for a long integer (e.g. integer*4 for Microsoft)
define(`LONG_INT',`integer')

# FORTRAN declaration for a short integer (e.g. integer*2)
define(`SHORT_INT',`integer')

# FORTRAN declaration for an integer byte (e.g. integer*1 or byte)
define(`BYTE_INT',`integer')

# FORTRAN declaration for double precision (e.g. real for a Cray)
define(`DOUBLE_PRECISION',`real')

divert(0)dnl
