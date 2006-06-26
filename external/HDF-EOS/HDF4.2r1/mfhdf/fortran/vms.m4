divert(-1)

# Name of system platform (for use in comments)
define(`M4__SYSTEM', VMS)

# Includes needed at the top of a file of C to be called from FORTRAN
define(`M4__STRING_DESCRIPTOR_INCLUDES',
`#include descrip'
)

# transformation from fortran name to name of C module
define(`NAMEF',`$1')	# for vms, just use same name

# transformation from string name to corresponding argument name
define(`STRINGF',`$1d')	# append d for argument name descriptor

# extra arguments, if any, for string length
define(`STRINGX',`')

# declaration to be used for argument name descriptor
define(`STRINGD',`
    struct dsc$descriptor_s * $1d;	`$2'')

# declarations and initializations of canonical local variables
define(`STRINGL',`
    char	*$1	= $1d->dsc$a_pointer;
    int		$1len	= $1d->dsc$w_length;') # use descriptor components

# C integral type equivalent to a FORTRAN INTEGER
define(`F_INTEGER',`int') 

# FORTRAN declaration for a long integer (e.g. integer*4 for Microsoft)
define(`LONG_INT',`integer')

# FORTRAN declaration for a short integer (e.g. integer*2)
define(`SHORT_INT',`integer*2')

# FORTRAN declaration for an integer byte (e.g. integer*1 or byte)
define(`BYTE_INT',`byte')

# FORTRAN declaration for double precision (e.g. real for a Cray)
define(`DOUBLE_PRECISION',`double precision')

divert(0)dnl
