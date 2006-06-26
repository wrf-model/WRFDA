divert(-1)dnl
#
# diversion 1 is for collecting formal arguments
# diversion 2 is for extra formal arguments for string lengths
# diversion 3 is for formal argument declarations
# diversion 4 is for extra local variables derived from formal arguments
#
define(`STRING',`divert(1)ifdef(`INIT',,`, ')STRINGF(`$1')`'undefine(`INIT')divert(2)`'STRINGX(`$1')`'divert(3)`'STRINGD(`$1',`$2')`'divert(4)`'STRINGL(`$1')`'divert(0)')dnl
define(`INTEGERSTAR',`divert(1)ifdef(`INIT',,`, ')$1`'undefine(`INIT')divert(3)
    F_INTEGER		*$1;	`$2'divert(0)')dnl
define(`INTSTAR',`divert(1)ifdef(`INIT',,`, ')$1`'undefine(`INIT')divert(3)
    int		*$1;	`$2'divert(0)')dnl
define(`LONGSTAR',`divert(1)ifdef(`INIT',,`, ')$1`'undefine(`INIT')divert(3)
    long		*$1;	`$2'divert(0)')dnl
define(`FLOATSTAR',`divert(1)ifdef(`INIT',,`, ')$1`'undefine(`INIT')divert(3)
    float	*$1;	`$2'divert(0)')dnl
define(`DOUBLESTAR',`divert(1)ifdef(`INIT',,`, ')$1`'undefine(`INIT')divert(3)
    double	*$1;	`$2'divert(0)')dnl
#
# The following is for a pointer to a single character, not a Fortran 
# character variable
#
define(`CHARSTAR',`divert(1)ifdef(`INIT',,`, ')$1`'undefine(`INIT')divert(3)
    char	*$1;	`$2'divert(0)')dnl
define(`VOIDSTAR',`divert(1)ifdef(`INIT',,`, ')$1`'undefine(`INIT')divert(3)
    void	*$1;	`$2'divert(0)')dnl
define(`M4__PROTO',`define(`INIT',1)$2`'NAMEF($1)(undivert(1)undivert(2))undivert(3)')
define(`M4__LOCALS',`undivert(4)')
# To make a C external struct name visible as a Fortran named common, e.g.
# if
#    extern struct foo {int n; float y} bar_;
# is available to fortran as 
#    common/bar/n, y
# then we want this macro to append a `_' to its argument.  NAMEF works
# for this in every case we know about.
define(`M4__STRUCT_FOR_COMMON',`NAMEF($1)')dnl

# Note: override the following default definitions in OS.m4, where necessary

# Includes needed at the top of a file of C to be called from FORTRAN,
# e.g. "#include descrip" for VMS
define(`M4__STRING_DESCRIPTOR_INCLUDES',`')

# Special #defines needed for this FORTRAN, e.g. FORTRAN_HAS_NO_SHORT 
define(`M4__FORTRAN_DEFINES',`')

# FORTRAN syntax for including a file, e.g. `$include: "filename"' for msoft
define(`M4__RIGHT_QUOTE',')
define(`F_INCLUDE',`      `include' M4__RIGHT_QUOTE`'$1`'M4__RIGHT_QUOTE')

# include declaring C interfaces, needed in FORTRAN when calling C, e.g.
# Microsoft FORTRAN needs to include msoft.int
define(`M4__C_INTERFACE_DECLARATIONS',`')
divert(0)dnl
