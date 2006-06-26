/*********************************************************************
 *   Copyright 1992, University Corporation for Atmospheric Research
 *   See netcdf/README file for copying and redistribution conditions.
 *
 *   Purpose:	implementation of classes of typed arrays for netCDF
 *
 *   $Header: /afs/ncsa/projects/hdf/cvs/hdf4/mfhdf/c++/ncvalues.cc,v 1.2 1993/04/30 20:29:40 koziol Exp $
 *********************************************************************/

#include <iostream.h>		// for debugging
#include "ncvalues.hh"

NcValues::NcValues( void ) : the_number(0), the_type(ncNoType)
{}

NcValues::NcValues(NcType type, long num)
	: the_number(num), the_type(type)
{}

NcValues::~NcValues( void )
{}

long NcValues::num( void )
{
    return the_number;
}    

ostream& operator<< (ostream& os, const NcValues& vals)
{
    return vals.print(os);
}

implement(NcValues,ncbyte)
implement(NcValues,char)
implement(NcValues,short)
implement(NcValues,long)
implement(NcValues,int)
implement(NcValues,float)
implement(NcValues,double)

Ncbytes_for_one_implement(ncbyte)
Ncbytes_for_one_implement(char)
Ncbytes_for_one_implement(short)
Ncbytes_for_one_implement(long)
Ncbytes_for_one_implement(float)
Ncbytes_for_one_implement(double)

int NcValues_int::bytes_for_one( void ) const
{
    return nctypelen( NC_LONG ); // *** treat "int" as "long" for now
}

as_ncbyte_implement(short)
as_ncbyte_implement(int)
as_ncbyte_implement(long)
as_ncbyte_implement(float)
as_ncbyte_implement(double)

inline ncbyte NcValues_char::as_ncbyte( int n ) const
{
    return the_values[n] < 0 ? ncBad_byte : (ncbyte) the_values[n];
}

inline ncbyte NcValues_ncbyte::as_ncbyte( int n ) const
{
    return the_values[n];
}

as_char_implement(short)
as_char_implement(int)
as_char_implement(long)
as_char_implement(float)
as_char_implement(double)

inline char NcValues_ncbyte::as_char( int n ) const
{
    return the_values[n] > CHAR_MAX ? ncBad_char : (char) the_values[n];
}

inline char NcValues_char::as_char( int n ) const
{
    return the_values[n];
}

as_short_implement(int)
as_short_implement(long)
as_short_implement(float)
as_short_implement(double)

inline short NcValues_ncbyte::as_short( int n ) const
{
    return the_values[n];
}

inline short NcValues_char::as_short( int n ) const
{
    return the_values[n];
}

inline short NcValues_short::as_short( int n ) const
{
    return the_values[n];
}

as_long_implement(float)
as_long_implement(double)

inline long NcValues_ncbyte::as_long( int n ) const
{
    return the_values[n];
}

inline long NcValues_char::as_long( int n ) const
{
    return the_values[n];
}

inline long NcValues_short::as_long( int n ) const
{
    return the_values[n];
}

inline long NcValues_int::as_long( int n ) const
{
    return the_values[n];
}

inline long NcValues_long::as_long( int n ) const
{
    return the_values[n];
}

as_float_implement(ncbyte)
as_float_implement(char)
as_float_implement(short)
as_float_implement(int)
as_float_implement(long)
as_float_implement(float)
as_float_implement(double)

as_double_implement(ncbyte)
as_double_implement(char)
as_double_implement(short)
as_double_implement(int)
as_double_implement(long)
as_double_implement(float)
as_double_implement(double)

as_string_implement(short)
as_string_implement(int)
as_string_implement(long)
as_string_implement(float)
as_string_implement(double)

inline char* NcValues_ncbyte::as_string( int n ) const
{
    return strdup((char*)the_values + n);
}

inline char* NcValues_char::as_string( int n ) const
{
    return strdup(the_values + n);
}

ostream& NcValues_short::print(ostream& os) const
{
    for(int i = 0; i < the_number - 1; i++)
      os << the_values[i] << ", ";
    if (the_number > 0)
      os << the_values[the_number-1] ;
    return os;
}

ostream& NcValues_long::print(ostream& os) const
{
    for(int i = 0; i < the_number - 1; i++)
      os << the_values[i] << ", ";
    if (the_number > 0)
      os << the_values[the_number-1] ;
    return os;
}

ostream& NcValues_int::print(ostream& os) const
{
    for(int i = 0; i < the_number - 1; i++)
      os << the_values[i] << ", ";
    if (the_number > 0)
      os << the_values[the_number-1] ;
    return os;
}

ostream& NcValues_ncbyte::print(ostream& os) const
{
    for(int i = 0; i < the_number - 1; i++)
      os << the_values[i] << ", ";
    if (the_number > 0)
      os << the_values[the_number-1] ;
    return os;
}

ostream& NcValues_char::print(ostream& os) const
{
    os << '"' << the_values <<'"';
    return os;
}

ostream& NcValues_float::print(ostream& os) const
{
    long save=os.flags();
    os.precision(7);
    for(int i = 0; i < the_number - 1; i++)
      os << the_values[i] << ", ";
    if (the_number > 0)
      os << the_values[the_number-1] ;
    os.flags(save);
    return os;
}

ostream& NcValues_double::print(ostream& os) const
{
    long save=os.flags();
    os.precision(15);
    for(int i = 0; i < the_number - 1; i++)
      os << the_values[i] << ", ";
    if (the_number > 0)
      os << the_values[the_number-1];
    os.flags(save);
    return os;
}
