/*********************************************************************
 *   Copyright 1992, University Corporation for Atmospheric Research
 *   See netcdf/README file for copying and redistribution conditions.
 *
 *   Purpose:	interface for classes of typed arrays for netCDF
 *
 *   $Header: /afs/ncsa/projects/hdf/cvs/hdf4/mfhdf/c++/ncvalues.hh,v 1.2 1993/04/30 20:29:42 koziol Exp $
 *********************************************************************/

#ifndef Ncvalues_def
#define Ncvalues_def

#include <generic.h>
#include <iostream.h>
#include <strstream.h>
#include <limits.h>
#include <string.h>
#include "netcdf.h"

typedef unsigned char ncbyte;

enum NcType 
{
  ncNoType = NC_UNSPECIFIED, 
  ncByte = NC_BYTE, 
  ncChar = NC_CHAR, 
  ncShort = NC_SHORT, 
  ncLong = NC_LONG, 
  ncFloat = NC_FLOAT, 
  ncDouble = NC_DOUBLE, 
  ncInt				// Note:  needed for int* overloading
};

static const ncbyte ncBad_byte = FILL_BYTE;
static const char ncBad_char = FILL_CHAR;
static const short ncBad_short = FILL_SHORT;
static const long ncBad_long = FILL_LONG;
static const float ncBad_float = FILL_FLOAT;
static const double ncBad_double = FILL_DOUBLE;

#define NcVal(TYPE) name2(NcValues_,TYPE)

#define NcValuesdeclare(TYPE)						      \
class NcVal(TYPE) : public NcValues					      \
{									      \
  public:								      \
    NcVal(TYPE)( void );						      \
    NcVal(TYPE)(long num);						      \
    NcVal(TYPE)(long num, const TYPE* vals);				      \
    NcVal(TYPE)(const NcVal(TYPE)&);					      \
    virtual NcVal(TYPE)& operator=(const NcVal(TYPE)&);			      \
    virtual ~NcVal(TYPE)( void );					      \
    virtual void* base( void ) const;					      \
    virtual int bytes_for_one( void ) const;				      \
    virtual ncbyte as_ncbyte( int n ) const;				      \
    virtual char as_char( int n ) const;				      \
    virtual short as_short( int n ) const;				      \
    virtual long as_long( int n ) const;				      \
    virtual float as_float( int n ) const;				      \
    virtual double as_double( int n ) const;				      \
    virtual char* as_string( int n ) const;				      \
  private:								      \
    TYPE* the_values;							      \
    ostream& print(ostream&) const;					      \
};

#define NcTypeEnum(TYPE) name2(_nc__,TYPE)
#define _nc__ncbyte ncByte
#define _nc__char ncChar
#define _nc__short ncShort
#define _nc__long ncLong
#define _nc__int ncInt
#define _nc__float ncFloat
#define _nc__double ncDouble
#define NcValuesimplement(TYPE)						      \
NcVal(TYPE)::NcVal(TYPE)( void )					      \
	: NcValues(NcTypeEnum(TYPE), 0), the_values(0)			      \
{}									      \
									      \
NcVal(TYPE)::NcVal(TYPE)(long num, const TYPE* vals)			      \
	: NcValues(NcTypeEnum(TYPE), num)				      \
{									      \
    the_values = new TYPE[num];						      \
    for(int i = 0; i < num; i++)					      \
      the_values[i] = vals[i];						      \
}									      \
									      \
NcVal(TYPE)::NcVal(TYPE)(long num)					      \
	: NcValues(NcTypeEnum(TYPE), num), the_values(new TYPE[num])	      \
{}									      \
									      \
NcVal(TYPE)::NcVal(TYPE)(const NcVal(TYPE)& v)				      \
{									      \
    delete[] the_values;						      \
    the_values = new TYPE[v.the_number];				      \
    for(int i = 0; i < v.the_number; i++)				      \
      the_values[i] = v.the_values[i];					      \
}									      \
									      \
NcVal(TYPE)& NcVal(TYPE)::operator=(const NcVal(TYPE)& v)		      \
{									      \
    delete[] the_values;						      \
    the_values = new TYPE[v.the_number];				      \
    for(int i = 0; i < v.the_number; i++)				      \
      the_values[i] = v.the_values[i];					      \
    return *this;							      \
}									      \
									      \
void* NcVal(TYPE)::base( void ) const					      \
{									      \
    return the_values;							      \
}									      \
									      \
NcVal(TYPE)::~NcVal(TYPE)( void )					      \
{									      \
    delete[] the_values;						      \
}


#define Ncbytes_for_one_implement(TYPE)					      \
int NcVal(TYPE)::bytes_for_one( void ) const				      \
{									      \
    return nctypelen((nc_type) NcTypeEnum(TYPE));			      \
}

#define as_ncbyte_implement(TYPE)					      \
ncbyte NcVal(TYPE)::as_ncbyte( int n ) const				      \
{									      \
    if (the_values[n] < 0 || the_values[n] > UCHAR_MAX)			      \
      return ncBad_byte;						      \
    return (ncbyte) the_values[n];					      \
}

#define as_char_implement(TYPE)						      \
char NcVal(TYPE)::as_char( int n ) const				      \
{									      \
    if (the_values[n] < CHAR_MIN || the_values[n] > CHAR_MAX)		      \
      return ncBad_char;						      \
    return (char) the_values[n];					      \
}

#define as_short_implement(TYPE)					      \
short NcVal(TYPE)::as_short( int n ) const				      \
{									      \
    if (the_values[n] < SHRT_MIN || the_values[n] > SHRT_MAX)		      \
      return ncBad_short;						      \
    return (short) the_values[n];					      \
}

#define as_long_implement(TYPE)						      \
long NcVal(TYPE)::as_long( int n ) const				      \
{									      \
    if (the_values[n] < LONG_MIN || the_values[n] > LONG_MAX)		      \
      return ncBad_long;						      \
    return (long) the_values[n];					      \
}

#define as_float_implement(TYPE)					      \
inline float NcVal(TYPE)::as_float( int n ) const			      \
{									      \
    return (float) the_values[n];					      \
}

#define as_double_implement(TYPE)					      \
inline double NcVal(TYPE)::as_double( int n ) const			      \
{									      \
    return (double) the_values[n];					      \
}

#define as_string_implement(TYPE)					      \
char* NcVal(TYPE)::as_string( int n ) const				      \
{									      \
    char* s = new char[32];						      \
    ostrstream(s, sizeof(s)) << the_values[n] << ends ;			      \
    return s;								      \
}

class NcValues			// ABC for value blocks
{
  public:
    NcValues( void );
    NcValues(NcType, long);
    virtual ~NcValues( void );
    virtual long num( void );
    virtual ostream& print(ostream&) const = 0;
    virtual void* base( void ) const = 0;
    virtual int bytes_for_one( void ) const = 0;

    // The following member functions provide conversions from the value
    // type to a desired basic type.  If the value is out of range, the
    // default "fill-value" for the appropriate type is returned.
    virtual ncbyte as_ncbyte( int n ) const = 0;   // nth value as a byte
    virtual char as_char( int n ) const = 0;   // nth value as char
    virtual short as_short( int n ) const = 0; // nth value as short
    virtual long as_long( int n ) const = 0;   // nth value as long
    virtual float as_float( int n ) const = 0; // nth value as floating-point
    virtual double as_double( int n ) const = 0; // nth value as double
    virtual char* as_string( int n ) const = 0;  // value as string
    
  protected:
    NcType the_type;
    long the_number;
    friend ostream& operator<< (ostream&, const NcValues&);
};

declare(NcValues,ncbyte)
declare(NcValues,char)
declare(NcValues,short)
declare(NcValues,long)
declare(NcValues,int)
declare(NcValues,float)
declare(NcValues,double)

#endif
