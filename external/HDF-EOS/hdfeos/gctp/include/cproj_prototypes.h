/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 1999, Raytheon Systems Company, its vendors, */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*****************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:

	cproj_prototypes.h

DESCRIPTION:

	This file contains function prototypes that are specific to the
	GCT Tools

AUTHOR:
	Ray Milburn / Steven Myers & Associates

HISTORY:
	28-Jan-99 RM Initial version
        02-Jul-03 Abe Taaheri Modified

END_FILE_PROLOG:
*****************************************************************************/

#ifndef cproj_prototypes_h
#define cproj_prototypes_h

#include "gctp_prototypes.h"

/*****************************************************************
    Function prototypes.
*****************************************************************/

void p_error(char *what, char *where);
void ptitle(char *A);
void tsincos(double val, double *sin_val, double *cos_val);
double msfnz(double eccent, double sinphi, double cosphi);
double qsfnz(double eccent, double sinphi, double cosphi);
double tsfnz(double eccent, double phi, double sinphi);
void radius2(double A, double B);
void radius(double A);
void stanparl(double A, double B);
void cenlonmer(double A);
void cenlon(double A);
void cenlat(double A);
void true_scale(double A);
void origin(double A);
void offsetp(double A, double B);
double adjust_lon(double x);
double phi1z(double eccent, double qs, long  *flag);
double phi2z(double eccent, double ts, long *flag);
double phi3z(double ml, double e0, double e1, double e2, 
             double e3, long *flag);
double phi4z(double eccent, double e0, double e1, double e2, 
             double e3, double a, double b, double *c, double *phi);
double asinz(double con);
int sign(double x);
double e0fn(double x);
double e1fn(double x);
double e2fn(double x);
double e3fn(double x);
double e4fn(double x);
double mlfn(double e0, double e1, double e2, double e3, double phi);
double paksz(double ang, long *iflg);
double pakcz(double pak);
void stparl1(double A);
void genrpt(double A, char *S);
void genrpt_long(long A, char *S);
void pblank();
int sphdz(long isph,double *parm,double *r_major,double *r_minor,double *radius);

#endif
