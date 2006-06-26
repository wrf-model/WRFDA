/* @(#)xdr_float.c	1.1 87/11/04 3.9 RPCSRC */
#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = "@(#)xdr_float.c 1.12 87/08/11 Copyr 1984 Sun Micro";
#endif

/*
 * xdr_float.c, Generic XDR routines impelmentation.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 * These are the "floating point" xdr routines used to (de)serialize
 * most common data items.  See xdr.h for more info on the interface to
 * xdr.
 */

#include <stdio.h>


#include "types.h"
#include "xdr.h"

#if defined(i386) | defined(__i386)
#define SWAP_DOUBLES
#endif
#ifdef MIPSEL
#define SWAP_DOUBLES
#endif
#ifdef MSDOS
#define SWAP_DOUBLES
#endif
#ifdef __FreeBSD__
#define SWAP_DOUBLES
#endif
#if defined WIN32 | defined WINNT
#define SWAP_DOUBLES
#endif

/*
 * NB: Not portable.
 * Only two cases handled here:
 *	IEEE floating point and Vaxen
 */

#ifdef vax

/* What IEEE single precision floating point looks like on a Vax */
struct	ieee_single {
	unsigned int	mantissa: 23;
	unsigned int	exp     : 8;
	unsigned int	sign    : 1;
};

/* Vax single precision floating point */
struct	vax_single {
	unsigned int	mantissa1 : 7;
	unsigned int	exp       : 8;
	unsigned int	sign      : 1;
	unsigned int	mantissa2 : 16;
};

#define VAX_SNG_BIAS	0x81
#define IEEE_SNG_BIAS	0x7f

static struct sgl_limits {
	struct vax_single s;
	struct ieee_single ieee;
} max = {
	{ 0x7f, 0xff, 0x0, 0xffff },	/* Max Vax */
	{ 0x0, 0xff, 0x0 }		/* Max IEEE */
};
static struct sgl_limits min = {
	{ 0x0, 0x0, 0x0, 0x0 },	/* Min Vax */
	{ 0x0, 0x0, 0x0 }		/* Min IEEE */
} ;
#endif /* vax */

bool_t
xdr_float(xdrs, fp)
	register XDR *xdrs;
	register float *fp;
{
#ifdef vax
	struct ieee_single is;
	struct vax_single vs, *vsp;
	struct sgl_limits *lim;
	int i;
#endif
	switch (xdrs->x_op) {

	case XDR_ENCODE:
#ifdef _CRAYMPP
		/* T3D longs are 64 bits but floats are 32 bits */
		return (XDR_PUTBYTES(xdrs, (caddr_t)fp, 4));
#else
#ifndef vax
		return (XDR_PUTLONG(xdrs, (long *)fp));
#else
		vs = *((struct vax_single *)fp);

		switch(vs.exp){
		case 0 :
			/* all vax float with zero exponent map to zero */
			is = min.ieee ;
			break ;
		case 2 :
		case 1 :
			/* These will map to subnormals */
			is.exp = 0 ;
			is.mantissa = (vs.mantissa1 << 16) | vs.mantissa2;
			/* lose some precision */
			is.mantissa >>= 3 - vs.exp ;
			is.mantissa += (1 << (20 + vs.exp)) ;
			break ;
		case 0xff : /* max.s.exp */
			if( vs.mantissa2 == max.s.mantissa2
				&& vs.mantissa1 == max.s.mantissa1)
			{
				/* map largest vax float to ieee infinity */
				is = max.ieee ;
				break ;
			} /* else, fall thru */
		default :
			is.exp = vs.exp - VAX_SNG_BIAS + IEEE_SNG_BIAS;
			is.mantissa = (vs.mantissa1 << 16) | vs.mantissa2;
		}

		is.sign = vs.sign;
		return (XDR_PUTLONG(xdrs, (long *)&is));
#endif
#endif

	case XDR_DECODE:
#ifdef _CRAYMPP
		/* T3D longs are 64 bits but floats are 32 bits */
		return (XDR_GETBYTES(xdrs, (caddr_t)fp, 4));
#else
#ifndef vax
		return (XDR_GETLONG(xdrs, (long *)fp));
#else
		vsp = (struct vax_single *)fp;
		if (!XDR_GETLONG(xdrs, (long *)&is))
			return (FALSE);

		switch(is.exp) {
		case 0 :
			if(is.mantissa == min.ieee.mantissa)
			{
				*vsp = min.s ;
			} else {
				unsigned tmp = is.mantissa >> 20 ;
				if(tmp >= 4) {
					vsp->exp = 2 ;
				} else if (tmp >= 2) {
					vsp->exp = 1 ;
				} else {
					*vsp = min.s ;
					break ;
				} /* else */
				tmp = is.mantissa - (1 << (20 + vsp->exp )) ;
				tmp <<= 3 - vsp->exp ;
				vsp->mantissa2 = tmp ;
				vsp->mantissa1 = (tmp >> 16);
			}
			break ;
		case 0xfe :
		case 0xff :
			*vsp = max.s ;
			break ;
		default :
			vsp->exp = is.exp - IEEE_SNG_BIAS + VAX_SNG_BIAS;
			vsp->mantissa2 = is.mantissa;
			vsp->mantissa1 = (is.mantissa >> 16);
		}

		vsp->sign = is.sign;
		return (TRUE);
#endif
#endif

	case XDR_FREE:
		return (TRUE);
	}
	return (FALSE);
}

/*
 * This routine works on Suns (Sky / 68000's) and Vaxen.
 */

#ifdef vax
/* What IEEE double precision floating point looks like on a Vax */
struct	ieee_double {
	unsigned int	mantissa1 : 20;
	unsigned int	exp       : 11;
	unsigned int	sign      : 1;
	unsigned int	mantissa2 : 32;
};

/* Vax double precision floating point */
struct  vax_double {
	unsigned int	mantissa1 : 7;
	unsigned int	exp       : 8;
	unsigned int	sign      : 1;
	unsigned int	mantissa2 : 16;
	unsigned int	mantissa3 : 16;
	unsigned int	mantissa4 : 16;
};

#define VAX_DBL_BIAS	0x81
#define IEEE_DBL_BIAS	0x3ff
#define MASK(nbits)	((1 << nbits) - 1)

static struct dbl_limits {
	struct	vax_double d;
	struct	ieee_double ieee;
} dbl_limits[2] = {
	{{ 0x7f, 0xff, 0x0, 0xffff, 0xffff, 0xffff },	/* Max Vax */
	{ 0x0, 0x7ff, 0x0, 0x0 }},			/* Max IEEE */
	{{ 0x0, 0x0, 0x0, 0x0, 0x0, 0x0},		/* Min Vax */
	{ 0x0, 0x0, 0x0, 0x0 }}				/* Min IEEE */
};

#endif /* vax */


bool_t
xdr_double(xdrs, dp)
	register XDR *xdrs;
	double *dp;
{
	register long *lp;
#ifdef vax
	struct	ieee_double id;
	struct	vax_double vd;
	register struct dbl_limits *lim;
	int i;
#endif

	switch (xdrs->x_op) {

	case XDR_ENCODE:
#ifndef vax
		lp = (long *)dp;
#else
		vd = *((struct vax_double *)dp);
		for (i = 0, lim = dbl_limits;
			i < sizeof(dbl_limits)/sizeof(struct dbl_limits);
			i++, lim++) {
			if ((vd.mantissa4 == lim->d.mantissa4) &&
				(vd.mantissa3 == lim->d.mantissa3) &&
				(vd.mantissa2 == lim->d.mantissa2) &&
				(vd.mantissa1 == lim->d.mantissa1) &&
				(vd.exp == lim->d.exp)) {
				id = lim->ieee;
				goto shipit;
			}
		}
		id.exp = vd.exp - VAX_DBL_BIAS + IEEE_DBL_BIAS;
		id.mantissa1 = (vd.mantissa1 << 13) | (vd.mantissa2 >> 3);
		id.mantissa2 = ((vd.mantissa2 & MASK(3)) << 29) |
				(vd.mantissa3 << 13) |
				((vd.mantissa4 >> 3) & MASK(13));
	shipit:
		id.sign = vd.sign;
		lp = (long *)&id;
#endif
#ifdef _CRAYMPP
		return (XDR_PUTBYTES(xdrs, (caddr_t)dp, 8));
#else
#ifndef SWAP_DOUBLES
		return (XDR_PUTLONG(xdrs, lp++) && XDR_PUTLONG(xdrs, lp));
#else /* SWAP_DOUBLES */
		return (XDR_PUTLONG(xdrs, lp+1) && XDR_PUTLONG(xdrs, lp));
#endif /* SWAP_DOUBLES */
#endif

	case XDR_DECODE:
#ifndef vax
		lp = (long *)dp;
#ifdef _CRAYMPP
		return (XDR_GETBYTES(xdrs, (caddr_t)dp, 8));
#else
#ifndef SWAP_DOUBLES
		return (XDR_GETLONG(xdrs, lp++) && XDR_GETLONG(xdrs, lp));
#else /* SWAP_DOUBLES */
		return (XDR_GETLONG(xdrs, lp+1) && XDR_GETLONG(xdrs, lp));
#endif /* SWAP_DOUBLES */
#endif
#else
		lp = (long *)&id;
		if (!XDR_GETLONG(xdrs, lp++) || !XDR_GETLONG(xdrs, lp))
			return (FALSE);
		for (i = 0, lim = dbl_limits;
			i < sizeof(dbl_limits)/sizeof(struct dbl_limits);
			i++, lim++) {
			if ((id.mantissa2 == lim->ieee.mantissa2) &&
				(id.mantissa1 == lim->ieee.mantissa1) &&
				(id.exp == lim->ieee.exp)) {
				vd = lim->d;
				goto doneit;
			}
		}
		vd.exp = id.exp - IEEE_DBL_BIAS + VAX_DBL_BIAS;
		vd.mantissa1 = (id.mantissa1 >> 13);
		vd.mantissa2 = ((id.mantissa1 & MASK(13)) << 3) |
				(id.mantissa2 >> 29);
		vd.mantissa3 = (id.mantissa2 >> 13);
		vd.mantissa4 = (id.mantissa2 << 3);
	doneit:
		vd.sign = id.sign;
		*dp = *((double *)&vd);
		return (TRUE);
#endif

	case XDR_FREE:
		return (TRUE);
	}
	return (FALSE);
}
