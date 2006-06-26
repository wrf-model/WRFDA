/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

#ifdef RCSID
static char RcsId[] = "@(#)$Revision: 1.24 $";
#endif

/* $Id: hdp.h,v 1.24 2002/03/08 05:58:25 bmribler Exp $ */

#ifndef __HDP_H
#define __HDP_H

#include "hdf.h"
#include "hfile.h"

/* Global Variables (ick) */
#ifndef HDP_MASTER
extern
#endif							/* !HDP_MASTER */
intn        vinit_done
#ifdef HDP_MASTER
= FALSE							/* indicates Vsets have been init'ed for the current file */
#endif							/* HDP_MASTER */
           ;

/* Global Definitions */
#define MAXCHOICES 50
#ifndef MAXNAMELEN
#define MAXNAMELEN 100
#endif /* !MAXNAMELEN */
#define MAXCLASSLEN 100
#define MAXPERLINE 65	/* max # of chars per line in the output */
#define MAXRANK 100
#define MAXFNLEN 256
#define CONDENSE 1
#define NO_SPECIFIC -1     /* no specific datasets are requested */
#define	ATTR_INDENT	0	/* # of spaces in front of attribute data */
#define ATTR_CONT_INDENT  25	/* # of spaces in front of attribute data 
					on a continuous line */
#define	DATA_INDENT	16	/* # of spaces in front of dataset data */
#define DATA_CONT_INDENT  16	/* # of spaces in front of dataset data 
					on a continuous line */

/* ERROR_GOTO_n macros are used to facilitate error printing.  Each
   macro prints the given message to the stderr, then uses the HDF 
   library macro HGOTO_DONE to set the variable ret_value to FAIL
   and jump to label "done"
	ERROR_GOTO_0 is used for fprintf with no parameters.
	ERROR_GOTO_1 is used for fprintf with 1 parameter.
	ERROR_GOTO_2 is used for fprintf with 2 parameters.
	ERROR_GOTO_3 is used for fprintf with 3 parameters.
	ERROR_GOTO_4 is used for fprintf with 4 parameters.
	ERROR_GOTO_5 is used for fprintf with 5 parameters.
	ERROR_GOTO_6 is used for fprintf with 6 parameters.
*/
#define ERROR_GOTO_0(txt) { \
	fprintf(stderr, "\nHDP ERROR>>> %s", txt); \
	fprintf(stderr, ".\n"); \
	HGOTO_DONE( FAIL ); }
#define ERROR_GOTO_1(txt, par1 ) {	\
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1 ); \
	fprintf(stderr, ".\n"); \
	HGOTO_DONE( FAIL ); }
#define ERROR_GOTO_2(txt, par1, par2) {	\
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1, par2); \
	fprintf(stderr, ".\n"); \
	HGOTO_DONE( FAIL ); }
#define ERROR_GOTO_3(txt, par1, par2, par3) { \
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1, par2, par3); \
	fprintf(stderr, ".\n"); \
	HGOTO_DONE( FAIL ); }
#define ERROR_GOTO_4(txt, par1, par2, par3, par4) { \
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1, par2, par3, par4); \
	fprintf(stderr, ".\n"); \
	HGOTO_DONE( FAIL ); }
#define ERROR_GOTO_5(txt, par1, par2, par3, par4, par5) { \
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1, par2, par3, par4, par5); \
	fprintf(stderr, ".\n"); \
	HGOTO_DONE( FAIL ); }
#define ERROR_GOTO_6(txt, par1, par2, par3, par4, par5, par6) { \
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1, par2, par3, par4, par5, par6); \
	fprintf(stderr, ".\n"); \
	HGOTO_DONE( FAIL ); }

/* ERROR_CONT_n macros are used to facilitate error printing.  Each
   macro prints the given message to the stderr, then "continue"s.
   Note: at this time, set ret_value to FAIL, but if it turns out
   that there are cases where ret_value should be SUCCEED, then 
   add another argument 'ret' (=FAIL/SUCCEED) to set ret_value to
   appropriate value.
   Note: having extra \n after Continued. separates the following
   output from the error messages.
	ERROR_CONT_0 is used for fprintf with no parameters.
	ERROR_CONT_1 is used for fprintf with 1 parameter.
	ERROR_CONT_2 is used for fprintf with 2 parameters.
	ERROR_CONT_3 is used for fprintf with 3 parameters.
	ERROR_CONT_4 is used for fprintf with 4 parameters.
	ERROR_CONT_5 is used for fprintf with 5 parameters.
	ERROR_CONT_6 is used for fprintf with 6 parameters.
*/
#define ERROR_CONT_0(txt) { \
	fprintf(stderr, "\nHDP ERROR>>> ", txt ); \
	fprintf(stderr, ".  Continued.\n\n"); \
	ret_value = FAIL; \
	continue; }
#define ERROR_CONT_1(txt, par1 ) {	\
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1 ); \
	fprintf(stderr, ".  Continued.\n\n"); \
	ret_value = FAIL; \
	continue; }
#define ERROR_CONT_2(txt, par1, par2 ) {	\
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1, par2); \
	fprintf(stderr, ".  Continued.\n\n"); \
	ret_value = FAIL; \
	continue; }
#define ERROR_CONT_3(txt, par1, par2, par3 ) { \
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1, par2, par3); \
	fprintf(stderr, ".  Continued.\n\n"); \
	ret_value = FAIL; \
	continue; }
#define ERROR_CONT_4(txt, par1, par2, par3, par4 ) { \
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1, par2, par3, par4); \
	fprintf(stderr, ".  Continued.\n\n"); \
	ret_value = FAIL; \
	continue; }

/* ERROR_CONT_END is used to facilitate error handling when an error
   occurs on a vdata.  It prints the provided error message to the
   stderr, end access to the vdata, then continue */
#define ERROR_CONT_END(txt, par1, par2, vd_id ) { \
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1, par2 ); \
	fprintf(stderr, ".  Continued.\n\n"); \
	ret_value = FAIL; \
        VSdetach(vd_id); \
        vd_id = FAIL; /* reset */ \
	continue; }

/* ERROR_BREAK_n macros are used to facilitate error printing.  Each
   macro prints the given message to the stderr, then sets the variable
   ret_value to the given value and "break"s.
	ERROR_BREAK_0 is used for fprintf with no parameters.
	ERROR_BREAK_1 is used for fprintf with 1 parameter.
	ERROR_BREAK_2 is used for fprintf with 2 parameters.
	ERROR_BREAK_3 is used for fprintf with 3 parameters.
	ERROR_BREAK_4 is used for fprintf with 4 parameters.
	ERROR_BREAK_5 is used for fprintf with 5 parameters.
	ERROR_BREAK_6 is used for fprintf with 6 parameters.
*/
#define ERROR_BREAK_0(txt, ret) { \
	fprintf(stderr, "\nHDP ERROR>>> %s", txt); \
	fprintf(stderr, ".\n"); \
	ret_value = ret; \
	break; }
#define ERROR_BREAK_1(txt, par1, ret ) {	\
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1 ); \
	fprintf(stderr, ".\n"); \
	ret_value = ret; \
	break; }
#define ERROR_BREAK_2(txt, par1, par2, ret) {	\
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1, par2); \
	fprintf(stderr, ".\n"); \
	ret_value = ret; \
	break; }
#define ERROR_BREAK_3(txt, par1, par2, par3, ret) {	\
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1, par2, par3 ); \
	fprintf(stderr, ".\n"); \
	ret_value = ret; \
	break; }
#define ERROR_BREAK_4(txt, par1, par2, par3, par4, ret) {	\
	fprintf(stderr, "\nHDP ERROR>>> "); \
	fprintf(stderr, txt, par1, par2, par3, par4 ); \
	fprintf(stderr, ".\n"); \
	ret_value = ret; \
	break; }

/* ERROR_NOTIFY macros are used to display a failure but does not do 
   anything else except setting ret_value to FAIL so that the failure
   can be traced back to the caller.
*/
#define ERROR_NOTIFY_2(txt, par1, par2) { \
	fprintf(stderr, "\nHDP ERROR>>> " ); \
	fprintf(stderr, txt, par1, par2 ); \
	fprintf(stderr, ".\n"); \
	ret_value = FAIL; }
#define ERROR_NOTIFY_3(txt, par1, par2, par3) { \
	fprintf(stderr, "\nHDP ERROR>>> " ); \
	fprintf(stderr, txt, par1, par2, par3 ); \
	fprintf(stderr, ".\n"); \
	ret_value = FAIL; }

/* CHECK_POS makes sure that number is > 0 so we are not going to
   allocate 0 elements.  Macro is used here instead of function call
   so if we decide to change exit(1) to goto done and return FAIL,
   it will be possible */
#define CHECK_POS( number, buf_size_name, func_name ) { \
     if( number <= 0 ) { \
        fprintf(stderr, "in %s: Attempting to allocate 0 items using '%s'!\n",\
		func_name, buf_size_name ); \
	exit(1); } \
}

/* CHECK_ALLOC macro validates that 'buffer' has been successfully
   allocated; if the allocation fails, exit the application after displaying
   an appropriate message.  Macro is used here to simplify the validation
   and to facilitate maintenance, i.e., if decide to change from exit(1)
   to goto "done" and return with FAIL in all allocation failure cases, it
   will be possible.
*/
#define CHECK_ALLOC(buffer, buf_name, func_name ) { \
      if (buffer == NULL) {\
         fprintf(stderr,"in %s: space allocation for %s failed.  Terminated!\n",\
		func_name, buf_name ); \
         exit(1); }  \
}

/* Add enum and string for new commands to both of the variables below. */
/* Preserve the correct/corresponding ordering */
typedef enum
  {
	  HELP, LIST, DUMPSDS, DUMPRIG, DUMPVG, DUMPVD, DUMPGR, NONE
  }
command_t;
#ifndef HDP_MASTER
extern
#endif							/* !HDP_MASTER */
const char *commands[]
#ifdef HDP_MASTER
=
{
	"help",
	"list",
	"dumpsds",
	"dumprig",
	"dumpvg",
	"dumpvd",
	"dumpgr"
}
#endif							/* HDP_MASTER */
           ;

/* Global options structure */
typedef struct
  {
	  intn        help;			/* Print help on this command */
  }
dump_opt_t;

typedef enum
  {
	  OTAG, OFILE, OGROUP, ONAME
  }
sort_t;							/* The order tag/refs are sorted */

/* 'list' command option structure */
typedef struct
  {
	  sort_t      order;		/* The sort order tag/refs are printed in */
	  enum
		{
			VSHORT, VLONG, VDEBUG
		}
	  verbosity;				/* verbosity level of list */
	  enum
		{
			LNONE, LTAGNUM, LTAGNAME, LGROUP
		}
	  limit;					/* How to limit tag/refs */
	  intn        class;		/* Whether to dump class information */
	  intn        name;			/* Whether to dump name information */
	  intn        desc;			/* Whether to dump description information */
	  intn        spec;			/* Whether to dump special element information */
	  intn        group;		/* Whether to dump group information */
	  uint16      limit_tag;	/* tag # to limit search to */
	  char       *limit_name;	/* tag name to limit search to */
  }
list_info_t;

/* Which dataset to dump */
/* BMR: added defined values to fix exclusive problem - 1/23/99 */
typedef enum
  {
	  DALL=0, DINDEX=1, DREFNUM=2, DNAME=4, DCLASS=8, DFIELDS=16 
  }
filter_t;

/* Which contents to dump */
typedef enum
  {
	  DVERBOSE, DHEADER, DDATA
  }
content_t;

/* What kind of data to dump to file */
typedef enum
  {
	  DASCII, DBINARY
  }
file_type_t;

/* BMR: numerical filter structure; used to hold a list of indices or reference numbers 
   and the number of indices or reference numbers given - 1/23/99 */
typedef struct
{
	int32 *num_list;
	int32 num_items;
}
number_filter_t;

/* BMR: character filter structure; used to hold a list of names or class names 
   and the number of names or class names given - 1/23/99 */
typedef struct
{
	char **str_list;
	int32 num_items;
}
char_filter_t;

/* 'dumpsds' command option structure */
/* BMR: added fields to hold indices, reference numbers, names, and classes
   separately - 1/23/99 */
typedef struct
{
   filter_t    filter;		/* which data object to dump */
   intn      *filter_num;       /* ref #'s or indices to use as filter */
   char      **filter_str;      /* names or classes to use as filter */
   number_filter_t by_index;	/* data objects requested by index */
   number_filter_t by_ref;	/* data objects requested by reference number */
   char_filter_t by_name;	/* data objects requested by name */
   char_filter_t by_class;	/* data objects requested by class name */
   char_filter_t by_field;	/* data objects requested by field - only VD */
   int32       num_chosen;	/* number of items chosen, totally (-1==ALL) */
   content_t   contents;        /* what contents to dump */
   intn        dump_to_file;	/* whether to dump to a file */
   file_type_t file_type;	/* Is data written in ASCII or binary */
   intn	       as_stream;	/* whether carriage return added to output data lines */
   intn	       clean_output;	/* whether to print space characters as they 
				   are or to print in \digit format */
   intn	       firstln_indent;	/* col# where data starts on the first line*/ 
   intn	       contln_indent;	/* col# where data continues on the next line*/ 
   char        file_name[MAXFNLEN];/* Name of file to dump into */
   char        ifile_name[MAXFNLEN];/* Name of input file being processed */

   intn        print_pal;	/* for GR only: TRUE if option -p selected */
   gr_interlace_t interlace;	/* user's choice of interlace mode to print data in */
   intn	       no_lattr_data;	/* GR & SD only: TRUE if option -l selected */
   intn	       no_gattr_data;	/* GR & SD only: TRUE if option -g selected */
  }
dump_info_t;

/* Filename list structure */
typedef struct
  {
	  intn        max_files;	/* the number of files in the file list */
	  intn        curr_file;	/* the current file */
	  char      **file_arr;		/* pointer to the filename information */
  }
filelist_t;

/* Group info structure */
typedef struct
  {
	  intn        max_dds;		/* the number of DDs in the group */
	  intn        curr_dd;		/* The current DD */
	  DFdi       *dd_arr;		/* array to hold the DDs in the group */
  }
groupinfo_t;

/* DD info structure */
typedef struct
  {
	uint16  tag, ref;	/* tag and ref of object */
	int32   offset, length;	/* offset and length of object in file */
	int32   index;		/* index of the object in the file */
	uintn   is_group:1,	/* flag to indicate item is a group */
	        is_special:1,	/* flag to indicate item is a special element */
	        has_label:1,	/* flag to indicate item has a label */
	        has_desc:1;	/* flag to indicate item has a desc. */
	intn    no_element;	/* TRUE if group_info is NULL */
	groupinfo_t     *group_info;/* pointer to group information */
	sp_info_block_t *spec_info;/* pointer to special element information */
  }
objinfo_t;
#define CHECK_SPECIAL   0x0001	/* Look for spec. elem. when building dd list */
#define CHECK_GROUP     0x0002	/* Look for groups when building dd list */
#define CHECK_LABEL     0x0004	/* Look for anno. labels when building dd list */
#define CHECK_DESC      0x0008	/* Look for anno. desc. when building dd list */

/* DD list structure */
typedef struct
  {
	  intn        max_obj;		/* the number of dds in the obj list */
	  intn        curr_obj;		/* the current obj */
	  uint32      options;		/* storage for options used to build the dd list */
	  objinfo_t **srt_obj_arr;	/* the sorted dd/object information array */
	  objinfo_t  *raw_obj_arr;	/* the raw dd/object information array */
  }
objlist_t;

/* hdp_vd.c */

/* Vdata information: used to hold various information of a vdata to 
   facilitate parameter passing */
typedef struct
  {
	int32   index;			/* vdata index */
	int32   nvf;			/* number of records in the vdata */
	int32   interlace;		/* interlace mode of the vdata */
	int32   vsize;			/* record size of the vdata */
	int32   ref;			/* vdata ref# */
	int32   tag;			/* vdata tag */
	char    class[VSNAMELENMAX];	/* vdata class */
	char    name[VSNAMELENMAX];	/* vdata name */
  }
vd_info_t;

/* hdp_vg.c */

#define NUM_VGS 20;

typedef struct 
{
    int32  index;
    int32  displayed;
    int32  treedisplayed;       /* BMR: added to do the loop - 01/16/99 */
    char   name[MAXNAMELEN];
    char **children;
    char **type;
}
vg_info_t;

/* hdp.c */
extern int32 VShdfsize(int32 vkey, char *fields);
extern intn VSattrhdfsize(int32 vsid, int32 findex, intn attrindex,int32 *size);
extern intn Vattrhdfsize(int32 vsid, intn attrindex, int32 *size);

/* hdp_list.c */
/*extern intn print_data_annotations(const char *fname, int32 an_id, ann_type annot_type, uint16 tag, uint16 ref);*/
extern intn print_all_data_labels(const char *fname, int32 an_id);
extern intn print_all_data_descs(const char *fname, int32 an_id);
extern intn do_list(intn curr_arg, intn argc, char *argv[], int help);
extern intn print_all_file_labels(const char *fname, int32 an_id);
extern intn print_file_descs(const char *f_name, int32 an_id );
/*intn print_annots_by_object( const char *fname, int32 an_id, ann_type annot_type, uint16 tag, uint16 ref);
intn print_annots_in_file( int32 an_id, char* fname, int32 n_annotations, ann_type annot_type );
*/
intn print_all_file_descs(const char *fname, list_info_t* list_opts, /* for print_SDattrs */ int32 an_id);
intn print_data_labels( const char *fname, int32 an_id, uint16 tag, uint16 ref);
intn print_data_descs( const char *fname, int32 an_id, uint16 tag, uint16 ref);

/* hdp_sds.c */
extern intn do_dumpsds(intn curr_arg, intn argc, char *argv[], intn help);

/* hdp_rig.c */
extern intn do_dumprig(intn curr_arg, intn argc, char *argv[], intn help);

/* hdp_vg.c */
extern intn do_dumpvg(intn curr_arg, intn argc, char *argv[], intn help);
extern intn print_data_annots(int32 file_id, const char *file_name, int32 tag, int32 ref);
extern intn print_file_annotations( int32 file_id, const char *file_name );
void print_fields( char *fields, char *field_title, FILE *fp );

/* hdp_vd.c */
extern intn do_dumpvd(intn curr_arg, intn argc, char *argv[], intn help);
extern intn parse_dumpvd_opts(dump_info_t * dumpvd_opts, intn *curr_arg, intn argc, char *argv[], char *flds_chosen[MAXCHOICES], int *dumpallfields);

/* hdp_gr.c */
extern intn do_dumpgr(intn curr_arg, intn argc, char *argv[], intn help);


/* hdp_dump.c */
extern intn fmtchar(VOIDP x, file_type_t ft, FILE * ofp);
extern intn fmtuchar8(VOIDP x, file_type_t ft, FILE * ofp);
extern intn fmtbyte(unsigned char *x, file_type_t ft, FILE * ofp);
extern intn fmtint(VOIDP x, file_type_t ft, FILE * ofp);
extern intn fmtshort(VOIDP x, file_type_t ft, FILE * ofp);
extern intn fmtint8(VOIDP x, file_type_t ft, FILE * ofp);
extern intn fmtuint8(VOIDP x, file_type_t ft, FILE * ofp);
extern intn fmtint16(VOIDP x, file_type_t ft, FILE * ofp);
extern intn fmtuint16(VOIDP x, file_type_t ft, FILE * ofp);
extern intn fmtint32(VOIDP x, file_type_t ft, FILE * ofp);
extern intn fmtuint32(VOIDP x, file_type_t ft, FILE * ofp);
extern intn fmtfloat32(VOIDP x, file_type_t ft, FILE * ofp);
extern intn fmtfloat64(VOIDP x, file_type_t ft, FILE * ofp);
extern intn dumpfull(int32 nt, dump_info_t * dump_opts, int32 cnt, VOIDP databuf, FILE * ofp, intn indent, intn cont_indent );
extern intn dumpclean(int32 nt, dump_info_t * dump_opts, int32 cnt, VOIDP databuf, FILE * ofp );
extern int32 dumpGR_SDattr(int32 nt, dump_info_t * dump_opts, int32 cnt, VOIDP databuf, FILE * ofp);

/* show.c */
extern int32 dumpvd(int32 vd, file_type_t ft, int data_only, FILE *fp, 
                    char separater[2],int32 flds_indices[VSFIELDMAX], 
                    int dumpallfields);
extern intn dumpattr(int32 vid, int32 findex, intn isvs, file_type_t ft, FILE *fp);

/* hdp_util.c */
	/* misc. functions */

void init_dump_opts(dump_info_t *dump_opts);
void parse_number_opts( char *argv[], int *curr_arg, number_filter_t *filter);
void parse_string_opts( char *argv[], int *curr_arg, char_filter_t *filter);
extern char *tagnum_to_name(intn num);
extern intn tagname_to_num(const char *name);
extern void sort(int32 *chosen, int32 choices);
int sort_obj_list_by_tag(const void *, const void *);
int int32_compare(const void *, const void *);

	/* filename list functions */
extern filelist_t *make_file_list(intn curr_arg, intn argc, char *argv[]);
extern char *get_next_file(filelist_t * f_list, intn advance);
extern int32* free_num_list(int32 *num_list );
extern char* free_char_list(char *char_list );
extern char** free_str_list(char **str_list, int32 num_items );
extern vg_info_t** free_vginfo_list(vg_info_t** list, int32 num_items );
extern void free_file_list(filelist_t * f_list);

	/* group list functions */
extern groupinfo_t *make_group_list(int32 fid, uint16 tag, uint16 ref);
extern DFdi *get_next_group(groupinfo_t * g_list, intn advance);
extern int32 get_group_max(groupinfo_t * g_list);
extern void free_group_list(groupinfo_t * g_list);
	/* object list functions */
extern objlist_t *make_obj_list(int32 fid, uint32 options);
extern objinfo_t *get_next_obj(objlist_t * o_list, intn advance);
extern objinfo_t *goto_nth_obj(objlist_t * o_list, intn n);
extern void reset_obj_list(objlist_t * o_list);
extern void resetBuff(VOIDP *buf);
extern void free_obj_list(objlist_t * o_list);
extern void sort_obj_list(objlist_t * o_list, sort_t sort_type);
extern intn print_SDattrs( int32 sd_id, FILE *fp, int32 n_file_attrs, dump_info_t *dumpsds_opts );
extern intn print_SDSattrs( int32 sds_id, int32 nattrs, FILE *fp, dump_info_t *dumpsds_opts);

extern intn print_GRattrs( int32 gr_id, int32 n_file_attrs, FILE *fp, dump_info_t *dumpgr_opts );
extern intn print_RIattrs( int32 ri_id, intn ri_index, int32 nattrs, FILE *fp, dump_info_t *dumpgr_opts);

extern void alloc_index_list( int32 **index_list, int32 num_chosen );

#define PPSTR( name, str) { \
   printf( " %s: %s\n", name, str ); }
#define PPNUM( name, num) { \
   printf(" %s: %d\n", name, num ); }
#define PSTR( name, str) { \
   fprintf(stderr, " %s: %s\n", name, str ); }
#define PNUM( name, num) { \
   fprintf(stderr, " %s: %d\n", name, num ); }
/***************** end remove when done ******************/

#endif /* __HDP_H */
