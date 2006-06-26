/*******************************************************************
 * NCSA HDF                                                        *
 * Software Development Group                                      *
 * National Center for Supercomputing Applications                 *
 * University of Illinois at Urbana-Champaign                      *
 * 605 E. Springfield, Champaign IL 61820                          *
 *                                                                 *
 * For conditions of distribution and use, see the accompanying    *
 * hdf/COPYING file.                                               *
 *                                                                 *
 *******************************************************************/

#ifdef RCSID
static char RcsId[] = "@(#)$Revision: 1.12 $";
#endif

/* $Id: tvattr.c,v 1.12 2004/11/05 05:19:14 bmribler Exp $ */

/**************************************************************
*
* tvattr.c
*
* This program tests routines in hdf/src/vattr.c.  Those routines
*  set or change attributes for a vgroup, a vdata or a field of 
*  a vdata; get info about an attribute; read values of an attr.
*
* wrtie_vset_stuff() creates 1 empty vgroup and 1 vgroup having 1 
*  vdata. wirte_vattr() adds attributes to the vgroups, vdata and
*  the fields of the vdata. It then creates new vdata and vgroup,
*  and adds attrs to them.  read_vattr reads the attributes back,
*  and check their correctness.  
*
**************************************************************/
#include "hdf.h"
#include "tproto.h"
#include "vg.h"
#include <math.h>

#define FILENAME   "tvattr.hdf"
#define VGNAME0    "vgname0"
#define VGNAME1    "vgname1"
#define VGNAME2    "vgname2"
#define VSNAME0    "vsname0"
#define VSNAME1    "vsname1"
#define VSNAME2    "vsname2"
#define VSCLASS0   "vsclass0"
#define VGCLASS2   "vgclass2"
#define FLDNAME0   "fldname0"
#define FLDNAME1   "fldname1"
#define FLDNAME2   "fldname2"
#define FLDNAMES   "fldname0,fldname1"
#define FLDNAMES_1 "fldname1,fldname2"
#define N_RECS     3
#define ATTNAME1   "attname1"
#define ATTNAME2   "attname2"
#define ATTNAME3   "attname3"
#define ATTNAME4   "attname4"
#define ATTNAME5   "attname5"
#define ATTNAME6   "attname6"
#define ATTNAME7   "attname7"
#define ATTNAME8   "attname8"
#define ATTNAME9   "attname9"
#define ATTNAME10  "attname10"
#define EPS64     (float64)1.0E-14
#define EPS32     (float32)1.0E-7

int32 data1[6]={0,-1,10,11,20,21}, idata1[6];
char  data2[6] = {'A','B','C','D','E','F'}, idata2[6];
uint16 attr2[2] = {16, 32}, iattr2[2];
uint32 attr1[4] = {100, 132, 10032, 10064}, iattr1[2];
char   attr3[6] = {'m','N','p', 'S', 't', '\0'}, iattr3[6];
float32 attr4[2] = {(float32)32.001, (float32)-34.002}, iattr4[2];
float64 attr5[2] = {64.12345, -64.12345}, iattr5[2];

static intn write_vset_stuff(void);
static intn write_vattrs(void);
static intn read_vattrs(void);

/* create vdatas and vgroups */

static intn write_vset_stuff(void)
{
   int32 fid, vgid, vsid;

   if (FAIL == (fid = Hopen(FILENAME, DFACC_CREATE, 0))) {
         num_errs++;
         return FAIL;
      } 
   if (Vstart(fid) == FAIL)  {
         num_errs++;
         return FAIL;
      } 
   /* Vgroup Generation */
   if (FAIL == (vgid = (Vattach(fid, -1, "w"))))   {
         num_errs++;
         return FAIL;
      } 
   if (FAIL == Vsetname(vgid, VGNAME0)) {
         num_errs++;
         return FAIL;
      } 
   if (FAIL == Vdetach(vgid)) {
         num_errs++;
         return FAIL;
      } 
   /* create a vgroup and a vdata, insert the vdata into the vgroup */
   if (FAIL == (vgid = (Vattach(fid, -1, "w"))))   {
         num_errs++;
         return FAIL;
      }
   if (FAIL == Vsetname(vgid, VGNAME1)) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == (vsid = (VSattach(fid, -1, "w"))))   {
         num_errs++;
         return FAIL;
      }
   if (FAIL == VSsetname(vsid, VSNAME1)) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == VSfdefine(vsid, FLDNAME1, DFNT_CHAR8, 1)) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == VSfdefine(vsid, FLDNAME2, DFNT_CHAR8, 1)) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == VSsetfields(vsid, FLDNAMES_1)) {
         num_errs++;
         return FAIL;
      }
   if (N_RECS != VSwrite(vsid, (unsigned char *)data2, N_RECS, FULL_INTERLACE)) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == Vinsert(vgid, vsid)) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == VSdetach(vsid)) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == Vdetach(vgid)) {
         num_errs++;
         return FAIL;
      }
   /* close the file */
   if (FAIL == Vend(fid))  {
         num_errs++;
         return FAIL;
      }
   if (FAIL == Hclose(fid)) {
         num_errs++;
         return FAIL;
      }
   return SUCCEED;
} /* write_vset_stuff */

/* test attribute routines */
static intn write_vattrs(void)
{
   int32 fid, vgid, vsid;
   int32 vsref, vgref;
   int32 fldindex, vsversion;
   intn  n_flds;
   
    /* add attrs to the 1 vg */
   if (FAIL == (fid = Hopen(FILENAME, DFACC_RDWR, -1))) {
         num_errs++;
         return FAIL;
      }
   if (Vstart(fid) == FAIL)  {
         num_errs++;
         return FAIL;
      }

   if (FAIL == (vgref = Vgetid(fid, -1)))  {
         num_errs++;
         return FAIL;
      }
   if (FAIL == (vgid = Vattach(fid, vgref, "w")))  {
         num_errs++;
         return FAIL;
      }
   if (FAIL == Vsetattr(vgid, ATTNAME1, DFNT_UINT32, 2, attr1)) {
         num_errs++;
         printf(">>> Vsetattr1 failed\n");
      }
   /* change data type of existing attr, should fail */
   if (FAIL != Vsetattr(vgid, ATTNAME1, DFNT_UINT16, 2, attr2)) {
         num_errs++;
         printf(">>> Vsetattr1 changes attr type, should failed\n");
      }
   /* change order of existing attr, should fail */
   if (FAIL != Vsetattr(vgid, ATTNAME1, DFNT_UINT32, 1, attr1)) {
         num_errs++;
         printf(">>> Vsetattr changes attr order, should failed\n");
      }
   /* change values of existing attr */
   if (FAIL == Vsetattr(vgid, ATTNAME1, DFNT_UINT32, 2, &attr1[2])) {
       num_errs++;
       printf(">>> Vsetattr failed in changing attr values.\n");
      }

   if (FAIL == Vsetattr(vgid, ATTNAME2, DFNT_UINT16, 2, attr2)) { 
         num_errs++;
         printf(">>> Vsetattr2 failed\n");
      }
   if (FAIL == Vdetach(vgid)) {
         num_errs++;
         return FAIL;
      }
   /* add attr to vdata */
   if (FAIL == (vsref = VSfind(fid, VSNAME1)))  {
         num_errs++;
         return FAIL;
      }
   if (FAIL == (vsid = VSattach(fid, vsref, "w")))  {
         num_errs++;
         return FAIL;
      }
   if (VSET_VERSION != (vsversion = VSgetversion(vsid))) {
         num_errs++;
         printf(">>> Wrong version, should be 3, got %d \n", (int)vsversion);
      }
   /* check number of fields */
   if (2 != (n_flds = VFnfields(vsid)))  {
         num_errs++;
         printf(">>> Wrong number of fields, should be 2, got %d.\n",
                      n_flds);
      }
   /* search for non-existing field  */
   if (FAIL != VSfindex(vsid, FLDNAME0, &fldindex)) {
         num_errs++;
         printf(">>> Search for non-existing field, should fail.\n");
      }
   if (FAIL == VSsetattr(vsid, _HDF_VDATA, ATTNAME3, DFNT_CHAR8, 3, attr3)) {
         num_errs++;
         printf(">>> VSsetattr3 failed\n");
      }
   if (FAIL == VSfindex(vsid, FLDNAME1, &fldindex) ||
       fldindex != 0) {
         num_errs++;
         printf(">>> VSfindex  failed in search for FLDNAME1 .\n");
      }
   if (FAIL == VSsetattr(vsid, 0, ATTNAME4, DFNT_FLOAT32,1, attr4)) {
         num_errs++;
         printf(">>> VSsetattr4 failed\n");
      }
   if (FAIL == VSfindex(vsid, FLDNAME2, &fldindex) ||
       fldindex != 1) { 
         num_errs++;
         printf(">>> VSfindex  failed in search for FLDNAME2 .\n");
      }
   if (FAIL == VSsetattr(vsid, 1, ATTNAME5, DFNT_FLOAT32,1, attr4)) {
         num_errs++;
         printf(">>> VSsetattr5 failed\n");
      }
   if (FAIL != VSsetattr(vsid, 2, ATTNAME3, DFNT_CHAR8,5, attr3)) {
         num_errs++;
         printf(">>> Set attr for non-existing field, should fail\n");
      }
   if (FAIL != VSsetattr(vsid, -2, ATTNAME3, DFNT_CHAR8,5, attr3)) {
         num_errs++;
         printf(">>> Set attr for non-existing field, should fail\n");
      }

   /* set same attr name to different fields */
   if (FAIL == VSsetattr(vsid, _HDF_VDATA, ATTNAME4, DFNT_FLOAT32,1, attr4)) {
         num_errs++;
         printf(">>> VSsetattr6 failed\n");
      }
   if (FAIL == VSsetattr(vsid, 0, ATTNAME5, DFNT_FLOAT64,1, attr5)) {
         num_errs++;
         printf(">>> VSsetattr5 failed\n");
      }
   /* create an attribute with the same name as an existing vdata */
   if (FAIL == VSsetattr(vsid, 0, VSNAME1, DFNT_FLOAT64,1, attr5)) {
         num_errs++;
         printf(">>> VSsetattr7 failed\n");
      }
   if (VSET_NEW_VERSION != (vsversion = VSgetversion(vsid))) {
         num_errs++;
         printf(">>> Wrong version, should be 4, got %d \n", (int)vsversion);
      }
   /* change datatype of existing attr, should fail */
   if (FAIL != VSsetattr(vsid, 0, ATTNAME5, DFNT_FLOAT32,1, attr4)) {
         num_errs++;
         printf(">>> VSsetattr changes attr type, should fail.\n");
      }
   /* change order of existing attr, should fail */
   if (FAIL != VSsetattr(vsid, 0, ATTNAME5, DFNT_FLOAT64,2, attr5)) {
         num_errs++;
         printf(">>> VSsetattr changes attr order, should fail.\n");
      }
   /* change values of existing attr */
   if (FAIL == VSsetattr(vsid, 0, ATTNAME5, DFNT_FLOAT64,1, &attr5[1])) {
         num_errs++;
         printf(">>> VSsetattr failed in changing attr values.\n");
      }
   if (FAIL == VSdetach(vsid)) {
         num_errs++;
         return FAIL;
      }

     /* create lone vdata  and add attributes to it */
   if (FAIL == (vsid = (VSattach(fid, -1, "w"))))   {
         num_errs++;
         return FAIL;
      }
   if (FAIL == VSsetname(vsid, VSNAME0)) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == VSfdefine(vsid, FLDNAME0, DFNT_INT32, 1)) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == VSfdefine(vsid, FLDNAME1, DFNT_INT32, 1)) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == VSsetfields(vsid, FLDNAMES)) {
         num_errs++;
         return FAIL;
      }
   if (N_RECS != VSwrite(vsid, (unsigned char *)data1, N_RECS, 
       FULL_INTERLACE)) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == VSfindex(vsid, FLDNAME0, &fldindex)) {
         num_errs++;
         printf(">>> VSfindex failed in searching for FLDNAME0.\n");
      }
   if (FAIL == VSsetattr(vsid, 0, ATTNAME6, DFNT_FLOAT32,1, attr4)) {
         num_errs++;
         printf(">>> VSsetattr6 failed\n");
      }

   if (FAIL == VSsetattr(vsid, _HDF_VDATA, ATTNAME7, DFNT_CHAR8,3, attr3)) {
         num_errs++;
         printf(">>> VSsetattr7 failed\n");
      }
   if (FAIL == VSfindex(vsid, FLDNAME1, &fldindex) ||
       fldindex != 1) {
         num_errs++;
         printf(">>> VSfindex  failed in searching for FLDNAME1.\n");
      }
   if (FAIL == VSsetattr(vsid, 0, ATTNAME8, DFNT_FLOAT32,1, attr4)) {
         num_errs++;
         printf(">>> VSsetattr8 failed\n");
      }
  if (FAIL == VSdetach(vsid)) {
         num_errs++;
         return FAIL;
      }
  /* attach again, and modify attr */
  if (FAIL == (vsref = VSfind(fid, VSNAME0)))  {
         num_errs++;
         return FAIL;
      }
  if (FAIL == (vsid = VSattach(fid, vsref, "w")))  {
         num_errs++;
         return FAIL;
      }
  if (FAIL == VSsetclass(vsid, VSCLASS0)) {
         num_errs++;
         return FAIL;
      }
  if (FAIL == VSsetattr(vsid, 1, ATTNAME8, DFNT_FLOAT32,1, &attr4[1])) {
         num_errs++;
         printf(">>> VSsetattr failed in modifying attr8.\n");
      }
  if (FAIL == VSsetattr(vsid, 0, ATTNAME9, DFNT_CHAR8,5, attr3)) {
         num_errs++;
         printf(">>> VSsetattr9  field. \n");
      }
  if (FAIL == VSdetach(vsid)) {
         num_errs++;
         return FAIL;
  }
  /* attach again with "r" access to test VSsetattr on "r" access vdata 
     BMR - Nov 4, 2004 */
  if (FAIL == (vsref = VSfind(fid, VSNAME0)))  {
         num_errs++;
         return FAIL;
      }
  if (FAIL == (vsid = VSattach(fid, vsref, "r")))  {
         num_errs++;
         return FAIL;
      }
  if (FAIL != VSsetattr(vsid, 1, "NO ATTRIBUTE", DFNT_FLOAT32,1, &attr4[1])) {
         num_errs++;
         printf(">>> VSsetattr did not fail on read access vdata.\n");
      }
  if (FAIL == VSdetach(vsid)) {
         num_errs++;
         return FAIL;
  }
   /* create vgroup and add attrs */
   if (FAIL == (vgid = (Vattach(fid, -1, "w"))))   {
         num_errs++;
         return FAIL;
      }
   if (FAIL == Vsetname(vgid, VGNAME2)) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == Vaddtagref(vgid, DFTAG_VH, vsref))  {
         num_errs++;
         return FAIL;
      }
   if (FAIL == Vsetclass(vgid, VGCLASS2)) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == Vdetach(vgid)) {
         num_errs++;
         return FAIL;
      }
   /* attach again, add attr */
   if (FAIL == (vgref = Vfind(fid, VGNAME2)))   {
         num_errs++;
         return FAIL;
      }
   if (FAIL == (vgid = (Vattach(fid, vgref, "w"))))   {
         num_errs++;
         return FAIL;
      }
   if (FAIL == Vsetattr(vgid, ATTNAME9, DFNT_UINT32, 1, attr1)) {
         num_errs++;
         printf(">>> Vsetattr1 failed\n");
      }
   /* change data type of existing attr, should fail */
   if (FAIL != Vsetattr(vgid, ATTNAME9, DFNT_UINT16, 1, attr2)) {
         num_errs++;
         printf(">>> Vsetattr changes attr type, should failed\n");
      }
   if (FAIL == Vsetattr(vgid, ATTNAME10, DFNT_UINT16, 2, attr2)) {
         num_errs++;
         printf(">>> Vsetattr1 failed\n");
      }
   if (FAIL == Vdetach(vgid)) {
         num_errs++;
         printf(">>> Vdetach failed in vgname2.\n");
      }

   if (FAIL == Vend(fid))   {
         num_errs++;
         return FAIL;
      }
   if (FAIL == Hclose(fid))  {
         num_errs++;
         return FAIL;
      }
   return SUCCEED;
}  /* write_vattr */

/*  Test reading routines */
static intn read_vattrs(void)
{
   int32 fid, vgid, vsid, vgref, vsref;
   intn n_vgattrs, n_vsattrs, n_fldattrs;
   intn  iattrindex, ret;
   int32 i_type, i_count, i_size, iversion;
   char iattrname[FIELDNAMELENMAX+1];

   if (FAIL == (fid = Hopen(FILENAME, DFACC_RDONLY, 0))) {
         num_errs++;
         return FAIL;
      }
   if (Vstart(fid) == FAIL)  {
         num_errs++;
         return FAIL;
      }
   /* Read vgroup attrs first */
   if (FAIL == (vgref = Vfind(fid, VGNAME0))) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == (vgid = (Vattach(fid, vgref, "r"))))   {
         num_errs++;
         return FAIL;
      }
   if ((iversion = Vgetversion(vgid)) < VSET_NEW_VERSION) {
         num_errs++;
         printf(">>> Wrong Vgroup version, should be %d, got %d.\n", 
                      VSET_NEW_VERSION, (int)iversion); 
      }
   if (FAIL == (n_vgattrs = Vnattrs(vgid)) || n_vgattrs != 2) {
         num_errs++;
         printf(">>> Wrong num of Vgroup0 attrs, should be %d, got %d.\n", 
                 2, n_vgattrs);
   }
   if (0 != (iattrindex = Vfindattr(vgid, ATTNAME1)))  {
         num_errs++;
         printf(">>> attname1 should be 0th attr of vgname0, ");
         printf("not %d.\n", iattrindex);
      }
   if (FAIL == Vattrinfo(vgid,0,iattrname,&i_type,&i_count,&i_size) ||
       HDstrncmp(iattrname, ATTNAME1, HDstrlen(ATTNAME1)) != 0 ||
       i_type != DFNT_UINT32 || i_count != 2 ||
       i_size != i_count * DFKNTsize(DFNT_UINT32 | DFNT_NATIVE))   {
         num_errs++;
         printf(">>> Wrong attrinfo for attname1 of vgname0; \
             got %s %d %d %d.\n", iattrname, (int)i_type,(int)i_count,(int)i_size);
      }
   if (FAIL == Vgetattr(vgid, 0, iattr1) ||
       iattr1[0] != attr1[2] || iattr1[1] != attr1[3])  {
         num_errs++;
         printf(">>> Wrong values for attname1 of vgname0; ");
         printf("got %u %u, should be %u %u.\n", (unsigned)iattr1[0],
             (unsigned)iattr1[1], (unsigned)attr1[2], (unsigned)attr1[3]);
      }

   if (FAIL == Vattrinfo(vgid,1,iattrname,&i_type,&i_count,&i_size) ||
       HDstrncmp(iattrname, ATTNAME2,HDstrlen(ATTNAME2)) != 0 ||
       i_type != DFNT_UINT16 || i_count != 2 ||
       i_size != i_count * DFKNTsize(DFNT_UINT16 | DFNT_NATIVE))   {
         num_errs++;
         printf(">>> Wrong attrinfo for attname2 of vgname0; \
            got %s %d %d %d.\n", iattrname, (int)i_type,(int)i_count,(int)i_size);
      }
   if (FAIL == Vgetattr(vgid, 1, iattr2) ||
       iattr2[0] != attr2[0] || iattr2[1] != attr2[1])  {
         num_errs++;
         printf(">>> Wrong values for attname2 of vgname0; \
                     got %u %u, should be %u %u.\n",
                     iattr2[0], iattr2[1], attr2[0], attr2[1]);
      }
   if (FAIL == Vdetach(vgid))  {
         num_errs++;
         printf(">>>Vdetach failed in vgname0.\n");
   }
   /* VGNAME1 has no attr */
   if (FAIL == (vgref = Vfind(fid, VGNAME1))) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == (vgid = (Vattach(fid, vgref, "r"))))   {
         num_errs++;
         return FAIL;
      }
   if ((iversion = Vgetversion(vgid)) != VSET_VERSION) {
         num_errs++;
         printf(">>> Wrong Vgroup version, should be %d, got %d.\n",
                      VSET_VERSION, (int)iversion);
      }
   if (FAIL == (n_vgattrs = Vnattrs(vgid)) || n_vgattrs != 0) {
         num_errs++;
         printf(">>> Wrong num of Vgroup1 attrs, should be %d, got %d.\n",
             0, n_vgattrs);
   }
   if (FAIL == Vdetach(vgid))  {
         num_errs++;
         printf(">>>Vdetach failed in detaching vgname1.\n");
   }
   /* VSNAME0 has 5 attrs */
   if (FAIL == (vsref = VSfind(fid, VSNAME0))) {
       num_errs++;
       return FAIL;
      }
   if (FAIL == (vsid = (VSattach(fid, vsref, "r"))))   {
       num_errs++;
       return FAIL;
      }
   if ((iversion = VSgetversion(vsid)) != VSET_NEW_VERSION) {
       num_errs++;
       printf(">>> Wrong Vdata version, should be %d, got %d.\n",
                      VSET_NEW_VERSION, (int)iversion);
      }
   if (FAIL == (n_vsattrs = VSnattrs(vsid)) || n_vsattrs != 5) {
       num_errs++;
       printf(">>> Wrong num of Vsname0 attrs, should be %d, got %d.\n",
                 5, n_vsattrs);
   }
   /* get num of attrs of fld 1 */
   if (FAIL == (n_fldattrs = VSfnattrs(vsid, 1)) || n_fldattrs != 1) {
        num_errs++;
        printf(">>> Wrong num of Vsname0 fld 1 attrs, ");
        printf("should be %d, got %d.\n ", 1, n_vsattrs);
   }
   /* read the 3rd attr of fld 0. The attr is char type. */
   if ((FAIL == VSattrinfo(vsid, 0, 2, iattrname,
               &i_type, &i_count, &i_size)) ||
               (HDstrcmp(iattrname, ATTNAME9) != 0) ||
               (i_type != DFNT_CHAR8) ||
               (i_count != 5) || (i_size != 5)) {
        num_errs++;
        printf(">>> Wrong attrinfo for attname9 of vsname0 fld0; ");
        printf(" got  %s %d %d %d.\n", iattrname, (int)i_type,
                 (int)i_count, (int)i_size);
   }
   if (FAIL == VSgetattr(vsid, 0, 2, iattr3) ||
       iattr3[0] != attr3[0] || iattr3[1] != attr3[1] ||
       iattr3[2] != attr3[2] || iattr3[3] != attr3[3] ||
       iattr3[4] != attr3[4] )  {
         num_errs++;
         printf(">>> Wrong values for attname9  of vsname0; \
                     got %5s, should be %5s.\n",
                     iattr3, attr3);
   }
   if (FAIL == VSdetach(vsid))  {
       num_errs++;
       printf(">>>Vsdetach failed in vsname0.\n");
   }
   /* VSNAME1 has 2 attrs, fld0  has 3 and fld1 has 1 attr */
   if (FAIL == (vsref = VSfind(fid, VSNAME1))) {
         num_errs++;
         return FAIL;
      }
   if (FAIL == (vsid = (VSattach(fid, vsref, "r"))))   {
         num_errs++;
         return FAIL;
      }
   if ((iversion = VSgetversion(vsid)) != VSET_NEW_VERSION) {
         num_errs++;
         printf(">>> Wrong Vdata version, should be %d, got %d.\n",
                      VSET_NEW_VERSION, (int)iversion);
      }
   if (FAIL == (n_vsattrs = VSnattrs(vsid)) || n_vsattrs != 6) {
         num_errs++;
         printf(">>> Wrong num of Vsname1 attrs, should be %d, ",6);
         printf("got %d.\n", n_vsattrs);
   }
   if (FAIL == (n_fldattrs = VSfnattrs(vsid, _HDF_VDATA)) || n_fldattrs != 2) {
        num_errs++;
        printf(">>> Wrong num of Vsname1 vdata attrs, ");
        printf("should be %d, got %d.\n ", 2, n_fldattrs);
   }
   /* look for non-existing attr, should fail */
   if (FAIL != (iattrindex = VSfindattr(vsid, _HDF_VDATA, ATTNAME9))) {
        num_errs++;
        printf(">>> attname9 is not an attr of vdata vsname1, ");
        printf(" should fail.\n");
   }
   /* use wrong findex, should fail */
   if (FAIL != (iattrindex = VSfindattr(vsid, 3, ATTNAME9))) {
        num_errs++;
        printf(">>> Vdata vsname1 has only 2 fields, ");
        printf(" should fail.\n");
   }
   /* use wrong findex, should fail */
   if (FAIL != VSattrinfo(vsid, 4, 0, NULL, &i_type, &i_count,
                          &i_size)) {
        num_errs++;
        printf(">>> Vdata vsname1 has only 2 fields, should fail.\n");
   }
   /* use wrong attrindex, should fail */
   if (FAIL != VSattrinfo(vsid, 1, 3, iattrname, &i_type, &i_count, NULL)) {
        num_errs++;
        printf(">>> Field1 of vsname1 has only 1 attr, should fail.\n");
   }
   /* use wrong findex, should fail */
   if (FAIL != VSgetattr(vsid, 4, 0, iattr1)) {
        num_errs++;
        printf(">>> Vdata vsname1 has only 2 fields, should fail.\n");
   }
   /* use wrong attrindex, should fail */
   if (FAIL != VSgetattr(vsid, 1, 3, iattr1)) {
        num_errs++;
        printf(">>> Field1 of vsname1 has only 1 attr, should fail.\n");
   }

   /* get the 2nd attr  */
   if ((FAIL == (iattrindex = VSfindattr(vsid, _HDF_VDATA, ATTNAME4))) ||
             (iattrindex != 1))  {
        num_errs++;
        printf(">>> attname4 should be index 1 of vsname1, not %d.\n",
                     iattrindex);
   }
   if ((FAIL == VSattrinfo(vsid, _HDF_VDATA, iattrindex, iattrname, 
               &i_type, &i_count, &i_size)) || 
               (HDstrcmp(iattrname, ATTNAME4) != 0) ||
               (i_type != DFNT_FLOAT32) || (i_count != 1) || 
               (i_size != DFKNTsize(DFNT_FLOAT | DFNT_NATIVE))) {
        num_errs++;
        printf(">>> Wrong attrinfo for attname4 of vdata vsname1; ");
        printf(" got  %s %d %d.\n", iattrname, (int)i_type,(int)i_count);
   }
   if (FAIL == VSgetattr(vsid, _HDF_VDATA, 1, iattr4) ||
       (fabs((double)(iattr4[0] - attr4[0])) > fabs((double)(attr4[0]*EPS32))))  {
         num_errs++;
         printf(">>> Wrong values for attname4  of vsname1; \
                     got %f, should be %f.\n",
                     iattr4[0], attr4[0]);
   }
   if (FALSE != VSisattr(vsid)) {
      num_errs++;
      printf(">>> VSisattr failed. Vsname1 is not attribute vdata.\n"); 
   }
   /* get  the 3rd attr of fld0. The attr name is VSNAME1 */
   if ((FAIL == (iattrindex = VSfindattr(vsid, 0, VSNAME1))) ||
             (iattrindex != 2))  {
        num_errs++;
        printf(">>> VSNAME1 should be index 2 of fld 0 of vsname1,");
        printf("  not %d.\n", iattrindex);
   }
   if ((FAIL == VSattrinfo(vsid, 0, iattrindex, iattrname,
               &i_type, &i_count, &i_size)) ||
               (HDstrcmp(iattrname, VSNAME1) != 0) ||
               (i_type != DFNT_FLOAT64) || (i_count != 1) || 
               (i_size != DFKNTsize(DFNT_FLOAT64 | DFNT_NATIVE))) {
        num_errs++;
        printf(">>> Wrong attrinfo for VSNAME1 of fld 0 of vdata vsname1; ");
        printf(" got  %s %d %d.\n", iattrname, (int)i_type,(int)i_count);
   }
   if (FAIL == VSgetattr(vsid, 0, 2, iattr5) ||
       (fabs((double)(iattr5[0] - attr5[0])) > fabs((double)(attr5[0]*EPS64))) )  {
         num_errs++;
         printf(">>> Wrong values for attr VSNAME1  of fld 0 of vsname1; \
                     got %f, should be %f.\n",
                     iattr5[0], attr5[0]);
   }
   if (FAIL == VSdetach(vsid))  {
      num_errs++;
      printf(">>>VSdetach failed in vsname1.\n");
   }
   /* test VSisattr for attr vdata */
   if (FAIL == (vsref = VSfind(fid, ATTNAME10))) {
       num_errs++;
       return FAIL;
      }
   if (FAIL == (vsid = (VSattach(fid, vsref, "r"))))   {
       num_errs++;
       return FAIL;
      }
   if (TRUE != VSisattr(vsid)) {
      num_errs++;
      printf(">>> VSisattr failed. ATTNAME10 is an attribute vdata.\n");
   }
   if (VSET_VERSION != (iversion = VSgetversion(vsid)))  {
        num_errs++;
        printf(">>> Wrong vdata version. ATTNAME10 should be of ");
        printf(" %d, got %d\n", VSET_VERSION, (int)iversion);
   }
   ret = VSdetach(vsid);
   CHECK(ret, FAIL, "VSdetach");
   ret = Vend(fid);
   CHECK(ret, FAIL, "Vend");
   ret = Hclose(fid);
   CHECK(ret, FAIL, "Hclose");  

   return 0;
}

/* main test driver */
void
test_vset_attr(void)   
{
   write_vset_stuff();
   write_vattrs();
   read_vattrs(); 
} /* test_vset_attr */
