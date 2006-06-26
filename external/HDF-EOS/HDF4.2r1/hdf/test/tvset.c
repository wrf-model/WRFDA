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
static char RcsId[] = "@(#)$Revision: 1.37 $";
#endif

/* $Id: tvset.c,v 1.37 2001/07/03 05:27:40 bmribler Exp $ */

/*
 *
 * Vset tests 
 *
 *
 * This file needs another pass at making sure all the return
 * values from function calls are checked in addtion to
 * verifying that the proper tests are performed on all Vxx fcns - GV 9/5/97
 *
 */
#include "hdf.h"
#include "hfile.h"
#include "tproto.h"

#define VDATA_COUNT  256   /* make this many Vdatas to check for memory leaks */

#define FNAME0   "tvset.hdf"
#define FNAME1   "tvset1.hdf"
#define FNAME2   "tvset2.hdf"
#define EXTFNM	 "tvsetext.hdf"
#define EMPTYNM  "tvsempty.hdf"
#define BLKINFO  "tvsblkinfo.hdf"

#define FIELD1       "FIELD_name_HERE"
#define FIELD1_UPPER "FIELD_NAME_HERE"
#define FIELD2       "DIFFERENT_FIELD_NAME"

#define ST "STATION_NAME"
#define VL "VALUES"
#define FL "FLOATS"
#define MX "STATION_NAME,VALUES,FLOATS"
#define EMPTY_VDATA "Empty"
#define APPENDABLE_VDATA "Appendable"

static int32 write_vset_stuff(void);
static int32 read_vset_stuff(void);
static void test_vsdelete(void);
static void test_vdelete(void);
static void test_vdeletetagref(void);
static void test_emptyvdata(void);

/* write some stuff to the file */
static int32
write_vset_stuff(void)
{
    int32       status;
    int32       fid, aid;
    int32       vg1, vg2;
    int32       vs1;
    int32       count, i, j, num, max_order;
    int32       ibuf[2000];     /* integer buffer */
    float32     fbuf[2000];     /* floating point buffer */
    char        gbuf[2000];     /* generic buffer */
    uint8       *gbuf1 = NULL;   /* buffer for uint8 */
    float32     *gbuf2 = NULL;   /* buffer for float32 */
    const char *name;
    char       *p;
    char8       c;
    float32     f;

    /* allocate these buffers dynamically and not off the stack
       as they were previously handled */
    if (gbuf1 == NULL)
     {
       gbuf1 = (uint8 *)HDmalloc(sizeof(uint8)*65536);
     }

    if (gbuf2 == NULL)
     {
       gbuf2 = (float32 *)HDmalloc(sizeof(float32)*20000);
     }


    fid = Hopen(FNAME0, DFACC_CREATE, 100);
    if (fid == FAIL)
      {
          num_errs++;
          return FAIL;
      }

    if (Vstart(fid) == FAIL)
      {
          num_errs++;
          return FAIL;
      }

    /*

     * Vgroup Generation routines
     *
     */

    /*
     *  start simple --- create a simple Vgroup
     */
    vg1 = Vattach(fid, -1, "w");
    if (vg1 == FAIL)
      {
          num_errs++;
          printf(">>> Failed creating initial Vgroup\n");
      }

    status = Vsetname(vg1, "Simple Vgroup");
    CHECK(status,FAIL,"Vsetname:vg1");

    status = Vsetclass(vg1, "Test object");
    CHECK(status,FAIL,"Vsetclass:vg1");

    MESSAGE(5, printf("created Vgroup %s (empty)\n", "Simple Vgroup"););

    /*
     * Lets do some more complex ones now
     */
    vg2 = Vattach(fid, -1, "w");
    if (vg2 == FAIL)
      {
          num_errs++;
          printf(">>> Failed creating second Vgroup\n");
      }

    /* keep track of how many in Vgroup */
    num = 0;

    /* add first group into the other */
    status = Vinsert(vg2, vg1);
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> Vinsert failed\n");
      }
    else
        num++;

    /* add a bogus element */
    status = Vaddtagref(vg2, (int32) 1000, (int32) 12345);
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> Vaddtagref failed for bogus element\n");
      }
    else
        num++;

    /* create an element and insert that */
    aid = Hstartwrite(fid, (uint16) 123, (uint16) 1234, 10);
    if (aid == FAIL)
      {
          num_errs++;
          printf(">>> Hstartwrite failed\n");
      }

    status = Hendaccess(aid);
    CHECK(status,FAIL,"Hendaccess:aid");

    /* add an existing HDF element */
    status = Vaddtagref(vg2, (int32) 123, (int32) 1234);
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> Vaddtagref failed for legit element\n");
      }
    else
        num++;

#ifdef NO_DUPLICATES
    /* attempt to add an element already in the Vgroup */
    status = Vaddtagref(vg2, (int32) 123, (int32) 1234);
    if (status != FAIL)
      {
          num_errs++;
          printf(">>> Vaddtagref added a duplicate element\n");
      }

    /* check that the number is correct */
    if (num != Vntagrefs(vg2))
      {
          num_errs++;
          printf(">>> Vntagrefs returned %d was expecting %d\n", Vntagrefs(vg2), num);
      }
#endif /* NO_DUPLICATES */

    /* lets check the contents */
    /* look for a valid one first */
    if (Vinqtagref(vg2, 1000, 12345) == FALSE)
      {
          num_errs++;
          printf(">>> Vinqtagref couldn't find valid element\n");
      }

    /* look for a bogus one */
    if (Vinqtagref(vg2, 1000, 123456) != FALSE)
      {
          num_errs++;
          printf(">>> Vinqtagref found a bogus element\n");
      }

    status = Vsetname(vg2, "Second Vgroup");
    CHECK(status,FAIL,"Vsetname:for vg2");

    Vsetclass(vg2, "Test object");
    CHECK(status,FAIL,"Vsetclass: for vg2");

    status = Vdetach(vg1);
    CHECK(status,FAIL,"Vdetach:vg1");

    status = Vdetach(vg2);
    CHECK(status,FAIL,"Vdetach:vg2");

    MESSAGE(5, printf("created Vgroup %s with %d elements\n", "Second Vgroup", 
                      (int) num););

    /*

     * Vdata Generation routines
     *
     */

    /* Float32 Vdata */
    vs1 = VSattach(fid, -1, "w");
    CHECK(vs1,FAIL,"VSattach");

    name = "Float Vdata";
    status=VSsetname(vs1, name);
    CHECK(status,FAIL,"VSsetname");

    status=VSsetclass(vs1, "Test object");
    CHECK(status,FAIL,"VSsetclass");

    status=VSfdefine(vs1, FIELD1, DFNT_FLOAT32, 1);
    CHECK(status,FAIL,"VSfdefine");

    /* Verify that VSsetfields will return FAIL when passing in a NULL
       for field name list (bug #554) - BMR 5/17/01 */
    status = VSsetfields(vs1, NULL);
    VERIFY(status, FAIL, "VSsetfields");

    status = VSsetfields(vs1, FIELD1);
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> Vsetfields failed for %s\n", name);
      }

    /* create some bogus data */
    for (i = 0, count = 100; i < count; i++)
        fbuf[i] = (float32) i;

    /* store it */
    status = VSwrite(vs1, (unsigned char *) fbuf, count, FULL_INTERLACE);
    CHECK(status,FAIL,"VSwrite:vs1");

    status = VSdetach(vs1);
    CHECK(status,FAIL,"Vdetach:vs1");
    
    MESSAGE(5, printf("created VDATA %s with %d elements\n", name, (int) count););

    /* Int32 Vdata */
    vs1 = VSattach(fid, -1, "w");
    CHECK(vs1,FAIL,"VSattach:vs1");

    name = "Integer Vdata";
    status = VSsetname(vs1, name);
    CHECK(status,FAIL,"VSsetname:vs1");

    status = VSsetclass(vs1, "Test object");
    CHECK(status,FAIL,"VSsetclass:vs1");

    status = VSfdefine(vs1, FIELD2, DFNT_INT32, 2);
    CHECK(status,FAIL,"VSfdefine:vs1");

    status = VSsetfields(vs1, FIELD2);
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> Vsetfields failed for %s\n", name);
      }
    /* change this vdata to store in an external file */
    status = VSsetexternalfile(vs1, EXTFNM, (int32) 0);
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> VSsetexternalfile failed\n");
      }

    /* create some bogus data */
    for (i = 0, count = 100; i < 2 * count; i++)
        ibuf[i] = i;

    /* store it */
    status = VSwrite(vs1, (unsigned char *) ibuf, count, FULL_INTERLACE);
    CHECK(status,FAIL,"VSwrite:vs1");

    status = VSdetach(vs1);
    CHECK(status,FAIL,"VSdetach:vs1");

    MESSAGE(5, printf("created VDATA %s with %d elements\n", 
                      name, (int) count); );

    /* Int32 and Float32 Vdata */
    vs1 = VSattach(fid, -1, "w");
    CHECK(vs1,FAIL,"VSattach:vs1");

    name = "Mixed Vdata";
    status = VSsetname(vs1, name);
    CHECK(status,FAIL,"VSsetname:vs1");

    status = VSsetclass(vs1, "No class specified");
    CHECK(status,FAIL,"VSsetclass:vs1");

    status = VSfdefine(vs1, "A", DFNT_FLOAT32, 1);
    CHECK(status,FAIL,"VSfdefine:vs1");

    status = VSfdefine(vs1, "B", DFNT_INT32, 1);
    CHECK(status,FAIL,"VSfdefine:vs1");

    status = VSsetfields(vs1, "A, B");
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> Vsetfields failed for %s\n", name);
      }

    /* create some bogus data */
    p = gbuf;
    for (i = 0, count = 100; i < count; i++)
      {
          float32     tf = (float32) (i * 2);
          HDmemcpy(p, &tf, sizeof(float32));
          p += sizeof(float32);
          HDmemcpy(p, &i, sizeof(int32));
          p += sizeof(int32);
      }

    /* store it */
    status = VSwrite(vs1, (unsigned char *) gbuf, count, FULL_INTERLACE);
    CHECK(status,FAIL,"VSwrite:vs1");

    status = VSdetach(vs1);
    CHECK(status,FAIL,"VSdetach:vs1");

    MESSAGE(5, printf("created VDATA %s with %d elements\n", 
                      name, (int) count););

    /* mixed order Vdata */
    vs1 = VSattach(fid, -1, "w");
    CHECK(vs1,FAIL,"VSattach:vs1");

    name = "Multi-Order Vdata";
    status = VSsetname(vs1, name);
    CHECK(status,FAIL,"VSsetname:vs1");

    status = VSsetclass(vs1, "No class specified");
    CHECK(status,FAIL,"VSsetclass:vs1");

    status = VSfdefine(vs1, ST, DFNT_CHAR8, 2);
    CHECK(status,FAIL,"VSfdefine:vs1");

    status = VSfdefine(vs1, VL, DFNT_INT32, 3);
    CHECK(status,FAIL,"VSfdefine:vs1");

    status = VSfdefine(vs1, FL, DFNT_FLOAT32, 1);
    CHECK(status,FAIL,"VSfdefine:vs1");

    status = VSsetfields(vs1, MX);
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> Vsetfields failed for %s\n", name);
      }

    /* create some bogus data */
    p = gbuf;
    c = 'a';
    j = 0;
    f = (float32) 15.5;
    for (i = 0, count = 10; i < count; i++)
      {
          HDmemcpy(p, &c, sizeof(char8));
          p += sizeof(char8);
          c++;
          HDmemcpy(p, &c, sizeof(char8));
          p += sizeof(char8);
          c++;
          HDmemcpy(p, &j, sizeof(int32));
          p += sizeof(int32);
          j++;
          HDmemcpy(p, &j, sizeof(int32));
          p += sizeof(int32);
          j++;
          HDmemcpy(p, &j, sizeof(int32));
          p += sizeof(int32);
          j++;
          HDmemcpy(p, &f, sizeof(float32));
          p += sizeof(float32);
          f += (float32) 0.5;
      }

    /* store it */
    status = VSwrite(vs1, (unsigned char *) gbuf, count, FULL_INTERLACE);
    CHECK(status,FAIL,"VSwrite:vs1");

    status = VSdetach(vs1);
    CHECK(status,FAIL,"VSdetach:vs1");

    MESSAGE(5, printf("created VDATA %s with %d elements\n", 
                      name, (int) count););

    /* test MAX_ORDER and MAX_FIELD_SIZE */
    vs1 = VSattach(fid, -1, "w");
    CHECK(vs1,FAIL,"VSattach:vs1");

    name = "Max_Order Vdata";
    status = VSsetname(vs1, name);
    CHECK(status,FAIL,"VSsetname:vs1");

    status = VSfdefine(vs1, "max_order", DFNT_UINT8, MAX_ORDER);
    CHECK(status,FAIL,"VSfdefine:vs1");

    status = VSsetfields(vs1, "max_order");
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> Vsetfields failed for %s\n", name);
      }

    /* create some bogus data */
    for (i = 0; i < MAX_ORDER; i++)
         gbuf1[i] = (uint8)(i % 256);

    status = VSwrite(vs1, (unsigned char *) gbuf1, 1, FULL_INTERLACE);
    CHECK(status,FAIL,"VSwrite:vs1");

    status = VSdetach(vs1);
    CHECK(status,FAIL,"VSdetach:vs1");

    MESSAGE(5, printf("created VDATA %s with %d order\n", 
                      name, (int)MAX_ORDER););

    vs1 = VSattach(fid, -1, "w");
    CHECK(vs1,FAIL,"VSattach:vs1");

    name = "Max_Fldsize Vdata";
    status = VSsetname(vs1, name);
    CHECK(status,FAIL,"VSsetname:vs1");

    max_order = MAX_FIELD_SIZE/SIZE_FLOAT32;
    status = VSfdefine(vs1, "max_fldsize", DFNT_FLOAT32, max_order);
    CHECK(status,FAIL,"VSfdefine:vs1");

    status = VSsetfields(vs1, "max_fldsize");
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> Vsetfields failed for %s\n", name);
      }
    
    /* create some bogus data */
    for (i = 0; i < max_order; i++)
         gbuf2[i] = (float32)i * (float32)0.11; 

    status = VSwrite(vs1, (unsigned char *) gbuf2, 1, FULL_INTERLACE);
    CHECK(status,FAIL,"VSwrite:vs1");

    status = VSdetach(vs1);
    CHECK(status,FAIL,"VSdetach:vs1");

    MESSAGE(5, printf("created VDATA %s with %d order\n", 
                      name, (int)max_order););

    /* create vdata exceeding MAX_FIELD_SIZE, should fail */
    vs1 = VSattach(fid, -1, "w");
    CHECK(vs1,FAIL,"VSattach:vs1");

    name = "Bad_Fldsize Vdata";
    status = VSsetname(vs1, name);
    CHECK(status,FAIL,"VSsetname:vs1");

    max_order = MAX_FIELD_SIZE/SIZE_FLOAT32 + 1;
    status = VSfdefine(vs1, "bad_fldsize", DFNT_FLOAT32, max_order);
    if (status != FAIL)
      {
          num_errs++;
          printf(">>> Vsetfields failed for %s\n", name);
      }

    status = VSsetfields(vs1, "bad_fldsize");
    if (status != FAIL)
      {
          num_errs++;
          printf(">>> Vsetfields failed for %s\n", name);
      }

    status = VSdetach(vs1);
    CHECK(status,FAIL,"VSdetach:vs1");

    /* create a whole bunch of Vdatas to check for memory leakage */
    for (i = 0; i < VDATA_COUNT; i++)
      {
          char        name2[80];
          vs1 = VSattach(fid, -1, "w");
          if (vs1 == FAIL)
            {
                num_errs++;
                printf(">>> Vsattach failed on loop %d\n", (int) i);
                continue;
            }
          sprintf(name2, "VdataLoop-%d", (int) i);
          status = VSsetname(vs1, name2);
          CHECK(status,FAIL,"VSsetname:vs1");

          status = VSfdefine(vs1, "A", DFNT_CHAR8, 1);
          if (status == FAIL)
            {
                num_errs++;
                printf(">>> VSfdefine failed on loop %d\n", (int) i);
                continue;
            }
          status = VSsetfields(vs1, "A");
          if (status == FAIL)
            {
                num_errs++;
                printf(">>> VSsetfields failed on loop %d\n", (int) i);
                continue;
            }
          status = VSwrite(vs1, (unsigned char *) name2, 1, FULL_INTERLACE);
          CHECK(status,FAIL,"VSwrite:vs1");

          status = VSdetach(vs1);
          CHECK(status,FAIL,"VSdetach:vs1");
      }

    status = Vend(fid);
    CHECK(status,FAIL,"Vend:fid");

    status = Hclose(fid);
    CHECK(status,FAIL,"Hclose:vs1");

    HDfree(gbuf1);
    HDfree(gbuf2);

    return SUCCEED;

}   /* write_vset_stuff */

/* read everything back in and check it */
static int32
read_vset_stuff(void)
{
    int32       ibuf[2000];     /* integer buffer */
    float32     fbuf[2000];     /* floating point buffer */
    char        gbuf[2000];     /* generic buffer */
    int32       list[50];
    int32       tags[100], refs[100], tag, ref;
    char        name[512], class[512], fields[512];
    char       *p;
    int32       fid;
    int32       vg1;
    int32       vs1;
    int32       status, num, i, count, intr, sz;
    float32     fl_expected;
    int32       in_expected;
    char8       c_expected;

    fid = Hopen(FNAME0, DFACC_RDONLY, 0);
    if (fid == FAIL)
      {
          num_errs++;
          return FAIL;
      }

    status = Vstart(fid);
if(status==FAIL)
    HEprint(stderr,0);
    CHECK(status,FAIL,"Vstart:fid");

    /*

     *   Verify the Vgroups
     *
     */

    /* test Vlone */
    num = 1;
    status = Vlone(fid, list, 10);
    if (status != num)
      {
          num_errs++;
          printf(">>> Vlone found %d was expecting %d\n", (int) status, (int) num);
      }

    /* test Vgetname and Vgetclass */
    vg1 = Vattach(fid, list[0], "r");
    if (vg1 == FAIL)
      {
          num_errs++;
          printf(">>> Was not able to attach (r) Vgroup %d\n", (int) list[0]);
      }

    status = Vgetname(vg1, name);
    CHECK(status,FAIL,"Vgetname:vg1");

    status = Vgetclass(vg1, class);
    CHECK(status,FAIL,"Vgetclass:vg1");

    if (HDstrcmp(name, "Second Vgroup"))
      {
          num_errs++;
          printf(">>> Got bogus Vgroup name : %s\n", name);
      }

    if (HDstrcmp(class, "Test object"))
      {
          num_errs++;
          printf(">>> Got bogus Vgroup class : %s\n", class);
      }

    num = 3;
    status = Vgettagrefs(vg1, tags, refs, 100);
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> Vgettagrefs found %d was expecting %d\n", (int) status, (int) num);
      }

    for (i = 0; i < num; i++)
      {
          status = Vgettagref(vg1, i, &tag, &ref);
          if (status == FAIL)
            {
                num_errs++;
                printf(">>> Vgettagref failed on call %d\n", (int) i);
            }

          if (tag != tags[i])
            {
                num_errs++;
                printf(">>> Vgettagref Tag #%d disagrees %d %d\n", (int) i, (int) tag, (int) tags[i]);
            }

          if (ref != refs[i])
            {
                num_errs++;
                printf(">>> Vgettagref Ref #%d disagrees %d %d\n", (int) i, (int) ref, (int) refs[i]);
            }

      }

    status = Vdetach(vg1);
    CHECK(status,FAIL,"Vdetach:vg1");

    /* test Vgetid */
    ref = Vgetid(fid, -1);
    if (ref == FAIL)
      {
          num_errs++;
          printf(">>> Vgetid was unable to find first Vgroup\n");
      }

    ref = Vgetid(fid, ref);
    if (ref != list[0])
      {
          num_errs++;
          printf(">>> Vgetid was unable to find second Vgroup (should have been first lone one)\n");
      }

    /*

     *   Verify the Vdatas
     *
     */

    /* test VSgetid */
    ref = VSgetid(fid, -1);
    if (ref == FAIL)
      {
          num_errs++;
          printf(">>> VSgetid was unable to find first Vdata\n");
      }

    /* read in the first data and verify metadata and contents */
    vs1 = VSattach(fid, ref, "r");
    CHECK(vs1,FAIL,"VSattach:vs1");

    status = VSgetname(vs1, name);
    CHECK(status,FAIL,"VSgetname:vs1");

    status = VSgetclass(vs1, class);
    CHECK(status,FAIL,"VSgetclass:vs1");

    if (HDstrcmp(name, "Float Vdata"))
      {
          num_errs++;
          printf(">>> Got bogus Vdata name (VSgetname) : %s\n", name);
      }

    if (HDstrcmp(class, "Test object"))
      {
          num_errs++;
          printf(">>> Got bogus Vdata class : %s\n", class);
      }

    status = VSinquire(vs1, &count, &intr, fields, &sz, name);
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> VSinquire failed on float Vdata\n");
      }

    if (HDstrcmp(name, "Float Vdata"))
      {
          num_errs++;
          printf(">>> Got bogus Float Vdata name (VSinquire) : %s\n", name);
      }

    if (count != 100)
      {
          num_errs++;
          printf(">>> Got wrong count %d expecting 100\n", (int) count);
      }

    if ((size_t)sz != sizeof(float32))
      {
          num_errs++;
          printf(">>> Got wrong data size %d should be sizeof(float32)\n", (int) sz);
      }

#ifndef VDATA_FIELDS_ALL_UPPER
    if (HDstrcmp(fields, FIELD1))
      {
          num_errs++;
          printf(">>> Got bogus field name %s\n", fields);
      }
#else
    if (HDstrcmp(fields, FIELD1_UPPER))
      {
          num_errs++;
          printf(">>> Got bogus field name %s\n", fields);
      }
#endif /* VDATA_FIELDS_ALL_UPPER */

    /* read it */
    status = VSsetfields(vs1, fields);
    CHECK(status,FAIL,"VSsetfields:vs1");

    for (i = 0; i < count; i++)
        fbuf[i] = (float32)0.0;

    status = VSread(vs1, (unsigned char *) fbuf, count, FULL_INTERLACE);
    CHECK(status,FAIL,"VSread:vs1");

    /* verify */
    for (i = 0; i < count; i++)
      {
          if (fbuf[i] != (float32) i)
            {
                num_errs++;
                printf(">>> Float value %d was expecting %d got %f\n", (int) i, (int) i, fbuf[i]);
            }
      }

    status = VSdetach(vs1);
    CHECK(status,FAIL,"VSdetach:vs1");

    /* Move to the next one (integers) */
    ref = VSgetid(fid, ref);
    if (ref == FAIL)
      {
          num_errs++;
          printf(">>> VSgetid was unable to find second Vdata\n");
      }

    /* read in the first data and verify metadata and contents */
    vs1 = VSattach(fid, ref, "r");
    CHECK(vs1,FAIL,"VSattach:vs1");

    status = VSgetname(vs1, name);
    CHECK(status,FAIL,"VSgetname:vs1");

    status = VSgetclass(vs1, class);
    CHECK(status,FAIL,"VSgetclass:vs1");

    if (HDstrcmp(name, "Integer Vdata"))
      {
          num_errs++;
          printf(">>> Got bogus Vdata name (VSgetname) : %s\n", name);
      }

    if (HDstrcmp(class, "Test object"))
      {
          num_errs++;
          printf(">>> Got bogus Vdata class : %s\n", class);
      }

    status = VSinquire(vs1, &count, &intr, fields, &sz, name);
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> VSinquire failed on float Vdata\n");
      }

    if (HDstrcmp(name, "Integer Vdata"))
      {
          num_errs++;
          printf(">>> Got bogus Integer Vdata name (VSinquire) : %s\n", name);
      }

    if (count != 100)
      {
          num_errs++;
          printf(">>> Got wrong count %d expecting 100\n", (int) count);
      }

    if ((size_t)sz != 2 * sizeof(int32))
      {
          num_errs++;
          printf(">>> Got wrong data size %d should be 2 * sizeof(int32)\n", (int) sz);
      }

    if (HDstrcmp(fields, FIELD2))
      {
          num_errs++;
          printf(">>> Got bogus field name %s\n", fields);
      }

    /* read it */
    status = VSsetfields(vs1, fields);
    CHECK(status,FAIL,"VSsetfields:vs1");

    for (i = 0; i < 2 * count; i++)
        ibuf[i] = 0;

    status = VSread(vs1, (unsigned char *) ibuf, count, FULL_INTERLACE);
    CHECK(status,FAIL,"VSread:vs1");

    /* verify */
    for (i = 0; i < 2 * count; i++)
      {
          if (ibuf[i] != i)
            {
                num_errs++;
                printf(">>> Int value %d was expecting %d got %d\n", (int) i, (int) i, (int) ibuf[i]);
            }
      }

    status = VSdetach(vs1);
    CHECK(status,FAIL,"VSdetach:vs1");

#ifndef HAVE_FMPOOL 
/* Commented out this test when using the file caching.This is beacause this 
   test opens the external file directly without using HDF calls. As a result
   the file memory pool buffer that was created for this external file will 
   not be shared with this low-level call as the low-level file cache open 
   creates a unique pool for every call. It is upto the programmer
   then to share the file pool. -GeorgeV
 */

    /* testing VSsetexternalfile by reading the external file directly */
    {   hdf_file_t fd;
        int j;
        int32 ival;

        /* low level open of external file */
        fd = HI_OPEN(EXTFNM, DFACC_RDONLY);
        if (OPENERR(fd))
          {
              num_errs++;
              printf(">>> Reopen External file %s failed\n", EXTFNM);
          }
        else
          {
              status = HI_READ(fd, gbuf, (2*count*DFKNTsize(DFNT_INT32)));
              if (status == FAIL)
                {
                    num_errs++;
                    printf(">>> Reading External file data failed\n");
                }
              else
                {

                    j = 0;
                    for (i = 0; i < 2 * count; i++)
                      {
                          ival = 0xff & gbuf[j++];
                          ival = ival<<8 | (0xff & gbuf[j++]);
                          ival = ival<<8 | (0xff & gbuf[j++]);
                          ival = ival<<8 | (0xff & gbuf[j++]);
                          
                          if (ival != i)
                            {
                                num_errs++;
                                printf(">>> External value %d was expecting %d got %d\n",
                                       (int) i, (int) i, (int) ival);
                            }
                      }
                }
              /* low level close of external file */
              HI_CLOSE(fd);
          }
    }
#endif /* HAVE_FMPOOL */

    /* Move to the next one (integers + floats) */
    ref = VSgetid(fid, ref);
    if (ref == FAIL)
      {
          num_errs++;
          printf(">>> VSgetid was unable to find third Vdata\n");
      }

    /* read in the first data and verify metadata and contents */
    vs1 = VSattach(fid, ref, "r");
    CHECK(vs1,FAIL,"VSattach:vs1");

    status = VSgetname(vs1, name);
    CHECK(status,FAIL,"VSgetname:vs1");

    status = VSgetclass(vs1, class);
    CHECK(status,FAIL,"VSgetclass:vs1");

    if (HDstrcmp(name, "Mixed Vdata"))
      {
          num_errs++;
          printf(">>> Got bogus Vdata name (VSgetname) : %s\n", name);
      }

    if (HDstrcmp(class, "No class specified"))
      {
          num_errs++;
          printf(">>> Got bogus Vdata class : %s\n", class);
      }

    status = VSinquire(vs1, &count, &intr, fields, &sz, name);
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> VSinquire failed on float Vdata\n");
      }

    if (HDstrcmp(name, "Mixed Vdata"))
      {
          num_errs++;
          printf(">>> Got bogus Mixed Vdata name (VSinquire) : %s\n", name);
      }

    if (count != 100)
      {
          num_errs++;
          printf(">>> Got wrong count %d expecting 100\n", (int) count);
      }

    if ((size_t)sz != sizeof(int32) + sizeof(float32))
      {
          num_errs++;
          printf(">>> Got wrong data size %d should be sizeof(int32) + sizeof(float32)\n", (int) sz);
      }

    if (HDstrcmp(fields, "A,B"))
      {
          num_errs++;
          printf(">>> Got bogus field name %s\n", fields);
      }

    /* read it */
    status = VSsetfields(vs1, fields);
    CHECK(status,FAIL,"VSsetfields:vs1");

    for (i = 0; i < 1000; i++)
        gbuf[i] = 0;

    status = VSread(vs1, (unsigned char *) gbuf, count, FULL_INTERLACE);
    CHECK(status,FAIL,"VSread:vs1");

    /* verify */
    p = gbuf;
    for (i = 0; i < count; i++)
      {
          float32     fl=(float32)0.0;
          int32       in=(int32)0;

          HDmemcpy(&fl, p, sizeof(float32));
          p += sizeof(float32);
          HDmemcpy(&in, p, sizeof(int32));
          p += sizeof(int32);

          if (in != i)
            {
                num_errs++;
                printf(">>> Mixed int value %d was expecting %d got %d\n", (int) i, (int) i, (int) in);
            }

          if (fl != (float32) (i * 2))
            {
                num_errs++;
                printf(">>> Mixed float value %d was expecting %d got %f\n", (int) i, (int) i, fl);
            }
      }

    status = VSdetach(vs1);
    CHECK(status,FAIL,"VSdetach:vs1");

    /* Move to the next one (multi-order) */
    ref = VSgetid(fid, ref);
    if (ref == FAIL)
      {
          num_errs++;
          printf(">>> VSgetid was unable to find multi-order Vdata\n");
      }

    /* read in the first data and verify metadata and contents */
    vs1 = VSattach(fid, ref, "r");
    CHECK(vs1,FAIL,"VSattach:vs1");

    status = VSgetname(vs1, name);
    CHECK(status,FAIL,"VSgetname:vs1");

    status = VSgetclass(vs1, class);
    CHECK(status,FAIL,"VSgetclass:vs1");

    if (HDstrcmp(name, "Multi-Order Vdata"))
      {
          num_errs++;
          printf(">>> Got bogus Vdata name (VSgetname) : %s\n", name);
      }

    if (HDstrcmp(class, "No class specified"))
      {
          num_errs++;
          printf(">>> Got bogus Vdata class : %s\n", class);
      }

    status = VSinquire(vs1, &count, &intr, fields, &sz, name);
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> VSinquire failed on multi-order Vdata\n");
      }

    if (count != 10)
      {
          num_errs++;
          printf(">>> Got wrong count %d expecting 10\n", (int) count);
      }

    if (HDstrcmp(fields, MX))
      {
          num_errs++;
          printf(">>> Got bogus field name %s\n", fields);
      }

    /*
     * verify - read in all fields
     */

    /* read it */
    status = VSsetfields(vs1, fields);
    CHECK(status,FAIL,"VSsetfields:vs1");

    for (i = 0; i < 1000; i++)
        gbuf[i] = 0;

    status = VSread(vs1, (unsigned char *) gbuf, count, FULL_INTERLACE);
    CHECK(status,FAIL,"VSread:vs1");

    p = gbuf;
    fl_expected = (float32) 15.5;
    in_expected = 0;
    c_expected = 'a';

    for (i = 0; i < count; i++)
      {
          float32     fl=(float32)0.0;
          int32       in=(int32)0;
          char8       c=(char8)0;

          /* read and verify characters */
          HDmemcpy(&c, p, sizeof(char8));
          p += sizeof(char8);

          if (c != c_expected)
            {
                num_errs++;
                printf(">>> Multi-order char value %d.0 was expecting %c got %c\n", (int) i, c_expected, c);
            }
          c_expected++;

          HDmemcpy(&c, p, sizeof(char8));
          p += sizeof(char8);

          if (c != c_expected)
            {
                num_errs++;
                printf(">>> Multi-order char value %d.1 was expecting %c got %c\n", (int) i, c_expected, c);
            }
          c_expected++;

          /* read and verify integers */
          HDmemcpy(&in, p, sizeof(int32));
          p += sizeof(int32);

          if (in != in_expected)
            {
                num_errs++;
                printf(">>> Multi-order int value %d.0 was expecting %d got %d\n", (int) i, (int) in_expected, (int) in);
            }
          in_expected++;
          HDmemcpy(&in, p, sizeof(int32));
          p += sizeof(int32);

          if (in != in_expected)
            {
                num_errs++;
                printf(">>> Multi-order int value %d.1 was expecting %d got %d\n", (int) i, (int) in_expected, (int) in);
            }
          in_expected++;
          HDmemcpy(&in, p, sizeof(int32));
          p += sizeof(int32);

          if (in != in_expected)
            {
                num_errs++;
                printf(">>> Multi-order int value %d.2 was expecting %d got %d\n", (int) i, (int) in_expected, (int) in);
            }
          in_expected++;

          /* read and verify floating point value */
          HDmemcpy(&fl, p, sizeof(float32));
          p += sizeof(float32);

          if (fl != fl_expected)
            {
                num_errs++;
                printf(">>> Multi-order float value %d was expecting %f got %f\n", (int) i, fl_expected, fl);
            }
          fl_expected += (float32) 0.5;

      }

    /*
     * verify - just read in the character field with FULL_INTERLACE
     */

    /* read it */
    status = VSseek(vs1, 0);
    CHECK(status,FAIL,"VSseek:vs1");

    status = VSsetfields(vs1, ST);
    CHECK(status,FAIL,"VSsetfields:vs1");

    for (i = 0; i < 1000; i++)
        gbuf[i] = 0;

    status = VSread(vs1, (unsigned char *) gbuf, count, FULL_INTERLACE);
    CHECK(status,FAIL,"VSread:vs1");

    p = gbuf;
    c_expected = 'a';

    for (i = 0; i < count; i++)
      {
          char8       c='\0';

          /* read and verify characters */
          HDmemcpy(&c, p, sizeof(char8));
          p += sizeof(char8);

          if (c != c_expected)
            {
                num_errs++;
                printf(">>> FULL_INTERLACE read char value %d.0 (%c) got %c %d\n", (int) i, c_expected, c, c);
            }
          c_expected++;

          HDmemcpy(&c, p, sizeof(char8));
          p += sizeof(char8);

          if (c != c_expected)
            {
                num_errs++;
                printf(">>> FULL_INTERLACE read char value %d.1 (%c) %c got %c\n", (int) i, c_expected, c, c);
            }
          c_expected++;

      }

    /*
     * verify - just read in the character field with NO_INTERLACE
     */

    /* read it */
    status = VSseek(vs1, 0);
    CHECK(status,FAIL,"VSseek:vs1");

    status = VSsetfields(vs1, ST);
    CHECK(status,FAIL,"VSsetfields:vs1");

    for (i = 0; i < 1000; i++)
        gbuf[i] = 0;

    status = VSread(vs1, (unsigned char *) gbuf, count, NO_INTERLACE);
    CHECK(status,FAIL,"VSread:vs1");

    p = gbuf;
    c_expected = 'a';

    for (i = 0; i < count; i++)
      {
          char8       c='\0';

          /* read and verify characters */
          HDmemcpy(&c, p, sizeof(char8));
          p += sizeof(char8);

          if (c != c_expected)
            {
                num_errs++;
                printf(">>> NO_INTERLACE read char value %d.0 (%c) got %c\n", (int) i, c_expected, c);
            }
          c_expected++;

          HDmemcpy(&c, p, sizeof(char8));
          p += sizeof(char8);

          if (c != c_expected)
            {
                num_errs++;
                printf(">>> NO_INTERLACE read char value %d.1 (%c) %c got\n", (int) i, c_expected, c);
            }
          c_expected++;

      }

    /* verify that VSfind does not mess up the AIDs of attached Vdatas */
    status = VSfind(fid, "foo");
    CHECK(status,FAIL,"VSfind:fid");

    if (VSseek(vs1, 0) == FAIL)
      {
          num_errs++;
          printf(">>> VSseek failed after VSfind call\n");
      }

    status = VSdetach(vs1);
    CHECK(status,FAIL,"VSdetach:vs1");

    status = Vend(fid);
    CHECK(status,FAIL,"Vend:fid");

    status = Hclose(fid);
    CHECK(status,FAIL,"Hclose:fid");

    return SUCCEED;
}   /* read_vset_stuff */

/* Testing VSdelete for vdatas.*/
static void
test_vsdelete(void)
{
#define FIELD_NAME     "Field Entries"
#define NUMBER_OF_ROWS 10
#define ORDER           3
    int32  file_id;
    int32  vdata_id;
    int32  status;
    int32  num_of_elements;
    int16  vdata_buf[NUMBER_OF_ROWS * ORDER];
    int32  v_ref;
    intn   i;

    /* Open the HDF file. */
    file_id = Hopen(FNAME0, DFACC_RDWR, 0);
    CHECK(file_id,FAIL,"Hopen:tvset.hdf");

    /* Initialize HDF for subsequent vgroup/vdata access. */
    status = Vstart(file_id);
    CHECK(status,FAIL,"Vstart:file_id");

    /* Create a new vdata. */
    vdata_id = VSattach(file_id, -1, "w");
    CHECK(vdata_id,FAIL,"VSattach:vdata_id");
          
    /* Define the field data name, type and order. */
    status = VSfdefine(vdata_id, FIELD_NAME, DFNT_INT16, ORDER);
    CHECK(status,FAIL,"VSfdefine:vdata_id");

    /* Specify the field(s) that will be written to. */
    status = VSsetfields(vdata_id, FIELD_NAME);
    CHECK(status,FAIL,"VSsetfields:vdata_id");

    /* Generate the Vset data. */
    for (i = 0; i < NUMBER_OF_ROWS * ORDER; i+=ORDER) 
      {
          vdata_buf[i] = i;
          vdata_buf[i + 1] = i + 1;
          vdata_buf[i + 2] = i + 2;
      }

    /* Write the data to the Vset. */
    num_of_elements = VSwrite(vdata_id, (const uint8 *)vdata_buf, 
                              NUMBER_OF_ROWS, FULL_INTERLACE);
    CHECK(num_of_elements,FAIL,"VSwrite:");

    /* Set the name and class. */
    status = VSsetname(vdata_id, "Vdata should have been deleted");
    CHECK(status,FAIL,"VSsetname:vdata_id");

    status = VSsetclass(vdata_id, "Vdata should have been deleted");
    CHECK(status,FAIL,"VSsetclass:vdata_id");

    /* get ref of Vdata */
    v_ref = VSQueryref(vdata_id);
    CHECK(v_ref,FAIL,"VSQueryref:vdata_id");

    /* Terminate access to the vdata. */
    status = VSdetach(vdata_id);
    CHECK(status,FAIL,"VSdetach:vdata_id");

    /* Terminate access to the Vxxx interface and close the file. */
    status = Vend(file_id);
    CHECK(status,FAIL,"Vend:file_id");

    status = Hclose(file_id);
    CHECK(status,FAIL,"Hclose:file_id");

    /* Now open the file again and delete the vdata */
    /* Open the HDF file. */
    file_id = Hopen(FNAME0, DFACC_RDWR, 0);
    CHECK(file_id,FAIL,"Hopen:tvset.hdf");

    /* Initialize HDF for subsequent vgroup/vdata access. */
    status = Vstart(file_id);
    CHECK(status,FAIL,"Vstart:file_id");

    /* attach to Vdata */
    vdata_id = VSattach(file_id, v_ref, "w");
    CHECK(vdata_id,FAIL,"VSattach:vdata_id");

    /* delete this Vdata */
    status = VSdelete (file_id, v_ref);
    CHECK(status,FAIL,"VSdelete:vdata_id");

    /* Terminate access to the vdata. */
    status = VSdetach(vdata_id);
    CHECK(status,FAIL,"VSdetach:vdata_id");

    /* Terminate access to the Vxxx interface and close the file. */
    status = Vend(file_id);
    CHECK(status,FAIL,"Vend:file_id");

    status = Hclose(file_id);
    CHECK(status,FAIL,"Hclose:file_id");

    /* Now open file again and try to attach to vdata with 'v_ref'.
       The VSattach should fail. */
    /* Open the HDF file. */
    file_id = Hopen(FNAME0, DFACC_RDONLY, 0);
    CHECK(file_id,FAIL,"Hopen:tvset.hdf");

    /* Initialize HDF for subsequent the vgroup/vdata access. */
    status = Vstart(file_id);
    CHECK(status,FAIL,"Vstart:file_id");

    /* Try to attach to Vdata. This should fail now */
    vdata_id = VSattach(file_id, v_ref, "w");
    if (vdata_id != FAIL)
      {
          num_errs++;
          printf(">>> VSdelete failed to delete vdata \n");
      }

    /* Terminate access to the Vxxx interface and close the file. */
    status = Vend(file_id);
    CHECK(status,FAIL,"Vend:file_id");

    status = Hclose(file_id);
    CHECK(status,FAIL,"Hclose:file_id");

} /* test_vsdelete */

/* Testing Vdelete for vgroups. */ 
static void
test_vdelete(void)
{
    int32 file_id;
    int32 vgroup_id;
    int32 status;
    int32 vg_ref;

    /* Open the HDF file. */
    file_id = Hopen(FNAME0, DFACC_RDWR, 0);
    CHECK(file_id,FAIL,"Hopen:tvset.hdf");

    /* Initialize HDF for subsequent vgroup/vdata access. */
    status = Vstart(file_id);
    CHECK(status,FAIL,"Vstart:file_id");

    /* Create a new vgroup. */
    vgroup_id = Vattach(file_id, -1, "w");
    CHECK(vgroup_id,FAIL,"Vattach:vgroup_id");
          
    /* Set the name and class. */
    status = Vsetname(vgroup_id, "Vgroup should have been deleted");
    CHECK(status,FAIL,"Vsetname:vgroup_id");

    status = Vsetclass(vgroup_id, "Vgroup should have been deleted");
    CHECK(status,FAIL,"Vsetclass:vgroup_id");

    /* get ref of vgroup */
    vg_ref = VQueryref(vgroup_id);
    CHECK(vg_ref,FAIL,"VQueryref:vgroup_id");

    /* Terminate access to the vgroup. */
    status = Vdetach(vgroup_id);
    CHECK(status,FAIL,"Vdetach:vgroup_id");
    
    /* Terminate access to the Vxxx interface and close the file. */
    status = Vend(file_id);
    CHECK(status,FAIL,"Vend:file_id");

    status = Hclose(file_id);
    CHECK(status,FAIL,"Hclose:file_id");

    /* Now open the file again and delete the vgroup */
    /* Open the HDF file. */
    file_id = Hopen(FNAME0, DFACC_RDWR, 0);
    CHECK(file_id,FAIL,"Hopen:tvset.hdf");

    /* Initialize HDF for subsequent vgroup/vdata access. */
    status = Vstart(file_id);
    CHECK(status,FAIL,"Vstart:file_id");

    /* attach to vgroup */
    vgroup_id = Vattach(file_id, vg_ref, "w");
    CHECK(vgroup_id,FAIL,"Vattach:vgroup_id");

    /* delete this vgroup */
    status = Vdelete (file_id, vg_ref);
    CHECK(status,FAIL,"Vdelete:vgroup_id");

    /* Terminate access to the vgroup. */
    status = Vdetach(vgroup_id);
    CHECK(status,FAIL,"VSdetach:vgroup_id");

    /* Terminate access to the Vxxx interface and close the file. */
    status = Vend(file_id);
    CHECK(status,FAIL,"Vend:file_id");

    status = Hclose(file_id);
    CHECK(status,FAIL,"Hclose:file_id");

    /* Now open file again and try to attach to vgroup with 'vg_ref'.
       The Vattach should fail. */
    /* Open the HDF file. */
    file_id = Hopen(FNAME0, DFACC_RDONLY, 0);
    CHECK(file_id,FAIL,"Hopen:tvset.hdf");

    /* Initialize HDF for subsequent the vgroup/vdata access. */
    status = Vstart(file_id);
    CHECK(status,FAIL,"Vstart:file_id");

    /* Try to attach to vgroup. This should fail now */
    vgroup_id = Vattach(file_id, vg_ref, "w");
    if (vgroup_id != FAIL)
      {
          num_errs++;
          printf(">>> Vdelete failed to delete vdata \n");
      }

    /* Terminate access to the Vxxx interface and close the file. */
    status = Vend(file_id);
    CHECK(status,FAIL,"Vend:file_id");

    status = Hclose(file_id);
    CHECK(status,FAIL,"Hclose:file_id");

} /* test_vdelete */

/* Testing Vdeletetagref() for vgroups. */ 
static void
test_vdeletetagref(void)
{
    int32 file_id;
    int32 vgroup_id;
    int32 status;
    int32 vg_ref;

    /* Open the HDF file. */
    file_id = Hopen(FNAME0, DFACC_RDWR, 0);
    CHECK(file_id,FAIL,"Hopen:tvset.hdf");

    /* Initialize HDF for subsequent vgroup/vdata access. */
    status = Vstart(file_id);
    CHECK(status,FAIL,"Vstart:file_id");

    /* Create a new vgroup. */
    vgroup_id = Vattach(file_id, -1, "w");
    CHECK(vgroup_id,FAIL,"Vattach:vgroup_id");
          
    /* Set the name and class. */
    status = Vsetname(vgroup_id, "Vgroup to delete elements from");
    CHECK(status,FAIL,"Vsetname:vgroup_id");

    status = Vsetclass(vgroup_id, "Vgroup to delete elements from");
    CHECK(status,FAIL,"Vsetclass:vgroup_id");

    /* add a few tag/ref pairs to Vgroup */
    status = Vaddtagref(vgroup_id, 1000, 12345);
    CHECK(status,FAIL,"Vaddtagref");
    status = Vaddtagref(vgroup_id, 1000, 12346);
    CHECK(status,FAIL,"Vaddtagref");

#ifndef NO_DUPLICATES
    /* duplicate tag/ref pairs allowed. 
       So add a duplicate */
    status = Vaddtagref(vgroup_id, 1000, 12346);
    CHECK(status,FAIL,"Vaddtagref");

#endif /* NO_DUPLICATES */

    status = Vaddtagref(vgroup_id, 2000, 12345);
    CHECK(status,FAIL,"Vaddtagref");
    status = Vaddtagref(vgroup_id, 2000, 12346);
    CHECK(status,FAIL,"Vaddtagref");

    status = Vaddtagref(vgroup_id, 3000, 12345);
    CHECK(status,FAIL,"Vaddtagref");
    status = Vaddtagref(vgroup_id, 3000, 12346);
    CHECK(status,FAIL,"Vaddtagref");

    /* get ref of vgroup */
    vg_ref = VQueryref(vgroup_id);
    CHECK(vg_ref,FAIL,"VQueryref:vgroup_id");

    /* delete one item in vgroup during this round */
    status = Vdeletetagref(vgroup_id, 1000, 12346);
    CHECK(status,FAIL,"Vdeletetagref:vgroup_id");

    /* Terminate access to the vgroup. */
    status = Vdetach(vgroup_id);
    CHECK(status,FAIL,"Vdetach:vgroup_id");
    
    /* Terminate access to the Vxxx interface and close the file. */
    status = Vend(file_id);
    CHECK(status,FAIL,"Vend:file_id");

    status = Hclose(file_id);
    CHECK(status,FAIL,"Hclose:file_id");

    /* Now open the file again and delete two elements in the vgroup
       during this round. */

    /* Open the HDF file. */
    file_id = Hopen(FNAME0, DFACC_RDWR, 0);
    CHECK(file_id,FAIL,"Hopen:tvset.hdf");

    /* Initialize HDF for subsequent vgroup/vdata access. */
    status = Vstart(file_id);
    CHECK(status,FAIL,"Vstart:file_id");

    /* attach to vgroup */
    vgroup_id = Vattach(file_id, vg_ref, "w");
    CHECK(vgroup_id,FAIL,"Vattach:vgroup_id");

#ifndef NO_DUPLICATES
    /* inquire about number of elments in Vgroup.
       There should only be 6 of them including one duplicate. */
    if (6 != Vntagrefs(vgroup_id))
      {
          num_errs++;
          printf(">>> Vntagrefs returned %d was expecting %d\n", 
                 (int)Vntagrefs(vgroup_id),6);
      }

    /* delete a duplicate in this vgroup */
    status = Vdeletetagref(vgroup_id, 1000, 12346);
    CHECK(status,FAIL,"Vdeletetagref:vgroup_id");
#else /* NO_DUPLICATES */
    /* inquire about number of elments in Vgroup.
       There should only be 5 of them since no duplicates . */
    if (5 != Vntagrefs(vgroup_id))
      {
          num_errs++;
          printf(">>> Vntagrefs returned %d was expecting %d\n", 
                 (int)Vntagrefs(vgroup_id),5);
      }
#endif /* NO_DUPLICATES */

    /* delete some tag/refs in this vgroup */
    status = Vdeletetagref(vgroup_id, 2000, 12346);
    CHECK(status,FAIL,"Vdeletetagref:vgroup_id");

    /* this should be the last element in the vgroup if I have
       the order right */
    status = Vdeletetagref(vgroup_id, 3000, 12346);
    CHECK(status,FAIL,"Vdeletetagref:vgroup_id");

    /* Terminate access to the vgroup. */
    status = Vdetach(vgroup_id);
    CHECK(status,FAIL,"VSdetach:vgroup_id");

    /* Terminate access to the Vxxx interface and close the file. */
    status = Vend(file_id);
    CHECK(status,FAIL,"Vend:file_id");

    status = Hclose(file_id);
    CHECK(status,FAIL,"Hclose:file_id");

    /* Now open file again and try to attach to vgroup with 'vg_ref'.
       There should only be 3 elements left in Vgroup left . */

    /* Open the HDF file. */
    file_id = Hopen(FNAME0, DFACC_RDONLY, 0);
    CHECK(file_id,FAIL,"Hopen:tvset.hdf");

    /* Initialize HDF for subsequent the vgroup/vdata access. */
    status = Vstart(file_id);
    CHECK(status,FAIL,"Vstart:file_id");

    /* attach to vgroup, read only */
    vgroup_id = Vattach(file_id, vg_ref, "r");
    CHECK(vgroup_id,FAIL,"Vattach:vgroup_id");

    /* inquire about number of elments left in Vgroup.
       There should only be 3 of them now. */
    if (3 != Vntagrefs(vgroup_id))
      {
          num_errs++;
          printf(">>> Vntagrefs returned %d was expecting %d\n", 
                 (int)Vntagrefs(vgroup_id), 3);
      }

    /* check tag/ref pair of those 3 elements */
    if (Vinqtagref(vgroup_id, 1000, 12345) == FALSE)
      {
          num_errs++;
          printf(">>> Vinqtagref couldn't find valid element\n");
      }
    if (Vinqtagref(vgroup_id, 2000, 12345) == FALSE)
      {
          num_errs++;
          printf(">>> Vinqtagref couldn't find valid element\n");
      }
    if (Vinqtagref(vgroup_id, 3000, 12345) == FALSE)
      {
          num_errs++;
          printf(">>> Vinqtagref couldn't find valid element\n");
      }

    /* Terminate access to the Vxxx interface and close the file. */
    status = Vend(file_id);
    CHECK(status,FAIL,"Vend:file_id");

    status = Hclose(file_id);
    CHECK(status,FAIL,"Hclose:file_id");

} /* test_vdeletetagref */

static void
test_emptyvdata(void)
{
    int32 status;       /* Status values from routines */
    int32 fid;          /* File ID */
    int32 vs1;          /* Vdata ID */
    int32 ref;          /* Vdata ref */
#ifndef macintosh
    char  name[VSNAMELENMAX], fields[FIELDNAMELENMAX*VSFIELDMAX];
#else
	// fields is too big - Mac has a 32K local data limit.
    char  name[VSNAMELENMAX], *fields;
    
    fields = malloc(FIELDNAMELENMAX*VSFIELDMAX*sizeof(char));
    if (fields == NULL)		return;
#endif

    /* Open the HDF file. */
    fid = Hopen(EMPTYNM, DFACC_RDWR, 0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize HDF for subsequent vgroup/vdata access. */
    status = Vstart(fid);
    CHECK(status,FAIL,"Vstart");

    /* Create a new vdata. */
    vs1 = VSattach(fid, -1, "w");
    CHECK(vs1,FAIL,"VSattach");

    status=VSsetname(vs1, EMPTY_VDATA);
    CHECK(status,FAIL,"VSsetname");

    status = VSdetach(vs1);
    CHECK(status,FAIL,"Vdetach");
    
    status = Vend(fid);
    CHECK(status,FAIL,"Vend");

    status = Hclose(fid);
    CHECK(status,FAIL,"Hclose");

    MESSAGE(5, printf("created empty VDATA %s\n", EMPTY_VDATA););

    /* Re-open the HDF file. */
    fid = Hopen(EMPTYNM, DFACC_RDWR, 0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize HDF for subsequent vgroup/vdata access. */
    status = Vstart(fid);
    CHECK(status,FAIL,"Vstart");

    /* Find the empty vdata. */
    ref=VSfind(fid,EMPTY_VDATA);
    CHECK(ref,FAIL,"VSfind");

    vs1 = VSattach(fid, ref, "r");
    CHECK(vs1,FAIL,"VSattach");

    status=VSgetname(vs1, name);
    CHECK(status,FAIL,"VSgetname");

    if (HDstrcmp(name, EMPTY_VDATA))
      {
          num_errs++;
          printf(">>> Got bogus Vdata name : %s\n", name);
      }

    status=VFnfields(vs1);
    VERIFY(status,0,"VFnfields");

    /* Verify that VSgetfields will return FAIL when passing in a NULL
       for field name list (from bug #554), although this might never 
       happen - BMR 5/17/01 */
    status = VSgetfields(vs1, NULL);
    VERIFY(status, FAIL, "VSgetfields");

    status=VSgetfields(vs1,fields);
    CHECK(status,FAIL,"VSgetfields");

    if (HDstrcmp(fields, ""))
      {
          num_errs++;
          printf(">>> Got bogus field names : %s\n", fields);
      }

    status = VSdetach(vs1);
    CHECK(status,FAIL,"Vdetach");
    
    status = Vend(fid);
    CHECK(status,FAIL,"Vend");

    status = Hclose(fid);
    CHECK(status,FAIL,"Hclose");

    MESSAGE(5, printf("read back in empty VDATA %s\n", EMPTY_VDATA););

    /* Re-open the HDF file. */
    fid = Hopen(EMPTYNM, DFACC_RDWR, 0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize HDF for subsequent vgroup/vdata access. */
    status = Vstart(fid);
    CHECK(status,FAIL,"Vstart");

    /* Find the empty vdata. */
    ref=VSfind(fid,EMPTY_VDATA);
    CHECK(ref,FAIL,"VSfind");

    vs1 = VSattach(fid, ref, "w");
    CHECK(vs1,FAIL,"VSattach");

    /* Write out simple vdata fields */
    status=VSfdefine(vs1, FIELD1, DFNT_FLOAT32, 1);
    CHECK(status,FAIL,"VSfdefine");

    status = VSfdefine(vs1, FIELD2, DFNT_INT32, 2);
    CHECK(status,FAIL,"VSfdefine");

    status = VSsetfields(vs1, FIELD1","FIELD2);
    if (status == FAIL)
      {
          num_errs++;
          printf(">>> Vsetfields failed for %s\n", name);
      }

    status = VSdetach(vs1);
    CHECK(status,FAIL,"Vdetach");
    
    status = Vend(fid);
    CHECK(status,FAIL,"Vend");

    status = Hclose(fid);
    CHECK(status,FAIL,"Hclose");

    MESSAGE(5, printf("changed empty VDATA %s to have two fields\n", EMPTY_VDATA););

    /* Re-open the HDF file. */
    fid = Hopen(EMPTYNM, DFACC_RDWR, 0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize HDF for subsequent vgroup/vdata access. */
    status = Vstart(fid);
    CHECK(status,FAIL,"Vstart");

    /* Find the empty vdata. */
    ref=VSfind(fid,EMPTY_VDATA);
    CHECK(ref,FAIL,"VSfind");

    vs1 = VSattach(fid, ref, "r");
    CHECK(vs1,FAIL,"VSattach");

    status=VFnfields(vs1);
    VERIFY(status,2,"VFnfields");

    status=VSgetfields(vs1,fields);
    CHECK(status,FAIL,"VSgetfields");

    if (HDstrcmp(fields, FIELD1","FIELD2))
      {
          num_errs++;
          printf(">>> Got bogus field names : %s\n", fields);
      }

    status = VSdetach(vs1);
    CHECK(status,FAIL,"Vdetach");
    
    status = Vend(fid);
    CHECK(status,FAIL,"Vend");

    status = Hclose(fid);
    CHECK(status,FAIL,"Hclose");

#ifdef macintosh
	free(fields);
#endif

} /* test_emptyvdata() */

/*************************** test_blockinfo ***************************

This test routine creates an hdf file, "Block_info.hdf", and creates
and writes two vdatas in a way that one of the vdatas will be promoted
to a linked-block element.

The first vdata is named "Appendable Vdata", contains 10 records, and 
belongs to a class, named "Linked-block Vdata".  The fields of the vdata 
include "Field1", "Field2", and "Field3" and all data are integer.  
"Field1" has an order of 3, "Field2" has an order of 1, and "Field3" has 
an order of 2.

The second vdata named "Another Vdata", contains 5 records, and also
belongs to class "Linked-block Vdata".  This vdata has only one field
of order 1 and its data are integer.

The test writes 5 records to the first vdata, "Appendable Vdata", then
creates and writes the second vdata, "Another Vdata", then, writes 
another 5 records to the first vdata.  The purpose of the second vdata 
is to cause the subsequent write to the first vdata, "Appendable Vdata", 
to promote the vdata to a linked-block element.

***********************************************************************/

#define FILE_NAME	"Block_info.hdf"
#define APPENDABLE_VD	"Appendable Vdata"
#define ANOTHER_VD	"Another Vdata"
#define CLASS_NAME	"Linked-block Vdata"
#define FIELD1_NAME	"Field1"	/* contains three integers */
#define FIELD2_NAME	"Field2"	/* contains one integer */
#define FIELD3_NAME	"Field3"	/* contains two integers */
#define	FIELD_NAME_LIST	"Field1,Field2,Field3"
#define ANOTHER_FD	"Another field"	/* contains one integer */
#define	ANOTHER_FD_LIST	"Another field"
#define N_RECORDS	5	/* number of records to be written to the
				   vdatas at every write */
#define ORDER_1 	3	/* order of first field of 1st vdata */
#define ORDER_2 	1	/* order of second field of 1st vdata */
#define ORDER_3 	2	/* order of third field of 1st vdata */
#define N_VALS_PER_REC_2   1    /* # of values per record in the 2nd vdata */
#define N_VALS_PER_REC_1 (ORDER_1+ORDER_2+ORDER_3) /* # of vals/rec. in 1st vd*/
#define	BLOCK_SIZE	128	/* arbitrary number for block size */
#define	NUM_BLOCKS	8	/* arbitrary number for number of blocks */

static void
test_blockinfo(void) 
{
   intn	 status_n;	/* returned status for functions returning an intn  */
   int32 status_32;	/* returned status for functions returning an int32 */
   int16 rec_num;	/* current record number */
   int32 file_id, vdata1_id, vdata2_id,
	 vdata_ref = -1,  /* ref number of a vdata, set to -1 to create  */
   	 num_of_records,  /* number of records actually written to vdata */
         data_buf1[N_RECORDS][N_VALS_PER_REC_1], /* for first vdata's data */
	 data_buf2[N_RECORDS][N_VALS_PER_REC_2], /* for second vdata's data */
	 block_size, num_blocks; /* retrieved by VSgetblockinfo */

    /* Create the HDF file for data used in this test routine */
    file_id = Hopen (FILE_NAME, DFACC_CREATE, 0);
    CHECK(file_id, FAIL, "Hopen");

    /* Initialize the VS interface */
    status_n = Vstart (file_id);
    CHECK(status_n, FAIL, "Vstart");

    /* Create the first vdata */
    vdata1_id = VSattach (file_id, vdata_ref, "w");
    CHECK(vdata1_id, FAIL, "VSattach");

    /* Set name and class name of the vdata. */
    status_32 = VSsetname (vdata1_id, APPENDABLE_VD);
    CHECK(status_32, FAIL, "VSsetname");
    status_32 = VSsetclass (vdata1_id, CLASS_NAME);
    CHECK(status_32, FAIL, "VSsetclass");

    /* Introduce each field's name, data type, and order.  This is the first
      part in defining a field.  */
    status_n = VSfdefine (vdata1_id, FIELD1_NAME, DFNT_INT32, ORDER_1);
    CHECK(status_n, FAIL, "VSfdefine");
    status_n = VSfdefine (vdata1_id, FIELD2_NAME, DFNT_INT32, ORDER_2);
    CHECK(status_n, FAIL, "VSfdefine");
    status_n = VSfdefine (vdata1_id, FIELD3_NAME, DFNT_INT32, ORDER_3);
    CHECK(status_n, FAIL, "VSfdefine");

    /* Finalize the definition of the fields. */
    status_n = VSsetfields (vdata1_id, FIELD_NAME_LIST);
    CHECK(status_n, FAIL, "VSsetfields");

    /* 
     * Buffer the data by the record for fully interlaced mode.  Note that the
     * first three elements contain the three values of the first field, the
     * fourth element contains the value of the second field, and the last two
     * elements contain the two values of the third field.
     */
    for (rec_num = 0; rec_num < N_RECORDS; rec_num++)
    {
        data_buf1[rec_num][0] = 1 + rec_num;
        data_buf1[rec_num][1] = 2 + rec_num;
        data_buf1[rec_num][2] = 3 + rec_num;
        data_buf1[rec_num][3] = 10 + rec_num;
        data_buf1[rec_num][4] = 10;
        data_buf1[rec_num][5] = 65;
    }

    /* Test for invalid arguments passed in these functions */
    status_n = VSsetblocksize(vdata1_id, -2);
    VERIFY(status_n, FAIL, "VSsetblocksize");
    status_n = VSsetnumblocks(vdata1_id, 0);
    VERIFY(status_n, FAIL, "VSsetnumblocks");

    /* Set the block size and the number of blocks the first vdata */
    status_n = VSsetblocksize(vdata1_id, BLOCK_SIZE);
    CHECK(status_n, FAIL, "VSsetblocksize");
    status_n = VSsetnumblocks(vdata1_id, NUM_BLOCKS);
    CHECK(status_n, FAIL, "VSsetnumblocks");

    /* Write the data from data_buf1 to the vdata with full interlacing mode. */
    num_of_records = VSwrite(vdata1_id, (uint8 *)data_buf1, N_RECORDS, 
				FULL_INTERLACE);
    VERIFY(num_of_records, N_RECORDS, "VSwrite:vdata1_id");

    /******************************************************************
     * Creates and writes another vdata right after APPENDABLE_VDATA.
     * This will cause the storage of APPENDABLE_VDATA to be promoted to a
     * linked-block element if a subsequent write to APPENDABLE_VDATA occurs. 
     ******************************************************************/
 
    /* Create another vdata. */
    vdata2_id = VSattach (file_id, vdata_ref, "w");
    CHECK(vdata2_id, FAIL, "VSattach");

    /* Set name and class name of the vdata. */
    status_32 = VSsetname (vdata2_id, ANOTHER_VD);
    CHECK(status_32, FAIL, "VSsetname");
    status_32 = VSsetclass (vdata2_id, CLASS_NAME);
    CHECK(status_32, FAIL, "VSsetclass");

    /* Define the vdata's field. */
    status_n = VSfdefine (vdata2_id, ANOTHER_FD, DFNT_INT32, ORDER_2);
    CHECK(status_n, FAIL, "VSfdefine");
    status_n = VSsetfields (vdata2_id, ANOTHER_FD_LIST);
    CHECK(status_n, FAIL, "VSsetfields");

    /* Buffer the data for ANOTHER_VDATA */
    for (rec_num = 0; rec_num < N_RECORDS; rec_num++)
    {
        data_buf2[rec_num][0] = 100 + rec_num;
    }

    /* Write the data from data_buf2 to the second vdata with full 
       interlacing mode. */
    num_of_records = VSwrite(vdata2_id, (uint8 *)data_buf2, N_RECORDS, 
				FULL_INTERLACE);
    VERIFY(num_of_records, N_RECORDS, "VSwrite:vdata2_id");

    /******************************************************************
     * Writes more data to APPENDABLE_VDATA, i.e. first vdata.  Its
     * storage will be promoted to a linked-block element.
     ******************************************************************/

    for (rec_num = 0; rec_num < N_RECORDS; rec_num++)
    {
        data_buf1[rec_num][0] = 10 + rec_num;
        data_buf1[rec_num][1] = 20 + rec_num;
        data_buf1[rec_num][2] = 30 + rec_num;
        data_buf1[rec_num][3] = 100 + rec_num;
        data_buf1[rec_num][4] = 100;
        data_buf1[rec_num][5] = 650;
    }

    /* Write the data from data_buf1 to the vdata with full interlacing mode. */
    num_of_records = VSwrite(vdata1_id, (uint8 *)data_buf1, N_RECORDS, 
				FULL_INTERLACE); 
    VERIFY(num_of_records, N_RECORDS, "VSwrite:vdata1_id");

    /* Retrieve the first vdata's block size and number of blocks and 
       verify them */
    status_n = VSgetblockinfo (vdata1_id, &block_size, &num_blocks);
    CHECK(status_n, FAIL, "VSsetfields");
    VERIFY(block_size, BLOCK_SIZE, "VSgetblockinfo");
    VERIFY(num_blocks, NUM_BLOCKS, "VSgetblockinfo");

    /* Terminate access to the vdatas and to the VS interface, then 
       close the HDF file. */
    status_32 = VSdetach (vdata1_id);
    CHECK(status_32, FAIL, "Vdetach");

    status_32 = VSdetach (vdata2_id);
    CHECK(status_32, FAIL, "Vdetach");

    status_n = Vend (file_id);
    CHECK(status_n, FAIL, "Vend");

    status_32 = Hclose (file_id);
    CHECK(status_32, FAIL, "Hclose");
} /* test_blockinfo() */

/* main test driver */
void
test_vsets(void)
{
    int32       status;

    status = write_vset_stuff();
    if (status == FAIL)
        return;

    status = read_vset_stuff();
    if (status == FAIL)
        return;

    /* test VSdelete() */
    test_vsdelete();

    /* test Vdelete() */
    test_vdelete();
   
    /* test Vdeletetagref() */
    test_vdeletetagref();

    /* test Vdatas with no fields defined */
    test_emptyvdata();

    /* test functionality about set/get linked-block information */
    test_blockinfo();

}   /* test_vsets */

