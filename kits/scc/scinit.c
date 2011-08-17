/* SCHEME->C */

/*              Copyright 1989 Digital Equipment Corporation
 *                         All Rights Reserved
 *
 * Permission to use, copy, and modify this software and its documentation is
 * hereby granted only under the following terms and conditions.  Both the
 * above copyright notice and this permission notice must appear in all copies
 * of the software, derivative works or modified versions, and any portions
 * thereof, and both notices must appear in supporting documentation.
 *
 * Users of this software agree to the terms and conditions set forth herein,
 * and hereby grant back to Digital a non-exclusive, unrestricted, royalty-free
 * right and license under any changes, enhancements or extensions made to the
 * core functions of the software, including but not limited to those affording
 * compatibility with other hardware or software environments, but excluding
 * applications which incorporate this software.  Users further agree to use
 * their best efforts to return to Digital any such changes, enhancements or
 * extensions that they make and inform Digital of noteworthy uses of this
 * software.  Correspondence should be provided to Digital at:
 * 
 *                       Director of Licensing
 *                       Western Research Laboratory
 *                       Digital Equipment Corporation
 *                       250 University Avenue
 *                       Palo Alto, California  94301  
 * 
 * This software may be distributed (but not offered for sale or transferred
 * for compensation) to third parties, provided such third parties agree to
 * abide by the terms and conditions of this notice.  
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
 * WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
 * CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
*/

/* This module defines some basic global objects and initializes those parts
   of the SCHEME->C runtime system which are written in C.  For
   compatibility with other modules, the routines and Scheme globals provided
   by these routines appear as members of the module "sc".
*/

/* External Definitions */

extern  char *sbrk();
extern  char *getenv();

extern  errno;			/* C-library Error flag */

#include <sys/file.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <strings.h>
#include <varargs.h>

/* Definitions for objects within sc */

#include "objects.h"
#include "scinit.h"
#include "heap.h"
#include "apply.h"
#include "callcc.h"
#include "signal.h"

/* Definitions for objects elsewhere in the Scheme system */

extern  TSCP  scrt1_reverse();
extern  TSCP  scrt6_error();

extern  etext;
#ifdef MIPS
#define ETEXT	((int)&etext)	/* First address after text */
#ifdef BIGMIPS
#include <sys/param.h>
#include <sys/immu.h> */
#define STACKBASE (int*)USERSTACK
#else
#include <mips/param.h>
#include <mips/vmparam.h>
#define STACKBASE (int*)USRSTACK
#endif
#endif
#ifdef TITAN
#define ETEXT	etext		/* First address after text */
#include <sys/mparam.h>
#define STACKBASE (int*)(MAXUSERADDR+1)
#endif
#ifdef VAX
#define ETEXT	((int)&etext)	/* First address after text */
#include <vax/param.h>
#include <vax/vmparam.h>
#define STACKBASE (int*)USRSTACK
#endif

/* Global data structure for this module. */

static int  emptyvector = VECTORTAG,
            emptystring[2] = {STRINGTAG, 0};

FILE   *sc_stdin,	/* Standard I/O Subroutine FILE pointers */
       *sc_stdout,
       *sc_stderr;

TSCP  sc__2dfile_2a_67475874_v;  	/*  *INITIAL-HEAP-FILE*  */

static int  expandfailed = 0;		/* Expansion failure flag */

static int  module_initialized = 0;

/* Command line arguments and environment variables which control the heap are
   interpreted by the following functions.
*/

static  char *heapfilename = NULL;	/* Pointer to heap file name */

static  int  defaultheap = 8,		/* Default heap size in megabytes */
	     minheap = 1,		/* Minimum heap size in megabytes */
	     maxheap = 1000,		/* Maximum heap size in megabytes */
	     defaultlimit = 33,		/* Default collection limit */
	     minlimit = 10,		/* Minimum total collection limit */
	     maxlimit = 45,		/* Maximun total collection limit */
	     scheap,			/* Heap size in megabytes */
	     scmaxheap,			/* Heap allowed to grow this big */
	     sclimit;			/* % at which to do total collection */

static char*  getargval( argc, argv, cl, env )
	int  argc;
	char  *argv[],
	      *cl,	/* Ptr to command line argument name */
	      *env;	/* Ptr to environment variable name */
{
	int  i;

	for  (i = 1; i < argc-1; i++)  {
	   if  (strcmp( argv[ i ], cl ) == 0)  return( argv[ i+1 ] );
	}
	return( getenv( env ) );
}
	
static void  decodearguments( argc, argv )
	int  argc;
	char  *argv[];
{
	char  *val;
	
	val = getargval( argc, argv, "-sch", "SCHEAP" );
	if  (val != NULL)  {
	   scheap = atoi( val );
	   if  (scheap < minheap)  scheap = minheap;
	   if  (scheap > maxheap)  scheap = maxheap;
	}
	else  scheap = defaultheap;
	val = getargval( argc, argv, "-scmh", "SCMAXHEAP" );
	if  (val != NULL)  {
	   scmaxheap = atoi( val );
	   if  (scmaxheap < scheap)  scmaxheap = scheap;
	   if  (scmaxheap > maxheap)  scmaxheap = maxheap;
	}
	else  scmaxheap = scheap*2;
	heapfilename = getargval( argc, argv, "-schf", "SCHEAPFILE" );
	val = getargval( argc, argv, "-scgc", "SCGCINFO" );
	if  (val != NULL)  {
	   sc_gcinfo = atoi( val );
	   if  (sc_gcinfo < 0  ||  sc_gcinfo > 2)  sc_gcinfo = 0;
	}
	else  sc_gcinfo = 0;
	val = getargval( argc, argv, "-scl", "SCLIMIT" );
	if  (val != NULL)  {
	   sclimit = atoi( val );
	   if  (sclimit < minlimit)  sclimit = defaultlimit;
	   if  (sclimit > maxlimit)  sclimit = defaultlimit;
	}
	else  sclimit = defaultlimit;
}

/* The variables holding the values of the functions defined in this module
   are initialized by the following procedure.
*/

DEFSTRING( t1030, "MY-RUSAGE", 9 );
DEFSTRING( t1032, "COLLECT-RUSAGE", 14 );
DEFSTRING( t1034, "COLLECT", 7 );
DEFSTRING( t1035, "COLLECT-ALL", 11 );
DEFSTRING( t1036, "CONS", 4 );
DEFSTRING( t1037, "WEAK-CONS", 9 );
DEFSTRING( t1038, "MAKE-STRING", 11 );
DEFSTRING( t1040, "STRING-COPY", 11 );
DEFSTRING( t1044, "MAKE-VECTOR", 11 );
DEFSTRING( t1046, "STRING->SYMBOL", 14 );
DEFSTRING( t1048, "STRING->UNINTERNED-SYMBOL", 25 );
DEFSTRING( t1050, "UNINTERNED-SYMBOL?", 18 );
DEFSTRING( t1052, "CALL-WITH-CURRENT-CONTINUATION", 30 );
DEFSTRING( t1056, "SAVE-HEAP", 9 );
DEFSTRING( t1058, "IMPLEMENTATION-INFORMATION", 26 );
DEFSTRING( t1060, "AFTER-COLLECT", 13 );
DEFSTRING( t1062, "*FROZEN-OBJECTS*", 16 );
DEFSTRING( t2003, "*INITIAL-HEAP-FILE*", 19 );

static  init_procs()
{
        INITIALIZEVAR( U_TX( ADR( t1030 ) ), 
                       ADR( sc_my_2drusage_v ), 
                       MAKEPROCEDURE( 0, 
                                      0, sc_my_2drusage, EMPTYLIST ) );
        INITIALIZEVAR( U_TX( ADR( t1032 ) ), 
                       ADR( sc_collect_2drusage_v ), 
                       MAKEPROCEDURE( 0, 
                                      0, 
                                      sc_collect_2drusage, EMPTYLIST ) );
        INITIALIZEVAR( U_TX( ADR( t1034 ) ), 
                       ADR( sc_collect_v ), 
                       MAKEPROCEDURE( 0, 
                                      0, sc_collect, EMPTYLIST ) );
        INITIALIZEVAR( U_TX( ADR( t1035 ) ), 
                       ADR( sc_collect_2dall_v ), 
                       MAKEPROCEDURE( 0, 
                                      0, sc_collect_2dall, EMPTYLIST ) );
        INITIALIZEVAR( U_TX( ADR( t1036 ) ), 
                       ADR( sc_cons_v ), 
                       MAKEPROCEDURE( 2, 0, sc_cons, EMPTYLIST ) );
	INITIALIZEVAR( U_TX( ADR( t1037 ) ),
		       ADR( sc_weak_2dcons_v ),
		       MAKEPROCEDURE( 2, 0, sc_weak_2dcons, EMPTYLIST ) );
        INITIALIZEVAR( U_TX( ADR( t1038 ) ), 
                       ADR( sc_make_2dstring_v ), 
                       MAKEPROCEDURE( 1, 
                                      1, 
                                      sc_make_2dstring, EMPTYLIST ) );
        INITIALIZEVAR( U_TX( ADR( t1040 ) ), 
                       ADR( sc_string_2dcopy_v ), 
                       MAKEPROCEDURE( 1, 
                                      0, 
                                      sc_string_2dcopy, EMPTYLIST ) );
        INITIALIZEVAR( U_TX( ADR( t1044 ) ), 
                       ADR( sc_make_2dvector_v ), 
                       MAKEPROCEDURE( 1, 
                                      1, 
                                      sc_make_2dvector, EMPTYLIST ) );
        INITIALIZEVAR( U_TX( ADR( t1046 ) ), 
                       ADR( sc_string_2d_3esymbol_v ), 
                       MAKEPROCEDURE( 1, 
                                      0, 
                                      sc_string_2d_3esymbol, EMPTYLIST ) );
        INITIALIZEVAR( U_TX( ADR( t1048 ) ), 
                       ADR( sc_d_2dsymbol_ab4b4447_v ), 
                       MAKEPROCEDURE( 1, 
                                      0, 
                                      sc_d_2dsymbol_ab4b4447, 
                                      EMPTYLIST ) );
        INITIALIZEVAR( U_TX( ADR( t1050 ) ), 
                       ADR( sc_uninterned_2dsymbol_3f_v ), 
                       MAKEPROCEDURE( 1, 
                                      0, 
                                      sc_uninterned_2dsymbol_3f, 
                                      EMPTYLIST ) );
        INITIALIZEVAR( U_TX( ADR( t1052 ) ), 
                       ADR( sc_ntinuation_1af38b9f_v ), 
                       MAKEPROCEDURE( 1, 
                                      0, 
                                      sc_ntinuation_1af38b9f, 
                                      EMPTYLIST ) );
        INITIALIZEVAR( U_TX( ADR( t1056 ) ), 
                       ADR( sc_save_2dheap_v ), 
                       MAKEPROCEDURE( 1, 
                                      1, sc_save_2dheap, EMPTYLIST ) );
	INITIALIZEVAR( U_TX( ADR( t1058 ) ),
		       ADR( sc_implementation_v ),
		       MAKEPROCEDURE( 0,
				      0, sc_implementation, EMPTYLIST ) );
	INITIALIZEVAR( U_TX( ADR( t1060 ) ),
		       ADR( sc_after_2dcollect_v ),
		       FALSEVALUE );
	INITIALIZEVAR( U_TX( ADR( t1062 ) ),
		       ADR( sc__2afrozen_2dobjects_2a_v ),
		       EMPTYLIST );
        INITIALIZEVAR( U_TX( ADR( t2003 ) ),
                       ADR( sc__2dfile_2a_67475874_v ),
                       FALSEVALUE );
        MAXDISPLAY( 0 );
        return;
}

/* Memory is allocated from the heap by calling the following function
   with a byte count.  It returns a pointer to the space.  Errors occurring
   during initialization will cause the program to abort.  Later errors will
   return -1 as the procedure's value.  Storage is allocated on PAGEBYTE
   boundaries and counts are rounded up to full pages.
*/

static char  *getmem( bytes )
	int  bytes;
{
	char  *memp;

	memp = sbrk( 0 );
	if  ((int)memp & (PAGEBYTES-1))
	   sbrk( PAGEBYTES-(int)memp & (PAGEBYTES-1) );
	bytes = (bytes+PAGEBYTES-1) & ~(PAGEBYTES-1);
	memp = sbrk( bytes );
	if  ((int)memp == -1)  {
	   memp = NULL;
	   expandfailed = 1;
	   if  (module_initialized == 0)  {
	   fprintf( stderr, "***** Memory allocation failed: sbrk( %d )\n",
	   	    bytes );
	   exit( 1 );
	   }
	}
	if  (sc_gcinfo > 1)
	   fprintf( stderr, "***** Memory  %x %x\n", memp, memp+bytes-1 );
	return( memp );
}

/* Side tables are allocated by calling the following procedure with the
   first and last heap pages, and pointers to the pagegeneration, type,
   lock and link tables.  An allocation failure will cause the pointers to
   be returned as NULL.
*/

static  char*  side_addr;	/* Address and size of last side tables. */
static  int    side_bytes;

static void  allocate_sidetables( first, last, pagegen, type, lock, link )
	int  first, last,	/* heap pages */
	     **pagegen,		/* Pointers to pointers to tables */
	     **type,
	     **link,
	     **lock;
{
	int  bytes;
	char*  addr;

	bytes = ((last-first+1)*sizeof( int )*4+PAGEBYTES-1) & ~(PAGEBYTES-1);
	addr = getmem( bytes );
	if  (addr == NULL)  {
	   *pagegen = *type = *lock = *link = NULL;
	   return;
	}
	side_addr = addr;
	side_bytes = bytes;
	*pagegen = ((int*)side_addr)-first;
	*type = *pagegen+last+1-first;
	*lock = *type+last+1-first;
	*link = *lock+last+1-first;
}

/* The following function is called to initialize the heap from scratch. */

sc_newheap()
{
	int  i;
	char  *freebase;
	TSCP  unknown;

	sc_limit = sclimit;
	sc_heappages = scheap*(ONEMB/PAGEBYTES);
	sc_maxheappages = scmaxheap*(ONEMB/PAGEBYTES);
	sc_allocatedheappages = 0;
	freebase = getmem( scheap*ONEMB );
	sc_firstheappage = ADDRESS_PAGE( freebase );
	sc_lastheappage = sc_firstheappage+sc_heappages-1;
	sc_freepage = sc_firstheappage;
	sc_firstheapp = (int*)freebase;
	sc_lastheapp = sc_firstheapp+PAGEWORDS*sc_heappages-1;
	sc_current_generation = 3;
	sc_next_generation = 3;
	sc_genlist = -1;
	allocate_sidetables( sc_firstheappage, sc_lastheappage,
			     &sc_pagegeneration, &sc_pagetype, &sc_pagelock,
			     &sc_pagelink );
	for  (i = sc_firstheappage; i <= sc_lastheappage; i++ )  {
	   sc_pagegeneration[ i ] = 1;
	   sc_pagelock[ i ] = 0;
	}
	sc_initiallink = OKTOSET;
	sc_conscnt = 0;
	sc_extobjwords = 0;
	sc_mutex = 0;
	sc_pendingsignals = 0;
	sc_emptylist = EMPTYLIST;
	sc_emptyvector = U_T( &emptyvector, EXTENDEDTAG );
	sc_emptystring = U_T( emptystring, EXTENDEDTAG );
	sc_falsevalue = FALSEVALUE;
	sc_truevalue = TRUEVALUE;
	sc_eofobject = EOFOBJECT;
	sc_undefined = UNDEFINED;
	sc_stdin = stdin;
	sc_stdout = stdout;
	sc_stderr = stderr;
	sc_constants = NULL;
	sc_globals = NULL;
	sc_stackbase = STACKBASE;
	sc_whenfreed = EMPTYLIST;
	sc_freed = EMPTYLIST;
	sc_clink = EMPTYLIST;
	sc_globals = addtoSCPTRS( sc_globals, &sc_clink );
	sc_stacktrace = NULL;
	sc_obarray = sc_make_2dvector( 1023*4, EMPTYLIST );
	sc_initializevar( sc_cstringtostring( "*OBARRAY*" ),
			  &sc_obarray,
			  sc_obarray );
	init_procs();
	unknown = sc_makeprocedure( 0, 0, sc_unknowncall, EMPTYLIST );
	TX_U( unknown )->procedure.required = 255;
	for  (i = 0;  i <= 3;  i++)  {
	   sc_unknownproc[ i ] = unknown;
	   sc_globals = addtoSCPTRS( sc_globals, &sc_unknownproc[ i ] );
	}
	module_initialized = 1;
	if  (sc_gcinfo)
	   fprintf( stderr,
	    "***** SCGCINFO = %d  SCHEAP = %d  SCMAXHEAP = %d  SCLIMIT = %d\n",
	   	    sc_gcinfo, scheap, scmaxheap, sclimit );
	/* Initialize dynwind as it has redefined call/cc */
	dynwind__init();
}

/* A block of storage is added to the heap by the following function.  Side
   tables are automatically expanded as required.
*/

static void  addrtoheap( addr, count )
	char  *addr;		/* Address of the block */
	int  count;		/* Size in bytes of the block */
{
	int  first_addr,	/* First page of addr */
	     last_addr,		/* Last page of addr */
	     i,
	     first_side,	/* First page of current side tables */
	     last_side,		/* Last page of current side tables */
	     new_first,		/* New first page of heap */
	     new_last,		/* New last page of heap */
	     *new_pagegeneration,
	     *new_pagetype,
	     *new_pagelock,
	     *new_pagelink;

	if  (addr == NULL)  return;
	first_addr = ADDRESS_PAGE( addr );
	last_addr = ADDRESS_PAGE( addr+count-1 );
	if  (first_addr >= sc_firstheappage  &&
	     last_addr <= sc_lastheappage)  {
	   /* Block fits in the side table */
	   if  (sc_gcinfo > 1)
	      fprintf( stderr, "***** To heap %x %x\n", addr, addr+count-1 );
	   for  (i = first_addr; i <= last_addr; i++)  {
	      if  (sc_pagegeneration[ i ])  {
	         fprintf( "***** COLLECT Trying to reallocate page %d\n", i );
		 abort();
	      }
	      sc_pagegeneration[ i ] = 1;
	      sc_pagelock[ i ] = 0;
	   }
	   sc_heappages = sc_heappages+last_addr-first_addr+1;
	   return;
	}
	/* Didn't fit, so figure out the new span of pages for the existing
	   heap, the current side tables, and the new block.
	*/
	first_side = ADDRESS_PAGE( side_addr );
	last_side = ADDRESS_PAGE( side_addr+side_bytes-1 );
	new_first = sc_firstheappage;
	if  (first_side < new_first)  new_first = first_side;
	if  (first_addr < new_first)  new_first = first_addr;
	new_last = sc_lastheappage;
	if  (last_side > new_last)  new_last = last_side;
	if  (last_addr > new_last)  new_last = last_addr;
	/* Try to allocate the new side tables */
	allocate_sidetables( new_first, new_last, &new_pagegeneration,
			     &new_pagetype, &new_pagelock, &new_pagelink );
	if  (new_first == NULL)  return;

	/* Copy the old side tables */
	for  (i = new_first; i < sc_firstheappage; i++)  {
	   new_pagegeneration[ i ] = 0;
	   new_pagelock[ i ] = 0;
	}
	for  (i = sc_firstheappage; i <= sc_lastheappage;  i++)  {
	   new_pagegeneration[ i ] = sc_pagegeneration[ i ];
	   new_pagetype[ i ] = sc_pagetype[ i ];
	   new_pagelock[ i ] = sc_pagelock[ i ];
	   new_pagelink[ i ] = sc_pagelink[ i ];
	}
	for  (i = sc_lastheappage+1; i <= new_last; i++)  {
	   new_pagegeneration[ i ] = 0;
	   new_pagelock[ i ] = 0;
	}
	/* Flip tables and set new bounds on the heap */
	sc_pagegeneration = new_pagegeneration;
	sc_pagetype = new_pagetype;
	sc_pagelock = new_pagelock;
	sc_pagelink = new_pagelink;
	sc_firstheappage = new_first;
	sc_lastheappage = new_last;
	sc_firstheapp = (int*)PAGE_ADDRESS( new_first );
	sc_lastheapp = ((int*)PAGE_ADDRESS( new_last+1 ))-1;
	/* Add old side tables and storage block to the heap */
	addrtoheap( addr, count );
	addrtoheap( PAGE_ADDRESS( first_side ),
		    (last_side-first_side+1)*PAGEBYTES );
}

/* The heap is expanded by calling the following procedure.  The boolean result
   is true iff the heap was expanded.  The amount added to the heap is the
   minimum of:  the existing heap size, the amount till the maximum, and
   25% of the maximum heap size.
*/

int  sc_expandheap()
{
	int  old_pages = sc_heappages,		/* Existing heap size */
	     add_pages = sc_heappages;		/* # of pages to add */
	char  *msgheader;

	if  ((sc_collecting == 0)  ||  (sc_collecting  && sc_gcinfo == 0))
	   msgheader = "\n***** COLLECT ";
	else
	   msgheader = "              ";
	if  (sc_heappages >= sc_maxheappages  ||  expandfailed != 0)  {
	   if  (expandfailed == 0)  {
	      fprintf( stderr, "%scannot further expand heap\n",
		       msgheader );
	      expandfailed = 1;
	   }
	   return( 0 );
	}
	if  (add_pages > sc_maxheappages-sc_heappages)
	   add_pages = sc_maxheappages-sc_heappages;
	if  (add_pages > (sc_maxheappages*25)/100)
	   add_pages = (sc_maxheappages*25)/100;
	if  (sc_gcinfo)
	   fprintf( stderr, "%sheap expanded to ", msgheader );
	addrtoheap( getmem( add_pages*PAGEBYTES ), add_pages*PAGEBYTES );
	if  (sc_gcinfo)
	   fprintf( stderr, "%d MB\n",
		    (sc_heappages*PAGEBYTES+ONEMB/2)/ONEMB );
	if  (expandfailed != 0)
	   fprintf( stderr, "%sunable to expand the heap\n", msgheader );
	return( sc_heappages != old_pages );
}

/* The routines which follow are responsible for saving the heap to disc
   and reloading it.  Saved heap images have the following header at the
   front of the file.  Following the header is the sc_constants array, the
   sc_globals array, thepagegeneration array, the pagetype array, and all
   valid pages of the heap.
*/

static struct  {
	char  id[4];		/* S->C */
	TSCP  procedure;	/* Restart procedure */
	TSCP  correct;		/* List of values for constants & globals */
	int  etext;
	int  locklist;			/* From heap.h */
	int  lockcnt;
	int  current_generation;
	int  next_generation;
	int  limit;
	int  heappages;
	int  firstheappage;
	int  freepage;
	int  allocatedheappages;
	int  *firstheapp;
	int  conscnt;
	SCP  consp;
	int  extobjwords;
	int  extwaste;
	SCP  extobjp;
	int  *sc_stackbase;
	TSCP  sc_whenfreed;
	int  sc_constants_limit;	/* From objects.h */
	int  sc_globals_limit;
	int  sc_maxdisplay;
       }  save;

/* I/O is done directly with system calls so as to not allocate any data
   from the heap when the heap must be restored.
*/

static  int  heapfile;		/* File descriptor for the heap file */

static void  heapin( address, count )
	char  *address;
	int  count;
{
	if  (read( heapfile, address, count ) != count)  {
	   fprintf( stderr, "***** SAVE-HEAP HEAP FILE read error: %d\n",
	   	    errno );
	   exit( 1 );
	}
}

static void  heapout( address, count )
	char  *address;
	int  count;
{
	int  error;

	if  (write( heapfile, address, count ) != count)  {
	   error = errno;
	   close( heapfile );
	   sc_error( "SAVE-HEAP", "HEAP FILE fwrite error: ~s", 1,
                     C_FIXED( error ) );
	}
}

/* A Scheme program may call (SAVE-HEAP filename . procedure) to save the
   heap in a file named "filename".  When the heap is reloaded into a
   newly created process, execution will start at the procedure "procedure"
   which will be called with the command line argument list.  If procedure is
   not supplied, then the normal start up procedure will be used.
*/

TSCP  sc_save_2dheap_v;

TSCP  sc_save_2dheap( filename, argl )
	TSCP  filename, argl;
{
	int  i, firstpage, pagecount;
	TSCP  correct, cl, symbol, procedure;

	procedure = FALSEVALUE;
	if  (argl != EMPTYLIST)  {
	   procedure = PAIR_CAR( argl );
	   if  (TSCPTAG( procedure ) != EXTENDEDTAG  ||
	      T_U( procedure )->procedure.tag != PROCEDURETAG)
	      sc_error( "SAVE-HEAP",
	      		"Restart procedure is not a PROCEDURE: ~s",
	   	        1, procedure );
	   if  (PROCEDURE_REQUIRED( procedure ) > 1  ||
	        (PROCEDURE_REQUIRED( procedure ) == 0  &&
		 PROCEDURE_OPTIONAL( procedure ) == 0))
	      sc_error( "SAVE-HEAP",
	      		"Restart procedure must take 1 argument", 0 );
	   if  (PAIR_CDR( argl ) != EMPTYLIST)  {
	      sc_error( "SAVE-HEAP", "Too many arguments", 0 );
	   }
	}
	if  (TSCPTAG( filename ) != EXTENDEDTAG  ||
	     T_U( filename )->string.tag != STRINGTAG)
	   sc_error( "SAVE-HEAP", "File name is not a STRING: ~s", 1,
	   	     filename );
	heapfile = open( &(T_U( filename )->string.char0),
			 (O_WRONLY | O_CREAT | O_TRUNC), 0755 );
	if  (heapfile == -1)
	   sc_error( "SAVE-HEAP", "Can't open HEAP FILE: ~s", 1,
	             C_FIXED( errno ) );
	sc_collect_2dall();
	/* Build the save-heap file header */
	correct = EMPTYLIST;
	for  (i = 0; i < sc_constants->count; i++)
	   correct = sc_cons( *(sc_constants->ptrs[ i ]), correct );
        for  (i = 0; i < sc_globals->count; i++)
	   correct = sc_cons( *(sc_globals->ptrs[ i ]), correct );
	strncpy( save.id, "S->C", 4 );
	save.procedure = procedure;
	save.correct = correct;
	save.etext = ETEXT;
	save.locklist = sc_locklist;
	save.lockcnt = sc_lockcnt;
	save.current_generation = sc_current_generation;
	save.next_generation = sc_next_generation;
	save.limit = sc_limit;
	save.heappages = sc_lastheappage-sc_firstheappage+1;
	save.firstheappage = sc_firstheappage;
	save.freepage = sc_freepage;
	save.allocatedheappages = sc_allocatedheappages;
	save.firstheapp = sc_firstheapp;
	save.conscnt = sc_conscnt;
	save.consp = sc_consp;
	save.extobjwords = sc_extobjwords;
	save.extwaste = sc_extwaste;
	save.extobjp = sc_extobjp;
	save.sc_stackbase = sc_stackbase;
	save.sc_whenfreed = sc_whenfreed;
	save.sc_constants_limit = sc_constants->limit;
	save.sc_globals_limit = sc_globals->limit;
	save.sc_maxdisplay = sc_maxdisplay;
	heapout( &save, sizeof( save ) );
	heapout( sc_constants, sizeofSCPTRS( sc_constants->limit ) );
	heapout( sc_globals, sizeofSCPTRS( sc_globals->limit ) );
	heapout( &sc_pagegeneration[ sc_firstheappage ], sc_heappages*4 );
	heapout( &sc_pagetype[ sc_firstheappage ], sc_heappages*4 );
	pagecount = 0;
	for  (i = sc_firstheappage; i <= sc_lastheappage; i++)  {
	   if  (sc_pagegeneration[ i ] == sc_current_generation  ||
		(~sc_pagegeneration[ i ] & 1  &&  sc_pagegeneration[ i ]))  {
	      if  (pagecount++ == 0)  firstpage = i;
	   }
	   else  if  (pagecount)  {
	      heapout( PAGE_ADDRESS( firstpage ), pagecount*PAGEBYTES );
	      pagecount = 0;
	   }
	}
	if  (pagecount)
	   heapout( PAGE_ADDRESS( firstpage ), pagecount*PAGEBYTES );
	close( heapfile );
	return( TRUEVALUE );
}

/* The following routine is called from a Scheme main program to determine
   how the heap is to be constructed.  If the heap is being constructed from
   a saved file, then this function will not return.  If there is no saved
   heap, then sc_newheap will be called to initialize the heap.
*/

void  sc_restoreheap( desiredheap, argc, argv, mainproc )
	int  desiredheap;
	int  argc;
	char  *argv[];
	void  (*mainproc)();
{
	int  i,
	     pagecount,
	     firstpage;
	char  *freebase;
	TSCP  cl,
	      *address,
	      address_value;

	if  (module_initialized)  return;
	if  (desiredheap)  {
	   defaultheap = desiredheap;
	   minheap = desiredheap;
	}
	decodearguments( argc, argv );
	if  (heapfilename == NULL)  {
	   sc_newheap();
	   return;
	}
	/* Saved heap exists, open it and validate the header */
	heapfile = open( heapfilename, O_RDONLY );
	if  (heapfile == -1)  {
	   fprintf( stderr, "***** Can't open heap file: %d\n", errno );
	   exit( 1 );
	}
	heapin( &save, sizeof( save ) );
	if  (strncmp( save.id, "S->C", 4)  ||  save.etext != ETEXT)  {
	   fprintf( stderr, "***** Incompatible heap file image\n" );
	   exit( 1 );
	}
	/* Initialize similar to sc__init */
	if  (scheap < save.heappages/(ONEMB/PAGEBYTES))
	   scheap = save.heappages/(ONEMB/PAGEBYTES);
	if  (sclimit < save.limit)  sclimit = save.limit;
	sc_limit = sclimit;
	sc_heappages = scheap*(ONEMB/PAGEBYTES);
	if  (scmaxheap < scheap)  scmaxheap = scheap;
	sc_maxheappages = scmaxheap*(ONEMB/PAGEBYTES);
	sc_allocatedheappages = save.allocatedheappages;
	freebase = getmem( scheap*ONEMB );
	sc_firstheappage = ADDRESS_PAGE( freebase );
	sc_lastheappage = sc_firstheappage+sc_heappages-1;
	sc_firstheapp = (int*)freebase;
	sc_lastheapp = sc_firstheapp+PAGEWORDS*sc_heappages-1;
	sc_freepage = save.freepage;
	allocate_sidetables( sc_firstheappage, sc_lastheappage,
			     &sc_pagegeneration, &sc_pagetype, &sc_pagelock,
			     &sc_pagelink );
	sc_current_generation = save.current_generation;
	sc_next_generation = save.next_generation;
	sc_constants =
	     (struct SCPTRS*)malloc( sizeofSCPTRS( save.sc_constants_limit ) );
	heapin( sc_constants, sizeofSCPTRS( save.sc_constants_limit ) );
	sc_globals =
	     (struct SCPTRS*)malloc( sizeofSCPTRS( save.sc_globals_limit ) );
	heapin( sc_globals, sizeofSCPTRS( save.sc_globals_limit ) );
	heapin( &sc_pagegeneration[ sc_firstheappage ], save.heappages*4 );
	for  (i = save.firstheappage+save.heappages; i <= sc_lastheappage;
	      i++ )
	   sc_pagegeneration[ i ] = 1;
	heapin( &sc_pagetype[ sc_firstheappage ], save.heappages*4 );
	sc_genlist = -1;
	for  (i = sc_firstheappage; i <= sc_lastheappage; i++)  {
	   sc_pagelink[ i ] = 0;
	   sc_pagelock[ i ] = 0;
	}
	sc_initiallink = OKTOSET;
	sc_conscnt = save.conscnt;
	sc_consp = save.consp;
	sc_extobjwords = save.extobjwords;
	sc_extobjp = save.extobjp;
	sc_extwaste = save.extwaste;
	sc_mutex = 0;
	sc_pendingsignals = 0;
	sc_emptylist = EMPTYLIST;
	sc_emptyvector = U_T( &emptyvector, EXTENDEDTAG );
	sc_emptystring = U_T( emptystring, EXTENDEDTAG );
	sc_falsevalue = FALSEVALUE;
	sc_truevalue = TRUEVALUE;
	sc_eofobject = EOFOBJECT;
	sc_undefined = UNDEFINED;
	sc_stdin = stdin;
	sc_stdout = stdout;
	sc_stderr = stderr;
	sc_maxdisplay = save.sc_maxdisplay;
	sc_stackbase = save.sc_stackbase;
	sc_whenfreed = save.sc_whenfreed;
	sc_freed = EMPTYLIST;
	sc_stacktrace = NULL;
	/* Reload the heap and correct globals which point into it */
	pagecount = 0;
	for  (i = sc_firstheappage; i < sc_firstheappage+save.heappages;
	      i++)  {
	   if  (sc_pagegeneration[ i ] == sc_current_generation  ||
		~sc_pagegeneration[ i ] & 1)  {
	      if  (pagecount++ == 0)  firstpage = i;
	   }
	   else  if  (pagecount)  {
	      heapin( PAGE_ADDRESS( firstpage ), pagecount*PAGEBYTES );
	      pagecount = 0;
	   }
	}
	if  (pagecount)
	   heapin( PAGE_ADDRESS( firstpage ), pagecount*PAGEBYTES );
	cl = save.correct;
        for  (i = sc_globals->count-1; i >= 0 ; i--)  {
	   *(sc_globals->ptrs[ i ]) = PAIR_CAR( cl );
	   cl = PAIR_CDR( cl );
	}
	for  (i = sc_constants->count-1; i >= 0; i--)  {
	   *(sc_constants->ptrs[ i ]) = PAIR_CAR( cl );
	   cl = PAIR_CDR( cl );
	}
	sc_clink = EMPTYLIST;
	close( heapfile );
	module_initialized = 1;
	sc__2dfile_2a_67475874_v = sc_cstringtostring( heapfilename );
	if  (sc_gcinfo)
	   fprintf( stderr,
	    "***** SCGCINFO = %d  SCHEAP = %d  SCMAXHEAP = %d  SCLIMIT = %d\n",
		    sc_gcinfo, scheap, scmaxheap, sclimit );
	/* Start execution at the appropriate procedure */
	if  (save.procedure != FALSEVALUE)
	   sc_apply_2dtwo( save.procedure,
	   	          sc_cons( sc_clarguments( argc, argv ), EMPTYLIST ) );
	else  if  (mainproc != NULL)  
	   (*mainproc)( sc_clarguments( argc, argv ) );
	else
	   return;
	SCHEMEEXIT();
}

/* This initialization function is provided to allow automatic initialization
   from a Modula-2 program.
*/

sc__init()
{
	sc_restoreheap( 0, 0, NULL, NULL );
}

/* Routines coded in C call the following function to access the Scheme ERROR
   function.  SYMBOL is a string representing the function name.  FORMAT is a
   string which is a format descriptor.  ARGC is the argument count which is
   followed by the arguments.
*/

sc_error( va_alist )
	va_dcl
{
	char  *symbol, *format;
	int  argc;
	TSCP  argl;
	va_list  argp;

	va_start( argp );
	symbol = va_arg( argp, char* );
	format = va_arg( argp, char* );
	argc = va_arg( argp, int );
	argl = sc_emptylist;
	while  (argc--)  argl = sc_cons( va_arg( argp, TSCP ), argl );
	scrt6_error( sc_string_2d_3esymbol( sc_cstringtostring( symbol ) ),
		     sc_cstringtostring( format ),
		     scrt1_reverse( argl ) );
}

/* The following function returns informations about the implementation.  The
   form of the function follows a recent proposal on rrrs-authors.  The result
   is a list of strings or #F's of the form:

	  (<name> <version> <MACHINE> <CPU> <OS> <FS> . <supports>)
*/

TSCP  sc_implementation_v;

TSCP  sc_implementation()
{
	return(
	   sc_cons(
	      sc_cstringtostring( "Scheme->C" ),
	      sc_cons(
	         sc_cstringtostring( "01nov91jfb+wcdw" ),
	         sc_cons(
#ifdef MIPS
#ifdef BIGENDIAN
		    sc_cstringtostring( "Big Endian MIPS" ),
#else
		    sc_cstringtostring( "DECstation" ),
#endif
#endif
#ifdef TITAN
		    sc_cstringtostring( "WRL-TITAN" ),
#endif
#ifdef VAX
		    sc_cstringtostring( "VAX" ),
#endif
		    sc_cons(
#ifdef MIPS
		       sc_cstringtostring( "R2000/R3000" ),
#endif
#ifdef TITAN
		       sc_cstringtostring( "BYTE-ADDRESSED" ),
#endif
#ifdef VAX
		       sc_cstringtostring( "VAX" ),
#endif
		       sc_cons(
		          sc_cstringtostring( "ULTRIX" ),
		          sc_cons(
			      FALSEVALUE,
			      EMPTYLIST
			         )
			      )
			   )
		        )
		     )
	          )
	      );
}
