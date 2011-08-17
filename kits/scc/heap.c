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

/* This module implements the object storage storage system. */

/* Import definitions */

#include "objects.h"
#include "scinit.h"
#include "heap.h"
#include "callcc.h"
#include "signal.h"
#include "apply.h"
extern  abort();
#ifdef MIPS
extern  sc_s0tos8();
#endif
#ifdef VAX
extern  sc_r2tor11();
#endif

/* Forward declarations */

static int  move_ptr();

static SCP  move_object();

static void  move_continuation_ptr();

/* Allocate storage which is defined in "heap.h" */

int  	*sc_pagegeneration,	/* page generation table */
	*sc_pagetype,		/* page type table */
	*sc_pagelock,		/* page lock table */
	*sc_pagelink,		/* page lock list link table */
	sc_initiallink,		/* Value to put in sc_pagelink field for a
				   newly allocated page */
	sc_locklist,		/* list header for locked pages */
	sc_genlist,		/* list of modified pages */
	sc_lockcnt,		/* # of locked pages */
	sc_current_generation,  /* current generation */
	sc_next_generation;	/* next generation */

int	sc_firstheappage,	/* first page in the Scheme heap */
	sc_lastheappage,	/* last page in the Scheme heap  */
	sc_limit,		/* % of heap allocated after collecton
				   that forces total collection */
	sc_freepage,		/* free page index */
	sc_heappages,		/* # of pages in the Scheme heap */
	sc_maxheappages,	/* Maximum # of pages in Scheme heap */
	sc_allocatedheappages,  /* # of pages currently allocated */
	sc_generationpages,	/* # of pages in saved generations */
	*sc_firstheapp,		/* ptr to first word in the Scheme heap */
	*sc_lastheapp;		/* ptr to last word in the Scheme heap */

int	sc_conscnt;		/* # cons cells in sc_consp */
SCP	sc_consp;		/* pointer to next cons cell */

int	sc_extobjwords,		/* # of words for ext objs in sc_extobjp */
	sc_extwaste;		/* # of words wasted on page crossings */
SCP	sc_extobjp;		/* pointer to next free extended obj word */

int	sc_gcinfo;		/* controls logging */

static struct rusage gcru,	/* resource consumption during collection */
	             startru,
	      	     stopru;

static int  sc_newlist;		/* list of newly allocated pages */

int	*sc_stackbase;		/* pointer to base of the stack */

TSCP	sc_whenfreed,		/* list of items needing cleanup when free */
	sc_freed;		/* list of free items to be cleanup */

TSCP	sc_after_2dcollect_v,	/* Collection status callback */
	sc__2afrozen_2dobjects_2a_v;
				/* User managed frozen object list */

/* Each time a weak-cons is created, an entry is made on the following list.
   Each entry of the list is a 3-element vector with the following fields:

      pointer to the next entry (or EMPTYLIST)
      pointer to the cons cell
      cell to hold the original value from the CAR of the cons cell
*/

static TSCP  weakconsl = EMPTYLIST;

#define  WEAK_LINK( x )	VECTOR_ELEMENT( x, C_FIXED( 0 ) )
#define  WEAK_CONS( x ) VECTOR_ELEMENT( x, C_FIXED( 1 ) )
#define  WEAK_CAR( x )  VECTOR_ELEMENT( x, C_FIXED( 2 ) )
#define  MAKE_WEAK	sc_make_2dvector( C_FIXED( 3 ), EMPTYLIST )

/* When pages are allocated during garbage collection, they are queued on the
   following two lists for later scanning.  One list holds pages allocated for
   cons cells and the other holds pages allocated for extended objects.  The
   lists are threaded through sc_pagelink, the pointer points to the tail of
   the list, and the tail link points to the head.
*/

static int  cons_pages = 0,
	    extended_pages = 0;

#define  QUEUE_PAGE( tail, page )  \
	if  (tail == 0)  {  \
	   tail = sc_pagelink[ page ] = page;  \
	}  else  {  \
	   sc_pagelink[ page ] = sc_pagelink[ tail ];  \
	   sc_pagelink[ tail ] = page;  \
	   tail = page;  \
	}

#define  DELETE_PAGE( tail, page )  \
	if  (tail == 0)  \
	   page = NULL;  \
	else  {  \
	   page = sc_pagelink[ tail ];  \
	   sc_pagelink[ tail ] = sc_pagelink[ page ];  \
	   if  (tail == page)  tail = 0;  \
	}

/* The following function converts a rusage structure into an 18 word Scheme
   vector composed of the same items.
*/

static TSCP  rusagevector( ru )
	struct rusage *ru;
{
	TSCP  v;
	PATSCP  ve;

	v = sc_make_2dvector( C_FIXED( 18 ), EMPTYLIST );
	ve = &(T_U( v )->vector.element0);
	*ve++ = C_FIXED( ru->ru_utime.tv_sec );
	*ve++ = C_FIXED( ru->ru_utime.tv_usec );
	*ve++ = C_FIXED( ru->ru_stime.tv_sec );
	*ve++ = C_FIXED( ru->ru_stime.tv_usec );
	*ve++ = C_FIXED( ru->ru_maxrss );
	*ve++ = C_FIXED( ru->ru_ixrss );
        *ve++ = C_FIXED( ru->ru_idrss );
        *ve++ = C_FIXED( ru->ru_isrss );
        *ve++ = C_FIXED( ru->ru_minflt );
        *ve++ = C_FIXED( ru->ru_majflt );
        *ve++ = C_FIXED( ru->ru_nswap );
        *ve++ = C_FIXED( ru->ru_inblock );
        *ve++ = C_FIXED( ru->ru_oublock );
        *ve++ = C_FIXED( ru->ru_msgsnd );
        *ve++ = C_FIXED( ru->ru_msgrcv );
	*ve++ = C_FIXED( ru->ru_nsignals );
	*ve++ = C_FIXED( ru->ru_nvcsw );
	*ve++ = C_FIXED( ru->ru_nivcsw );
	return( v );
}

/* Garbage collector resource usage is accumulated by the following function.
   It will accumlate the resources used in gcru, and change stopru to reflect
   the resource usage this collection.
*/

static updategcru()
{
	int  x;

	/* Compute deltas in stopru */
	if  (stopru.ru_utime.tv_usec < startru.ru_utime.tv_usec)  {
	   stopru.ru_utime.tv_sec = stopru.ru_utime.tv_sec-
				    startru.ru_utime.tv_sec-1;
	   stopru.ru_utime.tv_usec = 1000000+stopru.ru_utime.tv_usec-
	   			     startru.ru_utime.tv_usec;
	}
	else  {
	   stopru.ru_utime.tv_sec = stopru.ru_utime.tv_sec-
				    startru.ru_utime.tv_sec;
	   stopru.ru_utime.tv_usec = stopru.ru_utime.tv_usec-
				     startru.ru_utime.tv_usec;
	}	   
	if  (stopru.ru_stime.tv_usec < startru.ru_stime.tv_usec)  {
	   stopru.ru_stime.tv_sec = stopru.ru_stime.tv_sec-
				    startru.ru_stime.tv_sec-1;
	   stopru.ru_stime.tv_usec = 1000000+stopru.ru_stime.tv_usec-
	   			     startru.ru_stime.tv_usec;
	}
	else  {
	   stopru.ru_stime.tv_sec = stopru.ru_stime.tv_sec-
				    startru.ru_stime.tv_sec;
	   stopru.ru_stime.tv_usec = stopru.ru_stime.tv_usec-
				     startru.ru_stime.tv_usec;
	}
	stopru.ru_minflt -= startru.ru_minflt;
	stopru.ru_majflt -= startru.ru_majflt;
	stopru.ru_nswap -= startru.ru_nswap;
	stopru.ru_inblock -= startru.ru_inblock;
	stopru.ru_oublock -= startru.ru_oublock;
	stopru.ru_msgsnd -= startru.ru_msgsnd;
	stopru.ru_msgrcv -= startru.ru_msgrcv;
	stopru.ru_nsignals -= startru.ru_nsignals;
	stopru.ru_nvcsw -= startru.ru_nvcsw;
	stopru.ru_nivcsw -= startru.ru_nivcsw;

	/* Accumulate totals in gcru */
	x = gcru.ru_utime.tv_usec+stopru.ru_utime.tv_usec;
	gcru.ru_utime.tv_usec = x % 1000000;
	gcru.ru_utime.tv_sec = gcru.ru_utime.tv_sec+stopru.ru_utime.tv_sec+
			       x / 1000000;
	x = gcru.ru_stime.tv_usec+stopru.ru_stime.tv_usec;
	gcru.ru_stime.tv_usec = x % 1000000;
	gcru.ru_stime.tv_sec = gcru.ru_stime.tv_sec+stopru.ru_stime.tv_sec+
			       x / 1000000;
	gcru.ru_maxrss = stopru.ru_maxrss;
	gcru.ru_ixrss = stopru.ru_ixrss;
	gcru.ru_idrss = stopru.ru_idrss;
	gcru.ru_minflt += stopru.ru_minflt;
	gcru.ru_majflt += stopru.ru_majflt;
	gcru.ru_nswap += stopru.ru_nswap;
	gcru.ru_inblock += stopru.ru_inblock;
	gcru.ru_oublock += stopru.ru_oublock;
	gcru.ru_msgsnd += stopru.ru_msgsnd;
	gcru.ru_msgrcv += stopru.ru_msgrcv;
	gcru.ru_nsignals += stopru.ru_nsignals;
	gcru.ru_nvcsw += stopru.ru_nvcsw;
	gcru.ru_nivcsw += stopru.ru_nivcsw;
}
	
/* The following function returns the resource usage information for the
   process.  It returns a vector formed of the elements in the rusage struct
   returned by getrusage.  It is visible in Scheme as (MY-RUSAGE).  
*/

TSCP  sc_my_2drusage_v;

TSCP  sc_my_2drusage()
{
	struct rusage ru;

	getrusage( 0, &ru );
	return( rusagevector( &ru ) );
}

/* The following function returns the resource usage information for the
   garbage collector.  It returns a vector formed of the elements in the rusage
   struct maintained by the collector.  It is visible in Scheme as
   (COLLECT-RUSAGE).
*/

TSCP  sc_collect_2drusage_v;

TSCP  sc_collect_2drusage()
{
	return( rusagevector( &gcru ) );
}

/* Errors detected during garbage collection are logged by the following
   procedure.  If any errors occur, the program will abort after logging
   them.  More than 30 errors will result in the program being aborted at
   once.
*/

static SCP  moving_object;

static int  pointer_errors = 0;

static void  pointererror( msg, pp )
	char*  msg;
	SCP  pp;
{
	fprintf( stderr, "***** COLLECT pointer error in %x, ",
		 moving_object );
	fprintf( stderr, msg, pp );
	if  (++pointer_errors == 30)  abort();
}

#ifdef TITAN
/* The following function is called to read one of the Titan registers.  It
   must be open-coded using constant register numbers as zzReadRegister is
   actually a Mahler inline function which expects a constant register
   number.
*/

int  *sc_processor_register( regnum )
{
	switch (regnum)  {
		case  0: return( zzReadRegister(  0 ) ); 
		case  1: return( zzReadRegister(  1 ) ); 
		case  2: return( zzReadRegister(  2 ) ); 
		case  3: return( zzReadRegister(  3 ) ); 
		case  4: return( zzReadRegister(  4 ) ); 
                case  5: return( zzReadRegister(  5 ) );
                case  6: return( zzReadRegister(  6 ) );
                case  7: return( zzReadRegister(  7 ) );
                case  8: return( zzReadRegister(  8 ) );
                case  9: return( zzReadRegister(  9 ) );
		case 10: return( zzReadRegister( 10 ) ); 
		case 11: return( zzReadRegister( 11 ) ); 
		case 12: return( zzReadRegister( 12 ) ); 
		case 13: return( zzReadRegister( 13 ) ); 
		case 14: return( zzReadRegister( 14 ) ); 
		case 15: return( zzReadRegister( 15 ) );
		case 16: return( zzReadRegister( 16 ) );
		case 17: return( zzReadRegister( 17 ) );
		case 18: return( zzReadRegister( 18 ) );
		case 19: return( zzReadRegister( 19 ) );
		case 20: return( zzReadRegister( 20 ) ); 
		case 21: return( zzReadRegister( 21 ) ); 
		case 22: return( zzReadRegister( 22 ) ); 
		case 23: return( zzReadRegister( 23 ) ); 
		case 24: return( zzReadRegister( 24 ) ); 
                case 25: return( zzReadRegister( 25 ) );
                case 26: return( zzReadRegister( 26 ) );
                case 27: return( zzReadRegister( 27 ) );
                case 28: return( zzReadRegister( 28 ) );
                case 29: return( zzReadRegister( 29 ) );
		case 30: return( zzReadRegister( 30 ) ); 
		case 31: return( zzReadRegister( 31 ) ); 
		case 32: return( zzReadRegister( 32 ) ); 
		case 33: return( zzReadRegister( 33 ) ); 
		case 34: return( zzReadRegister( 34 ) ); 
		case 35: return( zzReadRegister( 35 ) );
		case 36: return( zzReadRegister( 36 ) );
		case 37: return( zzReadRegister( 37 ) );
		case 38: return( zzReadRegister( 38 ) );
		case 39: return( zzReadRegister( 39 ) );
		case 40: return( zzReadRegister( 40 ) ); 
		case 41: return( zzReadRegister( 41 ) ); 
		case 42: return( zzReadRegister( 42 ) ); 
		case 43: return( zzReadRegister( 43 ) ); 
		case 44: return( zzReadRegister( 44 ) ); 
		case 45: return( zzReadRegister( 45 ) );
		case 46: return( zzReadRegister( 46 ) );
		case 47: return( zzReadRegister( 47 ) );
		case 48: return( zzReadRegister( 48 ) );
		case 49: return( zzReadRegister( 49 ) );
		case 50: return( zzReadRegister( 50 ) ); 
		case 51: return( zzReadRegister( 51 ) ); 
		case 52: return( zzReadRegister( 52 ) ); 
		case 53: return( zzReadRegister( 53 ) ); 
		case 54: return( zzReadRegister( 54 ) ); 
		case 55: return( zzReadRegister( 55 ) );
		case 56: return( zzReadRegister( 56 ) );
		case 57: return( zzReadRegister( 57 ) );
		case 58: return( zzReadRegister( 58 ) );
		case 59: return( zzReadRegister( 59 ) );
		case 60: return( zzReadRegister( 60 ) ); 
		case 61: return( zzReadRegister( 61 ) ); 
		case 62: return( zzReadRegister( 62 ) ); 
		case 63: return( zzReadRegister( 63 ) );
		default: return( 0 );
	}
}

/* All processor registers are traced by the following procedure. */

static  trace_stack_and_registers()
{
	int  i, *r0tor60[ 61 ], *pp;

	for  (i = 0; i <= 60; i++)  r0tor60[ i ] = sc_processor_register( i );
	pp = STACKPTR;
	while  (pp != sc_stackbase)  move_continuation_ptr( *pp++ );
}
#endif

#ifdef VAX
/* The following code is used to read the stack pointer.  The register
   number is passed in to force an argument to be on the stack, which in
   turn can be used to find the address of the top of stack.
*/

int  *sc_processor_register( reg )
	int  reg;
{
	return( &reg+1 );
}

/* All processor registers which might contain pointers are traced by the
   following procedure.
*/

static  trace_stack_and_registers()
{
	int  i, r2tor11[10], *pp;

	sc_r2tor11( r2tor11 );
	pp = STACKPTR;
	while  (pp != sc_stackbase)  move_continuation_ptr( *pp++ );
}
#endif

#ifdef MIPS
/* The following code is used to read the stack pointer.  The register
   number is passed in to force an argument to be on the stack, which in
   turn can be used to find the address of the top of stack.
*/

int  *sc_processor_register( reg )
	int  reg;
{
	return( &reg );
}

/* All processor registers which might contain pointers are traced by the
   following procedure.
*/

static  trace_stack_and_registers()
{
	int  i, s0tos8[9], *pp;

	sc_s0tos8( s0tos8 );
	pp = STACKPTR;
	while  (pp != sc_stackbase)  move_continuation_ptr( *pp++ );
}
#endif


/* The size of an extended object in words is returned by the following
   function.
*/

static int  extendedsize( obj )
	SCP  obj;
{
	switch  (obj->extendedobj.tag)  {

	   case  SYMBOLTAG:
	      return( SYMBOLSIZE );

	   case  STRINGTAG:
	      return( STRINGSIZE( obj->string.length ) );

	   case  VECTORTAG:
	      return( VECTORSIZE( obj->vector.length ) );

	   case  PROCEDURETAG:
	      return( PROCEDURESIZE );

	   case  CLOSURETAG:
	      return( CLOSURESIZE( obj->closure.length ) );

	   case  CONTINUATIONTAG:
	      return( CONTINUATIONSIZE( obj->continuation.length ) );

	   case  FLOAT32TAG:
	      return( FLOAT32SIZE );

	   case  FLOAT64TAG:
	      return( FLOAT64SIZE );

	   case  FORWARDTAG:
	      return( FORWARDSIZE( obj->forward.length ) );

	   case  WORDALIGNTAG:
	      return( WORDALIGNSIZE );

	   default:
	      fprintf( stderr,
	      	       "***** COLLECT Unknown extended object: %x %x\n",
	      	       obj, obj->extendedobj.tag );
	      abort();
	}
}

/* Words inside continuations are checked by the following function.  If the
   word looks like a pointer, then the page containing the object will be
   locked and the object will be moved.  
*/

static void  move_continuation_ptr( pp )
	SCP  pp;
{
	int  page, tag;
	SCP  sweep, next;

	if (pp >= (SCP)sc_firstheapp  &&  pp < (SCP)sc_lastheapp)  {
	   page = ADDRESS_PAGE( pp );
	   if  (sc_current_generation == sc_pagegeneration[ page ])  {
	      tag = sc_pagetype[ page ];
	      if  (tag == PAIRTAG)  {
	         /* Trace just that PAIR */
	         pp = (SCP)(((int)pp) & ~(CONSBYTES-1));
		 if  (sc_pagelock[ page ] == 0)  {
		    sc_pagelock[ page ] = 1;
		    sc_pagelink[ page ] = sc_locklist;
		    sc_locklist = page;
		    sc_lockcnt = sc_lockcnt+1;
		 }
		 if  (sc_gcinfo == 2  &&  pp->forward.tag != FORWARDTAG)
		    fprintf( stderr,
		    	     "              move_continuation_ptr %x\n",
			     U_T( pp, PAIRTAG ) );
		 move_ptr( U_T( pp, PAIRTAG ) );
		 return;
	      }
	      /* Trace the referenced object */
	      if  (tag == BIGEXTENDEDTAG)  {
		  while (sc_pagetype[ page ] != EXTENDEDTAG)  page--;
	      }
	      sweep = (SCP)PAGE_ADDRESS( page );
	      if  (sc_pagelock[ page ] == 0)  {
		 sc_pagelock[ page ] = 1;
		 sc_pagelink[ page ] = sc_locklist;
		 sc_locklist = page;
	         if  (sweep->wordalign.tag == WORDALIGNTAG)  {
	            sweep = (SCP)( ((int*)sweep)+WORDALIGNSIZE );
	         }
		 sc_lockcnt = (extendedsize( sweep )+PAGEWORDS-1)/PAGEWORDS+
		 	      sc_lockcnt;
	      }
	      while  (ADDRESS_PAGE( sweep ) == page  &&
		      sweep->unsi.gned != ENDOFPAGE)  {
		 next = (SCP)( ((int*)sweep)+extendedsize( sweep ) );
		 if  ((unsigned)pp < (unsigned)next)  {
		    /* sweep points to object to move */
		    if  (sc_gcinfo == 2  &&  sweep->forward.tag != FORWARDTAG)
	               fprintf( stderr,
		 	        "              move_continuation_ptr %x\n",
			        U_TX( sweep ) );
		    move_ptr( U_TX( sweep ) );
		    return;
		 }
		 sweep = next;
	      }
	   }
	}
}

/* Objects are moved from old space to new space by calling this procedure
   with a Scheme pointer to the object.  Note that this function does not
   return the new value of the pointer, as it cannot be discerned at this time
   as all locked pages may not have been found yet.  N.B. in the generational
   scheme, only objects in sc_current_generation are moved.
*/

static  move_ptr( tpp )
	TSCP  tpp;
{
	int  length, words, *oldp, *newp, page;
	TSCP  new;
	SCP  pp;

	pp = T_U( tpp );
	switch  TSCPTAG( tpp )  {

	   case  FIXNUMTAG:
	   	return;

	   case  EXTENDEDTAG:
	   	page = ADDRESS_PAGE( pp );
		if  (NOT_S2CPAGE( page )  ||
		     pp->forward.tag == FORWARDTAG  ||
		     pp->wordalign.tag == WORDALIGNTAG  ||
		     sc_pagegeneration[ page ] != sc_current_generation)
		   return;
		if  (sc_pagetype[ page ] != EXTENDEDTAG)  {
		   pointererror( "%x not in an EXTENDEDTAG page\n", pp );
		   return;
		}
		words = extendedsize( pp );
		length = words;
		newp = (int*)sc_allocateheap( extendedsize( pp ),
					      pp->extendedobj.tag, 0 );
		new = U_T( newp, EXTENDEDTAG );
		oldp = (int*)pp;
		while  (words--)  *newp++ = *oldp++;
		pp->forward.tag = FORWARDTAG;
		pp->forward.length = length;
		pp->forward.forward = new;
		return;

	   case  IMMEDIATETAG:
	        return;

	   case  PAIRTAG:
		page = ADDRESS_PAGE( pp );
	        if  (pp->forward.tag == FORWARDTAG  ||
		     sc_pagegeneration[ page ] != sc_current_generation)
		   return;
		if  (sc_pagetype[ page ] != PAIRTAG)  {
		   pointererror( "%x not in a PAIRTAG page\n", pp );
		   return;
		}
		pp->forward.forward = sc_cons( pp->pair.car, pp->pair.cdr );
		pp->forward.tag = FORWARDTAG;
		pp->forward.length = CONSSIZE;
		return;
	}
}  

/* MOVE_OBJECT is called to move all extended objects in a page starting at
   a starting point.  It will return a pointer to the first object that it
   could not move, or NULL if the page was finished.
*/

static SCP  move_object( pp )
	SCP  pp;
{
	int  page, size, cnt, vpage;
	PATSCP  obj;

	page = ADDRESS_PAGE( pp );
	while  (ADDRESS_PAGE( pp ) == page  &&
		(pp != sc_extobjp  ||  sc_extobjwords == 0)  &&
		pp->unsi.gned != ENDOFPAGE)  {
	   moving_object = pp;
	   switch  ( pp->extendedobj.tag )  {
	      case  SYMBOLTAG:
	         move_ptr( pp->symbol.name );
		 vpage = ADDRESS_PAGE( pp->symbol.ptrtovalue );
		 if  (S2CPAGE( vpage ))
		    pp->symbol.ptrtovalue = &pp->symbol.value;
		 move_ptr( *pp->symbol.ptrtovalue );
		 move_ptr( pp->symbol.propertylist );
		 size = SYMBOLSIZE;
		 break;

	      case  STRINGTAG:
	   	 size = STRINGSIZE( pp->string.length );
		 break;

	      case  VECTORTAG:
	         cnt = pp->vector.length;
		 obj = &pp->vector.element0;
		 while  (cnt--)  move_ptr( *obj++ );
		 size = VECTORSIZE( pp->vector.length );
		 break;	         

	      case  PROCEDURETAG:
	         move_ptr( pp->procedure.closure );
	   	 size = PROCEDURESIZE;
		 break;

	      case  CLOSURETAG:
	         move_ptr( pp->closure.closure );
		 cnt = pp->closure.length;
		 obj = &pp->closure.var0;
		 while  (cnt--)  move_ptr( *obj++ );
		 size = CLOSURESIZE( pp->closure.length );
		 break;

	      case  CONTINUATIONTAG:
	   	 move_ptr( pp->continuation.continuation );
		 obj = &pp->continuation.continuation;
		 cnt = pp->continuation.length;
		 while  (cnt--)  move_continuation_ptr( *(++obj) );
		 size = CONTINUATIONSIZE( pp->continuation.length );
		 break;

	      case  FLOAT32TAG:
	   	 size = FLOAT32SIZE;
		 break;

	      case  FLOAT64TAG:
	   	 size = FLOAT64SIZE;
		 break;

	      case  FORWARDTAG:
	         size = FORWARDSIZE( pp->forward.length );
		 break;

	      case  WORDALIGNTAG:
		 size = WORDALIGNSIZE;
		 break;

	      default:
	         pointererror( "%x is not a valid extended object tag\n",
	         	       pp->extendedobj.tag );
	   }
	   pp = (SCP)( ((int*)pp)+size );
	}
	if  (ADDRESS_PAGE( pp ) == page  &&  pp == sc_extobjp  &&
	     sc_extobjwords != 0)
	   return( pp );
	return( NULL );
}

/* The following function is called to resolve a pointer that might be
   forwarded.  It returns the resolved pointer.
*/

static TSCP  resolveptr( obj )
	TSCP  obj;
{
	if  ((TSCPTAG( obj ) & 1) && (T_U( obj )->forward.tag == FORWARDTAG))
	   return( T_U( obj )->forward.forward );
	return( obj );
}

/* Save the car of each weak cons cell that contains a pointer into the heap
   and replace it with #F.
*/

static  save_weakconsl()
{
	TSCP  wl, weakcons;
	SCP  pp;

	wl = weakconsl;
	while  (wl != EMPTYLIST)  {
	   weakcons = WEAK_CONS( wl );
	   pp = T_U( PAIR_CAR( weakcons ) );
	   if  (TSCPTAG( PAIR_CAR( weakcons ) ) & 1  &&
	        pp >= (SCP)sc_firstheapp  &&  pp < (SCP)sc_lastheapp)  {
	      WEAK_CAR( wl ) = PAIR_CAR( weakcons );
	      PAIR_CAR( weakcons ) = FALSEVALUE;
	   }
	   else  {
	      WEAK_CAR( wl ) = FALSEVALUE;
	   }
	   wl = WEAK_LINK( wl );
	}
}

/* Rebuild the weak cons list. */

static  rebuild_weakconsl()
{
	TSCP  wl, oldcons, newcons, oldcar, newcar, weak;

	wl = weakconsl;
	weakconsl = EMPTYLIST;
	while  (wl != EMPTYLIST)  {
	   newcons = resolveptr( (oldcons = WEAK_CONS( wl )) );
	   newcar = resolveptr( (oldcar = WEAK_CAR( wl )) );
	   if  (oldcons == newcons  && 
		sc_pagegeneration[ ADDRESS_PAGE( oldcons ) ] == 
		sc_current_generation)  {
	      /* Cons cell was not retained so drop from list */
	      wl = resolveptr( WEAK_LINK( wl ) );
	   }  
	   else  {
	      if  (oldcar != FALSEVALUE  &&
		   (oldcar != newcar  ||
		    sc_pagegeneration[ ADDRESS_PAGE( oldcar ) ] !=
		    sc_current_generation))  {
	         /* Object is still in use so restore it's car ptr */
	         PAIR_CAR( newcons ) = oldcar;
	      }
	      weak = MAKE_WEAK;
	      WEAK_LINK( weak ) = weakconsl;
	      weakconsl = weak;
	      WEAK_CONS( weak ) = oldcons;
	      wl = resolveptr( WEAK_LINK( wl ) );
	   }
	}
}

/* Once all objects are moved, objects needing special action on deletion are
   discovered by examining SC_WHENFREED.  All objects that have not been moved
   are placed on SC_FREED, and those that have been moved are retained on
   SC_WHENFREED.
*/

static  check_unreferenced()
{
	TSCP  objects, object_procedure, object;

	objects = resolveptr( sc_whenfreed );
	sc_whenfreed = EMPTYLIST;
	while  (objects != EMPTYLIST)  {
	   object_procedure = resolveptr( PAIR_CAR( objects ) );
	   object = PAIR_CAR( object_procedure );
	   if  (object == resolveptr( object )  &&
		sc_pagegeneration[ ADDRESS_PAGE( object ) ] == 
		sc_current_generation)  {
	      /* Object was not forwarded, so it needs to be cleaned up. */
	      sc_freed = sc_cons( object_procedure, sc_freed );
	   }
	   else  {
	      /* Object was forwarded, so leave it on sc_whenfreed. */
	      sc_whenfreed = sc_cons( object_procedure, sc_whenfreed );
	   }
	   objects = resolveptr( PAIR_CDR( objects ) );
	}
}

/* The moves are coordinated by the following function which moves objects on
   newly allocated pages until there is nothing left to move.
*/

static  move_the_heap()
{
	int  progress, count, weaktodo, unreferenced, page;
	SCP  myconsp, myextobjp, newp;

	myconsp = NULL;
	myextobjp = NULL;
	weaktodo = 1;
	unreferenced = 1;
	progress = 1;
	while  (progress--)  {
	   /* Move all the currently allocated, but unmoved pairs. */
	   if  (myconsp == NULL)  {
	      DELETE_PAGE( cons_pages, page );
	      if  (page)  {
		 sc_pagelink[ page ] = sc_newlist;
		 sc_newlist = page;
	      }
	      myconsp = (SCP)PAGE_ADDRESS( page );
	   }
	   if  (myconsp != NULL  &&
	   	(myconsp != sc_consp || sc_conscnt == 0))  {
	      count = (PAGEBYTES-ADDRESS_OFFSET( myconsp ))/CONSBYTES;
	      progress = 1;
	      while  (count--  &&  (myconsp != sc_consp || sc_conscnt == 0))  {
	         moving_object = myconsp;
	         move_ptr( myconsp->pair.car );
	         move_ptr( myconsp->pair.cdr );
	         myconsp = (SCP)(((char*)myconsp)+CONSBYTES);
	      }
	      if  (count == -1)  myconsp = NULL;
	   }

	   /* Move all currently allocated, but unmoved extended items */
	   if  (myextobjp == NULL)  {
	      DELETE_PAGE( extended_pages, page );
	      if  (page)  {
		 sc_pagelink[ page ] = sc_newlist;
		 sc_newlist = page;
	      }
	      myextobjp = (SCP)PAGE_ADDRESS( page );
	   }
	   if  (myextobjp != NULL)  {
	      newp = move_object( myextobjp );
	      if  (newp != myextobjp)  progress = 1;
	      myextobjp = newp;
	   }
	   /* Find weak references needing cleanup */
	   if  (progress == 0  &&  weaktodo)  {
	      weaktodo = 0;
	      rebuild_weakconsl();
	      progress = 1;
	   }
	   /* Find unreferenced objects needing cleanup */
	   if  (progress == 0  &&  unreferenced)  {
	      unreferenced = 0;
	      check_unreferenced();
	      progress = 1;
	   }
	}
	if  (pointer_errors)  abort();
}

/* Objects in the current generation that have references in previous
   generations are moved in the following routine.
*/

static  move_the_generations()
{
	int  page = sc_genlist, count;
	SCP  myconsp;

	/* Correct the newly allocated pages */
	while  (page != -1)  {
	   switch  (sc_pagetype[ page ])  {

	      case  PAIRTAG:
		 myconsp = (SCP)PAGE_ADDRESS( page );
		 count = PAGEBYTES/CONSBYTES;
		 while  (count--)  {
		    move_ptr( myconsp->pair.car );
		    move_ptr( myconsp->pair.cdr );
		    myconsp = (SCP)(((char*)myconsp)+CONSBYTES);
		 }
		 break;

	      case  EXTENDEDTAG:
	         move_object( (SCP)PAGE_ADDRESS( page ) );
		 break;
	   }
	   page = sc_pagelink[ page ];
	}
}

/* Once all objects are moved, pointers can be corrected to either point to the
   new object (when it can be copied), or point to the old object (when the
   page is locked).  This is done by the following function which takes a
   tagged pointer as its argument and returns the new value of the pointer.
*/

static TSCP  correct( tobj )
	TSCP  tobj;
{
	SCP  obj;

	if  (((int)tobj) & 1)  {
	   obj = T_U( tobj );
	   if  ( (obj->forward.tag != FORWARDTAG)  ||
	         sc_pagelock[ ADDRESS_PAGE( obj ) ] )  return  tobj;
	   return( obj->forward.forward );
	}
	return( tobj );
}

/* The pointers within extended objects are corrected by the following
   function.  It is called with a pointer to an object.  All objects which
   follow it on that page will be corrected.
*/

static  correct_object( pp )
	SCP  pp;
{
	int  page, size, cnt;
	PATSCP  obj;

	page = ADDRESS_PAGE( pp );
	while  (ADDRESS_PAGE( pp ) == page  &&
		pp->unsi.gned != ENDOFPAGE  &&
		(pp != sc_extobjp  ||  sc_extobjwords == 0))  {
	   switch  ( pp->extendedobj.tag )  {
	      case  SYMBOLTAG:
	         pp->symbol.name = correct( pp->symbol.name );
		 *pp->symbol.ptrtovalue = correct( *pp->symbol.ptrtovalue );
		 pp->symbol.propertylist = correct( pp->symbol.propertylist );
		 size = SYMBOLSIZE;
		 break;

	      case  STRINGTAG:
	   	 size = STRINGSIZE( pp->string.length );
		 break;

	      case  VECTORTAG:
	         cnt = pp->vector.length;
		 obj = &pp->vector.element0;
		 while  (cnt--)  {
		    *obj = correct( *obj );
		    obj++;
		 }
		 size = VECTORSIZE( pp->vector.length );
		 break;	         

	      case  PROCEDURETAG:
	         pp->procedure.closure = correct( pp->procedure.closure );
	   	 size = PROCEDURESIZE;
		 break;

	      case  CLOSURETAG:
	         pp->closure.closure = correct( pp->closure.closure );
		 cnt = pp->closure.length;
		 obj = &pp->closure.var0;
		 while  (cnt--)  {
		    *obj = correct( *obj );
		    obj++;
		 }
		 size = CLOSURESIZE( pp->closure.length );
		 break;

	      case  CONTINUATIONTAG:
	   	 pp->continuation.continuation = 
		    correct( pp->continuation.continuation );
		 size = CONTINUATIONSIZE( pp->continuation.length );
		 break;

	      case  FLOAT32TAG:
	   	 size = FLOAT32SIZE;
		 break;

	      case  FLOAT64TAG:
	   	 size = FLOAT64SIZE;
		 break;

	      case  WORDALIGNTAG:
		 size = WORDALIGNSIZE;
		 break;

	      default:
	         fprintf( stderr,
	      	          "***** COLLECT Unknown extended object: %x %x\n",
	      	          pp, pp->extendedobj.tag );
	         abort();
	   }
	   pp = (SCP)( ((int*)pp)+size );
	}
}

/* Pointer correction to lists of pages is done by the following procedure.
   The list is terminated by a -1, and the sc_pagelink field for each page
   is set to linkvalue.
*/

static  correct_pointers( page, linkvalue )
	int  page, linkvalue;
{
	int  count, i;
	PATSCP  ptr;

	/* Correct the newly allocated pages */
	while  (page != -1)  {
	   switch  (sc_pagetype[ page ])  {
	      case  PAIRTAG:
		 ptr = (PATSCP)PAGE_ADDRESS( page );
		 count = PAGEBYTES/(CONSBYTES/2);
		 while  (count--)  {
		    if  ((*((int*)ptr) & 1)  &&
		         (T_U(*ptr)->forward.tag == FORWARDTAG)  &&
		         (sc_pagelock[ ADDRESS_PAGE( *ptr ) ] == 0))
		       *ptr = T_U(*ptr)->forward.forward;
		    ptr++;
		    }
		 i = page;
		 page = sc_pagelink[ page ];
		 sc_pagelink[ i ] = linkvalue;
		 break;

	       case  EXTENDEDTAG:
		 correct_object( (SCP)PAGE_ADDRESS( page ) );
		 i = page;
		 page = sc_pagelink[ page ];
		 do  sc_pagelink[ i++ ] = linkvalue;
		 while  (i <= sc_lastheappage  &&
			 sc_pagetype[ i ] == BIGEXTENDEDTAG);
		 break;
	   }
	}
}

/* After pointers have been corrected, the items on locked pages need to have
   their correct version (found in the new copy) copied to the old page.  In
   addition, objects which were not forwarded must be changed so that their
   pointers will no longer be followed.  This is done by setting the CAR and
   CDR of the pair to 0, and turning extended objects into strings.  Pages
   that are locked are added to sc_genlist so that will be checked on the
   next collection.
*/

static  copyback_locked_pages( locklist )
	int  locklist;
{
	int  page, count, vpage;
	SCP  obj, fobj, sobj;

	while  (locklist)  {
	   page = locklist;
	   obj = (SCP)PAGE_ADDRESS( page );
	   sc_pagelock[ page ] = 0;
	   sc_pagegeneration[ page ] = sc_next_generation;
	   locklist = sc_pagelink[ locklist ];
	   sc_pagelink[ page ] = sc_genlist;
	   sc_genlist = page;
	   if  (sc_pagetype[ page ] == PAIRTAG)  {
	      /* Move back only the forwarded CONS cells */
	      count = PAGEBYTES/CONSBYTES;
	      while  (count--)  {
	         if  (obj->forward.tag == FORWARDTAG)  {
		    fobj = T_U( obj->forward.forward );
		    obj->pair.car = fobj->pair.car;
		    obj->pair.cdr = fobj->pair.cdr;
		 }
		 else  {
		    obj->pair.car = 0;
		    obj->pair.cdr = 0;
		 }
		 obj = (SCP)((char*)(obj)+CONSBYTES);
	      }
	   }
	   else  if  (sc_pagetype[ page ] == EXTENDEDTAG)  {
	      /* Move extra pages into the next generation */
	      if  (obj->wordalign.tag == WORDALIGNTAG)  {
		 obj = (SCP)( ((int*)obj)+WORDALIGNSIZE );
	      }
	      count = extendedsize( obj );
	      vpage = page;
	      while (count > PAGEWORDS)  {
		 sc_pagegeneration[ ++vpage ] = sc_next_generation;
		 sc_pagelink[ vpage ] = OKTOSET;
		 count = count-PAGEWORDS;
	      }
	      /* Move back the forwarded extended items */
	      while  (ADDRESS_PAGE( obj ) == page  &&
	      	      (obj != sc_extobjp  ||  sc_extobjwords == 0)  &&
	      	      obj->unsi.gned != ENDOFPAGE)  {
		 if  (obj->forward.tag == FORWARDTAG)  {
		    sobj = obj;
		    fobj = T_U( obj->forward.forward );
		    count = obj->forward.length;
		    while  (count--)  {
		       *((int*)obj) = *((int*)fobj);
		       obj = (SCP)(((int*)obj)+1);
		       fobj = (SCP)(((int*)fobj)+1);
		    }
		    if  (sobj->symbol.tag == SYMBOLTAG)  {
		       vpage = ADDRESS_PAGE( sobj->symbol.ptrtovalue );
		       if  (vpage >= sc_firstheappage  &&
		            vpage <= sc_lastheappage)
		          sobj->symbol.ptrtovalue = &sobj->symbol.value;
		    }
		 }
		 else  if  (obj->wordalign.tag == WORDALIGNTAG)  {
		    obj = (SCP)( ((int*)obj)+WORDALIGNSIZE );
		 }
		 else  {
		    count = extendedsize( obj );
		    obj->string.length = ((count-2)*4)+3;
		    obj->string.tag = STRINGTAG;
		    obj = (SCP)( ((int*)obj)+count );
		 }
	      }
	   }
	}
}	   

/* This function is called to check the obarray to make sure that it is
   intact.
*/

static int check_obarray()
{
	int  i, len, page;
	PATSCP  ep;
	TSCP  lp, symbol, value;
	SCP  obarray;

	obarray = T_U( sc_obarray );
	if  (TSCPTAG( sc_obarray ) != EXTENDEDTAG  ||
	     obarray->vector.tag != VECTORTAG)  {
	   fprintf( stderr, "***** COLLECT OBARRAY is not a vector %x\n",
	            sc_obarray );
	   abort();
	}
	len = obarray->vector.length;
	if  (len != 1023)   {
	   fprintf( stderr, "***** COLLECT OBARRAY length is wrong %x\n",
	   	    sc_obarray );
	   abort();
	}
	ep = &obarray->vector.element0;
	for  (i = 0;  i < len;  i++)  {
	   lp = *ep++;
	   while  (lp != EMPTYLIST)  {
	      if  (TSCPTAG( lp ) != PAIRTAG)  {
	         fprintf( stderr,
		 	  "***** COLLECT OBARRAY element is not a list %x\n",
			  lp );
		 abort();
	      }
	      symbol = T_U( lp )->pair.car;
	      if  (T_U( symbol )->symbol.tag != SYMBOLTAG)  {
	         fprintf( stderr,
		 	  "***** COLLECT OBARRAY entry is not a symbol %x\n",
			  symbol );
	         abort();
	      }
	      page = ADDRESS_PAGE( symbol );
	      if  (sc_pagegeneration[ page ] & 1  &&
	           sc_pagegeneration[ page ] != sc_current_generation)  {
	         fprintf( stderr,
		 	  "***** COLLECT OBARRAY symbol generation error %x\n",
			  symbol );
	         abort();
	      }
	      value = *T_U( symbol )->symbol.ptrtovalue;
	      page = ADDRESS_PAGE( value );
	      if  (TSCPTAG( value ) & 1  &&
	           S2CPAGE( page )  &&
	      	   sc_pagegeneration[ page ] & 1  &&
		   sc_pagegeneration[ page ] != sc_current_generation)  {
	         fprintf( stderr,
		 	  "***** COLLECT OBARRAY value generation error %x\n",
			  symbol );
	         abort();
	      }
	      if  (TSCPTAG( value ) & 1  &&
		   (~sc_pagegeneration[ ADDRESS_PAGE( symbol ) ]) & 1  &&
		   sc_pagegeneration[ page ] == sc_current_generation  &&
		   sc_pagelink[ ADDRESS_PAGE( symbol ) ] == 0  &&
		   ADDRESS_PAGE( symbol ) == 
		   ADDRESS_PAGE( T_U( symbol )->symbol.ptrtovalue ))  {
		 fprintf( stderr,
		 	  "***** COLLECT OBARRAY missed a top-level set! %x\n",
			  symbol );
		 abort();
	      }
	      if  (sc_pagetype[ ADDRESS_PAGE( symbol ) ] != EXTENDEDTAG)  {
	         fprintf( stderr,
		          "***** COLLECT OBARRAY symbol page type error %x\n",
			  symbol );
	         abort();
	      }
	      lp = T_U( lp )->pair.cdr;
	   }
	}
}

/* The following procedure verifies that a pointer is correct. */

static  check_ptr( tpp )
	TSCP  tpp;
{
	int  page;

	page = ADDRESS_PAGE( tpp );
	if  (((int) tpp) & 1)  {
	   if  (S2CPAGE( page ))  {
	      if  ((sc_pagegeneration[ page ] != sc_current_generation  &&
	     	    sc_pagegeneration[ page ] & 1)  ||
		   sc_pagetype[ page ] != TSCPTAG( tpp ))  {
	         pointererror( "%x fails check_ptr\n", T_U( tpp ) );
	      }
	   }
	   else  if  (TSCPTAG( tpp ) == PAIRTAG)  {
	      pointererror( "%x fails check_ptr\n", T_U( tpp ) );
	   }
	}
}

/* A page of objects is checked by the following procedure. */

static SCP  check_object( pp )
	SCP  pp;
{
	int  page, size, cnt, vpage;
	PATSCP  obj;

	page = ADDRESS_PAGE( pp );
	while  (ADDRESS_PAGE( pp ) == page  &&
		(pp != sc_extobjp  ||  sc_extobjwords == 0)  &&
		pp->unsi.gned != ENDOFPAGE)  {
	   moving_object = pp;
	   switch  ( pp->extendedobj.tag )  {
	      case  SYMBOLTAG:
	         check_ptr( pp->symbol.name );
		 vpage = ADDRESS_PAGE( pp->symbol.ptrtovalue );
		 check_ptr( *pp->symbol.ptrtovalue );
		 check_ptr( pp->symbol.propertylist );
		 size = SYMBOLSIZE;
		 break;

	      case  STRINGTAG:
	   	 size = STRINGSIZE( pp->string.length );
		 break;

	      case  VECTORTAG:
	         cnt = pp->vector.length;
		 obj = &pp->vector.element0;
		 while  (cnt--)  check_ptr( *obj++ );
		 size = VECTORSIZE( pp->vector.length );
		 break;	         

	      case  PROCEDURETAG:
	         check_ptr( pp->procedure.closure );
	   	 size = PROCEDURESIZE;
		 break;

	      case  CLOSURETAG:
	         check_ptr( pp->closure.closure );
		 cnt = pp->closure.length;
		 obj = &pp->closure.var0;
		 while  (cnt--)  check_ptr( *obj++ );
		 size = CLOSURESIZE( pp->closure.length );
		 break;

	      case  CONTINUATIONTAG:
	   	 check_ptr( pp->continuation.continuation );
		 size = CONTINUATIONSIZE( pp->continuation.length );
		 break;

	      case  FLOAT32TAG:
	   	 size = FLOAT32SIZE;
		 break;

	      case  FLOAT64TAG:
	   	 size = FLOAT64SIZE;
		 break;

	      case  WORDALIGNTAG:
		 size = WORDALIGNSIZE;
		 break;

	      default:
	         pointererror( "%x is not a valid extended object tag\n",
	         	       pp->extendedobj.tag );
	   }
	   pp = (SCP)( ((int*)pp)+size );
	}
	if  (ADDRESS_PAGE( pp ) == page  &&  pp == sc_extobjp  &&
	     sc_extobjwords != 0)
	   return( pp );
	return( NULL );
}

/* A page of pairs is checkled by the following procedure. */

static void  check_pairs( pp )
	SCP  pp;
{
	int  count;
	PATSCP  ptr;

	ptr = (PATSCP)pp;
	count = (PAGEBYTES/CONSBYTES)*2;
	while  (count--  &&
		(ptr != (PATSCP)sc_consp  ||  sc_conscnt == 0))  {
	   moving_object = (SCP)(((unsigned)ptr) & 0xfffffff8);
	   check_ptr( *ptr );
	   ptr++;
	 }
}

/* The following function can be called to check that all objects in the
   heap are valid.
*/

static void  check_heap( )
{
	int  i;

	/* Verify that all pages containing pairs are in good shape */
	for  (i = sc_firstheappage; i <= sc_lastheappage; i++)  {
	   if  ((sc_pagegeneration[ i ] == sc_current_generation  ||
		 ~sc_pagegeneration[ i ] & 1)  &&
		sc_pagegeneration[ i ] != 0)  {
	      if  (sc_pagetype[ i ] == PAIRTAG)  {
		 check_pairs( (SCP)PAGE_ADDRESS( i ) );
	      }
	      if  (sc_pagetype[ i ] == EXTENDEDTAG)  {
		 check_object( (SCP)PAGE_ADDRESS( i ) );
	      }
	   }
	}
	if  (pointer_errors)  abort();
}

/* Check the weakconsl for proper format. */

static void  check_weakconsl()
{
	TSCP  wl = weakconsl;

	while  (wl != EMPTYLIST)  {
	   check_ptr( wl );
	   check_ptr( WEAK_LINK( wl ) );
	   check_ptr( WEAK_CONS( wl ) );
	   check_ptr( WEAK_CAR( wl ) );
	   wl = WEAK_LINK( wl );
	}
	if  (pointer_errors)  abort();
}
	
/* Garbage collection is invoked to attempt to recover free storage when a
   request for storage cannot be met.  It will recover using a generational
   version of the "mostly copying" method.  See the .h file or the research
   reports for more details.
*/

TSCP  sc_collect_v;

TSCP  sc_collect()
{
	int  i, wasallocated;
	TSCP  constl, fl;

	if  (sc_collecting)  {
	   fprintf( stderr, "***** COLLECT Out of space during collection\n" );
	   abort();
	}
	sc_gcinprogress( 1 );
	sc_initiallink = ~OKTOSET;
	wasallocated = sc_allocatedheappages;

	if  (sc_gcinfo == 2)  {
	   /* Perform additional consistency checks */
	   check_heap();
	   check_obarray();
	   check_weakconsl();
	}
	if  (sc_gcinfo)  {
	   fprintf( stderr,
	   	    "\n***** COLLECT %d%% allocated (%d%% waste, %d MB) -> \n",
	   	    (wasallocated*100)/sc_heappages,
		    (sc_extwaste*100)/(sc_heappages*PAGEWORDS),
		    (sc_heappages*PAGEBYTES+ONEMB/2)/ONEMB );
	}
	getrusage( 0, &startru );

	/* Zero the current cons block, end the current extended block,
	   initialize sc_locklist, advance the generation.
	*/
	sc_conscnt = sc_conscnt+sc_conscnt;
	while  (sc_conscnt-- > 0)  {
	   *((int*)sc_consp) = 0;
	   sc_consp = (SCP)(((int*)sc_consp)+1);
	}
	sc_conscnt = 0;
	if  (sc_extobjwords)  {
	   sc_extobjp->unsi.gned = ENDOFPAGE;
	   sc_extobjwords = 0;
	}
	sc_extwaste = 0;
	sc_allocatedheappages = 0;
	sc_newlist = -1;
	sc_locklist = 0;
	sc_lockcnt = 0;
	sc_next_generation = INC_GENERATION( sc_current_generation );

	/* Hide the car's of pairs on the weakconsl. */
	save_weakconsl();
	
	/* Move the globals, display, and constants */
	for  ( i = 0; i < sc_globals->count; i++ )  {
	   move_ptr( *(sc_globals->ptrs[ i ]) );
	}
	for  ( i = 0; i < sc_maxdisplay; i++ )  move_ptr( sc_display[ i ] );
	for  ( i = 0; i < sc_constants->count; i++ )  {
	   move_ptr( *(sc_constants->ptrs[ i ]) );
	}

	/* Look into the stack and the registers and treat anything that
	   might be a pointer as a root and move it.
	*/
	trace_stack_and_registers();

	/* Lock down user program's frozen objects. */
	fl = sc__2afrozen_2dobjects_2a_v;
	while  (TSCPTAG( fl ) == PAIRTAG)  {
	   move_continuation_ptr( T_U( PAIR_CAR( fl ) ) );
	   fl = PAIR_CDR( fl );
	}

	/* Move new objects referenced in previous generations */
	move_the_generations();

	/* Continue moving the current generation until it terminates
	   and then handle weak pointers and unreferenced.
	*/
	move_the_heap();
	sc_allocatedheappages = sc_allocatedheappages+sc_lockcnt;

	/* Fully allocate partial pages */
	sc_conscnt = sc_conscnt+sc_conscnt;
	while  (sc_conscnt-- > 0)  {
	   *((int*)sc_consp) = 0;
	   sc_consp = (SCP)(((int*)sc_consp)+1);
	}
	sc_conscnt = 0;
	if  (sc_extobjwords)  {
	   sc_extobjp->unsi.gned = ENDOFPAGE;
	   sc_extobjwords = 0;
	}

	/* Correct pointers in the copied heap */
	correct_pointers( sc_newlist, sc_initiallink );

	/* Correct pointers in previous generations */
	correct_pointers( sc_genlist, 0 );

	/* Correct pointers in globals, display, and constants */
	for  ( i = 0; i < sc_globals->count; i++ )
	   *(sc_globals->ptrs[ i ]) = correct( *(sc_globals->ptrs[ i ]) );
	for  ( i = 0; i < sc_maxdisplay; i++ )
	   sc_display[ i ] = correct( sc_display[ i ] );
	for  ( i = 0; i < sc_constants->count; i++ )
	   *(sc_constants->ptrs[ i ]) = correct( *(sc_constants->ptrs[ i ]) );

	/* Copy back the locked objects and add locked pages to sc_genlist */
	sc_genlist = -1;
	copyback_locked_pages( sc_locklist );

	/* Step to the next odd generation */
	sc_next_generation = sc_current_generation = 
			     INC_GENERATION( sc_next_generation );	
	sc_generationpages = sc_generationpages+sc_allocatedheappages;
	sc_allocatedheappages = sc_generationpages;

	/* Finish up */
	getrusage( 0, &stopru );
	updategcru();
	if  (sc_gcinfo)  { 
	   fprintf( stderr,
	            "              %d%% locked  %d%% retained  %d user ms",
		    (sc_lockcnt*100)/sc_heappages,
	            (sc_generationpages*100)/sc_heappages,
		    stopru.ru_utime.tv_sec*1000+stopru.ru_utime.tv_usec/1000 );
	   fprintf( stderr,
	   	    "  %d system ms  %d page faults\n",
		    stopru.ru_stime.tv_sec*1000+stopru.ru_stime.tv_usec/1000,
		    stopru.ru_majflt );
	}
	if  (sc_gcinfo == 2)  {
	   /* Perform additional consistency checks */
	   check_heap();
	   check_obarray();
	   check_weakconsl();
	}

	/* Compact the whole heap if > sc_limit % of pages allocated */
	sc_initiallink = OKTOSET;
	sc_gcinprogress( 0 );
	if  ((sc_allocatedheappages*100)/sc_heappages > sc_limit)  {
	   sc_collect_2dall();
	   if  (sc_allocatedheappages > (sc_limit*sc_heappages*8)/1000)  {
	      MUTEXON;
	      sc_expandheap();
	      MUTEXOFF;
	   }
	}
	if  (sc_after_2dcollect_v != FALSEVALUE)
	   sc_apply_2dtwo( sc_after_2dcollect_v,
	   	 sc_cons( C_FIXED( sc_heappages*PAGEBYTES ),
			  sc_cons( C_FIXED( sc_allocatedheappages*PAGEBYTES ),
			           sc_cons( C_FIXED( sc_limit ),
				   	    EMPTYLIST ) ) ) );
	return( TRUEVALUE );
}

/* A complete garbage collection can be forced by calling the following
   procedure.
*/

TSCP  sc_collect_2dall_v;

TSCP  sc_collect_2dall()
{
	int  i,
	     save_sc_limit = sc_limit;

	MUTEXON;
	sc_limit = 100;
	if  (sc_generationpages != sc_allocatedheappages)  sc_collect();
	sc_limit = save_sc_limit;
	MUTEXOFF;
	MUTEXON;
	sc_next_generation = 
		INC_GENERATION( INC_GENERATION( sc_next_generation ) );
	sc_current_generation = sc_next_generation;
	for  (i = sc_firstheappage; i <= sc_lastheappage; i++)  {
	   if  (sc_pagegeneration[ i ] != 0  &&  ~sc_pagegeneration[ i ] & 1)
	      sc_pagegeneration[ i ] = sc_current_generation;
	}
 	sc_generationpages = 0;
	sc_genlist = -1;
	sc_limit = 100;
	sc_collect();
	sc_limit = save_sc_limit;
	MUTEXOFF;
	return( TRUEVALUE );
}

/* Pages in the heap are allocated by the following function.  It is called
   with a page count and sets the appropriate allocation pointers as
   required.  The sc_pagegeneration, sc_pagelink, sc_pagetype fields are
   set for each page here.  The garbage collector is invoked as needed.
*/

static int  allocatepage_failed = 0;	/* Set following collection, cleared on
				   	   successful allocation */

static  allocatepage( count, tag )
	int  count, tag;
{
	int  start, page, freecnt, generation;

	if  ((count+sc_allocatedheappages) > sc_heappages/2)  {
failed:
	   if  ((allocatepage_failed  ||  sc_collecting)  &&
	        sc_expandheap() == 0)  {
	      fprintf( stderr,
		       "***** ALLOCATEPAGE cannot allocate %d bytes",
	   	       count*PAGEBYTES );
	      fprintf( stderr, " with %d %% of heap allocated\n",
	   	       (sc_allocatedheappages*100)/sc_heappages );
	   exit( 1 );
	   }
	   if  (sc_collecting == 0)  sc_collect();
	   allocatepage_failed = 1;
	   return;
	}
	start = sc_freepage;
	freecnt = 0;
	do  {
	   generation = sc_pagegeneration[ sc_freepage ];
	   if  (generation & 1  &&  generation != sc_current_generation)  {
	      if  (freecnt == 0)  page = sc_freepage;
	      freecnt++;
	   }
	   else
	      freecnt = 0;
	   if  (sc_freepage == sc_lastheappage)  {
	      if  (freecnt != count)  freecnt = 0;
	      sc_freepage = sc_firstheappage;
	   }
	   else  sc_freepage++;
	   if  (sc_freepage == start)  goto failed;
	}  while  (count != freecnt);
	allocatepage_failed = 0;
	sc_allocatedheappages = sc_allocatedheappages+count;
	sc_pagegeneration[ page ] = sc_next_generation;
	sc_pagetype[ page ] = tag;
	sc_pagelink[ page ] = sc_initiallink;
	if  (tag == PAIRTAG)  {
	   sc_conscnt = PAGEBYTES/CONSBYTES;
	   sc_consp = (SCP)PAGE_ADDRESS( page );
	   if  (sc_collecting)  QUEUE_PAGE( cons_pages, page );
	}
	else  {
	   sc_extobjp = (SCP)PAGE_ADDRESS( page );
	   sc_extobjwords = count*PAGEWORDS;
	   if  (sc_collecting)  QUEUE_PAGE( extended_pages, page );
	   while (--count)  {
	      sc_pagegeneration[ ++page ] = sc_next_generation;
	      sc_pagetype[ page ] = BIGEXTENDEDTAG;
	      sc_pagelink[ page ] = sc_initiallink;
	   }
	}	
}

/* When a pointer to a new object may be stored in a old page, the following
   procedure is called to add the old page to the list of changed older pages
   and then do the assignment.  N.B.  set-top-level-value! may set global
   values outside the heap.
*/

TSCP  sc_setgeneration( a, b )
	TSCP* a;
	TSCP  b;
{
	int  oldpage = ADDRESS_PAGE( a );

	MUTEXON;
	if  (S2CPAGE( oldpage )  &&  sc_pagelink[ oldpage ] == 0)  {
	   if  (sc_pagetype[ oldpage ] == PAIRTAG)  {
	      if  (sc_pagegeneration[ oldpage ] == sc_current_generation)  {
	         sc_pagelink[ oldpage ] = OKTOSET;
	      }
	      else  {
	         sc_pagelink[ oldpage ] = sc_genlist;
	         sc_genlist = oldpage;
	      }
	   }
	   else  {
	      while  (sc_pagetype[ oldpage ] == BIGEXTENDEDTAG)  oldpage--;
	      if  (sc_pagegeneration[ oldpage ] == sc_current_generation)  {
	         sc_pagelink[ oldpage ] = OKTOSET;
	      }
	      else  {
	         sc_pagelink[ oldpage ] = sc_genlist;
	         sc_genlist = oldpage;
	      }
	      while  (++oldpage < sc_lastheappage  &&  
		      sc_pagetype[ oldpage ] == BIGEXTENDEDTAG)  {
	         sc_pagelink[ oldpage ] = OKTOSET;
	      }
	   }
	}
	*a = b;
	MUTEXOFF;
	return( b );
}
	
/* Heap based storage is allocated by the following function.  It is called
   with a word count and a value to put in the first word.  It will return
   an UNTAGGED pointer to the storage.  Note that the minimum permissible
   allocation size is two words.

   N.B.  IT IS THE CALLER'S RESPONSIBILITY TO ASSURE THAT SIGNALS DO NOT
	 CAUSE PROBLEMS DURING ALLOCATION.
*/

SCP  sc_allocateheap( wordsize, tag, rest )
	int  wordsize, tag, rest;
{
	SCP  alloc;
	int  isastring = (tag == STRINGTAG);

	EVEN_EXTOBJP( tag == FLOAT64TAG );
	ODD_EXTOBJP( isastring );
	if  (wordsize <= sc_extobjwords)  {
	   alloc = sc_extobjp;
	   sc_extobjp = (SCP)(((int*)alloc)+wordsize);
	   sc_extobjwords = sc_extobjwords-wordsize;
	}
	else  if  (wordsize < PAGEWORDS)  {
	   while  (wordsize > sc_extobjwords)  {
	      sc_extwaste = sc_extwaste+sc_extobjwords;
	      if  (sc_extobjwords)  sc_extobjp->unsi.gned = ENDOFPAGE;
	      sc_extobjwords = 0;
	      allocatepage( 1, EXTENDEDTAG );
	      EVEN_EXTOBJP( tag == FLOAT64TAG );
	      ODD_EXTOBJP( isastring );
	   }
	   alloc = sc_extobjp;
	   sc_extobjwords = sc_extobjwords-wordsize;
	   sc_extobjp = (SCP)(((int*)alloc)+wordsize);
	}
	else  {
	   while  (wordsize > sc_extobjwords)  {
	      sc_extwaste = sc_extwaste+sc_extobjwords;
	      if  (sc_extobjwords)  sc_extobjp->unsi.gned = ENDOFPAGE;
	      sc_extobjwords = 0;
	      allocatepage( (wordsize+PAGEWORDS-1+isastring)/PAGEWORDS,
	      		    EXTENDEDTAG );
	   }
	   ODD_EXTOBJP( isastring );
	   alloc = sc_extobjp;
	   sc_extobjp = NULL;
	   sc_extobjwords = 0;
	}
	alloc->extendedobj.tag = tag;
	alloc->extendedobj.rest = rest;
	return( alloc );
}

/* 32-bit floating point numbers are constructed by the following function.  It
   is called with a 32-bit floating point value and it returns a pointer to
   the Scheme object with that value.
*/

TSCP sc_makefloat32( value )
	float  value;
{
	SCP  pp;

	MUTEXON;
	if  (sc_extobjwords >= FLOAT32SIZE)  {
	   pp = sc_extobjp;
	   sc_extobjp = (SCP)(((int*)sc_extobjp)+FLOAT32SIZE);
	   sc_extobjwords = sc_extobjwords-FLOAT32SIZE;
	   pp->unsi.gned = FLOAT32TAG;
	}
	else
	   pp = sc_allocateheap( FLOAT32SIZE, FLOAT32TAG, 0 );
	pp->float32.value = value;
	MUTEXOFF;
	return( U_T( pp, EXTENDEDTAG ) );
}

/* 64-bit floating point numbers are constructed by the following function.  It
   is called with a 64-bit floating point value and it returns a pointer to
   the Scheme object with that value.
*/

TSCP sc_makefloat64( value )
	double  value;
{
	SCP  pp;

	MUTEXON;
	EVEN_EXTOBJP( 1 );
	if  (sc_extobjwords >= FLOAT64SIZE)  {
	   pp = sc_extobjp;
	   sc_extobjp = (SCP)(((int*)sc_extobjp)+FLOAT64SIZE);
	   sc_extobjwords = sc_extobjwords-FLOAT64SIZE;
	   pp->unsi.gned = FLOAT64TAG;
	}
	else
	   pp = sc_allocateheap( FLOAT64SIZE, FLOAT64TAG, 0 );
	pp->float64.value = value;
	MUTEXOFF;
	return( U_T( pp, EXTENDEDTAG ) );
}

/* The following function forms a dotted-pair with any two Scheme pointers.  It
   returns a tagged pointer to the pair as its value.
*/

TSCP  sc_cons_v;

TSCP  sc_cons( x, y )
	TSCP x, y;
{
	SCP  oconsp;

	MUTEXON;
retry:
	if  (sc_conscnt > 0)  {
	   oconsp = sc_consp;
	   sc_consp->pair.car = x;
	   sc_consp->pair.cdr = y;
	   sc_consp = (SCP)(((int*)sc_consp)+2);
	   sc_conscnt--;
	   MUTEXOFF;
	   return( U_T( oconsp, PAIRTAG ) );
	}
	allocatepage( 1, PAIRTAG );
	goto retry;
}

/* The following function forms a weak dotted-pair with any two Scheme
   pointers.  A weak dotted-pair is a pair that has the property that the CAR
   of the pair may be set to #F by the garbage collector if it contains the
   only pointer to an object.
*/

TSCP  sc_weak_2dcons_v;

TSCP  sc_weak_2dcons( x, y )
	TSCP  x, y;
{
	TSCP  cons,	/* cons cell holding x & y */
	      weak;	/* 3 element "weak" string: link, cons, car */

	cons = sc_cons( x, y );
	weak = MAKE_WEAK;
	WEAK_LINK( weak ) = weakconsl;
	WEAK_CONS( weak ) = cons;
	WEAK_CAR( weak ) = FALSEVALUE;
	weakconsl = weak;
	return( cons );
}
