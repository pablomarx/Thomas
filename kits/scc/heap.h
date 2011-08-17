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

/* Import definitions */

#ifndef rusage
#include <time.h>
#include <sys/resource.h>
#endif

/* This module implements the object storage storage system for SCHEME->C.

   Unlike most Lisp systems, it is not intended that SCHEME->C provide a
   "one language" environment divorced from other programming languages.
   Instead, it is intended that SCHEME->C co-exist with other languages
   and share their development tools and runtime environment.

   Nor, is it intended that SCHEME->C have detailed knowledge and intimate
   control over the hardware.  Instead of generating actual instructions,
   it generates C intermediate language.

   By adhering to these two design goals, SCHEME->C can be a powerful tool
   for delivering Lisp based technology to non-Lisp environments.  However,
   these design goals also place some significant constraints on the design
   of the storage system.  For example:

   1.  The system must tolerate both Scheme and non-Scheme storage and
       data types.

   2.  The system will not have any control over register allocation or
       instruction sequences.

   3.  In examining register or stack contents, one can make a statement that
       something is not a pointer, but one cannot say that something is a
       pointer.  At best, one can say that it looks exactly like a pointer.

   Given these constraints, a conventional "stop-and-copy" garbage collector
   cannot be used.  Instead, a storage allocation method called
   "mostly-copying" is used (see WRL Research Report 88/2, Compacting Garbage
   Collection with Ambiguous Roots).

   In its simplist form, the algorithm works as follows.  The heap is divided
   into pages (which need not be the same size as the processor's page).
   Objects are allocated entirely within a page, or in a dedicated set of
   pages.  When half the storage in the heap has been allocated, the garbage
   collector is invoked.

   Garbage collection is divided into three phases.  The first is the a copy
   phase similar to that of the Minsky-Fenichel-Yochelson-Chaeny-Arnborg
   collector.  Items will be copied from the oldspace (pages in the current
   generation) to the newspace (pages in the next generation).  Indirect
   pointers to new objects will be placed in the old objects, but pointers
   to new objects are never stored in new objects.
   
   During this phase, the contents of continuations (including the current
   continuation which is in the registers and the stack) get special
   processing.  Each word in them is examined to see if it might be a pointer.
   If it is a pointer, then the object that it points to is copied, and the
   page is marked as locked.

   Thus at the end of the phase, all accessibile storage has been copied, and
   all pointers are indirect through the old space.  All pages which have items
   which must be left in place are marked as locked.

   The next phase is the correction phase which turns all indirect pointers
   into their correct values.  At the end of this phase, all pointers will
   point to the correct place, but items which were locked will be located in
   the newspace.

   The final phase is the copy back phase where items that are locked are
   copied back from the newspace to their correct position in the locked
   pages.  At this time, locked pages are unlocked and promoted to newspace.

   At this point, garbage collection is done and the generation number is
   advanced.  As with the classical "stop-and-copy" algorithm, the time used
   is proportional to the amount of storage retained, rather than the total
   amount of storage.  It needs somewhat more storage as it must retain locked
   pages, and has duplicate copies of items of locked pages.

   In order to avoid repeated copying of retained data, the collector
   implements a generational version of the algorithm.  Objects that survive
   a collection are retained and not moved until more than SCLIMIT of the heap
   is allocated following a collection.  At this point, the entire heap is
   collected (see WRL Technical Note TN-12, Mostly-Copying Garbage Collection
   Picks Up Generations and C++).

   A few simple changes to the previously described algorithm result in a
   generational collector.  Even generation numbers represent retained storage
   and storage is always allocated out of an odd numbered generation when the
   user program is executing.  During garbage collection, all retained
   objects in the odd generation are copied into a new even numbered space.
   During this copy phase, pointers into an object in an even numbered space
   need not be followed.  A total collection is done by changing the space
   number on all even numbered pages to the current odd generation and then
   doing a collection.

   In order for a generational scheme to work, all stores of pointers to new
   objects in old pages must be detected.  This is done by explicit checks
   in:  SET-CAR!, SET-CDR!, VECTOR-SET!, SET!, SET-TOP-LEVEL-VALUE!, PUTPROP,
   and SCHEME-TSCP-SET!.  While at first glance, explicit checks seem a slow
   way of doing things, the reduction in copying more than makes up for them.

   The garbage collector may be configured by the user setting any of the
   following environment variables:

	name:	 range:		default:	action:

   	SCHEAP	 [1:1000]	8		Number of megabytes to
						initially allocate for the heap
						(total).

	SCMAXHEAP [SCHEAP:1000]	SCHEAP		Number of megabytes to allow
						the heap to grow to.

	SCLIMIT	 [10:45]	33		Cause of total collection of
						the heap when more than this %
						of the heap is allocated
						following a generational
						collection.

	SCGCINFO [0:2]		0		C boolean indicating that
						garbage collection statistics
						should be printed on stderr.

						When set to 2, additional
						debugging information is
						printed and additional tests
						are done.
*/

/* Page related definitions.  The page size is defined as a power of 2, where
   2**PAGEPOWER = PAGEBYTES.
*/

#define PAGEPOWER	9		/* 512 bytes/page */
#define PAGEBYTES	(1<<PAGEPOWER)
#define PAGEWORDS	(PAGEBYTES/4)
#define ONEMB		1048576
#define PAGEBIT		PAGEPOWER
#define PAGEBITLEN	(32-PAGEPOWER)

/* Page number to address conversion is handled by the following defines */

#define ADDRESS_PAGE( adr )   ((int) (((unsigned)(adr)) >> PAGEBIT))
#define PAGE_ADDRESS( page )  ((page) << PAGEBIT)
#define ADDRESS_OFFSET( adr ) (((int)(adr)) & (PAGEBYTES-1))

/* Each page in the pool has the following flags associated with it:
   
	PAGEGENERATION	generation number associated with the page.  Even
			numbered generations are objects that survived a
			garbage collection.  Odd numbered generations are
			where storage is allocated during the execution of the
			user's program.  A zero value indicates that the page
			is not available for garbage collection.

	PAGETYPE	tag field indicating the type of data stored in the
			page.  It is either PAIRTAG, EXTENDEDTAG, or
			BIGEXTENDEDTAG.

	PAGELOCK	boolean indicating whether or not the page is locked
			by the garbage collector.

	PAGELINK	next page (or 0) of the lock list whose head is kept in
			LOCKLIST, and length in LOCKCNT (only during gc).
				-or-
			OKTOSET or ~OKTOSET (-1 or 0) indicating status of
			a just allocated page (value of INITIALLINK).
				-or-
			next page (or -1) of the GENLIST, whose head is kept
			in GENLIST.  This list contains all pages in older
			generations that might contain a pointer to a newer
			generation.

			If this value is non-zero, then it is possible to set
			pointers in the page without going through
			sc_setgeneration.

   It is possible to pack these fields into 1-2 words, but this has not been
   done.

   Objects which are longer than one page are allocated on an integral number
   of pages.  Pages other than the head are marked with a BIGEXTENDEDTAG in
   pagetype field to indicate that they are related to the previous page.

   CURRENT_GENERATION holds the generation number that is presently being
   allocated.  NEXT_GENERATION holds the obvious during garbage collection.

   N.B.:  There may be pages in the range sc_firstheappage through
	  sc_lastheappage that are not managed by the garbage collector.  This
	  is why the 0 flag is used in PAGEGENERATION.
*/

extern int  *sc_pagegeneration,
	    *sc_pagetype,
	    *sc_pagelock,
	    *sc_pagelink,
	    sc_initiallink,
	    sc_locklist,
	    sc_genlist,
	    sc_lockcnt,
	    sc_current_generation,
	    sc_next_generation;

#define INC_GENERATION( g ) (g + 1)  /* 1 collection/second will take over 32
					years to overflow g */
#define NEXTPAGE( page ) ((page==sc_lastheappage) ? sc_firstheappage : page+1)
#define BIGEXTENDEDTAG -1
#define OKTOSET -1
#define S2CPAGE( p ) (p >= sc_firstheappage &&  p <= sc_lastheappage  && \
		      sc_pagegeneration[ p ] != 0)
#define NOT_S2CPAGE( p ) (p < sc_firstheappage ||  p > sc_lastheappage  || \
		          sc_pagegeneration[ p ] == 0)

extern int  sc_firstheappage,		/* first page in the Scheme heap */
	    sc_lastheappage,		/* last page in the Scheme heap  */
	    sc_limit,			/* % of heap allocated after collecton
					   that forces total collection */
	    sc_freepage,		/* Free page index */
	    sc_heappages,		/* # of pages in the Scheme heap */
	    sc_maxheappages,		/* Maximum # of pages in Scheme heap */
	    sc_allocatedheappages,	/* # of pages currently allocated */
	    *sc_firstheapp,		/* ptr to first word in the heap */
	    *sc_lastheapp;		/* ptr to last word in the heap */

/* In order to speed up allocation of CONS cells, blocks of potential CONS
   cells are preallocated.  CONSP points to the next free cell, and CONSCNT
   is the number of free cells left.
*/

extern int  sc_conscnt;
extern SCP  sc_consp;

/* In order to speed up allocation of extended objects, EXTOBJWORDS is the
   number of words available in the current page pointed to by EXTOBJP.
   EXTWASTE keeps track of the number of words lost because of page alignment
   problems.  When only a part of the page is used, the first unused word is
   marked with ENDOFPAGE.
*/

extern int  sc_extobjwords,
	    sc_extwaste;
extern SCP  sc_extobjp;

#define ENDOFPAGE 0xAAAAAAAA

/* Some implementations require extended storage always be allocated so that
   double objects in it are on double word boundaries (addr mod 8 = 0).  This
   is handled by the following define that is used to force pointer alignment.
*/

#ifdef DOUBLE_ALIGN
#define ODD_EXTOBJP( e )  if  ((e) && sc_extobjwords &&\
				   (sc_extobjwords & 1) == 0)  {\
	   			     sc_extobjp->unsi.gned = WORDALIGNTAG;\
	   			     sc_extobjp = (SCP)(((int*)sc_extobjp)+1);\
	   			     sc_extobjwords = sc_extobjwords-1;\
				   }
#define EVEN_EXTOBJP( e )  if  ((e) && sc_extobjwords & 1)  {\
	   			     sc_extobjp->unsi.gned = WORDALIGNTAG;\
	   			     sc_extobjp = (SCP)(((int*)sc_extobjp)+1);\
	   			     sc_extobjwords = sc_extobjwords-1;\
				   }
#endif
#ifndef DOUBLE_ALIGN
#define ODD_EXTOBJP( e )
#define EVEN_EXTOBJP( e )
#endif

/* A running total of garbage collection resource usage in kept in GCRU.
   
   Garbage collection statistics are printed on stderr following each
   collection when SCGCINFO is true (set by the environment variable SCGCINFO,
   or by the command line flag -scgc, default = 0).
*/

extern int  sc_gcinfo;

/* Garbage collection and call-with-current-continuation need to know the
   base of the stack, i.e. the value of the stack pointer when the stack is
   empty.  It is computed at initialization time and stored in SC_STACKBASE.
   STACKPTR is the address of the current top of stack.
*/

extern int  *sc_stackbase;

#ifdef MIPS
#define STACKPTR sc_processor_register( 29 )
#endif

#ifdef TITAN
extern int  *zzReadRegister();
#define STACKPTR (zzReadRegister( 63 )+1)
#endif

#ifdef VAX
#define STACKPTR sc_processor_register( 14 )
#endif

/* Some objects require cleanup actions when they are freed.  For example,
   when a file port is recovered, the corresponding file needs to be closed.
   Such objects are noted by the procedure (WHEN-UNREFERENCED object action),
   where object is any Scheme object and action is either #F indicating that
   nothing should be done, or a procedure that takes one argument.  When a
   procedure is supplied, it will be called when a garbage collection occurs
   and there are no references to that object.  In order to implement this
   function, the runtime system will keep two alists, SC_WHENFREED and
   SC_FREED.  The first list is those items requiring cleanup when they
   become free, and the second list is those items freed that require
   cleanup now.
*/

extern TSCP  sc_whenfreed,
	     sc_freed;

/* A Scheme program can register a callback with the garbage collector that
   will be called following each collection.  This is done by setting the
   value of AFTER-COLLECT to a procedure that takes three arguments:  the
   heap size in bytes, the current allocation in bytes, and the percent of
   allocation that will force a total collection.
*/

extern TSCP  sc_after_2dcollect_v;

/* Objects on the *FROZEN-OBJECTS* list are never moved by the garbage
   collector.  User programs can use this to "lock" objects that are passed to
   other languages.
*/

extern TSCP  sc__2afrozen_2dobjects_2a_v;

/* The procedural interfaces to this module are:  */

extern int  *sc_processor_register();

extern TSCP  sc_my_2drusage_v;

extern TSCP  sc_my_2drusage();

extern TSCP  sc_collect_2drusage_v;

extern TSCP  sc_collect_2drusage();

extern TSCP  sc_collect_v;

extern TSCP  sc_collect();

extern TSCP  sc_collect_2dall_v;

extern TSCP  sc_collect_2dall();

extern TSCP  sc_setgeneration();

extern SCP  sc_allocateheap();

extern TSCP  sc_makefloat32();

extern TSCP  sc_makefloat64();

extern TSCP  sc_cons_v;

extern TSCP  sc_cons();

extern TSCP  sc_weak_2dcons_v;

extern TSCP  sc_weak_2dcons();
