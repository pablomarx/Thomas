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

/* The following procedures implement CALL-WITH-CURRENT-CONTINUATION.
   CALLCCCONTINUING is the function that is executed when a continuation is
   applied.  It is called with the result to be returned and the procedure's
   closure which is the continuation created by the initial call to
   TSC_CALLWITHCURRENTCONTINUATION.  It will unwind the stack until the right
   return point is found.  If it is not found, then it will restore the stack
   from the continuation(s).  Once the stack is known to have the right
   contents, it will restore the correct state with longjmp.
*/

/* External declarations */

#include "objects.h"
#include "scinit.h"
#include "heap.h"
#include "callcc.h"
#include "apply.h"
#include "signal.h"
#ifdef MIPS
extern  sc_setsp();
#endif

#ifdef VAX
#define  longjmp( x, y )	sc_longjmp( x, y )
#define  setjmp( x )		sc_setjmp( x )
#endif

extern  TSCP  dynwind_new_2dcall_2fcc();

TSCP  sc_clink;		/* Pointer to inner most continuation on stack. */

/* Static declarations for data structures internal to the module.  These
   variables may be static as they are only used under MUTEX. */

static TSCP  callccresult,	/* Passes result across longjmp. */
      	     callcccp;		/* Preserves cp during stack rebuilding. */

static int  *fp,		/* Temps for constructing continuation */
            *tp,
            *tos,
            rcount,
            count;

static  callcccontinuing( result, cp )
	TSCP  result, cp;
{
	MUTEXON;
	callccresult = result;
	callcccp = cp;
	/* Unwind CLINK to see if this continuation is currently on the
	   stack.  */
	while (sc_clink != EMPTYLIST)  {
	   if  (sc_clink == cp)
	      longjmp( (T_U(cp))->continuation.savedstate, 1 );
	   sc_clink = (T_U(sc_clink))->continuation.continuation;
	}
	/* Continuation is not currently on the stack, so transfer to it and
           it will restore the stack.  */
#ifdef MIPS
	sc_setsp( (T_U(callcccp))->continuation.address );
#endif
	longjmp( (T_U(callcccp))->continuation.savedstate, 1 );
}

/* Use the call-with-current-continuation provided by dynamic-wind.  Make the
   old call-with-current-continuation available for use from dynamic-wind.
*/

TSCP  sc_ntinuation_1af38b9f_v;

TSCP  sc_ntinuation_1af38b9f( function )
	TSCP  function;
{
	return( dynwind_new_2dcall_2fcc( function ) );
}

TSCP  sc_old_2dcall_2fcc( function )
	TSCP  function;
{
	SCP  cp;		/* Pointer to the continuation */
	int  *save_fp,		/* Save static values across heap allocate */
	     save_count;

	MUTEXON;
	if (sc_clink == EMPTYLIST)
	   fp = sc_stackbase;
	else
	   fp = (T_U(sc_clink))->continuation.address;
	count = ((unsigned)(fp)-(unsigned)(STACKPTR))/4;
	save_fp = fp;
	save_count = count;
	cp = sc_allocateheap( NULLCONTINUATIONSIZE+count+2+sc_maxdisplay,
			      CONTINUATIONTAG,
		              NULLCONTINUATIONSIZE+count+sc_maxdisplay );
	fp = save_fp;
	count = save_count;
	tos = STACKPTR;	
	cp->continuation.continuation = sc_clink;
	cp->continuation.stacktrace = sc_stacktrace;
	sc_clink = U_TX( cp );
	cp->continuation.address = tos;
	tp = &cp->continuation.word0;
	rcount = sc_maxdisplay;
	while  (rcount--)  *tp++ = (int)sc_display[ rcount ];
	while  (count--)  *tp++ = *tos++;
	MUTEXOFF;
	if  (setjmp( cp->continuation.savedstate ) == 0)  {
	   callccresult = sc_apply_2dtwo( function,
	   			    sc_cons( sc_makeprocedure( 1, 0,
							      callcccontinuing,
						              U_TX( cp ) ),
					     EMPTYLIST ) );
	   sc_clink = T_U( sc_clink )->continuation.continuation;
	   return( callccresult );
	}
	/* Return here when the continuation is invoked. */
	if  (sc_clink == EMPTYLIST)  {
	   sc_clink = callcccp;
	   while  (sc_clink != EMPTYLIST)  {
	      tp = (T_U(sc_clink))->continuation.address;
	      fp = &(T_U(sc_clink))->continuation.word0+sc_maxdisplay;
	      count = (T_U(sc_clink))->continuation.length-sc_maxdisplay-
	      				NULLCONTINUATIONSIZE;
	      while  (count--)  *tp++ = *fp++;
	      sc_clink = (T_U(sc_clink))->continuation.continuation;
	   }
	}
	tp = &T_U( callcccp )->continuation.word0;
	rcount = sc_maxdisplay;
	while  (rcount--)  sc_display[ rcount ] = (TSCP)(*tp++);
	sc_clink = T_U( callcccp )->continuation.continuation;
	sc_stacktrace = T_U( callcccp )->continuation.stacktrace;
	/* Move result onto the stack under mutex */
	function = callccresult;
	MUTEXOFF;
	return( function );
}

