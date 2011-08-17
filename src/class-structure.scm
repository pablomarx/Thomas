;*              Copyright 1992 Digital Equipment Corporation
;*                         All Rights Reserved
;*
;* Permission to use, copy, and modify this software and its documentation is
;* hereby granted only under the following terms and conditions.  Both the
;* above copyright notice and this permission notice must appear in all copies
;* of the software, derivative works or modified versions, and any portions
;* thereof, and both notices must appear in supporting documentation.
;*
;* Users of this software agree to the terms and conditions set forth herein,
;* and hereby grant back to Digital a non-exclusive, unrestricted, royalty-free
;* right and license under any changes, enhancements or extensions made to the
;* core functions of the software, including but not limited to those affording
;* compatibility with other hardware or software environments, but excluding
;* applications which incorporate this software.  Users further agree to use
;* their best efforts to return to Digital any such changes, enhancements or
;* extensions that they make and inform Digital of noteworthy uses of this
;* software.  Correspondence should be provided to Digital at:
;* 
;*			Director, Cambridge Research Lab
;*			Digital Equipment Corp
;*			One Kendall Square, Bldg 700
;*			Cambridge MA 02139
;* 
;* This software may be distributed (but not offered for sale or transferred
;* for compensation) to third parties, provided such third parties agree to
;* abide by the terms and conditions of this notice.  
;* 
;* THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
;* WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
;* MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
;* CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;* DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;* PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
;* ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;* SOFTWARE.

; $Id: class-structure.scm,v 1.4 1992/09/20 08:42:02 birkholz Exp $

;;;; Build the class structure for Dylan runtime environment.

(define <object>
  (make-dylan-class '<object> '() '() #T))

;;;; Number classes

;;; Abstract

(define <number>
  (make-dylan-class '<number> (list <object>) '() #F))

;;; Sealed

(define <complex>
  (make-dylan-class '<complex> (list <number>) '() #F))
(define <real>
  (make-dylan-class '<real> (list <complex>) '() #F))
(define <rectangular-complex>
  (make-dylan-class '<rectangular-complex> (list <complex>) '() #F))
(define <rational>
  (make-dylan-class '<rational> (list <real>) '() #F))
(define <integer>
  (make-dylan-class '<integer> (list <rational>) '() #F))
(define <ratio>
  (make-dylan-class '<ratio> (list <rational>) '() #F))
(define <float>
  (make-dylan-class '<float> (list <real>) '() #F))
(define <single-float>
  (make-dylan-class '<single-float> (list <float>) '() #F))
(define <double-float>
  (make-dylan-class '<double-float> (list <float>) '() #F))
(define <extended-float>
  (make-dylan-class '<extended-float> (list <float>) '() #F))

;;;; Collections

;;; Abstract

(define <collection>
  (make-dylan-class '<collection> (list <object>) '() #F)) 
(define <explicit-key-collection>
  (make-dylan-class '<explicit-key-collection> (list <collection>) '() #F))
(define <mutable-collection>
  (make-dylan-class '<mutable-collection> (list <collection>) '() #F))
(define <sequence>
  (make-dylan-class '<sequence> (list <collection>) '() #F))
(define <mutable-explicit-key-collection> 
  (make-dylan-class '<mutable-explicit-key-collection>
		    (list <explicit-key-collection> 
			  <mutable-collection>) '() #F))
(define <mutable-sequence>
  (make-dylan-class '<mutable-sequence>
		    (list <mutable-collection> <sequence>) '() #F))

;;; Instantiable

(define <array>
  (make-dylan-class '<array> (list <mutable-explicit-key-collection>) '() #F))
(define <table>
  (make-dylan-class '<table> (list <mutable-explicit-key-collection>) '() #F))
(define <vector>
  (make-dylan-class '<vector> (list <array> <mutable-sequence>) '() #F))
(define <string>
  (make-dylan-class '<string> (list <mutable-sequence>) '() #F))
(define <deque>
  (make-dylan-class '<deque> (list <mutable-sequence>) '() #F))
(define <range>
  (make-dylan-class '<range> (list <sequence>) '() #F))
(define <stretchy-vector>
  (make-dylan-class '<stretchy-vector> (list <vector>) '() #F))

;;; Sealed

(define <simple-object-vector>
  (make-dylan-class '<simple-object-vector> (list <vector>) '() #F))
(define <unicode-string>
  (make-dylan-class '<unicode-string> (list <vector> <string>) '() #F))
(define <byte-string>
  (make-dylan-class '<byte-string> (list <vector> <string>) '() #F))
(define <list>
  (make-dylan-class '<list> (list <mutable-sequence>) '() #F))
(define <pair>
  (make-dylan-class '<pair> (list <list>) '() #F))
(define <empty-list>
  (make-dylan-class '<empty-list> (list <list>) '() #F))

;;;; Conditions

(define <condition>
  (make-dylan-class '<condition> (list <object>) '() #F)) 
(define <serious-condition>
  (make-dylan-class '<serious-condition> (list <condition>) '() #F)) 
(define <warning>
  (make-dylan-class '<warning> (list <condition>) '() #F)) 
(define <restart>
  (make-dylan-class '<restart> (list <condition>) '() #F)) 
(define <error>
  (make-dylan-class '<error> (list <serious-condition>) '() #F)) 
(define <simple-error>
  (make-dylan-class '<simple-error> (list <error>) '() #F)) 
(define <type-error>
  (make-dylan-class '<type-error> (list <error>) '() #F)) 
(define <simple-warning>
  (make-dylan-class '<simple-warning> (list <warning>) '() #F)) 
(define <simple-restart>
  (make-dylan-class '<simple-restart> (list <restart>) '() #F)) 
(define <abort>
  (make-dylan-class '<abort> (list <restart>) '() #F)) 

;;;; Others

(define <function>			; Abstract
  (make-dylan-class '<function> (list <object>) '() #F))
(define <generic-function>		; Instantiable
  (make-dylan-class '<generic-function> (list <function>) '() #F))
(define <method>			; Abstract
  (make-dylan-class '<method> (list <function>) '() #F))
(define <class>				; Abstract
  (make-dylan-class '<class> (list <object>) '() #F))
(define <slot-descriptor>		; Abstract
  (make-dylan-class '<slot-descriptor> (list <object>) '() #F))
(define <singleton>			; Abstract
  (make-dylan-class '<singleton> (list <object>) '() #F))
(define <symbol>			; Instantiable
  (make-dylan-class '<symbol> (list <object>) '() #F))
(define <keyword>			; Instantiable
  (make-dylan-class '<keyword> (list <object>) '() #F))
(define <character>			; Instantiable
  (make-dylan-class '<character> (list <object>) '() #F))
