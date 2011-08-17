;;;; -*- Scheme -*- isn't Thomas (or Dylan(TM))
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
;*                      Director, Cambridge Research Lab
;*                      Digital Equipment Corp
;*                      One Kendall Square, Bldg 700
;*                      Cambridge MA 02139
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

; $Id: dylan-examples.dyl,v 1.13 1992/09/12 01:17:29 jmiller Exp $

;;; This is a copy of examples-from-book.text modified to be runnable in
;;; the Thomas REP.  The expected return value is given after ";Value: ".
;;; The book doesn't always show return values, so this value is what you
;;; can expect from Thomas.  Printed output appears after ";" before
;;; ";Value: ".  Error messages are quoted from the book.  Thomas doesn't
;;; report errors in exactly the same way.  Definitions added to make the
;;; example work are flagged by ";;; +".  Notes about differences from the
;;; examples as given in the book are prefixed by ";;; ".


;; Page 27

"abc"
;Value: "abc"

123
;Value: 123

foo:
;Value: foo:

#\a
;Value: #\a

#t
;Value: #t

#f
;Value: #f

(quote foo)
;Value: foo

'foo
;Value: foo

'(1 2 3)
;Value: (1 2 3)


;; Page 28

;;; +
(define-class <window> (<object>))
;Value: <window>

<window>
;Value: #[dylan-class ...]

concatenate
;Value: #[generic function ...]

(define my-variable 25)
;Value: my-variable

my-variable
;Value: 25

(bind ((x 50))
  (+ x x))
;Value: 100

(setter element)
;Value: #[generic function ...]

(define (setter my-variable) 20)
;Value: (setter my-variable)

(setter my-variable)
;Value: 20


;; Page 29

(+ 3 4)
;Value: 7

(* my-variable 3)
;Value: 75

(* (+ 3 4) 5)
;Value: 35

((if #t + *) 4 5)
;Value: 9


;; Page 30

; Creates and initializes a module variable
(define my-variable 25)
;Value: my-variable

; Sets the value to 12
(set! my-variable 12)
;Value: 12

; Returns 30.  Uses lexical variables x and y.
(bind ((x 10) (y 20))
  (+ x y))
;Value: 30

; Creates an anonymous method, which expects 2 numeric arguments.
(method ((a <number>) (b <number>))
  (list (- a b) (+ a b)))
;Value: #[method ..]

(values 1 2 3)
;Value[1]: 1
;Value[2]: 2
;Value[3]: 3

(define-method edges ((center <number>) (radius <number>))
  (values (- center radius) (+ center radius)))
;Value: edges

(edges 100 2)
;Value[1]: 98
;Value[2]: 102


;; Page 32

foo
;error: unbound variable foo

(define foo 10)
;Value: foo

foo
;Value: 10

(+ foo 100)
;Value: 110

bar
;error: unbound variable bar

(define bar foo)
;Value: bar

bar
;Value: 10

(define foo 20)
;;; Thomas doesn't warn that module variable foo is being redefined.
;Value: foo

foo
;Value: 20

bar
;Value: 10

(+ foo bar)
;Value: 30


;; Page 33

(bind ((number1 20)
       (number2 30))
 (+ number1 number2))
;Value: 50

(bind ((x 20)
       (y (+ x x)))
 (+ y y))
;Value: 80

(define foo 10)
;Value: foo

(+ foo foo)
;Value: 20

(bind ((foo 35))
  (+ foo foo))
;Value: 70

(bind ((foo 20))
  (bind ((foo 50))
    (+ foo foo)))
;Value: 100


;; Page 34

(bind (((x <integer>) (sqrt 2)))
  x)
;error: 1.4142135623730951 is not an instance of <integer>

(bind ((foo bar baz (values 1 2 3)))
  (list foo bar baz))
;Value: (1 2 3)

(define-method opposite-edges ((center <number>)
                               (radius <number>))
  (bind ((min max (edges center radius)))
    (values max min)))
;Value: opposite-edges

(opposite-edges 100 2)
;Value[1]: 102
;Value[2]: 98

(bind ((x 10)
       (y 20))
  (bind ((x y (values y x)))
    (list x y)))
;Value: (20 10)

(bind ((!rest nums (edges 100 2)))
  nums)
;Value: (98 102)


;; Page 41

(double 10)
;error: unbound variable double.

(define-method double ((thing <number>))
  (+ thing thing))
;Value: double

(double 10)
;Value: 20

(double "the rain in Spain.")
;error: no method for {the generic function double} was found for the
;       arguments ("the rain in Spain.")

(define-method double ((thing <sequence>))
  (concatenate thing thing))
;Value: double

(double "the rain in Spain.")
;Value: "the rain in Spain.the rain in Spain."

(double '(a b c))
;Value: (a b c a b c)


;; Page 43

(define-method show-rest (a !rest b)
  (print a)
  (print b)
  #t)
;Value: show-rest

(show-rest 10 20 30 40)
;10
;(20 30 40)
;Value: #t

(show-rest 10)
;10
;()
;Value: #t


;; Page 44

;;; +
(define-method make-coffee (!rest x) x)
;Value: make-coffee

(define-method percolate (!key (brand 'maxwell-house)
                               (cups 4)
                               (strength 'strong))
  (make-coffee brand cups strength))
;Value: percolate

;;; +
(define-method position ((x <list>)) (car x))
;Value: position

;;; +
(add-method position (method ((x <number>)) x))
;Value[1]: #[method ...]
;Value[2]: ()

;;; +
(define-method sibling ((x <number>)) (+ x 5))
;Value: sibling

;;; +
(define-method align-objects (a b c d) (list a b c d))
;Value: align-objects

(define-method layout (widget !key (position: the-pos)
                                   (size: the-size))
  (bind ((the-sibling (sibling widget)))
    (unless (= the-pos (position the-sibling))
      (align-objects widget the-sibling the-pos the-size))))
;Value: layout


;; Page 44

(percolate brand: 'folgers cups: 10)
;Value: (folgers 10 strong)

(percolate strength: 'weak
           brand: 'tasters-choice
           cups: 1)
;Value: (tasters-choice 1 weak)

;;; +
(define my-widget 3)
;Value: my-widget

;;; +
(define-method point ((x <number>) (y <number>)) (list x y))
;Value: point

;;; +
(define-method query-user-for-size () 3)
;Value: query-user-for-size

(layout my-widget position: (point 10 10)
                  size: (point 30 50))
;Value: (3 8 (10 10) (30 50))

;;; size: changed to position:
(layout my-widget position: (query-user-for-size))
;Value: (3 8 3 #f)


;; Page 45

;;; In lieu of format, print lists.
(define-method show-keys (req1 req2 !key foo)
  (print (list "requireds: " req1 req2))
  (print (list "key: " foo))
  #t)
;Value: show-keys

(show-keys 'one 'two foo: 'three)
;("requireds: " one two)
;("key: " three)
;Value: #t

(show-keys foo: 'three)
;("requireds: " foo: three)
;("key: " #f)
;Value: #t

;; Page 46

(define-method label ((x <object>) !key price)
  (list price x))
;Value: label

;;; length changed to size
(define-method label ((x <sequence>) !key unit-price)
  (add x (* unit-price (size x))))
;Value: label

(define-method label ((x <list>) !rest info !key calories)
  (add x calories))
;Value: label

(label 'grape price: 189 unit-price: 2)
;error:  illegal keyword argument unit-price:.  Accepted keyword arguments
;       are (price:).

(label 'grape price: 189)
;Value: (189 grape)

(label (vector 3 4 5) price: 189 unit-price: 2)
;Value: #(6 3 4 5)

(label (vector 3 4 5) protein: 7 fat: 8 calories: 9)
;error:  illegal keyword argument protein:.  Accepted keyword arguments are
;       (price: unit-price:).

(label (list 3 4 5) protein: 7 fat: 8 calories: 9)
;Value: (9 3 4 5)


;; Page 46

(define-method test (the-req !rest the-rest !key a b)
  (print the-req)
  (print the-rest)
  (print a)
  (print b))
;Value: test

(test 1 a: 2 b: 3 c: 4)
;1
;(a: 2 b: 3 c: 4)
;2
;3
;Value: #[undefined-value]


;; Page 49

(define-class <point> (<object>)
  horizontal
  vertical)
;Value: <point>


;; Page 49

;;; +
(define my-point (make <point>))
;Value: my-point

(horizontal my-point)
;;; The example wasn't intended to demonstrate an uninitialized slot, but
;;; it's a good thing to test here.
;error: uninitialized slot


;; Page 49

;;; +
(define my-point (make <point>))
;Value: my-point

((setter horizontal) my-point 10)
;Value: 10

;;; +
(horizontal my-point)
;Value: 10


;; Page 50

;;; +
(define my-point (make <point>))
;Value: my-point

(set! (horizontal my-point) 10)
;Value: 10

;;; +
(horizontal my-point)
;Value: 10


;; Page 51

;;; Not materially different from the definition of <point> above.


;; Page 55

(define-class <rectangle> (<object>)
  (top type: <integer>
       init-value: 0
       init-keyword: top:)
  (left type: <integer>
        init-value: 0
        init-keyword: left:)
  (bottom type: <integer>
          init-value: 100
          init-keyword: bottom:)
  (right type: <integer>
         init-value: 100
         init-keyword: right:))
;Value: <rectangle>

<rectangle>
;Value: #[dylan-class ...]

(define my-rectangle (make <rectangle> top: 50 left: 50))
;Value: my-rectangle

(top my-rectangle)
;Value: 50

(bottom my-rectangle)
;Value: 100

(set! (bottom my-rectangle) 55)
;Value: 55

(bottom my-rectangle)
;Value: 55

(set! (bottom my-rectangle) 'foo)
;error: foo is not an instance of <integer> while executing (setter bottom).


;; Page 58

;;; Punt dots.
(define-class <view> (<object>)
  (position allocation: instance))
;Value: <view>

;;; Punt dots.
(define-class <displaced-view> (<view>)
  (position allocation: virtual))
;Value: <displaced-view>

(define-method position ((v <displaced-view>))
  (displace-transform (next-method v)))
;Value: position

(define-method (setter position) ((v <displaced-view>)
                                  new-position)
  (next-method v (undisplace-transform new-position)))
;Value: (setter position)

;;; +
(define-method displace-transform (position)
  (list 'displace-transform position))
;Value: displace-transform

;;; +
(define-method undisplace-transform (position)
  (list 'undisplace-transform position))
;Value: undisplace-transform

;;; +
(define my-displaced-view (make <displaced-view> position: 'initial-position))
;Value: my-displaced-view

;;; +
(position my-displaced-view)
;Value: (displace-transform initial-position)
;;; ??? Got (displace-transform ()) instead!!!
;;; This is because of limitation (9) in DIFFERENCES.

;;; +
(set! (position my-displaced-view) 'next-position)
;Value: next-position

;;; +
(position my-displaced-view)
;Value: (displace-transform (undisplace-transform next-position))


;; Page 59

;;; Punt dots.
(define-class <shape> (<view>)
  (image allocation: virtual)
  (cached-image allocation: instance init-value: #f))
;Value: <shape>

(define-method image ((shape <shape>))
  (or (cached-image shape)
      (set! (cached-image shape) (compute-image shape))))
;Value: image

(define-method (setter image) ((shape <shape>) new-image)
  (set! (cached-image shape) new-image))
;Value: (setter image)

;;; +
(define-method compute-image (shape)
  (list 'compute-image shape))
;Value: compute-image

;;; +
(define my-shape (make <shape>))
;Value: my-shape

;;; +
(image my-shape)
;Value: (compute-image #[dylan-instance ...])

;;; +
((setter image) my-shape 'new-image)
;Value: new-image

;;; +
(image my-shape)
;Value: new-image


;; Page 61

(define foo 10)
;;; The book shows 10 being returned, but the definition of define says the
;;; variable name is returned.
;Value: foo

foo                                     ; this is a variable
;Value: 10                              ; this is the variable's contents

(set! foo (+ 10 10))
;Value: 20

foo
;Value: 20

(setter element)                        ; this is a variable
;Value: #[generic function ...]         ; the variable's contents

;;; +
;;; Save the original value to restore for later tests.
(define %original-set-element (setter element))
;Value: %original-set-element

;;; +
(define-method %set-element (seq index value)
  (print (list '%set-element seq index value))
  value)
;Value: %set-element

(set! (setter element) %set-element)
;Value: #[generic function ...]

(id? (setter element) %set-element)
;Value: #t

;;; Just for laughs.  The next two should work.

;;; +
(set! (element 'foo 'bar) 'baz)
;(%set-element foo bar baz)
;Value: baz

;;; +
((setter element) 'foo 'bar 'baz)
;(%set-element foo bar baz)
;Value: baz

;;; +
(set! (setter element) %original-set-element)
;Value: #[generic function ...]


;; Page 62

(define foo (vector 'a 'b 'c 'd))
;Value: foo

foo
;Value: #(a b c d)

(element foo 2)
;Value: c

(set! (element foo 2) 'sea)
;Value: sea

(element foo 2)
;Value: sea

foo
;Value: #(a b sea d)


;; Page 64

;;; Renamed test to test2, so this definition doesn't conflict with the
;;; previous definition of test.
(define-method test2 ((thing <object>))
  (if thing
      #t
      #f))
;Value: test2

(test2 'hello)
;Value: #t

(test2 #t)
;Value: #t

(test2 #f)
;Value: #f

(define-method double-negative ((num <number>))
  (if (< num 0)
      (+ num num)
      num))
;Value: double-negative

(double-negative 11)
;Value: 11

(double-negative -11)
;Value: -22


;; Page 65

(define-method show-and-tell ((thing <object>))
  (if thing
      (begin
        (print thing)
        #t)
      #f))
;Value: show-and-tell

(show-and-tell "hello")
;hello
;Value: #t


;; Page 65

;;; +
(define-method bonus-illuminated? (pinball post)
  #T)
;Value: bonus-illuminated?

;;; +
(define-method add-bonus-score (player delta)
  (list 'add-bonus-score player delta))
;Value: add-bonus-score

;;; +
(define current-player 'current-player)
;Value: current-player

;;; +
(define pinball 'pinball)
;Value: pinball

;;; +
(define post 'post)
;Value: post

(when (bonus-illuminated? pinball post)
  (add-bonus-score current-player 100000))
;Value: (add-bonus-score current-player 100000)


;; Page 65

;;; +
(define-method detect-gas? (nose)
  #F)
;Value: detect-gas?

;;; +
(define-method light (match)
  (print (list 'strike match))
  (print "KABOOM")
  'oh-well)
;Value: light

;;; +
(define nose 'nose)
;Value: nose

;;; +
(define match 'match)
;Value: match

(unless (detect-gas? nose)
        (light match))
;(strike match)
;"KABOOM"
;Value: oh-well


;; Page 66

;;; +
(define new-position 100)
;Value: new-position

;;; +
(define old-position 0)
;Value: old-position

(cond ((< new-position old-position)
       "the new position is less")
      ((= new-position old-position)
       "the positions are equal")
      (else: "the new position is greater"))
;Value: "the new position is greater"


;; Page 67

;;; +
(define-method career-choice (student)
  (cond ((id? student 'paul) 'art)
        ((id? student 'jim) 'history)
        ((id? student 'steve) 'science)
        (else: 'bum)))
;Value: career-choice

;;; Make this a method for easier testing.
(define babble
  (method (student)
    (case (career-choice student)
      ((art music drama)
       (print "Don't quit your day job."))
      ((literature history linguistics)
       (print "That really is fascinating."))
      ((science math engineering)
       (print "Say, can you fix my VCR?"))
      (else: "I wish you luck."))))
;Value: babble

;;; +
(babble 'neil)
;Value: "I wish you luck."

;;; +
(babble 'steve)
;"Say, can you fix my VCR?"
;Value: #[undefined-value]

;;; +
(babble 'jim)
;"That really is fascinating."
;Value: #[undefined-value]

;;; +
(babble 'paul)
;"Don't quit your day job."
;Value: #[undefined-value]


;; Page 67

;;; Make this a method for easier testing.
(define whatitis
  (method (my-object)
    (select my-object instance?
      ((<window> <view> <rectangle>) "it's a graphic object")
      ((<number> <list> <sequence>) "it's something computational")
      (else: "Don't know what it is"))))
;Value: whatitis

;;; +
(whatitis (make <view>))
;Value: "it's a graphic object"

;;; +
(whatitis #())
;Value: "it's something computational"

;;; +
(whatitis #F)
;Value: "Don't know what it is"


;; Page 68

(if #t
    (print "it was true")
    #t
    #f)
;error: too many arguments to if.

(if #t
    (begin
      (print "it was true")
      #t)
    #f)
;"it was true"
;Value: #t


;; Page 69

(define-method factorial ((n <integer>))
  (for ((i n (- i 1))                   ;variable clause 1
        (v 1 (* v i)))                  ;variable clause 2
       ((<= i 0) v)))                   ;end test and result
;Value: factorial


;; Page 69

(define-method first-even ((s <sequence>))
  (for-each ((number s))
            ((even? number) number)
                                ; No body forms needed
    ))
;Value: first-even


;; Page 70

;;; +
(define-method schedule-game ((city <symbol>) (year <number>))
  (print (list year city)))
;Value: schedule-game

(define-method schedule-olympic-games ((cities <sequence>)
                                       (start-year <number>))
  (for-each ((year (range from: start-year by: 4))
             (city cities))
            ()                  ; No end test needed.
    (schedule-game city year)))
;Value: schedule-olympic-games

;;; +
(schedule-olympic-games
 #(boston new-york baltimore chicago denver san-francisco)
 2000)
;(2000 boston)
;(2004 new-york)
;(2008 baltimore)
;(2012 chicago)
;(2016 denver)
;(2020 san-francisco)
;Value: #[undefined-value]


;; Page 70

(begin
  (dotimes (i 6) (print "bang!"))
  (print "click!"))
;bang!
;bang!
;bang!
;bang!
;band!
;bang!
;click!
;Value: #[undefined-value]


;; Page 71

(define-method first-even ((seq <sequence>))
  (bind-exit (exit)
    (do (method (item)
          (when (even? item)
            (exit item)))
        seq)))
;Value: first-even

(first-even '(1 3 5 4 7 9 10))
;Value: 4


;; Page 72

+
;Value: #[generic function ...]

'+
;Value: +

(quote +)
;Value: +

''+
;Value: (quote +)

(+ 10 10)
;Value: 20

'(+ 10 10)
;Value: (+ 10 10)

(quote (+ 10 10))
;Value: (+ 10 10)


;; Page 73

(apply + 1 '(2 3))
;Value: 6

(+ 1 2 3)
;Value: 6

(define math-functions (list + * / -))
;Value: math-functions

math-functions
;Value: (#[method ...] #[method ...] #[method ...] #[method ...])

(first math-functions)
;Value: #[method ...]

(apply (first math-functions) 1 2 '(3 4))
;Value: 10


;; Page 79

(method (num1 num2)
        (+ num1 num2))
;Value: #[method ...]


;; Page 80

;;; +
(define-class <person> (<object>)
  name
  age)
;Value: <person>

;;; +
(define person-list
  (list (make <person> name: "Paul" age: 15)
        (make <person> name: "Jill" age: 3)
        (make <person> name: "Jack" age: 2)
        (make <person> name: "Peter" age: 12)))
;Value: person-list

;;; Wrap this in a for-each that shows us the sorted list.
(for-each
     ((person
       ;the second argument to SORT is the test function
       (sort person-list
             (method (person1 person2)
                     (< (age person1)
                        (age person2))))))
     ()
  (print (name person)))
;"Jack"
;"Jill"
;"Peter"
;"Paul"
;Value: #[undefined-value]

(bind ((double (method (number)
                 (+ number number))))
  (double (double 10)))
;Value: 40


;; Page 80

(define-method double ((my-method <function>))
  (method (!rest args)
    (apply my-method args)
    (apply my-method args)
    #f))
;Value: double

;;; Changed print to display.
(define print-twice (double display))
;Value: print-twice

print-twice
;Value: #[method ...]

(print-twice "The rain in Spain. . .")
;The rain in Spain. . .The rain in Spain. . .
;Value: #f

(print-twice 55)
;5555
;Value: #f


;; Page 81

;;; Changed length to size.
(define-method root-mean-square ((s <sequence>))
  (bind-methods ((average (numbers)
                   (/ (reduce1 + numbers)
                      (size numbers)))
                 (square (n) (* n n)))
    (sqrt (average (map square s)))))
;Value: root-mean-square

(root-mean-square '(5 6 6 7 4))
;Value: 5.692099788303083


;; Page 81

(define-method newtons-sqrt (x)
  (bind-methods ((sqrt1 (guess)
                   (if (close? guess)
                       guess
                       (sqrt1 (improve guess))))
                 (close? (guess)
                   (< (abs (- (* guess guess) x)) .0001))
                 (improve (guess)
                   (/ (+ guess (/ x guess)) 2)))
    (sqrt1 1)))
;Value: newtons-sqrt

(newtons-sqrt 25)
;Value: 5.000000000053723


;; Page 82

(define-method double ((thing <number>))
  (+ thing thing))
;Value: double


;; Page 82

(double 10)
;Value: 20

(double 4.5)
;Value: 9.0


;; Page 82

(define-method double ((thing <integer>))
  (* thing 2))
;Value: double


;; Page 82

(define-method double ((thing (singleton 'cup)))
  'pint)
;Value: double

(double 'cup)
;Value: pint


;; Page 83

(define-method double ((num <float>))
  (print "doubling a floating-point number")
  (next-method))
;Value: double

(double 10.5)
;"doubling a floating-point number"
;Value: 21.0


;; Page 85

;;; +
(define-class <file> (<object>)
  name)
;Value: <file>

(define-method show ((device <window>) (thing <character>))
  (print '(show <window> <character>)))
;Value: show

(define-method show ((device <window>) (thing <string>))
  (print '(show <window> <string>)))
;Value: show

(define-method show ((device <window>) (thing <rectangle>))
  (print '(show <window> <rectangle>)))
;Value: show

(define-method show ((device <file>) (thing <character>))
  (print '(show <file> <character>)))
;Value: show

(define-method show ((device <file>) (thing <string>))
  (print '(show <file> <string>)))
;Value: show

;;; +
(show (make <window>) #\Return)
;(show <window> <character>)
;Value: #[undefined-value]

;;; +
(show (make <window>) "Return")
;(show <window> <string>)
;Value: #[undefined-value]

;;; +
(show (make <window>) (make <rectangle>))
;(show <window> <rectangle>)
;Value: #[undefined-value]

;;; +
(show (make <file>) #\Return)
;(show <file> <character>)
;Value: #[undefined-value]

;;; +
(show (make <file>) "Return")
;(show <file> <string>)
;Value: #[undefined-value]


;; Page 86

(make <generic-function> required: 3)
;Value: #[generic function ...]

(make <generic-function> required: 3
                         debug-name: 'foo)
;Value: #[generic function ...]

(define expand
  (make <generic-function> required: 1 debug-name: 'expand))
;;; The book shows a method being returned, but the definition of define
;;; says the variable name is returned.
;Value: expand

(expand 55)
;error: no applicable method for 55 in {the generic function expand}


;; Page 97

(define-method double ((thing (singleton 'cup)))
  'pint)
;;; The book shows a method being returned, but the definition of define
;;; says the variable name is returned.
;Value: double

(double 'cup)
;Value: pint

(double 10)
;Value: 20


;; Page 98

(define-method factorial ((num <integer>))
  (* num (factorial (- num 1))))
;Value: factorial

(define-method factorial ((num (singleton 0)))
  1)
;Value: factorial

(factorial 5)
;Value: 120


;; Page 100

(do (method (a b) (print (+ a b)))
    '(100 100 200 200)
    '(1 2 3 4))
;101
;102
;203
;204
;Value: #f


;; Page 101

(map + '(100 100 200 200)
       '(1 2 3 4))
;Value: (101 102 203 204)


;; Page 101

(map-as <vector> +
         '(100 100 200 200)
         '(1 2 3 4))
;Value: #(101 102 203 204)


;; Page 101

(define x '(100 100 200 200))
;Value: x

(map-into x + '(1 2 3 4))
;Value: (101 102 203 204)

x
;Value: (101 102 203 204)


;; Page 102

(any? > '(1 2 3 4) '(5 4 3 2))
;Value: #t

(any? even? '(1 3 5 7))
;Value: #f


;; Page 102

(every? > '(1 2 3 4) '(5 4 3 2))
;Value: #f

(every? odd? '(1 3 5 7))
;Value: #t


;; Page 102

(define high-score 10)
;Value: high-score

(reduce max high-score '(3 1 4 1 5 9))
;Value: 10

(reduce max high-score '(3 12 9 8 8 6))
;Value: 12


;; Page 103

(reduce1 + '(1 2 3 4 5))
;Value: 15


;; Page 103

(define flavors #(chocolate pistachio pumpkin))
;Value: flavors

(member? 'chocolate flavors)
;Value: #t

(member? 'banana flavors)
;Value: #f


;; Page 103

;;; +
(define-method has-nuts? (flavor)
  (member? flavor #(pistachio butter-pecan macadamia)))
;Value: has-nuts?

flavors
;Value: #(chocolate pistachio pumpkin)

(find-key flavors has-nuts?)
;Value: 1

(element flavors 1)
;Value: pistachio


;; Page 104

(define numbers (list 10 13 16 19))
;Value: numbers

(replace-elements! numbers odd? double)
;Value: (10 26 16 38)


;; Page 104

(define x (list 'a 'b 'c 'd 'e 'f))
;Value: x

(fill! x 3 start: 2)
;Value: (a b 3 3 3 3)


;; Page 105

(define numbers '(3 4 5))
;Value: numbers

(add numbers 1)
;Value: (1 3 4 5)

numbers
;Value: (3 4 5)


;; Page 105

(define numbers (list 3 4 5))
;Value: numbers

(add! numbers 1)
;Value: (1 3 4 5)


;; Page 105

(add-new '(3 4 5) 1)
;Value: (1 3 4 5)

(add-new '(3 4 5) 4)
;Value: (3 4 5)


;; Page 105

(add-new! (list 3 4 5) 1)
;Value: (1 3 4 5)

(add-new! (list 3 4 5) 4)
;Value: (3 4 5)


;; Page 106

(remove '(3 1 4 1 5 9) 1)
;Value: (3 4 5 9)


;; Page 106

(remove! (list 3 1 4 1 5 9) 1)
;Value: (3 4 5 9)


;; Page 106

(choose even? '(3 1 4 1 5 9))
;Value: (4)


;; Page 106

(choose-by even? (range from: 1)
                 '(a b c d e f g h i))
;Value: (b d f h)


;; Page 107

(intersection '(john paul george ringo)
              '(richard george edward charles john))
;Value: (john george)


;; Page 107

(union '(butter flour sugar salt eggs)
       '(eggs butter mushrooms onions salt))
;;; The union may have the elements in a different order.
;Value: (salt butter flour sugar eggs mushrooms onions)


;; Page 107

(remove-duplicates '(spam eggs spam sausage spam spam spam))
;Value: (spam eggs sausage)


;; Page 108

(remove-duplicates! '(spam eggs spam sausage spam spam))
;Value: (spam eggs sausage)


;; Page 108

(define hamlet '(to be or not to be))
;Value: hamlet

(id? hamlet (copy-sequence hamlet))
;Value: #f

(copy-sequence hamlet start: 2 end: 4)
;Value: (or not)


;; Page 108

(concatenate-as <string> '(#\n #\o #\n) '(#\f #\a #\t))
;Value: "nonfat"

(concatenate-as <vector> '(0 1 2) '(3 4 5) '(6 7 8))
;Value: #(0 1 2 3 4 5 6 7 8)


;; Page 108

(concatenate "low-" "calorie")
;Value: "low-calorie"

(concatenate '(0 1 2) '(3 4 5) '(6 7 8))
;Value: (0 1 2 3 4 5 6 7 8)


;; Page 109

(define phrase "I hate oatmeal.")
;Value: phrase

(replace-subsequence! phrase "like" start: 2)
;Value: "I like oatmeal."


;; Page 109

(define x '(bim bam boom))
;Value: x

(reverse x)
;Value: (boom bam bim)

x
;Value: (bim bam boom)


;; Page 109

(reverse! '(bim bam boom))
;Value: (boom bam bim)


;; Page 110

(define numbers '(3 1 4 1 5 9))
;Value: numbers

(sort numbers)
;Value: (1 1 3 4 5 9)

numbers
;Value: (3 1 4 1 5 9)


;; Page 110

(sort! '(3 1 4 1 5 9))
;Value: (1 1 3 4 5 9)


;; Page 110

(last '(emperor of china))
;Value: china


;; Page 111

(subsequence-position "Ralph Waldo Emerson" "Waldo")
;Value: 6


;; Page 113

(aref #(7 8 9) 1)
;Value: 8


;; Page 113

;;; +
(define x #(7 8 9))
;Value: x

(set! (aref #(7 8 9) 1) 5)
;;; The book shows #(7 5 9) being returned, but the definition of set! says
;;; the new value is returned.
;Value: 5

;;; +
x
;Value: #(7 5 9)

;;; +
(define x #(7 8 9))
;Value: x

;;; Using "x" rather than "#(7 8 9)"
((setter aref) x 1 5)
;;; The book shows #(7 5 9) being returned, but the definition of set! says
;;; the new value is returned.
;Value: 5

;;; +
x
;Value: #(7 5 9)


;; Page 113

(dimensions (make <array> dimensions: '(4 4)))
;Value: (4 4)


;; Page 115

(cons 1 2)
;Value: (1 . 2)

(cons 1 '(2 3 4 5))
;Value: (1 2 3 4 5)


;; Page 115

(list 1 2 3)
;Value: (1 2 3)

(list (+ 4 3) (- 4 3))
;Value: (7 1)


;; Page 115

(list* 1 2 3 '(4 5 6))
;Value: (1 2 3 4 5 6)


;; Page 116

(car '(4 5 6))
;Value: 4

(car '())
;Value: ()


;; Page 116

(cdr '(4 5 6))
;Value: (5 6)

(cdr '())
;Value: ()


;; Page 116

(define x '(4 5 6))
;;; The book shows (4 5 6) being returned, but the definition of define
;;; says the variable name is returned.
;Value: x

(set! (car x) 9)
;Value: 9

;;; +
x
;Value: (9 5 6)


;; Page 116

(define x '(4 5 6))
;;; The book shows (4 5 6) being returned, but the definition of define
;;; says the variable name is returned.
;Value: x

(set! (cdr x) '(a b c))
;Value: (a b c)

;;; +
x
;Value: (4 a b c)


;; Page 120

(define x "Van Gogh")
;Value: x

(as-lowercase x)
;Value: "van gogh"

;;; +
x
;Value: "Van Gogh"


;; Page 120

(define x "Van Gogh")
;Value: x

(as-lowercase! x)
;Value: "van gogh"

;;; +
x
;Value: "van gogh"


;; Page 120

(define x "Van Gogh")
;Value: x

(as-uppercase x)
;Value: "VAN GOGH"

;;; +
x
;Value: "Van Gogh"


;; Page 120

(define x "Van Gogh")
;Value: x

;;; as-uppercase changed to as-uppercase!
(as-uppercase! x)
;Value: "VAN GOGH"

;;; +
x
;Value: "VAN GOGH"


;; Page 123

;;; NOT tested.  From here through page 130, the "examples" are
;;; explanations, not true examples.


;; Page 142

;;; Make this a method for easier testing.
(define handleit
  (method (it)
    (handler-case (it)
      ((<type-error>) "there was a type-error")
      ((<error>) "there was an error")
      ((<warning>) "there was a warning"))))
;Value: handleit

;;; +
(handleit (method ()
             (signal (make <simple-warning>
                           format-string: "simple warning"
                           format-arguments: '()))))
;;;Value: "there was a warning"

;;; +
(handleit (method ()
             (check-type 'foo <string>)))
;;;Value: "there was a type-error"

;;; +
(handleit (method ()
            (error "simple error")))
;;;Value: "there was an error"


;; Page 144-146

;;; This example demonstrates handling a <file-not-found> error by
;;; signaling a <try-a-different-file> restart.  This example requires
;;; implementation-specific code, so it might be found in a file named
;;; "restart-example.dyl" in the implementation-specific subdirectories.


;; Page 153

(as <symbol> "foo")
;Value: foo

(id? 'FOO (as <symbol> "Foo"))
;Value: #t

'Foo
;Value: foo

(as <keyword> "foo")
;Value: foo:


;; Page 154

(as <string> 'Foo)
;Value: "foo"

(as <string> 'bar:)
;Value: "bar"


;; Page 157

(define-method sum ((numbers <sequence>))
  (reduce1 + numbers))
;Value: sum

(define-method square ((x <number>)) (* x x))
;Value: square

(define-method square-all ((coords <sequence>))
  (map square coords))
;Value: square-all

(define distance (compose sqrt sum square-all))
;Value: distance

(distance '(3 4 5))
;Value: 7.0710678118654755


;; Page 157

;;; +
(define-method female? (name)
  (member? name #(michelle anne ann barbara roseanne susan)))
;Value: female?

;;; +
(define michelle 'michelle)
;Value: michelle

;;; +
(define arnold 'arnold)
;Value: arnold

;;; +
(define roseanne 'roseanne)
;Value: roseanne

(map female? (list michelle arnold roseanne))
;Value: (#t #f #t)

(map (complement female?) (list michelle arnold roseanne))
;Value: (#f #t #f)


;;Page 158

(map (curry + 1) '(3 4 5))
;Value: (4 5 6)


;; Page 158

(define yuppify (rcurry concatenate ", ayup"))
;Value: yuppify

(yuppify "I'm from New Hampsha")
;Value: "I'm from New Hampsha, ayup"


;; Page 159

((always 1) 'x 'y 'z)
;Value: 1

((always #t) #f #f)
;Value: #t
