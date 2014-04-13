;;; SICP: Henderson Escher Example
;;; http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/3a-henderson-escher-example/
;;;
;;; Metalinguistic design.  Build a suitable, powerful language

;;; Some review and reconsideration
;;;
;;; Representing line segments again
;;;
;;; Pair of pairs

(define make-segment2 cons)
(define seg-start2 car)
(define seg-end2 cdr)

(define make-vec2 cons)
(define xcor2 car)
(define ycor2 cdr)

;;; The set of data objects in list is closed under the operation of forming
;;; pairs.  That's the thing that allows us to build complexity.  (Basic and
;;; Fortran allow you to make arrays of numbers, but not arrays of arrays, so
;;; they're not closed.)

;;; Lisp has a convention for representing a sequence of things as a chain of
;;; pairs, called a list.

;;; array of first four integers:
;;;
;;; [ | ] -> [ | ] -> [ | ] -> [ |/]
;;;  1        2        3        4
;;;
;;; (cons 1
;;;       (cons 2
;;;             (cons 3
;;;                   (cons 4 nil))))
;;;
;;; Conventionally printed as (1 2 3 4)
;;;
;;; The Lisp operaton LIST is an abbreviation for making this construct.
;;; (list 1 2 3 4)
;;;
;;; (cdr (cdr (cdr (cdr (list 1 2 3 4))))) -> ()

;;; (define (scale-list s l)
;;;   (if (null? l)
;;;       ()                            ; or nil in other dialects
;;;       (cons (* (car l) s)
;;;             (scale-list s (cdr l)))))

;;; Higher-order procedure map

(define (map p l)                   ; procedure p to map over list l
  (if (null? l)
      ()
      (cons (p (car l))
            (map p (cdr l)))))

(define (scale-list s l)
  (map (lambda (item) (* item s))
       l))

;;; We could define map to be iterative, not recursive, but it doesn't change
;;; the way we think about it.  We think about aggregates rather than control
;;; structures.

(define (for-each proc list)
  (cond ((null? list) "done")
        (else (proc (car list))
              (for-each proc
                        (cdr list)))))

;;; Metalinguistic abstraction
;;;
;;; When you think about a language, you ask, what are the
;;;  - primitives
;;;  - means of combination
;;;  - means of abstraction

;;; Peter Henderson's language for describing things that look like M C Escher
;;; drawings.
;;;
;;; Only one primitive, a Picture.  A Picture in this language is something
;;; thad draws an image scaled to fit a rectangle you specify.  (So different
;;; scalings are not different Pictures.)

; Specifying rectangles:
; Origin, Horiz, Vert (three vectors to specify a rectangle)
;
; A constructor make-rect, and selectors horiz, vert, origin
;
; A rectangle in some sense describes a transformation from a unit square into
; that rectangle.

(define (coord-map rect)
  (lambda (point)
    (add-vec
      (add-vec (scale (xcor point)
                      (horiz rect))
               (scale (ycor point)
                      (vert rect)))
      (origin rect))))

; Constructing primitive Pictures from lists of segments

(define (make-picture seglist)
  (lambda (rect)
    (for-each
      (lambda (s)
        (drawline
          ((coord-map rect) (seg-start s))
          ((coord-map rect) (seg-end s))))
      seglist)))

; beside - means of combination
; (beside p1 p2 a) -> picture
; place p1 and b2 side-by-side with the horizontal factor of the first
; rectangle scaled by a.  (p2 is scaled by 1-a.)

(define (beside p1 p2 a)
  (lambda (rect)
    (p1 (make-rect
          (origin rect)
          (scale a (horiz rect))
          (vert rect)))
    (p2 (make-rect
          (mul-vec (origin rect)
                   (scale a (horiz rect)))
          (scale (- 1 a) (horiz rect))
          (vert rect)))))

(define (rotate90 pict)
  (lambda (rect)
    (pict (make-rect
            (add-vec (origin rect)
                     (horiz rect))
            (vert rect)
            (scale -1 (horiz rect))))))

;;; Now we have implemented the means of abstractions as procedures.  Not only
;;; is this language implemented in lisp, but it is embedded in lisp.

; recursive means of combination, pushing a scaled picture to the right of
; itself
(define (right-push pict n a)
  (if (= n 0)
      pict
      (beside pict (right-push pict (- n 1) a)
              a)))

;;; "Lisp is a lousy language for doing any particular problem.  What it's good
;;; for is figuring out the right language that you want and embedding that in
;;; Lisp."  - Hal Abelson

(define (push comb)
  (lambda (pict n a)
    ((repated
       (lambda (p) (comb pict p a))
       n)
     pict)))

(define right-push (push beside))
(define up-push (push above))
; etc

;;; An example of building robust systems.  Key idea has been that in order to
;;; make a system that's robust, it must be insensitive to small changes.  A
;;; small chnage in the problem should yield only a small change in the
;;; solution. There ought to be a continuity.  Instead of solving a particular
;;; problem at every level of the decomposition of the problem, you solve a
;;; class of problems that are in the neighborhood of the particular problem
;;; you're trying to solve.  To do that, you produce a language at the level of
;;; detail in which the solutions to that class of problems is representable in
;;; that language.  Therefore, if you make small changes in the problem, you
;;; generally only have to make small changes in the solution, because the
;;; language you've made is expressive of solutions to problems of the same
;;; type.
