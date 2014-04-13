;;; SICP: Compound Data
;;; http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/2b-compound-data/
;;;
;;; wishful thinking ftw
;;;
;;; Data abstraction, isolating use from representation.
;;; List structure as a way of glueing things together.
;;;
;;; Make progress, but never be bound by the consequences of your decisions.
;;;
;;; You lose control of complexity when you don't use layers of abstraction.
;;;
;;; In response to a question about planning everything before implementing:
;;; The good part of computer science is a lot like magic.  There's a bad part
;;; of computer science that's a lot like religion.  People who think you
;;; should design everything before you've implemented it probably haven't
;;; designed designed very many things.  The real power is that you can pretend
;;; that you've made the decision, and later on figure out which decision you
;;; ought to have made.

;;; Rational numbers.  The axiom: We want three procedures, make-rat, num, and
;;; den, such that if x = (make-rat n d), (num x) / (den x) = n/d. Abstractly,
;;; that's what an abstract number really is, and that's what we want our
;;; programming contract to be.

; Make a rational number n/d, reducing to gcd
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))

; Return the numerator of a rational number
(define (num x)
  (car x))

; Return the denominator of a rational number
(define (den x)
  (cdr x))

; Add two rationals
(define (add-rat x y)
  (make-rat
    (+ (* (num x) (den y))
       (* (num y) (den x)))
    (* (den x) (den y))))

; Multiply two rationals
(define (mul-rat x y)
  (make-rat
    (* (num x) (num y))
    (* (den x) (den y))))

;;; representing vectors in the plane

; Make vector (x, y)
(define (make-vec x y) (cons x y))

; Return x component of p
(define (xcor p) (car p))

; Return y component of p
(define (ycor p) (cdr p))

;;; Alternately, because procdures can be objects, and you can name them, it
;;; could simply be:
;;;
;;; (define make-vec cons)
;;; (define xcor car)
;;; (define ycor cdr)

;;; representing line segments P -> Q

;;; Closure: The means of combination in your system are such that, when you
;;; put things together using them, you can then put those things together with
;;; the same means of combination.  So I can have a pair of numbers, but also a
;;; pair of pairs.

(define (scale-vec s v)
  (make-vec (* s (xcor v))
            (* s (ycor v))))

(define (make-seg p q) (cons p q))

(define (seg-start s) (car s))

(define (seg-end s) (cdr s))

; Midpoint of a segment
(define (seg-midpoint s)
  (let ((a (seg-start s))
        (b (seg-end s)))
    (make-vec
      (average (xcor a) (xcor b))
      (average (ycor a) (ycor b)))))

(define (average x y)
  (/ (+ x y) 2))

; Length of a segment
(define (seg-length s)
  (let ((dx (- (xcor (seg-end s))
               (xcor (seg-start s))))
        (dy (- (ycor (seg-end s))
               (ycor (seg-start s)))))
    (sqrt (+ (square dx)
             (square dy)))))

;;; What are pairs, really?

;;; Axiom for pairs: for any x and y, (car (cons x y)) is x, and (cdr (cons x
;;; y)) is y.  We built vectors and rationals out of pairs.  What do we build
;;; pairs out of?  Nothing at all:

(define (mycons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))

(define (mycar x) (x 1))

(define (mycdr x) (x 2))

;;; That completely weird implementation of cons, car, and cdr satisfies the
;;; axiom.  So it's a perfectly valid way of building data objects.  In lisp,
;;; they can all be built on existential nothing.
;;;
;;; Begin to blur the line of what's data and what's procedure.
;;;
;;; A procedure is not merely the act of doing something.  A procedure is a
;;; real object that has existence.

