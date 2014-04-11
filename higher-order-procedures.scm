;;; SICP: Higher-order procedures
;;; http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/2a-higher-order-procedures/
;;;
;;; Approximate square roots using Newton's method
;;;
;;; Executes like this, with printfs thrown in out of curiosity:
;;;
;;; 1 ]=> (sqrt 23492834)
;;; Get fixed-point of derivative
;;;  guess: 1.17436e+07
;;;  guess: 5.875e+06
;;;  guess: 2.9375e+06
;;;  guess: 1.46851e+06
;;;  guess: 734322
;;;  guess: 367173
;;;  guess: 183618
;;;  guess: 91873.2
;;;  guess: 46064.2
;;;  guess: 23287.1
;;;  guess: 12148
;;;  guess: 7040.93
;;;  guess: 5188.77
;;;  guess: 4858.2
;;;  guess: 4846.95
;;;  guess: 4846.94
;;;  guess: 4846.94
;;; ;Value: 4846.9406845968315
;;;
;;; 1 ]=> (square 4846.9406845968315)
;;;
;;; ;Value: 23492834.

; using slib
(require 'printf)

; Compute the square root of x using newton's method
(define (sqrt x)
 (newton
  (lambda (y) (- x (square y)))     ; find the 0 and we have found sqrt x
  1))                               ; initial arbitrary guess for sqrt

; compute the square of x
(define (square x) (* x x))

; Find zero of a function using Newton's method
(define (newton f guess)            ; procedure that computes a function,
                                    ; and initial guess
 (define df (derivative f))         ; compute df, derivative of function
                                    ; computed by procedure f
 (fixed-point
  (lambda (x) (- x (/ (f x) (df x))))
  guess))

; Given f, return a procedure to compute the derivative of x
(define derivative
 (lambda (f)                        ; given f
  (lambda (x)                       ; return f'
   (/ (- (f (+ x dx))
         (f x))
       dx))))

(define small-number 0.000001)
(define dx small-number)            ; arbitrary value for small dx

; Find the fixed-point of function f given an initial guess
(define (fixed-point f start)
 (define (iter old new)
  ;(printf " guess: %g\n" new)
  (if (close-enough? old new)
      new
      (iter new (f new))))
 (iter start (f start)))            ; refine guess until close enough

; Determine whether two values are close enough to be considered equal
(define (close-enough? x y)
 (< (abs (- x y)) tolerance))

(define tolerance small-number)     ; arbitrary tolerance value

; Compute the absolute value of x
(define (abs x)
 (if (< x 0)
     (- x)
     x))

sqrt
