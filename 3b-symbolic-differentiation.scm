;;; Symbolic differentiation: Quotation
;;; http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/3b-symbolic-differentiation-quotation/
;;;
;;; Confusing the question of what is a procedure and what is data.

; We have a numerical approximation from before

(define dx .000001)

(define approx-deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+x dx))
            (f x))
         dx))))

; That's a numerical approximation.  What about things like rules for finding
; derivatives of expressions; e.g., the derivative of a contstant is 0, the
; derivative of the variable with respect to which you are taking the
; derivative is 1, etc.  These are exact expressions.  Can we make programs
; that manipulate these expressions?  Hell yes we can.

; The derivative of an expression with respect to a variable.  (Different from
; the derivative of a function that gave us a numerical approximation.)  This
; is a syntactic phenomenon.

; Start with some wishful thinking in our dispatches
(define (deriv expr var)
  (cond ((constant? expr var) 0)
        ((same-var? expr var) 1)
        ((sum? expr) (make-sum (deriv (a1 expr) var)
                               (deriv (a2 expr) var)))
        ((product? expr)
         (make-sum
           (make-product (m1 expr) (deriv (m2 expr) var))
           (make-product (deriv (m1 expr) var) (m2 expr))))
        ; etc.
    ))

; Use the embedding in Lisp to advantage.  The car is the operator, the cdrs
; are the operands.

; In the lectures, the have atom?, which is not found in mit scheme.  I think
; this works?
(define atom?
  (lambda (x) (not (pair? x))))

; If the expression can't be broken into parts
(define (constant? expr var)
  (and (atom? expr)
       (not (eq? expr var))))


(define (same-var? expr var)
  (and (atom? expr)
       (eq? expr var)))

; How do we deal with sums?  A sum is something that's not atomic and begins
; with the symbol '+.

(define (sum? expr)
  (and (not (atom? expr))
       (eq? (car expr) '+)))

(define (make-sum a1 a2)
  (list '+ a1 a2))

; heh
(define a1 cadr)
(define a2 caddr)

(define (product? expr)
  (and (not (atom? expr))
       (eq? (car expr) '*)))

(define (make-product m1 m2)
  (list '* m1 m2))

(define m1 cadr)
(define m2 caddr)

; ax^2 + bx + c
(define foo
  '(+ (* a (* x x))
      (+ (* b x)
      c)))

; 1 ]=> (deriv foo 'x)
; Value: (+ (+ (* a (+ (* x 1) (* 1 x))) (* 0 (* x x))) (+ (+ (* b 1) (* 0 x)) 0))
;
; Well, that's equivalent, but it's not very legible.  I would like it to be
; just 2ax + b.
;
; (deriv foo 'a)
;  ; x*x
;  -> (+ (+ (* a (+ (* x 0) (* 0 x))) (* 1 (* x x))) (+ (+ (* b 0) (* 0 x)) 0))
;
; (deriv foo 'b)
;  ; x
;  -> (+ (+ (* a (+ (* x 0) (* 0 x))) (* 0 (* x x))) (+ (+ (* b 0) (* 1 x)) 0))
;
; (deriv foo 'c)
;  ; 1
;  ->  (+ (+ (* a (+ (* x 0) (* 0 x))) (* 0 (* x x))) (+ (+ (* b 0) (* 0 x)) 1))

; Let's fix the representation

(define (make-sum a1 a2)
  (cond ((and (number? a1)
              (number? a2))
         (+ a1 a2))
        ((and (number? a1)
              (= a1 0))
         a2)
        ((and (number? a2)
              (= a2 0))
         a1)
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((and (number? m1)
              (number? m2))
         (* m1 m2))
        ((and (number? m1)
              (= m1 0))
         0)
        ((and (number? m2)
              (= m2 0))
         0)
        ((and (number? m1)
              (= m1 1))
         m2)
        ((and (number? m2)
              (= m2 1))
         m1)
        (else (list '* m1 m2))))

; So now we get:
;
; (deriv foo 'x) ->
;   ; 2ax + b
;   (+ (* a (+ x x)) b)
;
; Yay!  That seems better, as do these:
;
; (deriv foo 'a) -> (* x x)
; (deriv foo 'b) -> x
; (deriv foo 'c) -> 1
