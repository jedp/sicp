; tree-recursive.  Naive and awful.  Ugh.  O(n^2)

(define (fib-tree n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-tree (- n 1))
                 (fib-tree (- n 2))))))

; an iterative approach.  Meh.  O(n)

(define (fib-iter n)
  (fib-iter1 1 0 n))

(define (fib-iter1 a b count)
  (if (= count 0)
    b
    (fib-iter1 (+ a b) a (- count 1))))

; definition.  O(1).  Take it when you can get it.
; The closest integer to (phi^n / sqrt(5))

(define phi 1.61803399)
; (define sqrt-5 2.23606797749979)
; apparently, this works also as (phi^n / (phi + 2))
; neato

(define (fib-def n)
  (round (/ (expt phi n) (+ phi 2))))

; use the mathematical definition

(define fib fib-def)
