#lang racket

(module+ test
  (require rackunit))

;; Compute primes with a sieve based on generators, which
;; are implemented as computations that perform a yield effect.

;; A counter effect instruments how many remainder computations
;; the sieve performs. Incrementing the counter requires crossing
;; many yield handlers installed by the nested calls filter-gen.

;; counter effect via set!
#;(begin
  (define ctr 0)
  (define (ctr-inc)
    (set! ctr (+ 1 ctr))
    ctr)
  (define (with-counter n thunk)
    (set! ctr (- n 1))
    (thunk)))

;; counter effect via handler
(begin
  (define ctr-tag (make-continuation-prompt-tag))

  (define (ctr-inc)
    (call-with-composable-continuation
     (lambda (k)
       (abort-current-continuation ctr-tag k))
     ctr-tag))

  (define (with-counter n thunk)
    (call-with-continuation-prompt
     thunk
     ctr-tag
     (lambda (k)
       (with-counter (+ n 1) (lambda () (k n)))))))

;; yield effect
(define yield-tag (make-continuation-prompt-tag))

(define (yield v)
  (call-with-composable-continuation
   (lambda (k)
     (abort-current-continuation yield-tag v k))
   yield-tag))

(define (handle-yield handler body)
  (call-with-continuation-prompt
   body
   yield-tag
   handler))

;; A Generator is a (-> void) that may yield values when called.

;;  Generator -> (ListOf Any)
(define (collect gen)
  (handle-yield
   (lambda (v k)
     (cons v (collect k)))
   (lambda ()
     (gen)
     '())))

;; Generator (Any -> Boolean) -> Generator
(define (filter-gen gen pred)
  (lambda ()
    (let loop ([gen gen])
      (handle-yield
       (lambda (v k)
         (when (pred v) (yield v))
         (loop k))
       gen))))

;; Integer Integer -> Generator
(define (naturals min max)
  (lambda ()
    (when (< min max)
      (yield min)
      ((naturals (+ min 1) max)))))

(module+ test
  (check-equal?
   (collect (naturals 2 5))
   '(2 3 4)))

;; Generator -> Generator
(define (sieve gen)
  (lambda ()
    (handle-yield
     (lambda (p k)
       (yield p)
       ((sieve (filter-gen k (lambda (x) (ctr-inc) (not (zero? (remainder x p))))))))
     gen)))

(module+ test
  (check-equal?
   (with-counter 0
    (lambda ()
      (collect (sieve (naturals 2 20)))))
   '(2 3 5 7 11 13 17 19)))

(module+ main
  (define n 1000)
  (with-counter 0
    (lambda ()
      (define primes (time (collect (sieve (naturals 2 n)))))
      (printf "~a primes found, ~a remainder computations\n"
              (length primes) (ctr-inc)))))
