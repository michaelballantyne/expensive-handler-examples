#lang racket

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

;; collect : Generator -> (listof any)
(define (collect gen)
  (handle-yield
   (lambda (v k)
     (cons v (collect k)))
   (lambda ()
     (gen)
     '())))

;; filter-gen : Generator (any -> boolean) -> Generator
(define (filter-gen gen pred)
  (lambda ()
    (handle-yield
     (lambda (v k)
       (when (pred v) (yield v))
       ((filter-gen k pred)))
     gen)))

;; sieve : Generator -> Generator
(define (sieve gen)
  (lambda ()
    (handle-yield
     (lambda (p k)
       (yield p)
       ((sieve (filter-gen k (lambda (x) (ctr-inc) (not (zero? (remainder x p))))))))
     gen)))

;; naturals : int int -> Generator
(define (naturals min max)
  (lambda ()
    (when (< min max)
      (yield min)
      ((naturals (+ min 1) max)))))

(module+ main
  (define n 1000)
  (with-counter 0
    (lambda ()
      (define primes (time (collect (sieve (naturals 2 n)))))
      (printf "~a primes found, ~a remainder computations\n"
              (length primes) (ctr-inc)))))
