#lang racket

;; An even tinier microbenchmark that exhibits similar control flow as
;; the mk example but without needing the mechanics of fresh, unify, etc.

;; Implementing counter effect via set!
#;(begin
  (define ctr 0)
  (define ctr-inc
    (lambda ()
      (set! ctr (+ 1 ctr))
      ctr))
  (define (with-ctr-inc n thunk)
    (set! ctr (- n 1))
    (thunk)))

;; Implementing counter effect by an outer handler
(begin
  (define ctr-inc-tag (make-continuation-prompt-tag))
  
  (define (ctr-inc)
    (call-with-composable-continuation
     (lambda (k)
       (abort-current-continuation ctr-inc-tag k))
     ctr-inc-tag))
  
  (define (handle-ctr-inc handler body)
    (call-with-continuation-prompt
     body
     ctr-inc-tag
     handler))
  
  (define (with-ctr-inc n thunk)
    (handle-ctr-inc
     (lambda (k)
       (with-ctr-inc (+ n 1) (lambda () (k n))))
     thunk)))

(module+ test
  (require rackunit)
  (check-equal?
   (with-ctr-inc 0
     (lambda ()
       (define v1 (ctr-inc))
       (define v2 (ctr-inc))
       (list v1 v2)))
   (list 0 1)))


(define succeed-tag (make-continuation-prompt-tag))

(define (succeed v)
  (call-with-composable-continuation
   (lambda (k)
     (abort-current-continuation succeed-tag v k))
   succeed-tag))

(define (fail) (void))

(define (assign key val)
  (lambda (st)
    (ctr-inc)
    ;;(printf "assign ~a to ~a\n" key val)
    ;;(printf "succeed prompts: ~a\n" (prompt-count-for succeed-tag))
    ;;(printf "ctr prompts: ~a\n" (prompt-count-for ctr-inc-tag))
    (if (hash-has-key? st key)
        (if (equal? val (hash-ref st key))
            (succeed st)
            (fail))
        (succeed (hash-set st key val)))))

(define (disj g1 g2)
  (lambda (st)
    (g1 st)
    (g2 st)))

(define (conj g1 g2)
  (lambda (st)
    (append-map (lambda () (g1 st)) g2)))

(define (append-map thunk g2)
  (handle-succeed
   (lambda (v k)
     (g2 v)
     (append-map k g2))
   thunk))

(define (handle-succeed handler body)
  (call-with-continuation-prompt
   body
   succeed-tag
   handler))

(define (take-all thunk)
  (handle-succeed
   (lambda (v k)
     (cons v (take-all k)))
   (lambda ()
     (thunk)
     '())))

(define empty-s (hash))

(define (run f)
  (with-ctr-inc 0
    (lambda ()
      (define res (take-all (lambda () (f empty-s))))
      (printf "explored ~a assignments to find ~a solutions\n" (ctr-inc) (length res))
      res)))

(define (check key pred)
  (lambda (st)
    (ctr-inc)
    (if (and (hash-has-key? st key)
             (pred (hash-ref st key)))
        (succeed st)
        (fail))))

(define (sieve var min max)
  (lambda (st)
    (if (>= min max)
        (fail)
        ((disj
           (assign var min)
           (conj (sieve var (+ min 1) max)
                 (check var (lambda (v) (not (zero? (remainder v min)))))))
         st))))

(module+ main
  (define n 300)
  #;(run (sieve 'x 2 n))
  (time (length (run (sieve 'x 2 n)))))