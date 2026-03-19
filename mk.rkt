#lang racket

;;
;; Global counter effect, used for allocating fresh variables
;; 

;; Implementing counter effect via set!
#;(begin
  (define ctr 0)
  (define fresh-var
    (lambda ()
      (set! ctr (+ 1 ctr))
      (var ctr)))
  (define (with-fresh-var n thunk)
    (set! ctr (- n 1))
    (thunk)))

;; Implementing counter effect by an outer handler
(begin
  (define fresh-var-tag (make-continuation-prompt-tag))
  
  (define (fresh-var)
    (call-with-composable-continuation
      (lambda (k)
        (abort-current-continuation fresh-var-tag k))
      fresh-var-tag))
  
  (define (handle-fresh-var handler body)
    (call-with-continuation-prompt
     body
     fresh-var-tag
     handler))
  
  (define (with-fresh-var n thunk)
    (handle-fresh-var
      (lambda (k)
        (with-fresh-var (+ n 1) (lambda () (k (var n)))))
      thunk)))

(module+ test
  (require rackunit)
  (check-equal?
    (with-fresh-var 0
     (lambda ()
       (define v1 (fresh-var))
       (define v2 (fresh-var))
       (list v1 v2)))
    (list (var 0) (var 1))))

;;
;; Terms and unification
;;

(struct var [id] #:transparent)

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (equal? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

(define (walk u s)
  (if (and (var? u) (hash-has-key? s u))
      (walk (hash-ref s u) s)
      u))

(define (ext-s x v s)
  (if (occurs-check x v s)
    #f
    (hash-set s x v)))

(define (occurs-check x v s)
  #f
  #;(let ((v (walk v s)))
    (cond
      ((var? v) (equal? v x))
      ((pair? v)
       (or (occurs-check x (car v) s)
           (occurs-check x (cdr v) s)))
      (else #f))))


;;
;; Goals
;;

(define (== t1 t2)
  (lambda (st)
    (let ([st^ (unify t1 t2 st)])
      (if st^
          (succeed st^)
          (fail)))))

(define (call/fresh f)
  (lambda (st)
    ((f (fresh-var)) st)))

(define succeed-tag (make-continuation-prompt-tag))

(define (fail) (void))

(define (succeed v)
  (call-with-composable-continuation
    (lambda (k)
      (abort-current-continuation succeed-tag v k))
    succeed-tag))

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

(define (disj g1 g2)
  (lambda (st)
    (g1 st)
    (g2 st)))

(define empty-s (hash))


;;
;; `run` interface
;;

(define (run n f)
  (with-fresh-var 0
    (lambda ()
      (let ([v (fresh-var)])
        (map (lambda (s) (walk* v s))
             (take n (lambda () ((f v) empty-s))))))))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v)
       (cons (walk* (car v) s)
             (walk* (cdr v) s)))
      (else v))))

(define (take n-or-f thunk)
  (reverse (take-acc n-or-f thunk '())))

(define (take-acc n-or-f thunk acc)
  (if (eqv? n-or-f 0)
      acc
      (handle-succeed
        (lambda (v k)
          (take-acc (and n-or-f (- n-or-f 1)) k (cons v acc)))
        (lambda ()
          (thunk)
          acc))))


;;
;; Relations
;;

(define (appendo l1 l2 out)
  (lambda (st)
    ((disj
       (conj (== l1 '()) (== l2 out))
       (call/fresh (lambda (a)
         (call/fresh (lambda (d)
           (call/fresh (lambda (res)
             (conj (== l1 (cons a d))
                   (conj 
                      (appendo d l2 res)
                      (== out (cons a res)))))))))))
       st)))


;;
;; Benchmark
;;

;; A benchmark query where almost all the cost is due to calling the `fresh-var`
;; handler. Key characteristics:
;;  - The appendo recursion sets up a conjunction with the recursion as g1, so we
;;    build up a stack of `succeed-tag` prompts.
;;      (If you swap the order of the recursive call and unification with out in
;;       appendo, you can see what happens without the deep stack of prompts.)
;;  - The query only has one solution, so succeed effects only bubble locally and
;;    don't dominate the cost.
;;  - I commented out the occurs check so that it doesn't dominate the cost.
(module+ main
  (define n 5000)
  (time
    (length
      (run (+ n 1) (lambda (r) (appendo (range n) r (range n)))))))


;;
;; Tests
;;

(module+ test
  ;; Basic unification
  (check-equal?
   (run 2 (lambda (v) (== v 1)))
   '(1))

  ;; Disjunction
  (check-equal?
   (run #f (lambda (v) (disj (== v 1) (== v 2))))
   '(1 2))

  ;; take limits results
  (check-equal?
   (run 1 (lambda (v) (disj (== v 1) (== v 2))))
   '(1))

  ;; Failure produces no results
  (check-equal?
   (run #f (lambda (_) (== 1 2)))
   '())

  ;; call/fresh inside run
  (check-equal?
   (run #f (lambda (x)
             (call/fresh (lambda (y)
               (conj (== x y) (== y 3))))))
   '(3))

  ;; Conjunction
  (check-equal?
   (run #f (lambda (x)
             (call/fresh (lambda (y)
               (conj (disj (== x 1) (== x 2))
                     (== y 3))))))
   '(1 2))

  ;; Nested disjunction
  (check-equal?
   (run #f (lambda (v)
             (disj (== v 'a) (disj (== v 'b) (== v 'c)))))
   '(a b c))

  ;; Conjunction of two disjunctions
  (check-equal?
   (run #f (lambda (q)
             (call/fresh (lambda (x)
               (call/fresh (lambda (y)
                 (conj (== q (list x y))
                       (conj (disj (== x 1) (== x 2))
                             (disj (== y 'a) (== y 'b))))))))))
   '((1 a) (1 b) (2 a) (2 b)))

  ;; appendo: forward mode
  (check-equal?
   (run #f (lambda (q) (appendo '(1 2) '(3 4) q)))
   '((1 2 3 4)))
  ;; appendo: empty first list
  (check-equal?
   (run #f (lambda (q) (appendo '() '(a b) q)))
   '((a b)))

  ;; appendo: empty second list
  (check-equal?
   (run #f (lambda (q) (appendo '(a b) '() q)))
   '((a b)))

  ;; appendo: both empty
  (check-equal?
   (run #f (lambda (q) (appendo '() '() q)))
   '(()))

  ;; appendo: decompose first list
  (check-equal?
   (run 1 (lambda (q) (appendo q '(3 4) '(1 2 3 4))))
   '((1 2)))

  ;; appendo: decompose second list
  (check-equal?
   (run #f (lambda (q) (appendo '(1 2) q '(1 2 3 4))))
   '((3 4)))

  ;; appendo: enumerate all splits
  (check-equal?
   (run 4 (lambda (q)
             (call/fresh (lambda (l)
               (call/fresh (lambda (r)
                 (conj (appendo l r '(1 2 3))
                       (== q (list l r)))))))))
   '((() (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) ())))
)