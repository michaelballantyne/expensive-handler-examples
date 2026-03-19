#lang racket

;;
;; Terms and unification
;;

(struct var [id] #:transparent)

(define fresh-var
  (let ([id 0])
    (lambda ()
      (set! id (+ 1 id))
      (var id))))

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
  (let ((v (walk v s)))
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

(define empty-s (hash))

(define (run n f)
  (let ([v (fresh-var)])
    (map (lambda (s) (walk* v s))
         (take n (lambda () ((f v) empty-s))))))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v)
       (cons (walk* (car v) s)
             (walk* (cdr v) s)))
      (else v))))

(module+ test
  (require rackunit)

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
)

