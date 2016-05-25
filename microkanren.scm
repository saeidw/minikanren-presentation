#lang racket

(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var==? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

(define (assp proc alist)
  (cond
    ((null? alist) #f)
    ((proc (car (car alist))) (car alist))
    (else (assp proc (cdr alist)))))

(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var==? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(define (occurs-check x v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) (var==? v x))
      (else (and (pair? v) (or (occurs-check x (car v) s)
                               (occurs-check x (cdr v) s)))))))

(define (ext-s x v s)
  (if (occurs-check x v s) #f `((,x . ,v) . ,s)))

(define mzero '())
(define (unit s/c) (cons s/c mzero))

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var==? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

(define (mplus S1 S2)
  (cond
    ((null? S1) S2)
    ((procedure? S1) (lambda () (mplus S2 (S1))))
    (else (cons (car S1) (mplus S2 (cdr S1))))))

(define (bind S g)
  (cond
    ((null? S) mzero)
    ((procedure? S) (lambda () (bind (S) g)))
    (else (mplus (g (car S)) (bind (cdr S) g)))))

(define (disj g1 g2)
  (lambda (s/c)
    (mplus (g1 s/c) (g2 s/c))))

(define (conj g1 g2)
  (lambda (s/c)
    (bind (g1 s/c) g2)))

(define empty-state '(() . 0))

(define-syntax snooze
  (syntax-rules ()
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) (snooze g))
    ((_ g0 g ...) (conj (snooze g0) (conj+ g ...)))))

(define-syntax disj+
  (syntax-rules ()
    ((_ g) (snooze g))
    ((_ g0 g ...) (disj (snooze g0) (disj+ g ...)))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh (lambda (x0) (fresh (x ...) g0 g ...))))))

(define (pull S)
  (if (procedure? S) (pull (S)) S))

(define (take-all S)
  (let ((S (pull S)))
    (if (null? S) '() (cons (car S) (take-all (cdr S))))))

(define (take n S)
  (if (zero? n) '()
      (let ((S (pull S)))
        (cond
          ((null? S) '())
          (else (cons (car S) (take (- n 1) (cdr S))))))))

(define (mK-reify s/c*)
  (map reify-state/1st-var s/c*))

(define (reify-state/1st-var s/c)
  (let ((v (walk* (var 0) (car s/c))))
    (walk* v (reify-s v '()))))

(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
      ((var? v)
       (let ((n (reify-name (length s))))
         (cons `(,v . ,n) s)))
      ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
      (else s))))

(define (reify-name n)
  (string->symbol
   (string-append "_" "." (number->string n))))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v) (cons (walk* (car v) s)
                       (walk* (cdr v) s)))
      (else v))))

(define (call/empty-state g)
  (g empty-state))

(define-syntax run
  (syntax-rules ()
    ((_ n (x ...) g0 g ...)
     (mK-reify (take n (call/empty-state
                        (fresh (x ...) g0 g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (mK-reify (take-all (call/empty-state
                          (fresh (x ...) g0 g ...)))))))

(define (appendo l s out)
  (conde
   ((== '() l) (== s out))
   ((fresh (a d)
           (== `(,a . ,d) l)
           (fresh (res)
                  (== `(,a . ,res) out)
                  (appendo d s res))))))

(define test
  (run* (q)
       (fresh (x y)
              (appendo x y (list 1 2 3 4 5 6))
              (== `(,x . ,y) q))))