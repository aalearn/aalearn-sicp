;; * _ Section 4.1

;;  * _ Exercise 4.1
; left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env))) 
	(cons left
	      (list-of-values (rest-operands exps) env)))))


; right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (rest-operands exps) env)))  
	(cons (eval (first-operand exps) env)
	      right))))

;;  * _ Exercise 4.2
; a. Louis's plan doesn't work because application? returns true for any pair,
;    which means that defines and all other special forms will not have their
;    special logic run.

; b. To make it possible to trap applications as a special form:
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

;;  * _ Exercise 4.3
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))

	((get 'eval (operator exp)) ((get 'eval (operator exp)) exp env))

        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


; change signature to receive exp and env, though env is useless
;  we need the same signature for all of these procedures
(define (text-of-quotation exp env) (cadr exp))

(put 'eval '(quote) text-of-quotation)
(put 'eval '(set!) eval-assignment)
(put 'eval '(define) eval-definition)
(put 'eval '(if) eval-if)

; some of these aren't written out in book
(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
		  (lambda-body exp)
		  env))
(put 'eval '(lambda) eval-lambda)

(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))
(put 'eval '(begin) eval-begin)

(define (eval-cond exp env)
  (eval (cond->if exp) env))
(put 'eval '(cond) eval-cond)


;;  * _ Exercise 4.4
; implementing as derived forms:
(define and-exps cdr)
(define (and->if exp)
  (expand-and (and-exps exp)))
(define (expand-and exps)
  (let ((clauses (and-exps exp)))
    (if (null? clauses)
	'true
	(make-if (car clauses)
	       (expand-and (cons 'and (cdr clauses)))
	       'false))))

(define or-exps cdr)
(define (or->if exp)
  (expand-or (or-exps exp)))
(define (expand-or exps)
  (let ((clauses (or-exps exp)))
    (if (null? clauses)
	'false
	(make-if (car clauses)
	       'true
	       (expand-or (cons 'or (cdr clauses)))))))


;;  * _ Exercise 4.5
(define (arrow-form? actions) (eq? '=> (car actions)))
(define (recipient actions) (cadr actions))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
		     (if (arrow-form? (cond-actions first))
			 ((recipient (cond-actions first)) (cond-predicate first))
			 (sequence->exp (cond-actions first)))
                     (expand-clauses rest))))))
; unsure if cond-predicate will pass correctly to recipient there


;;  * _ Exercise 4.6
(define let-assignments cadr)
(define let-body cddr)
(define let-assignment-var car)
(define let-assignment-val cadr)
 
(define (let->combination exp)
  (cons (make-lambda (map let-assignment-var (let-assignments exp))
		     (let-body exp))
	(map let-assignment-val (let-assignments exp))))

(define (let? exp) (tagged-list? exp 'let))

(define (eval exp env)
  ; ...
        ((let? exp) (eval (let->combination exp) env))
  ; ...
)

(let->combination '(let ((x 4) (y 5)) (+ x y)))
; =>  ((lambda (x y) (+ x y)) 4 5)

(let->combination '(let ((x 4) (y 5)) (+ x x) (+ x y)))

;;  * _ Exercise 4.7
(define (make-let assignments body)
  (cons 'let (cons assignments body)))

(define (let*->nested-lets exp)
  (expand-let* (let-assignments exp) (let-body exp)))


(define (expand-let* assignments body)
  (make-let (list (car assignments))
	    (if (null? (cdr assignments))
		body
		(list (expand-let* (cdr assignments) body)))))

(let-body '(let* ((x 3) (y (+ x 2))) (* x y)))
(let-assignments '(let* ((x 3) (y (+ x 2))) (* x y)))

(let*->nested-lets '(let* ((x 3) (y (+ x 2))) (+ x x) (* x y)))
; => (let ((x 3)) (let ((y (+ x 2))) (+ x x) (* x y)))
; => 15

; yes, it does seem as if (eval (let*->nested-lets exp) env) will handle it.


;;  * _ Exercise 4.8
(define (make-begin seq) (cons 'begin seq)) ; from book

(define (named-let? exp) (not (pair? (cadr exp))))
(define named-let-name cadr)
(define named-let-assignments caddr)
(define named-let-body cdddr)

(define (make-define name-and-parameters body)
  (cons 'define (cons name-and-parameters body)))
 
(make-define '(tester x) '(* x x))
; => (define (tester x) (* x x))

(define (let->combination exp)
  (if (named-let? exp)
      (make-begin
       (list (make-define (cons (named-let-name exp) 
				(map let-assignment-var (named-let-assignments exp)))
			  (named-let-body exp))
	     (cons (named-let-name exp) 
		   (map let-assignment-val (named-let-assignments exp)))))  
       (cons (make-lambda (map let-assignment-var (let-assignments exp))
			  (let-body exp))
	     (map let-assignment-val (let-assignments exp)))))


(let->combination '(let ((x 4) (y 5)) (+ x x) (+ x y)))
; => ((lambda (x y) (+ x x) (+ x y)) 4 5)  -- still works
(let->combination 
 '(let fib-iter ((a 1)
                 (b 0)
                 (count 8))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))
; => (begin (define (fib-iter a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))) (fib-iter 1 0 8))
; => 21

;;  * _ Exercise 4.9
; example 1: (for start end proc)
;  proc takes one variable
;  start and end are integers, end >= start
(define for-start cadr)
(define for-end caddr)
(define for-proc cadddr)

(define (for->if exp)
  (make-if (> start end)
	   (make-begin
	    (list

; skipped for now!


  
;;  * _ Exercise 4.10
; a new syntax:
; procedures are applied by being a single token or forming a list like: 
; (arg1 arg2 -> fn) or just (fn)
(define (application? exp) 
  (and (pair? exp)
       (or (eq? (length exp) 1)
	   (eq? (list-ref exp (- (length exp) 2)) '->))))

(define (operator exp) (list-ref exp (- (length exp) 1)))
(define (operands exp) 
  (if (> (length exp) 2)
      (cons (car exp) (operands (cdr exp)))
      ()))

(operator '(3 4 -> +)) ; => +
(operands '(3 4 -> +)) ; => (3 4)

; similarly, change (if predicate consequent alternative) to (predicate ? consequent alternative)
(define (if? exp) (eq? (cadr exp) '?))
(define (if-predicate exp) (car exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))


;;  * _ Exercise 4.11
(define (make-frame variables values)
  (map cons variables values))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame frame)
  (set-car! frame (cons var val)))

; check if set-variable-value! and define-variable! work fine -- probably they don't

;;  * _ Exercise 4.12
(define (scan-frame var frame if-found if-not-found)
  (define (scan-vars vars vals)
    (cond ((null? vars) (if-not-found))
	  ((eq? var (car vals)) (if-found vals))          ; non-intuitive
	  (else (scan-vars var (cdr vars) (cdr vals))))) 
  (scan-vars (frame-variables frame)
	     (frame-values frame)))

(define (env-loop-scan-do env var proc)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (scan-frame var
		  (first-frame env)
		  proc
		  (env-loop-scan-do (enclosing-environment env) var proc))))
    
; why does the book create an internal env-loop definition, when recursing is simpler?

(define (lookup-variable-value var env)
  (env-loop-scan-do env var car))

(define (set-variable-value! var val env)
  (env-loop-scan-do env var (lambda (vals) (set-car! vals val)))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan-frame var
		frame
		(lambda (vals) set-var! vals val)
		(lambda () (add-binding-to-frame! var val frame)))))

;;  * _ Exercise 4.13
; Since our implementation of set-variable-value! allows us to change state outside of the current frame,
; by analogy it is more intuitive to allow make-unbound! to operate in any enclosing frame.

; to make this work, if-found needs to operate on both vals and vars:
(define (scan-frame var frame if-found if-not-found)
  (define (scan-vars vars vals)
    (cond ((null? vars) (if-not-found))
	  ((eq? var (car vals)) (if-found vars vals))          ; non-intuitive
	  (else (scan-vars var (cdr vars) (cdr vals))))) 
  (scan-vars (frame-variables frame)
	     (frame-values frame)))

(define (delete-head! list)
  (set-car! list (cadr list))
  (set-cdr! list (cddr list)))

(define (make-unbound! var env)
  (env-loop-scan-do env var (lambda (vars vals)
			      (delete-head! vars)
			      (delete-head! vals))))

;;  * _ Exercise 4.14
; 
