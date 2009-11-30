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

; note: these should return the relevant VALUES when true, not just 'true
;  this creates interesting issues in the case of or-if, where if we use a derived-form,
;  it looks like we have to evaluate something twice

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
			 (cons (recipient (cond-actions first)) (cond-predicate first))
			 (sequence->exp (cond-actions first)))
                     (expand-clauses rest))))))
; unsure if cond-predicate will pass correctly to recipient there

; note that (cond-predicate first) gets eval'd twice -- that's not how cond should work!


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
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (cons var val)))

; inimino pointed out that (set-cdr! frame frame) creates an infinite-loop! -- fixed above
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
; If the system version of map is itself implemented as a primitive, Louis's version should work.
; Since it does not, we can conclude that the system version is based on other functions, such as
; car, cdr, cons, and apply.  Since apply is not a primitive in our interpreter, the system version
; of map will use the system version of apply instead of our version, which will lead to incompatibilities.

;;  * _ Exercise 4.15
; The halting problem! First assume that halts? works and (halts? try try) returns true or false
; If (halts? try try) => true, then, inside (try try), the if condition is true, and the function
;  run-forever is triggered.  This means that (try try) does not halt, and therefore the return value
;  of (halts? try try) was inaccurate.
; If (halts? try try) => false, then the if will choose the else branch and halt, and therefore
;  once again, (halts? try try) returns an inaccurate result.
; Therefore any implementation of halts? can be shown to fail on this implementation of try try.


;;  * _ Exercise 4.16

; a.  Change lookup-variable-value to signal an error if the value it finds is the symbol *unassigned*.
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
		 (error var " is *unassigned*")
		 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; b.  Write a procedure scan-out-defines that takes a procedure body and returns an equivalent one that has no internal definitions, by making the transformation described above.
(define (make-assignment var val)
  (list 'set! var val))

(define (scan-out-defines body)
  (let ((defines (filter definition? body))
	(non-defines (filter (lambda (x) (not (definition? x))) body)))
    (make-let 
     (map (lambda (x) (list (definition-variable x) '*unassigned*)) defines)
     (append
      (map (lambda (x) (make-assignment (definition-variable x) (definition-value x))) defines)
      non-defines))))

; c.  Install scan-out-defines in the interpreter, either in make-procedure or in procedure-body. Which place is better? Why? 
; make-procedure will involve this being run less often, so it's better
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))


;;  * _ Exercise 4.17
; The diagram itself is uninteresting.  As question notes, an additional frame/environment is created
;  by introducing the let, which is transformed into a lambda.  Since the additional inner frame includes
;  everything other than the definitions, and the definitions themselves can have no side effects in this
;  frame, a correct program will run exactly as before, despite one additional environment in play.
; There are some other odd side effects, however.  For example:
(define (x y)
  (+ y 1)
  (define (a) 7))
; This function ill-advisedly makes use of the return value of the definition as its return value.  This will
;  break after the rewrite above.

; If we tolerate this sort of problem and some additional limitations, we can also rewrite the function to 
; mimic "simultaneous" scope rules by filtering out definitions & moving them to the beginning of the body.

;;  * _ Exercise 4.18
; applying this transformation to solve, this is what happens:
(lambda (f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))
; In the inner let, a and b will execute their code immediately; b will immediately call stream-map,
;  but since y is still *unassigned*, we will see an error.

; Now look at the version in the text, applied to solve
(lambda (f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))
; No such problem in this case; this should work correctly.

;;  * _ Exercise 4.19
; The idea of simultaneous and scope-level definition is best, so Eva's answer of 20 seems best.
; We could implement this as follows:
;  - As in the text, initially assign a and b to *unassigned*.
;  - When evaluating a and b, instead of erroring when you hit *unassigned*, defer additional processing
;    and move on to the next definition.
;  - If you have circular definitions, then create an error.
;  - Otherwise you have evaluated all definitions in the order they are needed, e.g. in the example in 4.19,
;    you will evaluate a first, then b finishes evaluating, then (+ a b), giving the answer of 20.

;;  * _ Exercise 4.20
; a. Implement letrec as a derived expression
(define letrec-assignments cadr)
(define letrec-body cddr)
(define letrec-assignment-var car)
(define letrec-assignment-val cadr)

(define (letrec->let exp)
  (make-let
   (map (lambda (x) (list (let-rec-assignment-var x) '*unassigned)) 
	(letrec-assignments exp))
   (append
    (map (lambda (x) (make-assignment (letrec-assignment-var x) (letrec-assignment-val x))) 
	 (letrec-assignments exp))
    (letrec-body exp))))

; b. Diagrams to illustrate difference between letrec and let:
;  letrec:
;  ____________________________
; | global                     |
; |____________________________| 
;               ___________________________
;              | even? odd? = *unassigned* |
;              |___________________________|  <-\
;                    _______________________________________
;                   | set! changes even?, odd? to lambdas   |
;                   | body runs here                        |
;                   |_______________________________________|
;
;  let:
;  ____________________________
; | global                     |
; |____________________________| 
;                    ____________________________________________
;                   | even? odd? = lambdas                       |
;                   |  but even?, odd? not defined in parent env |
;                   | body runs here                             |
;                   |____________________________________________|
;
; Not very clearly illustrated by my diagrams, but in the "let" case, even and odd
;  are mapped to lambdas but the lambdas refer to even? and odd? in the parent evironment
;  where they are undefined.
; 
; In the letrec case, we set even and odd in the environment they will execute and
;  when they execute, they will find the definitions of those functions


;;  * _ Exercise 4.21
; a.
(define (fact n)
  (if (= n 1) 1 (* n (fact (- n 1)))))
(fact 10) ; => 3628800 

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10) ; => 3628800 check!


(define (fib n)
  (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2)))))
(fib 10) ; => 55

((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fb k)
      (if (< k 3)
          1
          (+ (fb fb (- k 2)) (fb fb (- k 1)))))))
 10) ; => 55 check!

; b.
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))
(f 19) ; => #f check!
(f 20) ; => #t check!

;;  * _ Exercise 4.22
; if we extend the analyze to understand lets...
; ((let? exp) (analyze-let exp))
; ... then we just need the following
(define (analyze-let exp)  
  (let ((var-procs (map analyze (map let-assignment-var (let-assignments exp))))
	(bproc (analyze-sequence (let-body exp))))
    (lambda (env) (execute-application 
		   (make-procedure vars bproc env)
		   (map (lamda (p) (p env) var-procs))))))
; not sure about this one
; get this one working in the meta-circular evaluator!

;;  * _ Exercise 4.23
; In a 1-expression body, Alyssa's version will analyze that expression ahead of time
;  and when actually evaluated, execute-sequence gets called, the first branch of the cond
;  is true, and we quickly go to run the single expression.  The text's version is similar
;  except that cdr procs does not need to be checked at execution time.
; In a 2-expression body, Alyssa's version needs to continually check the length of the
;  expressions in the body during execution time, and it is therefore doing a lot more work
;  at execution time than the text's version.

;;  * _ Exercise 4.24
; skipped!

;;  * _ Exercise 4.25
; In applicative order code, this will not work, since every call to unless will attempt to
;  evaluate all arguments, including the recursive call to factorial, so the program will
;  recurse infinitely.
; In a normal order language, yes, the function will work as desired.


;;  * _ Exercise 4.26
(define unless-condition cadr)
(define unless-usual-value caddr)
(define unless-exception-value cadddr)

(define (unless->if exp)
  (make-if (unless-condition exp)
	   (unless-exception-value exp)
	   (unless-usual-value exp)))

; Unless might be useful as a procedure when passed in as an argument, e.g. to map...
(map unless possible-error-conditions regular-values exception-values)


;;  * _ Exercise 4.27
; give following to lazy evaluator input...
(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))
count  ; => Value: 0; original definition
w      ; => 10
count  ; => 1; memoization means that the set is called once


;;  * _ Exercise 4.28
; example of something where the operator needs to be forced:
;  the operator must be the result of something that is 
(define operators (car cdr cadr))
(define a-list (list 0 1 2 3 4))
((cadr operators) a-list)

