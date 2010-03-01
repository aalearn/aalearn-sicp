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
count  ; => 2; since each call to id, when forced, will increment count


;;  * _ Exercise 4.28
; example of something where the operator needs to be forced:
;  the operator must be the result of something that is auto-converted to a thunk
(define operators (car cdr cadr))
(define a-list (list 0 1 2 3 4))
((cadr operators) a-list)

;;  * _ Exercise 4.29
; versions of fibonacci are longer without memoization
; if + is not a primitive operator, then our basic version will be very long
; if it is, we can compute some similar function, just for demonstration purposes
; which works as:
(define (plus x y) (+ x y))
(define (fib n) (if (< n 2) 1 (plus (fib (- n 1)) (fib (- n 2)))))

(define (square x)
  (* x x))
(square (id 10)) ; => 100
count ; => 1 with memoization, 2 if not


;;  * _ Exercise 4.30

; not sure about this one!

; a. print and newline are primitives, so they are executed rather than deferred.
; b. 
; Have we given printing the special property where it is executed, rather than deferred?
; If so, that would explain it.

; with original interpreter, 
; (p1 1) => 1
; (p2 1) => 1

; with the new interpreter
; (p1 1) => (1 2)
; (p2 1) => (1 2)

; c. Since the primitive procedures are getting forced anyway, Cy's recoding of eval-sequence
;  will run the code exactly as before.

; d. Cy's approach seems like the appropriate one to handle sequences, though reducing usage of sequences
;   makes the point less important.

;;  * _ Exercise 4.31
; skipped for now

;;  * _ Exercise 4.32
; In exercise 3.51-52, constructing the stream evaluates the head immediately, and if there are side effects
; (such as printing), the printing will happen when the stream is constructed; it may be more useful to have 
; it print when the stream is actively being accessed.

; As pointed out in footnote 4.31, if stream elements are themselves streams, it can be useful to have these
; lazier lists to create lazy trees and other more complex data structures.

;;  * _ Exercise 4.33
(define (list->new-style-list items)
  (if (null? items)
      '()
      (cons (car items) (list->new-style-list (cdr items)))))

(define (text-of-quotation exp) (list->new-style-list (cadr exp)))

; problematic use of "cons" here, in that it uses the cons-in-underlying-scheme
;  need to use cons-using-procedures, which much specifically be implemented
;  in the interpreter.

;;  * _ Exercise 4.34
; not sure how to install this in user-print
; one way to handle most cases is to simply set a  max for the number of items we will handle
(define (user-print-pair pair)
  (define (print-item pair n)
    (cond ((null? item) '.)
	  ((= n 0) '...)
	  (else (display (car pair))
		(display " ")
		(print-item (cdr pair) (- n 1)))))
  (print-item pair 3))

;;  * _ Exercise 4.35
; range is interpreted as inclusive of bounds
(define (an-integer-between low high)
  (require (not (> low high)))
  (amb low (an-integer-between (+ low 1) high)))

;;  * _ Exercise 4.36
; an-integer-starting-from would increase one of the numbers arbitrarily without ever increasing the other two
; This solution uses observation that i^2 + j^2 < (i+j)^2
(define (a-pythagorean-triple-above low)
  (let ((i (an-integer-starting-from low)))
    (let ((j (an-integer-between low i)))
      (let ((k (an-integer-between i (+ i j))))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;;  * _ Exercise 4.37
; Ben's example does seem like an improvement, since we don't have to iterate over all k's to check
; the equality; we're just iterating over i and j, and adding the "high" requirement on k using hsq.


;;  * _ Exercise 4.38
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

; This version has the original solution,
;  (smith cooper baker fletcher miller) + 4 more
;  (baker cooper miller fletcher smith)
;  (smith fletcher baker cooper miller)
;  (baker fletcher smith cooper miller)
;  (baker cooper smith fletcher miller)

;;  * _ Exercise 4.39
; The order does not affect the answer but if the most restrictive clauses are earlier,
; then we would make the program run faster by excluding more cases up front.
; Moving (require (> miller cooper)) up will speed things along slightly.

;;  * _ Exercise 4.40
; Before the distinct requirement, there are 5^5 possible answers
; After the distinct requirement, there are 5! or 120 possible answers. 
(define (multiple-dwelling)
  (let ((fletcher (amb 1 2 3 4 5)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (require (not (= fletcher cooper)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
	(require (distinct? (list cooper fletcher smith)))
	(require (not (= (abs (- smith fletcher)) 1)))
	(let ((miller (amb 1 2 3 4 5)))
	  (require (distinct? (list cooper fletcher miller smith)))
	  (require (> miller cooper))
	  (let ((baker (amb 1 2 3 4 5)))
	    (require (distinct? (list baker cooper fletcher miller smith)))
	    (require (not (= baker 5)))
	    (list (list 'baker baker)
		  (list 'cooper cooper)
		  (list 'fletcher fletcher)
		  (list 'miller miller)
		  (list 'smith smith))))))))

;;  * _ Exercise 4.41
; One solution is to attack this similar to the queens problem...

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (yank index items)
  (define (inner n head tail)
    (if (> n 0)
	(inner (- n 1) (append head (list (car tail))) (cdr tail))
	(append head (cdr tail))))
  (inner index '() items))

(define (permutations items)
  (if (null? items)
      (list '())
      (flatmap (lambda (i)
		 (map (lambda (x) (cons (list-ref items i) x))
		      (permutations (yank i items))))
	       (enumerate-interval 0 (- (length items) 1)))))

(define (old-style-multiple-dwelling)
  (define (allowed? baker cooper fletcher miller smith)
    (and (not (= baker 5))
	 (not (= cooper 1))
	 (and (not (= fletcher 5)) (not (= fletcher 1)))
	 (> miller cooper)
	 (not (= (abs (- smith fletcher)) 1))
	 (not (= (abs (- fletcher cooper)) 1))))
  (define (with-names solutions)
    (map (lambda (sol) 
	   (apply (lambda (baker cooper fletcher miller smith) 
		    (list
		     (list 'baker baker)
		     (list 'cooper cooper)
		     (list 'fletcher fletcher)
		     (list 'miller miller)
		     (list 'smith smith)))
		  sol))
	 solutions))	
  (with-names 
   (filter (lambda (x) (apply allowed? x))
	   (permutations (enumerate-interval 1 5)))))

(old-style-multiple-dwelling) ; => ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

;;  * _ Exercise 4.42

; lots of useful utility functions
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))
(define (or a b)
  (if a true (if b true false)))
(define (and a b)
  (if a (if b true false) false))
(define (xor a b)
  (or (and a (not b)) (and (not a) b)))
(define (cadr x) (car (cdr x)))

(define (one-true-and-one-untrue items)
  (xor (car items) (cadr items)))

(define (schoolgirls-placement)
  (let ((betty (amb 1 2 3 4 5))
	(ethel (amb 1 2 3 4 5))
	(joan  (amb 1 2 3 4 5))
	(kitty (amb 1 2 3 4 4))
	(mary  (amb 1 2 3 4 5)))
    (require
     (distinct? (list betty ethel joan kitty mary)))
    (map (lambda (letter)
	   (require (one-true-and-one-untrue letter)))
	 (list
	  (list (= kitty 2) (= betty 3))
	  (list (= ethel 1) (= joan 2))
	  (list (= joan 3) (= ethel 5))
	  (list (= kitty 2) (= mary 4))
	  (list (= mary 4) (= betty 1))))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

(schoolgirls-placement) ; => ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))

;;  * _ Exercise 4.43
(define (yacht-names)
  (define (yacht-of-named father name)
    (require (not (eq? name father))))
  (define (father dad name)
    (require (eq? dad name)))

  ; (father 'mr-moore mary-ann)
  ; (yacht-named 'colonel-downing melissa) <- l

  ; var's value = father's name
  (let ((mary-ann "Mr. Moore")
	(melissa "Sir Barnacle Hood")
	(rosalind (amb "Colonel Downing" "Mr. Hall" "Dr. Parker")))
    (yacht-of-named "Mr. Hall" rosalind)
    (let ((lorna (amb "Colonel Downing" "Mr. Hall" "Dr. Parker")))
      (yacht-of-named "Mr. Moore" lorna)
      (let ((gabrielle (amb "Colonel Downing" "Mr. Hall" "Dr. Parker")))
	(yacht-of-named gabrielle "Dr. Parker")
	(require (distinct? (list rosalind lorna gabrielle)))
	(list rosalind lorna gabrielle)))))


(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x))) 
(define (cadar x) (car (cdr (car x))))
(define (lookup object items)
  (if (null? items)
      false
      (if (eq? (caar items) object)
	  (cadar items)
	  (lookup object (cdr items)))))
(define (amb-detect proc items)
  (if (null? items)
      (amb) ; hack ?
      (if (proc (car items))
	  (car items)
	  (amb-detect proc (cdr items)))))

(define (yacht-club-daughters)
  (define (father-of daughter) (lookup 'father daughter))
  (define (yacht-of father) (lookup 'yacht father))
  (define (name-of person) (lookup 'name person))
  (define (fathers-yacht-matches-name? daughter)
    (eq? (name-of daughter) (yacht-of (father-of daughter))))
  (define (daughter-of father all-daughters) 
    (amb-detect (lambda (x) (eq? father (father-of x))) all-daughters))

  (let ((mr-moore '((name mr-moore) (yacht lorna)))
	(sir-barnacle '((name sir-barnacle) (yacht gabrielle)))
	(mr-hall '((name mr-hall) (yacht rosalind)))
	(colonel-downing '((name colonel-downing) (yacht melissa)))
	(dr-parker '((name dr-parker) (yacht mary-anne)))) ; by elimination

    (let ((mary-anne (list '(name mary-anne) (list 'father mr-moore)))
	  (melissa (list '(name melissa) (list 'father sir-barnacle)))
	  (gabrielle (list '(name gabrielle)
			   (list 'father (amb mr-hall colonel-downing dr-parker)))))
      (require (not (fathers-yacht-matches-name? gabrielle)))
      (let ((lorna (list '(name lorna)
			   (list 'father (amb mr-hall colonel-downing dr-parker)))))
	(require (not (fathers-yacht-matches-name? lorna)))
	(require (not (eq? (father-of lorna) (father-of gabrielle))))
	(let ((rosalind (list '(name rosalind)
			      (list 'father (amb mr-hall colonel-downing dr-parker)))))
	  (require (not (fathers-yacht-matches-name? rosalind)))
	  (require (not (eq? (father-of rosalind) (father-of gabrielle))))
	  (require (not (eq? (father-of rosalind) (father-of lorna))))
	  (require 
	   (eq? (yacht-of (father-of gabrielle))
		(name-of
		 (daughter-of dr-parker 
			      (list mary-anne melissa gabrielle lorna rosalind)))))
	  (father-of lorna))))))

(yacht-club-daughters) ; => ((name colonel-downing) (yacht melissa)) and no more values


(define (yacht-club-daughters-without-moore)
  (define (father-of daughter) (lookup 'father daughter))
  (define (yacht-of father) (lookup 'yacht father))
  (define (name-of person) (lookup 'name person))
  (define (fathers-yacht-matches-name? daughter)
    (eq? (name-of daughter) (yacht-of (father-of daughter))))
  (define (daughter-of father all-daughters) 
    (amb-detect (lambda (x) (eq? father (father-of x))) all-daughters))

  (let ((mr-moore '((name mr-moore) (yacht lorna)))
	(sir-barnacle '((name sir-barnacle) (yacht gabrielle)))
	(mr-hall '((name mr-hall) (yacht rosalind)))
	(colonel-downing '((name colonel-downing) (yacht melissa)))
	(dr-parker '((name dr-parker) (yacht mary-anne)))) ; by elimination

    ; changed line for mary-anne, also add mr-moore to other daughters
    (let ((mary-anne (list (list 'name 'mary-anne) 
			   (list 'father (amb mr-hall colonel-downing dr-parker mr-moore))))
	  (melissa (list (list 'name 'melissa) (list 'father sir-barnacle)))
	  (gabrielle (list (list 'name 'gabrielle)
			   (list 'father (amb mr-hall colonel-downing dr-parker mr-moore)))))
      (require (not (fathers-yacht-matches-name? gabrielle)))
      (require (not (eq? (father-of mary-anne) (father-of gabrielle))))

      (let ((lorna (list (list 'name 'lorna)
			 (list 'father (amb mr-hall colonel-downing dr-parker mr-moore)))))
	(require (not (fathers-yacht-matches-name? lorna)))
        ; add these extra distinct clauses to make sure dads differ
	(require (distinct? (map father-of (list mary-anne gabrielle lorna))))
	(let ((rosalind (list (list 'name 'rosalind)
			      (list 'father (amb mr-hall colonel-downing dr-parker mr-moore)))))
	  (require (not (fathers-yacht-matches-name? rosalind)))
	  (require (distinct? (map father-of (list mary-anne gabrielle lorna rosalind))))
	  (require 
	   (eq? (yacht-of (father-of gabrielle))
		(name-of
		 (daughter-of dr-parker 
			      (list mary-anne melissa gabrielle lorna rosalind)))))
	  (father-of lorna))))))

(yacht-club-daughters-without-moore) ; => solutions
; ((name dr-parker) (yacht mary-anne))
; ((name colonel-downing) (yacht melissa))
; that's it


;;  * _ Exercise 4.44
(define (length items)
  (define (inner n items)
    (if (null? items)
	n
	(inner (+ n 1) (cdr items))))
  (inner 0 items))
(length (list 1 2 3 4)) ; => 4

(define (filter proc items)
  (cond ((null? items) '())
	((proc (car items)) (cons (car items) (filter proc (cdr items))))
	(else (filter proc (cdr items)))))

(filter (lambda (x) (> x 4)) (list 1 9 3 6 5 2)) ; => (9 6 5)
      
(define (queens board-size)
  (define (safe? positions)
    (define (same-row? x y)
      (= (cadr x) (cadr y)))
    (define (same-diagonal? x y)
      (= (abs (- (car x) (car y))) 
	 (abs (- (cadr x) (cadr y)))))
    (= 0 (length
	  (filter (lambda (y)
		    (or (same-row? (car positions) y)
			(same-diagonal? (car positions) y)))
		  (cdr positions)))))


  (define (amb-enumerate n)
    (if (= n 1)
	(amb n)
	(amb n (amb-enumerate (- n 1)))))

  ; this seems pretty inefficient
  (define (safe-queens n positions-so-far)
    (if (= n 0)
	positions-so-far
	(let ((my-queens (cons (list n (amb-enumerate board-size)) positions-so-far)))
	  (require (safe? my-queens))
	  (safe-queens (- n 1) my-queens))))
  (safe-queens board-size '()))

(queens 4) ; =>
; ((4 3) (3 1) (2 4) (1 2))
; ((4 2) (3 4) (2 1) (1 3))
; and that's it (looks right)

(queens 5) ; => takes several seconds for first answer?
(queens 6) ; => takes a few minutes
; pretty inefficient, are there better ways to do it?
(queens 8)

;;  * _ Exercise 4.45

; Five parses of "The professor lectures to the student in the class with the cat."
; 1. 
(sentence (noun-phrase (simple-noun-phrase (article the) (noun professor))) 
	  (verb-phrase 
	   (verb-phrase
	    (verb-phrase
	     (verb lectures) 
	     (prep-phrase (prep to) (simple-noun-phrase (article the)
							(noun student))))
	    (prep-phrase (prep in) (simple-noun-phrase (article the)
						       (noun class))))
	   (prep-phrase (prep with) (simple-noun-phrase (article the)
							(noun cat)))))
; In other words, the target of the lecture is a student, and the lecturing is 
; happening in a class, and the lecture is accompanied by a cat. 
; 2. a noun-phrase wraps "student" and "in the class":
; In other words, the student is in the class, and the lecture is accompanied by a cat.
; 3. a noun-phrase wraps "class" and "with the cat"
; In other words, the lecture is to the student, and the lecture is happening in a
; class which has a cat in it.
; 4. a noun-phrase wraps "student in the class with a cat" and a noun-phrase also wraps 
; "class with a cat", meaning the lecture is to the the student, the student is in the 
; class, and the class has a cat in it.
; 5. a noun-phrase wraps "student in the class with a cat" is wrapped in a noun-phrase,
; meaning that the student is in the class, and the student is also with a cat.

;;  * _ Exercise 4.46

; Some parts of the grammar are potentially infinitely recursive.  We need our program
; to terminate in these cases by not checking the infinite versions, for example,
; (verb-phrase (verb-phrase (verb-phrase ... ))).  We do that by ensuring that the infinite
; recursion cannot happen without swallowing words, and that in turn can be accomplished
; by ensuring that the "word-swallowing" part of the code happens first.

;;  * _ Exercise 4.47

; Louis's version will infinitely recurse without "swallowing words", which happens 
; when parse-word is called.
;
; Changing the order of the amb expressions will make this even worse,
; since the infinite recursion will happen right away, without even a few parses
; that are valid at first.


;;  * _ Exercise 4.48
; adjectives:
(define adjectives '(adjective old young thrifty entitled brash))

(define (parse-decorated-noun)
  (amb (parse-word nouns)
       (list 'decorated-noun
	     (parse-word adjectives)
	     (parse-decorated-noun))))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-decorated-noun)))

(parse '(the professor lectures to the student with the cat))

(parse '(the brash young professor lectures to the entitled student))

; adverbs can be done very similarly


;;  * _ Exercise 4.49

; modified to generate instead of parse
(define (parse-word word-list)
  ; hacky way to ensure termination
  ; (require (not (null? *unparsed*)))
  ; (set! *unparsed* (cdr *unparsed*))

  (define (amb-any words)
    (cond ((null? words) (amb))
	  ((= (length words) 1) (amb (car words)))
	  (else (amb (car words) (amb-any (cdr words))))))
  (list (car word-list) (amb-any (cdr word-list))))

(parse-word nouns)

(parse '(the professor lectures to the student with the cat))
; => (sentence (simple-noun-phrase (article the) (noun student)) (verb-phrase (verb-phrase (verb studies) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))))
; i.e. "the student studies for the student for the student"
; try-again: "the student studies for the student for the professor"

; This is probably not the solution desired by the text, given what's noted in the footnote.

;;  * _ Exercise 4.50
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

; add analyze clause:
; ((ramb? exp) (analyze-ramb exp))

; re-use "yank" from above

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
	(let ((index-to-try (random (length choices))))
	  (if (null? choices)
	      (fail)
	      ((list-ref index-to-try choices) env
                           succeed
                           (lambda ()
                             (try-next (yank index-to-try choices)))))))
      (try-next cprocs))))



;;  * _ Exercise 4.51
; this one's really kind of trivial -- just remove the setting to old-var
; from analyze permanent assigment
(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (fail2))))
             fail))))

; if we had used set! instead of permanent-set! we would expect:
; (a b 1)
; (a c 1)

;;  * _ Exercise 4.52
(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-try exp) (cadr exp))
(define (if-fail-rescue exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((tproc (analyze (if-fail-try exp)))
	(rproc (analyze (if-fail-rescue exp))))
    (lambda (env succeed fail)
      (tproc env
	     succeed
	     (lambda () (rproc env succeed fail))))))

; is that right?

;;  * _ Exercise 4.53
; the procedure should try all pairs before failing
; (8 35) (3 110) (3 20)

;;  * _ Exercise 4.54
; answer from inimino
(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (false? pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))


;;  * _ Exercise 4.55
; a. all people supervised by Ben Bitdiddle;
(supervisor ?x (Bitdiddle Ben)) 

; b. the names and jobs of all people in the accounting division;
(job ?x (accounting . ?type))   

; c. the names and addresses of all people who live in Slumerville.
(address ?x (Slumerville . ?address)) 

 
;;  * _ Exercise 4.56
; a. the names of all people who are supervised by Ben Bitdiddle, together with their addresses;
(and (supervisor ?x (Bitdiddle Ben))
     (address ?x ?y))

; b. all people whose salary is less than Ben Bitdiddle's, together with their salary and Ben Bitdiddle's salary;
(and (salary ?name ?salary)
     (salary (Bitdiddle Ben) ?bensalary)
     (lisp-value > ?bensalary ?salary))

; c. all people who are supervised by someone who is not in the computer division, together with the supervisor's name and job.
(and (supervisor ?x ?supervisor)
     (not (job ?supervisor (computer . ?type)))
     (job ?supervisor ?job))


;;  * _ Exercise 4.57
(assert! (rule (can-replace ?person-1 ?person-2)
      (and (job ?person-1 ?job-1)
	   (job ?person-2 ?job-2)
	   (not (same ?person-1 ?person-2))
	   (or (same ?job-1 ?job-2)
	       (can-do-job ?job-1 ?job-2)))))

; a.
(can-replace ?x (Fect Cy D))

; b.
(and (can-replace ?cheaper ?pricier)
     (salary ?cheaper ?cheaper-salary)
     (salary ?pricier ?pricier-salary)
     (lisp-value < ?cheaper ?pricier))


;;  * _ Exercise 4.58
(rule (big-shot ?person)
      (and (job ?person (?department . ?rest))
	   (supervisor ?person ?super)
	   (job ?super (?super-department . ?super-rest))
	   (not (same ?department ?super-department))))

;;  * _ Exercise 4.59
; a.
(meeting ?department (Friday ?time))

; b. 
(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
	  (and (job ?person (?department . ?rest))
	       (meeting ?department ?day-and-time))))

; c.
(meeting-time (Hacker Alyssa P) (Wednesday ?time))

;;  * _ Exercise 4.60
; The query system tries out all possible values and returns all answers which
; match the required properties; since the lives-near rule is commutative, we 
; should see both answers.

; To eliminate duplicates we could add a requirement that is not commutative, for example
; we could compare the names of the two people (assuming the company has no two employees
; with exactly the same name) by including in lives-near a rule like:
(rule (alphabetically-before ?person-1 ?person-2)
      (lisp-value string<? (lisp-value stringify ?person-1) (lisp-value stringify ?person-2)))

; assumes an appropriate definition of stringify which converts a list to a string


;;  * _ Exercise 4.61
; next-to is actually implemented as "immediately before"
(assert! (rule (?x next-to ?y in (?x ?y . ?u))))
(assert! (rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z)))

; (?x next-to ?y in (1 (2 3) 4))
(1 next-to (2 3) in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))
(4 next-to () in (1 (2 3) 4))

; (?x next-to 1 in (2 1 3 1))
(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))

;;  * _ Exercise 4.62
(rule (last-pair (?x) (?x)))
(rule (last-pair (?u . ?v) ?x) (last-pair ?v ?x))

; This will not work correctly on (last-pair ?x (3)), since it will try to find anything
; that matches the second rule!  That means looking for any pairs that end in (3), which 
; means an infinite recursion.

;;  * _ Exercise 4.63
(rule (grandson ?g ?s)
      (and (son ?f ?s)
	   (son ?g ?f)))

(rule (son ?m ?s)
      (and (wife ?m ?w)
	   (son ?w ?s)))

(grandson Cain ?who)
(son Lamech ?who)
(grandson Methushael ?who)

;;  * _ Exercise 4.64
; Louis has changed the rule to switch where the recursion happens.  This causes out-ranked-by
;  to run repeatedly without applying the second "supervisor" filter (which would cut out the infinite cases). 
; Example: when checking if Ben is outranked by Alyssa, it evaluates the following pattern:
;  (outranked-by (Bitdiddle Ben) (Hacker Alyssa P))
;  which first checks if Alyssa is his supervisor (no)
;  then goes to run (outranked-by ?middle-manager (Hacker Alyssa P)).
;  This in turn will eventually check (outranked-by (Bitdiddle Ben) (Hacker Alyssa P))
;  which is what we were running before.  Therefore this never terminates.

 
;;  * _ Exercise 4.65
; Ben Bitdiddle satisfies the supervisor role 3 times over, so satisfies middle-manager three times over.
; The wheel rule contains an ?x variable; 3 different answers satisfy the pattern with ?middle-manager = Ben. 
; As a result Oliver Warbucks appears once as the wheel for middle manager Eben Scrooge and 3 times for Ben.

;;  * _ Exercise 4.66
; Ben's function assumes that he will get unique "facts" from the database, and therefore sum, average, etc.
; will work.  But instead Cy's result illustrates that if a set of entries in the database satisfies the patterns
; in distinct ways (i.e. if temporary variables created can have different values, as ?x does in the case of ?wheel).

; Depending on the desired behavior, one possible solution is just to accumulate the unique responses to the query,
; e.g. the accumulation-functions will accumulate the stream into a list as pairs of the form (?person ?amount)
; but only append to the list if ?person hasn't already been added to the list.  Then the sum can be computed
; by simply calling something like (accumulate + 0 (map cdr uniquified-list))


;;  * _ Exercise 4.67
; the basic idea is to keep the history of patterns and frames examined as pairs.  We have to examine the specific
; pattern-frame combination and see if it has appeared before in the history.


;;  * _ Exercise 4.68

(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z)))


(assert! (rule (reverse () ())))
(assert! (rule (reverse (?u . ?v) ?z)
	       (and (reverse ?v ?w)
		    (append-to-form ?w (?u) ?z))))

(reverse (1) ?x)      ; => (reverse (1) (1)) check
(reverse (1 2) ?x)    ; check

(reverse (1 2 3) ?x)  ; check


;;  * _ Exercise 4.69
; from text -- for convenience
(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

; from before
(assert! (rule (grandson ?g ?s)
	       (and (son ?f ?s)
		    (son ?g ?f))))
(assert! (rule (son ?m ?s)
	       (and (wife ?m ?w)
		    (son ?w ?s))))

(assert! (rule (ends-in-grandson (grandson))))
(assert! (rule (ends-in-grandson (?x . ?y))
	       (ends-in-grandson ?y)))

(ends-in-grandson (great ?x))       ; => (ends-in-grandson (great grandson)) ok
(ends-in-grandson (great great ?x)) ; ok

(assert! (rule ((great . ?rel) ?x ?y)
	       (and (son ?p ?y)
		    (?rel ?x ?p)
		    (ends-in-grandson ?rel))))

(assert! (rule ((grandson) ?x ?y)
	       (grandson ?x ?y)))

((great grandson) ?g ?ggs) ; => ok, though duplicates involved?
(?relationship Adam Irad)  ; => ((great grandson) adam irad)
(?rel Adam Jubal)          ; ok

;;  * _ Exercise 4.70
; Without the let, the set! sets both the outer THE-ASSERTIONS and the inner THE-ASSERTIONS
;  at the same time; the stream will be an infinite stream of whatever assertion is contained 
;  in the variable assertion.  Same, obviously, for the let in add-rule!

;;  * _ Exercise 4.71
; The main problem seems to be that without delaying the second stream, e.g. the one created
;  by apply-rules, you're forcing it to generate right away, which could generate an infinite loop?
;  Not sure -- create examples.


;;  * _ Exercise 4.72
; The advantage of interleave generally is that combining infinite streams otherwise would mean
;  that you'll only see items of stream 1.  So take a disjoin that's looking at an infinite stream
;  of these...


;;  * _ Exercise 4.73
; ...


;;  * _ Exercise 4.74
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (test-stream) (not (stream-null? test-stream))) stream)))

; Don't believe this should change the behavior of the program at all.

;;  * _ Exercise 4.75
(define (uniquely-asserted query frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((results (qeval (car query) (singleton-stream frame))))
       (if (and (not (stream-null? results)) (stream-null? (stream-cdr results)))
	   results
	   the-empty-stream)))
   frame-stream))
(put 'unique 'qeval uniquely-asserted)

(job ?x (computer wizard))          ; => okay from before
(unique (job ?x (computer wizard))) ; ok

(and (job ?x ?j) (unique (job ?anyone ?j))) ; ok
(and (supervisor ?someone ?x) (unique (supervisor ?one ?x))) ; => eben scrooge + alyssa hacker

;;  * _ Exercise 4.76
; original version of conjoin
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

; new version
(define (conjoin conjuncts frame-stream)
  (map-stream merge-frame-if-consistent
	      (qeval (first-conjunct conjuncts) frame-stream)
	      (qeval (first-conjunct (rest-conjuncts conjuncts)) frame-stream)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))


(define (merge-if-consistent frame1 frame2)
  (if (null? frame1)
      frame2
      (let (combined-frame 
	    (extend-if-consistent (binding-value binding) 
				  (binding-variable binding) 
				  frame2))
	(if combined-frame
	    (merge-if-consistent (cdr frame1) combined-frame)
	    false))))


;;  * _ Exercise 4.77

; The idea here is to return a promise; when a frame gets extended by a possible value, the promise checks to see if all the necessary variables in the lisp-value expression have been assigned a value.  If they have, then the lisp-value runs immediately and the expression is checked; if not, the same promise is returned.

;;  * _ Exercise 4.78

; skipped

;;  * _ Exercise 4.79

; skipped



; misc. utilities
; (restart 1)
; (initialize-data-base microshaft-data-base) 
; (query-driver-loop)