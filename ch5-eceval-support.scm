;;;;SIMULATION OF ECEVAL MACHINE OPERATIONS --
;;;;loaded by load-eceval.scm and by load-eceval-compiler.scm

;;;;FIRST A LOT FROM 4.1.2-4.1.4

(load "ch5-syntax.scm");               ;section 4.1.2 syntax procedures

;;;SECTION 4.1.3
;;; operations used by compiled code and eceval except as noted

(define (true? x)
  (not (eq? x false)))

;;* not used by eceval itself -- used by compiled code when that
;; is run in the eceval machine
(define (false? x)
  (eq? x false))

;;following compound-procedure operations not used by compiled code
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
;;(end of compound procedures)


(define (make-thunk exp env)
  (list 'thunk exp env))
(define (thunk? exp)
  (tagged-list? exp 'thunk))

(define thunk-exp cadr)
(define thunk-env caddr)

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; is there a better way to get a "distinguished condition code"?
(define unbound-variable-error '(unbound-variable))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
;;         (error "Unbound variable" var) ; original
	unbound-variable-error 		; customized
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (unbound-variable-error? val)
  (eq? val unbound-variable-error))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	;;above from book -- here are some more
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '= =)
	(list '/ /)
	(list '> >)
	(list '< <)

        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define inapplicable-arguments '(inapplicable-arguments))

(define (check-primitive-procedure proc args)
  (let ((p (cadr proc)))
    (cond ((eq? p car) (and (= (length args) 1) (pair? (car args))))
	  ((eq? p cdr) (and (= (length args) 1) (pair? (car args))))
	  ((eq? p cons) (= (length args) 2))
	  ((eq? p null?) (= (length args) 1))
	  ; ... skipping some ...
	  ((eq? p /) (and (= (length args) 2) (not (= (cadr args) 0))))
	(else true))))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (if (check-primitive-procedure proc args)
      (apply-in-underlying-scheme
       (primitive-implementation proc) args)
      inapplicable-arguments))


(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;; Simulation of new machine operations needed by
;;;  eceval machine (not used by compiled code)

;;; From section 5.4.1 footnote
(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))

;;; From section 5.4.2 footnote, for non-tail-recursive sequences
(define (no-more-exps? seq) (null? seq))

;;; From section 5.4.4 footnote
(define (get-global-environment)
  the-global-environment)
;; will do following when ready to run, not when load this file
;;(define the-global-environment (setup-environment))


;;; Simulation of new machine operations needed for compiled code
;;;  and eceval/compiler interface (not used by plain eceval machine)
;;; From section 5.5.2 footnote
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))


;;  * _ Exercise 5.39 -- for lexical address functions
(define (dec-car address)
  (cons (- (car address) 1) (cdr address)))

(define (lexical-address-lookup address env)
  (if (> (car address) 0)
      (lexical-address-lookup
       (dec-car address)
       (enclosing-environment env))
      (let ((value (list-ref 
		    (frame-values (first-frame env))
		    (cadr address))))
	(if (eq? value '*unassigned*)
	    (error "ERROR: unassigned value")
	    value))))

(define (lexical-address-set! address val env)
  (define (set-nth! items n val)
    (if (> n 0)
	(set-nth! (cdr items) (- n 1) val)
	(set-car! items val)))
  (if (> (car address) 0)
      (lexical-address-set!
       (dec-car address)
       val
       (enclosing-environment env))
      (set-nth! 
       (frame-values (first-frame env)) 
       (cadr address)
       val)))

;; from exercise 4.16, modified to add if
(define (scan-out-defines body)
  (let ((defines (filter definition? body))
	(non-defines (filter (lambda (x) (not (definition? x))) body)))
    (if (> (length defines) 0)
	(make-let 
	 (map (lambda (x) (list (definition-variable x) '*unassigned*)) defines)
	 (append
	  (map (lambda (x) (make-assignment (definition-variable x) (definition-value x))) defines)
	  non-defines))
	body)))

(define (list-index-eq x items) (list-index (lambda (i) (eq? x i)) items))

(define (find-variable var compile-time-env)
  (define (search-frame i env)
    (if (null? env)
	'not-found
	(let ((j (list-index-eq var (car env))))
	  (if j
	      (list i j)
	      (search-frame (+ i 1) (cdr env))))))
  (search-frame 0 compile-time-env))
