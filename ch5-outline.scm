
;;  * _ Exercise 5.1
; data path
[p]-c to p>[c]
[c]->[>]
[n]->[>]
[p]->[multiply]
[c]->[multiply]
[multiply]-m to x>[x]
[c]->[plus]
[1]->[plus]
[plus]-plus to c>[c]
[x]-x to p>[p]
[1]-1 to p>[p]
[1]-1 to c>[c]

; http://yuml.me/diagram/class/[p]-c to p>[c], [c]->[>], [n]->[>], [p]->[multiply], [c]->[multiply], [multiply]-m to x>[x], [c]->[plus], [1]->[plus], [plus]-plus to c>[c], [x]-x to p>[p], [1]-1 to p>[p], [1]-1 to c>[c].

; control diagram
[start]->[1 to p]
[1 to p]->[1 to c]
[1 to c]->[>]
[>]->[done]
[>]->[m to x]
[m to x]->[x to p]
[x to p]->[plus to c]
[plus to c]->[>]

; http://yuml.me/diagram/class/[start]->[1 to p], [1 to p]->[1 to c], [1 to c]->[>], [>]->[done], [>]->[m to x], [m to x]->[x to p], [x to p]->[plus to c], [plus to c]->[>].


;;  * _ Exercise 5.2
; assume reg n already has the number we want the factorial of
(controller
 (assign p (const 1))
 (assign c (const 1))
 test-c
   (test (op >) (reg c) (reg n))
   (branch (label factorial-done))
   (assign p (op *) (reg p) (reg c))
   (assign c (op +) (const 1) (reg c))
   (goto (label test-c))
 factorial-done)

;;  * _ Exercise 5.3

; simple versions, assuming good-enough? and improve are primitives
; data path diagram:
; http://yuml.me/diagram/class/[1]-1 to g>[g], [g]->[good enough], [g]->[improve], [improve]->[i], [i]-i to g>[g].

(controller
 (assign g (const 1.0))
 test-good-enough
   (test good-enough? (reg g))
   (branch (label sqrt-done))
   (assign i improve (reg g))
   (assign g (reg i))
   (goto (label test-good-enough))
 sqrt-done)

; now, the controller, expanded... (data path omitted)
(controller
 (assign g (const 1.0))
 test-good-enough
   (assign s (op *) (reg g) (reg g))
   (assign d (op -) (reg x) (reg s))
   (test (op >) (reg d) (const 0.0))
   (branch (label test-difference))
   (assign d (op -) 0.0 (reg d))
 test-difference
   (test (op >) (const 0.001) (reg d))
   (branch (label sqrt-done))
   
   (assign q (op /) (reg x) (reg g)) ; improve
   (assign a (op +) (reg q) (reg g))
   (assign g (op /) (reg a) (const 2.0))

   (goto (label test-good-enough))
 sqrt-done)

;;  * _ Exercise 5.4
; diagrams skipped
; a. recursive exponentiation
(controller
   (assign continue (label expt-done))     ; set up final return address
 expt-loop
   (test (op =) (reg n) (const 0))
   (branch (label base-case))

   ;; set up for recursion
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-expt))
   (goto (label expt-loop))
 after-expt
   (restore n)
   (restore continue)
   (assign val (op *) (reg b) (reg val))   ; val now contains b*b^(n - 1)
   (goto (reg continue))                   ; return to caller
 base-case
   (assign val (const 1))                  ; base case: b^0 = 1
   (goto (reg continue))                   ; return to caller
 expt-done)

; b. iterative exponentiation
; this one can be done with or without a stack, choosing to do it 
; without a stack...
(controller
  (assign p (const 1))
 test-expt
  (test (op =) (reg n) 0)
  (branch (label expt-done))
  (assign p (op *) (reg b) (reg p))
  (assign n (op -) (reg n) (const 1))
  (goto (label test-expt))
 expt-done)

;;  * _ Exercise 5.5
; 1. compute factorial of 3
; continue = l:fact-done
; @ fact-loop
; continue = l:fact-done  > stack = [ l:fact-done ]
; n = 3                   > stack = [ 3, l:fact-done ]
; n = 2
; continue = l:after-fact
; @ fact-loop
; continue = l:after-fact > stack = [ l:after-fact, 3, l:fact-done ]
; n = 2                   > stack = [ 2, l:after-fact, 3, l:fact-done ]
; n = 1
; continue = l:after-fact
; @ fact-loop
; @ base-case
; val = 1
; @ after-fact
; n = 2                   < stack = [ l:after-fact, 3, l:fact-done ]
; continue = l:after-fact < stack = [ 3, l:fact-done ]
; val = 2
; @ after-fact
; n = 3                   < stack = [ l:fact-done ]
; continue = l:fact-done  < stack = []
; val = 6
; @ fact-done

; 2. compute fib(3)
; continue = fib-done
; @ fib-loop
; continue = fib-done     > stack = [ fib-done ]
; continue = afterfib-n-1
; n = 3                   > stack = [ 3, fib-done ]
; n = 2
; @ fib-loop
; continue = afterfib-n-1 > stack = [ afterfib-n-1, 3, fib-done ]
; continue = afterfib-n-1
; n = 2                   > stack = [ 2, afterfib-n-1, 3, fib-done ]
; n = 1
; @ fib-loop
; @ immediate-answer
; val = 1
; @ afterfib-n-1
; n = 2                   < stack = [ afterfib-n-1, 3, fib-done ]
; continue = afterfib-n-1 < stack = [ 3, fib-done ]
; n = 0
; continue = afterfib-n-1 > stack = [ afterfib-n-1, 3, fib-done ]
; continue = afterfib-n-2
; val = 1                 > stack = [ 1, afterfib-n-1, 3, fib-done ]
; @ fib-loop
; @ immediate-answer
; val = 0
; @ afterfib-n-2
; n = 0
; val = 1                 < stack = [ afterfib-n-1, 3, fib-done ]
; continue = afterfib-n-1 < stack = [ 3, fib-done ]
; val = 1 + 0 = 1
; @ afterfib-n-1
; n = 3                   < stack = [ fib-done ]
; continue = fib-done     < stack = []
; n = 1
; continue = fib-done     > [ fib-done ]
; continue = afterfib-n-2
; val = 1                 > [ 1, fib-done ]
; @ fib-loop
; @ immediate-answer
; val = 1
; @ afterfib-n-2
; n = 1
; val = 1                 < [ fib-done ]
; continue = fib-done     < []
; val = 1 + 1 = 2
; @ fib-done


;;  * _ Exercise 5.6

; between labels afterfib-n-1 and afterfib-n-2, remove:
  (restore continue)
  ...
  (save continue)


;;  * _ Exercise 5.7
(define expt-machine
  (make-machine
   '(n b val continue)
   (list (list '* *) (list '- -) (list '= =))
   '(
     (assign continue (label expt-done))     ; set up final return address
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     
     ;; set up for recursion
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore n)
     (restore continue)
     (assign val (op *) (reg b) (reg val))   ; val now contains b*b^(n - 1)
     (goto (reg continue))                   ; return to caller
     base-case
     (assign val (const 1))                  ; base case: b^0 = 1
     (goto (reg continue))                   ; return to caller
     expt-done)))

(set-register-contents! expt-machine 'n 5)
(set-register-contents! expt-machine 'b 4)
(start expt-machine)
(get-register-contents expt-machine 'val)    ; => 1024 = 4^5

; iterative
(define expt-machine-iterative
  (make-machine
   '(n b p)
   (list (list '* *) (list '- -) (list '= =))
   '(
     (assign p (const 1))
     test-expt
     (test (op =) (reg n) (const 0))
     (branch (label expt-done))
     (assign p (op *) (reg b) (reg p))
     (assign n (op -) (reg n) (const 1))
     (goto (label test-expt))
     expt-done)))

(set-register-contents! expt-machine-iterative 'n 4)
(set-register-contents! expt-machine-iterative 'b 5)
(start expt-machine-iterative)
(get-register-contents expt-machine-iterative 'p) ; => 625

;;  * _ Exercise 5.8

; Looks like the first instance of a label will "win", since both versions
; are stored in the table, and assoc will choose the first label that qualifies.

; modified extract-labels
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
	       (if (assoc next-inst labels)
		   (error "Duplicate label -- ASSEMBLE" next-inst)
		   (receive insts
			    (cons (make-label-entry next-inst
						    insts)
				  labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))


;;  * _ Exercise 5.9
(define (operable? exp)
  (or (register-exp? exp) (constant-exp? exp)))
 
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
		(if (operable? e)
		    (make-primitive-exp e machine labels)
		    (error "Cannot operate on item -- ASSEMBLE" op e)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;;  * _ Exercise 5.10

; Not clear how meaningfully different the syntax can get without changing
; the requirement that the first key is a keyword as identified in
; make-execution-procedure.  So a more interesting syntax might require 
; introducing more abstractions for use in that procedure.


;;  * _ Exercise 5.11

; a. Can eliminate the (restore continue) ... (save continue) as noted in exercise 5.6.
;    Don't see an additional instruction that can be eliminated.
; b. 
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (cons (stack-inst-reg-name inst) (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (let ((stack-pop (pop stack)))
	(if (= (car stack-pop) (stack-inst-reg-name inst))
	    (set-contents! reg (cdr stack-pop))
	    (error "Wrong restore register -- ASSMBLE" (car stack-pop) (stack-inst-reg-name inst)))
	(advance-pc pc)))))

; c. extracted from make-new-machine
; Conceptually, modify allocate-register to additionally create a stack
; seems convenient to store it in the register table:

; ... extract from make-new-machine ...
 (set! register-table
                  (cons (list name (make-register name) (make-stack))
                        register-table)))

(define (get-register-stack reg)
  (caddr reg))

(define (make-save inst machine stack pc)
  (let* ((reg (get-register machine
                           (stack-inst-reg-name inst)))
	(reg-stack (get-register-stack reg)))
    (lambda ()
      (push reg-stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg (get-register machine
                           (stack-inst-reg-name inst)))
	 (reg-stack (get-register-stack reg)))
    (lambda ()
      (set-contents! reg (pop reg-stack))    
      (advance-pc pc))))


;;  * _ Exercise 5.12

; interesting, but skipped for now


;;  * _ Exercise 5.13
; just as simple as this, I think:
; ... extracted from make-new-machine ...
 (define (lookup-register name)
   (let ((val (assoc name register-table)))
     (if val
	 (cadr val)
	 (allocate-register name))))


;;  * _ Exercise 5.14

(define fact-machine
  (make-machine
   '(continue n val)
   (list (list '* *) (list '- -) (list '= =))
   '(
     (assign continue (label fact-done))     ; set up final return address
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving n and continue.
     ;; Set up continue so that the computation will continue
     ;; at after-fact when the subroutine returns.
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
     (goto (reg continue))                   ; return to caller
     base-case
     (assign val (const 1))                  ; base case: 1! = 1
     (goto (reg continue))                   ; return to caller
     fact-done)))

(set-register-contents! fact-machine 'n 6)
(start fact-machine)
(get-register-contents fact-machine 'val) ; => 720
((fact-machine 'stack) 'print-statistics) ; => (total-pushes = 10 maximum-depth = 10)

(define (quick-machine-fact n)
  ((fact-machine 'stack) 'initialize)
  (set-register-contents! fact-machine 'n n)
  (start fact-machine)
  (get-register-contents fact-machine 'val) 
  ((fact-machine 'stack) 'print-statistics))

(quick-machine-fact 6) ; => (total-pushes = 10 maximum-depth = 10)
(quick-machine-fact 7) ; => (total-pushes = 12 maximum-depth = 12)
(quick-machine-fact 8) ; => (total-pushes = 14 maximum-depth = 14)
(quick-machine-fact 9) ; => (total-pushes = 16 maximum-depth = 16)
(quick-machine-fact 25); => (total-pushes = 48 maximum-depth = 48)
; so apparently it's 2n-2 for both operations
