
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
; http://yuml.me/diagram/class/[1]-1 to g>[g], [g]->[good enough], [g]->[improve], [improve]-improve to g>[g].

(controller
 (assign g (const 1.0))
 test-good-enough
   (test good-enough? (reg g) (reg x))
   (branch (label sqrt-done))
   (assign g improve (reg g) (reg x))
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

; did we implement this one before? -- this one doesn't seem optimized
(define (unique-add-sort-sym-by items new-item proc)
  (cond ((null? items) (list new-item))
	((equal? (car items) new-item) items) ; eq? only checks for same object, we need structural equality here instead
	((symbol<? (proc new-item) (proc (car items))) (cons new-item items))
	(else (cons (car items) (unique-add-sort-sym-by (cdr items) new-item proc)))))

(unique-add-sort-sym-by '((apple 1) (daisy 2)) '(banana 3) car)        ; check
(unique-add-sort-sym-by '((apple x 1) (daisy y 2)) '(banana z 3) cadr) ; check

(define (identity x) x)
(define (unique-add-sort-sym items new-item) (unique-add-sort-sym-by items new-item identity))
(unique-add-sort-sym '(apple daisy) 'banana)                           ; check

(define (extract-sorted-instructions controller-text)
  (fold-left (lambda (list item) (unique-add-sort-sym-by list item car)) '()
	      (filter (lambda (x) (not (symbol? x))) controller-text)))

(define (extract-reg-entry-points controller-text)
  (fold-left unique-add-sort-sym '()
	     (map get-goto-reg (filter goto-reg? controller-text))))

(define (tagged? item tag)
  (and (list? item) (eq? (car item) tag)))

(define (get-goto-reg instr) (cadadr instr))
(define (goto-reg? instr) 
  (and (tagged? instr 'goto)
       (tagged? (cadr instr) 'reg)))

(define (extract-reg-entry-points controller-text)
  (fold-left (lambda (list item) (unique-add-sort-sym-by list item identity)) '()
	     (map get-goto-reg
		  (filter goto-reg? controller-text))))

(define (extract-suspended-registers controller-text)
  (fold-left unique-add-sort-sym '()
	     (map get-suspended-reg (filter suspended-reg? controller-text))))

(define (get-suspended-reg instr) (cadr instr))
(define (suspended-reg? instr)
  (or (tagged? instr 'save)
      (tagged? instr 'restore)))

(define (unique-add items new-item)
  (cond ((null? items) (list new-item))
	((equal? (car items) new-item) items)
	(else (cons (car items) (unique-add (cdr items) new-item)))))

(define assign-reg-name cadr)
(define assign-reg-source cddr)

(define (add-register-source items new-item)
  (let ((reg-source-list (assoc (assign-reg-name new-item) items)))
    (if reg-source-list
	(begin
	  (set-cdr! reg-source-list (unique-add (cdr reg-source-list) (assign-reg-source new-item)))
	  items)
	(cons
	 (cons (assign-reg-name new-item) (list (assign-reg-source new-item)))
	 items))))

(define (assign? instr) (tagged? instr 'assign))

(define (extract-register-sources controller-text)
  (fold-left add-register-source '()
	     (filter assign? controller-text)))
	     
(define (assemble controller-text machine)
  ((machine 'set-sorted-instructions) (extract-sorted-instructions controller-text))
  ((machine 'set-reg-entry-points) (extract-reg-entry-points controller-text))
  ((machine 'set-suspended-registers) (extract-suspended-registers controller-text))
  ((machine 'set-register-sources) (extract-register-sources controller-text))
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
	(sorted-instructions '())
	(reg-entry-points '())
	(suspended-registers '())
	(register-sources '())
	(instruction-count 0)
	(trace false)
	(last-label false))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)

	      ; new operations for 5.12
	      ((eq? message 'sorted-instructions) sorted-instructions)
	      ((eq? message 'set-sorted-instructions) 
	       (lambda (insts) (set! sorted-instructions insts)))

	      ((eq? message 'reg-entry-points) reg-entry-points)
	      ((eq? message 'set-reg-entry-points) 
	       (lambda (regs) (set! reg-entry-points regs)))

	      ((eq? message 'suspended-registers) suspended-registers)
	      ((eq? message 'set-suspended-registers) 
	       (lambda (regs) (set! suspended-registers regs)))

	      ((eq? message 'register-sources) register-sources)
	      ((eq? message 'set-register-sources) 
	       (lambda (sources) (set! register-sources sources)))

	      ; 5.15
	      ((eq? message 'instruction-count) instruction-count)
	      ((eq? message 'reset-instruction-count)
	       (set! instruction-count 0))
	      ((eq? message 'inc-instruction-count)
	       (set! instruction-count (+ 1 instruction-count)))
	      
	      ; 5.16
	      ((eq? message 'trace) trace)
	      ((eq? message 'trace-on)
	       (set! trace true))
	      ((eq? message 'trace-off)
	       (set! trace false))
	      
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


; fib-machine from figure 5.12, wrapped in make-machine
(define fib-machine
  (make-machine
   '(continue n val)
   (list (list '< <) (list '- -) (list '+ +))
   '(
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n - 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)                           ; save old value of n
     (assign n (op -) (reg n) (const 1)); clobber n to n - 1
     (goto (label fib-loop))            ; perform recursive call
     afterfib-n-1                         ; upon return, val contains Fib(n - 1)
     (restore n)
     (restore continue)
     ;; set up to compute Fib(n - 2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)                         ; save Fib(n - 1)
     (goto (label fib-loop))
     afterfib-n-2                         ; upon return, val contains Fib(n - 2)
     (assign n (reg val))               ; n now contains Fib(n - 2)
     (restore val)                      ; val now contains Fib(n - 1)
     (restore continue)
     (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
	     (op +) (reg val) (reg n)) 
     (goto (reg continue))              ; return to caller, answer is in val
     immediate-answer
     (assign val (reg n))               ; base case:  Fib(n) = n
     (goto (reg continue))
     fib-done)))

(fib-machine 'sorted-instructions) ; check!
(fib-machine 'reg-entry-points)    ; => (continue) check!
(fib-machine 'suspended-registers) ; => (continue n val) check!
(fib-machine 'register-sources)    ; looks okay, see below:
; =>  ((val ((op +) (reg val) (reg n)) ((reg n))) (n ((op -) (reg n) (const 1)) ((op -) (reg n) (const 2)) ((reg val))) (continue ((label fib-done)) ((label afterfib-n-1)) ((label afterfib-n-2))))


;;  * _ Exercise 5.13
; just as simple as this, I think:
; ... extracted from make-new-machine ...
 (define (lookup-register name)
   (let ((val (assoc name register-table)))
     (if val
	 (cadr val)
	 (allocate-register name))))

(define (allocate-register name)
  (if (assoc name register-table)
      (error "Multiply defined register: " name)
      (set! register-table
	    (cons (list name (make-register name))
		  register-table)))
  (cadar register-table))

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
; so apparently it's 2n-2 for both stats


;;  * _ Exercise 5.15

; see also answer to 5.12
; todo: combine 5.12, 5.15-19 for easier reading!

(define (println item) (display item) (newline))

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (lambda ()
    ; for 5.17
    (if (tagged? inst 'label)
	(begin
	  (if (machine 'trace)
	      (println (cadr inst)))
	  (advance-pc pc))
	(begin
	  ; for 5.15
	  (machine 'inc-instruction-count)

	  ; for 5.16
	  (if (machine 'trace)
	      (println inst))
	  ((cond ((eq? (car inst) 'assign)
		  (make-assign inst machine labels ops pc))
		 ((eq? (car inst) 'test)
		  (make-test inst machine labels ops flag pc))
		 ((eq? (car inst) 'branch)
		  (make-branch inst machine labels flag pc))
		 ((eq? (car inst) 'goto)
		  (make-goto inst machine labels pc))
		 ((eq? (car inst) 'save)
		  (make-save inst machine stack pc))
		 ((eq? (car inst) 'restore)
		  (make-restore inst machine stack pc))
		 ((eq? (car inst) 'perform)
		  (make-perform inst machine labels ops pc))
		 (else (error "Unknown instruction type -- ASSEMBLE"
			      inst))))))))

((fact-machine 'stack) 'initialize)
(set-register-contents! fact-machine 'n 7)
(start fact-machine)
(fact-machine 'instruction-count) ; => 71
(fact-machine 'reset-instruction-count) 
(fact-machine 'instruction-count) ; => 0

((fact-machine 'stack) 'initialize)
(set-register-contents! fact-machine 'n 8)
(start fact-machine)
(fact-machine 'instruction-count) ; => 82
(fact-machine 'reset-instruction-count) 
(fact-machine 'instruction-count) ; => 0

(get-register-contents fact-machine 'val) ; => 5040

;;  * _ Exercise 5.16

; see exercise 5.15 above

; testing ...
((fact-machine 'stack) 'initialize)
(set-register-contents! fact-machine 'n 4)
(fact-machine 'trace-on)
(fact-machine 'trace)
(fact-machine 'trace-off)
(start fact-machine)
(get-register-contents fact-machine 'val)

; seems to work

;;  * _ Exercise 5.17

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive (cons (list (list 'label next-inst)) insts)
                        (cons (make-label-entry next-inst
                                                insts)
                              labels))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))
; implemented above
((fact-machine 'stack) 'initialize)
(set-register-contents! fact-machine 'n 4)
(fact-machine 'trace-on)
(fact-machine 'start)
(get-register-contents fact-machine 'val) 
; checks out


;;  * _ Exercise 5.18

; skipped

;;  * _ Exercise 5.19

; skipped

;;  * _ Exercise 5.20

(define x (cons 1 2))
(define y (list x x))
; y -> [*][*]->[*][/]
;       |       |
;       +-------/      
;       V
; x -> [*][*]
;       |  |
;       V  V
;      [1][2]
; 
;
; Index       0   1    2    3   4
; the-cars        n1   p1   p1  
; the-cdrs        n2   p3   e0
;                 x    y        free


;;  * _ Exercise 5.21

; a.
(define count-leaves-recursive-machine
  (make-machine
   '(continue tree val)
   (list (list 'car car) (list 'cdr cdr) 
	 (list 'null? null?) (list 'pair? pair?) (list '+ +))
   '(
       (assign continue (label count-leaves-done))
      test-tree
       (test (op null?) (reg tree))
       (branch (label return-0))
       (test (op pair?) (reg-tree))
       (branch (label return-recurse-left))
       (assign val (const 1))
       (goto (reg continue))
      return-0
       (assign val (const 0))
       (goto (reg continue))
      return-recurse-left
       (save continue)
       (assign continue (label return-recurse-right))
       (save val)
       (save tree)
       (assign tree (op car) (reg tree))
       (goto (reg test-tree))
      return-recurse-right
       (assign tmp (reg val))
       (restore tree)
       (restore val)
       (assign val (op +) (reg tmp) (reg val))
       (assign continue (label return-recurse-done))
       (save val)
       (save tree)
       (assign tree (op cdr) (reg tree))
       (goto (reg test-tree))
      return-recurse-done
       (assign tmp (reg val))
       (restore tree)
       (restore val)
       (restore continue)
       (assign val (op +) (reg tmp) (reg val))
       (goto (reg continue))
      count-leaves-done)))

; b.
(define count-leaves-iterative-machine
  (make-machine
   '(continue tree n)
   (list (list 'car car) (list 'cdr cdr)
	 (list 'null? null?) (list 'pair? pair?) (list '+ +))
   '(
       (assign continue (label count-leaves-done))
       (assign n (const 0))
      test-tree
       (test (op null?) (reg tree))
       (branch (label return-0))
       (test (op pair?) (reg tree))
       (branch (label return-recurse-left))
       (assign n (op +) (reg n) (const 1))
       (goto (reg continue))
      return-0
       (goto (reg continue))
      return-recurse-left
       (save continue)
       (assign continue (label return-recurse-right))
       (save tree)
       (assign tree (op car) (reg tree))
       (goto (label test-tree))
      return-recurse-right
       (restore tree)
       (assign continue (label return-recurse-done))
       (save tree)
       (assign tree (op cdr) (reg tree)) ; using tree saving correctly here?
       (goto (reg test-tree))
      return-recurse-done
       (restore tree)
       (restore continue)
       (goto (reg continue))
      count-leaves-done)))


;;  * _ Exercise 5.22

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define append-machine
  (make-machine
   '(lista listb combined add-point tmp)
   (list (list 'car car) (list 'cdr cdr) (list 'cons cons)
	 (list 'set-cdr! set-cdr!)
	 (list 'null? null?))
   '(
       (assign combined (op cons) (const 0) (const '())) ; start with a dummy value (how to eliminate?)
       (assign add-point (reg combined))

      test-list-a
       (test (op null?) (reg lista))
       (branch (label test-list-b))

       (assign tmp (op car) (reg lista))
       (assign tmp (op cons) (reg tmp) (const '()))
       (perform (op set-cdr!) (reg add-point) (reg tmp))
       (assign (reg add-point) (op cdr) (reg add-point))
       (assign lista (op cdr) (reg lista))
       (goto (label test-list-a))
    
      test-list-b
       (test (op null?) (reg listb))
       (branch (label append-done))
       
       (perform (op set-cdr!) (reg add-point) (reg listb)) ; don't copy the second list

      append-done
       (assign combined (op cdr) (reg combined))  ; remove dummy value
       )))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define append!-machine
  (make-machine
   '(continue lista listb combined add-point tmp)
   (list (list 'car car) (list 'cdr cdr) (list 'cons cons)
	 (list 'set-car! set-car!) (list 'set-cdr! set-cdr!)
	 (list 'null? null?) (list 'pair? pair?))
   '(
       (assign add-point (reg lista))

       (test (op null?) (reg lista))
       (branch (label set-a-to-b))

      test-add-point
       (assign tmp (op cdr) (reg add-point))
       (test (op null?) (reg tmp))
       (branch (label set-cdr-to-b))
       (assign add-point (reg tmp))
       (goto (label test-add-point))

      set-a-to-b
       (assign (reg lista) (reg listb))
       (goto (label append!-done))

      set-cdr-to-b
       (perform (op set-cdr!) (reg add-point) (reg listb))

      append!-done
       )))


;;  * _ Exercise 5.23

; just as simple as this, right?
eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op cond?) (reg exp))    ; added
  (branch (label ev-cond))       ; added
  (test (op let?) (reg let))     ; added
  (branch (label ev-let))        ; added
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

ev-cond
  (assign exp (op cond->if) (reg exp))
  (goto (label ev-if))

; combines lambda with sequence -- not sure about this one
ev-let
  (assign exp (op let->combination) (reg exp))
  (save continue)
  (save env) ; necessary?
  (save unev)
  (assign continue (label ev-let-done))
  (goto (label ev-lambda))

ev-let-done
  (restore unev)
  (restore env)
  (restore continue)
  (goto (reg continue))


;;  * _ Exercise 5.24
ev-cond
  (save exp)
  (save unev)
  (save env)
  (save continue)
  (assign unev (op operands) (reg exp))
ev-cond-test-predicate
  (assign exp (op first-exp) (reg unev))
  (assign exp (op first-exp) (reg exp))

  (test (op eq?) (reg exp) (const 'else))
  (branch (label ev-cond-sequence))

  (assign continue (label ev-cond-decide))
  (goto (label eval-dispatch))

ev-cond-decide
  (test (op true?) (reg val))
  (branch (label ev-cond-sequence))

  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-cond-test-predicate))

ev-cond-sequence
  (assign exp (op first-exp) (reg unev))
  (assign unev (op rest-exps) (reg unev))
  (assign continue (label ev-cond-finish))
  (goto (label ev-sequence))

ev-cond-finish
  (restore continue)
  (restore env)
  (restore unev)
  (restore exp)
  (goto continue)


;;  * _ Exercise 5.25

ev-delayed
  (assign exp (op delayed-body) (reg exp)) ; delayed-body, env not defined (number of ways to do it)
  (save env)
  (assign env (op delayed-env) (reg env))  ; needed?
  (save continue)
  (assign continue (label ev-delayed-finish))
  (goto (label eval-dispatch))

ev-delayed-finish
  (restore continue)
  (restore env)

ev-delay
  (assign val (op make-delayed) (reg exp) (reg env))  ; make-delayed not defined
  (goto (reg continue))


; need to force something
force
  (
; when to force it?


; not finished

; some code from book below...
ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))

ev-appl-did-operator
  (restore unev)                  ; the operands
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))         ; the operator
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)

ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))

ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))

