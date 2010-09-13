
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
  (assign unev (op operands) (reg exp))

ev-cond-test-predicate
  (test (op null?) (reg unev))
  (branch (reg continue))

  (save env)
  (assign exp (op first-exp) (reg unev))
  (assign unev (op rest-exps) (reg exp))
  (assign exp (op first-exp) (reg exp))
  (save unev)
  (save exp)

  (test (op eq?) (reg exp) (const 'else))
  (branch (label ev-cond-sequence))

  (assign continue (label ev-cond-decide))
  (goto (label eval-dispatch))

ev-cond-decide
  (restore exp)
  (restore unev)
  (restore env)

  (test (op true?) (reg val))
  (branch (label ev-cond-sequence))

  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-cond-test-predicate))

ev-cond-sequence
  (assign exp (op first-exp) (reg unev))
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))

ev-cond-finish
  (goto continue)


;;  * _ Exercise 5.25

; added make-thunk and related functions to support
; primitive procs are strict
; self-evaluating are immediately evaluated
; printing forces thunks
; see commit: 7a9aab73f8ce562ce12cf4f2fec4adaad0d00b7b

; TODO: also check if variable forcing isn't working, e.g. with formal parameters
; maybe variables need to be forced immediately after looking them up.
; also force the operator if necessary
; also force if-predicates

;;  * _ Exercise 5.26
; iterative
(factorial 3)  ; (total-pushes = 134 maximum-depth = 10)
(factorial 4)  ; (total-pushes = 169 maximum-depth = 10)
(factorial 10) ; (total-pushes = 379 maximum-depth = 10)

; a. maximum-depth = 10
; b. total-pushes = 35n + 29


;;  * _ Exercise 5.27
; recursive
(factorial 3)  ; (total-pushes = 80 maximum-depth = 18)
(factorial 4)  ; (total-pushes = 112 maximum-depth = 23)
(factorial 10) ; (total-pushes = 304 maximum-depth = 53)

; maximum-depth = 5n + 3
; total-pushes = 32n - 16

;              max-depth  num-pushes
; recursive    5n + 3     32n - 16
; iterative    10         35n + 29


;;  * _ Exercise 5.28

; iterative
(factorial 3)  ; (total-pushes = 144 maximum-depth = 23)
(factorial 4)  ; (total-pushes = 181 maximum-depth = 26)
(factorial 10) ; (total-pushes = 403 maximum-depth = 44)

; recursive
(factorial 3)  ; (total-pushes = 86 maximum-depth = 27)
(factorial 4)  ; (total-pushes = 120 maximum-depth = 35)
(factorial 10) ; (total-pushes = 324 maximum-depth = 83)

;              max-depth  num-pushes
; recursive    8n + 3     34n - 16
; iterative    3n + 14    37n + 33


;;  * _ Exercise 5.29
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 2)  ; (total-pushes = 72 maximum-depth = 13)
(fib 3)  ; (total-pushes = 128 maximum-depth = 18) 
(fib 4)  ; (total-pushes = 240 maximum-depth = 23)
(fib 5)  ; (total-pushes = 408 maximum-depth = 28)
(fib 6)  ; (total-pushes = 688 maximum-depth = 33)
(fib 10) ; (total-pushes = 4944 maximum-depth = 53)

; a. maximum-depth = 5n + 3 (linear, as stated)
; b. total-pushes = S(n) = S(n-1) + 2 *  ( S(n-1)-S(n-2) )

; n  S(n)  S(n)-S(n-1)  Fib(n)  Fib(n+1)
; 2   72                1       2
; 3  128    56          2       3
; 4  240   112          3       5
; 5  408   168          5       8
; 6  688   280          8       13

; Since we're embedding the calls to fib(n-1) and fib(n-2)
;  we expect that the result will be a sum of cost of those
;  two calls plus however many pushes it takes to do the comparison
;  and the addition.
; We observe that S(n) = S(n-1) + S(n-2) + 40  (k = 40)

; S(n) = 56 * Fib(n+1) - 40



;;  * _ Exercise 5.30
; The question is -- how many different kinds of errors are there?
;  Variable lookups that fail (given as an example in a)
;  Applications that are wrong (as in part b)
;  Arity errors -- but how can these be trapped automatically?

; a. The variable lookup seems like a trivial patch! What am I missing?
; see diffs in ch5-eceval and ch5-eceval-support

; b. simple diffs to ch5-eceval-support.scm
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


;;  * _ Exercise 5.31
; we were previously saving the following:
;  env, for operator
;  env, for each operand except the last one
;  argl for each operand
;  proc around operand sequence

(f 'x 'y)
; all save/restore operations are superflous

((f) 'x 'y)
; all are superfluous except env around the operator
; Note: argl does not need to be saved because operators are handled
;  before we even create an argl for the operands

(f (g 'x) y)
; superfluous:
;  argl and proc do not need to be saved around eval of y or 'x
;  env does not need to be saved anywhere, except around (g 'x)

(f (g 'x) 'y)
; why is this possibly different than the above?

;;  * _ Exercise 5.32

; a. see commit: 06c3dd5cccfe51bdf8b6fd4a1cd9b1bae0d5fd3f
; b. Introducing special cases will make the evaluator more efficient than it was,
;  but there is a cost to checking all the special cases, and this cost is incurred
;  every time an expression is analyzed.  The compiler will incur this cost only once
;  at compile time, which will therefore still give it an advantage.


;;  * _ Exercise 5.33
(compile
  '(define (factorial n)
     (if (= n 1)
	 1
	 (* (factorial (- n 1)) n)))
  'val
  'next)

;; construct the procedure and skip over code for the procedure body
  (assign val
          (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))

entry2     ; calls to factorial will enter here
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env
          (op extend-environment) (const (n)) (reg argl) (reg env))
;; begin actual procedure body
  (save continue)
  (save env)

;; compute (= n 1)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch17))
compiled-branch16
  (assign continue (label after-call15))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch17
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

after-call15   ; val now contains result of (= n 1)
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch4))
true-branch5  ; return 1
  (assign val (const 1))
  (goto (reg continue))

false-branch4
;; compute and return (* (factorial (- n 1)) n)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc)   ; save * procedure
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (save argl)   ; save partial argument list for *

;; compute (factorial (- n 1)), which is the other argument for *
  (assign proc
          (op lookup-variable-value) (const factorial) (reg env))
  (save proc)  ; save factorial procedure


(compile
 '(define (factorial-alt n)
  (if (= n 1)
      1
      (* n (factorial-alt (- n 1)))))
 'val
 'next)

((env) (val) 
 (
  (assign val (op make-compiled-procedure) (label entry19) (reg env)) 
  (goto (label after-lambda18))  
entry19 
  (assign env (op compiled-procedure-env) (reg proc)) 
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) 

;; begin actual procedure body
  (save continue) 
  (save env) 

;; compute (= n 1)
  (assign proc (op lookup-variable-value) (const =) (reg env)) 
  (assign val (const 1)) 
  (assign argl (op list) (reg val)) 
  (assign val (op lookup-variable-value) (const n) (reg env)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch34)) 
compiled-branch33 
  (assign continue (label after-call32)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
primitive-branch34 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
after-call32 
  (restore env) 
  (restore continue) 
  (test (op false?) (reg val)) 
  (branch (label false-branch21)) 
true-branch22 
  (assign val (const 1)) (goto (reg continue)) 
false-branch21 
;; compute and return (* n (factorial-alt (- n 1)))
  (assign proc (op lookup-variable-value) (const *) (reg env)) 
  (save continue) 
  (save proc) 
;; ** env is saved here
  (save env) 
;; ** The factorial-alt is looked up before getting n
  (assign proc (op lookup-variable-value) (const factorial-alt) (reg env)) 
  (save proc) 
  (assign proc (op lookup-variable-value) (const -) (reg env)) 
  (assign val (const 1)) 
  (assign argl (op list) (reg val)) 
  (assign val (op lookup-variable-value) (const n) (reg env)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch25)) 
compiled-branch24 
  (assign continue (label after-call23)) 
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val)) 
primitive-branch25 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
after-call23 
  (assign argl (op list) (reg val)) 
  (restore proc) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch28)) 
compiled-branch27 
  (assign continue (label after-call26)) 
;; factorial-alt called here
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
primitive-branch28 
 (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
after-call26 
  (assign argl (op list) (reg val)) 
;; ** env is restored here
  (restore env) 
  (assign val (op lookup-variable-value) (const n) (reg env)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (restore proc) 
  (restore continue) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch31)) 
compiled-branch30 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
primitive-branch31 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue)) 
after-call29 
after-if20 
after-lambda18 
  (perform (op define-variable!) (const factorial-alt) (reg val) (reg env)) 
  (assign val (const ok))
))

; It appears that this version does not actually use tail call optimization --
; we will get extra envs stored on the stack for each recursion.  Marked above.
; This happens because the compiler can detect the earlier call


;;  * _ Exercise 5.34
(compile
 '(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
 'val
 'next)

;; iterative factorial compiled code
;;  most interesting divergences from recursive version marked with *
(
 (env) 
 (val) 
 (
  ;; construct the procedure and skip over code for the procedure body
  (assign val (op make-compiled-procedure) (label entry2) (reg env)) 
  (goto (label after-lambda1)) 
  entry2 				; calls to factorial enter here
  (assign env (op compiled-procedure-env) (reg proc)) 
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) 

  ;; * construct the iter procedure and skip over code for the procedure body
  (assign val (op make-compiled-procedure) (label entry7) (reg env)) 
  (goto (label after-lambda6)) 
  entry7				; calls to iter enter here
  (assign env (op compiled-procedure-env) (reg proc)) 
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env)) 

  ;; begin iter body
  (save continue) 			
  (save env) 

  ;; compute (> counter n)
  (assign proc (op lookup-variable-value) (const >) (reg env)) 
  (assign val (op lookup-variable-value) (const n) (reg env)) 
  (assign argl (op list) (reg val)) 
  (assign val (op lookup-variable-value) (const counter) (reg env)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch22)) 
  compiled-branch21 
  (assign continue (label after-call20)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
  primitive-branch22 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
  after-call20 				; val contains result of (> counter n)
  (restore env) 
  (restore continue) 
  (test (op false?) (reg val)) 
  (branch (label false-branch9)) 
  true-branch10 
  (assign val (op lookup-variable-value) (const product) (reg env)) 
  (goto (reg continue)) 
  false-branch9

  ;; * construct additional call to iter
  (assign proc (op lookup-variable-value) (const iter) (reg env)) 
  (save continue)		  
  (save proc)			 
  (save env)			

  ;; compute (+ n 1)
  (assign proc (op lookup-variable-value) (const +) (reg env)) 
  (assign val (const 1)) 
  (assign argl (op list) (reg val)) 
  (assign val (op lookup-variable-value) (const counter) (reg env)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch16)) 
  compiled-branch15 
  (assign continue (label after-call14)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) primitive-branch16 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
  after-call14 				; val contains result of (+ n 1)
  (assign argl (op list) (reg val)) 
  (restore env) 			; * additional stack usage: restore env
  (save argl) 

  ;; compute (* counter product)
  (assign proc (op lookup-variable-value) (const *) (reg env)) 
  (assign val (op lookup-variable-value) (const product) (reg env)) 
  (assign argl (op list) (reg val)) 
  (assign val (op lookup-variable-value) (const counter) (reg env)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch13)) 
  compiled-branch12 
  (assign continue (label after-call11)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) primitive-branch13 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
  after-call11 				; val now contains result of (* counter product) 
  (restore argl) 
  (assign argl (op cons) (reg val) (reg argl)) 

  ;; * actual call to iter
  (restore proc) 		
  (restore continue) 			; * reduced stack usage, since we can just
					; use the prior version of continue, instead of 
					; saving it and trying something different

  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch19)) 
  compiled-branch18 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) primitive-branch19 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
  (goto (reg continue)) 
  after-call17 
  after-if8 
  after-lambda6 

  ;; * define iter variable (end of iter procedure)
  (perform (op define-variable!) (const iter) (reg val) (reg env)) 
  (assign val (const ok)) 

  ;; call (iter 1 1)
  (assign proc (op lookup-variable-value) (const iter) (reg env)) 
  (assign val (const 1)) 
  (assign argl (op list) (reg val)) 
  (assign val (const 1)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch5)) 
  compiled-branch4 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) primitive-branch5 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
  (goto (reg continue)) 
  after-call3 
  after-lambda1

  ;; define factorial variable
  (perform (op define-variable!) (const factorial) (reg val) (reg env)) 
  (assign val (const ok))))

;;  * _ Exercise 5.35
(define (f x) (+ x (g (+ x 2))))

(compile '(define (f x) (+ x (g (+ x 2)))) 'val 'next) ; looks right


;;  * _ Exercise 5.36
; The compiler evaluates procedures in right-to-left order.
; construct-arglist determines this order, by processing the
;  arguments from last to first.

; the original version of the code
(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

; a modified version of the code, to go left-to-right
(define (construct-arglist operand-codes)
  ;; no let statement to reverse operands
  (if (null? operand-codes)
      (make-instruction-sequence '() '(argl)
				 '((assign argl (const ()))))
      (let ((code-to-get-first-arg
	     (append-instruction-sequences
	      (car operand-codes)
	      (make-instruction-sequence '(val) '(argl)
					 '((assign argl (op list) (reg val)))))))
	(if (null? (cdr operand-codes))
	    code-to-get-first-arg
	    (preserving '(env)
			code-to-get-first-arg
			(code-to-get-rest-args
			 (cdr operand-codes)))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
	  ;; arguably cheating: using "append" op instead of cons
          (make-instruction-sequence '(val argl) '(argl)
           '(
	     (assign val (op list) (reg val))
	     (assign argl (op append) (reg argl) (reg val)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

; we should expect this to be more inefficient, since the append code will require
; more operations than a simple cons

;;  * _ Exercise 5.37
(compile '(+ 4 5) 'val 'next) ; =>
((env) 
 (env proc argl continue val) 
 (
  (assign proc (op lookup-variable-value) (const +) (reg env)) 
  (assign val (const 5)) 
  (assign argl (op list) (reg val)) 
  (assign val (const 4)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch36)) 
  compiled-branch35 
  (assign continue (label after-call34)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
  primitive-branch36 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
  after-call34))

(compile '(define (increment x) (+ x 1)) 'val 'next) ; =>
((env) 
 (val) 
 (
  (assign val (op make-compiled-procedure) (label entry38) (reg env)) 
  (goto (label after-lambda37)) entry38 
  (assign env (op compiled-procedure-env) (reg proc)) 
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env)) 
  (assign proc (op lookup-variable-value) (const +) (reg env)) 
  (assign val (const 1)) 
  (assign argl (op list) (reg val)) 
  (assign val (op lookup-variable-value) (const x) (reg env)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch41)) 
  compiled-branch40 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
  primitive-branch41 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
  (goto (reg continue)) 
  after-call39 
  after-lambda37 
  (perform (op define-variable!) (const increment) (reg val) (reg env)) 
  (assign val (const ok))))

; with modified preserving
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
	(preserving (cdr regs)
		    (make-instruction-sequence
		     (list-union (list first-reg)
				 (registers-needed seq1))
		     (list-difference (registers-modified seq1)
				      (list first-reg))
		     (append `((save ,first-reg))
			     (statements seq1)
			     `((restore ,first-reg))))
		    seq2))))

(compile '(+ 4 5) 'val 'next) ; =>
;; all calls to save and restore are new! none are needed!
((env continue) 
 (env proc argl continue val) 
 ((save continue) 
  (save env) 
  (save continue) 
  (assign proc (op lookup-variable-value) (const +) (reg env)) 
  (restore continue) 
  (restore env) 
  (restore continue) 
  (save continue) 
  (save proc) 
  (save env) 
  (save continue) 
  (assign val (const 5)) 
  (restore continue) 
  (assign argl (op list) (reg val)) 
  (restore env) 
  (save argl) 
  (save continue) 
  (assign val (const 4)) 
  (restore continue) 
  (restore argl) 
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc) 
  (restore continue) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch44)) 
  compiled-branch43 
  (assign continue (label after-call42)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
  primitive-branch44 
  (save continue) 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
  (restore continue) 
  after-call42))

(compile '(define (increment x) (+ x 1)) 'val 'next) ; =>
;; again, all calls to save and restore are new! none are needed!
((continue env) 
 (val) 
 (
  (save continue) 
  (save env) 
  (save continue) 
  (assign val (op make-compiled-procedure) (label entry46) (reg env)) 
  (restore continue) 
  (goto (label after-lambda45)) 
  entry46 
  (assign env (op compiled-procedure-env) (reg proc)) 
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env)) 
  (save continue) 
  (save env) 
  (save continue) 
  (assign proc (op lookup-variable-value) (const +) (reg env)) 
  (restore continue) 
  (restore env) 
  (restore continue) 
  (save continue) 
  (save proc) 
  (save env) 
  (save continue) 
  (assign val (const 1)) 
  (restore continue) 
  (assign argl (op list) (reg val)) 
  (restore env) 
  (save argl) 
  (save continue) 
  (assign val (op lookup-variable-value) (const x) (reg env)) 
  (restore continue) 
  (restore argl) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (restore proc) 
  (restore continue) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch49)) 
  compiled-branch48 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
  primitive-branch49 
  (save continue) 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
  (restore continue) 
  (goto (reg continue)) 
  after-call47 
  after-lambda45 
  (restore env) 
  (perform (op define-variable!) (const increment) (reg val) (reg env)) 
  (assign val (const ok)) 
  (restore continue)))

;;  * _ Exercise 5.38

; a. spread-arguments
(define (spread-arguments operand-codes final-code)
  (append-instruction-sequences
     (compile (car operand-codes) 'arg1 'next)
     (preserving '(arg1)
		 (compile (cadr operand-codes) 'arg2 'next)
		 final-code)))

; b. primitives
(define (plus? exp) (tagged-list? exp '+))
(define (minus? exp) (tagged-list? exp '-))
(define (multiply? exp) (tagged-list? exp '*))
(define (equals? exp) (tagged-list? exp '=))
(define (divide? exp) (tagged-list? exp '/))

(define (compile-open-coded op exp target linkage)
  (end-with-linkage
   linkage
   (spread-arguments (cdr exp)
    (make-instruction-sequence '(arg1 arg2) (list target)
      `(
	(assign ,target (op ,op) (reg arg1) (reg arg2)))))))

(define (compile-plus exp target linkage) (compile-open-coded '+ exp target linkage))
(define (compile-minus exp target linkage) (compile-open-coded '- exp target linkage))
(define (compile-multiply exp target linkage) (compile-open-coded '* exp target linkage))
(define (compile-equals exp target linkage) (compile-open-coded '= exp target linkage))
(define (compile-divide exp target linkage) (compile-open-coded '/ exp target linkage))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))

	((plus? exp) (compile-plus exp target linkage))
	((minus? exp) (compile-minus exp target linkage))
	((multiply? exp) (compile-multiply exp target linkage))
	((equals? exp) (compile-equals exp target linkage))
	;; etc.

        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(compile '(+ 4 5) 'val 'next)
(compile '(+ (+ 1 2) (+ 5 0)) 'val 'next)

; c.
(compile
 '(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))
 'val
 'next) ; => below

(
 (env) 
 (val) 
 (
  (assign val (op make-compiled-procedure) (label entry138) (reg env)) 
  (goto (label after-lambda137)) 
  entry138 
  (assign env (op compiled-procedure-env) (reg proc)) 
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) 
  (assign arg1 (op lookup-variable-value) (const n) (reg env)) 
  (assign arg2 (const 1)) 
  (assign val (op =) (reg arg1) (reg arg2)) 
  (test (op false?) (reg val)) 
  (branch (label false-branch140)) 
  true-branch141 
  (assign val (const 1)) 
  (goto (reg continue)) 
  false-branch140 
  (save continue) 
  (assign proc (op lookup-variable-value) (const factorial) (reg env)) 
  (assign arg1 (op lookup-variable-value) (const n) (reg env)) 
  (assign arg2 (const 1)) 
  (assign val (op -) (reg arg1) (reg arg2)) 
  (assign argl (op list) (reg val)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch144)) 
  compiled-branch143 
  (assign continue (label proc-return145)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
  proc-return145 
  (assign arg1 (reg val)) 
  (goto (label after-call142)) 
  primitive-branch144 
  (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl)) 
  after-call142 
  (assign arg2 (op lookup-variable-value) (const n) (reg env)) 
  (assign val (op *) (reg arg1) (reg arg2)) 
  (restore continue) 
  (goto (reg continue)) 
  after-if139 after-lambda137 
  (perform (op define-variable!) (const factorial) (reg val) (reg env)) 
  (assign val (const ok))))


; d.
; conceptually, compile-plus should replace (+ 1 2 3) with (+ (+ 1 2) 3)
(define (compile-plus exp target linkage)
  (if (null? (cdddr exp))
      (compile-open-coded '+ exp target linkage)
      (compile-plus 
       (cons '+ (cons (compile-open-coded '+ (cadr exp) (caddr exp))
		      (cdddr exp))))))

(define (compile-multiply exp target linkage)
  (if (null? (cdddr exp))
      (compile-open-coded '* exp target linkage)
      (compile-multiply 
       (cons '* (cons (compile-open-coded '* (cadr exp) (caddr exp))
		      (cdddr exp))))))

; problems with these! the calls to compile-open-coded are wrong
; mixing the literal '+ with some compiled code!

; 17:22 inimino: yeah maybe something like (let (nested-expr (n-ary-to-nested exp))) and then compile that normally?



;;  * _ Exercise 5.39
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


;;  * _ Exercise 5.40
; see commit a6d4129443b184ddce7ae992cf618bc81aa837e2
; addnl fixes: commit 57e8e5f43f9d8c0a3f66b25fb12ad65b80472811

;;  * _ Exercise 5.41
; see commit a6d4129443b184ddce7ae992cf618bc81aa837e2
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


(find-variable 'c '((y z) (a b c d e) (x y))) ; => (1 2)
(find-variable 'x '((y z) (a b c d e) (x y))) ; => (2 0)
(find-variable 'w '((y z) (a b c d e) (x y))) ; => not-found

;;  * _ Exercise 5.42
(compile
 '(define (funny-square n)
    ((lambda (x) (* x n)) n))
 'val
 'next
 '()) ; => looks ok

(compile
 '(let ((x 3) (y 4))
    (lambda (a b c d e)
      (let ((y (* a b x))
	    (z (+ c d x)))
	(* x y z))))
 'val
 'next
 '()) ; => looks ok

;; see diffs in commit 57e8e5f43f9d8c0a3f66b25fb12ad65b80472811

;;  * _ Exercise 5.43

;; from ex. 4.7
(define (make-let assignments body)
  (cons 'let (cons assignments body)))

(define (make-assignment var val)
  (list 'set! var val))

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

; called in compile-definition
; ...(compile (definition-value (scan-out-defines exp)) 'val 'next compile-time-env)...
(scan-out-defines '((lambda (x) (* x n)) n))

(compile
 '(define (funny-square n)
    ((lambda (x) (* x n)) (+ n 0)))
 'val
 'next
 '()) ; => looks ok

(compile
 '(define (funny-square n)
    (define (even? x) (= (remainder x 2) 0))
    (if (even? n)
	(* n n)
	(* n n)))
 'val
 'next
 '()) ; => works, didn't check in detail

; trying out another one
(pp (compile '(define (f x) (define (g x) (* x x)) (g x)) 'val 'next '()))

 ((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry188) (reg env))
  (goto (label after-lambda187))
  entry188
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
  (assign env (op get-global-environment))
  (assign val (op lookup-variable-value) (const let) (reg env))
  (save continue)
  (save env)
  (assign env (op get-global-environment))
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (assign env (op get-global-environment))
  (assign val (op lookup-variable-value) (const *unassigned*) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch196))
  compiled-branch195
  (assign continue (label proc-return197))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  proc-return197
  (assign proc (reg val))
  (goto (label after-call194))
  primitive-branch196
  (assign proc (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call194
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch200))
  compiled-branch199
  (assign continue (label after-call198))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch200
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call198
  (restore env)
  (restore continue)
  (assign val (op make-compiled-procedure) (label entry193) (reg env))
  (goto (label after-lambda192))
  entry193
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign arg2 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign val (op *) (reg arg1) (reg arg2))
  (goto (reg continue))
  after-lambda192
  (perform (op set-variable-value!) (const g) (reg val) (reg env))
  (assign val (const ok))
  (assign env (op get-global-environment))
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (assign val (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch191))
  compiled-branch190
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch191
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call189
  after-lambda187
  (perform (op define-variable!) (const f) (reg val) (reg env))
  (assign val (const ok))))


;;  * _ Exercise 5.44
;; slightly modifying approach previously used for open coding
(define (spread-arguments operand-codes final-code compile-time-env)
  (append-instruction-sequences
     (compile (car operand-codes) 'arg1 'next compile-time-env)
     (preserving '(arg1)
		 (compile (cadr operand-codes) 'arg2 'next compile-time-env)
		 final-code)))

(define (open-codeable? exp compile-time-env)
  (and (memq (car exp) '(+ - * /))
       (eq? 'not-found (find-variable (car exp) compile-time-env))))

(open-codeable? '(+ 4 5) '()) ; => #t

(define (open-coded-version original-op)
  (cadr (assoc original-op '((+ +) (- -) (* *) (/ /)))))

(define (compile-open-coded exp target linkage compile-time-env)
  (let ((op (open-coded-version (car exp))))
    (end-with-linkage linkage
     (spread-arguments (cdr exp)
      (make-instruction-sequence '(arg1 arg2) (list target)
       `(
	 (assign ,target (op ,op) (reg arg1) (reg arg2))))
      compile-time-env))))

(define (compile exp target linkage compile-time-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage compile-time-env))
        ((quoted? exp) 
	 (compile-quoted exp target linkage compile-time-env))
        ((variable? exp)
         (compile-variable exp target linkage compile-time-env))
        ((assignment? exp)
         (compile-assignment exp target linkage compile-time-env))
        ((definition? exp)
         (compile-definition exp target linkage compile-time-env))
        ((if? exp) 
	 (compile-if exp target linkage compile-time-env))
        ((lambda? exp) 
	 (compile-lambda exp target linkage compile-time-env))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
			   compile-time-env))
        ((cond? exp) 
	 (compile (cond->if exp) target linkage compile-time-env))

        ((open-codeable? exp compile-time-env) 
	 (compile-open-coded exp target linkage compile-time-env))

        ((application? exp)
         (compile-application exp target linkage compile-time-env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(compile
 '(+ (* 2 3) (+ 4 5))
 'val
 'next
 '()) ; => looks ok

(pp (compile '(define (f * x y) (+ (* x x) (* y y))) 'val 'next '()))
; =>
((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry202) (reg env))
  (goto (label after-lambda201))
  entry202
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (* x y)) (reg argl) (reg env))
  (save continue)
  (assign proc (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign val (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch209))
  compiled-branch208
  (assign continue (label proc-return210))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  proc-return210
  (assign arg1 (reg val))
  (goto (label after-call207))
  primitive-branch209
  (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call207
  (assign proc (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign val (op lexical-address-lookup) (const (0 2)) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 2)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch205))
  compiled-branch204
  (assign continue (label proc-return206))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  proc-return206
  (assign arg2 (reg val))
  (goto (label after-call203))
  primitive-branch205
  (assign arg2 (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call203
  (assign val (op +) (reg arg1) (reg arg2))
  (restore continue)
  (goto (reg continue))
  after-lambda201
  (perform (op define-variable!) (const f) (reg val) (reg env))
  (assign val (const ok))))

;;  * _ Exercise 5.45
; a. 
;  in 5.27, the evaluator required: 
;   maximum-depth: 5n + 3
;   total-pushes:  32n - 16
;  in 5.14, the special purpose machine required:
;   maximum-depth: 2n - 2
;   total-pushes:  2n - 2
;  the compile code examples:
;   n = 5: total-pushes 31, max-depth 14
;   n = 6: total-pushes 37, max-depth 17
;   n = 7: total-pushes 43, max-depth 20
;  so generally for the compiled code:
;   maximum-depth: 3n - 1
;   total-pushes:  6n + 1
; 
;  When computing ratios, assume large n & ignore constant addend
;  So the ratios for compiled vs. eval'd and special-purpose vs. eval'd are:
;   maximum-depth: 3/5 (compiled) and 2/5 (special-purpose)
;   total-pushes: 3/16 (compiled) and 1/16 (special-purpose)

; b. Certainly some optimization can be achieved by open-coding
;   the =, *, and - values.  Looking up these procedures do cause
;   additional unneeded saves, and restores.
;
;  Additionally, it seems possible to avoid the lookup to factorial,
;   or at least looking it up, immediately saving it, then restoring it later.
;   This would work as follows: the compiler detects that nothing inside
;   the call to factorial redefines factorial.  Then, it would remove:
;   (assign proc
;          (op lookup-variable-value) (const factorial) (reg env))
;   (save proc)
;   ...
;   (restore proc)

;;  * _ Exercise 5.46
(compile-and-go
 '(define (fib n)
    (if (< n 2)
	n
	(+ (fib (- n 1)) (fib (- n 2))))))
; compiled performance:
;  n = 3: total-pushes = 27,  maximum-depth = 8
;  n = 4: total-pushes = 47,  maximum-depth = 11
;  n = 5: total-pushes = 77,  maximum-depth = 14
;  n = 6: total-pushes = 127, maximum-depth = 17
;  n = 7: total-pushes = 207, maximum-depth = 20
;  n = 8: total-pushes = 337, maximum-depth = 23
; => generally for any n
;  maximum-depth = 3n - 1
;  total-pushes = 10*Fib(n+1) - 3

; interpreted performance (from 5.29):
;  maximum-depth: 5n + 3
;  total-pushes:  56*Fib(n+1) - 40

(define fib-special-machine
 (make-machine
   '(n val continue)
   (list (list '+ +) (list '- -) (list '= =) (list '< <))
   '(
controller
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

(define (quick-machine-fib n)
  ((fib-special-machine 'stack) 'initialize)
  (set-register-contents! fib-special-machine 'n n)
  (start fib-special-machine) 
  ((fib-special-machine 'stack) 'print-statistics)
  (get-register-contents fib-special-machine 'val))

(quick-machine-fib 3) ; => 2,  tp=8,   md=4
(quick-machine-fib 4) ; => 3,  tp=16,  md=6
(quick-machine-fib 5) ; => 5,  tp=28,  md=8
(quick-machine-fib 6) ; => 8,  tp=48,  md=10
(quick-machine-fib 7) ; => 13, tp=80,  md=12
(quick-machine-fib 8) ; => 21, tp=132, md=14
; => maximum-depth = 2n - 2
;    total-depth = 4*Fib(n+1) - 4

; so ratio of compiled and special-purpose to interpreted is:
;  max-depth: 3/5 (compiled) and 2/5 (special-purpose)
;  total-pushes: 5/28 (compiled) and 1/17 (special-purpose)

; The book appears to be wrong, even though tp is not linear in n,
;  the ratios do approach a constant independent of n, it seems.

;;  * _ Exercise 5.47

(compile-and-go
 '(define (f) (+ 1 (g))))
; then run (define (g) 4)
; then run (g)
; then run (f)
; => 5

; changes in ch5-compiler.scm
;  commit: 5233c7863e073e5dcbea5e5b5d8818eb0cdfa218

;;  * _ Exercise 5.48

; Idea is to first compile and assemble the code provided
; Then to get this into exp
; Then to call ev-dispatch on it

; When we start applying a primitive, we have continue = print result
; In this case, we need to save that, and try a different one.

; compile-and-run in ch5-eceval-compiler.scm

(define (fresh-start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag false)
  (start eceval))

(fresh-start-eceval)

; to test:
;;; EC-Eval input:
(compile-and-run
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))
;;; EC-Eval value:
ok
;;; EC-Eval input:
(factorial 5)
;;; EC-Eval value:
120

; see commits:
; 0d2db7a1cebda9dbc4a5f5b4a60ca530e90ff4e7
; b18ac509823fa3456def1d3eb19047ab5f35d4bd
; 0f5ea35d5950e28061149cd1186fce67eb37db8f


;;  * _ Exercise 5.49
; This one becomes trivial with compile-and-run code already implemented
; see commit: ca8c47486ea8a4b42a054d56c613fccd51edffd7

; P.S. This version actually breaks a few things, because it tries to compile
; expressions that can't really be compiled, like "'(a b)" and "cons"
; Fixable.

;;  * _ Exercise 5.51

; compiler needs to learn how to compile "let" -- DONE
; eceval interpreter not comfortable with "cond/let" -- ignored for now, just fixed compiler
; compiler needs to learn how to compile "error": print something and exit -- DONE
; some other i/o features: display, newline, etc. -- DONE
; ca*d*rs needed -- DONE (mostly -- regular expressions might have been nice)
; set-car!, set-cdr! -- DONE
; map and length need implementation -- DONE

; what about apply? -- eceval needs to have some mechanism to handle this
;  multiple ways to handle this, makes sense to make the compiler smart enough to figure it out
;  not bothering with the interpreter
;  compiler -- DONE

; okay, working finally, at least for basic programs: 

; start by executing code in ch5-compile-mceval.scm

;; transcript ----------------------------

1 ]=> 
(total-pushes = 19 maximum-depth = 5)
;;; EC-Eval value:
metacircular-evaluator-loaded

;;; EC-Eval input:
(define the-global-environment (setup-environment))

(total-pushes = 548 maximum-depth = 39)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(driver-loop)


;;; M-Eval input:
(+ 4 28)

;;; M-Eval value:
32

;;; M-Eval input:
(define (fact n) (if (= n 1) 1 (* n (fact (- n 1)))))

;;; M-Eval value:
ok

;;; M-Eval input:
(fact 9)

;;; M-Eval value:
362880

;;; M-Eval input:

;;  end transcript -----------------------------------




;; More 5.51 dabbling
; gut check math
(+ 2 3)
(+ (* 4 5) (* 3 2))

(car '(1 2))


; Solve a kenken
(define (cadr-assoc x a) 
  (if (assoc x a)
      (cadr (assoc x a))
      false))

(define (dbg x)
  (display x)
  (newline)
  x)

(define (any? a)
  (if (null? a)
      false
      (if (car a)
	  true
	  (any? (cdr a)))))

(define (all? a)
  (if (null? a)
      true
      (if (car a)
	  (all? (cdr a))
	  false)))


; remove false items
(define (compact a)
  (if (null? a)
      '()
      (if (car a)
	  (cons (car a) (compact (cdr a)))
	  (compact (cdr a)))))

(define (insertions e list)
  (if (null? list)
      (cons (cons e list) list)
      (cons (cons e list)
            (map (lambda (tail) (cons (car list) tail))
                 (insertions e (cdr list))))))
 
(define (permutations list)
  (if (null? list)
      (cons list list)
      (apply append (map (lambda (permutation)
                           (insertions (car list) permutation))
                         (permutations (cdr list))))))
 
(define (any-permutation-equals? result op args)
  (any? (map (lambda (x) (= result (apply op x))) (permutations args))))

(define (solve-kenken size constraints)
  (define (valid-row? j board)
    (define (iter n numbers-seen)
      (if (> n size)
	  true
	  (let ((filled-number (cadr-assoc (list n j) board)))
	    (if filled-number
		(if (memq filled-number numbers-seen)
		    false
		    (iter (+ n 1) (cons filled-number numbers-seen)))
		(iter (+ n 1) numbers-seen)))))
    (iter 1 '()))

  (define (valid-col? i board)
    (define (iter n numbers-seen)
      (if (> n size)
	  true
	  (let ((filled-number (cadr-assoc (list i n) board)))
	    (if filled-number
		(if (memq filled-number numbers-seen)
		    false
		    (iter (+ n 1) (cons filled-number numbers-seen)))))))
    (iter 1 '()))

  (define (check-math result op args)
    (cond ((eq? op '+) (= result (apply + args)))
	  ((eq? op '*) (= result (apply * args)))
	  ((eq? op '/) (any-permutation-equals? result / args))
	  ((eq? op '-) (any-permutation-equals? result - args))))

  (define (fits-constraints? board rem-constraints)
    (if (null? rem-constraints)
	true
	(let ((constraint (car rem-constraints)))
	  (let ((result (car constraint))
		(op (cadr constraint))
		(cells (caddr constraint)))
	    (let ((cell-values (map (lambda (x) (cadr-assoc x board)) cells)))
	      (and 
	       (if (all? cell-values)
		   (check-math result op cell-values)
		   true)
	       (fits-constraints? board (cdr rem-constraints))))))))

  (define (all-iter? op board)
    (define (iter n)
      (if (> n size)
	  true
	  (and (op n board)
	       (iter (+ n 1)))))
    (iter 1))

  (define (valid? board)
    (and (all-iter? valid-row? board)
	 (all-iter? valid-col? board)
	 (fits-constraints? board constraints)))

  (define (x)
    (and (all? valid-row? board)
	 (all? valid-col? board)))

  (define (try-values-for-cell i j so-far)
    (define (iter n)
      (let ((new-board (cons (list (list i j) n) so-far))
	    (done (= i j 1))
	    (next-j (if (= i 1) (- j 1) j))
	    (next-i (if (= i 1) size (- i 1))))
	(if (> n size)
	    '()
	    (if (valid? new-board)
		(if done
		    new-board
		    (cons (try-values-for-cell next-i next-j new-board) (iter (+ n 1))))
		(iter (+ n 1))))))
    (iter 1))
  (try-values-for-cell size size '()))


(solve-kenken 4 
'((7 + ((1 1) (1 2)))
(2 - ((2 1) (2 2)))
(2 / ((3 1) (4 1)))
(24 * ((3 2) (4 2) (4 3)))
(2 / ((1 3) (1 4)))
(5 + ((2 3) (3 3)))
(2 = ((2 4)))
(1 - ((3 4) (4 4)))))
