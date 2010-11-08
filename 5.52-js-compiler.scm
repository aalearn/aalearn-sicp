;;;;COMPILER FROM SECTION 5.5 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch5.scm

;;;;This file can be loaded into Scheme as a whole.
;;;;**NOTE**This file loads the metacircular evaluator's syntax procedures
;;;;  from section 4.1.2
;;;;  You may need to change the (load ...) expression to work in your
;;;;  version of Scheme.

;;;;Then you can compile Scheme programs as shown in section 5.5.5

;;**implementation-dependent loading of syntax procedures
(load "ch5-syntax.scm")			;section 4.1.2 syntax procedures


;;;SECTION 5.5.1

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

	; added
	((let? exp) (compile (let->combination exp) target linkage))
	((error? exp) (compile (error->combination exp) 'val 'return))

        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))


(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() ""))


;;;SECTION 5.5.2

;;;linkage code

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue_to) '() (code-line "branch = continue_to;\nbreak")))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence 
	  '() '()
	  (code-line "branch = '" (x->string linkage) "';\n  break")))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue_to)
   instruction-sequence
   (compile-linkage linkage)))


;;;simple expressions
(define (x->string e) 
  (cond ((number? e) (number->string e))
	((symbol? e) (symbol->string e))
	(else e)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence 
    '() (list target)
    (code-line (symbol->string target) " = " (x->string exp)))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence 
    '() (list target)
    (code-line (symbol->string target) " = " (text-of-quotation exp)))))

(define (compile-variable exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence 
    '(env) (list target)
    ;; TODO: check if variable is unbound?
    (code-line (symbol->string target) " = lookup_variable_value('" (x->string exp) "', env)"))))

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence 
       '(env val) (list target)
       (code-line "set_variable_value(" (x->string var) ", val, env)"))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence 
       '(env val) (list target)
       (code-line "define_variable('" (symbol->string var) "', val, env);\n"
		      "  " (symbol->string target) " = 'ok: " (symbol->string var) " set'"))))))


;;;conditional expressions

;;;labels (from footnote)
(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string-append (symbol->string name)
		 (number->string (new-label-number))))
;; end of footnote

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage))
            (a-code
             (compile (if-alternative exp) target linkage)))
        (preserving '(env continue_to)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence 
	   '(val) '()
	   ; note: in the original, we "fell through" automatically to the t-branch
	   ; this is possible, but messier in js, so we just make that an extra "step"
	   (code-line "branch = val ? '" t-branch "' : '" f-branch "';\n  break"))
          (parallel-instruction-sequences
           (append-instruction-sequences (label-header t-branch) c-code)
           (append-instruction-sequences (label-header f-branch) a-code))
          (label-header after-if)))))))

;;; sequences

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue_to)
       (compile (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq) target linkage))))

;;;lambda expressions

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence 
	  '(env) (list target)
	  (code-line (symbol->string target) " = make_compiled_procedure('" proc-entry "', env)")))
        (compile-lambda-body exp proc-entry))
       (label-header after-lambda)))))


(define (scheme-list->js a)
  (define (string-join items delimiter)
    (cond ((null? items) "")
	  ((null? (cdr items)) (car items))
	  (else (string-append (car items) delimiter (string-join (cdr items))))))
  (string-join (map (lambda (x) (string-append "'" (x->string x) "'")) a) ","))
			
(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence 
      '(env proc argl) '(env)
      (string-append (label-header proc-entry)
		     (code-line "env = compiled_procedure_env(proc)")
		     (code-line "env = extend_environment(" (scheme-list->js formals) ", argl, env)")))
     (compile-sequence (lambda-body exp) 'val 'return))))


;;;SECTION 5.5.3

;;;combinations

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving '(env continue_to)
     proc-code
     (preserving '(proc continue_to)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl) (code-line "argl = []"))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence 
		 '(val) '(argl)
		 (code-line "argl = [val]")))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence 
	   '(val argl) '(argl)
	   (code-line "argl.unshift(val)")))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

;;;applying procedures
(define (label-header label)
  ;; TODO: move break to a less hacky place
  (string-append "case '" label "':\n"))

; modified heavily
(define (compile-procedure-call target linkage)
  (let ((compile-procedure-call-start (make-label 'compile-procedure-call-start)) ; added for "apply"
	(explicit-apply-branch (make-label 'explicit-apply-branch))               ; added for "apply" 
	(primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
	(interpreted-branch (make-label 'interpreted-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (append-instruction-sequences
	(label-header compile-procedure-call-start)
	(make-instruction-sequence 
	 '(proc) '()
	 (string-append "  if (primitive_procedure(proc)) {\n"
			"    branch = '" primitive-branch "';\n    break;\n"
			"  } else if (compound_procedure(proc)) {\n"
			"    branch = '" interpreted-branch "';\n    break;\n"
			;; TODO: explicit_apply does not yet work -- see argl? below as well
			"  } else if (explicit_apply_procedure(proc)) {\n"
			"    branch = '" explicit-apply-branch "';\n    break;\n"
			"  }\n")))
       (parallel-instruction-sequences
	(append-instruction-sequences
	 (label-header compiled-branch)
	 (compile-proc-appl target compiled-linkage))
	(parallel-instruction-sequences
	 (append-instruction-sequences
	  (label-header interpreted-branch)
	  (interpreted-proc-appl target compiled-linkage))
	 (parallel-instruction-sequences
	  ; code added just to check in real time whether
	  ; this is an explicit (apply ...) -- quite inefficient!
	  (append-instruction-sequences                         
	   (label-header explicit-apply-branch)
	   (make-instruction-sequence                           
	    '(proc argl) '(proc argl)
	    (string-append "  proc = explicit_apply_procedure(proc);\n"
			   "  // argl? = explicit-apply-args argl;\n"
			   "  branch = " compile-procedure-call-start "l\;\n"
			   "  break;\n")))
	  (append-instruction-sequences
	   (label-header primitive-branch)
	   (end-with-linkage linkage
            (make-instruction-sequence 
	     '(proc argl) (list target)
	     ;; TODO: need primitive support from eceval.js?
	     ;; the current approach won't work for car/cdr, right?
	     ;; TODO: fix eval cheat here
	     (code-line (symbol->string target) " = proc[1](argl)")))))))
       (label-header after-call)))))

;;;applying compiled procedures

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence 
	  '(proc) all-regs
	  (code-line "continue_to = '" (x->string linkage) "';\n"
		     "  branch = compiled_procedure_entry(proc);\n"
		     "  break")))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence 
	    '(proc) all-regs
	    (code-line "continue_to = '" proc-return "';\n"
		       "  branch = compiled_procedure_entry(proc);\n"
		       "  break;\n"
		       (label-handler proc-return)
		       (symbol->string target) " = val;\n"
		       "branch = '" (symbol->string linkage) "';\n"
			   "break"))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence 
	  '(proc continue_to) all-regs
	  (code-line "branch = compiled_procedure_entry(proc);\n  break")))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

;;;applying interpreted procedures
(define (interpreted-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence 
	  '(proc) all-regs
	  (string-append "  continue_to = '" linkage "';\n"
			 "  save(continue_to);\n" ; why?
			 "  branch = compapp;\n"
			 "  break;\n")))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence 
	    '(proc) all-regs
	    (string-append "  continue_to = '" proc-return "';\n"
			   "  save(continue_to);\n" ; why?
			   "  branch = compapp;\n"
			   "  break;\n"
			   (label-header proc-return)
			   (code-line target " = val")
			   "  branch = '" linkage "';\n"
			   "  break;\n"))))
        ((and (eq? target 'val) (eq? linkage 'return))
	 (make-instruction-sequence 
	  '(proc continue_to) all-regs
	  (string-append "  save(continue_to);\n" ; why?
			 "  branch = compapp;\n"
			 "  break;\n")))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

;; footnote
(define all-regs '(env proc val argl continue_to))


;;;SECTION 5.5.4

(define (registers-needed s)
  (if (string? s) '() (car s)))

(define (registers-modified s)
  (if (string? s) '() (cadr s)))

(define (statements s)
  (if (string? s) s (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))


(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (string-append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (string-append (code-line "save(" (symbol->string first-reg) ")")
			     (statements seq1)
			     (code-line (symbol->string first-reg) " = restore()")))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (string-append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (string-append (statements seq1) (statements seq2))))


;; Tools for generating compiled code
(define (code-line . x)
  (string-append "  " (apply string-append x) ";\n"))

;; Tools for working conveniently with compiler
(define (compile-with-wrapper exp)
  (string-append "function step() {\n"
		 "switch (branch) {\n"
		 "case 'main':\n\n"
		 (caddr (compile exp 'val 'next))
		 "\n\n  branch = 'done';\n"
		 "break;\n"

		 "case 'unbound_variable':\n" ;; HACK: traps js proc lookup errors
		 "case 'unbound-variable':\n"
		 "  val = 'unbound-variable!';\n"
		 "  branch = 'signal-error';\n"
		 "case 'signal-error':\n"
		 "  val = 'ERROR: ' + val;\n"
		 "  branch = 'done';\n"
		 "  break;\n"
		 "default:\n"
		 "  val = 'COMPILER ERROR: bad-branch: ' + branch;\n"
		 "  break;\n"
		 "} }\n"))

(define (compile-to-file exp)
  (with-output-to-file "compiled.js"
    (lambda ()
      (write-string (compile-with-wrapper exp)))))
      
'(COMPILER LOADED)
