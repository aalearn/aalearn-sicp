
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
 fact-done)

; b. interative exponentiation
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

