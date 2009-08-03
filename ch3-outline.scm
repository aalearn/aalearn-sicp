;; * _ Section 3.1

;;  * _ Exercise 3.1
(define (make-accumulator accum)
  (lambda (amount)
    (set! accum (+ accum amount))
    accum))

(define A (make-accumulator 5))
(A 10) ; => 15
(A 10) ; => 25

;;  * _ Exercise 3.2
(define (make-monitored fn)
  (let ((calls 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) calls)
	    ((eq? arg 'reset-count)
	     (set! calls 0))
	    (else
	     (begin
	       (set! calls (+ 1 calls))
	       (fn arg)))))))

(define ms (make-monitored square))
(ms 'how-many-calls?) ; => 0
(ms 5) ; => 25
(ms 'how-many-calls?) ; => 1
(ms 12) ; => 144
(ms 'how-many-calls?) ; => 2

;;  * _ Exercise 3.3
(define (make-account balance password-set)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
    (if (eq? password password-set)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m)))
	(lambda (ignore) "Incorrect password")))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ; => 60
((acc 'secret-other-password 'deposit) 50) ; => "Incorrect password"

;;  * _ Exercise 3.4
(define (make-account balance password-set)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((bad-calls 0))
    (define (dispatch password m)
      (if (eq? password password-set)
	  (begin
	    (set! bad-calls 0)
	    (cond ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Unknown request -- MAKE-ACCOUNT"
			       m))))
	  (begin
	    (set! bad-calls (+ 1 bad-calls))
		  (lambda (ignore)
		    (if (> bad-calls 7)
			(call-the-cops)
			"Incorrect password")))))
  dispatch))

(define (call-the-cops)
  "Calling the cops!")
(call-the-cops) ; => "Calling the cops!"

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)      ; => 60
((acc 'secret-other-password 'deposit) 50) ; => "Incorrect password"
((acc 'secret-other-password 'deposit) 50) ; => "Incorrect password"
((acc 'secret-other-password 'deposit) 50) ; => "Incorrect password"
((acc 'secret-other-password 'deposit) 50) ; => "Incorrect password"
((acc 'secret-other-password 'deposit) 50) ; => "Incorrect password"
((acc 'secret-other-password 'deposit) 50) ; => "Incorrect password"
((acc 'secret-other-password 'deposit) 50) ; => "Incorrect password"
((acc 'secret-other-password 'deposit) 50) ; => "Calling the cops!"
((acc 'secret-other-password 'deposit) 50) ; => "Calling the cops!"
((acc 'secret-password 'withdraw) 40)      ; => 20
((acc 'secret-other-password 'deposit) 50) ; => "Incorrect password"

;;  * _ Exercise 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (*
   (- x2 x1) 
   (- y2 y1)
   (monte-carlo trials 
		(lambda () (p (random-in-range x1 x2)
			      (random-in-range y1 y2))))))

; use this to estimate pi
(define (test-circle-p x y)
  (<= (+ (square x) (square y)) 1.0))

(estimate-integral test-circle-p -1.0 1.0 -1.0 1.0 100000) ; => 3.1362

;;  * _ Exercise 3.6
; original rand function from book
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
; test versions -- not meant to be genuinely pseudo-random
(define random-init 19)
(define (rand-update val)
  (remainder (+ (* val 43) 7) 31))
(rand)

; modified version, allows resetting seed
(define rand
  (let ((x random-init))
    (lambda (method)
      (cond ((eq? method 'generate)
	     (set! x (rand-update x))
	     x)
	    ((eq? method 'reset)
	     (lambda (seed)
	       (set! x seed)
	       seed))))))
(rand 'generate)
((rand 'reset) 11)

;;  * _ Exercise 3.7

(define (make-account balance password-set)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (check-password p)
    (eq? p password-set))
  (let ((password-ok? check-password))
    (define (make-joint other-password)
      (set! password-ok?
	    (lambda (p) (or (check-password p) (eq? p other-password))))
      dispatch)
    (define (dispatch password m)
      (if (password-ok? password)
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		((eq? m 'make-joint) make-joint)
		(else (error "Unknown request -- MAKE-ACCOUNT"
			     m)))
	  (lambda (ignore) "Incorrect password")))
    dispatch))
(define (make-joint account original-password second-password)
  ((account original-password 'make-joint) second-password))

(define acc (make-account 100 'open-sesame)) ; => acc
((acc 'open-sesame 'withdraw) 40)            ; => 60
((acc 'secret-other-password 'deposit) 50)   ; => "Incorrect password"
(define paul-acc 
  (make-joint acc 'open-sesame 'rosebud))    ; => paul-acc
((paul-acc 'rosebud 'withdraw) 15)           ; => 45
((acc 'rosebud 'withdraw) 5)                 ; => 40

; better solution from inimino:
(define (make-joint account oldpw newpw)
  (lambda (p m)
    (if (eq? p newpw)
        (account oldpw m)
        "Incorrect password")))

;;  * _ Exercise 3.8
(define nil ())
(define f
  (let ((state nil))
    (lambda (signal)
      (if (eq? state nil)
	  (set! state signal))
      (/ state 2))))

(+ (f 0) (f 1)) ; => 1

;; * _ Section 3.2

;;  * _ Exercise 3.9

; recursive
; [factorial -> -------------------global-----------------------------]
; E1        -> [n=6] (each environment [] implicitly points to environment above) 
;              [n=5] 
;              [n=4] 
;              [n=3] 
;              [n=2] 
;              [n=1]

; iterative
; [ factorial -> ---------------------------------------------global------------------------------------]
; E1 -> [----------------------------------------------n=6---------------------------------------]
; fact-iter -> [p=1,c=1,m=6] [p=1,c=2,m=6] [p=2,c=3,m=6] [p=6,c=4,m=6] [p=24,c=5,m=6] [p=120,c=6,m=6] [p=720,c=6,m=6]

;;  * _ Exercise 3.10

; global -> |--------------------------------------------------------------------------|
;           | make-withdraw ...                                                        |
;           | W2 ----------------------|                                               |
;           | W1 -|                    |                                               |
;           |-----|--------------------|-----------------------------------------------|
;                 |  E1->[ia=100]      |  E2->[ia=100]
;                 |  E3->[b=100 ]      |  E4->[b=100]
;                ()()-----|            ()()------|
;                |                     |

;;  * _ Exercise 3.11

; global -> |--------------------------------------------------------------------------|
;           | make-account ...                                                         |
;           | acc-|                                                                    |
;           |-----|--------------------------------------------------------------------|
;                 |  E1->|----------------------------------------|      
;                 |      | balance = 50                           |
;                 |      | withdraw ...                           |
;                 |      | deposit ...                            |
;                 |      | dispatch                               |
;                 |      |----------------------------------------|
;                ()()-----|                      |             |
;                                       E2->|---------|        |
;                                           |amount=40|        |
;                                           |---------|        |
;                                       call to deposit        |
;                                                              |
;                                                     E3->|---------|
;                                                         |amount=60|
;                                                         |---------|
;                                                     call to withdraw
; omitted environment used when inside "dispatch"
;
; Local state for acc is kept inside E1, the environment created
;  when make-account is called.
;
; If a second account is created with make-account, the only part of the
;  environment structure which is saved is the global environment


;; * _ Section 3.3

;;  * _ Exercise 3.12
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z        ; => (a b c d)
(cdr x)  ; => <response> is (b)
; x -> [·][·]->[·][/]
;       ↓       ↓
;      [a]     [b]


(define w (append! x y))
w        ; => (a b c d)
(cdr x)  ; => <response> is (b c d)
; x -> [·][·]->[·][·]->[·][·]->[·][/]
;       ↓       ↓       ↓       ↓
;      [a]     [b]     [c]     [d]

;;  * _ Exercise 3.13
; z -> [·][·]->[·][·]->[·][·]-\
;   ↑   ↓       ↓       ↓     | 
;   |  [a]     [b]     [c]    |
;   \-------------------------/
; (last-pair z) would loop infinitely

;;  * _ Exercise 3.14
; mystery returns a reversed list
; v -> [·][·]->[·][·]->[·][·]->[·][/]
;       ↓       ↓       ↓       ↓
;      [a]     [b]     [c]     [d]

; after (define w (mystery v))
; v ----------------------------↓
; w -> [·][·]->[·][·]->[·][·]->[·][/]
;       ↓       ↓       ↓       ↓
;      [d]     [c]     [b]     [a]
; w = (d c b a)
; v = (a) (the v pointer isn't updated, but the list's cdr is)

;;  * _ Exercise 3.15
; z1 -> [·][·]
;        ↓  ↓
; x  -> [·][·]->[·][/]
;        ↓       ↓  
;      [wow]    [b]
;
; z2 -> [·][·]->[·][·]->[·][/]
;        |       ↓       ↓
;        |      [a]     [b]
;        |               ↑
;        \----->[·][·]->[·][/]
;                ↓
;              [wow]

;;  * _ Exercise 3.16
; x3 -> [·][·]->[·][·]->[·][/]
;        ↓       ↓       ↓
;       [a]     [b]     [c]
;
; x4 -> [·][·]---\
;        ↓       ↓
; x  -> [·][·]->[·][/]
;        ↓       ↓  
;       [a]     [b]
;
; x7 -> [·][·]
;        ↓  ↓
; y  -> [·][·]
;        ↓  ↓  
; z  -> [·][/]
;        ↓ 
;       [a]
;
; x-inf -> [·][·]->[·][·]->[·][/]
;           ↓  ↑     ↓      |
;          [a] |   [b]      |
;              \------------/

; proof
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define x3 (list 'a 'b 'c))
(count-pairs x3)    ; => 3

(define x (list 'a 'b))
(define x4 (cons x (cdr x)))  
(count-pairs x4)    ; => 4

(define z (cons 'a '()))
(define y (cons z z))
(define x7 (cons y y))
(count-pairs x7)    ; => 7

(define x-inf (list 'a 'b))
(set-cdr! x-inf (list 'b x-inf))
(count-pairs x-inf) ; => Aborting!: maximum recursion depth exceeded

;;  * _ Exercise 3.17
(define (count-pairs obj)
  (define found-pairs (list 'start))
  (define (inner x)
    (if (not (pair? x))
	0
	(if (memq x found-pairs)
	    0
	    (begin
	      (append! found-pairs (list x))
	      (+ (inner (car x))
		 (inner (cdr x))
		 1)))))
  (inner obj))

(count-pairs x3)    ; => 3
(count-pairs x4)    ; => 3
(count-pairs x7)    ; => 3
(count-pairs x-inf) ; => 3

; inimino's answer -- doesn't handle case of car and cdr having same pair
(define (count-pairs x)
  (define (count x seen)
    (if (or (not (pair? x)) (null? x) (memq x seen))
        0
        (+ (count (car x) (cons x seen))
           (count (cdr x) (cons x seen))
           1)))
  (count x '()))

;;  * _ Exercise 3.18
(define (find-loop obj)
  (define found-pairs (list 'start))
  (define (inner x)
    (if (not (pair? x))
	false
	(if (memq x found-pairs)
	    true
	    (begin
	      (append! found-pairs (list x))
	      (inner (cdr x))))))
  (inner obj))

(define no-loop (list 'a 'b 'c))
(find-loop no-loop)   ; => #f

(define with-loop (list 'a 'b 'c))
(set-cdr! (cddr with-loop) with-loop)
(find-loop with-loop) ; => #t

; inimino's better solution -- mutability not needed!
(define (cyclical? x)
  (define (go x seen)
    (cond ((memq x seen) true)
          ((null? x) false)
          (else (go (cdr x) (cons x seen)))))
  (go x '()))

;;  * _ Exercise 3.19
(define (find-loop obj)
  (define (check x y)
    (if (eq? x y)
	true
	(if (or (not (pair? y)) (not (pair? (cdr y))) (not (pair? (cddr y))))
	    false
	    (check (cdr x) (cddr y)))))
  (check obj (cdr obj)))

(find-loop no-loop)   ; => #f
(find-loop with-loop) ; => #t

;;  * _ Exercise 3.20
; global -> |--------------------------------------------------------------------------|
;           | cons ..., car ..., cdr ..., set-car! ..., set-cdr! ...                   |
;           | x---|                                                                    |
;           | z-| |                                                                    |
;           |---|-|--------------------------------------------------------------------|
;               | |  E1->|----------------------------------------|      
;               | |      | x = 1  (set! changes to 17)            |
;               | |      | y = 2                                  |
;               | |      |----------------------------------------|
;               | |        |
;               ()()-------|
;

;;  * _ Exercise 3.21
; The Lisp printer is printing the queue as the full contents found under the front-ptr and the rear-ptr.
; If you've deleted all items from the list, the front-ptr will point at the empty list, which is the 
; behavior observed.  However the rear-ptr still points at the last-appended item, but the interface
; does not truly give access to this item.  The last item will not be garbage-collected as long as the
; queue is kept in memory, but otherwise there are no ill effects.
(define (print-queue q)
  (display (front-ptr q)))

;;  * _ Exercise 3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT! called with empty queue")
	  (car front-ptr)))
    (define (insert-queue! x)
      (let ((new-pair (cons x '())))
	(cond ((empty-queue?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)))
	front-ptr)) ; instead of returning front-ptr, could return dispatch (not clear if that's better)
    (define (delete-queue!)
      (cond ((empty-queue?)
	     (error "DELETE! called with empty queue"))
	    (else
	       (set! front-ptr (cdr front-ptr))
	       front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
	    ((eq? m 'front-queue) front-queue)
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) delete-queue!)
	    (else (error "Undefined operation -- Queue" m))))
    dispatch))

(define q1 (make-queue))
((q1 'insert-queue!) 'a) ; => (a)
((q1 'insert-queue!) 'b) ; => (b)
((q1 'empty-queue?))     ; => #f
((q1 'front-queue))      ; => a
((q1 'delete-queue!))    ; => (b)
((q1 'front-queue))      ; => b

;;  * _ Exercise 3.23
(define (make-deque) (cons '() '()))
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-deque? queue) (null? (front-ptr queue)))
(define (front-deque queue)
  (if (empty-deque? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (rear-deque queue)
  (if (empty-deque? queue)
      (error "REAR called with an empty queue" queue)
      (car (rear-ptr queue))))
; Deleting an object from the rear of the list requires a backwards
;  pointer from the nth object to the n-1 (doubly linked list) 
;  => instead of pairs, we need triples (item (prev-ptr next-ptr))
(define (make-triple item) (cons item (cons '() '())))
(define (next-ptr item) (cddr item))
(define (prev-ptr item) (cadr item))
(define (set-next-ptr! item value) (set-cdr! (cdr item) value))
(define (set-prev-ptr! item value) (set-car! (cdr item) value))
(define (front-insert-deque! queue item)
  (let ((new-triple (make-triple item)))
    (cond ((empty-deque? queue)
           (set-front-ptr! queue new-triple)
           (set-rear-ptr! queue new-triple)
           queue)
          (else
           (set-next-ptr! new-triple (front-ptr queue))
           (set-prev-ptr! (front-ptr queue) new-triple) 
           (set-front-ptr! queue new-triple)
           queue))))
(define (rear-insert-deque! queue item)
  (let ((new-triple (make-triple item)))
    (cond ((empty-deque? queue)
           (set-front-ptr! queue new-triple)
           (set-rear-ptr! queue new-triple)
           queue)
          (else
           (set-next-ptr! (rear-ptr queue) new-triple)
	   (set-prev-ptr! new-triple rear-ptr)
           (set-rear-ptr! queue new-triple)
           queue))))
(define (only-one-item? queue) (eq? (front-ptr queue) (rear-ptr queue)))
(define (front-delete-deque! queue)
  (cond ((empty-deque? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
	 (if (only-one-item? queue)
	     (set-rear-ptr! queue '())
	     (set-prev-ptr! (next-ptr (front-ptr queue)) '()))
         (set-front-ptr! queue (next-ptr (front-ptr queue)))
         queue)))
(define (rear-delete-deque! queue)
  (cond ((empty-deque? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
	 (if (only-one-item? queue)
	     (set-front-ptr! queue '())
	     (set-next-ptr! (prev-ptr (rear-ptr queue)) '()))
         (set-rear-ptr! queue (prev-ptr (rear-ptr queue)))
         queue)))
(define (print-deque queue)
  (define (inner temp-ptr)
    (if (null? temp-ptr)
	temp-ptr
	(cons (car temp-ptr) (inner (cddr temp-ptr)))))
  (inner (front-ptr queue)))

(define dq1 (make-deque))
(print-deque (front-insert-deque! dq1 'a)) ; => (a)
(print-deque (rear-insert-deque! dq1 'b))  ; => (a b)
(print-deque (front-insert-deque! dq1 'c)) ; => (c a b)
(print-deque (rear-insert-deque! dq1 'd))  ; => (c a b d)
(print-deque (front-delete-deque! dq1))    ; => (a b d)
(print-deque (rear-delete-deque! dq1))    ; => (a b d)

(cadr (make-triple 'b))

;;  * _ Exercise 3.24
(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
	  ((same-key? key (caar records)) (car records))
	  (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
	(if record
	    (cdr record)
	    false)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
	(if record
                  (set-cdr! record value)
                  (set-cdr! local-table
			    (cons (cons key value) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define atable (make-table (lambda (x1 x2) (< (abs (- x1 x2)) 0.1))))

((atable 'insert-proc!) 3.0 6.0)
((atable 'insert-proc!) 4.0 8.0)
((atable 'lookup-proc) 3.05)      ; => 6.
((atable 'insert-proc!) 3.02 99)
((atable 'lookup-proc) 2.93)      ; => 99

;;  * _ Exercise 3.25
(define (lookup key-list table)
  (let ((subtable (assoc (car key-list) (cdr table))))
    (if subtable
	(if (null? (cdr key-list))
	    (cdr subtable)
	    (lookup (cdr key-list) subtable))
	false)))
(define (insert! key-list value table)
  (define (new-branch key-list value)
    (if (null? (cdr key-list))
	(cons (car key-list) value)
	(list (car key-list) (new-branch (cdr key-list) value))))
  (let ((subtable (assoc (car key-list) (cdr table))))
    (if subtable
	(if (null? (cdr key-list))
	    (set-cdr! subtable value)
	    (insert! (cdr key-list) value subtable))
	(set-cdr! table (cons (new-branch key-list value)
			      (cdr table))))
    'ok))
(define (make-table)
  (list '*table*))

(define kl (list 'apple 1 5))

(define btable (make-table))
(insert! (list 'apple 1 5) 'red btable)
(insert! (list 'apple 2) 'green btable)
(lookup (list 'apple 1 5) btable)          ; => red
(lookup (list 'apple 2) btable)            ; => green
(insert! (list 'apple 1 5) 'golden btable) ; => golden

;;  * _ Exercise 3.26
; If we order by key, we can structure the records by a binary tree.  There are many ways to do this,
;  but this diagram illustrates one possible way:
;      [*table*][]
;                V
;                [][]->[][]------>[][]------------------------>[][/]
;                |      |         |                            | 
;               'ckey  'cvalue    [][]->[][]->[][]->[][/]      [][]->[][]->[][]->[][/]      
;                                 |     |     |     |          |     |     |     |
;                               'akey 'avalue nil   nil      'fkey 'fvalue nil   nil 
;

;;  * _ Exercise 3.27
; global -> |--------------------------------------------------------------------------|
;           | memoize                                                                  |
;           | memo-fib-|                                                               |
;           |          |                                                               |
;           |----------|---------------------------------------------------------------|
;                      |  E1->|----------------------------------------|      
;                      |      | table = make-table, modified           |
;                      |      | f                                      |
;                      |      |----------------------------------------|
;                      |         |
;                     ()()-------|
;
; This would not have worked if we had just used (memoize fib) because the recursive calls
;  would not have themselves been memoized, therefore we would only memoize the outermost call,
;  not saving any work.

;;  * _ Exercise 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
(define (logical-or s t)
  (cond ((= s 1) 1)
        ((= t 1) 1)
	((and (= s 0) (= t 0)) 0)
        (else (error "Invalid signal" s t))))

;;  * _ Exercise 3.29
(define (or-gate in1 in2 output)
  (let ((not-in1 (make-wire))
	(not-in2 (make-wire))
	(and-out (make-wire)))
    (inverter in1 not-in1)
    (inverter in2 not-in2)
    (and-gate not-in1 not-in2 and-out)
    (inverter and-out output)))
; delaytime = 2 * inverter-delay + and-gate-delay

;;  * _ Exercise 3.30

(define ripple-carry-adder
  ... )

; Delay time for 1 half adder = 
; DSHA = and + greater of (and + inverter, or)
; DCHA = and
; For full adder =>
; DSFA = 2 * DSHA = 2a + 2(greater(a+i,o))
; DCFA = greater(DCHA,DSHA + DCHA)+or = DSha + DCha + or = 2a + (greater(a+i,o)) + o

; For n = 2
; DS2 = DSFA = 2a + 2(greater(a+i,o))
; DS1 = DCFA + DSFA = 2a + (greater(a+i,o)) + o + 2a + 2(greater(a+i,o)) = 4a + 3(greater(a+i,o)) + o
; DC = DCFA + DCFA = 2a + 2(greater(a+i,o)) + 2o

; For n = 3
; DS1 = (n-1)*DCFA + DSFA = (2a + (greater(a+i,o)) + o)*(n-1) + 2a + 2(greater(a+i,o)) = 
;     = 2na + (n+1)(greater(a+i,o)) + (n-1)o

;;  * _ Exercise 3.31

; We need to run the proc right away because running the proc will add actions to the agenda.
; If we don't run proc, we will never add that first action to the agenda, and propagating
;  will never add any further actions to the agenda.

;;  * _ Exercise 3.32

; We store a queue of items for each time segment, and we can operate that queue in two ways --
;  either FIFO (first in, first out) or LIFO (last in, first out).
; As per the hint, examine an and-gate whose inputs change from 0,1 to 1,0 in the same segment.
; Under LIFO, we would run the action-procedure in the following sequence:
;  Change in a2 from 0->1 would create an agenda item to set output signal to 1
;  Change in a1 from 1->0 would create an agenda item to set output signal to 0
;  Under LIFO, after the delay, first we would set the output to 0, then we set output to 1
;   which is the incorrect final result at the end of the time segment.
; Under FIFO, we set the output to 1 first, then 0, giving the correct final result.

;;  * _ Exercise 3.33

(define (averager a b c)
  (let ((absum (make-connector))
       ((two (make-connector))))
    (adder a b absum)
    (multiplier two c absum)
    (constant 2 two)
    'ok))

;;  *_ Exercise 3.34

; The squarer whch results will properly set b to a^2 if a is set, however it won't be 
;  reversible, which is a necessary requirement for this constraint-based programming.
; It won't be reversible because when determining a, the other divisor (a) won't be defined.

;;  * _ Exercise 3.35

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (begin
	      (set-value! a (sqrt b))
	      me))
        (if (has-value? a)
	    (begin
	      (set-value! b (* a a))
	      me))))
  (define (process-forget-value)
    (forget-value! b me)
    (forget-value! a me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;;  * _ Exercise 3.36
; global -> |--------------------------------------------------------------------------|
;           | b                                                                        |
;           | a--------|                                                               |
;           |          |                                                               |
;           |----------|---------------------------------------------------------------|
;                      |  E1->|----------------------------------------|      
;                      |      | newval = 10, constraints = ()          |
;                      |      | *                                      |
;                      |      |----------------------------------------|
;                      |         |
;                     ()()-------|
; 
; a contains a reference to an environment in which for-each-except runs
; The * in the above diagram indicates where
; Since constraints are still none, nothing meaningful will be done here in this case.

;;  * _ Exercise 3.37
(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv c)
  (let ((z (make-connector)))
    (constant c z)
    z))
