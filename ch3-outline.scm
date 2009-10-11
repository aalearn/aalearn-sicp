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
	      (set-value! a (sqrt (get-value b)))
	      me))
        (if (has-value? a)
	    (begin
	      (set-value! b (square (get-value a)))
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

;; * _ Section 3.4
;;  * _ Exercise 3.38
; a. all possible values for balance:
; Pe, Pa, Ma: 45
; Pe, Ma, Pa: 35
; Pa, Pe, Ma: 45
; Pa, Ma, Pe: 50
; Ma, Pa, Pe: 40
; Ma, Pe, Pa: 40
; b. if processes can be interleaved:
; Pe-access, Pa-access, Pe-set, Ma-access, Ma-set, Pa-set: 80
; Pe-access, Pe-set, Ma-access, Pa-access, Pa-set, Ma-set: 55
; Pa-access, Pa-set, Ma-access, Pe-access, Ma-set, Pe-set: 90
; Note, many more possibilities exist, especially considering that Ma-access
; can be broken up into two different accesses.

;;  * _ Exercise 3.39
; a = (s (lambda () (* x x)))
; b = set! x
; c = (lambda () (set! x (+ x 1)))
; (a, b, c) -> 101
; (a, c, b) -> 100
; (c, b, a) -> 121

;;  * _ Exercise 3.40
; a = (set! x (* x x))   = a-set, a1, a2
; b = (set! x (* x x x)) = b-set, b1, b2, b3
; a, b -> 10^6
; b, a -> 10^6
; a1, a2, b1, b2, b3, a-set, b-set -> 10^3
; a1, a2, b1, b2, b3, b-set, a-set -> 10^2
; a1, a2, b1, a-set, b2, b3, b-set -> 10^5
; a1, a2, b1, b2, a-set, b3, b-set -> 10^4
; several other ways to get 10^4
; If we serialize, only the 10^6 outcome remains.

;;  * _ Exercise 3.41
; Do not agree, access to the bank balance is already 'atomic' and can't on its own cause anomalous behavior.

;;  * _ Exercise 3.42
; Constructed protected versions might introduce one issue, depending on how it is implemented (the below
; implementation does seem to have this problem) -- two calls to withdraw won't actually be serialized separately;
; they will be able to run concurrently since they are calls to the same serialized procedure.

;;  * _ Exercise 3.43
; If all processes are run sequentially, any given operation leaves 10, 20, and 30 (in some order) as the value
;  the three accounts.  So running many operations will still maintain this, by induction.
; If we have two exchanges happening concurrently, e1 (on 1 and 2), e2 (on 1 and 3):,
;  e1-acc-1-access, e1-acc-2-access, e2-acc-1-access, e2-acc-3-access, e1-set-acc-1, e1-set-acc-2, e2-set-acc-1, e2-set-acc-3
;  e1: 10, 20 -> diff=-10,              set-acc-1 to 20, set-acc-2 to 10
;                    e2: 10, 30 -> diff=-20,                           set-acc-1 to 40, set-acc-3 to 10
; This results in 40, 10, 10.  You'll note that as long as the withdraws and deposits are serialized,
;  every call to exchange must have a net-zero impact on the accounts, it adds to one account exactly the same amount
;  it withdraws from another.  So the sum will always be the same.
; If withdraw/deposit are not serialized then the "withdraw -20" on acc-1 in e2 above could be broken down so 
;  that it looks at the balance of account 1 when it's still 10, then in the background it gets set to 20 by a different
;  procedure, then e2 would set it to 30 instead of 40.  Then the account balances would be 30, 10, 10, and money has
;  been lost.
 
;;  * _ Exercise 3.44
; The transfer method works adequately because 'withdaw and 'deposit are already themselves serialized
;  and since the amount is specified as a number, no problems arise from concurrent processes between a read
;  and a write to the same balance.  The difference between this and the exchange problem is that in order
;  for the exchange to work, it needs to read an account balance, then write to an account balance -- reads
;  and writes, when not serialized, may cause unexpected behavior.

;;  * _ Exercise 3.45
; Looks as if in this case if you call serialized-exchange, we'll run a serialized procedure inside of another 
;  serialized procedure that is acting on the same account.  This would cause a deadlock.

;;  * _ Exercise 3.46
; In this sequence, both instances of test-and-set acquire a mutex, so the mutex is not truly
;  performing its stated purpose:
; tas1: (if (car cell)) => #f     -> (set-car! cell true) 
; tas2:    (if (car cell) => #f                          -> (set-car! cell true)

;;  * _ Exercise 3.47
; a. a semaphore in terms of mutexes
(define (make-semaphore n)
  (let ((cells (list))
	(semaphore-mutex (make-mutex)))
    (define (the-semaphore m)
      (semaphore-mutex 'acquire)
      (cond ((eq? m 'acquire) 
	     (if (< (length cells) n)
		 (set! cells (cons (make-mutex) cells)))
	     ((car cells) 'acquire))
	    ((eq? m 'release)
	     ((car cells) 'release)
	     (set! cells (cdr cells))))
      (semaphore-mutex 'release))
    the-semaphore))

; b. a semaphore in terms of atomic test-and-set! operations
(define (make-semaphore n)
  (define (semaphore-list m)
    (if (= m 0) 
	() 
	(cons false (semaphore-list (- m 1)))))
  (define (acquire-first-free l)
    (if (test-and-set! (car l))
	(if (null? (cdr l))
	    (acquire-first-free l)
	    (acquire-first-free (cdr l)))
	false))
  (define (release-one l)
    ; this part probably doesn't work, since it's not atomic
    ; not sure how to fix it
    (if (car l)
	(set-car! l false)
	(release-one (cdr l))))
  (let ((cells (semaphore-list n)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire-first-free cells))
	    ((eq? m 'release) (release-one cells))))
    the-semaphore))


;;  * _ Exercise 3.48
; A system where the smaller-number-account is protected first solves the exchange problem because two different
;  processes will either acquire a lock, and then proceed to acquire the remainder of the locks or fail when 
;  trying to acquire the first lock.  Therefore, you never have multiple processes which concurrently hold
;  locks on some resoruce, and therefore you cannot have a deadlock created by exchange processes.

(define (make-account-and-serializer balance number)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
	    ((eq? m 'number) number)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (if (< (account1 'number) (account2 'number))
      (let ((serializer1 (account1 'serializer))
	    (serializer2 (account2 'serializer)))
	((serializer1 (serializer2 exchange))
	 account1
	 account2))
      (let ((serializer2 (account2 'serializer))
	    (serializer1 (account1 'serializer)))
	((serializer2 (serializer1 exchange))
	 account1
	 account2))))

;;  * _ Exercise 3.49

; Define a procedure which, given an account A, finds the account closest in balance to it B,
;  and takes one dollar from B and adds it to A.  In this case, you can't use the same procedure
;  of finding the lower numbered one first, because you actually have to check the balance of A
;  before you know which account to look for, and if you expect the behavior to be consistent
;  you might createa deadlock where another process performing the same procedure locks B and then
;  attempts to get a lock on A.



;; * _ Section 3.5
;;  * _ Exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;;  * _ Exercise 3.51
(define x (stream-map show (stream-enumerate-interval 0 10))) ; shows 0
(stream-ref x 5) ; shows 0 - 5
(stream-ref x 7) ; shows 6 - 7, since the previous ones have already been memoized
; above includes only the "displayed" section, the return values should be x, 5, 7 respectively

;;  * _ Exercise 3.52
; value of sum after each expression
(define sum 0)            ; => 0
(define (accum x)
  (set! sum (+ x sum))
  sum)                    ; => 0
(define seq (stream-map accum (stream-enumerate-interval 1 20))) ; => 1
(define y (stream-filter even? seq))                             ; => 6 = (+ 1..3)
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) 
                         seq))                                   ; => 10 = (+ 1..4)
(stream-ref y 7)     ; => 136 = (1->4, 2->7, 3->8, 4->11, 5->12, 6->15, 7->16 => + 1..16)
(display-stream z)   ; => 210 (adds to 20)
; note that above I give the value 'sum', not the output or return values
; since sum is returned after each call to accum, stream-ref will also return 136
; display-stream will show all the interim sums that are divisible by 5, which are:
; 10, 15, 45, 55, 105, 120, 190, 210 

; These responses would have differed considerably if we had not memoized, since each
; expression would restart the enumeration from 1, and we would increase sum with each
; expression, even if we were evaluating elements we had seen before.

;;  * _ Exercise 3.53
; (1 2 4 8 16 ...)

;;  * _ Exercise 3.54
(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))
(stream-ref factorials 4) ; => 120 = n+1 factorial, as requested

;;  * _ Exercise 3.55
(define (partial-sums s)
  (cons-stream (stream-car s) 
	       (add-streams (partial-sums s) (stream-cdr s))))

; inimino's version below avoids re-calculation at every step 
;  need to avoid using the (partial-sums s) stream, since it won't get memoized properly
(define (partial-sums s)
  (define stream (cons-stream 0 (add-streams s stream)))
  (stream-cdr stream))

(display-stream (partial-sums (stream-enumerate-interval 1 5))) ; => 1 3 6 10 15

;;  * _ Exercise 3.56
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))
(define S (cons-stream 1 (merge (scale-stream S 2) 
				(merge (scale-stream S 3)
				       (scale-stream S 5)))))
; test it
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))
(define (test-display s start finish)
  (display-stream 
   (stream-map (lambda (x) 
		 (stream-ref s x))
	       (stream-enumerate-interval start finish))))
(test-display S 20 25) ; => 40 45 48 50 54 60

;;  * _ Exercise 3.57
; the nth Fibonacci number (counting starting at 0) requires us to have n additions
; If we did not memoize, we would be recomputing each addition for each element -- O(2^n) additions

;;  * _ Exercise 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

; This procedure performs long division in an arbitrary base, 
;  returning the 'digits' in a stream.
(expand 1 7 10) ; => should be 1 4 2 8 5 7 repeating
(test-display (expand 1 7 10) 0 12) ; looks good!

(expand 3 8 10) ; => should be 3 7 5 then 0s forever
(test-display (expand 3 8 10) 0 12) ; looks good!

;;  * _ Exercise 3.59
; a. 
(define (integrate-series s)
  (mul-streams s (stream-map (lambda (x) (/ 1 x)) integers)))
(test-display (integrate-series ones) 0 3) ; => 1 1/2 1/3 1/4

; b.
; deriv(cos) = -sin => cos = -integ(sin)
(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(test-display cosine-series 0 6) ; check
(test-display sine-series 0 6)   ; check

;;  * _ Exercise 3.60
; take
;  a0 + a1x + a2x^2 + ... = series a
;  b0 + b1x + b2x^2 + ... = series b
; then a * b =
;  a0*b0 + (a1*b0 + b1*a0)x + (a2b0 + a1b1 + b2a0)x^2 ... ->
;  a0*b0    a1*b0              a2*b0                 a3*b0 ...
;           b1*a0              b2*a0                 b3*a0 ...
;                              a1*b1                 a2*b1 ...
;                                                    b2*a1 ...
; collect all the a0 terms to the bottom ...
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (mul-series (stream-cdr s1) s2)
			    (scale-stream (stream-cdr s2) (stream-car s1)))))

(test-display (mul-series ones ones) 0 6)         ; => 1 2 3 4 5 6 7
(test-display (mul-series integers integers) 0 6) ; => 1 4 10 20 ...

(test-display (add-streams (mul-series sine-series sine-series)
			   (mul-series cosine-series cosine-series))
	      0 8) ; => 1 0 0 0 0 ...
; not easy!

;;  * _ Exercise 3.61
(define (invert-unit-series s)
  (cons-stream 1 (scale-stream (mul-series (invert-unit-series s) (stream-cdr s)) -1)))

(test-display (invert-unit-series integers) 0 9) ; => 1 -2 1 0 0 0 0 ...
; does that really work? (1 + 2x + 3x^2 + 4x^3 + ...) * (1 - 2x + x^2) = 1 + 0 + 0 ... it does!
(test-display (mul-series integers (invert-unit-series integers)) 0 9) ; => 1 0 0 0 ...

; to make more efficient, make memoizable by including internal define

;;  * _ Exercise 3.62
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "Error -- Divisor series cannot have constant term of 0")
      (let ((scale-factor (/ 1 (stream-car s2))))
	(scale-stream 
	 (mul-series s1 
		     (invert-unit-series (scale-stream s2 scale-factor))) 
	 scale-factor))))

(test-display (div-series (scale-stream sine-series 1/5) (scale-stream cosine-series 1/5)) 0 7) ; => 0 1 0 1/3 0 2/15 0 17/315 check!

;;  * _ Exercise 3.63
; In the case of sqrt-stream, we're passing an argument into it, e.g. (sqrt-stream 2). This causes
; the useful memoization to not operate.  Turning the stream into another variable (a procedure which
; takes no arguments) means the memoization can occur as it did before.  If we skipped memoization,
; the performance benefit of the local variable would go away.

;;  * _ Exercise 3.64
(define (average a b) (/ (+ a b) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)
(test-display (sqrt-stream 2) 0 7)

(define (stream-limit s tolerance)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1)))
    (if (< (abs (- s1 s0)) tolerance)
	s1
	(stream-limit (stream-cdr s) tolerance))))

(stream-limit (sqrt-stream 2) 0.1) ; => 1.41666

;;  * _ Exercise 3.65
(define ln-2-stream
  (partial-sums (stream-map (lambda (x) (/ (if (even? x) -1.0 1.0) x)) integers)))

(test-display ln-2-stream 0 5) ; check

(log 2) ; => .6931471805599453
(test-display ln-2-stream 0 8) ; => .7456349206349207 (last value) (within 0.1)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(test-display (euler-transform ln-2-stream) 0 8) ; => .6932539682539683 (last value) (correct to 3 digits)

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(test-display (accelerated-sequence euler-transform ln-2-stream) 0 8) ; => .6931471805599427 (correct to 14 digits!)

;;  * _ Exercise 3.66
; After picking the first elements of the lists (1 1), the pairs function interleaves all pairs of the form (1 n)
; with the remainder of the pairs needed, which means, which in turn means (2 n) interleaved with the remainder.
; Following this pattern, you'll notice that (1 100) will have 99 items before it of the form (1 n) and 98 items
; before it not of the form (1 n), so 197 total items. To go further, writing out some examples
; 1-1 1-2 2-2 1-3 2-3 1-4 3-3 1-5 2-4 1-6 3-4 1-7 2-5 1-8 4-4 1-9 
; 0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15
; Look at just where pairs beginning with m start (# of pairs before them)
; m=1 -> 0, 2 -> 2, 3 -> 6, 4 -> 14
; => pairs before (m m) = 2^m - 2
; => pairs before (m m+1) = 2^m - 2+2^(m-1)
; => pairs before (m p) = 2^m - 2 + (p - m)*2^(m-1) = 2^(m-1)*(2 + p - m) - 2 
; => pairs before (99 100) = 3 * 2^98 - 2
;  & pairs before (100 100) = 2^100 - 2

;;  * _ Exercise 3.67
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list x (stream-car t))) 
		(stream-cdr s))
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
		 (stream-cdr t))
     (pairs (stream-cdr s) (stream-cdr t))))))

(test-display (pairs integers integers) 0 20) ; works

;;  * _ Exercise 3.68
; This stream never yields a first element, since when we inteleave pairs.

;;  * _ Exercise 3.69
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))  ; i = j = k
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) (stream-car t) x))
		 (stream-cdr u))                        ; i = j <= k
     (stream-map (lambda (x) (cons (stream-car s) x))
                (pairs (stream-cdr t) (stream-cdr u)))) ; i < j <= k
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(test-display (triples integers integers integers) 0 20) ; => looks okay

(define pythagorean-triples
  (stream-filter (lambda (x) (= (+ (square (car x)) (square (cadr x))) (square (caddr x))))
		 (triples integers integers integers)))

;;  * _ Exercise 3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (if (< (weight s1car) (weight s2car))
	       (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))
               (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

; a
(define pos-by-sum (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x)))))
(test-display pos-by-sum 0 10)

; b
(define (indivisible-by-2-3-5 x)
  (not
   (or
    (= 0 (remainder x 2))
    (= 0 (remainder x 3))
    (= 0 (remainder x 5)))))
(define (both x y test)
  (and (test x) (test y)))
(define pos-by-formula 
  (stream-filter (lambda (x) (both (car x) (cadr x) indivisible-by-2-3-5))
		 (weighted-pairs integers integers
				 (lambda (x)
				   (let ((i (car x))
					 (j (cadr x)))
				     (+
				      (* 2 i)
				      (* 3 j)
				      (* 5 i j)))))))

(test-display pos-by-formula 0 16)

;;  * _ Exercise 3.71
(define (cube x) (* x x x))
(define (sum-cubes pair)
  (+ (cube (car pair)) (cube (cadr pair))))

(define (dupes s)
  (if (= (stream-car s) (stream-car (stream-cdr s)))
      (cons-stream (stream-car s) (dupes (stream-cdr s)))
      (dupes (stream-cdr s))))

(test-display (dupes ones) 0 2)

(define ramanujan-numbers
  (dupes 
   (stream-map sum-cubes
	       (weighted-pairs integers integers sum-cubes))))


(test-display ramanujan-numbers 0 5)
; 1729
; 4104
; 13832
; 20683
; 32832
; 39312

;;  * _ Exercise 3.72
(define (sum-squares pair)
  (+ (square (car pair)) (square (cadr pair))))
(define (three-in-a-row s)
  (if (= (stream-car s) (stream-ref s 1) (stream-ref s 2))
      (cons-stream (stream-car s) (three-in-a-row (stream-cdr s)))
      (three-in-a-row (stream-cdr s))))

(define ramanujan-square-triples
  (three-in-a-row
   (stream-map sum-squares
	       (weighted-pairs integers integers sum-squares))))
(test-display ramanujan-square-triples 0 5)
; 325
; 425
; 650
; 725
; 845
; 850

;;  * _ Exercise 3.73
(define (RC r c dt)
  (define (voltages i-stream v0)
    (define inner
      (cons-stream v0
		   (add-streams (scale-stream i-stream (/ dt C))
				inner)))
    (define include-resistance
      (add-streams inner (scale-stream i-stream r)))
    include-resistances)
  voltages)

;;  * _ Exercise 3.74
(define (sign-change-detector next prev)
  (cond ((and (< prev 0) (>= next 0)) 1)
	((and (>= prev 0) (< next 0)) -1)
	(else 0)))

(sign-change-detector -4 2)  ; => -1
(sign-change-detector -4 -2) ; => 0
(sign-change-detector -4 0)  ; => -1
(sign-change-detector 4 -1)  ; => 1
(sign-change-detector 4 0)   ; => 0

; Alyssa's version -- from book	
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define sense-data (cons-stream -1 (cons-stream 4 (cons-stream 3 (cons-stream -2 sense-data)))))
(define zero-crossings (make-zero-crossings sense-data 0))
(test-display zero-crossings 0 4) ; => -1, 1, 0, -1, 0

; Eva's version
(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))
(test-display zero-crossings 0 4) ; => -1, 1, 0, -1, 0


;;  * _ Exercise 3.75
; Louis's version computes the current average by averaging the current value with the previous
; average.  Instead it should compute the average using the previous value.  Fixed here:
(define (make-zero-crossings input-stream last-value last-avg)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avg)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
				      avpt))))


;;  * _ Exercise 3.76
(define (smooth stream)
  (define (average x y) (/ (+ x y) 2)) 
  (define smoothed
    (stream-map average stream (stream-cdr stream)))
  smoothed)

(test-display (smooth sense-data) 0 5) ; => 3/2, 7/2, 1/2, -3/2  ...


;;  * _ Exercise 3.77
; from book
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

; exercise -- modify alternate version of integral to accept delayed argument
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
	       (let ((integrand (force delayed-integrand)))
		 (if (stream-null? integrand)
		     the-empty-stream
		     (integral (delay (stream-cdr integrand))
			       (+ (* dt (stream-car integrand))
				  initial-value)
			       dt)))))

(stream-ref (solve (lambda (y) y) 1 0.001) 1000) ; =>  2.716923932235896

;;  * _ Exercise 3.78
(define (solve-2nd a b y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams
	       (scale-stream dy a)
	       (scale-stream y b)))
  y)

; consider tracking down an example...

;;  * _ Exercise 3.79
(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;;  * _ Exercise 3.80
(define (RLC R L C dt)
  (define (states-stream vC0 iL0)
    (define vC-stream
      (cons-stream vC0
		   (scale-stream
		    (integral (delay iL-stream) 0 dt)
		    (/ -1 C))))
    (define iL-stream
      (cons-stream iL0
		   (add-streams 
		    (scale-stream (integral (delay vC-stream) 0 dt) (/ 1 L))
		    (scale-stream (integral (delay iL-stream) 0 dt) (- (/ R L))))))
    (stream-map cons vC-stream iL-stream))
  states-stream)

(define sample-RLC (RLC 1 1 0.2 0.1))
(test-display (sample-RLC 10 0) 0 5) 
 
; (10 . 0)
; (0 . 0)
; (0 . 1.)
; (0 . 1.)
; (-.5 . .9)
; (-1. . .8)
; check if correct

;;  * _ Exercise 3.81
(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (random-generator request-stream)
  (define (inner-stream requests stream-in-use)
    (let ((stream-to-use
	   (cond ((eq? (stream-car requests) 'generate) stream-in-use)
		 ((eq? (stream-car requests) 'reset) random-numbers) 
		 (else (error "Cannot understand request")))))
      (cons-stream (stream-car stream-to-use)
		   (inner-stream (stream-cdr requests) (stream-cdr stream-to-use)))))
  (define the-stream (inner-stream request-stream random-numbers))
  the-stream)

(define sample-request-stream 
  (cons-stream 'generate (cons-stream 'generate (cons-stream 'reset sample-request-stream))))

(test-display sample-request-stream 0 10) ; => gen, gen, res, gen, gen, res ...
(test-display random-numbers 0 10) ; => 19, 18, 6, 17, 25 ... (based on test generator above)

(test-display (random-generator sample-request-stream) 0 10) ; => 19, 18, 19, 18, 6, 19, 18, 6 ...


;;  * _ Exercise 3.82
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (random-stream low high)
  (define inner-stream
    (cons-stream (random-in-range low high)
		 (random-stream low high)))
  inner-stream)
(test-display (random-stream 0 100.0) 0 10) ; => looks ok

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))


(define (estimate-integral p x1 x2 y1 y2)
  (scale-stream
   (monte-carlo
    (stream-map p (random-stream x1 x2) (random-stream y1 y2))
    0 0)
   (* (- x2 x1) (- y2 y1))))

(define (test-circle x y)
  (<= (+ (square x) (square y)) 1.0))

(define pi
  (estimate-integral test-circle -1.0 1.0 -1.0 1.0))

(test-display pi 1500 1510) ; => 3.1422899 -- getting there 

