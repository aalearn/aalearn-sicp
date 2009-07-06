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
; Consider the interaction

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
      (error "FRONT called with an empty queue" queue)
      (car (rear-ptr queue))))
; Deleting an object from the rear of the list requires a backwards
;  pointer from the nth object to the n-1 (doubly linked list) 
;  => instead of pairs, we need triples (item (prev-ptr next-ptr))
(define (make-triple item) (cons item (cons '() '())))
(define (front-insert-deque! queue item)
  (let ((new-triple (make-triple item)))
    (cond ((empty-deque? queue)
           (set-front-ptr! queue new-triple)
           (set-rear-ptr! queue new-triple)
           queue)
          (else
           (set-cdr! (cdr new-triple) (front-ptr queue))
           (set-car! (cdr (front-ptr queue)) new-triple) 
           (set-front-ptr! queue new-triple)
           queue))))
(define (rear-insert-deque! queue item)
  (let ((new-triple (make-triple item)))
    (cond ((empty-deque? queue)
           (set-front-ptr! queue new-triple)
           (set-rear-ptr! queue new-triple)
           queue)
          (else
           (set-cdr! (cdr (rear-ptr queue)) new-triple)
	   (set-car! (cdr new-triple) rear-ptr)
           (set-rear-ptr! queue new-triple)
           queue))))
(define (only-one-item? queue) (eq? (front-ptr queue) (rear-ptr queue)))
(define (front-delete-deque! queue)
  (cond ((empty-deque? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
	 (if (only-one-item? queue)
	     (set-rear-ptr! queue '())
	     (set-car! (cddr (front-ptr queue)) '()))
         (set-front-ptr! queue (cddr (front-ptr queue)))
         queue)))
(define (rear-delete-deque! queue)
  (cond ((empty-deque? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
	 (if (only-one-item? queue)
	     (set-front-ptr! queue '())
	     (set-cdr! (cadr (rear-ptr queue)) '())) 
         (set-rear-ptr! queue (cadr (rear-ptr queue)))
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
(print-deque (front-delete-deque! dq1))  ; => (c a b d)

(cadr (make-triple 'b))
