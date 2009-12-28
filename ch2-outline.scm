;; -*- outline-regexp: ";; *\\*" -*-
;; (outline-minor-mode 1)

;;* Chapter 2
;; * _ Section 2.1.1
;  * _ Exercise 2.1
(define (make-rat n d)
  (define (fix-sign x)
    (if (< d 0) (- x) x))
  (let ((n (fix-sign n))
	(d (fix-sign d)))
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g)))))

(make-rat 4 6)
(make-rat -4 5)
(make-rat 12 -18)
(make-rat -238 -595)

;; * _ Section 2.1.2
;;  * _ Exercise 2.2
(define (make-segment a b) (cons a b))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point a) (car a))
(define (y-point a) (cdr a))

(define (average a b) (/ (+ a b) 2))

(define (midpoint-segment s)
  (make-point 
   (average (x-point (start-segment s)) (x-point (end-segment s)))
   (average (y-point (start-segment s)) (y-point (end-segment s)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (midpoint-segment 
	      (make-segment (make-point 4 3) (make-point 18 11))))

;;  * _ Exercise 2.3
; rectangle out of 2 lines
(define (make-rectangle line-1 line-2)
  (cons line-1 line-2))
(define (side-1 r) (car r))
(define (side-2 r) (cdr r))

(define (perimeter r) 
  (* 2 (+ (side-length (side-1 r)) (side-length (side-2 r)))))

(define (area r)
  (* (side-length (side-1 r)) (side-length (side-2 r))))

(define (side-length s)
  (define (delta f line)
    (- (f (end-segment line)) (f (start-segment line))))
  (sqrt (+ (square (delta x-point s)) (square (delta y-point s)))))

(let ((r (make-rectangle 
	  (make-segment (make-point 2 2) (make-point 8 8))
	  (make-segment (make-point 8 8) (make-point 10 6)))))
  (newline)
  (display (perimeter r))
  (newline)
  (display (area r)))

; also can represent it as a line and a point
(define (make-rectangle line point)
  (cons line point))
(define (side-1 r) (car r))
(define (side-2 r) (make-segment (end-segment (car r)) (cdr r)))

(let ((r (make-rectangle 
	  (make-segment (make-point 2 2) (make-point 8 8))
	  (make-point 10 6))))
  (newline)
  (display (perimeter r))
  (newline)
  (display (area r)))


;; * _ Section 2.1.3
;;  * _ Exercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(car (cons 12 4))

(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons 12 4))

;;  * _ Exercise 2.5
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (divides-times a b)
  (if (> (remainder a b) 0)
      0
      (+ 1 (divides-times (/ a b) b))))
(divides-times (* 64 81) 2)

(define (car a) 
  (divides-times a 2))

(define (cdr a)
  (divides-times a 3))

;;  * _ Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; add-1 zero
; lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) x)) x)))
; lambda (f) (lambda (x) (f (lambda (x) x) ))
(define one (lambda (f) (lambda (x) (f x))))


(((add-1 zero) square) 5)
((one square) 5)
(((add-1 one) square) 5)

; add-1 one
; lambda (f) (lambda (x) (f (f x)))
(define two (lambda (f) (lambda (x) (f (f x)))))

((two square) 5)

(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(((plus one one) square) 5)

(((plus two two) square) 3)
(square (square (square (square 3))))

;; * _ Section 2.1.4
;;  * _ Exercise 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))
;;  * _ Exercise 2.8
(define (sub-interval i j)
  (make-interval (- (lower-bound i) (upper-bound j))
		 (- (upper-bound i) (lower-bound j))))

(sub-interval (make-interval 8.1 9.1)
	      (make-interval 3.3 3.6))
;;  * _ Exercise 2.9
; w = (u - l) / 2
; Adding: show w(i + j) = f(w(i), w(j))
; w(i + j) = (u(i + j) - l(i + j))/2 = (u(i) + u(j) - l(i) - l(j))/2
;    = (u(i) - l(i))/2 + (u(j) - l(j))/2 = w(i) + w(j)
; Subtracting:
; w(i - j) = (u(i-j) - l(i-j))/ 2 = (u(i) - l(j) - l(i) + u(j))/2
;    = (u(i)-l(i))/2 + (u(j)-l(j))/2 = w(i) + w(j)
; multiplication: show that it is not a function:
;  (1, 2) * (3, 4) = (3, 8), w(i * j) = 5 =? f(w(i), w(j)) = f(1, 1)
;  (1, 2) * (1, 2) = (1, 4), w(i * j) = 3 =? f(1, 1)
;  so f is not a function
; division:
;  (4, 5) / (1, 2) = (2, 5), w(i/j) = 3 =? f(1, 1)
;  (2, 3) / (1, 2) = (1, 3), w(i/j) = 2 =? f(1, 1)
;  so f is not a function

;;  * _ Exercise 2.10
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Divisor spans zero" (lower-bound y) (upper-bound y))
      (mul-interval x 
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

(div-interval (make-interval 1.0 2.0)
	      (make-interval -0.3 0.3))

(div-interval (make-interval 1.0 2.0)
	      (make-interval -0.7 -0.3))

;; * _ Exercise 2.11
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval-cases x y)
  (define (positive? i) (>= (lower-bound i) 0))
  (define (negative? i) (<= (upper-bound i) 0))
  (let ((lx (lower-bound x))
	(ux (upper-bound x))
	(ly (lower-bound y))
	(uy (upper-bound y))
	(positive-y? (positive? y))
	(negative-y? (negative? y)))
    (cond ((positive? x)
	   (cond (positive-y? (make-interval (* lx ly) (* ux uy)))
		 (negative-y? (make-interval (* ux ly) (* lx uy)))
		 (else (make-interval (* ux ly) (* ux uy)))))
	  ((negative? x)
	   (cond (positive-y? (make-interval (* lx uy) (* ux ly)))
		 (negative-y? (make-interval (* ux uy) (* lx ly)))
		 (else (make-interval (* lx uy) (* lx ly)))))
	  (else
	   (cond (positive-y? (make-interval (* lx uy) (* ux uy)))
		 (negative-y? (make-interval (* ux ly) (* lx ly)))
		 (else (make-interval (min (* lx uy) (* uy lx))
				      (max (* lx ly) (* ux uy))))) ))))


(mul-interval-cases (make-interval  3  4) (make-interval  5  9))
(mul-interval-cases (make-interval  3  4) (make-interval -5 -2))
(mul-interval-cases (make-interval  3  4) (make-interval -5  2))
(mul-interval-cases (make-interval -3  4) (make-interval  5  9))
(mul-interval-cases (make-interval -3  4) (make-interval -5  2))
(mul-interval-cases (make-interval -3  4) (make-interval -5 -2))
(mul-interval-cases (make-interval -6 -4) (make-interval  5  9))
(mul-interval-cases (make-interval -6 -4) (make-interval -5  2))
(mul-interval-cases (make-interval -6 -4) (make-interval -5 -2))

;;  * _ Exercise 2.12
(define (make-center-percent c p)
  (make-interval (- c (* (/ p 100) c)) (+ c (* (/ p 100) c))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (percent i)
  (* 100 (/ (- (upper-bound i) (lower-bound i)) (* 2 (center i)))))

(make-center-percent 20 5)
(percent (make-center-percent 20 5))

;;  * _ Exercise 2.13
; (x ± ax) * (y ± by) = x-ax * y-by ... x+ax * y+by
;       = xy - xyb - xya + abxy  ... xy + xyb + xya + abxy 
;       = xy ( 1 - b - a + ab ) .. xy ( 1 + b + a + ab)
;       (a & b small => ab near 0, can be dropped )
;       = xy ± xy(a + b)
; so approximate tolerance is sum of prior tolerances
 
;;  * _ Exercise 2.14
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(par1 (make-interval 10 12x) (make-interval 10 12)) ; 4.167, 7.2
(center (par1 (make-interval 10 12) (make-interval 10 12))) ; => 5.683
(percent (par1 (make-interval 10 12) (make-interval 10 12))) ; => 26

(par2 (make-interval 10 12) (make-interval 10 12))           ; => 5, 6
(center (par2 (make-interval 10 12) (make-interval 10 12)))  ; => 5.5
(percent (par2 (make-interval 10 12) (make-interval 10 12))) ; => 9.09

(center (par1 (make-interval 10 12) (make-interval 20 21))) ; => 7.23
(percent (par1 (make-interval 10 12) (make-interval 20 21))) ; => 16.1

(center (par2 (make-interval 10 12) (make-interval 20 21))) ; => 7.15
(percent (par2 (make-interval 10 12) (make-interval 20 21))) ; => 6.78

; further investigation

; par1
; (low1*low2 to high1*high2) / (low1+low2 to high1+high2) = 
; low1*low2/(high1+high2) to high1*high2/(low1+low2)

; par2
; 1/(1/high1 + 1/high2 to 1/low1 + 1/low2) =
; 1/((high1+high2)/high1*high2 to (low1+low2)/low1*low2) = 
; high1*high2*low1*low2 / low1*low2*(high1+high2) to high1*high2*(low1+low2)  =
; low1*low2/(low1+low2) to high1*high2/(high1 + high2)

;;  * _ Exercise 2.15
; Every time an interval is included in an algebraic equation, you're including the
;  uncertainty twice.  E.g. since R1 has an uncertain but inherently singular value,
;  R1/R1 should be equal to 1, but according to our procedure, we'll get some
;  different, increased range:
(let ((r1 (make-interval 2 3)))
  (div-interval r1 r1)) ; => .667, 1.5

; So under that reasoning, an answer which eliminates duplicates is more accurate

;;  * _ Exercise 2.16
; If we try to make equivalent algebraic expressions lead to the same answer,
;  we need arithmetic inverses and identities to compute as expected:
;  R1 - R1 = 0, R1/R1 = 1
;
; This can't be done as long as we store intervals as simply two numbers (low, hi)
;  because then R1 = (2, 3) is indistinguishable from R2 = (2, 3),
;  even though R1 - R1 = 0 != R1 - R2
; 
; There are a few ways around this.  One is to force the algebra to be processed by
;  by our package, and rely on caller to pass in distinct intervals one time only.
;  This way we can ensure that we treat each independent uncertainty as a single
;  uncertainty, and compute our variables such that R1-R1 = 0 and R1/R1 = 1

(define (interval-algebra f x y)
  (let ((p1 (f (lower-bound x) (lower-bound y)))
	(p2 (f (upper-bound x) (upper-bound y)))
	(p3 (f (lower-bound x) (upper-bound y)))
	(p4 (f (upper-bound x) (lower-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

; This only supports algebra of two inputs.
; Solving this for arbitrary number of variables requires lists, etc.

(interval-algebra (lambda (x y) (+ x y)) 
		  (make-interval 3 5)
		  (make-interval 1 2)) ; => 4 . 7

(interval-algebra (lambda (x y) (- x y)) 
		  (make-interval 2 3)
		  (make-interval 2 3)) ; => -1 . 1

(interval-algebra (lambda (x y) (- x x)) 
		  (make-interval 2 3)
		  (make-interval 2 3)) ; => 0 . 0

; par1
(interval-algebra (lambda (x y) (/ (* x y) (+ x y))) 
		  (make-interval 10 12)
		  (make-interval 20 21)) ; => 20/3 . 84/11

; par2
(interval-algebra (lambda (x y) (/ 1 (+ (/ 1 x) (/ 1 y)))) 
		  (make-interval 10 12)
		  (make-interval 20 21)) ; => 20/3 . 84/11


; But checking endpoints only handles linear algebra; also need to handle (5-x)^2.
(interval-algebra (lambda (x y) (square (- x y))) 
		  (make-interval 5 5)
		  (make-interval 4 6)) ; => 1 . 1  (wrong, should be 0 . 1)


; This could be done by computing the derivative and finding all zeros (difficult).
; Alternately, we can just compute every interval or so, as a rough approximation:
(define (interval-algebra-i f x y)
  (define (widen i val)
    (make-interval (min (lower-bound i) val)
		   (max (upper-bound i) val)))
  (define (next val)
    (+ val 0.01))
  (define (iter result val-x val-y)
    (if (> val-y (upper-bound y))
	result
	(if (> val-x (upper-bound x))
	    (iter result (lower-bound x) (next val-y))
	    (iter (widen result (f val-x val-y)) (next val-x) val-y))))
  (let ((low-x (lower-bound x))
	(low-y (lower-bound y)))
    (let ((first-val (f low-x low-y)))
      (iter (make-interval first-val first-val) low-x low-y))))

(interval-algebra-i (lambda (x y) (square (- x y))) 
		  (make-interval 5 5)
		  (make-interval 4 6)) ; => 0.000454 . 1 (pretty close to right)

;; * _ Section 2.2.1
;;  * _ Exercise 2.17
(define (last-pair items)
  (if (= (length items) 1)
      items
      (last-pair (cdr items))))

(last-pair (list 23 72 149 34))

;;  * _ Exercise 2.18
(define (reverse items)
  (if (= (length items) 1)
      items
      (append (reverse (cdr items)) (list (car items)))))

(reverse (list 1 4 9 16 25))

;;  * _ Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define (cc amount coin-values)
  (define (except-first-denomination coins)
    (cdr coins))
  (define (first-denomination coins)
    (car coins))
  (define (no-more? coins)
    (= (length coins) 0))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(cc 100 us-coins) ; => 292
(cc 100 (list 5 1 10 25 50)) ; => 292
(cc 16 (list 100 50 20 10 5 2 1 0.5))

; the algorithm doesn't depend on the order of the coins

;;  * _ Exercise 2.20
(define (same-parity first . rest)
  (define (match-parity? x)
    (= (remainder first 2) (remainder x 2)))
  (define (iter items)
    (if (= (length items) 0)
	(list)
	(let ((recurse (iter (cdr items))))
	  (if (match-parity? (car items))
	      (cons (car items) recurse)
	      recurse))))
  (cons first (iter rest)))

; inimino
(define (same-parity x . ys)
  (cons x (same-parity-list x ys)))

(define (same-parity-list x ys)
  (cond ((null? ys) ())
        ((eq? (odd? x) (odd? (car ys)))
          (cons (car ys) (same-parity-list x (cdr ys))))
        (else
          (same-parity-list x (cdr ys)))))

(define (same-parity x . ys)
  (display ys)
  (cond ((null? ys) ())
        ((= 1 1)
          (cons (car ys) (same-parity x (cdr ys))))
        (else
          (same-parity x (cdr ys)))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

; revised
(define (same-parity first . rest)
  (define (match-parity? x)
    (= (remainder first 2) (remainder x 2)))
  (define (filter-parity items)
    (let ((extra (filter-parity (cdr items))))
      (cond ((= (length items) 0) ())
	    ((match-parity? (car items)) (cons (car items) extra))
	    extra)))
  (cons first (filter-parity rest)))


;;  * _ Exercise 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4)) ; => 1 4 9 16

(define (square-list items)
  (map square items))

(square-list (list 1 2 3 4)) ; => 1 4 9 16

;;  * _ Exercise 2.22
; In the first version, the procedure is cons-ing the car of the list
;  onto the answer list repeatedly, reversing the order.

; In the second version, the cons is calling cons list new-item,
;  which is the opposite of the appropriate method, and starts with a nil.

(define (square-list-b items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items ()))

(square-list-b (list 1 2 3 4))

;;  * _ Exercise 2.23
(define (for-each f items)
  (cond ((null? items) true)
	(else
	 (f (car items))
	 (for-each f (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;; * _ Section 2.2.2
;;  * _ Exercise 2.24
(list 1 (list 2 (list 3 4))) ; => (1 (2 (3 4)))
;    (1 (2 (3 4)))
;       /    \ 
;     1       (2 (3 4))
;              /   \
;             2     (3 4)
;                   /  \
;                  3    4

;;  * _ Exercise 2.25
((lambda (x) 
   (car (cdr (car (cdr (cdr x))))))
 (list 1 3 (list 5 7) 9))

((lambda (x)
   (car (car x)))
 (list (list 7)))


((lambda (x)
   (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x)))))))))))))
 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;;  * _ Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; =>  (1 2 3 4 5 6)
(cons x y)   ; =>  ((1 2 3) 4 5 6)
(list x y)   ; =>  ((1 2 3) (4 5 6))

;;  * _ Exercise 2.27
(define x (list (list 1 2) (list 3 4)))
(reverse x)

(define (deep-reverse items)
  (let ((first (car items)))
    (if (= (length items) 1)
	(if (pair? first)
	  (list (deep-reverse first))
	  items)
	(append (deep-reverse (cdr items)) (list (if (pair? first)
						     (deep-reverse first)
						     first))))))
(deep-reverse x) ; => ((4 3) (2 1))

;;  * _ Exercise 2.28
(define x (list (list 1 2) (list 3 4)))

(define (fringe items)
  (if (pair? items)
      (append (fringe (car items)) (fringe (cdr items)))
      (if (null? items)
	  ()
	  (list items))))
(fringe x) ; => (1 2 3 4)
(fringe (list x x)) ; => (1 2 3 4 1 2 3 4)

;;  * _ Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a. selectors
(define (left-branch m) (car m))
(define (right-branch m) (car (cdr m)))

(left-branch (make-mobile 4 5))  ; => 4
(right-branch (make-mobile 4 5)) ; => 5

(define (branch-length b) (car b))
(define (branch-structure b) (car (cdr b)))

(branch-length (make-branch 4 5))    ; => 4
(branch-structure (make-branch 4 5)) ; => 5

; b. total-weight
(define (total-weight m)
  (define (branch-weight b) (total-weight (branch-structure b)))
  (if (pair? m)
      (+ (branch-weight (left-branch m)) (branch-weight (right-branch m)))
      m))

(define mobile-1 
  (make-mobile (make-branch 2 3)
	       (make-branch 1 (make-mobile (make-branch 2 4)
					   (make-branch 1 5)))))
(define mobile-2
  (make-mobile (make-branch 3 4)
	       (make-branch 1 (make-mobile (make-branch 2 9)
					   (make-branch 6 3)))))

(total-weight mobile-1) ; => 12
(total-weight mobile-2) ; => 16

; c. balance
(define (torque b)
  (* (branch-length b)
     (total-weight (branch-structure b))))

(define (balanced? m)
  (if (pair? m)
      (and (= (torque (left-branch m)) (torque (right-branch m)))
	   (balanced? (branch-structure (left-branch m)))
	   (balanced? (branch-structure (right-branch m))))
      true))

(balanced? mobile-1) ; => #f
(balanced? mobile-2) ; => #t 

; d. change underlying implementation
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

; rewrite couple of selectors & we're done
(define (right-branch m) (cdr m))
(define (branch-structure b) (cdr b))


(define mobile-1 
  (make-mobile (make-branch 2 3)
	       (make-branch 1 (make-mobile (make-branch 2 4)
					   (make-branch 1 5)))))
(define mobile-2
  (make-mobile (make-branch 3 4)
	       (make-branch 1 (make-mobile (make-branch 2 9)
					   (make-branch 6 3)))))

(balanced? mobile-1) ; => #f
(balanced? mobile-2) ; => #t 

;;  * _ Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7))) ; => (1 (4 (9 16) 25) (36 49))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7))) ; => (1 (4 (9 16) 25) (36 49))

;;  * _ Exercise 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(tree-map square
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7))) ; => (1 (4 (9 16) 25) (36 49))
  
;;  * _ Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3)) ; => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; This procedure takes the head off the list, then computes subsets
;  of the remaining set.  Then we take those subsets and add them to 
;  our result set, along with those same subsets combined with our
;  head element, i.e. results = append (subsets of remainder), 
;     (head + each of subsets of remainder) 

;; * _ Section 2.2.3
;;  * _ Exercise 2.33
(define nil ())
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y) ) nil sequence))

(map (lambda (x) (* x 10)) (list 1 2 3)) ; => (10 20 30)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5)) ; => (1 2 3 4 5)

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 5 6 7)) ; => 3

;;  * _ Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1)) ; => 79 = 1 + 6 + 40 + 32 (check)

;;  * _ Exercise 2.35
(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y)) 
	      0 
	      (map (lambda (x)
		     (if (pair? x)
			 (count-leaves x)
			 1))
		   t)))

(count-leaves (cons (list 1 2) (list 3 4))) ; => 4
(count-leaves (cons (cons (list 1 2) (list 3 4 5)) (cons (list 1 2) (list 3 4)))) ; => 9

;;  * _ Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))) ; => (22 26 30)

;;  * _ Exercise 2.37
(define matrix-a (list (list 1 2 3 4) (list 4 5 6 7) (list 6 7 8 9)))
(define vector-b (list 1 10 100 1000))

(define (matrix-*-vector m v)
  (map (lambda (m-i) (accumulate + 0 (accumulate-n * 1 (list m-i v))))  m))

(matrix-*-vector matrix-a vector-b) ; => (4321 7654 9876)

(define (transpose mat)
  (accumulate-n cons () mat))

(transpose matrix-a) ; => ((1 4 6) (2 5 7) (3 6 8) (4 7 9))

; (define (matrix-*-matrix m n)
;  (let ((cols (transpose n)))
;    (map <??> m)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map 
     (lambda (m-row) (matrix-*-vector cols m-row))
     m)))

(define matrix-c (list (list 1 2 3) (list 5 10 15) (list 20 30 40)))
(matrix-*-matrix matrix-c matrix-c) ; => ((71 112 153) (355 560 765) (970 1540 2110))

;;  * _ Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3)) ; => 3/2 = ( (3 / 2) / 1)
(fold-left / 1 (list 1 2 3))  ; => 1/6 = ( (1 / 2) / 3) 
(fold-right list nil (list 1 2 3)) ; => (1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ; => (((() 1) 2) 3)

; associative property -- (op (op a b) c) = (op a (op b c)) -- guarantees equality
;  commutative property is not sufficient, e.g.
(fold-left (lambda (a b) (abs (- a b))) 0 (list 10 4 1)) ; => 5
(fold-right (lambda (a b) (abs (- a b))) 0 (list 10 4 1)) ; => 7

;;  * _ Exercise 2.39
(define nil ())
(define (reverse-fr sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(reverse-fr (list 1 2 3))

(define (reverse-fl sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
(reverse-fl (list 1 2 3))

;;  * _ Exercise 2.40
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(define accumulate fold-right)
(accumulate append
            nil
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 10)))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(prime-sum-pairs 20)

; pull fast-prime? definition from ch1-outline
(define (prime? n) (fast-prime? n 100))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(unique-pairs 5) ; => ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 5) ; => ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7))

;;  * _ Exercise 2.41
(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
	    (map (lambda (k) (list i j k))
		 (enumerate-interval 1 (- j 1))))
	  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))
(unique-triples 4) ; => ((3 2 1) (4 2 1) (4 3 1) (4 3 2))

(define (triples-summing-to s n)
  (define (triple-sum x)
    (+ (car x) (cadr x) (caddr x)))
  (filter (lambda (x) (= (triple-sum x) s))
	  (unique-triples n)))

(triples-summing-to 9 7) ; => ((4 3 2) (5 3 1) (6 2 1))

;;  * _ Exercise 2.42
(define (queens board-size)
  (define empty-board ())
  (define (adjoin-position row col rest-of-queens)
    (cons (list col row) rest-of-queens))
  (define (safe? k positions)
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
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(define nil ())
(queens 3) ; => () correct!
(queens 4) ; => (((4 3) (3 1) (2 4) (1 2)) ((4 2) (3 4) (2 1) (1 3)))

;;  * _ Exercise 2.43
; Louis's code is, for each column, recomputing all queens for all earlier columns.
; This adds time recursively, so row 8 will take 8 times as long as computing row 7,
;  which takes 8 times as long as row 6, etc.
; So Louis's program should take about 8^7 * T
(define (time fn trials)
  (define (run n starttime)
    (fn)
    (if (> n 1)
	(run (- n 1) starttime)
	(- (runtime) starttime)))
  (run trials (runtime)))

(define (queens-lr board-size)
  (define empty-board ())
  (define (adjoin-position row col rest-of-queens)
    (cons (list col row) rest-of-queens))
  (define (safe? k positions)
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
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
	  (lambda (new-row)
	    (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
		 (queen-cols (- k 1))))
	(enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(queens-lr 4) ; => (((4 2) (3 4) (2 1) (1 3)) ((4 3) (3 1) (2 4) (1 2)))
(time (lambda () (queens 4)) 1000)   ; => 0.88 => 0.00088 each
(time (lambda () (queens-lr 4)) 100) ; => 0.61 => 0.0061 each => 6.9 as much, approx 3^2 ?

(time (lambda () (queens 5)) 100)   ; => 0.43 => 0.0043 each
(time (lambda () (queens-lr 5)) 10) ; => 0.84 => 0.084 each => 20 times as much, approx 

(time (lambda () (queens 6)) 100)   ; => 2.12 => 0.0212 each
(time (lambda () (queens-lr 6)) 10) ; => 13.6 => 1.36 each => 643 as much, approx 5^4, 

(time (lambda () (queens 7)) 100)   ; => 9.97 => 0.1 each
(time (lambda () (queens-lr 7)) 1)  ; => 26.3 => 26.3 each => 263 as much

; This doesn't much agree with the prediction!

;; * _ Section 2.2.4
;;  * _ Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

;;  * _ Exercise 2.45
(define (split big-op small-op)
  (define (iter painter n)
    (if (= n 0)
	painter
	(let ((smaller (iter painter (- n 1))))
	  (big-op painter (small-op smaller smaller)))))
  iter)

;;  * _ Exercise 2.46
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)
(define (op-vect op)
  (lambda (v1 v2) 
    (make-vect (op (xcor-vect v1) (xcor-vect v2))
	       (op (ycor-vect v1) (ycor-vect v1)))))
(define add-vect (op-vect +))
(define sub-vect (op-vect -))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

;;  * _ Exercise 2.47
; Definition of origin-frame, edge1-frame, edge2-frame
; Frame definition 1
; (define (make-frame origin edge1 edge2)
;   (list origin edge1 edge2))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

; Frame definition 2
; (define (make-frame origin edge1 edge2)
;   (cons origin (cons edge1 edge2)))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame cddr)

;;  * _ Exercise 2.48
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

;;  * _ Exercise 2.49
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
; a.
(define (quick-segment v1-x v1-y v2-x v2-y) 
  (make-segment (make-vect v1-x v1-y) (make-vect v2-x v2-y)))
(define outline-frame
  (segments->painter (list (quick-segment  0 0  0 1)
			   (quick-segment  0 1  1 1)
			   (quick-segment  1 1  1 0)
			   (quick-segment  1 0  0 0))))
; b.
(define x-frame
  (segments->painter (list (quick-segment  0 0  1 1)
			   (quick-segment  0 1  1 0))))

; c.
(define diamond-frame
  (segments->painter (list (quick-segment  0.5 0  1 0.5)
			   (quick-segment  1 0.5  0.5 1)
			   (quick-segment  0.5 1  0 0.5)
			   (quick-segment  0 0.5  0.5 0))))

; d.
(define wave
  (segments->painter (list (quick-segment  0.3  0     0.35 0.5 )
			   (quick-segment  0.35 0.5   0.33 0.55)
			   (quick-segment  0.33 0.55  0.2  0.45)
			   (quick-segment  0.2  0.45  0    0.6)

			   (quick-segment  0    0.8   0.2  0.55)
			   (quick-segment  0.2  0.55  0.33 0.6)
			   (quick-segment  0.33 0.6   0.4  0.6)
			   (quick-segment  0.4  0.6   0.35 0.8)
			   (quick-segment  0.35 0.8   0.4  1.0)

			   (quick-segment  0.6  1.0   0.65 0.8)
			   (quick-segment  0.65 0.8   0.6  0.6)
			   (quick-segment  0.6  0.6   0.7  0.6)
			   (quick-segment  0.7  0.6   1.0  0.3)

			   (quick-segment  1.0  0.15   0.6 0.45)
			   (quick-segment  0.6  0.45   0.7 0)

			   (quick-segment  0.4  0      0.5 0.3)
			   (quick-segment  0.5  0.3    0.6 0))))

;;  * _ Exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;;  * _ Exercise 2.51
; write below analogously to beside
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
			      split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

; write below in terms of rotates and beside)
(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate 270 painter2))))

;;  * _ Exercise 2.52
; a. adding a smile by adding a couple segments:
;	 (quick-segment  0.45 0.75   0.5  0.7)
;        (quick-segment  0.5  0.7    0.55 0.75)

; b. change corner-split
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner))))))

; c. revise square-limit to make rogers look out in each direction
(define (square-limit painter n)
  (let ((combine4 (square-of-four rotate270 rotate180
                                  identity  rotate90)))
    (combine4 (corner-split painter n))))

;; * _ Section 2.3.1
;;  * _ Exercise 2.53
(list 'a 'b 'c) ; => (a b c)

(list (list 'george))     ; => ((george))
(cdr '((x1 x2) (y1 y2)))  ; => ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; => (y1 y2)

(pair? (car '(a short list)))  ; => false
(memq 'red '((red shoes) (blue socks))) ; => false

(memq 'red '(red shoes blue socks)) ; => (red shoes blue socks)

;;  * _ Exercise 2.54
(define (equal? list1 list2)
  (cond ((not (= (length list1) (length list2))) false)
	((= 0 (length list1) (length list2)) true)
	(else (and (eq? (car list1) (car list2))
		   (equal? (cdr list1) (cdr list2))))))

(equal? '(this is a list) '(this is a list))   ; => #t
(equal? '(this is a list) '(this (is a) list)) ; => #f
(equal? '(this is a list) '(this is an apple)) ; => #f

;;  * _ Exercise 2.55
; ''text does the same as (cons 'quote 'text)
; the quote operator quotes first the next quote then the text itself

;; * _ Section 2.3.2
;;  * _ Exercise 2.56

; code from text....
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(+ x 3) 'x) ; => 1

; add exponentiation...
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation base exponent)
  (cond ((=number? base 0) 0)
	((=number? base 1) 1)
	((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
	  (make-product
	   (exponent exp)
           (make-exponentiation (base exp)
			  (- (exponent exp) 1)))
	   (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(** x 2) 'x)       ; => (* 2 x) correct!
(deriv '(** (+ x 2) 3) 'x) ; => (* 3 (** (+ x 2) 2)) correct!

;;  * _ Exercise 2.57

; this make-sum list doesn't handle fully reducing in the case of
; 5 + 'x + 7 -- could improve by sorting all numbers to front
(define (make-sum . summands)
  (make-sum-list summands))
(define (make-sum-list summands)
  (if (null? (cdr summands))
      (car summands)
      (let ((head (car summands))
	    (next (cadr summands))
	    (rest (cddr summands)))
	(define (identity x) x)
	(define (include-rest item combiner)
	  (if (null? rest)
	      item
	      (combiner (cons item rest))))
	(cond ((null? next) head)
	      ((=number? head 0) (include-rest next make-sum-list))
	      ((and (number? head) (number? next)) (include-rest (+ head next) make-sum-list))
	      (else (list '+ head (include-rest next identity)))))))


(define (make-sum . summands)
  (make-sum-list summands))
(define (make-sum-list summands)
  (let ((numbers-added 
	 (fold-right (lambda (x y)
		       (if (number? x)
			   (cons (+ x (car y)) (cdr y))
			   (cons (car y) (cons x (cdr y)))))
		     (list 0) summands)))
    (cond ((null? (cdr numbers-added)) (car numbers-added))
	  ((= (car numbers-added) 0) (cons '+ (cdr numbers-added)))
	  (else (cons '+ numbers-added)))))

(define (augend s) (make-sum-list (cddr s)))

(make-sum 3 4 5 'x) ; => (+ 12 x) correct!
(make-sum 'x 3 4 5) ; => (+ 12 x) correct!
(make-sum 3)        ; => 3 correct!

(make-sum 6 'y '(** x 2) 4 5)   ; => (+ 15 y (** x 2))

(make-sum 3 4 5 'x 'y)          ; => (+ 12 x y) correct!
(addend (make-sum 3 4 5 'x 'y)) ; => 12 correct!
(augend (make-sum 3 4 5 'x 'y)) ; => (+ x y) correct

(make-sum '(** x 2) '(* 3 x) '(* 4 x))   ; =>  (+ (** x 2) ((* 3 x) (* 4 x))) correct!
(deriv '(+ (** x 2) (* 3 x) (* 4 x)) 'x) ; => (+ 7 (* 2 x)) correct!

; b. product
(define (make-product . multiplicands)
  (make-product-list multiplicands))
(define (make-product-list multiplicands)
  (let ((numbers-multiplied 
	 (fold-right (lambda (x y)
		       (if (number? x)
			   (cons (* x (car y)) (cdr y))
			   (cons (car y) (cons x (cdr y)))))
		     (list 1) multiplicands)))
    (cond ((null? (cdr numbers-multiplied)) (car numbers-multiplied))
	  ((= (car numbers-multiplied) 0) 0)
	  ((= (car numbers-multiplied) 1) (if (null? (cddr numbers-multiplied))
					      (cadr numbers-multiplied)
					      (cons '* (cdr numbers-multiplied))))
	  (else (cons '* numbers-multiplied)))))

(define (multiplicand p) (make-product (caddr p))

(make-product 5 4 'x) ; => (* 20 x)
(make-product 12 5 4 'x 'y) ; => (* 240 x y)
(make-product 12 5 4 'x '(+ x 5)) ; => (* 240 x y)
(multiplicand (make-product 12 5 4 'x 'y)) ; => (* x y) 
(make-product 4 'x) ; => (* 4 x) correct!
(multiplicand (make-product 4 'x)) ; => x correct!

(deriv '(* 4 x (+ x 5)) 'x) ; => 4 (x + 5) + (4 x ) correct!

;;  * _ Exercise 2.58
; a. make operators infix
; in order to do this, we just have to change the sum? and product? etc. to rearrange how they expect data to be stored
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(deriv '(x + (3 * (x + 2))) 'x) ; => 4 correct!

; b. do it without parentheses!
(product? '(x + 4 * x))

; find the weakest binding operations, and flag those first
(define (sum? x)
  (and (pair? x) (memq '+ x))

(define (product? x)
  (and (pair? x) (not (sum? x)) (eq? (cadr x) '*)))

; ... not done ....

;; * _ Section 2.3.3
;;  * _ Exercise 2.59
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((element-of-set? (car set1) set2) 
	 (union-set (cdr set1) set2))
	(else (cons (car set1) (union-set (cdr set1) set2)))))

(union-set (list 1 2 3 ) (list 2 4 6)) ; => (1 3 2 4 6)

;;  * _ Exercise 2.60
; element-of-set? is unchanged
(define adjoin-set cons)  ; this version is O(1) compared to O(n) above
(define union-set append) ; this version is O(n) compared of O(n^2) above
; intersection-set is unchanged
; this version might be better if we do a lot of unions and have plenty of space to spare

;;  * _ Exercise 2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 5 (list 1 2 8 9)) ; => (1 2 5 8 9)

;;  * _ Exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1)) (x2 (car set2)))
	   (cond ((= x1 x2)
		  (cons x1 (union-set (cdr set1) (cdr set2))))
		 ((< x1 x2)
		  (cons x1 (union-set (cdr set1) set2)))
		 ((< x2 x1)
		  (cons x2 (union-set set1 (cdr set2)))))))))

(union-set (list 1 3 5 7) (list 2 4 6)) ; => (1 2 3 4 5 6 7)

;;  * _ Exercise 2.63
; a.
; tree->list-1 will create a list leftmost to rightmost
; tree->list-2 does the same
; b.
; The order of growth in number of steps is the same in both cases
; Note: iminimo points out that append in the first one is an O(n) operation => O(n^2) for tree->list-1

;;  * _ Exercise 2.64
; a.
; The procedure divides the list in half repeatedly, taking the element after the split point
;  as the root of the tree, and then processing each half recursively
;        5
;      /   \
;    1      9
;     \    / \ 
;      3  7   11
;
; b.
; Since each step divides the list in half, essentially, we grow as O(log n)
; CORRECTION: since we have to process every element, we actually grow as O(n)

;;  * _ Exercise 2.65
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; since these conversion functions are <= O(n), we can just convert to sorted lists, use previous O(n) solution, convert-back
(define (union-set-tree set1 set2)
  (list->tree (union-set (tree->list-1 set1) (tree->list-1 set2))))

(union-set-tree (list->tree (list 1 2 4 5 7 8)) (list->tree (list 1 3 6 9))) ; correct

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (intersection-set-tree set1 set2)
  (list->tree (intersection-set (tree->list-1 set1) (tree->list-1 set2))))

(intersection-set-tree (list->tree (list 1 2 4 5 6 7 8)) (list->tree (list 1 3 6 9))) ; correct


;;  * _ Exercise 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records))) (entry set-of-records))
	((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))
	(else (lookup given-key (right-branch set-of-records)))))

;; * _ Section 2.3.4
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

;;  * _ Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ; => (a d a b b c a)

;;  * _ Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((and (leaf? tree) (eq? symbol (symbol-leaf tree))) ())
	((memq symbol (symbols (left-branch tree)))
	 (cons 0 (encode-symbol symbol (left-branch tree))))
	((memq symbol (symbols (right-branch tree)))
	 (cons 1 (encode-symbol symbol (right-branch tree))))
	(else (error "symbol not found in tree"))))

(encode-symbol 'a sample-tree)
(encode-symbol 'c sample-tree)
(encode '(a d a b b c a) sample-tree) ; =>  (0 1 1 0 0 1 0 1 0 1 1 1 0)

;;  * _ Exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (equal? (length leaf-set) 1)
      (car leaf-set)
      (successive-merge 
       (adjoin-set 
	(make-code-tree (car leaf-set) (cadr leaf-set))
	(cddr leaf-set)))))

(generate-huffman-tree (list '(a 4) '(b 2) '(c 1) '(d 1)))
	      ; =>  ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)
sample-tree   ; =>  ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)

;;  * _ Exercise 2.70
(length (let ((code-tree (generate-huffman-tree 
		  '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))))
  (encode '(
	    Get a job 
	    Sha na na na na na na na na 
	    Get a job 
	    Sha na na na na na na na na
	    Wah yip yip yip yip yip yip yip yip yip
	    Sha boom) code-tree)))
; length = 84 bits
; In a fixed-length code, 8 symbols => 3 bits per symbol
; The list has 36 symbols => 36 x 3 = 108 bits

;;  * _ Exercise 2.71
; n = 5
;  {a, b, c, d, e} 31 
;    /          \
;  e 16       {a, b, c, d} 15
;               /     \
;             d 8    {a, b, c} 7
;                    /       \
;                  c 4      {a, b} 3
;                            /  \
;                          b 2   a 1

; n = 10, somewhat simplified
;            *
;          /   \ 
;       j 512    *
;              /   \
;          i 256     *
;                  /   \
;              h 128     *
;                      /   \
;                    g 64   *
;                         /   \
;                       f 32    *
;                             /   \
;                           e 16    *
;                                 /   \ 
;                               d 8     *
;                                     /   \
;                                   c 4     *
;                                         /   \
;                                       b 2   a 1
;
; In general it takes 1 bit to code the most frequent symbol
;  and n-1 bits to code the least freqent symbol


;;  * _ Exercise 2.72
; Considering the special case only.
; The most frequent symbol requires only one comparison => O(1)
; The least frequent symbol requires searching n chars, then n-1, then n-2, etc. down to 1
; => (n + (n-1))/2 comparisons, i.e. O(n^2)


;; * _ Section 2.4.1
;; * _ Section 2.4.2
;; * _ Section 2.4.3
;;  * _ Exercise 2.73
; a. The principle is to dispatch on those expressions where we can
;    specify the type of the function.  Numbers and raw symbols could be
;    explicitly tagged with a type, but then we'd have to 'construct' them
;    explicitly, but our goal is to simply pass in an expression.
; b. 
; background code
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; simplified 2-item versions of deriv functions
(define (deriv-add e var)
  (make-sum (deriv (car e) var) (deriv (cadr e) var)))
(define (deriv-product e var)
  (make-sum
   (make-product (car e) (deriv (cadr e) var))
   (make-product (deriv (car e) var) (cadr e))))
(put 'deriv '(+) deriv-add)
(put 'deriv '(*) deriv-product)

; untested -- get/put apparently not implemented

; c.
(define (make-exponentiation base exponent)
  (cond ((=number? base 0) 0)
	((=number? base 1) 1)
	((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (deriv-exp e var)
  (make-product 
   (- (cadr e) 1)
   (car e)
   (deriv (car e) var)))
(put 'deriv '(**) deriv-exp)

; d. If instead of getting 'deriv '+, we get '+ 'deriv, we have technically made
;    the 'type' of all expressions 'deriv, but the operation is '+; there's no real
;    difference.  Everything will work fine if we similarly reverse the order of
;    the arguments to put in the same manner.

;;  * _ Exercise 2.74
; a. Implement for headquarters a get-record procedure that retrieves a specified employee's record from a specified personnel file. The procedure should be applicable to any division's file. 

(define (get-record division employee-name)
  ((get 'get-record division) employee-name))

; Call in this way: (get-record 'accounting 'john_smith)
; Since divisions map to files 1-to-1, it's convenient to refer to files by division.

; a. (cont.) Explain how the individual divisions' files should be structured. In particular, what type information must be supplied?
; Presumably, we do not want to rewrite all data structures to match, but rather provide a common
; interface that works regardless of the data structure.  This interface should install
; procedures into a table as following:
; (put 'get-record 'accounting our-get-record)
; where our-get-record should be wrapped up into a package
; Also, the division must be embedded into the record itself -- see below

; b.  Implement for headquarters a get-salary procedure that returns the salary information from a given employee's record from any division's personnel file. How should the record be structured in order to make this operation work?
(define (get-salary record)
  ((get 'get-salary (get-division record)) record))
; Note that each record must include its division in a common way, 
;  e.g. (car record) returns 'accounting

; c.  Implement for headquarters a find-employee-record procedure. This should search all the divisions' files for the record of a given employee and return the record. Assume that this procedure takes as arguments an employee's name and a list of all the divisions' files.
(define (find-employee-record employee-name divisions)
  (if (null? divisions)
      (error "Employee not found in any division.")
      (let ((record (get-record (car divisions) employee-name)))
	(if (nil? record)
	    (find-employee-record employee-name (cdr divisions))
	    record))))

; d.  When Insatiable takes over a new company, what changes must be made in order to incorporate the new personnel information into the central system?
; Nothing needs to be modified with the old divisions' data, but the new one must implement
;  get-record, get-salary, etc. -- all elements of the common interface

;;  * _ Exercise 2.75
(define (make-from-mag-ang r a)
  (lambda (op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op)))))

;;  * _ Exercise 2.76
; Describe what changes must be made when adding a new type or operation in 3 cases:
; Explicit dispatch -- must change all operations to be aware of the new types
;  for each new operation, we write a new operation that must be aware of all types
; Data-directed style -- each new type must provide an interface for each operation
;  for each new operation, each type must implement its own version of that operation
; Message-passing style -- each new type must know how to understand all operations
;  for each new operation, each type must implement a way of handling the new message

; If new types are added frequently, data-directed and message-passing both work well,
;  since each new type must match an existing interface & one can leave most code untouched

; If operations are added frequently and new types are added very infrequently, then it might
;  be appropriate to use explicit-dispatch, since we can write one procedure for each new operation
;  and we don't need to peek into multiple packages.

;; * _ Section 2.5.1
;;  * _ Exercise 2.77
; In this example, the intepreter doesn't know how to handle the basic operations
; on a complex number -- in particular, because we never registered those operations
; directly with the 'complex' type.  However, if we 'put' them as pass-throughs, apply-generic
; will get called once on 'complex', dispatching to '(rectangular) magnitude, then apply-generic
; gets called again, mapping to ' then on 'rectangular' to magnitude-rectangular

;;  * _ Exercise 2.78
; Modify the definitions of type-tag, contents, and attach-tag from section 2.4.2 
;  so that our generic system takes advantage of Scheme's internal type system. 
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
	((number? datum) 'scheme-number)
	(else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
	((number? datum) datum)
	(else (error "Bad tagged datum -- CONTENTS" datum))))

(attach-tag 'scheme-number 5) ; => 5
(type-tag 5) ; => scheme-number
(contents 5) ; => 5

;;  * _ Exercise 2.79
; Define a generic equality predicate equ? that tests the equality of two numbers, 
;  and install it in the generic arithmetic package. This operation should work for 
;  ordinary numbers, rational numbers, and complex numbers.

; from book
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

; answer
(define (scheme-number-extensions)
  (define (equ? x y) (= x y))
  (put 'equ? '(scheme-number scheme-number) equ?)
  'done)

; subset of code from book, with additions
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (tag x) (attach-tag 'rational x))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  ; added
  ; note: always reduced
  (define (equ? x y)
    (and (= (numer x) (numer y))
	 (= (denom x) (denom y))))
  (put 'equ? '(rational rational) equ?)

  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (tag z) (attach-tag 'complex z))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ; added these
  (define (equ? x y)
    (and
     (= (real-part x) (real-part y))
     (= (imag-part x) (imag-part y))))
  (put 'equ? '(complex complex) equ?)

  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))


;;  * _ Exercise 2.80
(define (scheme-number-extensions)
  (define (zero? x) (= x 0))
  (put 'zero? '(scheme-number) zero?)
  'done)

; subset of code from book, with additions
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (tag x) (attach-tag 'rational x))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  ; added
  (define (zero? x) (= (numer x) 0))
  (put 'zero? '(rational) zero?)

  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (tag z) (attach-tag 'complex z))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ; added
  (define (zero? x)
    (and
     (= (real-part x) 0)
     (= (imag-part x) 0)))
  (put 'zero? '(complex) zero?)

  'done)

;; * _ Section 2.5.2
;;  * _ Exercise 2.81
; a. Before Louis's coercion procedures are installed, if you try to call "exp" on a type
;    that des not have it installed, you will correctly get an error "No method for these types".
;    After, apply-generic will endlessly recurse, at each level coercing complex->complex
; b. apply-generic works as is, assuming packages define ops with two variables of the same type.
;    As long as that is done, the proc should be found in the first "if" if it is defined, and
;    if not, the lack of self-coercions will lead to correct error message.
; c. 
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
		(if (eq? type1 type2)
		    (error "No method for this type" (list op type1))
		    (let ((t1->t2 (get-coercion type1 type2))
			  (t2->t1 (get-coercion type2 type1)))
		      (cond (t1->t2
			     (apply-generic op (t1->t2 a1) a2))
			    (t2->t1
			     (apply-generic op a1 (t2->t1 a2)))
			    (else
			     (error "No method for these types"
				    (list op type-tags)))))))
		(error "No method for these types"
		       (list op type-tags)))))))


;;  * _ Exercise 2.82
; implementing simple strategy of mapping all to first arg's type
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
	  (let ((type1 (car type-tags))
		(a1 (car args)))
	    (if (and 
		 (map (lambda (type-y) 
			(or (= type1 type-y) (get-coercion type1 type-y))) 
		      (cdr type-tags)))
		(apply apply-generic 
		       (append (list op a1)
			       (map (lambda (y) 
				      (let ((type-y (type-tag y)))
					(if (= type1 type-y)
					    y
					    ((get-coercion type1 type-y) y))))
				    (cdr args))))
		(error "No method for these types" (list op type-tags))))))))
; This will fail in cases where we would need to coerce to the 'highest' level,
;  e.g. take (sub rational complex) would try to coerce the complex to a rational, which
;  will fail.  But if we instead had a more complete solution, we would coerce rational->complex
;  and get the correct answer.  Note that the 2-adic version of apply-generic succeeds in that case.

;;  * _ Exercise 2.83
; add to integer package
(define (integer->rational x) (make-rational (contents x) 1))
(put 'raise '(integer) 'integer->rational)

(define (rational->real x) (* 1.0 (/ (numer x) (denom x))))
(put 'raise '(rational) 'rational->real)

(define (real->complex x) (make-complex-from-real-imag (contents x) 0))
(put 'raise '(real) 'real->complex)

;;  * _ Exercise 2.84

; rather than identifying which type is higher in the abstract,
;  we just try to raise the first value, then the second
(define (raise-until-same-type x y)
  (define (raise-first-until-same-type a b)
    (let ((raise-a (get 'raise (type-tag a))))
      (cond ((= (type-tag a) (type-tag b)) a)
	    (raise-a (raise-first-until-same-type (raise-a a) b))
	    (else false))))
  (let ((raised-x (raise-first-until-same-type x y))
	(raised-y (raise-first-until-same-type y x)))
    (cond (raised-x (list raised-x y))
	  (raised-y (list x raised-y))
	  (else 
	   (error "could not raise to match types" (list x y))))))

; 2-adic version only
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
	      (apply apply-generic (cons op (raise-until-same-type (car args) (cadr args))))
              (error "No method for these types"
                     (list op type-tags)))))))

;;  * _ Exercise 2.85
(define (rational->integer x) (round (/ (numer x) (denom x))))
(put 'project '(rational) 'rational->integer)

; not clear how to project effectively here
(define (real->rational x) (make-rational (round (* x 8400)) 8400))
(put 'project '(real) 'real->rational)

(define (complex->real x) (real-part x))
(put 'project '(complex) 'complex->real)

(define (drop x)
  (if (get 'project (type-tag x))
      (let ((dropped-x (apply-generic 'project x)))
	(if (= x (apply-generic 'raise dropped-x))
	    (drop dropped-x)
	    x))
      x))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
	      (apply apply-generic (cons op (raise-until-same-type (car args) (cadr args))))
              (error "No method for these types"
                     (list op type-tags)))))))

;;  * _ Exercise 2.86
; skipped

;; * _ Section 2.5.3
;;  * _ Exercise 2.87

;;  * _ Exercise 2.88
;;  * _ Exercise 2.89
;;  * _ Exercise 2.90
;;  * _ Exercise 2.91
;;  * _ Exercise 2.92
;;  * _ Exercise 2.93
;;  * _ Exercise 2.94
;;  * _ Exercise 2.95
;;  * _ Exercise 2.96
;;  * _ Exercise 2.97
