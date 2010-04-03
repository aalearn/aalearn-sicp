
;; -*- mode:Scheme; outline-regexp: ";; *\\*" -*-
;; (outline-minor-mode 1)


;;* Noodling
;; * Emacs to do
;- M-x run-scheme
;  - how would this run automatically?
;- Apple-2 to split window
;- C-x o to toggle "frame"
;- C-c C-o to run outline section
;- C-x C-e send last sexpr to Scheme
;- C-c @ C-s: show outline
;- C-c @ C-a: show all
;- C-c @ C-d: disappear

;; * x Noodling 1
(+ 5 432 (* 9 7))
(+ 
 (* 8 2)
 (* 3 7)
 (+ 7 8))
(define size 2)
(define (top-two x y z)
  (cond ((> x y) 
	   '(x (cond ((> y z) y (else z)))))
	(else '(list y (cond ((> x z) x (else z)))))))

;;* Chapter 1
;; * x Section 1.1.6
;;  * x Exercise 1.1
'(10 12 8 3 6 a b 19 #f 4 16 6 16)
;;  * x Exercise 1.2
(/ 
 (+ 5 4 
    (- 2 
       (- 3 
	  (- 6 
	     (/ 4 5)))))
 (* 3 
    (- 6 2)
    (- 2 7))
)
;;  * x Exercise 1.4
; if b is positive then it's added, otherwise a negative number is subtracted.
;;  * x Exercise 1.5
; Reversed these initially!
; We get an infinite loop under applicative order, because we try to evaluate p before we evaluate the "if"
; Under normal order, we evaluate the outer piece first, and then inner pieces as needed.   So under normal-order, we get a 0.
;; * x Section 1.1.7
;;  * x Exercise 1.6
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
; Maximum recursion depth exceeded, because the else clause is being evaluated before being passed into new-if.
;;  * _ Exercise 1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x) (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x) (sqrt-iter 1.0 x))
;  Small numbers: precision will be the same order of magnitude as the values we are concerned about, so that the error will be high compared to the value itself.  Therefore we're less likely to get a number we're concerned about. E.g.
(sqrt 0.0004)
;  Value: .0354 is 77% off of the correct answer
;  Large numbers: the precision may lead us to iterating many times to get the right answer, more times than we need; also if the numbers are big, we may never get the right answer, as the error due to limited precision may be permanently higher than our fixed error threshold.
(define (good-enough-using-ratio? guess previous-guess)
  (< (abs (/ (- previous-guess guess) guess)) 0.0001))
(define (sqrt-iter-using-ratio guess previous-guess x)
  (if (good-enough-using-ratio? guess previous-guess)
      guess
      (sqrt-iter-using-ratio (improve guess x) guess
                 x)))
(define (sqrt-using-ratio x) (sqrt-iter-using-ratio 1.0 0.5 x))
;  This works better for small numbers and very large numbers, but it is less accurate for medium-large ones.  The error in the square can still be quite high.

;;  * _ Exercise 1.8
(define (cbrt-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cbrt-iter (cube-improve guess x)
		 x)))
(define (cube-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))
(define (cube x) (* x x x))

(define (cube-improve guess x) 
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (cbrt x) (cbrt-iter 1.0 x))
; Upon discussion moved the "utility" functions to higher up,
;  so subsequent functions build on top of what went before
; Downside -- the "public" interface is at the bottom of the file.
; Solution, lexical-scoping -- "most public" interface at top,
;  sub-functions with-in. 
;; * x Section 1.2.1
;;  * x Exercise 1.9
; --first method--
; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
; recursive
; O(n) time and space

; --second method--
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
; iterative
; O(1) space, O(n) time

;;  * _ Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(A 1 10) ; 1024 = 2^10
(A 2 4)  ; 65536 = 2^16 = 2^(2^4)
(A 3 3)  ; 65536 = 2^16 = 2
; f(n) = 2n
; g(n) = (A 1 n) = (A 0 (A 1 n-1)) = 2 * g(n-1)
;  since g(1) = 2, g(n) = 2^n
; h(n) = (A 2 n) = (A 1 (A 2 n-1)) = 2^(h(n-1))
;  since h(1) = (A 2 1) = 2, h(1) = 2^1
;  h(2) = 2^2^1
;  h(3) = 2^2^2^1 = 2^4 = 16
;  h(4) = 2^16 = ×
;  => h(n) = 2↑↑n
;; * x Section 1.2.2
;;  * x Exercise 1.11
; recursive
(define (f-1-11 n)
  (cond ((< n 3) n)
	(else (+
	       (f-1-11 (- n 1))
	       (* 2 (f-1-11 (- n 2)))
	       (* 3 (f-1-11 (- n 3)))))))

(f-1-11 5)

; iterative
(define (f-1-11-b n)
  (define (f-1-11-b-iter x y z index)
    (if (= n index) 
	z
	(f-1-11-b-iter y z 
		       (+ z (* 2 y) (* 3 x))
		       (+ index 1))))
  (if (< n 3)
      n
      (f-1-11-b-iter 0 1 2 2)))

(f-1-11-b 5) ; => 25

(f-1-11 12)   ; => 10661
(f-1-11-b 12) ; => 10661

; (f-1-11 25)   ; => 10661
; (f-1-11-b 25) ; => 10661

;;  * x Exercise 1.12
; Pascal's triangle
(define (pascal row elem)
  (cond ((= elem 1) 1)
	((= elem row) 1)
	(else (+ 
	       (pascal (- row 1) (- elem 1))
	       (pascal (- row 1) elem)))))

(pascal 1 1)
(pascal 3 2)
(pascal 4 2)
(pascal 5 3)
;;  * _ Exercise 1.13
; ϕ = (1 + √5)/2
; ψ  = (1 - √5)/2
; fib(n) = (ϕ^n - ψ^n)/√5
;  iff 1 through 3 below
;   1) fib(1) = 1 = (ϕ - ψ)/√5 = (2√5/2)/√5 = 1  -- check
;   2) fib(2) = 1 = (ϕ^2 - ψ^2)/√5 = (1 + 2√5 + 5 - 1 + 2√5 - 5)/(4 * √5)
;             = (4√5/4√5) = 1 -- check
;   3) (induction) fib(n) = fib(n-1) + fib(n-2) 
;      (ϕ^n - ψ^n)/√5 = (ϕ^(n-1) - ψ^(n-1))/√5 + (ϕ^(n-2) - ψ^(n-2))/√5
;       ϕ^n - ψ^n = ϕ^(n-1) - ψ^(n-1) + ϕ^(n-2) - ψ^(n-2)
;       ((1 + √5)^n - (1 - √5)^n)/2^n = 
;             ((1 + √5)^(n-1) - (1 - √5)^(n-1))/2^(n-1) 
;             + ((1 + √5)^(n-2) - (1 - √5)^(n-2))/2^(n-2)
;       (1 + √5)^n - (1 - √5)^n = 
;             2 * (1 + √5)^(n-1) - 2 * (1 - √5)^(n-1) 
;             + 4 * (1 + √5)^(n-2) - 4 * (1 - √5)^(n-2)
;       (1 + √5)^n - 2 * (1 + √5)^(n-1) - 4 * (1 + √5)^(n-2) = 
;            (1 - √5)^n - 2 * (1 - √5)^(n-1) - 4 * (1 - √5)^(n-2) 
;       (1 + √5)^(n-2) * ((1 + √5)^2 - 2 * (1 + √5) - 4) = 
;            (1 - √5)^(n-2) * ((1 - √5)^2 - 2 * (1 - √5) - 4) 
;       (1 + √5)^(n-2) * (1 + 2√5 + 5 - 2 - 2√5 - 4) = 
;            (1 - √5)^(n-2) * (1 - 2√5 + 5 - 2 + 2√5 - 4)
;       0 = 0 -- check
; Finally, need to prove that (ϕ^n - ψ^n)/√5 is the closest integer to ϕ^n/√5
;  From above, we know that (ϕ^n - ψ^n)/√5 is always an integer.
;  The "error", ϕ^n/√5 - (ϕ^n - ψ^n)/√5 = ψ^n/√5
;  For n=1, abs(ψ^n/√5) = (1 - √5)/2√5 = 
(abs (/ (- 1 (sqrt 5)) (* 2 (sqrt 5)))) ; => .27639
;  Since ψ = 
(/ (- 1 (sqrt 5)) 2) ; => -.618
;  i.e. that abs(ψ) < 1, we know that abs(ψ^n/√5) will decrease as n increases
;  => abs(ψ^n/√5) < 0.5 for all n
;  => ϕ^n/√5 will be within 0.5 of (ϕ^n - ψ^n)/√5 for all n
;  => the closest integer to ϕ^n/√5 is always Fib(n)

;; * x Section 1.2.3
;;  * x Exercise 1.14
; cc 11 5
;  -> cc 11 4
;      -> cc 11 3    
;          -> cc 11 2
;              -> cc 11 1
;                  -> cc 11 0
;                  -> cc 10 1
;                      -> cc 10 0
;                      -> cc 9 1
;                      ...
;              -> cc 6 2
;                  -> cc 1 2
;                      -> cc 1 1
;                          -> cc 1 0
;                      -> cc -4 2
;          -> cc 1  3
;              -> cc 1 2
;                  -> cc 1 1
;                      -> cc 1 0
;                  -> cc -4 2
;              -> cc -9 3
;      -> cc -14 4
;  -> cc -39 5
; Space is linear: O(n)
; Time: if we double(n), we more than double work,
;  Something on the order of: if we had x branches before,
;  We're increasing the number of branches by x for each, so it's about x^2
;  So I estimate something like n^2, empirically it seems > n^3

; 
;;  * x Exercise 1.15
; a. How many times is procedure p used when (sine 12.15 is evaluated?)
(define (count-divisions amount so-far)
  (if (< amount 0.1) 
      so-far 
      (count-divisions (/ amount 3.0) (+ so-far 1))))
(count-divisions 12.15 0) ; => 5
(count-divisions 1215.0 0) ; => 9
(count-divisions 121500.0 0) ; => 13
; Space and steps are the same.
;  The solution strength goes as 3m, which means that
;  both should be O(log n)

;; * x Section 1.2.4
;;  * x Exercise 1.16
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (cond ((= counter 0) product)
	((even? counter) (expt-iter (* b b) (/ counter 2) product))
	(else (expt-iter b (- counter 1) (* b product)))))
(define (even? n)
  (= (remainder n 2) 0))

(expt 3 6)
(expt 5 20)
(expt 2 20000)

;;  * x Exercise 1.17
(define (double a) (+ a a))
(define (halve a) (/ a 2))

(define (fast-* a b)
  (cond ((= b 0) 0)
	((= b 1) a)
        ((even? b) (fast-* (double a) (halve b)))
        (else (+ a (fast-* a (- b 1))))))

(fast-* 7 8)
(fast-* 26 11)
;;  * x Exercise 1.18
(define (fast-*-i a b)
  (fast-*-iter a b 0))

(define (fast-*-iter a b sum)
  (cond ((= b 0) sum)
	((even? b) (fast-*-iter (double a) (halve b) sum))
	(else (fast-*-iter a (- b 1) (+ a sum)))))

(fast-*-i 7 8)
(fast-*-i 26 11)

;;  * x Exercise 1.19
; Tpq (a, b) = (bq + aq + ap, bp + aq)
; Tpq Tpq (a, b) = ((bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p,
;                   (bp + aq)p + (bq + aq + ap)q)
;                = (2bpq + 2aq^2 + bq^2 + 2apq + ap^2, 
;                   bp^2 + 2aqp + bq^2 + aq^2)
;                = (b(2pq + q^2) + a(2q^2 + 2pq + p^2), b(p^2 + q^2) + a(2qp + q^2)
;                = Tp'q'
;          where
;            p' = p^2 + q^2
;            q' = 2qp + q^2
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))      ; compute p'
                   (+ (* 2 q p) (* q q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
(fib 6)
(fib 10)

;; * x Section 1.2.5
;;  * x Exercise 1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
; normal order:
;  substitute the operand expression into the body
;  special case for "if" -- run conditional, then substitute appropriately
; (gcd 206 40) 
;          ; 40 != 0
; (gcd 40 (remainder 206 40)) 
;          ; remainder 206 40 != 0
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40))) 
;          ; remainder 206 40 = 6
;          ; remainder 40 6 != 0
; (gcd (remainder 40 (remainder 206 40)) 
;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 
;          ; remainder 206 40 = 6
;          ; remainder 206 40 = 6
;          ; remainder 40 6 = 4
;          ; remainder 6 4 != 0
; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;      (remainder (remainder 40 (remainder 206 40))
;          (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;          ; remainder 206 40 = 6
;          ; remainder 206 40 = 6
;          ; remainder 206 40 = 6
;          ; remainder 40 6 = 4
;          ; remainder 40 6 = 4
;          ; remainder 6 4 = 2
;          ; remainder 4 2 = 0 == 0
; (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;          ; remainder 206 40 = 6
;          ; remainder 206 40 = 6
;          ; remainder 40 6 = 4
;          ; remainder 6 4 = 2
; Under normal order, we're doing: 18 remainder operations
;
; Under applicative order
;  "evaluate the operands then apply"
; (gcd 206 40)
;          ; 40 != 0
; (gcd 40 (remainder 206 40))
;           ; remainder 206 40 = 6
;           ; gcd 40 6
;           ; 6 != 0
; (gcd 6 (remainder 40 6))
;           ; remainder 40 6 = 4
;           ; gcd 6 4
;           ; 4 != 0
; (gcd 4 (remainder 6 4))
;           ; remainder 6 4 = 2
;           ; gcd 4 2
;           ; 2 != 0
; (gcd 2 (remainder 4 2))
;           ; remainder 4 2 = 0
;           ; gcd 2 0
;           ; 2 == 0
; 2
; total number of remainder calls = 4

;; * x Section 1.2.6
;;  * x Exercise 1.21
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)   ; => 199
(smallest-divisor 1999)  ; => 1999
(smallest-divisor 19999) ; => 7

;;  * x Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= (smallest-divisor n) n))

(define (search-for-primes start end)
  (timed-prime-test start)
  (if (< start end) 
      (search-for-primes (+ start 2) end)))

(search-for-primes 1001 1031)   ; 1009, 1013, 1019 -- all zeros
(search-for-primes 10001 10051) ; 10007, 10009, 10037 -- all zeros
(search-for-primes 100001 100051) ; 100003, 100019, 100043 -- all zeros

(search-for-primes 10000001 10000051) ; 10000019 *** 1.0000000000000009e-2
(search-for-primes 100000001 100000051) ; 100000007 *** .03
(search-for-primes 1000000001 1000000051) ; 1000000007 *** .08999999999999997
; so confirmed O(sqrt(n)) timing; matches expectation that steps=timing

;;  * x Exercise 1.23
(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
; first 12 will all reveal zeros, so using larger numbers
(search-for-primes 10000001 10000051) ; 10000019 *** 0.
(search-for-primes 100000001 100000051) ; 100000007 *** 2.0000000000000018e-2
(search-for-primes 1000000001 1000000051) ; 1000000007 *** .05999999999999983
; ratio is more like 2/3, not 1/2 as we expect
; possibly introducing "next" and "if" causes slowdown
;  let's try controlling for that:
(define (next n)
  (if (= n 2) 3 (+ n 1)))
(search-for-primes 100000001 100000051) ; 100000007 *** 4.0000000000000036e-2
(search-for-primes 1000000001 1000000051) ; 1000000007 *** .1200000000000001
; Now we can see that the 2,3,5,7,9 algorithm is twice as fast as the
;  2,3,4,5,6... algorithm
; But introducing next slows down algorithm by about 4/3 vs just adding 1
; It's still a net win, just not a 2x net win.

;;  * x Exercise 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
      (report-prime (- (runtime) start-time))))

(timed-prime-test 1009) ; 1009 *** 4.0000000000000036e-2
(timed-prime-test 1013) ; 1013 *** 5.0000000000000036e-2
(timed-prime-test 1019) ; 1019 *** 6.0000000000000036e-2
(timed-prime-test 10007) ; 10007 *** .05999999999999961
(timed-prime-test 10009) ; 10009 *** 5.0000000000000266e-2
(timed-prime-test 10037) ; 10037 *** .06000000000000005
(timed-prime-test 100003) ; 10007 *** .08
(timed-prime-test 100019) ; 10007 *** .07
(timed-prime-test 100043) ; 100043 *** .07

(timed-prime-test 10000019) ; 10000019 *** .10999999999999988
(timed-prime-test 100000007) ; 100000007 *** .1200000000000001
(timed-prime-test 1000000007) ; 1000000007 *** .1299999999999999

; Overall trend is increase in 0.01s for every factor of 10

;;  * _ Exercise 1.25
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
; Does this work as a substitute?
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
; (define (expmod base exp m)
;  (remainder (fast-expt base exp) m))

; the original is faster because it doesn't compute the full exponent
;  it uses the intermediary modulos
(expmod 8 100019 100019)

;;  * _ Exercise 1.26
; The problem with Louis's approach is that he is branching twice for every call
;  when he introduces two calls to expmod inside of a single path through expmod.
; So you're embedding a O(p^n) process inside a O(log n) process, giving
;  O(log(p^n)) = O(n * log p) = O(n)
 
;;  * _ Exercise 1.27
(define (test-carmichael n)
  (define (test-one a)
    (= a (expmod a n n)))
  (define (test-recurse a)
    (if (< a n)
	(and (test-one a) (test-recurse (+ a 1)))
	true))
  (test-recurse 1))

(test-carmichael 2500)

(test-carmichael 561)
(test-carmichael 1105)
(test-carmichael 1729)
(test-carmichael 2465)
(test-carmichael 2821)
(test-carmichael 6601)
; all check out

;;  * _ Exercise 1.28
(define (expmod-miller-rabin base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
	 (non-trivial-square-root-of-1? base m 
	  (remainder (square (expmod-miller-rabin base (/ exp 2) m))
                    m)))
        (else
         (remainder (* base (expmod-miller-rabin base (- exp 1) m))
                    m))))
(define (non-trivial-square-root-of-1? base m result)
  (if (and 
       (= result 1)
       (not (= base 1))
       (not (= base (- m 1))))
      0 ; signal non-trivial-square-root
      result))

(define (fermat-miller-rabin-test n)
  (define (try-it a)
    (display a)
    (= (expmod-miller-rabin a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin-prime-test? n times)
  (cond ((= times 0) true)
        ((fermat-miller-rabin-test n) (miller-rabin-prime-test? n (- times 1)))
        (else false)))

(define (miller-rabin-prime? n)
  (miller-rabin-prime-test? n 100))

(miller-rabin-prime? 121)  ; #f
(miller-rabin-prime? 561)  ; #f
(miller-rabin-prime? 1999) ; #t
(miller-rabin-prime? 6601) ; #f 
(miller-rabin-prime? 10043) ; #f
(miller-rabin-prime? 100043) ; #t
(miller-rabin-prime? 10000019) ; #t 

(define prime? miller-rabin-prime?)
;; * _ Section 1.3.1
;;  * _ Exercise 1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))
(define (inc n) (+ n 1))

(define (cube x) (* x x x))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k) 
    (* (y k)
     (if (even? k) 2 4)))
  (* (/ h 3)
     (+ (y 0) 
	(sum term 1 inc (- n 1))
	(y n))))

(simpson-integral cube 0 1 100)  ; => 1/4
(simpson-integral cube 0 1 1000) ; => 1/4
     
;;  * _ Exercise 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
(define (id n) n)

(sum id 1 inc 10)
(sum cube 2 inc 5)

;;  * _ Exercise 1.31
; recursive
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(product id 1 inc 6)

; factorial
(define (factorial n) (product id 1 inc n))
(factorial 6)

; pi
(define (approximate-pi terms)
  (define (term n)
    (/ (if (even? n) (+ n 2) (+ n 3))
       (if (even? n) (+ n 3) (+ n 2))))
  (* 4 (product term 0 inc (- terms 1))))

(approximate-pi 10)  ; 524288/160083 = 3.275
(approximate-pi 100) ; 3.1570301764551676

; iterative
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(product id 1 inc 6)

;;  * _ Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(define (accumulate-sum term a next b)
  (accumulate + 0 term a next b))
(accumulate-sum id 1 inc 10)

(define (accumulate-product term a next b)
  (accumulate * 1 term a next b))
(accumulate-product id 1 inc 6)

; iterative
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(accumulate + 0 id 1 inc 10)
(accumulate * 1 id 1 inc 6)

;;  * _ Exercise 1.33
; haven't learned lambda yet, so not able to just reuse accumulate
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (filter a)
			(combiner result (term a))
			result))))
  (iter a null-value))

; a. sum of squares of primes
(define (prime? n) (miller-rabin-prime? n))
(define (sum-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(sum-primes 2 10)

; b. product of all positive ints < n, relatively prime to n
(define (product-relative-primes-to n)
  (define (filter i) (= (gcd i n) 1))
  (filtered-accumulate * 1 id 1 inc n filter))

(product-relative-primes-to 10) ; => 189

;; * _ Section 1.3.2
;;  * _ Exercise 1.34
; Failure, because (f f) evaluates to (f 2),
; but then we get (2 2); the first 2 is not a function
; so the interpreter fails.

;; * _ Section 1.3.3
;;  * _ Exercise 1.35
(define ϕ (/ (+ 1 (sqrt 5)) 2))
(+ 1 (/ 1 ϕ)) ; => ϕ = 1.618033988749895

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1) ; => 987 / 610 = 1.6180327869

;;  * _ Exercise 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2) ; => 4.55553227080 in 34 steps

(define (fixed-point-dampened f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (/ (+ guess (f guess)) 2)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point-dampened (lambda (x) (/ (log 1000) (log x))) 2) ; => 4.555538 in 8 steps


;;  * _ Exercise 1.37
; a
(define (cont-frac n d k)
 (define (iter i result)
   (if (= i 0)
       result
       (iter (- i 1) (/ (n i) (+ result (d i))))))
 (iter k 0))

(/ 1 ϕ) ; => .61803...
(define (approximate-inv-phi k) 
   (cont-frac (lambda (i) 1.0)
	       (lambda (i) 1.0)
	       k)
   )

(approximate-inv-phi 3) ; => .6666
(approximate-inv-phi 4) ; => .6000
(approximate-inv-phi 5) ; => .625
(approximate-inv-phi 7) ; => .6190
(approximate-inv-phi 9) ; => .61818
(approximate-inv-phi 10) ; => .617977
(approximate-inv-phi 11) ; => .618056 "accurate to 4 decimal places"

; b. recursive - is there any way to avoid the "i" counter?
(define (cont-frac n d k)
  (define (recurse i)
    (if (> i k)
	0
	(/ (n i) (+ (d i) (recurse (+ i 1))))))
  (recurse 1))

;;  * _ Exercise 1.38
; d = 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8...
(define (d i)
  (if (= (remainder i 3) 2)
      (* 2 (/ (+ i 1) 3))
      1))
(d 1) ; => 1
(d 2) ; => 2
(d 3) ; => 1
(d 5) ; => 4
(d 8) ; => 6

(define (approximate-e terms)
  (+ 2 (cont-frac (lambda (i) 1.0) d 10)))

(approximate-e 10)

;;  * _ Exercise 1.39
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
	x
	(- (* x x))))
  (cont-frac n
	      (lambda (i) (- (* 2 i) 1))
	      k))

(tan (/ 3.141592654 4))
(tan-cf (/ 3.141592654 4) 4)
(tan-cf (/ 3.141592654 4) 10)

;; * _ Section 1.3.5
;;  * _ Exercise 1.40
; setup and test
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-nm x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

; define cubic
(define (cubic a b c)
  (lambda (x) (+
	       (* x x x)
	       (* a x x)
	       (* b x)
	       c)))

(newtons-method (cubic -2 -4 3 ) 1) ; .6180339887498948
(newtons-method (cubic -2 -1 -6 ) 1) ;  3.0000000000081006

;;  * _ Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

(((double double) inc) 5)          ; => 9
(((double (double double)) inc) 5) ; => 21

;;  * _ Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

;;  * _ Exercise 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (lambda (x) (f ((repeated f (- n 1)) x)))))

((repeated square 2) 5) ; => 625

;;  * _ Exercise 1.44
(define dx 0.01)
(define (smooth f)
  (lambda (x) (/ (+ 
		  (f (- x dx))
		  (f x)
		  (f (+ x dx))) 3)))

((smooth cos) 0) ; => .9999666669444435
(/ (+ (cos -0.01) 1 (cos 0.01)) 3) ; match

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

((n-fold-smooth cos 3) 0) ; => .9999000041665712
((smooth (smooth (smooth cos))) 0) ; match

;;  * _ Exercise 1.45
(define tolerance 0.00001)
(define (average a b) (/ (+ a b) 2))
(define (fixed-point-damp-n f n first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (average-damp f)
    (lambda (x) (average x (f x))))
  (define (try guess guesses-left)
    (newline)
    (display guess)
    (let ((next (((repeated average-damp n) f) guess)))
      (cond ((close-enough? guess next) next)
	    ((= guesses-left 0) false)
	    (else (try next (- guesses-left 1))))))
  (try first-guess 1500))

(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))

(fixed-point-damp-n (lambda(y) (/ 27 (* y y))) 1 2.0)   ; 3rd root converges
(fixed-point-damp-n (lambda(y) (/ 81 (* y y y))) 1 2.0) ; 4th root/1 damp does NOT
(fixed-point-damp-n (lambda(y) (/ 81 (* y y y))) 2 2.0) ; 4th root/2 damp does
(fixed-point-damp-n (lambda(y) (/ 81 (pow y 4))) 2 2.0) ; 5th rt/2 damp does
(fixed-point-damp-n (lambda(y) (/ 81 (pow y 5))) 2 2.0) ; 6th rt/2 damp does
(fixed-point-damp-n (lambda(y) (/ 81 (pow y 6))) 2 2.0) ; 7th rt/2 damp does
(fixed-point-damp-n (lambda(y) (/ 81 (pow y 7))) 2 2.0) ; 8th rt/2 damp does NOT
(fixed-point-damp-n (lambda(y) (/ 81 (pow y 7))) 3 2.0) ; 8th rt/3 damp does
(fixed-point-damp-n (lambda(y) (/ 81 (pow y 14))) 3 2.0) ; 15th rt/3 damp does
(fixed-point-damp-n (lambda(y) (/ 81 (pow y 15))) 3 2.0) ; 16th rt/3 damp does NOT
(fixed-point-damp-n (lambda(y) (/ 81 (pow y 15))) 4 2.0) ; 16th rt/4 damp does

; conclusion of experimentation use at least (floor (base-2-log n)) damps
(define (nth-root x n)
  (fixed-point-damp-n
   (lambda (y) (/ x (pow y (- n 1))))
   (floor (/ (log n) (log 2)))
   (/ (log x) (log n) )))

(nth-root 81 2)
(nth-root 1296 4)
(nth-root 12789452 16)
(nth-root 127894523423422 16)
(nth-root 127894523423422 17)
(nth-root 127894523423422 17)
(nth-root 1296422342342342342342324 123)
(nth-root 1296342 1234)
(nth-root 129 129)
 
;;  * _ Exercise 1.46
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
	guess
	(iter (improve guess))))
  iter)

((iterative-improve (lambda (x) (> x 6)) (lambda (x) (+ x 1))) 3)

(define (sqrt-ii x)
  ((iterative-improve
    (lambda (guess) (< (abs (- (square guess) x)) 0.001))
    (lambda (guess) (average guess (/ x guess))) 
   ) 1.0))

(sqrt-ii 64)

(define (fixed-point-ii f first-guess)
  ((iterative-improve
   (lambda (guess) (< (abs (- guess (f guess))) tolerance))
   f) first-guess))

(fixed-point-ii sin 1.0) ; => .0391432343461307
(sin .0391432343461307)  ; => .039133...

