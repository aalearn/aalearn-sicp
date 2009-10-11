;; * _ Section 4.1

;;  * _ Exercise 4.1
; left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env))) 
	(cons left
	      (list-of-values (rest-operands exps) env)))))


; right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (rest-operands exps) env)))  
	(cons (eval (first-operand exps) env)
	      right))))

