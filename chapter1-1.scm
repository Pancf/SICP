;;Utility
(define (square x) (* x x))
(define (abs x)
  (if (< x 0)
      (- x)
      x))

;;Exercise 1.2
(/ (+ 5 4 (- 2
             (- 3
                (+ 6 (/ 4 3)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;;Exercis 1.3
(define (sum-of-squares x y z)
  (cond ((and (< x y) (< x z)) (square y z))
        ((and (< y x) (< y z)) (square x z))
        ((and (< z x) (< z y)) (square x y))))


;;Square Roots by Newton's Method
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

;;new-if
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
;;;;
;(define (sqrt-iter guess x)
;  (new-if (good-enough? guess x)
;          guess
;          (sqrt-iter (improve guess x)
;                     x)))
;因为解释器使用applicative-order，new-if是一个procedure，在调用时会
;对所有参数进行evaluate，从而导致else-clause发生递归爆栈
;;;;

;;Exercise 1.7
;;之前的good-enough?在面对特别小或者特别大的参数时会导致结果和真实结果相差较大
(define (new-sqrt-iter guess oldguess x)
  (if (new-good-enough? guess oldguess)
      guess
      (new-sqrt-iter (improve guess x) guess x)))
(define (new-good-enough? guess oldguess)
  (< (abs (- guess oldguess))
     (* guess 0.001)))
(define (new-sqrt x) (new-sqrt-iter 1.0 2.0 x))
(new-sqrt 0.009)

;;Exercise 1.8 求x的三次方根
(define (cube-root x) (cube-iter 1.0 2.0 x))
(define (cube-iter guess oldguess x)
  (if (new-good-enough? guess oldguess)
      guess
      (cube-iter (cube-improve guess x) guess x)))
(define (cube-improve guess x)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))
(cube-root 27)
