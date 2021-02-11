;;Exercise 1.9
;;Fisrt is recursive
;;Second is iterative

;;Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10) ;;2^10
(A 2 4)  ;;2^16
(A 3 3)  ;;2^16

(define (f n) (A 0 n)) ;;2*n
(define (g n) (A 1 n)) ;;if n=0 then 0 else 2^n
(define (h n) (A 2 n)) ;;if n=0 then 0 else (2^2^..nge2)

;;Counting change
;;Exercise 1.11
(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1))
         (* 2 (f-recur (- n 2)))
         (* 3 (f-recur (- n 3))))))

(define (f-iter n)
  (if (< n 3)
      n
      (f-iter-impl 0 1 2 n))

(define (f-iter-impl i j k count)
  (if (= count 2)
      k
      (f-iter-impl j k (+ (* 3 i) (* 2 j) k) (- count 1))))

;;Exercise 1.12
(define (pascal-triangle row col)
  (cond ((> row col)
         (error "invalid parameters"))
        ((or (= col 0) (= row col))
         1)
        (else (+ (pascal-triangle (- row 1) (- col 1))
                 (pascal-triangle (- row 1) col)))))

;;Exerise 1.16
(define (fast-expt b n)
  (fast-expt-impl b n 1))

(define (fast-expt-impl b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-impl (* b b) (/ n 2) a))
        (else (fast-expt-impl b (- n 1) (* a b)))))

;;Exercise 1.17
(define (double n)
  (* n 2))
(define (halve n)
  (/ n 2))
(define (multi a b)
  (cond ((= b 0) 0)
        ((even? b) (double (multi a (halve b))))
        (else (+ a (multi a (- b 1)))))

;;Exercise 1.18
(define (fast-multi a b)
  (fast-multi-impl a b 0))
(define (fast-multi-impl a b sum)
  (cond ((= b 0) sum)
        ((even? b) (fast-multi-impl (double a) (halve b) sum))
        (else (fast-multi-impl a (- b 1) (+ sum a)))))

;;Exercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
