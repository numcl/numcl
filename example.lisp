(ql:quickload :numcl)
(in-package :numcl)

;; initialization
(zeros 5)
(zeros '(5 5))
(zeros 5 :type 'fixnum)
(ones 5)
(arange 10)

;; reshaping
(reshape (arange 10) '(2 5))

;; arange with negative steps
(arange -10 10 3)
(arange 10 -10 -3)

;; concatenation, stacking, unstacking
(concatenate (list (zeros 10) (ones  10)))

(concatenate (list (reshape (zeros 10) '(2 5))
                   (reshape (ones  10) '(2 5))))
(concatenate (list (reshape (zeros 10) '(2 5))
                   (reshape (ones  10) '(2 5)))
             :axis 1)

(stack (list (reshape (zeros 10) '(2 5))
             (reshape (ones  10) '(2 5))))
(stack (list (reshape (zeros 10) '(2 5))
             (reshape (ones  10) '(2 5)))
       :axis 1)
(stack (list (reshape (zeros 10) '(2 5))
             (reshape (ones  10) '(2 5)))
       :axis 2)

(unstack (reshape (ones  10) '(2 5)))
(unstack (reshape (ones  10) '(2 5)) :axis 1)

;; array slice access
(defparameter *a* (reshape (arange 100) '(4 5 5)))
*a*
(aref *a* 0)
(aref *a* '(0 1))
(aref *a* 0 '(0 2) 0)
(setf (aref *a* 2 '(0 2) 1) (full 2 100))
*a*

;; broadcasting
(numcl:+ (arange 5) (reshape (arange 3) '(3 1)))
(numcl:* (arange 5) (reshape (arange 3) '(3 1)))

(defparameter *a* (arange 10))
(defparameter *b* (reshape (asarray '(1 5 2)) '(t 1)))
*b*
(* *b* *a*)
(sin (* *b* *a*))
(exp (* *b* *a*))
(log (1+ (* *b* *a*)))
(+ 3 (log (1+ (* *b* *a*))))
(arange -5 5 2)
(log (arange -5 5 2))
(arange 10.0d0)
(+ 1 (arange 10.0d0))
(+ (arange 10.0d0) (arange 10.0))
(arange 3.3)
(arange 3.3 5.5 1.1)
(arange 3.3 5.5 1.1d0)
(arange 3.3d0 5.5d0 1.1d0)
(arange 3.3d0 11/2 1.1d0)
(arange 2 1 1)
(arange 1 2 5)
(arange 5 :type 'double-float)
(arange 2 5 :type 'double-float)
(arange 2 5 2 :type 'double-float)

(arange 5)
(type-of *)
(arange 5.0)
(type-of *)
(arange 5.0d0)
(type-of *)
(+ (arange 5) 3)
(type-of *)
(+ (arange 5) 3.0)
(type-of *)
(+ (arange 5.0) 3)
(type-of *)
(/ (arange 5) (arange 1 6))
(type-of *)

(= (arange 5) (reshape (arange 1 6) '(t 1))) ;; #2A((0 1 0 0 0) (0 0 1 0 0) (0 0 0 1 0) (0 0 0 0 1) (0 0 0 0 0))
(< (arange 5) (reshape (arange 1 6) '(t 1))) ;; #2A((1 0 0 0 0) (1 1 0 0 0) (1 1 1 0 0) (1 1 1 1 0) (1 1 1 1 1))
(logand (arange 5) (arange 1 6))

;; type inference on logand
(logand (uniform 3 7 3) (reshape (uniform 3 7 3) '(t 1)))
(logand (uniform 3 7 3) (reshape (uniform 3 7 3) '(t 1)))
(logand (uniform -3 7 3) (reshape (uniform 3 7 3) '(t 1)))
;; (logand minus minus) is guaranteed to be minus, therefore this is a signed-integer
(logand (uniform -3 7 3) (reshape (uniform -3 7 3) '(t 1)))


;; reduction

(sum (arange 5))
(sum (reshape (arange 125) '(5 5 5)))                ; sum all elements
(sum (reshape (arange 125) '(5 5 5)) :axes '())      ; does nothing
(sum (reshape (arange 125) '(5 5 5)) :axes '(0))
(sum (reshape (arange 125) '(5 5 5)) :axes '(0 1))
(sum (reshape (arange 125) '(5 5 5)) :axes '(0 1 2)) ; sum all elements, again

(prod (reshape (1+ (arange 16)) '(4 4)))
(prod (reshape (1+ (arange 16)) '(4 4)) :axes '(0))

(amax (reshape (arange 16) '(4 4)))
(amax (reshape (arange 16) '(4 4)) :axes '(0))
(amax (reshape (arange 16) '(4 4)) :axes '(1))

(amin (reshape (arange 16) '(4 4)))
(amin (reshape (arange 16) '(4 4)) :axes '(0))
(amin (reshape (arange 16) '(4 4)) :axes '(1))

(let ((b (uniform 0 10 '(5 5))))
  (print b)
  (print (nonzero b))
  (print (where b #'zerop))
  (print (argwhere b #'zerop))
  
  (print (apply #'mapcar (alexandria:curry #'aref b) (where b #'zerop)))
  (print (apply #'mapcar (alexandria:curry #'aref b) (where b #'plusp)))
  (print (apply #'mapcar (alexandria:curry #'aref b) (where b #'evenp)))

  (print (take b (where b #'zerop)))
  (print (take b (where b #'plusp)))
  (print (take b (where b #'evenp)))

  ;; (print (mapcar (alexandria:curry #'aref b) (where b #'zerop)))
  ;; (print (mapcar (alexandria:curry #'aref b) (where b #'plusp)))
  ;; (print (mapcar (alexandria:curry #'aref b) (where b #'evenp)))
  )

;; einsum

;; gemm
(einsum '(ij jk -> ik)
        #2A((0 1) (2 3))
        #2A((5 6) (7 8)))

;; equivalent
(einsum '((i j) (j k) -> (i k))
        #2A((0 1) (2 3))
        #2A((5 6) (7 8)))

;; diagonal
(einsum '(ii -> i) #2A((0 1) (2 3)))
;; equivalent
(einsum '(ii) #2A((0 1) (2 3)))

;; sum of diagonal
(einsum '(ii ->) #2A((0 1) (2 3)))

;; provide an output array
(let ((result (zeros '(2 2) :type 'single-float)))
  (einsum '(ij -> ji)
          #2A((0 1) (2 3))
          result))

;; return multiple arrays
(einsum '(ijk -> ij ik jk)
        #3A(((0 1) (2 3)) ((4 5) (6 7))))

;; linear algebra

(transpose #2A((0 1) (2 3)))

(matmul #2A((0 1) (2 3))
        #2A((5 6) (7 8)))

(vdot #(0 1 2 3)
      #(5 6 7 8))

(outer #(0 1 2 3)
       #(5 6 7 8))

(kron #2A((0 1) (2 3))
      #2A((5 6) (7 8)))

(diag #2A((0  1  2  3)
          (4  5  6  7)
          (8  9  10 11)
          (12 13 14 15)))

(tril #2A((0  1  2  3)
          (4  5  6  7)
          (8  9  10 11)
          (12 13 14 15)))

(triu #2A((0  1  2  3)
          (4  5  6  7)
          (8  9  10 11)
          (12 13 14 15)))

(tri 3)
(tri 3 :m 5)
(tri 3 :m 5 :k 2)

(eye 3)
(eye 3 :m 5)
(eye 3 :m 5 :k 2)


(vander #(1 2 3 5) :n 3)
(vander #(1 2 3 5) :n 3 :increasing t)
