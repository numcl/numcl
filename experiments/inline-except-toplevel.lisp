
(in-package :numcl.impl)

(defparameter *a* (zeros '(100 100) :type 'single-float))
(defparameter *b* (zeros '(100 100) :type 'single-float))

;; this gets inlined to the inlinable-matmul
(defun f () (matmul *a* *b*))

;; However, when you use matmul on REPL, it does not get inlined into einsum,
;; resusing the original specialized functions cached in matmul

;; (matmul *a* *b*)
