#|

This file is a part of NUMCL project.
Copyright (c) 2019 IBM Corporation
SPDX-License-Identifier: LGPL-3.0-or-later

NUMCL is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

NUMCL is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
NUMCL.  If not, see <http://www.gnu.org/licenses/>.

|#

(in-package :numcl.impl)

;; note: consider using specialized-function

;; on sbcl, trivia's pattern match can be entirely constant folded through compile-time information.
;; Example:
#+(or)
(defun fn ()
  (declare (optimize (speed 3)))
  (let ((a (make-array 5 :element-type 'fixnum)))
    (match a
      ((array :element-type (integer-subtype _ high))
       high))))

;; thus by inlining map-array under a suitable set of declarations, the pattern matching simply disappears.

(declaim (inline numcl:map-into))
(defun numcl:map-into (result-sequence function &rest sequences)
  (if (every (of-type 'sequence) sequences)
      (apply #'map-into result-sequence function sequences)
      (apply #'map-array-into result-sequence function sequences)))

(declaim (inline numcl:map))
(defun numcl:map (result-type function &rest sequences)
  (if (every (of-type 'sequence) sequences)
      (apply #'map result-type function sequences)
      (if result-type
          (apply #'map-array-into
                 (empty (shape (first sequences)) :type (array-subtype-element-type result-type))
                 function
                 sequences)
          ;; when result-type is nil, do not collect the results into a new array.
          (apply #'map nil
                 function
                 (mapcar #'flatten sequences)))))

(declaim (inline map-array-into))
(defun map-array-into (result-sequence function &rest sequences)

  (assert (every #'arrayp sequences))
  (assert (every #'equal
                 (map 'vector #'shape sequences)
                 (map 'vector #'shape (cdr sequences))))
  (assert (equal (shape result-sequence)
                 (shape (first sequences))))
  
  (let ((type (array-element-type result-sequence)))
    (flet ((fn (x)
             (coerce (funcall function x) type)))
      (declare (inline fn))
      (declare (dynamic-extent #'fn))
      (apply #'map-into (flatten result-sequence) #'fn (mapcar #'flatten sequences))
      result-sequence)))

(declaim (inline map-array))
(defun map-array (function &rest sequences)
  (let ((type (apply #'infer-type function
                     (mapcar #'array-element-type sequences))))
    (apply #'map-array-into
           (empty (shape (first sequences)) :type type)
           function
           sequences)))

(declaim (inline broadcast-p))
(defun broadcast-p (x y)
  (every (lambda (dx dy)
           (declare (fixnum dx dy))
           (or (= dx dy)
               (= dx 1)
               (= dy 1)))
         (reverse (shape x))
         (reverse (shape y))))

(declaim (inline broadcast))
(defun broadcast (fn x y &key type (atomic (fdefinition fn)))
  "For binary functions"
  (if (and (not (arrayp x)) (not (arrayp y)))
      (funcall atomic x y)
      ;; example: 
      ;; x shape: (10 3 1 4)
      ;; y shape: (2 10 1 2 4)
      (progn
        (assert (broadcast-p x y) nil
                "Given arrays have incompatible shape for broadcasting the ~a op:~% ~a and ~a.~_ Arrays:~%~a~%~a"
                fn (shape x) (shape y) x y)
        (let* ((x (asarray x))
               (y (asarray y))
               (type (or type (infer-type fn
                                          (array-element-type x)
                                          (array-element-type y))))
               (rrank (max (rank x) (rank y)))
               ;; make their ranks equal, e.g.
               ;; (1 10 3 1 4)
               ;; (2 10 1 2 4)
               ;; 
               (xshape (append (make-list (- rrank (rank x)) :initial-element 1) (shape x)))
               (yshape (append (make-list (- rrank (rank y)) :initial-element 1) (shape y)))
               ;; result shape:       (2 10 3 2 4)
               (rshape (mapcar #'max xshape yshape)))
          (multiple-value-bind (r rbase) (empty rshape :type type)
            (multiple-value-bind (xbase xoffset) (array-displacement x)
              (multiple-value-bind (ybase yoffset) (array-displacement y)
                (labels ((broadcast-core (fn
                                          xbase xoffset xshape
                                          ybase yoffset yshape
                                          rbase roffset rshape)
                           (match* (xshape yshape rshape)
                             (((list 1)
                               (list ydim))
                              (let ((x (aref xbase xoffset)))
                                (dotimes (i ydim)
                                  (setf (aref rbase (+ roffset i))
                                        (%coerce (funcall fn x (aref ybase (+ yoffset i))) (array-element-type rbase))))))
                             (((list xdim)
                               (list 1))
                              (let ((y (aref ybase yoffset)))
                                (dotimes (i xdim)
                                  (setf (aref rbase (+ roffset i))
                                        (%coerce (funcall fn (aref xbase (+ xoffset i)) y) (array-element-type rbase))))))
                             (((list xdim)
                               _)
                              (dotimes (i xdim)
                                (setf (aref rbase (+ roffset i))
                                      (%coerce (funcall fn
                                                        (aref xbase (+ xoffset i))
                                                        (aref ybase (+ yoffset i)))
                                               (array-element-type rbase)))))
                             
                             (((list* 1    xrest)
                               (list* ydim yrest)
                               (list* _    rrest))
                              (iter (with yrest-size = (reduce #'* yrest))
                                    (with rrest-size = (reduce #'* rrest))
                                    (for i below ydim)
                                    (broadcast-core fn
                                                    xbase xoffset xrest
                                                    ybase yoffset yrest
                                                    rbase roffset rrest)
                                    (incf yoffset yrest-size)
                                    (incf roffset rrest-size)))

                             (((list* xdim xrest)
                               (list* 1    yrest)
                               (list* _    rrest))
                              (iter (with xrest-size = (reduce #'* xrest))
                                    (with rrest-size = (reduce #'* rrest))
                                    (for i below xdim)
                                    (broadcast-core fn
                                                    xbase xoffset xrest
                                                    ybase yoffset yrest
                                                    rbase roffset rrest)
                                    (incf xoffset xrest-size)
                                    (incf roffset rrest-size)))

                             (((list* xdim xrest)
                               (list* _    yrest)
                               (list* _    rrest))
                              (iter (with xrest-size = (reduce #'* xrest))
                                    (with yrest-size = (reduce #'* yrest))
                                    (with rrest-size = (reduce #'* rrest))
                                    (for i below xdim)
                                    (broadcast-core fn
                                                    xbase xoffset xrest
                                                    ybase yoffset yrest
                                                    rbase roffset rrest)
                                    (incf xoffset xrest-size)
                                    (incf yoffset yrest-size)
                                    (incf roffset rrest-size))))))
                           
                  (broadcast-core fn
                                  xbase xoffset xshape
                                  ybase yoffset yshape
                                  rbase 0       rshape)
                  (values r rbase)))))))))

;; (broadcast '+ (arange 10) (arange 10))
;; #(0 2 4 6 8 10 12 14 16 18)
;; (broadcast '+ (reshape (arange 10) '(10 1)) (reshape (arange 10) '(10 1)))
;; #((0) (2) (4) (6) (8) (10) (12) (14) (16) (18))
;; (broadcast '* (reshape (arange 10) '(10 1)) (reshape (arange 10) '(1 10)))

;; (broadcast '* (arange 3) 4)
;; #(0 4 8)



;; TODO: optimization
(declaim (inline numcl:sin numcl:cos numcl:tan numcl:asin numcl:acos numcl:atan
                 numcl:sinh numcl:cosh numcl:tanh numcl:exp numcl:log numcl:abs
                 numcl:signum numcl:cis numcl:conjugate numcl:phase
                 numcl:realpart numcl:imagpart numcl:numerator numcl:denominator
                 numcl:logcount numcl:integer-length))

;; inlining makes them as fast as the normal CL functions when the arguments are numbers

#+(or)
(progn
  ;; sbcl is not able to propagate the constant
  (defun fn1 (x)
    (print x))
  (define-compiler-macro fn1 (&whole whole x)
    (format t "~%;; ~50a : ~:[fluent~;constant~]" x (constantp x))
    whole)

  (declaim (inline fn2))
  (defun fn2 (x)
    (fn1 x)
    (fn1 '(1 2))
    (fn1 `(1 ,x 2))
    (fn1 `(1 3 2)))
  
  (defun fn3 ()
    (fn2 3))

  ;; ; compiling (DEFUN FN1 ...)
  ;; ; compiling (DEFINE-COMPILER-MACRO FN1 ...)
  ;; ; compiling (DECLAIM (INLINE FN2))
  ;; ; compiling (DEFUN FN2 ...)
  ;; ;; X                                                  : fluent
  ;; ;; '(1 2)                                             : constant
  ;; ;; `(1 ,X 2)                                          : fluent
  ;; ;; `(1 3 2)                                           : constant
  ;; ; compiling (DEFUN FN3 ...)
  ;; ;; X                                                  : fluent
  ;; ;; '(1 2)                                             : constant
  ;; ;; `(1 ,X 2)                                          : fluent
  ;; ;; `(1 3 2)                                           : constant
  )

#+(or)
(defun numcl:sin (x)
  (if (numberp x)
      (sin x)
      (let ((y (empty (shape x) :type (infer-type 'sin (array-element-type x)))))
        (einsum '(i -> (sin $1) -> i)
                (flatten x)
                (flatten y))
        y)))

(defmacro define-simple-mapper (numcl-fn cl-fn)
  `(defun ,numcl-fn (x)
     (if (numberp x)
         (,cl-fn x)
         (let ((y (empty (shape x) :type (infer-type ',cl-fn (array-element-type x)))))
           (einsum '(i -> (,cl-fn $1) -> i)
                   (flatten x)
                   (flatten y))
           y))))

(define-simple-mapper numcl:sin sin)
(define-simple-mapper numcl:cos cos)
(define-simple-mapper numcl:tan tan)
(define-simple-mapper numcl:asin asin)
(define-simple-mapper numcl:acos acos)
(define-simple-mapper numcl:atan atan)
(define-simple-mapper numcl:sinh sinh)
(define-simple-mapper numcl:cosh cosh)
(define-simple-mapper numcl:tanh tanh)
(define-simple-mapper numcl:exp exp)
(define-simple-mapper numcl:log log)
(define-simple-mapper numcl:sqrt sqrt)
(define-simple-mapper numcl:abs abs)
(define-simple-mapper numcl:signum signum)
(define-simple-mapper numcl:cis cis)
;; (defun complex        (x) (map-array 'complex x))
(define-simple-mapper numcl:conjugate conjugate)
(define-simple-mapper numcl:phase phase)
(define-simple-mapper numcl:realpart realpart)
(define-simple-mapper numcl:imagpart imagpart)
(define-simple-mapper numcl:numerator numerator)
(define-simple-mapper numcl:denominator denominator)
(define-simple-mapper numcl:logcount logcount)
(define-simple-mapper numcl:integer-length length)
;; (defun float          (x) (map-array 'float x))

(declaim (inline square))
(defun square (x) (* x x))
(define-simple-mapper numcl:square square)

;; type upgrading. Why this is not available by default in common lisp??

(declaim (inline numcl:1+))
(defun numcl:1+ (array) (numcl:+ array 1))

(declaim (inline numcl:1-))
(defun numcl:1- (array) (numcl:- array 1))

;; TODO: optimizing broadcast ordering
;; TODO: optimize by reusing intermediate results
;; TODO: optimize by loop fusion

(declaim (inline numcl:+ numcl:- numcl:* numcl:/ numcl:max numcl:min numcl:clip))

(defun numcl:+   (&rest args) (reduce (lambda (x y) (broadcast '+ x y)) args))
(defun numcl:-   (&rest args) (reduce (lambda (x y) (broadcast '- x y)) args))
(defun numcl:*   (&rest args) (reduce (lambda (x y) (broadcast '* x y)) args))
(defun numcl:/   (&rest args) (reduce (lambda (x y) (broadcast '/ x y)) args))
(defun numcl:max (&rest args) (reduce (lambda (x y) (broadcast 'max x y)) args))
(defun numcl:min (&rest args) (reduce (lambda (x y) (broadcast 'min x y)) args))

(defun numcl:clip (array min max) (broadcast 'max min (broadcast 'min array max)))

(define-symbol-macro numcl:+ +)
(define-symbol-macro numcl:++ ++)
(define-symbol-macro numcl:+++ +++)
(define-symbol-macro numcl:* *)
(define-symbol-macro numcl:** **)
(define-symbol-macro numcl:*** ***)
(define-symbol-macro numcl:/ /)
(define-symbol-macro numcl:// //)
(define-symbol-macro numcl:/// ///)

;; round

(declaim (inline numcl:mod numcl:rem numcl:round numcl:floor numcl:ceiling
                 numcl:truncate numcl:fround numcl:ffloor numcl:fceiling
                 numcl:ftruncate))

(defun numcl:mod       (number &optional (divisor 1)) (broadcast 'mod       number divisor))
(defun numcl:rem       (number &optional (divisor 1)) (broadcast 'rem       number divisor))
(defun numcl:round     (number &optional (divisor 1)) (broadcast 'round     number divisor))
(defun numcl:floor     (number &optional (divisor 1)) (broadcast 'floor     number divisor))
(defun numcl:ceiling   (number &optional (divisor 1)) (broadcast 'ceiling   number divisor))
(defun numcl:truncate  (number &optional (divisor 1)) (broadcast 'truncate  number divisor))
(defun numcl:fround    (number &optional (divisor 1)) (broadcast 'fround    number divisor))
(defun numcl:ffloor    (number &optional (divisor 1)) (broadcast 'ffloor    number divisor))
(defun numcl:fceiling  (number &optional (divisor 1)) (broadcast 'fceiling  number divisor))
(defun numcl:ftruncate (number &optional (divisor 1)) (broadcast 'ftruncate number divisor))

;; comparisons

;; TODO: &rest arguments

(declaim (inline =/bit))
(defun =/bit  (x y) (if (= x y) 1 0))
(declaim (inline /=/bit))
(defun /=/bit (x y) (if (/= x y) 1 0))
(declaim (inline <=/bit))
(defun <=/bit (x y) (if (<= x y) 1 0))
(declaim (inline >=/bit))
(defun >=/bit (x y) (if (>= x y) 1 0))
(declaim (inline </bit))
(defun </bit  (x y) (if (<  x y) 1 0))
(declaim (inline >/bit))
(defun >/bit  (x y) (if (>  x y) 1 0))

(declaim (inline numcl:= numcl:/= numcl:<= numcl:>= numcl:< numcl:>))

(defun numcl:=  (x y) (broadcast '=/bit  x y :atomic #'= ))
(defun numcl:/= (x y) (broadcast '/=/bit x y :atomic #'/=))
(defun numcl:<= (x y) (broadcast '<=/bit x y :atomic #'<=))
(defun numcl:>= (x y) (broadcast '>=/bit x y :atomic #'>=))
(defun numcl:<  (x y) (broadcast '</bit  x y :atomic #'< ))
(defun numcl:>  (x y) (broadcast '>/bit  x y :atomic #'> ))

;; better trivia pattern integration

(defpattern numcl:=  (x) `(=  ,x))
(defpattern numcl:/= (x) `(/= ,x))
(defpattern numcl:<= (x) `(<= ,x))
(defpattern numcl:>= (x) `(>= ,x))
(defpattern numcl:<  (x) `(<  ,x))
(defpattern numcl:>  (x) `(>  ,x))


;; bitwise operations

(declaim (inline numcl:logand numcl:logandc1 numcl:logandc2 numcl:logeqv
                 numcl:logior numcl:lognand numcl:lognor numcl:logorc1
                 numcl:logorc2 numcl:logxor numcl:lognot))


(defun numcl:logand   (&rest args) (reduce (lambda (x y) (broadcast 'logand   x y)) args))
(defun numcl:logandc1 (&rest args) (reduce (lambda (x y) (broadcast 'logandc1 x y)) args))
(defun numcl:logandc2 (&rest args) (reduce (lambda (x y) (broadcast 'logandc2 x y)) args))
(defun numcl:logeqv   (&rest args) (reduce (lambda (x y) (broadcast 'logeqv   x y)) args))
(defun numcl:logior   (&rest args) (reduce (lambda (x y) (broadcast 'logior   x y)) args))
(defun numcl:lognand  (&rest args) (reduce (lambda (x y) (broadcast 'lognand  x y)) args))
(defun numcl:lognor   (&rest args) (reduce (lambda (x y) (broadcast 'lognor   x y)) args))
(defun numcl:logorc1  (&rest args) (reduce (lambda (x y) (broadcast 'logorc1  x y)) args))
(defun numcl:logorc2  (&rest args) (reduce (lambda (x y) (broadcast 'logorc2  x y)) args))
(defun numcl:logxor   (&rest args) (reduce (lambda (x y) (broadcast 'logxor   x y)) args))

(define-simple-mapper numcl:lognot lognot)




;; unused functions

#+(or)
(defun m2shape (mshape)
  (iter (for mdim in mshape)
        (for mdim-prev previous mdim)
        (when (first-iteration-p)
          (collect mdim)
          (next-iteration))
        (if (= 1 mdim)
            (if (= 1 mdim-prev)
                nil
                (collect mdim))
            (collect mdim))))

;; (m2shape '(1 1 10 1 1 1 3 1 1))
;; (1 10 1 3 1)

#+(or)
(defun r2shape (mshape rshape)
  (iter (for mdim in mshape)
        (for mdim-prev previous mdim)
        (for rdim in rshape)
        (with acc = nil)
        (when (first-iteration-p)
          (push rdim acc)
          (next-iteration))
        (if (= 1 mdim mdim-prev)
            (setf (first acc) (* (first acc) rdim))
            (push rdim acc))
        (finally
         (return (nreverse acc)))))

;; (r2shape '(1 10 1 1 4) '(2 10 2 3 4))
;; (2 10 6 4)

