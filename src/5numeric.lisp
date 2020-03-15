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

(declaim (inline map-array-into))
(defun map-array-into (result-sequence function &rest sequences)

  (assert (numcl-array-p result-sequence))
  (iter (with result-shape = (shape result-sequence))
        (for sequence in sequences)
        (assert (numcl-array-p sequence))
        (assert (equal (shape sequence) result-shape)))
  
  (flet ((fn (&rest args)
           (%coerce (apply function args) (array-element-type result-sequence))))
    (declare (inline fn))
    (declare (dynamic-extent #'fn))
    (apply #'map-into (flatten result-sequence) #'fn (mapcar #'flatten sequences))
    result-sequence))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %ok-to-use-fn-specialized (function env)
  (match function
    ((list 'function (list* 'lambda _))
     t)
    ((list 'function name)
     (multiple-value-match (cltl2:function-information name env)
       ((:function nil _)
        ;; The first indicates the type of function definition or binding
        ;; The second value is true if NAME is bound locally
        t)))
    ((list 'quote _)                 ; global function
     t)))
)

(defmacro %when-function-is-global (&body body)
  "Internal use only! assumes a fixed set of variable names"
  `(let ((fn (macroexpand function env)))
     (if (not (%ok-to-use-fn-specialized fn env))
         (progn
           (simple-style-warning "~a -> ~a : function ~a cannot be inlined by compiler macro (is it a local function?)"
                                 function fn fn)
           whole)
         (progn ,@body))))

(define-compiler-macro map-array-into (&whole whole result-sequence function &rest sequences &environment env)
  (%when-function-is-global
    (let ((offsets (make-gensym-list (1+ (length sequences)) "OFFSET"))
          (bases   (make-gensym-list (1+ (length sequences)) "BASE"))
          (arrays  (make-gensym-list (1+ (length sequences)) "ARRAY")))
      (labels ((get-bases (offsets bases arrays forms body)
                 (ematch* (offsets bases arrays forms)
                   (((list* o orest) (list* b brest) (list* a arest) (list* f frest))
                    `(let ((,a ,f))
                       (assert (numcl-array-p ,a))
                       (multiple-value-bind (,b ,o) (array-displacement ,a)
                         (declare (base-array ,b))
                         ,(get-bases orest brest arest frest body))))
                   ((nil nil nil nil)
                    body))))
        (get-bases
         offsets
         bases
         arrays
         (list* result-sequence sequences)
         (with-gensyms (index limit)
           `(let ((,limit (array-total-size ,(first arrays))))
              (declare (index ,limit))
              (specializing ,(append offsets bases) ()
                (do ((,index 0 (1+ ,index)))
                    ((< ,limit ,index))
                  (declare (index ,index))
                  (setf (aref ,(first bases) (+ ,(first offsets) ,index))
                        (%coerce
                         (funcall ,fn
                                  ,@(iter (for b in (cdr bases))
                                          (for o in (cdr offsets))
                                          (collecting
                                           `(aref ,b (+ ,o ,index)))))
                         (array-element-type ,(first bases))))))
              ,(first arrays))))))))

;; wrapper functions

(declaim (inline numcl:map-into))
(defun numcl:map-into (result-sequence function &rest sequences)
  (if (and (numcl-array-p result-sequence)
           (every #'numcl-array-p sequences))
      (apply #'map-array-into result-sequence function sequences)
      (apply #'map-into result-sequence function sequences)))

(declaim (inline numcl:map))
(defun numcl:map (result-type function &rest sequences)
  (if (every (of-type 'sequence) sequences)
      (apply #'map result-type function sequences)
      (if result-type
          (progn
            (unless (eq '* (array-subtype-dimensions result-type))
              (warn "RESULT-TYPE ~a contains dimension specifiers ~a, but this is ignored in NUMCL:MAP."
                    result-type (array-subtype-dimensions result-type)))
            (apply #'%map-array-with-type
                   (array-subtype-element-type result-type)
                   function
                   sequences))
          ;; when result-type is nil, do not collect the results into a new array.
          (apply #'map nil
                 function
                 (mapcar #'flatten sequences)))))

(declaim (inline map-array))
(defun map-array (function &rest sequences)
  (apply #'%map-array-with-type
         (apply #'infer-type function
                (mapcar #'array-element-type sequences))
         function
         sequences))

(declaim (inline %map-array-with-type))
(defun %map-array-with-type (type function &rest sequences)
  (apply #'map-array-into
         (empty-like (first sequences) :type type)
         function
         sequences))

#+(or)
(define-compiler-macro map-array (&whole whole function &rest sequences &environment env)
  (let ((fn (macroexpand function env)))
    (if (not (%ok-to-use-fn-specialized fn env))
        (progn
          (simple-style-warning "~a -> ~a : function ~a cannot be inlined by compiler macro (is it a local function?)"
                                function fn fn)
          whole)
        (let ((arrays (make-gensym-list (length sequences) "ARRAY")))
          `(let* (,@(mapcar #'list arrays sequences))
             (map-array-into (empty-like ,(first arrays)
                                         :type (infer-type ,fn ,@(iter (for a in arrays)
                                                                       (collecting
                                                                        `(array-element-type ,a)))))
                             ,function
                             ,@arrays))))))

(declaim (inline broadcast-p))
(defun broadcast-p (x y)
  "Broadcasting the binary operation is allowed when
 the axes in the right-aligned common subscripts of the arrays are same or either subscript is 1"
  (declare (numcl-array x y))
  (every (lambda (dx dy)
           (declare (fixnum dx dy))
           (or (= dx dy)
               (= dx 1)
               (= dy 1)))
         (reverse (shape x))
         (reverse (shape y))))

(declaim (inline broadcast-result-shape))
(defun broadcast-result-shape (x y)
  (declare (numcl-array x y))
  (let* ((rrank (max (rank x) (rank y)))
         ;; make their ranks equal, e.g.
         ;; (1 10 3 1 4)
         ;; (2 10 1 2 4)
         ;; 
         (xshape (append (make-list (- rrank (rank x)) :initial-element 1) (shape x)))
         (yshape (append (make-list (- rrank (rank y)) :initial-element 1) (shape y)))
         ;; result shape:       (2 10 3 2 4)
         (rshape (mapcar #'max xshape yshape)))
    (values rshape xshape yshape)))

(declaim (inline broadcast))
(defun broadcast (function x y &key type (atomic function))
  "For binary functions"
  (cond
    ((and (not (arrayp x)) (not (arrayp y)))
     (funcall atomic x y))
    (t
     ;; example: 
     ;; x shape: (10 3 1 4)
     ;; y shape: (2 10 1 2 4)
     (let ((x      (if (arrayp x) x (full nil x)))
           (y      (if (arrayp y) y (full nil y)))
           (x-type (if (arrayp x) (array-element-type x) (strict-type-of x)))
           (y-type (if (arrayp y) (array-element-type y) (strict-type-of y))))
       ;; force singleton
       (declare (numcl-array x y))
       (assert (broadcast-p x y) nil
               "Given arrays have incompatible shape for broadcasting the ~a op:~% ~a and ~a.~_ Arrays:~%~a~%~a"
               function (shape x) (shape y) x y)
       (let* ((type (or type (infer-type function x-type y-type))))
         (multiple-value-bind (rshape xshape yshape) (broadcast-result-shape x y)
           (multiple-value-bind (r rbase) (empty rshape :type type)
             (declare (base-array rbase))
             (multiple-value-bind (xbase xoffset) (array-displacement x)
               (declare (base-array xbase))
               (multiple-value-bind (ybase yoffset) (array-displacement y)
                 (declare (base-array ybase))
                 (labels ((broadcast-core (xoffset xshape
                                                   yoffset yshape
                                                   roffset rshape)
                            (match* (xshape yshape rshape)
                              (((list 1)
                                (list ydim))
                               (let ((x (aref xbase xoffset)))
                                 (dotimes (i ydim)
                                   (setf (aref rbase (+ roffset i))
                                         (%coerce (funcall function x (aref ybase (+ yoffset i))) (array-element-type rbase))))))
                              (((list xdim)
                                (list 1))
                               (let ((y (aref ybase yoffset)))
                                 (dotimes (i xdim)
                                   (setf (aref rbase (+ roffset i))
                                         (%coerce (funcall function (aref xbase (+ xoffset i)) y) (array-element-type rbase))))))
                              (((list xdim)
                                _)
                               (dotimes (i xdim)
                                 (setf (aref rbase (+ roffset i))
                                       (%coerce (funcall function
                                                         (aref xbase (+ xoffset i))
                                                         (aref ybase (+ yoffset i)))
                                                (array-element-type rbase)))))
                              
                              (((list* 1    xrest)
                                (list* ydim yrest)
                                (list* _    rrest))
                               (iter (with yrest-size = (reduce #'* yrest))
                                     (with rrest-size = (reduce #'* rrest))
                                     (for i below ydim)
                                     (broadcast-core xoffset xrest
                                                     yoffset yrest
                                                     roffset rrest)
                                     (incf yoffset yrest-size)
                                     (incf roffset rrest-size)))

                              (((list* xdim xrest)
                                (list* 1    yrest)
                                (list* _    rrest))
                               (iter (with xrest-size = (reduce #'* xrest))
                                     (with rrest-size = (reduce #'* rrest))
                                     (for i below xdim)
                                     (broadcast-core xoffset xrest
                                                     yoffset yrest
                                                     roffset rrest)
                                     (incf xoffset xrest-size)
                                     (incf roffset rrest-size)))

                              (((list* xdim xrest)
                                (list* _    yrest)
                                (list* _    rrest))
                               (iter (with xrest-size = (reduce #'* xrest))
                                     (with yrest-size = (reduce #'* yrest))
                                     (with rrest-size = (reduce #'* rrest))
                                     (for i below xdim)
                                     (broadcast-core xoffset xrest
                                                     yoffset yrest
                                                     roffset rrest)
                                     (incf xoffset xrest-size)
                                     (incf yoffset yrest-size)
                                     (incf roffset rrest-size))))))
                   
                   (broadcast-core xoffset xshape
                                   yoffset yshape
                                   0       rshape)
                   (values r rbase)))))))))))

(define-compiler-macro broadcast (&whole whole function x y &key type (atomic function) &environment env)
  "For binary functions"
  (%when-function-is-global
    (once-only (x y type)
      (with-gensyms (r
                     x-type y-type
                     xshape yshape rshape
                     xbase ybase rbase
                     xoffset yoffset roffset
                     xdim ydim
                     xrest yrest rrest
                     xrest-size yrest-size rrest-size)
        `(cond
           ((and (not (arrayp ,x)) (not (arrayp ,y)))
            (funcall ,atomic ,x ,y))
           (t
            ;; example: 
            ;; x shape: (10 3 1 4)
            ;; y shape: (2 10 1 2 4)
            (let ((,x (if (arrayp ,x) ,x (full nil ,x)))
                  (,y (if (arrayp ,y) ,y (full nil ,y)))
                  (,x-type (if (arrayp ,x) (array-element-type ,x) (strict-type-of ,x)))
                  (,y-type (if (arrayp ,y) (array-element-type ,y) (strict-type-of ,y))))
              (declare (numcl-array ,x ,y))
              (declare (optimize (debug 3)))
              (assert (broadcast-p ,x ,y) nil
                      "Given arrays have incompatible shape for broadcasting the ~a op:~% ~a and ~a.~_ Arrays:~%~a~%~a"
                      ,fn (shape ,x) (shape ,y) ,x ,y)
              (let* ((,type (or ,type (infer-type ,fn ,x-type ,y-type))))
                (multiple-value-bind (,rshape ,xshape ,yshape) (broadcast-result-shape ,x ,y)
                  (multiple-value-bind (,r ,rbase) (empty ,rshape :type ,type)
                    (declare (base-array ,rbase))
                    (multiple-value-bind (,xbase ,xoffset) (array-displacement ,x)
                      (declare (base-array ,xbase))
                      (multiple-value-bind (,ybase ,yoffset) (array-displacement ,y)
                        (declare (base-array ,ybase))
                        (specializing (,xoffset ,yoffset ,xbase ,ybase ,rbase) ()
                          (labels ((broadcast-core (,xoffset ,xshape
                                                    ,yoffset ,yshape
                                                    ,roffset ,rshape)
                                     (match* (,xshape ,yshape ,rshape)
                                       (((list 1)
                                         (list ,ydim))
                                        (let ((,x (aref ,xbase ,xoffset)))
                                          (dotimes (i ,ydim)
                                            (setf (aref ,rbase (+ ,roffset i))
                                                  (%coerce (funcall ,fn ,x (aref ,ybase (+ ,yoffset i))) (array-element-type ,rbase))))))
                                       (((list ,xdim)
                                         (list 1))
                                        (let ((,y (aref ,ybase ,yoffset)))
                                          (dotimes (i ,xdim)
                                            (setf (aref ,rbase (+ ,roffset i))
                                                  (%coerce (funcall ,fn (aref ,xbase (+ ,xoffset i)) ,y) (array-element-type ,rbase))))))
                                       (((list ,xdim)
                                         _)
                                        (dotimes (i ,xdim)
                                          (setf (aref ,rbase (+ ,roffset i))
                                                (%coerce (funcall ,fn
                                                                  (aref ,xbase (+ ,xoffset i))
                                                                  (aref ,ybase (+ ,yoffset i)))
                                                         (array-element-type ,rbase)))))
                                       
                                       (((list* 1    ,xrest)
                                         (list* ,ydim ,yrest)
                                         (list* _    ,rrest))
                                        (iter (with ,yrest-size = (reduce #'* ,yrest))
                                              (with ,rrest-size = (reduce #'* ,rrest))
                                              (for i below ,ydim)
                                              (broadcast-core ,xoffset ,xrest
                                                              ,yoffset ,yrest
                                                              ,roffset ,rrest)
                                              (incf ,yoffset ,yrest-size)
                                              (incf ,roffset ,rrest-size)))

                                       (((list* ,xdim ,xrest)
                                         (list* 1    ,yrest)
                                         (list* _    ,rrest))
                                        (iter (with ,xrest-size = (reduce #'* ,xrest))
                                              (with ,rrest-size = (reduce #'* ,rrest))
                                              (for i below ,xdim)
                                              (broadcast-core ,xoffset ,xrest
                                                              ,yoffset ,yrest
                                                              ,roffset ,rrest)
                                              (incf ,xoffset ,xrest-size)
                                              (incf ,roffset ,rrest-size)))

                                       (((list* ,xdim ,xrest)
                                         (list* _    ,yrest)
                                         (list* _    ,rrest))
                                        (iter (with ,xrest-size = (reduce #'* ,xrest))
                                              (with ,yrest-size = (reduce #'* ,yrest))
                                              (with ,rrest-size = (reduce #'* ,rrest))
                                              (for i below ,xdim)
                                              (broadcast-core ,xoffset ,xrest
                                                              ,yoffset ,yrest
                                                              ,roffset ,rrest)
                                              (incf ,xoffset ,xrest-size)
                                              (incf ,yoffset ,yrest-size)
                                              (incf ,roffset ,rrest-size))))))
                            
                            (broadcast-core ,xoffset ,xshape
                                            ,yoffset ,yshape
                                            0       ,rshape)
                            (values ,r ,rbase)))))))))))))))

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

(defmacro define-simple-mapper (numcl-fn cl-fn)
  `(defun ,numcl-fn (x)
     (if (numberp x)
         (,cl-fn x)
         (let ((y (empty (shape x) :type (infer-type ',cl-fn (array-element-type x)))))
           (einsum '(i -> (,cl-fn $1) -> i)
                   (flatten x)
                   (flatten y))
           y))))

(define-simple-mapper numcl:sqrt sqrt)
(define-simple-mapper numcl:isqrt isqrt)
(define-simple-mapper numcl:sin sin)
(define-simple-mapper numcl:cos cos)
(define-simple-mapper numcl:tan tan)
(define-simple-mapper numcl:asin asin)
(define-simple-mapper numcl:acos acos)
(define-simple-mapper numcl:atan atan)
(define-simple-mapper numcl:sinh sinh)
(define-simple-mapper numcl:cosh cosh)
(define-simple-mapper numcl:tanh tanh)
(define-simple-mapper numcl:asinh asinh)
(define-simple-mapper numcl:acosh acosh)
(define-simple-mapper numcl:atanh atanh)
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
(define-simple-mapper numcl:integer-length integer-length)
;; (defun float          (x) (map-array 'float x))

(declaim (inline %square))
(defun %square (x) (* x x))
(define-simple-mapper numcl:square %square)


(declaim (inline %log2))
(defun %log2 (x) (log x 2))
(define-simple-mapper numcl:log2 %log2)


;; type upgrading. Why this is not available by default in common lisp??

(declaim (inline numcl:1+))
(defun numcl:1+ (array) (numcl:+ array 1))

(declaim (inline numcl:1-))
(defun numcl:1- (array) (numcl:- array 1))

;; TODO: optimizing broadcast ordering
;; TODO: optimize by reusing intermediate results
;; TODO: optimize by loop fusion

(declaim (inline numcl:+ numcl:- numcl:* numcl:/ numcl:max numcl:min numcl:clip))

(defun numcl:+   (&rest args) (reduce (lambda (x y) (broadcast '+ x y)) args :initial-value 0))
(defun numcl:*   (&rest args) (reduce (lambda (x y) (broadcast '* x y)) args :initial-value 1))
(defun numcl:max (&rest args) (reduce (lambda (x y) (broadcast 'max x y)) args))
(defun numcl:min (&rest args) (reduce (lambda (x y) (broadcast 'min x y)) args))

(defun numcl:expt (base power) (broadcast 'expt base power))

(defun numcl:-   (first &rest args)
  (if args
      (reduce (lambda (x y) (broadcast '- x y)) args :initial-value first)
      (broadcast '- 0 first)))
(defun numcl:/   (first &rest args)
  (if args
      (reduce (lambda (x y) (broadcast '/ x y)) args :initial-value first)
      (broadcast '/ 1 first)))

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

(declaim (inline =/2 /=/2 <=/2 >=/2 </2 >/2))
(declaim (inline boolean-wrapper))

(defun =/2  (x y) (broadcast '=/bit  x y :atomic #'= ))
(defun /=/2 (x y) (broadcast '/=/bit x y :atomic #'/=))
(defun <=/2 (x y) (broadcast '<=/bit x y :atomic #'<=))
(defun >=/2 (x y) (broadcast '>=/bit x y :atomic #'>=))
(defun </2  (x y) (broadcast '</bit  x y :atomic #'< ))
(defun >/2  (x y) (broadcast '>/bit  x y :atomic #'> ))

(defun boolean-wrapper (x y more binary multiary)
  (declare (function binary multiary))
  (let ((result (funcall binary x y)))
    (if more
        (let ((result2 (apply multiary y more)))
          (numcl:logand result result2))
        result)))

#+(or)
(defun numcl:=  (x y &rest more)
  (let ((result (funcall #'=/2 x y)))
    (if more
        (let ((result2 (apply #'numcl:= y more)))
          (numcl:logand result result2))
        result)))

(declaim (inline numcl:= numcl:/= numcl:<= numcl:>= numcl:< numcl:>))

(defun numcl:=  (x y &rest more) (boolean-wrapper x y more #'=/2  #'numcl:= ))
(defun numcl:/= (x y &rest more) (boolean-wrapper x y more #'/=/2 #'numcl:/=))
(defun numcl:<= (x y &rest more) (boolean-wrapper x y more #'<=/2 #'numcl:<=))
(defun numcl:>= (x y &rest more) (boolean-wrapper x y more #'>=/2 #'numcl:>=))
(defun numcl:<  (x y &rest more) (boolean-wrapper x y more #'</2  #'numcl:<))
(defun numcl:>  (x y &rest more) (boolean-wrapper x y more #'>/2  #'numcl:>))


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


