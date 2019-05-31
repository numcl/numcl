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


;; dot(a, b[, out]) 	Dot product of two arrays.
;; linalg.multi_dot(arrays) 	Compute the dot product of two or more arrays in a single function call, while automatically selecting the fastest evaluation order.
;; vdot(a, b) 	Return the dot product of two vectors.
;; inner(a, b) 	Inner product of two arrays.
;; outer(a, b[, out]) 	Compute the outer product of two vectors.
;; matmul(a, b[, out]) 	Matrix product of two arrays.
;; tensordot(a, b[, axes]) 	Compute tensor dot product along specified axes for arrays >= 1-D.
;; einsum(subscripts, *operands[, out, dtype, …]) 	Evaluates the Einstein summation convention on the operands.
;; einsum_path(subscripts, *operands[, optimize]) 	Evaluates the lowest cost contraction order for an einsum expression by considering the creation of intermediate arrays.
;; linalg.matrix_power(a, n) 	Raise a square matrix to the (integer) power n.
;; kron(a, b) 	Kronecker product of two arrays.


;; not supporting ellipses for now.
;; docstring:
;; The symbol - has a special meaning that allows broadcasting,
;;  and can be used only once in each SPEC, similar to Numpy's ellipses (...).

(defun einsum (subscripts &rest args)
  "Performs Einstein's summation.
SUBSCRIPTS is a sequence of the form (<SPEC>+ [-> <SPEC>*]).
The first set of specs specifies the input subscripts, and the last spec specifies the output subscripts.

Each SPEC is an alphabetical string designator, such as a symbol IJK, where each alphabet is considered as an index.
The shape of the input array should unify against the spec. For example,
with a spec IJI, the input array should be of rank 3 as well as
the 1st and the 3rd dimensions of the input array should be the same.

Performace tips: If SUBSCRIPTS is a constant, the compiler macro
builds an iterator function and make them inlined. Otherwise,
a new function is made in each call to einsum, resulting in a huge bottleneck.

"
  (apply (compile nil (einsum-lambda subscripts))
         args))

(define-compiler-macro einsum (&whole whole subscripts &rest args)
  (match subscripts
    ((list 'quote subscripts)
     `(funcall ,(einsum-lambda subscripts)
               ,@args))
    (_
     whole)))

(defpackage "NUMCL.SPEC" (:use))

(defun explode-spec (s)
  "string-designator -> a list of symbols, whose name is appended ? in the beginning.
E.g. 'aaa -> '(?a ?a ?a).
The symbols are interned in NUMCL.SPEC package.
"
  ;; The reason for adding ?-mark is to make it recognized as a gtype parameter.
  (iter (for c in-vector (symbol-name s))
        (for str = (make-string 2))
        (setf (aref str 0) #\?)
        (setf (aref str 1) c)
        (collecting
         (intern str :numcl.spec))))

#+(or)
(explode-spec 'aaa)

(defun einsum-lambda (subscripts)
  "Parses SUBSCRIPTS (<SPEC>+ [-> <SPEC>*]) and returns a lambda form that iterates over it."
  (let* ((pos (position :-> subscripts :test #'string=))
         (subscripts (mapcar #'explode-spec subscripts))
         (i-specs (subseq subscripts 0 pos))
         (i-flat (remove-duplicates (flatten i-specs)))
         (o-specs (if pos
                      (or (subseq subscripts (1+ pos))
                          '(nil))
                      (list
                       (sort i-flat #'string<))))
         (o-flat (remove-duplicates (flatten o-specs)))
         (i-vars
          (make-gensym-list (length i-specs) "I"))
         (o-vars
          (make-gensym-list (length o-specs) "O"))
         (iter-specs
          (union i-flat o-flat)))
    (assert (subsetp o-flat i-flat)
            nil
            "The output spec contains ~a which are not used in the input specs"
            (set-difference o-flat i-flat))
    (with-gensyms (type)
      `(lambda ,i-vars
         (resolving
           ,@(iter (for var in i-vars)
                   (for spec in i-specs)
                   (collecting
                    `(declare (gtype (array * ,spec) ,var))))
           (let* ((,type (apply #'union-to-float-type
                                (mapcar #'array-element-type
                                        (list ,@i-vars))))
                  ,@(iter (for o-var in o-vars)
                          (for o-spec in o-specs)
                          (collecting
                           `(,o-var (zeros (list ,@o-spec)
                                           :type ,type)))))
             ,@(einsum-body-iter iter-specs i-specs o-specs i-vars o-vars)
             (values ,@(mapcar (lambda (var) `(ensure-singleton ,var))
                               o-vars))))))))

(defun einsum-body-iter (iter-specs i-specs o-specs i-vars o-vars)
  (match iter-specs
    (nil
     (iter (for o-var in o-vars)
           (for o-spec in o-specs)
           (collecting
            `(incf (aref ,o-var ,@o-spec)
                   (* ,@(iter (for i-var in i-vars)
                              (for i-spec in i-specs)
                              (collecting
                               `(aref ,i-var ,@i-spec))))))))
    ((list* ?s rest)
     `((dotimes (,?s ,?s)
         ,@(einsum-body-iter rest i-specs o-specs i-vars o-vars))))))


#+(or)
(einsum-lambda '(i i))
;; = (einsum-lambda '(i i -> i))
;; == inner

#+(or)
(einsum-lambda '(i))
;; = (einsum-lambda '(i -> i))
;; == identity

#+(or)
(einsum-lambda '(ij))
;; = (einsum-lambda '(ij -> ij))
;; == identity

#+(or)
(einsum-lambda '(ji))
;; = (einsum-lambda '(ji -> ij))
;; == transpose

;; (einsum-lambda '(ij -> ))
;; (einsum-lambda '(ij -> nil))

#+(or)
(einsum-lambda '(ik kj))
#+(or)
(einsum-lambda '(ik kj -> ))

#+(or)    
(einsum '(ik kj) a b)

