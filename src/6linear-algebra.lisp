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

(declaim (inline einsum))
(defun einsum (subscripts &rest args)
  "Performs Einstein's summation.
SUBSCRIPTS is a sequence of the form (<SPEC>+ [-> [<SPEC>*]]).
The first set of specs before the `->` symbol specifies the input subscripts,
and the second set of specs after `->` symbol specifies the output subscripts.
Unlike Numpy, there can be multiple output subscripts
and it performs two operations in the same loop, then return multiple values.
The symbol `->` can be a string and can belong to any package
 because it is compared by STRING=.

Each SPEC is an alphabetical string designator, such as a symbol IJK or a string \"IJK\",
 where each alphabet is considered as an index. It signals a type-error when it contains any
non-alpha char. Alternatively, each SPEC can be a list that contains a list of symbols.
Note that a symbol NIL is interpreted as an empty list rather than N, I and L.

The remaining arguments ARGS contains the input arrays and optionally the output arrays.
The shape of each input array should unify against the corresponding input spec. For example,
with a spec IJI, the input array should be of rank 3 as well as
the 1st and the 3rd dimensions of the input array should be the same.

The shape of each output array is determined by the corresponding output spec.
For example, if SUBSCRIPTS is '(ij jk -> ik), the output is an array of rank 2,
and the output shape has the same dimension as the first input in the first axis,
and the same dimension as the second input in the second axis.

If provided, output array shapes and types are also checked against the
corresponding output spec.  The types should match the result of the numerical
operations on the elements of the input arrays.

The output value is calculated in the following rule.
+ First, the output array is initialized by 0.
+ Einsum nests one loop for each index in the input specs.
  For example, '(ij jk -> ik) results in a triple loop.
+ When an index used in the input spec is missing in the output spec,
  the axis is aggregated over the iteration by summation.
+ If the same index appears multiple times in a single spec,
  they share the same value in each iteration.
  For example, '(ii -> i) returns the diagonal element of the matrix.
+ If the same index appears across the different input specs,
  the element values from the multiple input arrays are aggregated by multiplication.
  For example, '(ij jk -> ik) will perform
  (setf (aref a2 i k) (* (aref a0 i j) (aref a1 j k)))
  when a0, a1 are the input arrays and a2 is the output array.

When the separator -> is omitted, a single output is assumed and its spec is
an alphabetically sorted indices of the union of the input specs.
For example, '(ij jk) is equivalent to '(ij jk -> ijk), therefore
(einsum '(ij jk) a b) is equivalent to :

 (dotimes (i <max> output)
   (dotimes (j <max>)
     (dotimes (k <max>)
       (setf (aref output i j k) (* (aref a i j) (aref b j k))))))

Performace tips: If SUBSCRIPTS is a constant, the compiler macro
builds an iterator function and make them inlined. Otherwise,
a new function is made in each call to einsum, resulting in a huge bottleneck.

The nesting order of the loops depends on the spec.
The nesting order affects the memory access pattern and therefore the performance due to
the access locality. For example, when writing a GEMM which accesses three matrices
by (setf (aref output i j) (* (aref a i k) (aref b k j))),
it is well known that ikj-loop is the fastest among other loops, e.g. ijk-loop.
EINSUM reorders the indices so that it maximizes the cache locality.
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

(defun to-spec (character)
  (assert (alpha-char-p character) nil
          'simple-type-error
          :format-control "Tried to make a spec from a non-alpha char ~a"
          :format-arguments (list character))
  (let ((str (make-string 2)))
    (setf (aref str 0) #\?)
    (setf (aref str 1) character)
    (intern str :numcl.spec)))

(defun explode-spec (s)
  "string-designator -> a list of symbols, whose name is appended ? in the beginning.
E.g. 'aaa -> '(?a ?a ?a).
The symbols are interned in NUMCL.SPEC package.
"
  ;; The reason for adding ?-mark is to make it recognized as a gtype parameter.
  (typecase s
    (list
     (assert (every #'symbolp s))
     (mapcar (curry #'symbolicate "?") s))
    (symbol
     (let ((name (symbol-name s)))
       (iter (for c in-vector name)
             (collecting
              (to-spec c)))))))

#+(or)
(explode-spec 'aaa)

(defun einsum-lambda (subscripts)
  "Parses SUBSCRIPTS (<SPEC>+ [-> <SPEC>*]) and returns a lambda form that iterates over it."
  (let* ((pos (position '-> subscripts :test
                        (lambda (a b) (and (typep a 'string-designator)
                                           (typep b 'string-designator)
                                           (string= a b)))))
         (subscripts (mapcar #'explode-spec
                             (remove '-> subscripts :test
                                     (lambda (a b) (and (typep a 'string-designator)
                                                        (typep b 'string-designator)
                                                        (string= a b))))))
         (i-specs (subseq subscripts 0 pos))
         (i-flat (remove-duplicates (flatten i-specs)))
         (o-specs (if pos
                      (or (subseq subscripts pos)
                          '(nil))
                      (list
                       (sort i-flat #'string<))))
         (o-flat (remove-duplicates (flatten o-specs)))
         (i-vars
          (make-gensym-list (length i-specs) "I"))
         (o-vars
          (make-gensym-list (length o-specs) "O"))
         (iter-specs
          (sort-locality (union i-flat o-flat) subscripts)))
    (assert (subsetp o-flat i-flat)
            nil
            "The output spec contains ~a which are not used in the input specs:~% input spec: ~a~%output spec: ~a"
            (set-difference o-flat i-flat) i-flat o-flat)
    (with-gensyms (type)
      `(lambda (,@i-vars &optional ,@o-vars)
         (resolving
           ,@(iter (for var in i-vars)
                   (for spec in i-specs)
                   (collecting
                    `(declare (gtype (array * ,spec) ,var))))
           (let* ((,type (union-to-float-type
                          ,@(mapcar (curry #'list 'array-element-type)
                                    i-vars)))
                  ,@(iter (for o-var in o-vars)
                          (for o-spec in o-specs)
                          (collecting
                           `(,o-var (or ,o-var (zeros (list ,@o-spec)
                                                      :type ,type))))))
             (resolving
               ,@(iter (for var in o-vars)
                       (for spec in o-specs)
                       (collecting
                        `(declare (gtype (array * ,spec) ,var))))
               ,@(einsum-body-iter iter-specs i-specs o-specs i-vars o-vars)
               (values ,@(mapcar (lambda (var) `(ensure-singleton ,var))
                                 o-vars)))))))))

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

(defun sort-locality (indices subscripts)
  (sort indices
        (lambda (index1 index2)
          (locality< index1 index2 subscripts))))

(defun locality< (index1 index2 subscripts)
  "Returns true when index1 is less local than index2; i.e. index2 is more local"
  (assert (not (eq index1 index2)))
  (flet ((score (index1 index2)
           ;; the number of ordering violations
           ;; for looping index1 first 
           (iter (for spec in subscripts)
                 (for pos1 = (position index1 spec))
                 (for pos2 = (position index2 spec))
                 (counting
                  ;; count the violation
                  (and pos1 pos2 (< pos2 pos1))))))
    (< (score index1 index2)
       (score index2 index1))))

;; (einsum '(ij jk -> ik) a b)             ; -> becomes an ijk loop
;; (einsum '(ik kj -> ij) a b)             ; -> becomes an ikj loop
