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

(defun array-dimension* (array axes)
  "An extension of array-dimension that can handle multiple axes. --- WIP"
  ;; axis : int or list of ints
  (declare (array array)
           ((or fixnum list) axes))
  (the fixnum
       (if (listp axes)
           (iter (for axis in axes)
             (multiplying (array-dimension array axis)))
           (array-dimension array axes))))


(defun reduce-array (fn array &key (axes (iota (rank array))) type (initial-element 0))
  (ensure-singleton
   (ematch axes
     (nil
      (%reduce-array fn array axes type initial-element))
     ((fixnum)
      (%reduce-array fn array (list axes) type initial-element))
     ((type list)
      (%reduce-array fn array (sort (copy-list axes) #'<) type initial-element)))))

(defun %reduce-array-result-shape (array axes)
  (iter (for i from 0 below (array-rank array))
    (for j = (first axes))
    (if (and j (= i j))
        (pop axes)
        (collect (array-dimension array i)))))

(assert (equal '(5 8) (print (%reduce-array-result-shape (zeros '(4 5 6 7 8)) '(0 2 3)))))
(assert (equal '(5 6 8) (print (%reduce-array-result-shape (zeros '(4 5 6 7 8)) '(0 3)))))
(assert (equal nil (print (%reduce-array-result-shape (zeros '(4 5 6 7 8)) '(0 1 2 3 4)))))

(defun %reduce-array (fn array axes type initial-element)
  (let ((shape (%reduce-array-result-shape array axes)))
    (multiple-value-bind (result base-array) (full shape initial-element :type (float-substitution type :int-result 'fixnum))
      ;; I know this is super slow due to the compilation overhead, but this is
      ;; the most intuitive way to implement it
      (funcall (compile nil (with-gensyms (r a)
                              `(lambda (,r ,a)
                                 ,(make-reducer-lambda r a fn 0 (shape array) axes nil nil))))
               result
               array)
      (values result base-array))))

(defun make-reducer-lambda (rvar avar fn current-axis dims sum-axes loop-index sum-index)
  (match dims
    (nil
     `(setf (aref ,rvar ,@(reverse sum-index))
            (funcall ',fn
                     (aref ,rvar ,@(reverse sum-index))
                     (aref ,avar ,@(reverse loop-index)))))
    ((list* dim  dims)
     (with-gensyms (i)
       `(dotimes (,i ,dim)
          ,(if (member current-axis sum-axes)
               (make-reducer-lambda rvar avar fn (1+ current-axis) dims sum-axes (cons i loop-index) sum-index)
               (make-reducer-lambda rvar avar fn (1+ current-axis) dims sum-axes (cons i loop-index) (cons i sum-index))))))))

(print (make-reducer-lambda 'r 'a '+ 0 '(5 5 5 5 5) '(1 3) nil nil))

(defun numcl:sum  (array &key (axes (iota (rank array))) (type (array-element-type array))) (reduce-array '+   array :axes axes :type type))
(defun numcl:prod (array &key (axes (iota (rank array))) (type (array-element-type array))) (reduce-array '*   array :axes axes :type type :initial-element 1))
(defun numcl:max  (array &key (axes (iota (rank array))) (type (array-element-type array))) (reduce-array 'max array :axes axes :type type :initial-element (%minimum-value type)))
(defun numcl:min  (array &key (axes (iota (rank array))) (type (array-element-type array))) (reduce-array 'min array :axes axes :type type :initial-element (%maximum-value type)))

(defun %maximum-value (t1)
  (ematch t1
    ((or (ratio-type) (float-type))
     (case *numcl-default-float-format*
       (short-float most-positive-short-float)
       (single-float most-positive-single-float)
       (single-float most-positive-double-float)
       (long-float most-positive-long-float)))
    ((long-float-type)           most-positive-long-float)
    ((double-float-type)         most-positive-double-float)
    ((single-float-type)         most-positive-single-float)
    ((short-float-type)          most-positive-short-float)
    ((integer-subtype)           most-positive-fixnum)))

(defun %minimum-value (t1)
  (ematch t1
    ((or (ratio-type) (float-type))
     (case *numcl-default-float-format*
       (short-float most-negative-short-float)
       (single-float most-negative-single-float)
       (single-float most-negative-double-float)
       (long-float most-negative-long-float)))
    ((long-float-type)           most-negative-long-float)
    ((double-float-type)         most-negative-double-float)
    ((single-float-type)         most-negative-single-float)
    ((short-float-type)          most-negative-short-float)
    ((integer-subtype)           most-negative-fixnum)))


(defun numcl:mean  (array &key (axes (iota (rank array))) (type (array-element-type array)))
  (if (sequencep array)
      (alexandria:mean array)
      (numcl:/ (reduce-array '+ array :axes axes :type type)
               (array-dimension* array axes))))
(defun numcl:variance (array &key (axes (iota (rank array))) (type (array-element-type array)))
  (if (sequencep array)
      (alexandria:variance array)
      (numcl:/ (reduce-array '+ (square array) :axes axes :type type)
               (array-dimension* array axes))))
(defun numcl:standard-deviation (array &key (axes (iota (rank array))) (type (array-element-type array)))
  (if (sequencep array)
      (alexandria:standard-deviation array)
      (numcl:sqrt
       (numcl:/ (reduce-array '+ (square array) :axes axes :type type)
                (array-dimension* array axes)))))

;; unsure either CL idioms should supersede or numpy idioms should
;; (defun all (array &key axes type) (reduce-array '+ axes type))
;; (defun any (array &key axes type) (reduce-array '+ axes type))

;; memo: perhaps eye-based argmax is better

#+(or)
(defun argmax (array &key (axis 0) (type (array-element-type array)))
  (print array)
  (print (numcl:max array :axes axis :type type))
  (print (expand-dims (numcl:max array :axes axis :type type) axis))
  (print (numcl:= array
                  (expand-dims (numcl:max array :axes axis :type type) axis)))
  (print (arange (elt (shape array) axis)))
  ;; (print (expand-dims (arange (elt (shape array) axis)) axis))
  (numcl:sum (numcl:* (arange (elt (shape array) axis))
                      (numcl:= array
                               (expand-dims (numcl:max array :axes axis :type type) axis)))
             :axes axis))
  
  
#+(or)
(defun argmin (array &key axes type)
  (reduce-array '+ axes type))
