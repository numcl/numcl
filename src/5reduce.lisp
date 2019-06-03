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

(defun %reduce-array-result-type (array fn)
  (float-substitution
   (infer-type
    fn
    (array-element-type array)
    (array-element-type array))
   :int-result 'fixnum))

(defun reduce-array (fn array &key
                                axes
                                (type
                                 (%reduce-array-result-type array fn))
                                (initial-element
                                 (zero-value type)))
  (let* ((axes
          (etypecase axes
            (null
             (iota (rank array)))
            (fixnum
             (list axes))
            (list
             (sort (copy-list axes) #'<))))
         (remaining-axes
          (sort (set-difference (iota (rank array)) axes) #'<))
         (result-shape (mapcar (lambda (d) (array-dimension array d)) remaining-axes))
         (result (full result-shape initial-element :type type)))
    
    (funcall (compile nil (reduce-lambda (rank array) axes))
             result
             array
             fn)
    (ensure-singleton result)))

(defun reduce-lambda (rank axes)
  (with-gensyms (rvar avar fnvar)
    (let ((dims-vars (make-gensym-list rank "?")))
      `(lambda (,rvar ,avar ,fnvar)
         (resolving
           (declare (gtype (array * ,dims-vars) ,avar))
           ,(%reduce-lambda rvar avar fnvar 0 dims-vars axes nil nil))))))

(defun %reduce-lambda (rvar avar fnvar current-axis dims sum-axes loop-index sum-index)
  (match dims
    (nil
     `(setf (aref ,rvar ,@(reverse sum-index))
            (funcall ,fnvar
                     (aref ,rvar ,@(reverse sum-index))
                     (aref ,avar ,@(reverse loop-index)))))
    ((list* dim  dims)
     (with-gensyms (i)
       `(dotimes (,i ,dim)
          ,(if (member current-axis sum-axes)
               (%reduce-lambda rvar avar fnvar (1+ current-axis) dims sum-axes (cons i loop-index) sum-index)
               (%reduce-lambda rvar avar fnvar (1+ current-axis) dims sum-axes (cons i loop-index) (cons i sum-index))))))))

#+(or)
(print (reduce-lambda 5 '(1 3)))

(declaim (inline numcl:sum
                 numcl:prod
                 numcl:amax
                 numcl:amin
                 numcl:mean
                 numcl:variance
                 numcl:standard-deviation
                 numcl:avg
                 numcl:var
                 numcl:stdev))

(defun numcl:sum  (array &rest args &key axes type)
  (declare (ignorable axes type))
  (apply #'reduce-array '+ array args))
(defun numcl:prod (array &rest args &key axes type)
  (declare (ignorable axes type))
  (apply #'reduce-array '* array
         :initial-element
         (one-value
          (%reduce-array-result-type array '*))
         args))
(defun numcl:amax (array &rest args &key axes type)
  (declare (ignorable axes type))
  (apply #'reduce-array 'max array
         :initial-element
         (most-negative-value
          (%reduce-array-result-type array 'max))
         args))
(defun numcl:amin (array &rest args &key axes type)
  (declare (ignorable axes type))
  (apply #'reduce-array 'min array
         :initial-element
         (most-positive-value
          (%reduce-array-result-type array 'min))
         args))

(defun numcl:mean (array &key axes)
  (if (typep array 'sequence)
      (alexandria:mean array)
      (let ((axes (or axes (iota (rank array)))))
        (numcl:/ (numcl:sum array :axes axes)
                 (array-dimension* array axes)))))
(defun numcl:variance (array &key axes)
  (if (typep array 'sequence)
      (alexandria:variance array)
      (let ((axes (or axes (iota (rank array)))))
        (numcl:/ (numcl:sum (square array) :axes axes)
                 (array-dimension* array axes)))))
(defun numcl:standard-deviation (array &key axes)
  (if (typep array 'sequence)
      (alexandria:standard-deviation array)
      (numcl:sqrt
       (let ((axes (or axes (iota (rank array)))))
         (numcl:/ (numcl:sum (square array) :axes axes)
                  (array-dimension* array axes))))))

;; aliases
(defun numcl:avg (array &key axes)
  (if (typep array 'sequence)
      (alexandria:mean array)
      (let ((axes (or axes (iota (rank array)))))
        (numcl:/ (numcl:sum array :axes axes)
                 (array-dimension* array axes)))))
(defun numcl:var (array &key axes)
  (if (typep array 'sequence)
      (alexandria:variance array)
      (let ((axes (or axes (iota (rank array)))))
        (numcl:/ (numcl:sum (square array) :axes axes)
                 (array-dimension* array axes)))))
(defun numcl:stdev (array &key axes)
  (if (typep array 'sequence)
      (alexandria:standard-deviation array)
      (numcl:sqrt
       (let ((axes (or axes (iota (rank array)))))
         (numcl:/ (numcl:sum (square array) :axes axes)
                  (array-dimension* array axes))))))

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







(defun numcl:histogram (array &key (low (amin array)) (high (amax array)) (split 1))
  "Returns a fixnum vector representing a histogram of values.
The interval between LOW and HIGH are split by STEP value.
All values less than LOW are put in the 0-th bucket;
All values greater than equal to HIGH are put in the last bucket."
  (let* ((size (floor (/ (- high low) split)))
         (histogram (zeros (+ 2 size) :type 'fixnum))
         ;; full width + 1 (all values above the lowest interval) + 1 (all values above the highest interval)
         (index (clip (numcl:1+ (numcl:floor (numcl:- array low) split)) 0 (1+ size))))
    (numcl:map nil
               (lambda (i)
                 (incf (aref histogram i)))
               index)
    histogram))



