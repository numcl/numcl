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

;; size-increasing operations

;; concatenate

(defun numcl:concatenate (arrays &key (axis 0) out)
  (let* ((type (reduce #'union-to-float-type arrays :key #'array-element-type))
         (dims1 (shape (first arrays)))
         (axis (mod axis (length dims1)))
         (out (or out
                  (empty (append (subseq dims1 0 axis)
                                 (list (reduce #'+ arrays :key (lambda (array) (array-dimension array axis))))
                                 (subseq dims1 (1+ axis)))
                         :type type)))
         (arrays-simplified
          (iter (for a in arrays)
                (for dims = (shape a))
                (collecting
                 (reshape a (list (reduce #'* dims :start 0 :end axis)
                                  (elt dims axis)
                                  (reduce #'* dims :start (1+ axis)))))))
         (dims (shape out))
         (dims-before (reduce #'* dims :start 0 :end axis))
         (dims-after  (reduce #'* dims :start (1+ axis)))
         (simplified-dims (list dims-before
                                (elt dims axis)
                                dims-after))
         (out-simplified
          (reshape out simplified-dims)))

    
    (iter (for i below (rank (first arrays)))
          (unless (= i axis)
            (iter (for array1 in arrays)
                  (for dim1 = (array-dimension array1 i))
                  (for dim2 previous dim1)
                  (when dim2
                    (assert (= dim1 dim2))))))

    (iter (for array in arrays-simplified)
          (for dim-axis = (array-dimension array 1))
          (setf (numcl:aref out-simplified t (list sum (+ sum dim-axis)) t)
                array)
          (summing dim-axis into sum))
    out))

;; (concatenate (list (zeros '(2 2)) (ones '(2 2))))
;; (concatenate (list (zeros '(2 2)) (ones '(2 2))) :axis 0)
;; (concatenate (list (zeros '(2 2)) (ones '(2 2))) :axis 1)
;; (concatenate (list (zeros '(2 2)) (ones '(2 2))) :axis 2)

;; stack

(defun stack (arrays &key (axis 0) out)
  (let* ((type (reduce #'union-to-float-type arrays :key #'array-element-type))
         (dims1 (shape (first arrays)))
         (len (length arrays))
         ;; if len = 5, dims1 = '(2 2 2) and axis = -1 -> axis = 3, dims = '(2 2 2 5)
         ;; if len = 5, dims1 = '()      and axis = -1 -> axis = 0, dims = '(5)
         (axis (mod axis (1+ (length dims1))))
         (dims (append (subseq dims1 0 axis)
                       (list len)
                       (subseq dims1 axis)))
         (out (or out
                  (empty dims :type type)))
         (simplified-dims
          (list (reduce #'* dims1 :start 0 :end axis)
                (reduce #'* dims1 :start axis)))
         (arrays-simplified
          (iter (for a in arrays)
                (collecting
                 (reshape a simplified-dims))))
         (dims-before (reduce #'* dims :start 0 :end axis))
         (dims-after  (reduce #'* dims :start (1+ axis)))
         (simplified-dims (list dims-before
                                len
                                dims-after))
         (out-simplified
          (reshape out simplified-dims)))

    (iter (for i below (rank (first arrays)))
          (unless (= i axis)
            (iter (for array1 in arrays)
                  (for dim1 = (array-dimension array1 i))
                  (for dim2 previous dim1)
                  (when dim2
                    (assert (= dim1 dim2))))))
    
    (iter (for array in arrays-simplified)
          (for dim-axis from 0)
          (setf (numcl:aref out-simplified t dim-axis t)
                array)
          (summing dim-axis into sum))
    out))

;; (stack (list (zeros '(2 2)) (ones '(2 2))))
;; (stack (list (zeros '(2 2)) (ones '(2 2))) :axis 0)
;; (stack (list (zeros '(2 2)) (ones '(2 2))) :axis 1)
;; (stack (list (zeros '(2 2)) (ones '(2 2))) :axis 2)


;; different
#+(or)
(defun repeat (array n &key axis)
  (if (arrayp array)
      (if axis
          (concatenate (make-list n :initial-element array) :axis axis)
          (flatten
           (concatenate (make-list n :initial-element (reshape array `(,@(shape array) -1))) :axis -1)))
      (progn
        (assert (null axis))
        (full n array))))

;; unstack

(defun unstack (array &key (axis 0))
  (let* ((type (array-element-type array))
         (dims (shape array))
         (axis (mod axis (length dims)))
         (len  (elt dims axis))
         
         (dims-before (reduce #'* dims :start 0 :end axis))
         (dims-after  (reduce #'* dims :start (1+ axis)))
         
         (simplified
          (reshape array (list dims-before
                               len
                               dims-after)))
         (tmp
          (empty (list len
                       dims-before
                       dims-after)
                 :type type))
         
         (dims-each (append (subseq dims 0 axis)
                            (subseq dims (1+ axis))))
         (arrays
          (iter (for i from 0 below len)
                (collecting
                 (%make-array dims-each
                             :element-type type
                             :displaced-to tmp
                             :displaced-index-offset (* i dims-before dims-after))))))

    
    (iter (for dim-axis below len)
          (setf (numcl:aref tmp dim-axis)
                (numcl:aref simplified t dim-axis t)))

    arrays))


;; (unstack (stack (list (zeros '(2 2)) (ones '(2 2)))))
;; (unstack (stack (list (zeros '(2 2)) (ones '(2 2))) :axis 0))
;; (unstack (stack (list (zeros '(2 2)) (ones '(2 2))) :axis 1))
;; (unstack (stack (list (zeros '(2 2)) (ones '(2 2))) :axis 2))
