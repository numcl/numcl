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

;; for a 3D array a,

;; range
;; a[1:5,2,3]   = (aref a '(1 5) 2 3)
;; a[2,1:5,3]   = (aref a 2 '(1 5) 3)
;; a[2,1:2:5,3] = (aref a 2 '(1 2 5) 3)
;; a[2,1:,3]    = (aref a 2 '(1 t) 3)
;; a[2,:1,3]    = (aref a 2 '(t 1) 3)
;; a[2,:,3]     = (aref a 2 '(t t) 3)
;; a[2,:,3]     = (aref a 2    t   3)



;; insufficient axis
;; (aref a '(1 5)) == (aref a '(1 5) t t)
;; (aref a 2 '(1 5)) == (aref a 2 '(1 5) t)

;; newaxis
;; (aref a '(1 2 5) nil 2 3)

;; ellipsis
;; (aref a '- 2) = (aref a t t 2) = a[...,2]
;; (aref a 2 '-) = (aref a 2 t t) = a[2,...]
;; (aref a 2 '- 3) = (aref a 2 t 3) = a[2,...,3]
;; (aref a 2 3 '-) = (aref a 2 3 t) = a[2,3,...]

;; defstruct
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (sub (:constructor sub (start stop step width full contiguous singleton)))
    "A structure that represents the range of iteration.
FULL        When true, the entire elements in the axis is covered
CONTIGUOUS  When true, there are no gap in the range of iteration
SINGLETON   Differentiates the index (2 3) (== python [2:3]) and 2
"
    (start 0 :type fixnum :read-only t)
    (stop  0 :type fixnum :read-only t)
    (step  0 :type fixnum :read-only t)
    (width 0 :type fixnum :read-only t)
    (full nil :type boolean :read-only t)
    (contiguous nil :type boolean :read-only t)
    (singleton nil :type boolean :read-only t)))

(defun %normalize-subscript (subscript dim)
  "Returns a subscript object."
  (ematch subscript
    (t
     (sub 0 dim 1 dim t t nil))
    (nil
     (sub 0 dim 1 dim t t nil))
    ((number)
     (let ((subscript (mod subscript dim)))
       (sub subscript (1+ subscript) 1 1 (= dim 1) t t)))
    ;; aliasing
    ((list start)
     (%normalize-subscript `(,start ,dim 1) dim))
    ((list t t)
     (%normalize-subscript `(,0 ,dim 1) dim))
    ((list start t)
     (%normalize-subscript `(,start ,dim 1) dim))
    ((list t stop)
     (%normalize-subscript `(0 ,stop 1) dim))
    ((list start stop)
     (%normalize-subscript `(,start ,stop 1) dim))
    
    ((list start stop step)
     (let* ((start (if (minusp start) (1+ (mod stop dim)) (min start dim)))
            (stop  (if (minusp stop) (1+ (mod stop dim))  (min stop  dim)))
            (width  (ceiling (- stop start) step)))
       ;; step could be negative
       (sub start stop step width
            (= dim width)
            (or (= 1 step) (= 1 width))
            nil)))))

(defun normalize-subscripts (subscripts shape)
  "Normalizes the input for %aref.

      - corresponds to numpy's elipses `...`, and it should appear only once.
      t corresponds to the unspecified end in numpy's `:` . "
  (mapcar #'%normalize-subscript
          ;; address ellipses and missing dims
          (if-let ((pos (position 'numcl:- subscripts)))
              (progn
                (assert (= (count 'numcl:- subscripts) 1))
                (append (subseq subscripts 0 pos)
                        (make-list (- (length shape)
                                      (- (length subscripts) 1)) :initial-element t)
                        (subseq subscripts (1+ pos))))
            (if (< (length subscripts) (length shape))
                (append subscripts
                        (make-list (- (length shape)
                                      (length subscripts))
                                   :initial-element t))
                subscripts))
          shape))

(defun result-shape (subscripts)
  (mapcar #'sub-width (remove-if #'sub-singleton subscripts)))

#+(or)
(defun %contiguous-subscripts-p (subscripts)
  ;; this function is not used.
  ;; a contiguous region is defined by s*c?f* (if expressed in regexp)
  (labels ((single* (subscripts)
             (match subscripts
               ((list (sub :width (= 1)) rest)
                (single* rest))
               (_
                (contiguous subscripts))))
           (contiguous (subscripts)
             (match subscripts
               ((list (sub :contiguous t) rest)
                (every #'sub-full rest)))))
    (single* subscripts)))


(defun numcl:aref (array &rest subscripts)
  (ensure-singleton
   (let* ((shape (shape array))
          (subscripts (normalize-subscripts subscripts shape)))
     (reshape
      (%aref array subscripts)
      (result-shape subscripts)))))

(defun %aref (array subscripts)
  (ematch subscripts
    (nil
     array)
    ((list* (sub start stop step width full contiguous) rest)
     (cond
       ((= 1 width)
        ;; the shape is later collapsed in AREF
        (%aref (%displace-at array start) rest))
       ((and full (every #'sub-full rest))
        array)
       ((and contiguous (every #'sub-full rest))
        (%displace-range array start stop))
       (t
        (stack                         ; needs copying
         (iter (for i from start below stop by step)
               (collecting
                (%aref (%displace-at array i) rest)))))))))

(defun ensure-singleton (array-or-number)
  (if (zerop (array-rank array-or-number))
      ;; singleton array #0A<number>
      (aref array-or-number)
      array-or-number))

(defun (setf numcl:aref) (newvar array &rest subscripts)
  (let* ((shape (shape array))
         (type (array-element-type array))
         (subscripts (normalize-subscripts subscripts shape)))
    (if (arrayp newvar)
        (let* ((newvar (asarray newvar :type type))
               (value-shape (shape newvar))
               (range-shape (result-shape subscripts)))
          (assert (when-let ((pos (search (reverse value-shape)
                                          (reverse range-shape))))
                    (= 0 pos))
                  nil "The shape of the array of new elements:~% ~a~%~
                       The shape of the range specified by the subscript:~% ~a~%~
                       The tails of the subscripts do not match; broadcasting failed."
                  range-shape
                  value-shape)
          (%aset-replace newvar value-shape array subscripts))
        (%aset-fill (%coerce newvar type) array subscripts))))

(defun %aset-fill (newvar array subscripts)
  (ematch subscripts
    (nil
     (fill (flatten array) newvar))
    
    ((list* (sub start stop step contiguous) rest)
     (if (and contiguous (every #'sub-full rest)) ;;this also covers REST=NIL case
         (fill (flatten (%displace-range array start stop)) newvar)
         (iter (for i from start below stop by step)
               (%aset-fill newvar (%displace-at array i) rest))))))

(defun %aset-replace (newvar shape array subscripts)
  (flet ((replace! (a b)
           (assert (= (length a) (length b)))
           (replace a b)))
    (ematch subscripts
      (nil
       ;; This case only happens for rank-0 arrays e.g. (setf (aref #0A0) #0A1)
       (replace! (flatten array) (flatten newvar)))

      ((list* (sub start stop step contiguous singleton) rest)
       (cond
         (singleton
          (%aset-replace newvar shape (%displace-at array start) rest))
         
         ((equal (result-shape subscripts) shape)
          (if (and contiguous (every #'sub-full rest))
              (replace! (flatten (%displace-range array start stop))
                        (flatten newvar))
              (iter (for i from start below stop by step)
                    (for j from 0)
                    (%aset-replace (%displace-at newvar j) (cdr shape)
                                   (%displace-at array i) rest))))
         (t
          (iter (for i from start below stop by step)
                (%aset-replace newvar shape (%displace-at array i) rest))))))))

