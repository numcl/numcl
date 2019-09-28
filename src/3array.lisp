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

;; microbenchmark
#+(or)
(let ((a (zeros '(10000 10000) :type 'single-float)))
  (time (asarray a :type 'fixnum)))

(defun asarray (contents &key type)
  #.*asarray-documentation*
  (when (subtypep 'array type)
    (assert (subtypep 'vector type)))
  (identity
   ;; ensure-singleton
   (multiple-value-bind (shape type) (determine-array-spec contents type)
     (cond
       ((and (arrayp contents)
             (equal (shape contents) shape))
        (multiple-value-bind (a base) (%make-array shape :element-type type)
          (if (numcl-array-p contents)
              ;; flattening is available only for numcl arrays.
              (multiple-value-bind (base2 offset) (array-displacement contents)
                (declare (index offset))
                (let ((end (+ offset (array-total-size contents))))
                  (declare (index end))
                  (specializing (base2 base) ()
                    (do ((i offset (1+ i))
                         (j 0      (1+ j)))
                        ((= i end))
                    (setf (aref base j)
                          (%coerce (aref base2 i)
                                   ;; (array-element-type base) can't be replaced by the variable TYPE,
                                   ;; which precludes constant propagation.
                                   (array-element-type base)))))))
              (dotimes (i (array-total-size contents))
                (setf (aref base i)
                      (%coerce (row-major-aref contents i)
                               (array-element-type base)))))
          (values a base)))
       ((typep contents 'sequence)
        (multiple-value-bind (a base) (%make-array shape :element-type type)
          (%nested-coerce-and-insert base contents type (length shape))
          (values a base)))
       (t
        (multiple-value-bind (a base) (%make-array nil :element-type type)
          (setf (aref base 0) (%coerce contents type))
          (values a base)))))))

(defun infer-type-from-contents (contents)
  (if (every #'numberp contents)
      (if (every #'realp contents)
          (let ((type (reduce #'float-contagion contents :key #'strict-type-of)))
            (list type
                  (coerce (reduce #'min contents) type)
                  (coerce (reduce #'max contents) type)))
          (let ((contents (map 'simple-vector
                               (lambda (x)
                                 (typecase x
                                   (rational (complex (coerce x *numcl-default-float-format*)))
                                   (float    (complex x))
                                   (complex  (coerce x `(complex ,*numcl-default-float-format*)))))
                               contents))
                (type (reduce #'float-contagion contents :key #'strict-type-of)))
            (list 'complex
                  (list type
                        (coerce (min (reduce #'min contents :key #'realpart)
                                     (reduce #'min contents :key #'imagpart))
                                type)
                        (coerce (max (reduce #'max contents :key #'realpart)
                                     (reduce #'max contents :key #'imagpart))
                                type)))))
      t))

(defun infer-type-from-content (x)
  (if (numberp x)
      (if (realp x)
          ;; without this mechanism, any number can return a larger subtype, e.g. 5 -> (unsigned-byte 62)
          (let ((type (float-substitution (strict-type-of x))))
            (list type
                  (coerce x type)
                  (coerce x type)))
          (etypecase x
            ((complex float)
             (let ((type (float-substitution (strict-type-of x))))
               `(complex (,type
                          ,(coerce (min (realpart x) (imagpart x)) type)
                          ,(coerce (max (realpart x) (imagpart x)) type)))))
            
            ((complex rational)
             (let ((type *numcl-default-float-format*))
               `(complex (,type
                          ,(coerce (min (realpart x) (imagpart x)) type)
                          ,(coerce (max (realpart x) (imagpart x)) type)))))))
      t))

(defun determine-array-spec (contents type)
  "Walk over the CONTENTS recursively and decides the output shape and the element type.
When TYPE is non-nil, it overrides the type deduction."
  (cond
    ((and (arrayp contents)
          (> (rank contents) 1))
     
     (values (array-dimensions contents)
             (if type
                 type
                 (array-element-type contents))))

    ((and (subtypep type 'vector)
          (typep contents type)
          (notevery (of-type type) contents))
     (values nil
             (if type
                 type
                 (strict-type-of contents))))
    
    ((typep contents 'sequence)
     (if (every (lambda (x) (typep x 'sequence)) contents)

         (iter (for x in-sequence contents)
               (with shape*       = nil)
               (with inconsistent = nil)
               (with types        = nil)
               (for (values shape deduced-type) = (determine-array-spec x type))
               (unless type
                 (push deduced-type types))
               (setf shape*
                     (if (first-time-p)
                         shape
                         (iter (for i1 in shape*)
                               (for i2 in shape)
                               ;; (while (= i1 i2))
                               (when (/= i1 i2)
                                 (setf inconsistent t)
                                 (finish))
                               ;; collect only up to the largest common prefix
                               (collect i1))))
               (finally
                (return
                  (values (list* (length contents) shape*)
                          (if type
                              type
                              (if inconsistent
                                  t
                                  (if (every #'number-subtype-p types)
                                      (reduce #'bind-to-float-type types)
                                      (if (every #'equal types (rest types)) ; compare adjacent
                                          (first types)
                                          t))))))))

         (values (list (length contents))
                 (if type
                     type
                     (if (= 1 (length contents))
                         (infer-type-from-content (elt contents 0))
                         (infer-type-from-contents contents))))))
    (t
     (values nil
             (if type
                 type
                 (infer-type-from-content contents))))))

;; (print (multiple-value-list (determine-array-spec 1)))
;; 
;; (print (multiple-value-list (determine-array-spec '(3 5))))
;; 
;; (print (multiple-value-list (determine-array-spec '((1 2) (2 3)))))
;; 
;; (print (multiple-value-list (determine-array-spec '(((1 2)) ((2 3))))))
;; 
;; (print (multiple-value-list (determine-array-spec '(((1 2)) ((2 3.0))))))
;; 
;; (print (multiple-value-list (determine-array-spec '((((1 2))) (((2 3 4)))))))
;; 
;; (print (multiple-value-list (determine-array-spec '((1 2) (2 3 4)))))
;; 
;; (print (multiple-value-list (determine-array-spec '(("aaa" "aaa") ("aaa" "aaa")))))
;; ;; just numbers at the moment

(defun %nested-coerce-and-insert (base-array contents newtype level)
  (let ((index 0))
    (labels ((rec (level contents)
               (if (= 0 level)
                   (setf (aref base-array index) (%coerce contents newtype)
                         index (1+ index))
                   (iter (for e in-sequence contents)
                         (rec (1- level) e)))))
      (rec level contents))))


;; (describe (as-array '((2 2) (1 1.0))))

