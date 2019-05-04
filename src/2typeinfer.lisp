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

(lisp-namespace:define-namespace inferer (or symbol function) nil "namespace for type inference")

(defun set-type-inferer (name fname)
  (setf (symbol-inferer name)
        fname))

(defun infer-type (name &rest args)
  (if (inferer-boundp name)
      (apply (symbol-inferer name) args)
      (progn
        (warn "Missing type inference function for ~a, defaults to T" name)
        t)))



(set-type-inferer
 '+
 (defun add-to-float-type (&rest typespecs)
   (infer-rational-arithmetic-result #'interval-add typespecs 'integer)))

(set-type-inferer
 '-
 (defun sub-to-float-type (&rest typespecs)
   (infer-rational-arithmetic-result #'interval-sub typespecs 'integer)))

(set-type-inferer
 '*
 (defun mul-to-float-type (&rest typespecs)
   (infer-rational-arithmetic-result #'interval-mul typespecs 'integer)))

;; division may result in ratios; we coerce it into *numcl-default-float-format*

(set-type-inferer
 '/
 (defun div-to-float-type (&rest typespecs)
   (infer-rational-arithmetic-result #'interval-div typespecs *numcl-default-float-format*)))

(set-type-inferer
 'max
 (defun max-to-float-type (&rest typespecs)
   (infer-rational-arithmetic-result #'interval-max typespecs 'integer)))

(set-type-inferer
 'min
 (defun min-to-float-type (&rest typespecs)
   (infer-rational-arithmetic-result #'interval-min typespecs 'integer)))

(set-type-inferer
 'cos
 (defun cos-inferer (x)
   (ematch x
     ((or (real-subtype _ '*)
          (real-subtype '* _))
      `(,%float%
        ,(coerce -1 %float%)
        ,(coerce 1 %float%)))
     ((real-subtype low high)
      (if (< (* 2 pi) (- high low))
          `(,%float%
            ,(coerce -1 %float%)
            ,(coerce 1 %float%))
          (let* ((high (mod high (* 2 pi)))
                 (low  (mod low  (* 2 pi)))
                 (low  (if (< high low) (- low (* 2 pi)) low)))
              
            `(,%float%
              ,(coerce
                (if (<= low pi high)
                    -1
                    (min (cos low) (cos high)))
                %float%)
              ,(coerce
                (if (<= low 0 high)
                    1
                    (max (cos low) (cos high)))
                %float%)))))
     ((complex-type)
      ;; TBD
      'complex)
     ((or-type types)
      (reduce #'union-to-float-type types :key #'cos-inferer))
     ((and-type types)
      (reduce #'intersection-to-float-type types :key #'cos-inferer)))))

(set-type-inferer
 'sin
 (defun sin-inferer (x)
   (ematch x
     ((or (real-subtype _ '*)
          (real-subtype '* _))
      `(,%float%
        ,(coerce -1 %float%)
        ,(coerce 1 %float%)))
     ((real-subtype)
      (cos-inferer
       (sub-to-float-type
        x
        ;; http://clhs.lisp.se/Body/v_pi.htm
        ;; The best long float approximation to the mathematical constant <PI>.
        `(long-float ,(/ pi 2) ,(/ pi 2)))))
     ((complex-type)
      ;; TBD
      'complex)
     ((or-type types)
      (reduce #'union-to-float-type types :key #'sin-inferer))
     ((and-type types)
      (reduce #'intersection-to-float-type types :key #'sin-inferer)))))

(set-type-inferer
 'tan
 (defun tan-inferer (x)
   (infer-type '/ (infer-type 'sin x) (infer-type 'cos x))))

(set-type-inferer
 'exp
 (defun exp-inferer (x)
   (flet ((fn* (x &optional (default '*))
            (if (numberp x)
                (handler-case (exp x)
                  (floating-point-overflow ()
                    '*))
                default)))
     (ematch x
       ((real-subtype low high)
        (let ((head (float-substitution x :int-result *numcl-default-float-format*)))
          `(,head ,(fn* low (coerce 0 head)) ,(fn* high))))
       ((complex-type)
        ;; TBD
        'complex)
       ((or-type types)
        (reduce #'union-to-float-type types :key #'exp-inferer))
       ((and-type types)
        (reduce #'intersection-to-float-type types :key #'exp-inferer))))))

(set-type-inferer
 'log
 (defun log-inferer (x)
   (flet ((fn* (x &optional (default '*))
            (if (numberp x)
                (handler-case (log x)
                  (floating-point-overflow ()
                    '*))
                default)))
     (ematch x
       ((real-subtype low high)
        (let ((head (float-substitution x :int-result *numcl-default-float-format*)))
          ;; when minus, may become complex
          (cond
            ((interval1-< low 0)
             `(complex ,head))
            ((= low 0)
             `(,head * ,(fn* high)))
            (t
             `(,head ,(fn* low) ,(fn* high))))))
       ((complex-type)
        ;; TBD
        'complex)
       ((or-type types)
        (reduce #'union-to-float-type types :key #'log-inferer))
       ((and-type types)
        (reduce #'intersection-to-float-type types :key #'log-inferer))))))

(set-type-inferer
 'abs
 (defun abs-inferer (x)
   (flet ((fn* (x &optional (default '*))
            (if (numberp x)
                (abs x)
                default)))
     (ematch x
       ((real-subtype)
        (let ((head (float-substitution x)))
          (union-to-float-type
           (intersection-to-float-type x `(,head ,(coerce 0 head) *))
           (mul-to-float-type
            `(,head ,(coerce -1 head) ,(coerce -1 head))
            (intersection-to-float-type x `(,head * ,(coerce 0 head)))))))
       ((complex-type)
        ;; TBD
        'complex)
       ((or-type types)
        (reduce #'union-to-float-type types :key #'abs-inferer))
       ((and-type types)
        (reduce #'intersection-to-float-type types :key #'abs-inferer))))))

;; floor is same as / except the handling of integer-integer

(set-type-inferer
 'floor
 (defun floor-inferer (x y)
   (ematch* (x y)
     (((real-subtype l1 h1) (real-subtype l2 h2))
      `(integer ,@(interval-floor l1 h1 l2 h2)))
     (((or-type types1) (or-type types2))
      (reduce #'union-to-float-type
              (mapcar #'floor-inferer types1 types2)))
     (((and-type types1) (and-type types2))
      (reduce #'intersection-to-float-type
              (mapcar #'floor-inferer types1 types2))))))

(set-type-inferer
 'round
 (defun round-inferer (x y)
   (ematch* (x y)
     (((real-subtype l1 h1) (real-subtype l2 h2))
      `(integer ,@(interval-round l1 h1 l2 h2)))
     (((or-type types1) (or-type types2))
      (reduce #'union-to-float-type
              (mapcar #'round-inferer types1 types2)))
     (((and-type types1) (and-type types2))
      (reduce #'intersection-to-float-type
              (mapcar #'round-inferer types1 types2))))))

(set-type-inferer
 'ceiling
 (defun ceiling-inferer (x y)
   (ematch* (x y)
     (((real-subtype l1 h1) (real-subtype l2 h2))
      `(integer ,@(interval-ceiling l1 h1 l2 h2)))
     (((or-type types1) (or-type types2))
      (reduce #'union-to-float-type
              (mapcar #'ceiling-inferer types1 types2)))
     (((and-type types1) (and-type types2))
      (reduce #'intersection-to-float-type
              (mapcar #'ceiling-inferer types1 types2))))))

(set-type-inferer
 'truncate
 (defun truncate-inferer (x y)
   (ematch* (x y)
     (((real-subtype l1 h1) (real-subtype l2 h2))
      `(integer ,@(interval-truncate l1 h1 l2 h2)))
     (((or-type types1) (or-type types2))
      (reduce #'union-to-float-type
              (mapcar #'truncate-inferer types1 types2)))
     (((and-type types1) (and-type types2))
      (reduce #'intersection-to-float-type
              (mapcar #'truncate-inferer types1 types2))))))

(set-type-inferer 'mod 'intersection-to-float-type)

(set-type-inferer
 'rem
 (defun rem-inferer (x y)
   ;; floor documentation:
   ;;
   ;; The remainder is an integer if both x and y are integers, is a rational
   ;; if both x and y are rationals, and is a float if either x or y is
   ;; a float.
   ;; 
   (ematch* (x y)
     (((real-subtype) (real-subtype))
      (intersection-to-float-type
       x
       (union-to-float-type
         y
         (mul-to-float-type y `(integer -1 -1)))))
     (((or-type types1) (or-type types2))
      (reduce #'union-to-float-type
              (mapcar #'rem-inferer types1 types2)))
     (((and-type types1) (and-type types2))
      (reduce #'intersection-to-float-type
              (mapcar #'rem-inferer types1 types2))))))


#|
(defun asin           (x) )
(defun acos           (x) )
(defun atan           (x) )
(defun sinh           (x) )
(defun cosh           (x) )
(defun tanh           (x) )
(defun signum         (x) )
(defun cis            (x) )
(defun complex        (x) (map-array #'complex x))
(defun conjugate      (x) )
(defun phase          (x) )
(defun realpart       (x) )
(defun imagpart       (x) )
(defun numerator      (x) )
(defun denominator    (x) )
(defun logcount       (x) )
(defun integer-length (x) )
(defun float          (x) )
|#


(set-type-inferer '=/bit (constantly 'bit))
(set-type-inferer '/=/bit (constantly 'bit))
(set-type-inferer '>=/bit (constantly 'bit))
(set-type-inferer '<=/bit (constantly 'bit))
(set-type-inferer '>/bit (constantly 'bit))
(set-type-inferer '</bit (constantly 'bit))

(set-type-inferer
 'logior
 (defun infer-logior (x y)
   (labels ((fn (prev now)
              (ematch* (prev now)
                (((integer-subtype l1 h1) (integer-subtype l2 h2))
                 (flet ((integer-length* (x)
                          (if (eq x '*) '* (integer-length x))))
                   `(,(if (or (interval1-< l1 0)
                              (interval1-< l2 0))
                          'signed-byte
                          'unsigned-byte)
                      ,(interval2-max (interval2-max (integer-length* l1)
                                                     (integer-length* h1))
                                      (interval2-max (integer-length* l2)
                                                     (integer-length* h2))))))
                (((or-type types1) _)
                 (simplify-or-types
                  (mapcar (lambda (type) (fn type now)) types1)))
                ((_ (or-type types2))
                 (simplify-or-types
                  (mapcar (lambda (type) (fn prev type)) types2))))))
     (fn x y))))

(set-type-inferer
 'logand
 (defun infer-logand (x y)
   (labels ((fn (prev now)
              (ematch* (prev now)
                (((integer-subtype l1 h1) (integer-subtype l2 h2))
                 (flet ((integer-length* (x)
                          (if (eq x '*) '* (integer-length x))))
                   `(,(if (and (interval1-< l1 0)
                               (interval1-< l2 0))
                          'signed-byte
                          'unsigned-byte)
                      ,(interval2-min (interval2-max (integer-length* l1)
                                                     (integer-length* h1))
                                      (interval2-max (integer-length* l2)
                                                     (integer-length* h2))))))
                (((or-type types1) _)
                 (simplify-or-types
                  (mapcar (lambda (type) (fn type now)) types1)))
                ((_ (or-type types2))
                 (simplify-or-types
                  (mapcar (lambda (type) (fn prev type)) types2))))))
     (fn x y))))

(set-type-inferer
 'logeqv
 (defun infer-logeqv (x y)
   (labels ((fn (prev now)
              (ematch* (prev now)
                (((integer-subtype l1 h1) (integer-subtype l2 h2))
                 (flet ((integer-length* (x)
                          (if (eq x '*) '* (integer-length x))))
                   `(signed-byte
                     ,(interval2-max (interval2-max (integer-length* l1)
                                                    (integer-length* h1))
                                     (interval2-max (integer-length* l2)
                                                    (integer-length* h2))))))
                (((or-type types1) _)
                 (simplify-or-types
                  (mapcar (lambda (type) (fn type now)) types1)))
                ((_ (or-type types2))
                 (simplify-or-types
                  (mapcar (lambda (type) (fn prev type)) types2))))))
     (fn x y))))

(set-type-inferer 'logxor 'infer-logeqv)

(set-type-inferer
 'lognot
 (defun infer-lognot (x)
   (labels ((fn (x)
              (ematch (x)
                ((integer-subtype l1 h1)
                 (flet ((integer-length* (x)
                          (if (eq x '*) '* (integer-length x)))
                        (lognot* (x)
                          (if (eq x '*) '* (lognot x))))
                   `(,(if (or (interval1-< (lognot* l1) 0)
                              (interval1-< (lognot* h1) 0))
                          'signed-byte
                          'unsigned-byte)
                      ,(interval2-max (interval2-max (integer-length* l1)
                                                     (integer-length* h1))
                                      (interval2-max (integer-length* l2)
                                                     (integer-length* h2))))))
                ((or-type types1)
                 (simplify-or-types
                  (mapcar #'fn types1))))))
     (fn x))))

(set-type-inferer 'logandc1 (defun infer-logandc1 (x y) (infer-logand (infer-lognot x) y)))
(set-type-inferer 'logandc2 (defun infer-logandc2 (x y) (infer-logand x (infer-lognot y))))
(set-type-inferer 'logorc1 (defun infer-logorc1 (x y) (infer-logior (infer-lognot x) y)))
(set-type-inferer 'logorc2 (defun infer-logorc2 (x y) (infer-logior x (infer-lognot y))))
(set-type-inferer 'lognand (defun infer-lognand (x y) (infer-lognot (infer-logand x y))))
(set-type-inferer 'lognor  (defun infer-lognor  (x y) (infer-lognot (infer-logior x y))))
  

