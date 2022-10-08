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

(define-symbol-macro %float% +numcl-default-float-format+)

;;;; interval arithmetic

;; low-level functions.
;; These are used for computing the result for the lower/upper bounds.
;; It assumes a number, potentially including a float infinity or quiet NaN.

(defun %interval-max (x y)
  (float-features:with-float-traps-masked (:invalid)
    (max x y)))
(defun %interval-min (x y)
  (float-features:with-float-traps-masked (:invalid)
    (min x y)))
(defun %interval-add (x y)
  (float-features:with-float-traps-masked (:invalid)
    (+ x y)))
(defun %interval-sub (x y)
  (float-features:with-float-traps-masked (:invalid)
    (- x y)))
(defun %interval-mul (x y)
  (float-features:with-float-traps-masked (:invalid)
    (* x y)))
(defun %interval-divlike (fn x y)
  (float-features:with-float-traps-masked (:invalid :divide-by-zero)
    #+(or)
    (funcall fn x y)
    (handle-sbcl-bug fn x y)))

;; SBCL bug:
;; (floor float-features:single-float-positive-infinity 1.0) --> error
;; (round float-features:single-float-positive-infinity 1.0) --> error
;; (ceiling float-features:single-float-positive-infinity 1.0) --> error

(defun handle-sbcl-bug (fn x y)
  (if (integerp y)
      (cond                             ; note: y cannot be infinity or NaN.
        ((and (floatp x) (float-features:float-infinity-p x))
         (* x (signum y)))
        ((and (floatp x) (float-features:float-nan-p x))
         x)
        (t (funcall fn x (float y))))
      (funcall fn x y)))

(defun %interval-div       (number divisor) (%interval-divlike '/         number divisor))
(defun %interval-round     (number divisor) (%interval-divlike 'round     number divisor))
(defun %interval-floor     (number divisor) (%interval-divlike 'floor     number divisor))
(defun %interval-ceiling   (number divisor) (%interval-divlike 'ceiling   number divisor))
(defun %interval-truncate  (number divisor) (%interval-divlike 'truncate  number divisor))
(defun %interval-fround    (number divisor) (%interval-divlike 'fround    number divisor))
(defun %interval-ffloor    (number divisor) (%interval-divlike 'ffloor    number divisor))
(defun %interval-fceiling  (number divisor) (%interval-divlike 'fceiling  number divisor))
(defun %interval-ftruncate (number divisor) (%interval-divlike 'ftruncate number divisor))

(defun %interval-< (x y)
  (float-features:with-float-traps-masked (:invalid)
    (< x y)))
(defun %interval-> (x y)
  (float-features:with-float-traps-masked (:invalid)
    (> x y)))
(defun %interval-= (x y)
  (float-features:with-float-traps-masked (:invalid)
    (= x y)))
(defun %interval-<= (x y)
  (float-features:with-float-traps-masked (:invalid)
    (<= x y)))
(defun %interval->= (x y)
  (float-features:with-float-traps-masked (:invalid)
    (>= x y)))
(defun %interval-/= (x y)
  (float-features:with-float-traps-masked (:invalid)
    (/= x y)))

;; converters used for passing values to the low-level functions.

(defun interval-preprocess-low (x)
  (ematch x
    ('* float-features:long-float-negative-infinity)
    (_ x)))

(defun interval-preprocess-high (x)
  (ematch x
    ('* float-features:long-float-positive-infinity)
    (_ x)))

(defun interval-postprocess-low (x)
  (cond
    ((and (floatp x) (float-features:float-infinity-p x) (minusp x)) '*)
    ((and (floatp x) (float-features:float-infinity-p x) (plusp  x)) (throw 'empty-interval nil))
    ((and (floatp x) (float-features:float-nan-p x)) '*)
    (t x)))

(defun interval-postprocess-high (x)
  (cond
    ((and (floatp x) (float-features:float-infinity-p x) (plusp  x)) '*)
    ((and (floatp x) (float-features:float-infinity-p x) (minusp x)) (throw 'empty-interval nil))
    ((and (floatp x) (float-features:float-nan-p x)) '*)
    (t x)))


(defmacro without-* (&body body)
  "Internal macro"
  `(let ((l1 (interval-preprocess-low l1))
         (l2 (interval-preprocess-low l2))
         (h1 (interval-preprocess-high h1))
         (h2 (interval-preprocess-high h2)))
     ,@body))

(defun postprocess (interval)
  (list (interval-postprocess-low (first interval))
        (interval-postprocess-high (second interval))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(defun interval-min          (l1 h1 l2 h2 &key) (without-* (postprocess (list (%interval-min l1 l2) (%interval-min h1 h2)))))
(defun interval-max          (l1 h1 l2 h2 &key) (without-* (postprocess (list (%interval-max l1 l2) (%interval-max h1 h2)))))
(defun interval-union        (l1 h1 l2 h2 &key) (without-* (postprocess (list (%interval-min l1 l2) (%interval-max h1 h2)))))
(defun interval-intersection (l1 h1 l2 h2 &key) (without-* (postprocess (list (%interval-max l1 l2) (%interval-min h1 h2)))))

#|

IEEE Std 754-2008
IEEE Standard for Floating-Point Arithmetic

7.2 Invalid operation

The invalid operation exception is signaled if and only if there is no usefully definable result. In these cases
the operands are invalid for the operation to be performed.
For operations producing results in floating-point format, the default result of an operation that signals the
invalid operation exception shall be a quiet NaN that should provide some diagnostic information (see 6.2).

These operations are:
a) any general-computational or signaling-computational operation on a signaling NaN (see 6.2),except for some conversions (see 5.12)
b) multiplication: multiplication(0, ∞) or multiplication(∞, 0)
c) fusedMultiplyAdd: fusedMultiplyAdd(0, ∞, c) or fusedMultiplyAdd(∞, 0, c) unless c is a quiet NaN; if c is a quiet NaN then it is implementation defined whether the invalid operation exception is signaled
d) addition or subtraction or fusedMultiplyAdd: magnitude subtraction of infinities, such as: addition(+∞, −∞)
e) division: division(0, 0) or division(∞, ∞)
f) remainder: remainder(x, y), when y is zero or x is infinite and neither is NaN
g) squareRoot if the operand is less than zero
h) quantize when the result does not fit in the destination format or when one operand is finite and the other is infinite

For operations producing no result in floating-point format, the operations that signal the invalid operation
exception are:

i) conversion of a floating-point number to an integer format, when the source is NaN, infinity, or a value that would convert to an integer outside the range of the result format under the applicable rounding attribute
j) comparison by way of unordered-signaling predicates listed in Table 5.2, when the operands are unordered
k) logB(NaN), logB(∞), or logB(0) when logBFormat is an integer format (see 5.3.3).

7.3 Division by zero

The divideByZero exception shall be signaled if and only if an exact infinite result is defined for an
operation on finite operands. The default result of divideByZero shall be an ∞ correctly signed according to
the operation:

- For division, when the divisor is zero and the dividend is a finite non-zero number, the sign of the
infinity is the exclusive OR of the operands’ signs (see 6.3).
- For logB(0) when logBFormat is a floating-point format, the sign of the infinity is minus (−∞).

------------------------------------

If the pointwise operation returns NaN, it is due to invalid operations, and
additionally division-by-zero in case of division.
We need to handle them case-by-case.

In add, there is no possibility of +inf + -inf (or its inverse), since it is computed between the lower / upper bounds.
In sub, there is no possibility of +inf - +inf (or its inverse), since it is computed between the different side of the bounds.

|#

(defun interval-add (l1 h1 l2 h2 &key) (without-* (postprocess (list (%interval-add l1 l2) (%interval-add h1 h2)))))
(defun interval-sub (l1 h1 l2 h2 &key) (without-* (postprocess (list (%interval-sub l1 h2) (%interval-sub h1 l2)))))

(defun safe-float-nan-p (x)
  (and (floatp x) (float-features:float-nan-p x)))
(defun safe-float-infinity-p (x)
  (and (floatp x) (float-features:float-infinity-p x)))

;; convert zeros (0, +0.0, -0.0) to the correct zeros (+0.0 for lb, -0.0 for ub) when another operand is also a float
(defun signed-zero-lb (x other) (if (and (floatp other) (zerop x)) 0.0l0 x))
(defun signed-zero-ub (x other) (if (and (floatp other) (zerop x)) -0.0l0 x))

(defun nan-result-positive-lb (x) (if (safe-float-nan-p x) 0.0l0 x))
(defun nan-result-positive-ub (x) (if (safe-float-nan-p x) float-features:long-float-positive-infinity x))
(defun nan-result-negative-lb (x) (if (safe-float-nan-p x) float-features:long-float-negative-infinity x))
(defun nan-result-negative-ub (x) (if (safe-float-nan-p x) -0.0l0 x))

(defun interval-mul (l1 h1 l2 h2 &key)
  (without-*
    (let* ((v0 (%interval-mul (signed-zero-lb l1 l2) (signed-zero-lb l2 l1)))
           (v1 (%interval-mul (signed-zero-lb l1 h2) (signed-zero-ub h2 l1)))
           (v2 (%interval-mul (signed-zero-ub h1 l2) (signed-zero-lb l2 h1)))
           (v3 (%interval-mul (signed-zero-ub h1 h2) (signed-zero-ub h2 h1)))
           ;;
           ;; In multiplication, possible modes of nan are 0 * inf or inf * 0.
           ;; We can determine the sign of the inifinity from whether the number is
           ;; a lower bound (in which case it should be -inf) or an upper bound (should be +inf).
           ;; We can also determine the sign of zeros (lb -> +0, ub -> -0)
           ;; 
           ;; +0 * -inf, -inf * +0 . Result range is from -inf to -0
           (lb0 (nan-result-negative-lb v0))
           ;; +0 * +inf, -inf * -0 . Result range is from +0 to +inf
           (lb1 (nan-result-positive-lb v1))
           ;; -0 * -inf, +inf * +0 . Result range is from +0 to +inf
           (lb2 (nan-result-positive-lb v2))
           ;; -0 * +inf, +inf * -0 . Result range is from -inf to -0
           (lb3 (nan-result-negative-lb v3))
           ;;
           ;; +0 * -inf, -inf * +0 . Result range is from -inf to -0
           (ub0 (nan-result-negative-ub v0))
           ;; +0 * +inf, -inf * -0 . Result range is from +0 to +inf
           (ub1 (nan-result-positive-ub v1))
           ;; -0 * -inf, +inf * +0 . Result range is from +0 to +inf
           (ub2 (nan-result-positive-ub v2))
           ;; -0 * +inf, +inf * -0 . Result range is from -inf to -0
           (ub3 (nan-result-negative-ub v3)))
      (postprocess
       (list (reduce #'%interval-min (list lb0 lb1 lb2 lb3))
             (reduce #'%interval-max (list ub0 ub1 ub2 ub3)))))))

;; division: split the divisor interval into positive/negative and combine results

(defun interval-divlike (fn l1 h1 l2 h2 &key divisor-type)
  (without-*
    (if (< l2 0 h2)
        ;; divide into positive divisor and negative divisor
        (apply #'interval-union
               (if (eq divisor-type 'integer)
                   (append (interval-divlike fn l1 h1 l2 -1 :divisor-type divisor-type)
                           (interval-divlike fn l1 h1 1 h2  :divisor-type divisor-type))
                   (append (interval-divlike fn l1 h1 l2 -0.0 :divisor-type divisor-type)
                           (interval-divlike fn l1 h1 0.0 h2  :divisor-type divisor-type))))
        (let* ((v0 (funcall fn (signed-zero-lb l1 l2) (signed-zero-lb l2 l1)))
               (v1 (funcall fn (signed-zero-lb l1 h2) (signed-zero-ub h2 l1)))
               (v2 (funcall fn (signed-zero-ub h1 l2) (signed-zero-lb l2 h1)))
               (v3 (funcall fn (signed-zero-ub h1 h2) (signed-zero-ub h2 h1)))
               ;; 
               ;; in division, possible modes of nan is 0 / 0 and inf / inf
               ;; 
               ;; -inf / -inf, +0 / +0 . Result range is from +0 to +inf
               (lb0 (nan-result-positive-lb v0))
               ;; -inf / +inf, +0 / -0 . Result range is from -inf to -0
               (lb1 (nan-result-negative-lb v1))
               ;; +inf / -inf, -0 / +0 . Result range is from -inf to -0
               (lb2 (nan-result-negative-lb v2))
               ;; +inf / +inf, -0 / -0 . Result range is from +0 to +inf
               (lb3 (nan-result-positive-lb v3))
               ;;
               ;; -inf / -inf, +0 / +0 . Result range is from +0 to +inf
               (ub0 (nan-result-positive-ub v0))
               ;; -inf / +inf, +0 / -0 . Result range is from -inf to -0
               (ub1 (nan-result-negative-ub v1))
               ;; +inf / -inf, -0 / +0 . Result range is from -inf to -0
               (ub2 (nan-result-negative-ub v2))
               ;; +inf / +inf, -0 / -0 . Result range is from +0 to +inf
               (ub3 (nan-result-positive-ub v3)))
          (postprocess
           (list (reduce #'%interval-min (list lb0 lb1 lb2 lb3))
                 (reduce #'%interval-max (list ub0 ub1 ub2 ub3))))))))

(defun interval-div       (l1 h1 l2 h2 &key divisor-type) (interval-divlike '%interval-div       l1 h1 l2 h2 :divisor-type divisor-type))
(defun interval-round     (l1 h1 l2 h2 &key divisor-type) (interval-divlike '%interval-round     l1 h1 l2 h2 :divisor-type divisor-type))
(defun interval-floor     (l1 h1 l2 h2 &key divisor-type) (interval-divlike '%interval-floor     l1 h1 l2 h2 :divisor-type divisor-type))
(defun interval-ceiling   (l1 h1 l2 h2 &key divisor-type) (interval-divlike '%interval-ceiling   l1 h1 l2 h2 :divisor-type divisor-type))
(defun interval-truncate  (l1 h1 l2 h2 &key divisor-type) (interval-divlike '%interval-truncate  l1 h1 l2 h2 :divisor-type divisor-type))
(defun interval-fround    (l1 h1 l2 h2 &key divisor-type) (interval-divlike '%interval-fround    l1 h1 l2 h2 :divisor-type divisor-type))
(defun interval-ffloor    (l1 h1 l2 h2 &key divisor-type) (interval-divlike '%interval-ffloor    l1 h1 l2 h2 :divisor-type divisor-type))
(defun interval-fceiling  (l1 h1 l2 h2 &key divisor-type) (interval-divlike '%interval-fceiling  l1 h1 l2 h2 :divisor-type divisor-type))
(defun interval-ftruncate (l1 h1 l2 h2 &key divisor-type) (interval-divlike '%interval-ftruncate l1 h1 l2 h2 :divisor-type divisor-type))

(defun interval-connected-p (l1 h1 l2 h2)
  (without-*
    (%interval-<= (%interval-max l1 l2) (%interval-min h1 h2))))

(defun interval-< (l1 h1 l2 h2)
  "Sort types accorting to the interval-designators. Provides a full ordering"
  (if (%interval-< l1 l2)
      t
      (if (%interval-= l1 l2)
          (%interval-< h1 h2)
          nil)))

(defun interval-coerce (value type)
  (declare ((or number (eql *)) value))
  (if (eq '* value)
      '*
      (coerce value type)))

;;;; float conversions

#|

Common Lisp defines several rules for the type of the values returned by mathematical functions.
number = (or complex real)
real   = (or float rational)
rational = (or ratio integer)
integer  = (or fixnum bignum)
float    = (or short-float ... long-float) (== irrational).

Rational functions behave as rational... -> rational, float... -> float, {rational,float}... -> float.
This rule is called `float contagion` rule.

Rational functions do not guarantee integer -> integer, prmarily due to / ,
 which returns (integer, integer) -> (or ratio integer).

Irrational functions behaves as rational -> (or rational float), float -> float.
Examples are (sin pi) -> 1/2 (or a float near epsilon).
The behavior depends on the implementation and is called `float substitution rule`.

---

In NUMCL, RATIO does not exist: They are always converted to +numcl-default-float-format+.
This means that / always returns a float array (except atomic numbers are given).
We also assume irrational functions always return floats.

(ARRAY BIGNUM) does not exist either. When the result of numerical computation causes
an overflow, it signals an error.

For complex arrays, only (COMPLEX *-FLOAT) exists. both complex integers and complex ratios
are converted into floats. This is because CL does not allow rational complex with imagpart 0.
(cf. http://clhs.lisp.se/Body/t_comple.htm)
This prevents us from having (ARRAY (COMPLEX FIXNUM)).


|#

#+(or)
(define-condition fixnum-overflow (arithmetic-error) ())
#+(or)
(define-condition fixnum-underflow (arithmetic-error) ())

;; float-substitution and float-contagion do not handle intervals.

(constantfold float-substitution)

(defun float-substitution (t1 &key (int-result 'integer))
  "Performs a float substitution.

For the original float substitution rule, check out:
12.1.3.3 Rule of Float Substitutability http://clhs.lisp.se/Body/12_acc.htm

It returns a type specifier that can be combined with intervals.
For example, when the contagion result is FIXNUM, it returns 'INTEGER instead.
Specifically, it returns one of 'integer, 'short-float, ... 'long-float, '(complex integer),
'(complex short-float) ... '(complex long-float) or nil (i.e. empty type).

The optional key argument :int-result can alter the behavior when the input is an integer.
This is useful for specifying the appropriate default behavior for rational functions
(e.g. mod: integer -> integer) and irrational functions (e.g. exp: integer -> float).

If the argument is FLOAT (without precision), it returns +numcl-default-float-format+.
If the argument is a type specifier for a concrete float subtype, it returns its primary type (e.g. 'single-float).
If the argument is a type specifier for an integer subtype, it returns the value of an optional key argument int-result, defaulted to 'integer.
If the argument is a type specifier for a real subtype, it returns +numcl-default-float-format+.
If the argument is a complex type, it applies itself to the complex element type.

If the argument is a compound type (OR/AND), it applies itself to each type.
For OR, it returns the OR of each result.
For AND, all result should be eq to each other, otherwise NIL.

If the argument is a NIL type (empty type), NIL is returned.

For an unsupported type, it signals an error.
"
  (declare ((or list symbol) t1))
  (declare (symbol int-result))
  (labels ((rec (t1)
             (ematch t1
               (t t)
               (nil nil)
               ;; irrationals
               ((float-type)                +numcl-default-float-format+)
               ((long-float-type)           'long-float)
               ((double-float-type)         'double-float)
               ((single-float-type)         'single-float)
               ((short-float-type)          'short-float)
               ;; rationals
               ((integer-subtype)           int-result)
	       ((ratio-type)                +numcl-default-float-format+)
	       ((rational-type)             +numcl-default-float-format+)
               ((real-subtype)              (error "this should not happen"))
               ((complex-type element-type) `(complex ,(float-substitution
                                                        element-type
                                                        :int-result +numcl-default-float-format+)))
               ((or-type types)             `(or ,@(remove-duplicates (remove nil (mapcar #'rec types)))))
               ((and-type types)
                (match (mapcar #'rec types)
                  ((list* first rest)
                   (if (every (curry #'eq first) rest)
                       first
                       nil)))))))
    (rec t1)))

(constantfold float-contagion)

(defun float-contagion (t1 t2 &key (int-int-result 'integer))
  "Performs a float contagion.

It returns a type specifier that can be combined with intervals.
For example, when the contagion result is FIXNUM, it returns 'INTEGER instead.
Specifically, it returns one of 'integer, 'short-float, ... 'long-float, '(complex integer),
'(complex short-float) ... '(complex long-float) or nil (i.e. empty type).

The rule applied here is stricter than in CL.
All ratios are converted to +numcl-default-float-format+.
NUMCL is aware of BIGNUM, but never handles it.
When the result of numerical computation causes an overflow/underflow, it signals an error.

For the original float contagion rule, check out:
12.1.4 Floating-point Computations http://clhs.lisp.se/Body/12_ad.htm

The optional key argument :int-int-result can alter the behavior when the both inputs are integers.
This is useful for specifying the appropriate default behavior for rational functions
(e.g. +: integer, integer -> integer) and irrational functions (e.g. /: integer, integer -> float).
"
  (declare ((or list symbol) t1 t2))
  (declare (symbol int-int-result))
  (labels ((rec (t1 t2)
             (ematch* (t1 t2)
               ((t _) t)
               ((_ t) t)
               ((nil _) nil)
               ((_ nil) nil)
	       (((complex-type t1) (complex-type t2))
                `(complex ,(rec t1 t2)))
               (((complex-type t1) t2)
                `(complex ,(rec t1 t2)))
               ((t1 (complex-type t2))
                `(complex ,(rec t1 t2)))
               ((_ (long-float-type))   'long-float)
               (((long-float-type) _)   'long-float)
               ((_ (double-float-type)) 'double-float)
               (((double-float-type) _) 'double-float)
               ((_ (single-float-type)) 'single-float)
               (((single-float-type) _) 'single-float)
               ((_ (short-float-type))  'short-float)
               (((short-float-type) _)  'short-float)
               ((_ (float-type))        +numcl-default-float-format+)
               (((float-type) _)        +numcl-default-float-format+)
               ;; no contagion.
               (((integer-subtype) (integer-subtype))
                ;; this should be altered in some rational function (especially / )
                (float-substitution int-int-result))
               ;; note that t1 and t2 are guaranteed to be clean of ratio type.
               ;; they are either nil, a float subtype, an integer subtype, a
               ;; complex type with float or integer element type, or the compound
               ;; types of those.
               ((_ (real-subtype))      (error "this should not happen"))
               (((real-subtype) _)      (error "this should not happen"))

               ;; compound types
               (((or-type types) _)
                (rec (reduce #'rec types) t2))
               ((_ (or-type types))
                (rec t1 (reduce #'rec types)))
               (((and-type types) _)
                (rec (reduce #'rec types) t2))
               ((_ (and-type types))
                (rec t1 (reduce #'rec types))))))
    (rec (float-substitution t1)
         (float-substitution t2))))

(defun type-interval-< (t1 t2)
  (ematch* (t1 t2)
    (((real-subtype l1 h1) (real-subtype l2 h2))
     (interval-< l1 h1 l2 h2))))

(defun type-connected-p (t1 t2)
  (ematch* (t1 t2)
    (((real-subtype l1 h1) (real-subtype l2 h2))
     (interval-connected-p l1 h1 l2 h2))))

(defun simplify-or-types (types)
  (iter (for type in (sort (copy-list (remove nil types)) #'type-interval-<))
        (with acc = nil)
        (when (first-iteration-p)
          (setf acc type))
        
        (if (type-connected-p acc type)
            (setf acc (union-to-float-type acc type))
            (progn (collect acc into result)
                   (setf acc type)))
        (finally
         (return (if result
                     `(or ,@result ,acc)
                     acc)))))

;; (simplify-or-types (list '(real 0 10) '(integer 5 15)))
;; -> (SINGLE-FLOAT 0.0 15.0)
;; (simplify-or-types (list 'ratio '(integer 5 15)))
;; -> (SINGLE-FLOAT * *)
;; (simplify-or-types (list '(float 2.4 10.0) '(integer 5 15)))
;; -> (SINGLE-FLOAT 2.4 15.0)
;; IMPL> (simplify-or-types (list '(float 2.4 10.0) '(integer 5 15)))
;; -> (SINGLE-FLOAT 2.4 15.0)
;; IMPL> (simplify-or-types (list '(float 2.4 15.0) '(integer 5 10)))
;; -> (SINGLE-FLOAT 2.4 15.0)
;; IMPL> (simplify-or-types (list '(float 5.5 15.0) '(integer 5 10)))
;; -> (SINGLE-FLOAT 5.0 15.0)

(constantfold union-to-float-type :associative t :commutative t)
   
(defun union-to-float-type (&rest typespecs)
  "Convert a set of type specifiers under REAL, e.g. RATIO, INTEGERS, FLOATS,
to the least specific FLOAT type when any one of them are not fixnums.

When two interval designators are not connected, it returns OR types.
If you want them to be merged as a single type, use bind-to-float-type."
  (labels ((fn (prev now)
             (ematch* (prev now)
               ((t _) t)
               ((_ t) t)
               ((nil _) now)
               ((_ nil) prev)
               
               (((complex-type prev) (complex-type now))
                `(complex ,(fn prev now)))
               (((complex-type prev) _)
                `(complex ,(fn prev now)))
               ((_ (complex-type now))
                `(complex ,(fn prev now)))
               
               (((or-type types1) (or-type types2))
                (simplify-or-types (append types1 types2)))
               (((or-type types1) _)
                (simplify-or-types (list now types1)))
               ((_ (or-type types2))
                (simplify-or-types (list prev types2)))
               
               (((real-subtype l1 h1) (real-subtype l2 h2))
                (let ((type (float-contagion prev now)))
                  (flet ((c (value) (interval-coerce value type)))
                    (if (interval-connected-p l1 h1 l2 h2)
                        (catch 'empty-interval
                          (ematch (interval-union l1 h1 l2 h2)
                            ((list low high)
                             (if (or (eq low '*) (eq high '*) (<= low high))
                                 (if (eq type 'integer)
                                     (progn
                                       (check-type low  (or integer (eql *)))
                                       (check-type high (or integer (eql *)))
                                       `(integer ,@(interval-intersection
                                                    low high
                                                    most-negative-fixnum
                                                    most-positive-fixnum)))
                                     `(,type ,(c low) ,(c high)))
                                 nil))))
                        `(or (,type ,(c l1) ,(c h1))
                             (,type ,(c l2) ,(c h2))))))))))
    
    (reduce #'fn typespecs)))

(defun infer-rational-arithmetic-result (interval-op typespecs int-int-result)
  "Convert a set of type specifiers under REAL, e.g. RATIO, INTEGERS, FLOATS,
to the least specific FLOAT type when any one of them are not fixnums."
  (labels ((fn (prev now)
             (ematch* (prev now)
               ((t _) t)
               ((_ t) t)
               ((nil _) nil)
               ((_ nil) nil)
               
               (((complex-type prev) (complex-type now))
                `(complex ,(fn prev now)))
               (((complex-type prev) _)
                `(complex ,(fn prev now)))
               ((_ (complex-type now))
                `(complex ,(fn prev now)))
               
               (((or-type types1) (or-type types2))
                (simplify-or-types
                 (map-product #'fn types1 types2)))
               (((or-type types1) _)
                (simplify-or-types
                 (mapcar (lambda (type) (fn type now)) types1)))
               ((_ (or-type types2))
                (simplify-or-types
                 (mapcar (lambda (type) (fn prev type)) types2)))

               (((real-subtype l1 h1) (real-subtype l2 h2))
                (let ((type (float-contagion prev now  :int-int-result int-int-result)))
                  (flet ((c (value) (interval-coerce value type)))
                    (catch 'empty-interval
                      (ematch (funcall interval-op l1 h1 l2 h2
                                       :divisor-type (float-substitution now)
                                       :allow-other-keys t)
                        ((list low high)
                         (if (or (eq low '*) (eq high '*) (<= low high))
                             (if (eq type 'integer)
                                 (progn
                                   (check-type low  (or integer (eql *)))
                                   (check-type high (or integer (eql *)))
                                   `(integer ,@(interval-intersection
                                                low high
                                                most-negative-fixnum
                                                most-positive-fixnum)))
                                 `(,type ,(c low) ,(c high)))
                             nil))))))))))
    (reduce #'fn typespecs)))

(constantfold intersection-to-float-type :associative t :commutative t)

(defun intersection-to-float-type (&rest typespecs)
  (infer-rational-arithmetic-result #'interval-intersection typespecs 'integer))

;; (intersection-to-float-type
;;  '(or (integer 0 10) (integer 20 30) (integer 40 50))
;;  '(or (integer 8 22) (integer 28 42)))
;; -> (OR (INTEGER 8 10) (INTEGER 20 22) (INTEGER 28 30) (INTEGER 40 42))

(constantfold bind-to-float-type :associative t :commutative t)

(defun bind-to-float-type (&rest typespecs)
  "Returns the upper/lower bound of the number type specifiers."
  (infer-rational-arithmetic-result #'interval-union typespecs 'integer))


;;;; helper functions for ensuring bit length

(declaim (inline ub))
(defun ub (N num)
  "Truncate NUM as (unsigned-byte N) integer, ignoring the sign"
  (logand num (1- (expt 2 N))))
(declaim (inline sb))
(defun sb (N num)
  "Truncate NUM as (signed-byte N) integer, preserving the sign"
  (- (mod (+ num (expt 2 (1- N)))
          (expt 2 N))
     (expt 2 (1- N))))
;; is this the most efficient pattern??

;; (defun fn (x)
;;   (declare (fixnum x)
;;            (optimize (speed 3)))
;;   (ub 4 x))
;; (defun fn (x)
;;   (declare (fixnum x)
;;            (optimize (speed 3)))
;;   (sb 4 x))


;;;; coercing

(constantfold float-to-integer-p)
(defun float-to-integer-p (newtype oldtype)
  (and (integer-subtype-p newtype)
       (float-subtype-p oldtype)))

(constantfold %coerce)
(declaim (inline %coerce))
(defun %coerce (object type)
  "COERCE that additionally converts NUMBER into INTEGER by rounding, and NUMBER to (un/signed-byte N) by modular arithmetic."
  ;; wrong; could be used for char-conversions
  ;; (assert (numberp object))
  #.(iter (for width from 1 to 64)
          ;; Note : This implicitly covers the case of TYPE = FIXNUM, as long as
          ;; FIXNUM bit width being below 64
          (when (= width 1)
            (collecting 'cond))
          (collecting
           `((csubtypep type '(unsigned-byte ,width))
             (ub ,width (round object))))
          (collecting
           `((csubtypep type '(signed-byte ,width))
             (sb ,width (round object))))
          (when (= width 64)
            (collecting
             `((csubtypep type 'integer)
               (round object)))
            (collecting
             `(t
               (coerce object type))))))

;; on sbcl, subtypep is not constant foldable after inlining!
;; (subtypep 'integer '(unsigned-byte 2))
;; (csubtypep 'integer '(unsigned-byte 2))

;; these are NOT constant folded!
;; (defun fn0 () (subtypep '(unsigned-byte 2) '(unsigned-byte 8)))
;; (defun fn1 () (subtypep '(unsigned-byte 16) '(unsigned-byte 8)))
;; (declaim (inline fn2))
;; (defun fn2 (x) (subtypep x '(unsigned-byte 8)))
;; (defun fn3 () (fn2 '(unsigned-byte 2)))
;; (defun fn3 () (fn2 '(unsigned-byte 16)))
;; 
;; With constantfolded functions, these are folded
;; (declaim (inline fn2c))
;; (defun fn2c (x) (csubtypep x '(unsigned-byte 8)))
;; (defun fn3c () (fn2c '(unsigned-byte 2)))
;; (defun fn4c () (fn2c '(unsigned-byte 16)))

;; This must be inferred as returning (UNSIGNED-BYTE 8)
;; (defun fn (x)
;;   (%coerce x '(unsigned-byte 8)))

;; This must be inferred as returning FIXNUM, not (unsigned-byte 64)
;; (defun fn (x)
;;   (%coerce x 'fixnum))

;;;; type-of

(declaim (inline strict-type-of))
(defun strict-type-of (x)
  "stricter version of type-of, which does not simplify the type -- e.g., (type-of 5) is (integer 5 5), not fixnum."
  (etypecase x
    (integer
     `(integer ,x ,x))
    ;; note: ratio does not have any upper bound
    (ratio
     'ratio)
    (rational
     `(rational ,x ,x))
    (single-float
     `(single-float ,x ,x))
    (double-float
     `(double-float ,x ,x))
    (short-float
     `(short-float ,x ,x))
    (long-float
     `(long-float ,x ,x))
    (t
     (type-of x))))

#+(or)
(print
 (list (list (sb 1 -4) (sb 1 -3) (sb 1 -2) (sb 1 -1) (sb 1 0) (sb 1 1) (sb 1 2) (sb 1 3))
       (list (sb 2 -4) (sb 2 -3) (sb 2 -2) (sb 2 -1) (sb 2 0) (sb 2 1) (sb 2 2) (sb 2 3))
       (list (ub 1 -4) (ub 1 -3) (ub 1 -2) (ub 1 -1) (ub 1 0) (ub 1 1) (ub 1 2) (ub 1 3))
       (list (ub 2 -4) (ub 2 -3) (ub 2 -2) (ub 2 -1) (ub 2 0) (ub 2 1) (ub 2 2) (ub 2 3))))

(defmacro compile-time-type-of (variable &environment e)
  (list 'quote
        (cdr
         (assoc 'type
                (nth-value 2 (cltl2:variable-information variable e))))))

(declaim (inline numcl-array-p))
(defun numcl-array-p (array)
  "Returns true when ARRAY satisfies the NUMCL assumption, that is,
an array displaced to a non-displaced 1D array."
  (when (arrayp array)
    (typep (array-displacement array)
           '(simple-array * 1))))

(deftype numcl-array (&optional element-type dimensions)
  "Type specifier for the arrays satifying the NUMCL assumption, that is,
an array displaced to a non-displaced 1D array."
  `(and (array ,element-type ,dimensions)
        (not simple-array)
        (satisfies numcl-array-p)))

(deftype base-array (&optional element-type length)
  "Type specifier for the base, 1D arrays for the NUMCL arrays."
  `(simple-array ,element-type (,length)))

(declaim (inline base-array-p))
(defun base-array-p (array)
  "Returns true when ARRAY is a base array of numcl array."
  (typep array 'base-array))

(deftype index () (upgraded-array-element-type `(integer 0 (,array-dimension-limit))))

(defun typexpand (type &optional env)
  (declare (type (or symbol cons) type)
           #+(or ecl lispworks) (ignore env))
  #+ccl
  (ccl::type-expand type env)
  #+(or sbcl cmucl)
  (sb-ext:typexpand type env)
  #+ecl
  (si::expand-deftype type)
  #+clisp
  (ext:type-expand type env)
  #+abcl
  (system::expand-deftype type)
  #+allegro
  (excl::deftype-expand type env)
  #+lispworks
  (type::expand-user-type type))
