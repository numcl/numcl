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


;; shuffle

;; cf. alexandria code
(defun shuffle (array-or-sequence &key (start 0) end)
  "
This code extends alexandria:shuffle.

> Returns a random permutation of SEQUENCE bounded by START and END.
> Original sequence may be destructively modified, and (if it contains
> CONS or lists themselv) share storage with the original one.
> Signals an error if SEQUENCE is not a proper sequence.



 "
  (declare (type fixnum start)
           (type (or fixnum null) end))
  (etypecase array-or-sequence
    (list
     (let* ((end (or end (proper-list-length array-or-sequence)))
            (n (- end start)))
       (do ((tail (nthcdr start array-or-sequence) (cdr tail)))
           ((zerop n))
         (rotatef (car tail) (car (nthcdr (random n) tail)))
         (decf n))))
    (vector
     (let ((end (or end (length array-or-sequence))))
       (loop for i from start below end
             do (rotatef (aref array-or-sequence i)
                         (aref array-or-sequence (+ i (random (- end i))))))))
    (sequence
     (let ((end (or end (length array-or-sequence))))
       (loop for i from (- end 1) downto start
             do (rotatef (elt array-or-sequence i)
                         (elt array-or-sequence (+ i (random (- end i))))))))
    (array
     (let ((end (or end (first (shape array-or-sequence))))
           (copy (empty-like (numcl:aref array-or-sequence 0))))
       (loop for i from (- end 1) downto start
          do (let ((rand (+ i (random (- end i)))))
               (replace (flatten copy) (flatten (numcl:aref array-or-sequence rand)))
               (replace (flatten (numcl:aref array-or-sequence rand)) (flatten (numcl:aref array-or-sequence i)))
               (replace (flatten (numcl:aref array-or-sequence i)) (flatten copy)))))))
  array-or-sequence)

;;*beta(a, b[, size]) 	Draw samples from a Beta distribution.
;;*binomial(n, p[, size]) 	Draw samples from a binomial distribution.
;; chisquare(df[, size]) 	Draw samples from a chi-square distribution.
;; dirichlet(alpha[, size]) 	Draw samples from the Dirichlet distribution.
;;*exponential([scale, size]) 	Draw samples from an exponential distribution.
;;*f(dfnum, dfden[, size]) 	Draw samples from an F distribution.
;;*gamma(shape[, scale, size]) 	Draw samples from a Gamma distribution.
;; geometric(p[, size]) 	Draw samples from the geometric distribution.
;; gumbel([loc, scale, size]) 	Draw samples from a Gumbel distribution.
;; hypergeometric(ngood, nbad, nsample[, size]) 	Draw samples from a Hypergeometric distribution.
;; laplace([loc, scale, size]) 	Draw samples from the Laplace or double exponential distribution with specified location (or mean) and scale (decay).
;; logistic([loc, scale, size]) 	Draw samples from a logistic distribution.
;; lognormal([mean, sigma, size]) 	Draw samples from a log-normal distribution.
;; logseries(p[, size]) 	Draw samples from a logarithmic series distribution.
;; multinomial(n, pvals[, size]) 	Draw samples from a multinomial distribution.
;; multivariate_normal(mean, cov[, size, …) 	Draw random samples from a multivariate normal distribution.
;;*negative_binomial(n, p[, size]) 	Draw samples from a negative binomial distribution.
;; noncentral_chisquare(df, nonc[, size]) 	Draw samples from a noncentral chi-square distribution.
;; noncentral_f(dfnum, dfden, nonc[, size]) 	Draw samples from the noncentral F distribution.
;;*normal([loc, scale, size]) 	Draw random samples from a normal (Gaussian) distribution.
;; pareto(a[, size]) 	Draw samples from a Pareto II or Lomax distribution with specified shape.
;;*poisson([lam, size]) 	Draw samples from a Poisson distribution.
;; power(a[, size]) 	Draws samples in [0, 1] from a power distribution with positive exponent a - 1.
;; rayleigh([scale, size]) 	Draw samples from a Rayleigh distribution.
;; standard_cauchy([size]) 	Draw samples from a standard Cauchy distribution with mode = 0.
;; standard_exponential([size]) 	Draw samples from the standard exponential distribution.
;; standard_gamma(shape[, size]) 	Draw samples from a standard Gamma distribution.
;; standard_normal([size]) 	Draw samples from a standard Normal distribution (mean=0, stdev=1).
;; standard_t(df[, size]) 	Draw samples from a standard Student’s t distribution with df degrees of freedom.
;; triangular(left, mode, right[, size]) 	Draw samples from the triangular distribution over the interval [left, right].
;;*uniform([low, high, size]) 	Draw samples from a uniform distribution.
;; vonmises(mu, kappa[, size]) 	Draw samples from a von Mises distribution.
;; wald(mean, scale[, size]) 	Draw samples from a Wald, or inverse Gaussian, distribution.
;; weibull(a[, size]) 	Draw samples from a Weibull distribution.
;; zipf(a[, size]) 	Draw samples from a Zipf distribution.

#+(or)
(declaim (inline random))
#+(or)
(defun random (&optional shape)
  (multiple-value-bind (a base-array) (empty shape :type 'single-float)
    (map-into base-array (lambda () (random 1.0)))
    (values a base-array)))

(declaim (inline bernoulli))
(defun bernoulli (p &optional shape)
  "Returns a bit array whose elements are 1 with probability P"
  (multiple-value-bind (a base-array) (empty shape :type 'bit)
    (map-into base-array (lambda () (if (< (random 1.0) p) 1 0)))
    (values a base-array)))

(declaim (inline bernoulli))
(defun bernoulli-like (a)
  (bernoulli
   (/ (count 1 (flatten a))
      (size a))
   (shape a)))

#|

More complicated random distributions

Library Considerations

random --- is not what we want

cl-random --- by tpapp, abandoned, but seems fairly complete
          --- needs so many his stuff, and libRmath

random-sample --- is not what we want

cl-randist --- seems fairly complete

cl-variates --- not feature-rich

|#

;; beta(a, b[, size]) 	Draw samples from a Beta distribution.
(declaim (inline beta))
(defun beta (a b &optional shape (type (union-to-float-type (type-of a) (type-of b))))
  (multiple-value-bind (array base-array) (empty shape :type type)
    (map-into base-array (lambda () (coerce (random-distributions:random-beta (coerce a 'double-float) (coerce b 'double-float)) type)))
    (values array base-array)))

;; binomial(n, p[, size]) 	Draw samples from a binomial distribution.

(declaim (inline binomial))
(defun binomial (n p &optional shape)
  (check-type n (and fixnum (integer 0)))
  (check-type p float)
  (multiple-value-bind (array base-array) (empty shape :type `(integer 0 (,n)))
    (map-into base-array (lambda () (random-distributions:random-binomial (coerce p 'double-float) n)))
    (values array base-array)))

;; chisquare(df[, size]) 	Draw samples from a chi-square distribution.

;; dirichlet(alpha[, size]) 	Draw samples from the Dirichlet distribution.

;; exponential([scale, size]) 	Draw samples from an exponential distribution.

(declaim (inline exponential))
(defun exponential (scale &optional shape (type (union-to-float-type (type-of scale))))
  (multiple-value-bind (array base-array) (empty shape :type type)
    (map-into base-array (lambda () (coerce (random-distributions:random-exponential (coerce scale 'double-float)) type)))
    (values array base-array)))

;; f(dfnum, dfden[, size]) 	Draw samples from an F distribution.

(declaim (inline f))
(defun f (dfnum dfden &optional shape (type (union-to-float-type (type-of dfnum) (type-of dfden))))
  (multiple-value-bind (array base-array) (empty shape :type type)
    (map-into base-array (lambda () (coerce (random-distributions:random-f (coerce dfnum 'double-float) (coerce dfden 'double-float)) type)))
    (values array base-array)))

;; gamma(shape[, scale, size]) 	Draw samples from a Gamma distribution.

(declaim (inline gamma))
(defun gamma (k &optional (theta 1.0) shape (type (union-to-float-type (type-of k) (type-of theta))))
  (multiple-value-bind (array base-array) (empty shape :type type)
    (map-into base-array (lambda () (coerce (random-distributions:random-gamma (coerce k 'double-float) (coerce theta 'double-float)) type)))
    (values array base-array)))

;; geometric(p[, size]) 	Draw samples from the geometric distribution.

;; gumbel([loc, scale, size]) 	Draw samples from a Gumbel distribution.

;; hypergeometric(ngood, nbad, nsample[, size]) 	Draw samples from a Hypergeometric distribution.

;; laplace([loc, scale, size]) 	Draw samples from the Laplace or double exponential distribution with specified location (or mean) and scale (decay).

;; logistic([loc, scale, size]) 	Draw samples from a logistic distribution.

;; lognormal([mean, sigma, size]) 	Draw samples from a log-normal distribution.

;; logseries(p[, size]) 	Draw samples from a logarithmic series distribution.

;; multinomial(n, pvals[, size]) 	Draw samples from a multinomial distribution.

(declaim (inline multinomial))
(defun multinomial (n pvals &optional shape)
  "pvals is a sequence of probabilities summing up to 1."
  (multiple-value-bind (array base-array) (empty shape :type `(integer 0 (,n)))
    (map-into base-array (lambda () (random-distributions:random-multinomial n pvals)))
    (values array base-array)))

;; multivariate_normal(mean, cov[, size, …) 	Draw random samples from a multivariate normal distribution.

;; negative_binomial(n, p[, size]) 	Draw samples from a negative binomial distribution.

(declaim (inline negative-binomial))
(defun negative-binomial (n p &optional shape)
  (multiple-value-bind (array base-array) (empty shape :type `(integer 0 (,n)))
    (map-into base-array (lambda () (random-distributions:random-negative-binomial (coerce p 'double-float) n)))
    (values array base-array)))

;; noncentral_chisquare(df, nonc[, size]) 	Draw samples from a noncentral chi-square distribution.

;; noncentral_f(dfnum, dfden, nonc[, size]) 	Draw samples from the noncentral F distribution.

;; normal([loc, scale, size]) 	Draw random samples from a normal (Gaussian) distribution.

(declaim (inline normal))
(defun normal (&optional (mean 0.0) (var 1.0) shape (type (union-to-float-type (type-of mean) (type-of var))))
  (multiple-value-bind (array base-array) (empty shape :type type)
    (map-into base-array (lambda () (coerce (random-distributions:random-normal mean var) type)))
    (values array base-array)))

;; pareto(a[, size]) 	Draw samples from a Pareto II or Lomax distribution with specified shape.

;; not for now --- randist provides two-arg pareto (a and m)

;; (declaim (inline pareto))
;; (defun pareto (a &optional shape (type (union-to-float-type (type-of a))))
;;   (multiple-value-bind (array base-array) (empty shape :type type)
;;     (map-into base-array (lambda () (coerce (random-distributions:random-pareto a) type)))
;;     (values array base-array)))

;; poisson([lam, size]) 	Draw samples from a Poisson distribution.

(declaim (inline poisson))
(defun poisson (&optional (lambda 1.0) shape (type (union-to-float-type (type-of lambda))))
  (multiple-value-bind (array base-array) (empty shape :type type)
    (map-into base-array (lambda () (coerce (random-distributions:random-poisson (coerce lambda 'double-float)) type)))
    (values array base-array)))

;; power(a[, size]) 	Draws samples in [0, 1] from a power distribution with positive exponent a - 1.

;; rayleigh([scale, size]) 	Draw samples from a Rayleigh distribution.

;; standard_cauchy([size]) 	Draw samples from a standard Cauchy distribution with mode = 0.

;; standard_exponential([size]) 	Draw samples from the standard exponential distribution.

;; standard_gamma(shape[, size]) 	Draw samples from a standard Gamma distribution.

;; standard_normal([size]) 	Draw samples from a standard Normal distribution (mean=0, stdev=1).

;; standard_t(df[, size]) 	Draw samples from a standard Student’s t distribution with df degrees of freedom.

;; triangular(left, mode, right[, size]) 	Draw samples from the triangular distribution over the interval [left, right].

;; uniform([low, high, size]) 	Draw samples from a uniform distribution.

(declaim (inline uniform))
(defun uniform (&optional (low 0.0) (high 1.0) shape type)
  (let ((d (- high low)))
    (if shape
        (let ((type (or type (bind-to-float-type (strict-type-of low) (strict-type-of high)))))
          (multiple-value-bind (array base-array) (empty shape :type type)
            (map-into base-array (lambda () (coerce (+ low (random d)) type)))
            (values array base-array)))
        (+ low (random d)))))

;; vonmises(mu, kappa[, size]) 	Draw samples from a von Mises distribution.

;; wald(mean, scale[, size]) 	Draw samples from a Wald, or inverse Gaussian, distribution.

;; weibull(a[, size]) 	Draw samples from a Weibull distribution.

;; zipf(a[, size]) 	Draw samples from a Zipf distribution.
