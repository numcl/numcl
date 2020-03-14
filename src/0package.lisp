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

(in-package :cl-user)

(defpackage :numcl.exported
  ;; pitfall: When you do not provide :use keyword in defpackage, it defaults to
  ;; the default value of make-package, which has an implementation dependent
  ;; value. SBCL defaults to NIL and CCL defaults to '(:common-lisp :ccl).
  ;; It is thus necessary to explicitly provide (:use) so that it creates an empty package.
  (:use )
  (:export

   ;; 1version
   #:numcl-version
 
   ;; 1type
   #:numcl-array-p
   #:numcl-array
   #+(or)
   #:fixnum-overflow
   #+(or)
   #:fixnum-underflow
   
   ;; 2alias
   #:shape
   #:size
   #:rank
   #:dtype
   #:length
   #:to-simple-array
   #:reshape
   #:flatten
   #:squeeze
   #:expand-dims
   ;; #:transpose ; not yet

   ;; 3aref
   #:aref
   #:invalid-array-index-error
   #:invalid-array-index-error-axis
   #:invalid-array-index-error-shape
   #:invalid-array-index-error-subscripts

   ;; 3arange
   #:arange
   #:linspace

   ;; 3array
   #:asarray

   ;; 3copy
   #:copy
   #:astype

   ;; 3zeros
   #:zeros
   #:ones
   #:empty
   #:full
   #:empty-like
   #:full-like
   #:zeros-like
   #:ones-like

   ;; 4concatenate
   #:concatenate
   #:stack

   ;; 4split
   #:unstack

   ;; 5numeric
   #:map
   #:map-into
   #:map-array
   #:map-array-into
   #:broadcast

   #:expt
   
   #:sin #:cos #:tan
   #:asin #:acos #:atan
   #:sinh #:cosh #:tanh
   #:exp #:log
   #:abs #:signum
   #:1+ #:1-
   #:cis #:conjugate #:phase #:realpart #:imagpart
   ;; #:complex
   #:numerator #:denominator
   #:logcount #:integer-length
   ;; #:float

   #:-
   #:+
   #:*
   #:/
   #:max
   #:min
   #:clip
   #:=
   #:<
   #:>
   #:<=
   #:>=
   #:/=

   #:square
   #:sqrt
   #:log2
   
   #:logand  
   #:logandc1
   #:logandc2
   #:logeqv  
   #:logior  
   #:lognand 
   #:lognor  
   #:logorc1 
   #:logorc2 
   #:logxor
   #:lognot
   
   #:+ #:++ #:+++
   #:* #:** #:***
   #:/ #:// #:///

   #:mod      
   #:rem      
   #:round    
   #:floor    
   #:ceiling  
   #:truncate 
   #:fround   
   #:ffloor           
   #:fceiling 
   #:ftruncate

   ;; 5reduce
   #:reduce-array
   #:sum
   #:prod
   #:amax
   #:amin
   #:mean
   #:variance
   #:standard-deviation
   #:var
   #:stdev
   #:avg
   #:onehot
   #:argmax
   #:argmin
   #:histogram

   ;; where
   #:array-index-from-row-major-index
   #:argwhere
   #:where
   #:nonzero
   #:take
   
   ;; 5random
   #:shuffle
   #:bernoulli
   #:bernoulli-like
   #:beta
   #:binomial
   #:chisquare
   #:dirichlet
   #:exponential
   #:f
   #:gamma
   #:geometric
   #:gumbel
   #:hypergeometric
   #:laplace
   #:logistic
   #:lognormal
   #:logseries
   #:multinomial
   #:multivariate-normal
   #:negative-binomial
   #:noncentral-chisquare
   #:noncentral-f
   #:normal
   #:pareto
   #:poisson
   #:power
   #:rayleigh
   #:standard-cauchy
   #:standard-exponential
   #:standard-gamma
   #:standard-normal
   #:standard-t
   #:triangular
   #:uniform
   #:vonmises
   #:wald
   #:weibull
   #:zipf

   ;; 6 linarg
   #:einsum
   #:transpose
   #:matmul
   #:matmul*
   #:dot
   #:inner
   #:vdot
   #:outer
   #:kron
   #:eye
   #:diag
   #:tri
   #:tril
   #:triu
   #:vander

   )
  (:documentation "External package for storing the exported symbols."))

(uiop:define-package :numcl.impl
    (:shadow :let :let* :do :do* :locally)
    (:shadow :@)                        ; accidental import from trivia
    (:mix :cl                           ; want to use cl:aref
          :numcl.exported
          :alexandria                   ; but not want to use alexandria:flatten etc
          :iterate)                     ; same for iterate; for SUM
  ;;  :magicl
    (:use :trivia :type-r :constantfold :gtype :specialized-function)
    (:documentation "Internal package for implementing numcl."))

(uiop:define-package :numcl
    (:mix :numcl.exported :cl)
  (:reexport :numcl.exported)
  (:reexport :cl)
  (:documentation "
NUMCL defines several symbols which have the same name as the corresponding CL
symbols. We call them **conflicting symbols**.  To avoid the confusion in the code
base, we use 3 packages: NUMCL.EXPORTED, NUMCL.IMPL, NUMCL.

This package replaces COMMON-LISP package by shadowing-import symbols from
NUMCL.EXPORTED on top of COMMON-LISP package.
"))

#+sbcl
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
