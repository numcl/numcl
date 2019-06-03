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

#|

This file contains a constant-foldable variant of the standard functions, such
as list, vector. The functions mainly consists of sequense function.

Standard functions, e.g. list, require that a new object (a list) is created every time.
However, there are many cases in numcl that it is preferable to constant-fold them
assuming that the result value is only used as a compile-time constant.

These are typically the functions that are declared flushable but not foldable in SBCL.

We do not include copier functions.

New functions are named with an additional C in the beginning. For example,
CLIST is a constant-foldable version of LIST.

We thank Baggers' TAMEI library for the reference database.

|#

(defmacro define-foldable-version (symbol)
  (let ((csymbol (symbolicate 'c symbol)))
    `(progn
       (constantfold ,csymbol)
       (defun ,csymbol (&rest args) (apply #',symbol args)))))

;; Arrays --- required to allocate a new array
(define-foldable-version make-array)
(define-foldable-version vector)
(define-foldable-version bit-and)
(define-foldable-version bit-andc1)
(define-foldable-version bit-andc2)
(define-foldable-version bit-eqv)
(define-foldable-version bit-ior)
(define-foldable-version bit-nand)
(define-foldable-version bit-nor)
(define-foldable-version bit-not)
(define-foldable-version bit-orc1)
(define-foldable-version bit-orc2)
(define-foldable-version bit-xor)

;; Conses --- functions required to cons a new cons cells
(define-foldable-version mapcar)
(define-foldable-version cons)
(define-foldable-version adjoin)
(define-foldable-version append)
(define-foldable-version acons)
(define-foldable-version pairlis)
(define-foldable-version sublis)
(define-foldable-version list)
(define-foldable-version list*)
(define-foldable-version make-list)
(define-foldable-version maplist)
(define-foldable-version subst)
(define-foldable-version subst-if)
(define-foldable-version subst-if-not)
(define-foldable-version intersection)
(define-foldable-version set-difference)
(define-foldable-version set-exclusive-or)
(define-foldable-version union)
(define-foldable-version ldiff)

;; Sequences
(define-foldable-version make-sequence)
(define-foldable-version subseq)
(define-foldable-version concatenate)
(define-foldable-version map)
(define-foldable-version reduce)
(define-foldable-version reverse)
(define-foldable-version remove)
(define-foldable-version remove-duplicates)
(define-foldable-version remove-if)
(define-foldable-version remove-if-not)
(define-foldable-version substitute)
(define-foldable-version substitute-if)
(define-foldable-version substitute-if-not)

;; Strings
(define-foldable-version make-string)
(define-foldable-version string)
(define-foldable-version string-capitalize)
(define-foldable-version string-downcase)
(define-foldable-version string-left-trim)
(define-foldable-version string-right-trim)
(define-foldable-version string-trim)
(define-foldable-version string-upcase)

