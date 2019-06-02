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

;;;; zero, maximum, minimum, one

(constantfold zero-value)
(defun zero-value (type)
  (ematch type
    ((or (ratio-type) (float-type))
     (case *numcl-default-float-format*
       (short-float  0.0s0)
       (single-float 0.0f0)
       (double-float 0.0d0)
       (long-float   0.0l0)))
    ((long-float-type)           0.0l0)
    ((double-float-type)         0.0d0)
    ((single-float-type)         0.0f0)
    ((short-float-type)          0.0s0)
    (_                           0)))

(constantfold one-value)
(defun one-value (type)
  (ematch type
    ((or (ratio-type) (float-type))
     (case *numcl-default-float-format*
       (short-float  1.0s0)
       (single-float 1.0f0)
       (double-float 1.0d0)
       (long-float   1.0l0)))
    ((long-float-type)           1.0l0)
    ((double-float-type)         1.0d0)
    ((single-float-type)         1.0f0)
    ((short-float-type)          1.0s0)
    (_                           1)))

(constantfold most-positive-value)
(defun most-positive-value (type)
  (ematch type
    ((fixnum-subtype _ high) high)
    ((or (ratio-type) (float-type) (integer-subtype))
     (case *numcl-default-float-format*
       (short-float  most-positive-short-float)
       (single-float most-positive-single-float)
       (double-float most-positive-double-float)
       (long-float   most-positive-long-float)))
    ((long-float-type)           most-positive-long-float)
    ((double-float-type)         most-positive-double-float)
    ((single-float-type)         most-positive-single-float)
    ((short-float-type)          most-positive-short-float)
    (_                           most-positive-long-float)))

(constantfold most-negative-value)
(defun most-negative-value (type)
  (ematch type
    ((fixnum-subtype low _) low)
    ((or (ratio-type) (float-type) (integer-subtype))
     (case *numcl-default-float-format*
       (short-float  most-negative-short-float)
       (single-float most-negative-single-float)
       (double-float most-negative-double-float)
       (long-float   most-negative-long-float)))
    ((long-float-type)           most-negative-long-float)
    ((double-float-type)         most-negative-double-float)
    ((single-float-type)         most-negative-single-float)
    ((short-float-type)          most-negative-short-float)
    (_                           most-negative-long-float)))

