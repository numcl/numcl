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


;; copy

(declaim (inline copy))
(defun copy (array)
  (multiple-value-bind (a base-array) (empty-like array)
    (replace base-array array)
    (values a base-array)))

;; astype

(declaim (inline astype))
(defun astype (array type)
  (multiple-value-bind (a base-array2) (empty-like array :type type)
    (map-into base-array2 (lambda (x) (%coerce x type)) array)
    (values a base-array2)))
