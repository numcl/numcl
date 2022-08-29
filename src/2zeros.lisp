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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +zeros-doc+
    "Equivalent of the same function in numpy. Note the default `type` difference.

+ `empty` : does not explicitly fill the array. In an unsafe compiler setting,
  junk value may appear.
+ `full`  : fill the array with a certain value.
+ `zeros`, `ones` : fill the array with zeros / ones. `type` affects the actual
  value being filled.
+ `X-like` : similar to above functions, but takes another array and returns the
  array of the same shape.
"))

(declaim (inline empty empty-like
                 full  full-like
                 zeros zeros-like
                 ones  ones-like))

(defun empty (shape &key (type 'bit))
  #.+zeros-doc+
  (%make-array shape
               :element-type type))

(defun full (shape value &key (type (type-of value)))
  #.+zeros-doc+
  (%make-array shape
               :element-type type
               :initial-element (%coerce value type)))

(defun zeros (shape &key (type 'bit))
  #.+zeros-doc+
  (full shape (%coerce 0 type) :type type))

(defun ones (shape &key (type 'bit))
  #.+zeros-doc+
  (full shape (%coerce 1 type) :type type))



(defun empty-like (array &key (type (array-element-type array)))
  #.+zeros-doc+
  (empty (shape array) :type type))

(defun full-like (array value &key (type (array-element-type array)))
  #.+zeros-doc+
  (full (shape array) (%coerce value type) :type type))

(defun zeros-like (array &key (type (array-element-type array)))
  #.+zeros-doc+
  (full-like array (%coerce 0 type) :type type))

(defun ones-like (array &key (type (array-element-type array)))
  #.+zeros-doc+
  (full-like array (%coerce 1 type) :type type))

#+(or)
(defun fn ()
  (ones '(1 2 3)))


;; (defun identity (shape &key (type 'bit))
;;   )


