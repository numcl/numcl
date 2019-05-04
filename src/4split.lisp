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




;; unstack

(defun unstack (array &key (axis 0))
  (let* ((type (array-element-type array))
         (dims (shape array))
         (axis (mod axis (length dims)))
         (len  (elt dims axis))
         
         (dims-before (reduce #'* dims :start 0 :end axis))
         (dims-after  (reduce #'* dims :start (1+ axis)))
         
         (simplified
          (reshape array (list dims-before
                               len
                               dims-after)))
         (tmp
          (empty (list len
                       dims-before
                       dims-after)
                 :type type))
         
         (dims-each (append (subseq dims 0 axis)
                            (subseq dims (1+ axis))))
         (arrays
          (iter (for i from 0 below len)
                (collecting
                 (%make-array dims-each
                             :element-type type
                             :displaced-to tmp
                             :displaced-index-offset (* i dims-before dims-after))))))

    
    (iter (for dim-axis below len)
          (iter (for i below dims-before)
                (iter (for k below dims-after)
                      (setf (aref tmp dim-axis i k)
                            (aref simplified i dim-axis k)))))

    arrays))


;; (unstack (stack (list (zeros '(2 2)) (ones '(2 2)))))
;; (unstack (stack (list (zeros '(2 2)) (ones '(2 2))) :axis 0))
;; (unstack (stack (list (zeros '(2 2)) (ones '(2 2))) :axis 1))
;; (unstack (stack (list (zeros '(2 2)) (ones '(2 2))) :axis 2))





