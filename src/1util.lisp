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


(defmacro printing (&body body)
  "debugging function -- A progn that prints each form in the body"
  `(progn
     ,@(mapcar (lambda (form)
                 (with-gensyms (result)
                   (match form
                     ((type atom)
                      `(format *trace-output* "~&~<; ~@;~10,,,a~_ -> ~_~a~:>~%" (list ',form ,form)))
                     ((list* head args-form)
                      (let ((args-vars (make-gensym-list (length args-form))))
                        `(let ,(mapcar #'list args-vars args-form)
                           (let ((,result (,head ,@args-vars))
                                 (*print-level* 4)
                                 (*print-length* 4))
                             (format *trace-output* "~&~<; ~@;~10,,,a~_ -> ~_~a~_ -> ~_~a~:>~%"
                                     (list ',form
                                           (list ',head ,@args-vars)
                                           ,result))
                             ,result)))))))
               body)))



