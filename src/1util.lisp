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




(defmacro in-current-package (&body body)
  "Bind *package* to the load-time-value of *package*.
Used frequently for controlling the default package for the reader."
  `(let ((*package* (load-time-value *package*)))
     ,@body))


(defmacro inline-except-toplevel ((&key) &body defun-form)
  "Takes a DEFUN form, then return a PROGN containing two versions of the defun form:
One is the original form, and another, which have the same definition (arguments and body)
but has the name prefixed with `inlinable-`. This inlinable- version is declared inline.
Furthermore, it defines the compiler-macro for the original function
which expands to the inlinable version when `*compile-file-pathanme*` is set in the compile time.
This way it prevents inlining in the REPL.

This macro is important for certain functions (e.g. MATMUL) that contain delayed compilation
using specialized-function, mainly EINSUM.
We want those functions to be inline-expanded (to expose EINSUM) which is later fused with
other EINSUMs. However, when the function is used on the toplevel form (e.g. REPL)
without this macro, it always expands to EINSUM and thus always creates a new function table.
"
  (ematch (first defun-form)
    ((list* 'defun name args body)
     (multiple-value-bind (body2 decl2 doc) (parse-body body :documentation t)
       (declare (ignore body2 decl2))
       (let ((inlinable-name (symbolicate 'inlinable- name)))
         (with-gensyms (wholevar args-var)
           `(progn
              (declaim (inline ,inlinable-name))
              (defun ,inlinable-name ,args ,@body)
              ,(first defun-form)
              (define-compiler-macro ,name (&whole ,wholevar ,@args)
                ,doc
                (declare (ignore ,@(set-difference
                                    (mapcar (compose #'first #'ensure-list) args)
                                    lambda-list-keywords)))
                (if *compile-file-pathname*
                    (match ,wholevar
                      ((or (list* 'funcall (list 'function (eq ',name)) ,args-var)
                           (list* (eq ',name) ,args-var))
                       (format t "~&; inlining ~a" ,wholevar)
                       `(,',inlinable-name ,@,args-var))
                      (_
                       (error "what the heck?")))
                    ,wholevar)))))))))
