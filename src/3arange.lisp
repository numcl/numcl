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

;; (defpythonfun (start stop step &optional type)
    

;; (defun arange (&rest args &key type)
;;   (print args))
;; 
;; (print (arange 1 2 3 :type 'fixnum))
;; (print (arange 1 2 :type 'fixnum))
;; (print (arange 1 :type 'fixnum))
;; (print (arange 1 2 3))
;; (print (arange 1 2))
;; (print (arange 1))

(declaim (inline %arange))
(defun %arange (start stop step type)
  (let* ((length (max 0 (floor (- stop start) step))))
    (multiple-value-bind (a base) (%make-array length :element-type type)
      (let ((tmp start))
        (dotimes (i length)
          (setf (aref base i) tmp)
          (incf tmp step)))
      (values a base))))

(declaim (inline %%arange))
(defun %%arange (start stop step type)
  (%arange (%coerce start type)
           stop                         ; stop does not need to be coerced
           (%coerce step  type)
           type))

(constantfold %arange-infer-type)
(declaim (inline %arange-infer-type))
(defun %arange-infer-type (start stop step)
  (let* ((head (float-contagion
                (type-of step)
                (float-contagion (type-of start) (type-of stop)))))
    (upgraded-array-element-type
     ;; without it, the compiler complains when START is non-0
     `(,head ,(%coerce (min start stop) head)
             ,(%coerce (max start stop) head)))))

(declaim (inline arange))
(defun arange (&rest args)
  "Arange's argument signature is irregular, following the API of numpy.
The interpretation of its arguments depends on the number of arguments.

 (arange stop            &key type)
 (arange start stop      &key type)
 (arange start stop step &key type)

Don't worry, we provide a compiler-macro to avoid the runtime dispatch.
"
  (ematch args
    ((list stop :type type)
     (%%arange 0 stop 1 type))
    
    ((list start stop :type type)
     (%%arange start stop 1 type))

    ((list start stop step :type type)
     (%%arange start stop step type))

    ((list stop)
     (%%arange 0 stop 1 (%arange-infer-type 0 stop 1)))
    
    ((list start stop)
     (%%arange start stop 1 (%arange-infer-type start stop 1)))

    ((list start stop step)
     (%%arange start stop step (%arange-infer-type start stop step)))))

(define-compiler-macro arange (&rest args &environment env)
  (ematch args
    ((list stop :type type)
     `(%%arange 0 ,stop 1 ,type))
    
    ((list start stop :type type)
     `(%%arange ,start ,stop 1 ,type))

    ((list start stop step :type type)
     `(%%arange ,start ,stop ,step ,type))

    ((list stop)
     (if (every (rcurry #'constantp env) args)
         `(%%arange 0 ,stop 1 ',(%arange-infer-type 0 stop 1))
         (once-only (stop)
           `(%%arange 0 ,stop 1 (%arange-infer-type 0 ,stop 1)))))
    
    ((list start stop)
     (if (every (rcurry #'constantp env) args)
         `(%%arange ,start ,stop 1 ',(%arange-infer-type start stop 1))
         (once-only (start stop)
           `(%%arange ,start ,stop 1 (%arange-infer-type ,start ,stop 1)))))

    ((list start stop step)
     (if (every (rcurry #'constantp env) args)
         `(%%arange ,start ,stop ,step ',(%arange-infer-type start stop step))
         (once-only (start stop step)
           `(%%arange ,start ,stop ,step (%arange-infer-type ,start ,stop ,step)))))))

#+(or)
(defun fn ()
  ;; (arange 5.0)
  ;; (arange 1.0 5.0)
  (arange 1 5)
  )

;; unfortunately it cannot infer this; must to be a constant
#+(or)
(defun fn (a)
  (declare (single-float a))
  (arange a))

#+(or)
(progn
  (defun fn1 ()
    (arange 5))
  (defun fn2 ()
    (arange 5.0))
  (defun fn3 ()
    (arange 5 :type 'fixnum))
  (defun fn4 ()
    (arange 5.0 :type 'fixnum))
  (defun fn ()
    (declare (optimize (speed 3)))
    (arange 5.0))
  (defun mytype () 'float) 
  ;; (defun fn ()
  ;;   (declare (optimize (speed 1)))
  ;;   ;; non-constant type
  ;;   (arange 5.0 :type (mytype)))
  )

;; benchmarking

#+(or)
(progn
  (defun benchmark1 ()
    (loop repeat 1000 do (arange 200000.0)))
  (print :benchmark1)
  (benchmark1)
  (time (benchmark1))
  (defun benchmark2 (a)
    (loop repeat 1000 do (arange a)))
  (print :benchmark2)
  (benchmark2 1.0)
  (time (benchmark2 200000.0))
  (defun benchmark3 (a b)
    (loop repeat 1000 do (arange a b)))
  (print :benchmark3)
  (benchmark3 0.0 1.0)
  (time (benchmark3 0.0 200000.0))
  (disassemble #'last-specialized-function))



(declaim (inline linspace))
(defun linspace (start stop length &key type endpoint)
  (declare (fixnum length))
  (let* ((length (if endpoint (1+ length) length))
         (step (/ (- stop start) length)))
    (if type
        (arange start stop step :type type)
        (arange start stop step))))

#+(or)
(progn
  (defun fn ()
    (linspace 1 5 3)))

;; logspace --- = exp (linspace start stop), skip

;; geomspace --- = exp (linspace (log start) (log stop)), skip

