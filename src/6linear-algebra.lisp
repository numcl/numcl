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


(declaim (inline einsum))
(defun einsum (subscripts &rest args)
  #.*einsum-documentation*
  (apply (memoized-einsum-function (einsum-normalize-subscripts subscripts))
         args))

(function-cache:defcached memoized-einsum-function (normalized-subscripts)
  (compile nil (einsum-lambda normalized-subscripts)))

(define-compiler-macro einsum (&whole whole subscripts &rest args)
  (match subscripts
    ((list 'quote subscripts)
     `(funcall ,(einsum-lambda (einsum-normalize-subscripts subscripts))
               ,@args))
    (_
     whole)))

(defun safe-string= (a b)
  (and (typep a 'string-designator)
       (typep b 'string-designator)
       (string= a b)))

(defun einsum-normalize-subscripts (subscripts)
  "Normalizes the input to the einsum.
It first 'explodes' each spec into the list form.
It then generates the default output form, if missing.

It then translates the index symbols into a number
based on the order of appearance; This should make
the specs with the different symbols into the canonical form,
e.g. `(ij jk -> ik)` and `(ik kj -> ij)` both results in something equivalent to
 `((0 1) (1 2) -> (0 2))`.

It then inserts the default transforms, if missing.

The value returned is a plist of :inputs, :transforms, :outputs. 
For example, `(einsum-normalize-subscripts '(ik kj -> ij))` returns

```lisp
  (:inputs     ((0 1) (1 2))
   :transforms ((+ @1 (* $1 $2)))
   :outputs    ((0 2)))
```

"
  (flet ((explode (s)
           (typecase s
             (list (assert (every #'symbolp s)) s)
             (symbol (iter (for c in-vector (symbol-name s))
                           (assert (alpha-char-p c) nil
                                   'simple-type-error
                                   :format-control "Tried to make a spec from a non-alpha char ~a"
                                   :format-arguments (list c))
                           (collecting (intern (string c)))))))
         (indices (specs)
           (let (list)
             (iter (for spec in specs)
                   (iter (for index in spec)
                         (pushnew index list)))
             (nreverse list))))
    (ecase (count '-> subscripts :test #'safe-string=)
      (0
       (let* ((i-specs (mapcar #'explode subscripts))
              (indices  (indices i-specs))
              (o-specs (list (sort (copy-list indices) #'string<)))

              (alist (mapcar #'cons indices (iota (length indices))))
              (i-specs-num (sublis alist i-specs))
              (o-specs-num (sublis alist o-specs))
              
              (i-len (length i-specs))
              (o-len (length o-specs))
              (transforms
               (iter (for o from 1 to o-len)
                     (collect
                         `(+ ,(@ o) (* ,@(mapcar #'$ (iota i-len :start 1))))))))
         (list :inputs i-specs-num :transforms transforms :outputs o-specs-num)))
      (1
       (let* ((pos (position '-> subscripts :test #'safe-string=))
              (i-specs (mapcar #'explode (subseq subscripts 0 pos)))
              (o-specs (mapcar #'explode (or (subseq subscripts (1+ pos)) '(nil)))) ; default

              (indices (indices (append i-specs o-specs)))
              (alist (mapcar #'cons indices (iota (length indices))))
              (i-specs-num (sublis alist i-specs))
              (o-specs-num (sublis alist o-specs))

              (i-len (length i-specs))
              (o-len (length o-specs))
              (transforms
               (iter (for o from 1 to o-len)
                     (collecting
                      `(+ ,(@ o) (* ,@(mapcar #'$ (iota i-len :start 1))))))))
         (list :inputs i-specs-num :transforms transforms :outputs o-specs-num)))
      (2
       (let* ((pos (position '-> subscripts :test #'safe-string=))
              (i-specs (mapcar #'explode (subseq subscripts 0 pos)))
              (pos2 (position '-> subscripts :test #'safe-string= :start (1+ pos)))
              (transforms (subseq subscripts (1+ pos) pos2))
              (o-specs (mapcar #'explode (or (subseq subscripts (1+ pos2)) '(nil)))) ; default

              (indices (indices (append i-specs o-specs)))
              (alist (mapcar #'cons indices (iota (length indices))))
              (i-specs-num (sublis alist i-specs))
              (o-specs-num (sublis alist o-specs))

              (i-len (length i-specs))
              (o-len (length o-specs))
              (transforms
               (iter (for o from 1 to o-len)
                     (collecting
                      (or (nth (1- o) transforms)
                          `(+ ,(@ o) (* ,@(mapcar #'$ (iota i-len :start 1)))))))))
         (list :inputs i-specs-num :transforms transforms :outputs o-specs-num))))))

(defun ? (i) (in-current-package (symbolicate '? (princ-to-string i)))) ; index limit var
(defun $ (i) (in-current-package (symbolicate '$ (princ-to-string i)))) ; input element var
(defun @ (i) (in-current-package (symbolicate '@ (princ-to-string i)))) ; output element var
(defun & (i) (in-current-package (symbolicate '& (princ-to-string i)))) ; index iteration var
(defun map-specs (fn specs)
  (iter (for spec in specs)
        (collecting
         (iter (for index in spec)
               (collecting (funcall fn index))))))

(defun einsum-parse-subscripts (normalized-subscripts)
  (match normalized-subscripts
    ((plist :inputs     i-specs
            :transforms transforms
            :outputs    o-specs)
     (let* ((i-flat (remove-duplicates (flatten i-specs)))
            (o-flat (remove-duplicates (flatten o-specs)))
            (i-len (length i-specs))
            (o-len (length o-specs))
            (iter-specs
             (sort-locality (union i-flat o-flat) (append i-specs o-specs))))
       (assert (subsetp o-flat i-flat)
               nil
               "The output spec contains ~a which are not used in the input specs:~% input spec: ~a~%output spec: ~a"
               (set-difference o-flat i-flat) i-flat o-flat)
       (values
        i-specs
        (make-gensym-list i-len "I")
        (mapcar #'$ (iota i-len :start 1))

        o-specs
        (make-gensym-list o-len "O")
        (mapcar #'@ (iota o-len :start 1))

        iter-specs
        transforms)))))

(deftype index () `(integer 0 (,array-dimension-limit)))

(defun einsum-lambda (normalized-subscripts)
  "Takes a normalized-subscripts and returns a lambda form that iterates over it."
  (multiple-value-bind (i-specs i-vars i-evars
                        o-specs o-vars o-evars iter-specs transforms)
      (einsum-parse-subscripts normalized-subscripts)
    (with-gensyms (o-types)
      `(lambda (,@i-vars &optional ,@o-vars)
         (resolving
           ,@(iter (for var in i-vars)
                   (for spec in i-specs)
                   (collecting
                    `(declare (gtype (array * ,(mapcar #'? spec)) ,var))))
           (let* ((,o-types (einsum-output-types
                             ',transforms ',i-evars ',o-evars ,@i-vars))
                  ,@(iter (for o-var     in o-vars)
                          (for o-spec    in o-specs)
                          (for o from 0)
                          (collecting
                           `(,o-var
                             (or ,o-var
                                 (zeros (list ,@(mapcar #'? o-spec)) :type (nth ,o ,o-types)))))))
             (resolving
               ,@(iter (for var in o-vars)
                       (for spec in o-specs)
                       (collecting
                        `(declare (gtype (array * ,(mapcar #'? spec)) ,var))))
               (let ,(iter (for var in (append i-vars o-vars))
                           (collecting
                            ;; extract the base array
                            `(,var (array-displacement ,var))))
                 (specializing (,@i-vars ,@o-vars) ()
                   (declare (optimize (speed 2) (safety 0)))
                   (declare (type index ,@(mapcar #'? iter-specs)))
                   ,(einsum-body *compiler* iter-specs i-specs o-specs i-vars o-vars i-evars o-evars transforms)))
               (values ,@(mapcar (lambda (var) `(ensure-singleton ,var))
                                 o-vars)))))))))

(defun einsum-output-types (transforms i-evars o-evars &rest arrays)
  "Try to simulate the range for 10 iterations; Stop if it converges.
Otherwise call float-substitution and simplify integers to fixnums."
  (iter (with i-types = (mapcar #'array-element-type arrays))
        (repeat 10)
        (for o-types
             initially (make-list (length o-evars)
                                  :initial-element
                                  '(integer 0 0))
             then      (iter (with alist =
                                   (mapcar #'cons
                                           (append i-evars o-evars)
                                           (append i-types o-types)))
                             (for transform in transforms)
                             (collecting
                              (interpret-type (sublis alist transform)))))
        ;; (print o-types)
        (for o-types-prev previous o-types)
        (until (equal o-types o-types-prev))
        (finally
         (if (equal o-types o-types-prev)
             (return o-types)
             (return (mapcar (lambda (type) (float-substitution type :int-result 'fixnum))
                             o-types))))))

(defun spec-depends-on (spec iter-specs)
  (intersection iter-specs spec))

(defun sort-locality (indices subscripts)
  (sort (copy-list indices)
        (lambda (index1 index2)
          (locality< index1 index2 subscripts))))

(defun locality< (index1 index2 subscripts)
  "Returns true when index1 is less local than index2; i.e. index2 is more local"
  (assert (not (eq index1 index2)))
  (flet ((score (index1 index2)
           ;; the number of ordering violations
           ;; for looping index1 first 
           (iter (for spec in subscripts)
                 (for pos1 = (position index1 spec))
                 (for pos2 = (position index2 spec))
                 (counting
                  ;; count the violation
                  (and pos1 pos2 (< pos2 pos1))))))
    (< (score index1 index2)
       (score index2 index1))))

;; (einsum '(ij jk -> ik) a b)             ; -> becomes an ijk loop
;; (einsum '(ik kj -> ij) a b)             ; -> becomes an ikj loop
