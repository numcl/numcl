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
  (if (constantp subscripts)
      `(funcall ,(einsum-lambda (einsum-normalize-subscripts (eval subscripts)))
                ,@args)
      whole))

(defun safe-string= (a b)
  (and (typep a 'string-designator)
       (typep b 'string-designator)
       (string= a b)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct einsum-specs
    "A temporary structure holding the information for the einsum compiler.
* `iter-specs`         : A list of integers that defines the nested loops.
                         For example, (0 1 2) for the ijk loop in gemm.

* `i-specs`, `o-specs` : A list of lists of integers that specifies the indices
                         for each input/output array. Each list should be a
                         subset of iter-specs.
                         For example, ((0 1) (1 2) (0 2)) for the ijk loop in gemm.

* `transforms`         : A list of forms. One form is for each output array.
                         Each form is evaluated in each loop and the value is
                         assigned to the array element.
"
    iter-specs
    transforms
    i-specs i-options
    o-specs o-options))

(defun einsum-normalize-subscripts (subscripts)
  (%einsum-normalize-subscripts
   (ecase (count '-> subscripts :test #'safe-string=)
     (0
      (list :i-specs subscripts))
     (1
      (let ((pos1 (position '-> subscripts :test #'safe-string=)))
        (list :i-specs (subseq subscripts 0 pos1)
              :o-specs (or (subseq subscripts (1+ pos1)) '(())))))
     (2
      (let* ((pos1 (position '-> subscripts :test #'safe-string=))
             (pos2 (position '-> subscripts :test #'safe-string= :start (1+ pos1))))
        (list :i-specs    (subseq subscripts 0 pos1)
              :transforms (subseq subscripts (1+ pos1) pos2)
              :o-specs    (or (subseq subscripts (1+ pos2)) '(())))))
     (3
      (let* ((pos1 (position '-> subscripts :test #'safe-string=))
             (pos2 (position '-> subscripts :test #'safe-string= :start (1+ pos1)))
             (pos3 (position '-> subscripts :test #'safe-string= :start (1+ pos2))))
        (list :i-specs    (subseq subscripts 0 pos1)
              :transforms (subseq subscripts (1+ pos1) pos2)
              :o-specs    (or (subseq subscripts (1+ pos2) pos3) '(()))
              :i-options  (subseq subscripts (1+ pos3)))))
     (4
      (let* ((pos1 (position '-> subscripts :test #'safe-string=))
             (pos2 (position '-> subscripts :test #'safe-string= :start (1+ pos1)))
             (pos3 (position '-> subscripts :test #'safe-string= :start (1+ pos2)))
             (pos4 (position '-> subscripts :test #'safe-string= :start (1+ pos3))))
        (list :i-specs    (subseq subscripts 0 pos1)
              :transforms (subseq subscripts (1+ pos1) pos2)
              :o-specs    (or (subseq subscripts (1+ pos2) pos3) '(()))
              :i-options  (subseq subscripts (1+ pos3) pos4)
              :o-options  (subseq subscripts (1+ pos4))))))))

(defun %einsum-normalize-subscripts (subscripts)
  "Normalizes the input to the einsum.
It first 'explodes' each spec into the list form.
It then generates the default output form, if missing.

It then translates the index symbols into a number
based on the order of appearance; This should make
the specs with the different symbols into the canonical form,
e.g. `(ij jk -> ik)` and `(ik kj -> ij)` both results in something equivalent to
 `((0 1) (1 2) -> (0 2))`.

The subscripts are converted into numbers in order to cache the resulting compiled function
when einsum is called with a dynamically generated subscript.

It then inserts the default transforms, if missing.

The value returned is a plist of :i-specs, :transforms, :o-specs.
For example, `(einsum-normalize-subscripts '(ik kj -> ij))` returns

```lisp
  (:i-specs    ((0 1) (1 2))
   :transforms ((+ @1 (* $1 $2)))
   :o-specs    ((0 2)))
```

Broadcasting can be specified by a hyphen. For example, '(-k -k -> -k)
is equivalent to numpy's ellipses '...k,...k->...k' .
In the returned list, indices corresponding to the broadcasted axes are -1.
"
  (flet ((explode (s)
           (typecase s
             (list   (iter (for c in s)
                           (assert (symbolp c))
                           (collecting c)))
             (symbol (iter (for c in-vector (symbol-name s))
                           (assert (or (char= #\- c) (alpha-char-p c)) nil
                                   'simple-type-error
                                   :format-control "Tried to make a spec from a non-alpha char ~a"
                                   :format-arguments (list c))
                           (collecting (intern (string c)
                                               (symbol-package s)))))))
         (make-map (indices)
           (iter (for index in indices)
                 (for i from 0)
                 (collecting
                  (cons index
                        (if (safe-string= index '-)
                            -1
                            i))))
           #+(or)                       ; old code before hyphens were introduced
           (mapcar #'cons indices (iota (length indices))))
         (indices (specs)
           (let (list)
             (iter (for spec in specs)
                   (iter (for index in spec)
                         (pushnew index list)))
             (nreverse list))))
    (destructuring-bind (&key i-specs o-specs transforms i-options o-options &allow-other-keys) subscripts
      (let* ((i-specs (mapcar #'explode i-specs))
             (indices  (indices i-specs)) ; list of symbols
             (o-specs (if o-specs
                          (mapcar #'explode o-specs)
                          (list (sort (copy-list indices) #'string<))))
             (alist (make-map indices))
             (i-specs-num (sublis alist i-specs)) ; list of numbers
             (o-specs-num (sublis alist o-specs)) ; list of numbers
             
             (i-len (length i-specs))
             (o-len (length o-specs))
             (transforms
              (if transforms
                  (sublis (iter (for elem in (remove-duplicates (alexandria:flatten transforms)))
                                (match elem
                                  ((symbol name)
                                   (when (and (member (aref name 0) '(#\@ #\$))
                                              (every #'digit-char-p (subseq name 1)))
                                     (collecting (cons elem (intern name :numcl.impl)))))))
                          transforms)
                  (iter (for o from 1 to o-len)
                        (collect
                            `(+ ,(@ o) (* ,@(mapcar #'$ (iota i-len :start 1))))))))
             (i-options-full (make-list i-len))
             (o-options-full (make-list o-len))
             (i-flat (remove-duplicates (alexandria:flatten i-specs-num)))
             (o-flat (remove-duplicates (alexandria:flatten o-specs-num)))
             (iter-specs
              (sort-locality (union i-flat o-flat)
                             (append i-specs-num o-specs-num))))
        (replace i-options-full i-options) ;stops at the shorter sequence (ANSI)
        (replace o-options-full o-options) ;stops at the shorter sequence (ANSI)
        (setf i-options-full (sublis alist i-options-full))
        (setf o-options-full (sublis alist o-options-full))
        (assert (= o-len (length transforms)))
        (assert (subsetp o-flat i-flat)
                nil
                "The output spec contains ~a which are not used in the input specs:~% input spec: ~a~%output spec: ~a"
                (set-difference o-flat i-flat) i-flat o-flat)
        (make-einsum-specs :iter-specs iter-specs
                           :transforms transforms
                           :i-specs i-specs-num
                           :o-specs o-specs-num
                           :i-options i-options-full
                           :o-options o-options-full)))))

(defun ? (i) "Variable for the iteration limit"      (intern (format nil "?~a" i) :numcl.impl))
(defun $ (i) "Variable for the input array element"  (intern (format nil "$~a" i) :numcl.impl))
(defun @ (i) "Variable for the output array element" (intern (format nil "@~a" i) :numcl.impl))
(defun & (i) "Variable for the iteration count"      (intern (format nil "&~a" i) :numcl.impl))
(defun i-var (i) "Variable for the input array"      (intern (format nil "I~a" i) :numcl.impl))
(defun o-var (i) "Variable for the output array"     (intern (format nil "O~a" i) :numcl.impl))
(defun i-vars (specs) (mapcar #'i-var (iota (length specs))))
(defun o-vars (specs) (mapcar #'o-var (iota (length specs))))
(defun i-evars (specs) (mapcar #'$ (iota (length specs) :start 1)))
(defun o-evars (specs) (mapcar #'@ (iota (length specs) :start 1)))
(defun i-idx (i) "Variable for the 1D index of input array"  (in-current-package (symbolicate '$IDX (princ-to-string i))))
(defun o-idx (i) "Variable for the 1D index of output array" (in-current-package (symbolicate '@IDX (princ-to-string i))))
(defun i-idxs (specs) (mapcar #'i-idx (iota (length specs))))
(defun o-idxs (specs) (mapcar #'o-idx (iota (length specs))))

(defun map-specs (fn specs)
  (iter (for spec in specs)
        (collecting
         (iter (for index in spec)
               (collecting (funcall fn index))))))

#|

how to make it loop at a certain index?

example scenario: broadcast + between (2 1 5) array A and (5 5) array B.

first reshape B to (1 5 5).

(einsum '(ijk ijk ->  (+ $1 $2)  -> ijk) A B) ??

(einsum '(-k -k ->  (+ $1 $2)  -> -k) A B) ??

you will get an array C with shape (2 5 5).

(einsum '(ij jk -> ik) a b c)
(einsum '(i-j -jk -> ik-) a b c)
(einsum '(i-j -jk -> ik) a b c)
(einsum '(i-i i-i i-i -> -i) a b c)

(einsum '(i- -j -> ij) a b c)

(einsum '(i- i- -> i-) a b c)

(einsum '(ij jk -> (+ @1 (* $1 $2)) -> ik) a b c)

(einsum '(ii -> i ) a b)

(einsum '(i -> i) (ones 5))

(einsum '(ij jk -> (+ @1 (* $1 $2)) -> ik -> ((i :step 2)) ) a b c)

;; should error
(einsum '(ij jk -> (+ @1 (* $1 $2)) -> ik -> ((i :step *special-value*)) ) a b c)

|#

(defvar *compiler* :common-lisp)

(defgeneric einsum-body (*compiler* einsum-specs)
  (:documentation
   " 
* `*compiler*` : Special variable (supposed to be a keyword) used for
  dispatching the compilation scheme.

* `einsum-specs` : einsum-specs structure. "))

(defun shape-resolver (vars specs &optional out-p)
  "
out-p: Produces the binding for output arrays."
  (iter outer
        (with unifiers-plist = nil)
        (for var  in vars)
        (for spec in specs)
        ;; collect axis information forward until broadcast
        (iter (for dim in spec)
              (for dimvar = (gensym))
              (for i from 0)
              (in outer
                  ;; bind the axis size to dimvar
                  (collecting `(,dimvar (array-dimension ,var ,i))
                              into bindings)
                  ;; unify dimvar to the variable (? dim)
                  (push dimvar (getf unifiers-plist (? dim)))))
        ;; unify the collected informations, bind them to ?-variables
        (finally
         (return-from shape-resolver
           (append bindings
                   (iter (for (unified sources . rest) on unifiers-plist by #'cddr)
                         (collecting
                          (if out-p
                              `(,unified (progn (assert (= ,unified ,@sources)) ,unified))
                              `(,unified (progn (assert (= ,@sources)) ,(first sources)))))))))))

(defun %output-generator (o-specs o-vars i-specs i-vars transforms)
  (with-gensyms (o-types)
    (iter (for o-var     in o-vars)
          (for o-spec    in o-specs)
          (for o from 0)
          (when (first-iteration-p)
            ;; compute the output array type
            (collecting
             `(,o-types (einsum-output-types
                         ',transforms ',(i-evars i-specs) ',(o-evars o-specs) ,@i-vars))))
          ;; generate or reuse output arrays
          (collecting
           `(,o-var (or ,o-var (zeros (list ,@(mapcar #'? o-spec)) :type (nth ,o ,o-types))))))))

(defun einsum-lambda (einsum-specs)
  "Takes a normalized-subscripts and returns a lambda form that iterates over it."
  (ematch einsum-specs
    ((einsum-specs i-specs o-specs iter-specs transforms)
     (let ((i-vars (i-vars i-specs))
           (o-vars (o-vars o-specs)))
       `(lambda (,@i-vars &optional ,@o-vars)
          ;; resolve input array shapes
          (let* ,(shape-resolver i-vars i-specs)
            ;; generate or reuse output arrays
            (let* ,(%output-generator o-specs o-vars i-specs i-vars transforms)
              (let* ,(shape-resolver o-vars o-specs t)
                (let ,(iter (for var in (append i-vars o-vars))
                            (collecting
                              ;; extract the base array
                              `(,var (array-displacement ,var))))
                  (specializing (,@i-vars ,@o-vars) ()
                    (declare (optimize (speed 2) (safety 0)))
                    (declare (type index ,@(mapcar #'? (remove -1 iter-specs))))
                    ,(einsum-body *compiler* einsum-specs)))
                (values ,@(mapcar (lambda (var) `(ensure-singleton ,var))
                                  o-vars))))))))))

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
  "In each iteration, greedily select the earliest index.
However, it penalize removing the index from the same set of subscripts that
 the past indices are removed from."
  (let ((h (make-hash-table :test 'equal)))
    (labels ((score (index)
               (iter (for subscript in subscripts)
                     (for pos = (position index subscript))
                     (summing (if pos (1+ pos) 0))))
             (sticky-score (index)
               (gethash
                (iter (for subscript in subscripts)
                      (for i from 0)
                      (for pos = (position index subscript))
                      (when pos
                        (collecting i)))
                h
                0))
             ((setf sticky-score) (newval index)
               (setf (gethash
                      (iter (for subscript in subscripts)
                            (for i from 0)
                            (for pos = (position index subscript))
                            (when pos
                              (collecting i)))
                      h
                      0)
                     newval))
             
             (next-index ()
               (iter (for index in indices)
                     (for score =
                          (+ (score index)
                             (* (length indices) (sticky-score index))))
                     (finding index minimizing score))))
      (iter (while indices)
            ;; (print indices)
            ;; (print (hash-table-alist h))
            (for next = (next-index))
            (incf (sticky-score next))
            (collecting next)
            (setf indices (remove next indices))))))

;; (defun sort-locality (indices subscripts)
;;   (sort (copy-list indices)
;;         (lambda (index1 index2)
;;           (locality< index1 index2 subscripts))))
;; 
;; (defun locality< (index1 index2 subscripts)
;;   "Returns true when index1 is less local than index2; i.e. index2 is more local"
;;   (assert (not (eq index1 index2)))
;;   (flet ((score (index1 index2)
;;            ;; the number of ordering violations
;;            ;; for looping index1 first 
;;            (iter (for spec in subscripts)
;;                  (for pos1 = (position index1 spec))
;;                  (for pos2 = (position index2 spec))
;;                  (counting
;;                   ;; count the violation
;;                   (and pos1 pos2 (< pos2 pos1))))))
;;     (< (score index1 index2)
;;        (score index2 index1))))

;; (einsum '(ij jk -> ik) a b)             ; -> becomes an ijk loop
;; (einsum '(ik kj -> ij) a b)             ; -> becomes an ikj loop

;; sbcl does not automatically do this...
#+(or)
(defun fn (c)
  (declare ((mod 100) c))
  (dotimes (i 10 c)
    (dotimes (j 10)
      (setf c (+ (* i 10) j)))))

#+(or)
(defun fn2 (c)
  (declare ((mod 100) c))
  (dotimes (i 10 c)
    (let ((i10 (* i 10)))
      (dotimes (j 10)
        (setf c (+ i10 j))))))

;; checking the behavior of recent sbcl regarding
;; embedding the constant offset into the assembly e.g. [RAX+3].

;; SBCL is able to do it only when there is no boundary checking.
#+(or)
(defun fn3 (a i)
  (declare ((base-array (unsigned-byte 8) 100) a))
  (declare ((unsigned-byte 8) i))
  ;; Requires boundary checking since i could be > 100.
  ;; This does not get embedded:
  ;; 12:       4981F8C8000000   CMP R8, 200 <--- boundary checking
  ;; 19:       0F83A8000000     JNB L0      <--- jumps to the index error
  ;; 1F:       498BF8           MOV RDI, R8
  ;; 22:       48D1FF           SAR RDI, 1
  ;; 25:       0FB6543801       MOVZX EDX, BYTE PTR [RAX+RDI+1] <--- offset not embedded
  ;; 2A:       48D1E2           SHL RDX, 1
  ;; 2D:       498D7802         LEA RDI, [R8+2]
  ;; 31:       4881FFC8000000   CMP RDI, 200
  ;; 38:       0F8391000000     JNB L1
  ;; 3E:       48D1FF           SAR RDI, 1
  ;; 41:       0FB6743801       MOVZX ESI, BYTE PTR [RAX+RDI+1] <--- offset not embedded

  (print (+ (aref a i)
            (aref a (+ i 1))
            (aref a (+ i 2))))
  ;; SBCL can prove a constant offset, which can be embedded:
  ;; 80:       488B45F0         MOV RAX, [RBP-16]
  ;; 84:       0FB65006         MOVZX EDX, BYTE PTR [RAX+6]
  ;; 88:       48D1E2           SHL RDX, 1
  ;; 8B:       0FB67007         MOVZX ESI, BYTE PTR [RAX+7]
  ;; 8F:       48D1E6           SHL RSI, 1
  ;; 92:       4801F2           ADD RDX, RSI
  ;; 95:       0FB67008         MOVZX ESI, BYTE PTR [RAX+8]
  (let ((i 5))
    (print (+ (aref a i)
              (aref a (+ i 1))
              (aref a (+ i 2)))))
  (when (< i 98)
    ;; this works via type inference
    ;; F39:       498BF8           MOV RDI, R8
    ;; F3C:       48D1FF           SAR RDI, 1
    ;; F3F:       0FB6543801       MOVZX EDX, BYTE PTR [RAX+RDI+1]
    ;; F44:       48D1E2           SHL RDX, 1
    ;; F47:       498BF8           MOV RDI, R8
    ;; F4A:       48D1FF           SAR RDI, 1
    ;; F4D:       0FB6743802       MOVZX ESI, BYTE PTR [RAX+RDI+2]
    ;; F52:       48D1E6           SHL RSI, 1
    ;; F55:       4801F2           ADD RDX, RSI
    ;; F58:       498BF8           MOV RDI, R8
    ;; F5B:       48D1FF           SAR RDI, 1
    ;; F5E:       0FB6743803       MOVZX ESI, BYTE PTR [RAX+RDI+3]
    (print (+ (aref a i)
              (aref a (+ i 1))
              (aref a (+ i 2)))))
  (locally
      (declare ((mod 98) i))
    ;; this also works via type inference
    (print (+ (aref a i)
              (aref a (+ i 1))
              (aref a (+ i 2)))))
  
  (locally
      (declare (optimize (speed 3) (safety 0)))
    ;; this also works; removes bound checking.
    (print (+ (aref a i)
              (aref a (+ i 1))
              (aref a (+ i 2)))))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    ;; this also works; removes bound checking.
    (print (+ (row-major-aref a i)
              (row-major-aref a (+ i 1))
              (row-major-aref a (+ i 2)))))
  a)

