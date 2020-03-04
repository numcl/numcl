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

#|

Basic implementation without much optimization.

+ EINSUM-BODY is the global entry point to this set of functions.
+ %EINSUM-LOOP is a dispatch function.
+ %EINSUM-EARLY-BINDING does a very limited form of loop invariant analysis.
  It tries to bind the array element as early as possible.
  This does not mean all loop variants (e.g. index calcurations) are detected.
+ %EINSUM-STATIC and %EINSUM-BROADCAST implements a loop.
+ %EINSUM-BROADCAST generates a loop for broadcasting, which must loop over multiple axes
  using the dynamic information stored in the PLAN array.
+ %EINSUM-STATIC generates a loop for a non-broadcasting axis.
+ %EINSUM-LEAF performs the base-level loop.
+ See EINSUM-SPECS for the datastructure description.

|#

(in-package :numcl.impl)

(defvar *used-evars*)
(defvar *initialized-idxs*)

(defmethod einsum-body ((*compiler* (eql :common-lisp)) ev)
  (let (*used-evars*
        *initialized-idxs*)
    (%einsum-loop (einsum-specs-iter-specs ev) ev)))

(defun %einsum-loop (iter-specs ev)
  (match iter-specs
    (nil
     (%einsum-leaf ev))

    ((list* -1 _)
     (%einsum-broadcast iter-specs ev))

    ((list* _ _)
     (%einsum-static iter-specs ev))))

(defun %dimension-step-size (spec array-index out-p)
  (if (= -1 spec)
      (if out-p
          ;; see plan-broadcast
          `(aref plan 1 0 0)
          `(aref plan 1 ,array-index 0))
      (? spec)))

(defun %einsum-early-binding (rest ev)
  (match ev
    ((einsum-specs i-options o-options i-specs o-specs
                   :i-specs (structure i- :avars  i-avars :evars i-evars :idxs  i-idxs)
                   :o-specs (structure o- :avars  o-avars :evars o-evars :idxs  o-idxs))
     
     (iter MIDDLE
           (for i from 0)
           ;; For each input/output array,
           (for a-specs in (append o-specs i-specs))
           (for a-opts  in (append o-options i-options))
           (for out-p =  (< i (length o-specs)))
           (for index in (append o-idxs i-idxs))    ; index variable for accessing the array element
           (for var   in (append o-avars i-avars))  ; array variable
           (for evar  in (append o-evars i-evars))  ; array element variable, i.e., evar = (aref var index)

           ;; Generating the early binding information.
           (when (and (not (spec-depends-on a-specs rest))
                      (not (member evar *used-evars*)))
             ;; Accessing the array element as early as possible.
             ;; It ensures that a-specs does not depend on any more iteration variables.
             ;; If so, the array element can be bound to the element variable evar.
             (push evar *used-evars*)
             (collecting `(,evar (aref ,var ,index))
                         into late-bindings)
             (collecting `(derive ,var type (array-subtype-element-type type) ,evar)
                         into late-declarations)
             (when out-p
               ;; Code for storing the result back to the output array.
               (collecting `(setf (aref ,var ,index) ,evar) into store)))

           ;; Aggregate the information to do-node structure.
           (finally
            (return-from middle
              (if late-bindings
                  `(let* ,late-bindings
                     (declare ,@late-declarations)
                     ,(%einsum-loop rest ev)
                     ,@store)
                  (%einsum-loop rest ev))))))))

(defun %einsum-static (iter-specs ev)
  (match* (iter-specs ev)
    (((list* spec rest)
      (einsum-specs i-options o-options i-specs o-specs
                    :i-specs (structure i- :avars  i-avars :evars i-evars :idxs  i-idxs)
                    :o-specs (structure o- :avars  o-avars :evars o-evars :idxs  o-idxs)))
     
     (iter MIDDLE
           (for i from 0)
           ;; For each input/output array,
           (for a-specs in (append o-specs i-specs))
           (for a-opts  in (append o-options i-options))
           (for out-p =  (< i (length o-specs)))
           (for index in (append o-idxs i-idxs))    ; index variable for accessing the array element
           (for var   in (append o-avars i-avars))  ; array variable
           (for evar  in (append o-evars i-evars))  ; array element variable, i.e., evar = (aref var index)

           ;; Generating the index updating information.
           (iter INNER
                 ;; For each axis of each array,
                 (for (a-spec . rest2) on a-specs)
                 (assert (integerp a-spec))
                 (for a-axis from 0)
                 (destructuring-bind (&key (start 0) (step 1) &allow-other-keys) (cdr (assoc a-axis a-opts))
                   ;; Step size is the product of the rest of the loop limit variables.
                   
                   (for step-size = (iter (for spec in rest2)
                                          (collecting
                                           (%dimension-step-size spec i out-p))))
                   (when (= spec a-spec) ; this is a number.
                     (in MIDDLE
                         (if-let ((pos (position index bindings :key #'first)))
                           ;; When the same index variable appears more than
                           ;; twice (e.g. (einsum '(ii -> i)) ), it needs an adjustment;
                           ;; For example, X_ii of a (N, N) matrix X can be accessed by i*N+i in the
                           ;; row-major order, so the index must be updated with the step size 1+N.
                           (setf (third (elt bindings pos))
                                 `(+ (* ,step ,@step-size) ,(third (elt bindings pos))))
                           (let ((offset (if (member index *initialized-idxs*)
                                             index
                                             0)))
                             (collecting `(,index (+ ,offset (* ,start ,@step-size))
                                                  (+ ,index  (* ,step  ,@step-size)))
                                         into bindings)
                             (pushnew index *initialized-idxs*)
                             (if-let ((index-decl (assoc 'index declarations)))
                               (pushnew index (cdr index-decl))
                               (collecting `(index ,index) into declarations))))))))

           ;; Special case for singleton arrays.
           (when (and (null a-specs)
                      (not (member index *initialized-idxs*)))
             (pushnew index *initialized-idxs*)
             (collecting `(,index 0) into bindings)
             (if-let ((index-decl (assoc 'index declarations)))
               (pushnew index (cdr index-decl))
               (collecting `(index ,index) into declarations)))

           ;; Aggregate the information to do-node structure.
           (finally
            (if-let ((index-decl (assoc 'index declarations)))
              (push (& spec) (cdr index-decl))
              (push `(index ,(& spec)) declarations))
            (return-from middle
              `(do* ((,(& spec) 0 (1+ ,(& spec)))
                     ,@bindings)
                    ((<= ,(? spec) ,(& spec)))
                 (declare ,@declarations)
                 ,(%einsum-early-binding rest ev))))))))

(defun %einsum-broadcast (iter-specs ev)
  (match* (iter-specs ev)
    (((list* spec rest)
      (einsum-specs i-specs o-specs
                    :i-specs (structure i- :avars  i-avars :evars i-evars :idxs  i-idxs)
                    :o-specs (structure o- :avars  o-avars :evars o-evars :idxs  o-idxs)))
     (iter MIDDLE ; For each input/output array,
           (for i from 0)
           (for a-specs in (append o-specs i-specs))
           (for out-p =  (< i (length o-specs)))
           (for index in (append o-idxs i-idxs))    ; index variable for accessing the array element
           (for var   in (append o-avars i-avars))    ; array variable
           (for evar  in (append o-evars i-evars))  ; array element variable, i.e., evar = (aref var index)
           
           ;; Generating the index updating information.
           (iter INNER ; For each axis of each array,
                 (for (a-spec . rest2) on a-specs)
                 (for step-size = (iter (for spec in rest2)
                                        (collecting
                                         (%dimension-step-size spec i out-p))))
                 (when (= spec a-spec)
                   (in MIDDLE
                       (collecting index     into broadcast-idxs)
                       (collecting i         into broadcast-idxs-arrayindex)
                       (collecting out-p     into broadcast-idxs-outp)
                       (if (member index *initialized-idxs*)
                           (collecting index into broadcast-idxs-init)
                           (collecting 0     into broadcast-idxs-init))
                       (pushnew index *initialized-idxs*)
                       (collecting step-size into broadcast-idxs-step))))

           ;; Aggregate the information to do-node structure.
           (finally
            (return-from MIDDLE
              `(labels ((rec (axis ,@broadcast-idxs)
                          (declare (index ,@broadcast-idxs))
                          (if (<= (array-dimension plan 2) axis)
                              ,(%einsum-early-binding rest ev)
                              (dotimes (i (aref plan 0 0 axis))
                                (rec (1+ axis) ,@broadcast-idxs)
                                ,@(iter (for index in broadcast-idxs)
                                        (for step  in broadcast-idxs-step)
                                        (for i     in broadcast-idxs-arrayindex)
                                        (for out-p in broadcast-idxs-outp)
                                        (collecting
                                         (if out-p
                                             `(incf ,index (* (aref plan 1 0 axis) ,@step))
                                             `(incf ,index (if (= 1 (aref plan 0 ,i axis))
                                                               0
                                                               (* (aref plan 1 ,i axis) ,@step))))))))))
                 (rec 0 ,@broadcast-idxs-init))))))))

(defun %einsum-leaf (ev)
  ;; Innermost code block: No loop variable left.
  ;; Invoked by the bottom loop expander.
  ;; Also a special case for when there are no loop variables.
  (iter (for transform in (einsum-specs-transforms ev))
        (for o-avar    in (o-avars (einsum-specs-o-specs ev)))
        (for o-evar    in (o-evars (einsum-specs-o-specs ev)))
        (when (first-iteration-p)
          (collecting 'progn))
        (collecting
         `(setf ,o-evar
                ;; Coerce the result to the array element type.
                ;; This is necessary because some potentially-complex function may return
                ;; a real value, and storing a real value to a complex array results in an error.
                (%coerce ,transform
                         (compile-time-type-of ,o-evar))))))

