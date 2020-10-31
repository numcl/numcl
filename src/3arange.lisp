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
  (let* ((length (max 0 (ceiling (- stop start) step))))
    (declare (fixnum length))
    (multiple-value-bind (a base) (%make-array length :element-type type)
      ;; benchnmark1 0.321 seconds
      ;; benchnmark2 1.906 seconds
      ;; benchnmark3 2.168 seconds
      #+(or)
      (let ((tmp start))
        (dotimes (i length)
          (setf (aref base i) tmp)
          (incf tmp step)))

      ;; 0.980 seconds
      ;; 0.888 seconds
      ;; 0.885 seconds
                                        ; 1F:       31C0             XOR EAX, EAX
                                        ; 21:       EB42             JMP L1
                                        ; 23:       660F1F840000000000 NOP
                                        ; 2C:       0F1F4000         NOP
                                        ; 30: L0:   66480F6EC7       MOVQ XMM0, RDI
                                        ; 35:       0FC6C0FD         SHUFPS XMM0, XMM0, #4r3331
                                        ; 39:       F30F11444201     MOVSS [RDX+RAX*2+1], XMM0
                                        ; 3F:       66480F6ECF       MOVQ XMM1, RDI
                                        ; 44:       0FC6C9FD         SHUFPS XMM1, XMM1, #4r3331
                                        ; 48:       66480F6ED1       MOVQ XMM2, RCX
                                        ; 4D:       0FC6D2FD         SHUFPS XMM2, XMM2, #4r3331
                                        ; 51:       F30F58D1         ADDSS XMM2, XMM1
                                        ; 55:       660F7ED7         MOVD EDI, XMM2
                                        ; 59:       48C1E720         SHL RDI, 32
                                        ; 5D:       4080CF19         OR DIL, 25
                                        ; 61:       4883C002         ADD RAX, 2
                                        ; 65: L1:   4839D8           CMP RAX, RBX
                                        ; 68:       7CC6             JL L0
                                        ; 6A:       BA17001050       MOV EDX, #x50100017              ; NIL
                                        ; 6F:       488BE5           MOV RSP, RBP
                                        ; 72:       F8               CLC
                                        ; 73:       5D               POP RBP
                                        ; 74:       C3               RET
      #+(or)
      (specializing (base) ()
        (declare (optimize (speed 3) (safety 0)))
        (locally                        ; != cl:locally
            (declare (derive base type (array-subtype-element-type type) start step))
          (dotimes (i length)
            (setf (aref base i) start)
            (setf start (+ start step)))))
      
      ;; 0.493 seconds
      ;; 0.383 seconds
      ;; 0.369 seconds
                                        ; 0D:       31C0             XOR EAX, EAX
                                        ; 0F:       EB20             JMP L1
                                        ; 11:       660F1F840000000000 NOP
                                        ; 1A:       660F1F440000     NOP
                                        ; 20: L0:   F30F114C4201     MOVSS [RDX+RAX*2+1], XMM1
                                        ; 26:       0F28C1           MOVAPS XMM0, XMM1
                                        ; 29:       F30F58CA         ADDSS XMM1, XMM2
                                        ; 2D:       4883C002         ADD RAX, 2
                                        ; 31: L1:   4839C8           CMP RAX, RCX
                                        ; 34:       7CEA             JL L0
                                        ; 36:       BA17001050       MOV EDX, #x50100017              ; NIL
                                        ; 3B:       488BE5           MOV RSP, RBP
                                        ; 3E:       F8               CLC
                                        ; 3F:       5D               POP RBP
                                        ; 40:       C3               RET
      ;; #+(or)
      (specializing (base start step) ()
        (declare (optimize (speed 3) (safety 0)))
        (dotimes (i length)
          (setf (aref base i) start)
          (setf start (+ start step))))
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
(defun linspace (start stop num &key (endpoint t) type)
  (declare (fixnum num))
  (declare (number start stop))
  (let* ((step (/ (- stop start) (1- num)))
         (type (or type (%arange-infer-type start stop step)))
         (start (coerce start type))
         (stop  (coerce stop  type))
         (step  (coerce step  type))
         (lst (loop for i from 0 below (1- num)
                    for x from start by step
                    collect x))
         (lst (if endpoint (append lst (list stop)) lst))
         (arr (asarray lst :type type)))
    (values arr step)))


#+(or)
(progn
  (defun fn ()
    (linspace 1 5 3)))

;; logspace --- = exp (linspace start stop), skip

;; geomspace --- = exp (linspace (log start) (log stop)), skip

