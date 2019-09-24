
(in-package :numcl.impl)

;; access:     (aref a i j) (aref b j k) (aref c i k)
;; dimensions: (5 6) (6 7) (5 7)

#+(or)
(dotimes (i 5)
  (dotimes (j 6)
    (dotimes (k 7)
      (incf (aref c i k)
            (* (aref a i j)
               (aref b j k))))))

#+(or)
(let ((a-offset 0)
      (b-offset 0)
      (c-offset 0)
      (a-step1 (* 6))
      (a-step2 (*))
      (b-step1 (* 7))
      (b-step2 (*))
      (c-step1 (* 7))
      (c-step2 (*)))
  
  (do ((i 0 (1+ i))
       (a-step a-step1)
       (c-step c-step1)
       (a-offset a-offset (+ a-offset a-step))
       (c-offset c-offset (+ c-offset c-step)))
      ((<= 5 i))

    (do ((j 0 (1+ j))
         (a-step a-step2)
         (b-step b-step1)
         (a-offset a-offset (+ a-offset a-step))
         (b-offset b-offset (+ b-offset b-step))
         ($1 (aref a a-offset) (aref a a-offset)))
        ((<= 6 j))

      (do ((k 0 (1+ k))
           (b-step b-step2)
           (c-step c-step2)
           (b-offset b-offset (+ b-offset b-step))
           (c-offset c-offset (+ c-offset c-step))
           ($2 (aref b b-offset) (aref b b-offset))
           (@1 (aref c c-offset) (aref c c-offset)))
          ((<= 7 k)
           (setf (aref c c-offset) @1))

        (setf @1 (+ @1 (* $1 $2)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct do-node
    base-var
    base-limit
    vars
    inits
    steps
    late-vars
    late-inits
    late-declaration
    store                               ; array store operations
    declaration))

(defmethod einsum-body ((*compiler* (eql :common-lisp)) ev)
  (let* ((iter-specs (einsum-vars-iter-specs ev))
         (i-specs (einsum-vars-i-specs ev))
         (o-specs (einsum-vars-o-specs ev))
         (i-vars  (einsum-vars-i-vars ev))
         (o-vars  (einsum-vars-o-vars ev))
         (i-evars (einsum-vars-i-evars ev))
         (o-evars (einsum-vars-o-evars ev))
         (i-idx (make-gensym-list (length i-vars) "$IDX"))
         (o-idx (make-gensym-list (length o-vars) "@IDX"))
         (const-vars  (append o-idx i-idx))
         (const-inits (mapcar (constantly 0) const-vars)) ;initialized to 0
         used-evars)
    ((lambda (nodes)
       ;; note: const-vars and const-inits are updated in the MIDDLE loop
       `(let* ,(mapcar #'list const-vars const-inits)
          (declare (type index ,@const-vars))
          ,(einsum-body-iter nodes ev)))
     (iter OUTER
           (for (spec . rest) on iter-specs)
           (for ? = (? spec))           ; iteration limit variable
           (for & = (& spec))           ; iteration count variable

           (iter MIDDLE
                 (for i from 0)
                 (for spec2 in (append o-specs i-specs))
                 (for out-p = (< i (length o-specs)))
                 (for index in (append o-idx i-idx))      ; index variable for accessing the array element
                 (for var   in (append o-vars i-vars))    ; array variable
                 (for evar  in (append o-evars i-evars))  ; array element variable, i.e., evar = (aref var index)
                 (iter INNER
                       ;; For the loop spec of each array (spec2), 
                       (for (spec3 . rest2) on spec2)
                       ;; Variable to store the loop step size
                       (for step  = (gensym (if out-p "@STEP" "$STEP")))
                       (when (eql spec spec3)
                         ;; Step size is the product of the rest of the loop limit variables.
                         ;; This value can be cached at the top of the loop.
                         (push step const-vars)
                         (push `(* ,@(mapcar #'? rest2)) const-inits)
                         (in MIDDLE
                             (if-let ((pos (position index vars)))
                               ;; When the same index variable appears more than
                               ;; twice (e.g. (einsum '(ii -> i)) ), it needs an adjustment;
                               ;; For example, X_ii of a (N, N) matrix X can be accessed by i*N+i in the
                               ;; row-major order, so the index must be updated with the step size 1+N.
                               (setf (elt steps pos)
                                     `(+ ,step ,(elt steps pos)))
                               (progn
                                 (collecting index                          into vars)
                                 (collecting index                          into inits)
                                 (collecting `(+ ,step ,index)              into steps)
                                 (collecting `(declare (type index ,index)) into declaration))))))
                 (when (and (not (spec-depends-on spec2 rest))
                            (not (member evar used-evars)))
                   ;; Accessing the array element as early as possible.
                   ;; It ensures that spec2 does not depend on any more iteration variables.
                   ;; If so, the array element can be bound to the element variable evar.
                   (push evar used-evars)
                   
                   (collecting evar                into late-vars)
                   (collecting `(aref ,var ,index) into late-inits)
                   (collecting `(declare (derive ,var type (array-subtype-element-type type) ,evar))
                               into late-declaration)
                   (when out-p
                     ;; Code for storing the result back to the output array.
                     (collecting `(setf (aref ,var ,index) ,evar) into store)))
                 (finally
                  (in OUTER
                      (collecting
                       (make-do-node
                        :base-var   &
                        :base-limit ?
                        :vars vars
                        :inits inits
                        :steps steps
                        :late-vars  late-vars
                        :late-inits late-inits
                        :late-declaration late-declaration
                        :declaration `((declare (type index ,&)) ,@declaration)
                        :store     store)))))))))


(defparameter *unroll-width* 8)

(defun einsum-body-iter (nodes ev)
  "Consume one index in iter-specs and use it for dotimes."
  (labels ((step-form (vars steps)
             "construct the update forms for the loop index."
             (iter (for v in vars)
                   (for s in steps)
                   (when (first-iteration-p)
                     (collect 'setf))
                   (collect v)
                   (collect s)))
           (rec (nodes)
             "Consume one loop index by generating a DO loop."
             (ematch nodes
               ((list* (do-node base-var base-limit
                                vars inits steps
                                late-vars late-inits late-declaration
                                declaration store)
                       nil)
                ;; Bottom loop: No more loop variables left. Perform unrolling
                (let ((form `(let ,(mapcar #'list late-vars late-inits)
                               ,@late-declaration
                               ,(rec nil)
                               ,@store))
                      (step-form (step-form vars steps))
                      (end-var (gensym "END")))
                  `(do* ((,base-var 0 ,end-var)
                         (,end-var ,*unroll-width* (+ ,end-var ,*unroll-width*))
                         ,@(mapcar #'list vars inits))
                        ((<= ,base-limit ,end-var)
                         ;; Processing the unroll remainder
                         (do* ((,base-var ,base-var (+ ,base-var 1))
                               ,@(mapcar #'list vars inits))
                              ((<= ,base-limit ,base-var))
                           ,@declaration
                           ,form
                           ,step-form))
                     (declare (type index ,end-var))
                     ,@declaration
                     ;; Unrolling the bottom loop
                     ,@(iter (repeat *unroll-width*)
                             (collecting form)
                             (collecting step-form)))))
               ((list* (do-node base-var base-limit
                                vars inits steps
                                late-vars late-inits late-declaration
                                declaration store)
                       rest)
                ;; Intermediate loop: Consume one loop variable.
                `(do* ((,base-var 0 (+ ,base-var 1))
                       ,@(mapcar #'list vars inits))
                      ((<= ,base-limit ,base-var))
                   ,@declaration
                   (let ,(mapcar #'list late-vars late-inits)
                     ,@late-declaration
                     ,(rec rest)
                     ,@store)
                   ,(step-form vars steps)))
               
               (nil
                ;; Innermost code block: No loop variable left.
                ;; Invoked by the bottom loop expander.
                ;; Also a special case for when there are no loop variables.
                (iter (for transform in (einsum-vars-transforms ev))
                      (for o-var     in (einsum-vars-o-vars ev))
                      (for o-evar    in (einsum-vars-o-evars ev))
                      (when (first-iteration-p)
                        (collecting 'progn))
                      (collecting
                       `(setf ,o-evar
                              ;; Coerce the result to the array element type.
                              ;; This is necessary because some potentially-complex function may return
                              ;; a real value, and storing a real value to a complex array results in an error.
                              (%coerce ,transform
                                       (compile-time-type-of ,o-evar)))))))))
    (rec nodes)))
