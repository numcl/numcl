
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

(defmethod einsum-body ((*compiler* (eql :common-lisp)) (ev einsum-vars))
  (let* ((iter-specs (einsum-vars-iter-specs ev))
         (i-specs (einsum-vars-i-specs ev))
         (o-specs (einsum-vars-o-specs ev))
         (i-vars  (einsum-vars-i-vars ev))
         (o-vars  (einsum-vars-o-vars ev))
         (i-evars (einsum-vars-i-evars ev))
         (o-evars (einsum-vars-o-evars ev))
         (transforms (einsum-vars-transforms ev))
         (i-idx (make-gensym-list (length i-vars) "$IDX"))
         (o-idx (make-gensym-list (length o-vars) "@IDX"))
         (const-vars  (append o-idx i-idx))
         (const-inits (mapcar (constantly 0) const-vars))
         used-evars)
    ((lambda (nodes)
       ;; note: const-vars and const-inits are updated in the loop
       `(let* ,(mapcar #'list const-vars const-inits)
          (declare (type index ,@const-vars))
          ,(einsum-body-iter nodes transforms)))
     (iter outer
           (for (spec . rest) on iter-specs)
           (for ? = (? spec))
           (for & = (& spec))

           (iter middle
                 (for i from 0)
                 (for spec2 in (append o-specs i-specs))
                 (for out-p = (< i (length o-specs)))
                 (for index in (append o-idx i-idx))
                 (for var   in (append o-vars i-vars))
                 (for evar  in (append o-evars i-evars))
                 (iter (for (spec3 . rest2) on spec2)
                       (for step  = (gensym (if out-p "@STEP" "$STEP")))
                       (when (eql spec spec3)
                         (push step const-vars)
                         (push `(* ,@(mapcar #'? rest2)) const-inits)
                         (in middle
                             (if-let ((pos (position index vars)))
                                 (setf (elt steps pos)
                                       `(+ ,step ,(elt steps pos)))
                               (progn
                                 (collecting index             into vars)
                                 (collecting index             into inits)
                                 (collecting `(+ ,step ,index) into steps)
                                 (collecting `(declare (type index ,index))
                                             into declaration))))))
                 (when (and (not (spec-depends-on spec2 rest))
                            (not (member evar used-evars)))
                   ;; check if spec2 does not depend on any more iteration variables.
                   ;; If so, then the array element can be bound to the element variable evar.
                   (push evar used-evars)
                   
                   (collecting evar                into late-vars)
                   (collecting `(aref ,var ,index) into late-inits)
                   (collecting `(declare (derive ,var type (array-subtype-element-type type) ,evar))
                               into late-declaration)
                   (when out-p
                     (collecting `(setf (aref ,var ,index) ,evar) into store)))
                 (finally
                  (in outer
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


(defvar *unroll-width* 8)

(defun einsum-body-iter (nodes transforms)
  "Consume one index in iter-specs and use it for dotimes."

  (labels ((step-form (vars steps)
             (iter (for v in vars)
                   (for s in steps)
                   (when (first-iteration-p)
                     (collect 'setf))
                   (collect v)
                   (collect s)))
           (rec (nodes)
             (ematch nodes
               ;; unroll
               ((list* (do-node base-var base-limit
                                vars inits steps
                                late-vars late-inits late-declaration
                                declaration store)
                       nil)
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
                         (do* ((,base-var ,base-var (+ ,base-var 1))
                               ,@(mapcar #'list vars inits))
                              ((<= ,base-limit ,base-var))
                           ,@declaration
                           ,form
                           ,step-form))
                     (declare (type index ,end-var))
                     ,@declaration
                     ,@(iter (repeat *unroll-width*)
                             (collecting form)
                             (collecting step-form)))))
               ((list* (do-node base-var base-limit
                                vars inits steps
                                late-vars late-inits late-declaration
                                declaration store)
                       rest)
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
                
                (iter (for transform in transforms)
                      (for o from 1)
                      (for o-sym = (@ o))
                      (when (first-iteration-p)
                        (collecting 'progn))
                      (collecting
                       `(setf ,o-sym ,transform)))))))
    (rec nodes)))
