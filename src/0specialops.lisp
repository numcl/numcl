
(in-package :numcl.impl)

(declaim (declaration derive))

#|

Deriving a type from another source type is handy if you don't know who will declare the source type where.
For example, it could be done outside a macro, or could be written by hand.
While you can design your macro to always expect such information passed as part of the argument,
it is inconvenient for the users and the interface would look ugly.
Instead, there is no need to pass such information explicitly
if it can be obtained from the lexical environment object.

This is handy for writing a macro which needs some type propagation:

 (let ((a ...))
   (declare ((type (array fixnum (2 2)))))
   (let ((b ...) (c ...) (d ...))
     (declare (derive a (array-subtype element-type) element-type b c d))
     ...))

You also need something similar when the macro is sufficiently complex
and needs to be separated into multiple functions and / or multiple layers of macros.

Note that this does not provide a runtime checking and unification of type variables.

|#

(defmacro let (&whole whole bindings &body body &environment env)
  "A LET wrapper that recognizes DERIVE declaration."
  (multiple-value-bind (body declarations) (parse-body body :documentation nil :whole whole)
    `(cl:let ,bindings
             ,@(%expand-decls declarations env)
             ,@body)))

(defun %expand-decls (decls env)
  (iter (for decl in decls)
        (collecting
         (match decl
           ((list* 'declare sub-decls)
            (list* 'declare (mapcar (lambda (decl)
                                      (%%expand-decls decl env))
                                    sub-decls)))))))

(defun %%expand-decls (decl env)
  (match decl
    ((list* 'derive variable type newform rest)
     `(type ,(derive-type-expander variable type newform env) ,@rest))
    (_
     decl)))

(defun derive-type-expander (variable type newform env)
  (progv (list type) (list (cdr (assoc 'type (nth-value 2 (cltl2:variable-information variable env)))))
    (eval newform)))

;; TODO: define this for all special ops

;; THROW 
;; SETQ 
;; THE 
;; SYMBOL-MACROLET 
;; LOAD-TIME-VALUE 
;; BLOCK 
;; FLET 
;; LET* 
;; RETURN-FROM 
;; PROGN 
;; PROGV 
;; MACROLET 
;; EVAL-WHEN 
;; CATCH 
;; TAGBODY 
;; UNWIND-PROTECT 
;; MULTIPLE-VALUE-PROG1 
;; LOCALLY 
;; QUOTE 
;; MULTIPLE-VALUE-CALL 
;; IF 
;; GO 
;; FUNCTION 
;; LABELS 
;; LET 
;; QUOTE 
;; BLOCK 
;; PROGN 
;; CATCH 
;; LOCALLY 
;; GO 
;; PROGV 
;; SYMBOL-MACROLET 
;; LOAD-TIME-VALUE 
;; MACROLET 
;; UNWIND-PROTECT 
;; FLET 
;; THROW 
;; EVAL-WHEN 
;; MULTIPLE-VALUE-CALL 
;; IF 
;; RETURN-FROM 
;; LET 
;; THE 
;; TAGBODY 
;; SETQ 
;; LET* 
;; LABELS 
;; MULTIPLE-VALUE-PROG1 
;; FUNCTION 
;; LOCALLY 
;; FLET 
;; CATCH 
;; IF 
;; UNWIND-PROTECT 
;; FUNCTION 
;; LET 
;; TAGBODY 
;; EVAL-WHEN 
;; QUOTE 
;; BLOCK 
;; SYMBOL-MACROLET 
;; GO 
;; LABELS 
;; SETQ 
;; RETURN-FROM 
;; LET* 
;; MULTIPLE-VALUE-CALL 
;; LOAD-TIME-VALUE 
;; PROGN 
;; MULTIPLE-VALUE-PROG1 
;; PROGV 
;; THE 
;; THROW 
;; MACROLET 
;; SB-CLTL2:COMPILER-LET 
;; QUOTE 
;; SB-KERNEL:THE* 
;; SB-CLTL2:COMPILER-LET 
;; SB-SYS:NLX-PROTECT 
;; SB-SYS:%PRIMITIVE 
;; FUNCTION 
;; SETQ 
;; THE 
;; SYMBOL-MACROLET 
;; LOAD-TIME-VALUE 
;; BLOCK 
;; MULTIPLE-VALUE-PROG1 
;; FLET 
;; LET* 
;; PROGN 
;; PROGV 
;; MACROLET 
;; EVAL-WHEN 
;; CATCH 
;; TAGBODY 
;; UNWIND-PROTECT 
;; FUNCTION 
;; QUOTE 
;; LOCALLY 
;; RETURN-FROM 
;; MULTIPLE-VALUE-CALL 
;; IF 
;; GO 
;; LABELS 
;; THROW 
;; FUNCTION 
;; SB-C::WITH-ANNOTATIONS 
;; SB-C::%CLEANUP-FUN 
;; SB-C::%ESCAPE-FUN 
;; SB-C::%FUNCALL-LVAR 
;; SB-C::GLOBAL-FUNCTION 
;; SB-C::BOUND-CAST 
;; SB-C::%FUNCALL 
;; SB-C::%WITHIN-CLEANUP 
;; SB-C::%%ALLOCATE-CLOSURES 
;; TRULY-THE 
