

(in-package :numcl.impl)

(defvar *compiler* :common-lisp)

(defgeneric einsum-body (*compiler* einsum-vars)
  (:documentation
   " 
* `*compiler*` : Special variable (supposed to be a keyword) used for
  dispatching the compilation scheme.

* `einsum-vars` : einsum-vars structure.

"))
