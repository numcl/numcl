

(in-package :numcl.impl)

(defvar *compiler* :common-lisp)

(defgeneric einsum-body (*compiler* iter-specs i-specs o-specs i-vars o-vars i-evars o-evars transforms))
