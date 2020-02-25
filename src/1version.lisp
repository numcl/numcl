
(in-package :numcl.impl)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-version-string ()
    (if (probe-file (merge-pathnames ".git/" (asdf:system-source-directory :numcl)))
        (nth-value 0
                   (uiop:run-program
                    (format nil "cd ~a; git rev-parse HEAD"
                            (asdf:system-source-directory :numcl))
                    :output '(:string :stripped t)))
        (lastcar (pathname-directory (asdf:system-source-directory :numcl))))))

(let ((version #.(make-version-string)))
  (defun numcl-version ()
    "Returns a string that represents the version of NUMCL currently installed."
    version))
