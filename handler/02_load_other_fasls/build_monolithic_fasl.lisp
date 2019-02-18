(in-package :cl-user)

;;; I want to put this code at a good place.. but where it it?

(ql:quickload :asdf)
(ql:quickload :cl-fad)

(defun build-monolithic-fasl (system-name target-dir)
  (asdf:operate 'asdf:monolithic-compile-bundle-op system-name)
  (loop for fasl in (asdf:output-files 'asdf:monolithic-compile-bundle-op system-name)
     as to-path = (make-pathname :name (pathname-name fasl)
				 :type (pathname-type fasl)
				 :defaults target-dir)
     do (cl-fad:copy-file fasl to-path :overwrite t)))
