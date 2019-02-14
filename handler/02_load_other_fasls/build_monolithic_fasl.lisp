(in-package :cl-user)

(ql:quickload "cl-fad")

(defun build-monilithic-fasl (system-name target-dir)
  (ql:quickload system-name)
  (asdf:operate 'asdf:monolithic-compile-bundle-op system-name)
  (loop for fasl in (asdf:output-files 'asdf:monolithic-compile-bundle-op system-name)
     as to-path = (make-pathname :name (pathname-name fasl)
				 :type (pathname-type fasl)
				 :defaults target-dir)
     do (cl-fad:copy-file fasl to-path :overwrite t)))
