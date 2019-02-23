(defpackage #:aws-lambda-function-util
  (:use #:cl)
  (:export
   #:list-installed-systems
   #:build-monolithic-fasl
   #:load-fasls-in-directory))

(in-package #:aws-lambda-function-util)

(defun list-installed-systems (&optional (dists (ql-dist:all-dists)))
  (mapcan #'ql-dist:installed-systems dists))

(defun build-monolithic-fasl (system-name output-directory)
  (unless (cl-fad:directory-pathname-p output-directory)
    (let ((orig (shiftf output-directory
			(cl-fad:pathname-as-directory output-directory))))
      (warn "output-directory ~A is not a directory-pathname, considered as ~A"
	    orig output-directory)))
  (asdf:operate 'asdf:monolithic-compile-bundle-op system-name)
  (loop for fasl in (asdf:output-files 'asdf:monolithic-compile-bundle-op system-name)
     as to-path = (make-pathname :name (pathname-name fasl)
				 :type (pathname-type fasl)
				 :defaults output-directory)
     do (cl-fad:copy-file fasl to-path :overwrite t)
     collect to-path))
;;; Should I use `(asdf:apply-output-translations ".")' ?

(defun load-fasls-in-directory (pathname)
  (let ((fasls (directory (make-pathname :name :wild
					 :type "fasl"
					 :defaults pathname))))
    (mapc #'load fasls)))

;;; I consider loading other fasls automatically, but it may break dependency.
;;; I think every file should define its own load call.
