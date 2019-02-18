;;; Test codes.

(in-package :aws-lambda-runtime)

;; string-suffix-p

(assert (string-suffix-p ".ros" "hoge.ros"))

(assert (not (string-suffix-p ".ros" "hoge.ros.bak")))

;; AWS syntax pattern

(with-open-file (out "./hoge.lisp" :direction :output :if-exists :supersede)
  (format out "(defpackage #:hoge-package (:use :cl))")
  (format out "(in-package #:hoge-package)")
  (format out "(defun hoge-func ())"))

(assert (eql (find-handler "hoge.hoge-package::hoge-func")
             (fdefinition (find-symbol "HOGE-FUNC" "HOGE-PACKAGE"))))

;; ros script pattern

(assert (functionp
         (find-handler "handler/03_roswell_script_text/hello.ros") ; assume cwd is repository root.
	 ))

(assert (functionp
         (find-handler "handler/03_roswell_script_text/empty.ros") ; assume cwd is repository root.
	 ))

;; Lisp form pattern

(assert (eql (find-handler "'identity")
            #'cl-user::identity))

(assert (eql (find-handler "'aws-lambda-runtime::bootstrap")
            #'aws-lambda-runtime::bootstrap))

(assert (eq (find-handler "(progn`,1`,2'identity)")
            #'cl-user::identity))

(progn
  (delete-package "HOGE-PACKAGE")
  (assert (eq (find-handler "(load\"hoge.lisp\")'hoge-package::hoge-func")
	      (fdefinition (find-symbol "HOGE-FUNC" "HOGE-PACKAGE")))))
