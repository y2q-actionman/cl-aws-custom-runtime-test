(in-package :aws-lambda-runtime)

(defgeneric default-handler (data headers)
  (:documentation "`aws-bootstrap''s default hander for AWS lambda invocation.
This runtime itself does not defines any methods.  (So, users must
define this, or `no-applicable-method' will be signaled.)"))

(defun find-handler-from-standard-format (handler-string)
  "Tries to find a handler by AWS Lambda's standard format.
 See `find-handler''s docstring"
  (let* ((dot-pos (position #\. handler-string))
	 (file-name (subseq handler-string 0 dot-pos))
	 (symbol-name (subseq handler-string (1+ dot-pos))))
    (with-standard-io-syntax
      (unless (equal "" file-name)
	(load file-name))
      (read-from-string symbol-name))))

(defun find-handler-from-roswell-script (handler-string)
  "Tries to find a handler from Roswell script name.
 See `find-handler''s docstring"
  (declare (ignore handler-string))
  (assert nil ()  "This function is stub"))

(defun find-handler-from-lisp-forms (forms)
  "Tries to find a handler from lisp forms.
 See `find-handler''s docstring"
  (loop with ret = nil
     for form in forms
     do (setf ret (eval form))
     finally (return ret)))

(defun string-suffix-p (suffix string)
  "Returns non-nil value when `string' ends with `suffix'."
  (search suffix string
	  :start2 (- (length string) (length suffix))))

(defun find-handler (handler-string)
  "Find a handler from AWS-Lambda function's --handler parameter.
`handler-string' is read as following:

* `equalp' with \"nil\"

  Returns `aws-bootstrap:default-handler'.

* AWS Lambda's standard format: \"file.method\"

  Tries to `load' the 'file' and find a symbol denoted by 'method'.
  If 'file' is an empty, it loads no file. (This may be useful for
  finding a built-in function.)
  'method' is read in the `CL-USER' package.

* Roswell script name.

  If `handler-string' ends with \".ros\", tries to load the file
  as a Roswell script, and returns its main function.

* Any number of Lisp forms.

  Otherwise, `handler-string' is considered as a string contains Lisp forms.
  `find-handler' evaluates the forms in order with `cl:eval' (wow!),
  and uses the result of the last form."
  (assert (and (null handler-string)
	       (not (equal handler-string "")))
	  () "AWS Lambda function's handler parameter must not be an empty string")
  ;; `default-handler' pattern.
  (when (equalp handler-string "nil")
    (return-from find-handler 'default-handler))
  ;; Checks 2nd or 3rd pattern.
  ;; If contains some space or (), I assume it is a Lisp form.
  (when (loop for c across handler-string
	   always (and (graphic-char-p c)
		       (char/= c #\() (char/= c #\))))
    (cond ((find #\. handler-string)
	   (return-from find-handler
	     (find-handler-from-standard-format handler-string)))
	  #+ ()				; TODO
	  ((string-ends-with-p ".ros" handler-string)
	   (return-from find-handler
	     (find-handler-from-standard-format handler-string)))))
  ;; 4th path.
  (with-input-from-string (in handler-string)
    (with-standard-io-syntax
      (loop for form = (read in) then next-form
	 for next-form = (read in nil 'eof)
	 collect form into lisp-forms
	 until (eq next-form 'eof)
	 finally (return (find-handler-from-lisp-forms lisp-forms))))))


#|
;;; Test codes.

(in-package :aws-lambda-runtime)

;; string-suffix-p

(assert (string-suffix-p ".ros" "hoge.ros"))

(assert (not (string-suffix-p ".ros" "hoge.ros.bak")))

;; 1st pattern

(assert (eq (find-handler "nil")
            'default-handler))

(assert (eq (find-handler "NIL")
            'default-handler))

;; 2nd pattern

(assert (eq (find-handler ".identity")
            'cl-user::identity))

(assert (eq (find-handler ".aws-bootstrap::bootstrap")
            'aws-bootstrap::bootstrap))

(with-open-file (out "./hoge.lisp" :direction :output :if-exists :supersede)
  (format out "(defpackage #:hoge-package (:use :cl))")
  (format out "(in-package #:hoge-package)")
  (format out "(defun hoge-func ())"))

(assert (eq (find-handler "hoge.hoge-package::hoge-func")
            (find-symbol "HOGE-FUNC" "HOGE-PACKAGE")))


;; 3rd pattern

stub

;; 4th pattern

(assert (eq (find-handler "(progn 1 2 3 4 5 'identity)")
            'cl-user::identity))

(delete-package "HOGE-PACKAGE")

(assert (eq (find-handler "(load \"hoge.lisp\") 'format")
            'format))
|#
