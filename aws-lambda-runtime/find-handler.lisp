(in-package :aws-lambda-runtime)

(defun find-handler-from-aws-standard-format (handler-string)
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

(defun find-handler-from-lisp-forms (handler-string)
  "Tries to find a handler from lisp forms.
 See `find-handler''s docstring"
  (with-input-from-string (in handler-string)
    (with-standard-io-syntax
      (loop with ret
	 for form = (read in) then (read in nil 'eof)
	 until (eq form 'eof)
	 ;; I `eval' forms one-by-one because I want to affect it to subsequent forms.
	 do (setf ret (eval form))
	 finally
	   (return ret)))))

(defun string-suffix-p (suffix string)
  "Returns non-nil value when `string' ends with `suffix'."
  (search suffix string
	  :start2 (- (length string) (length suffix))))

(defun find-handler (handler-string)
  "Find a handler from AWS-Lambda function's --handler parameter.
`handler-string' is read as following:

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
  (assert (and handler-string
	       (not (equal handler-string "")))
	  () "AWS Lambda function's handler parameter must not be an empty string")
  (let* ((special-format-like?
	  (loop for c across handler-string
	     ;; If contains a whitespace or (), I assume it is a Lisp form.
	     always (and (graphic-char-p c)
			 (char/= c #\()
			 (char/= c #\)))))
	 (found-handler
	  (cond ((and special-format-like?
		      (find #\. handler-string))
		 (find-handler-from-aws-standard-format handler-string))
		#+ ()			; TODO
		((and special-format-like?
		      (string-ends-with-p ".ros" handler-string))
		 (find-handler-from-roswell-script handler-string))
		(t
		 (find-handler-from-lisp-forms handler-string)))))
    (alexandria:ensure-function found-handler))) ; If not funcallble, raises an error.


#|
;;; Test codes.

(in-package :aws-lambda-runtime)

;; string-suffix-p

(assert (string-suffix-p ".ros" "hoge.ros"))

(assert (not (string-suffix-p ".ros" "hoge.ros.bak")))

;; AWS syntax pattern

(assert (eql (find-handler ".identity")
            #'cl-user::identity))

(assert (eql (find-handler ".aws-lambda-runtime::bootstrap")
            #'aws-lambda-runtime::bootstrap))

(with-open-file (out "./hoge.lisp" :direction :output :if-exists :supersede)
  (format out "(defpackage #:hoge-package (:use :cl))")
  (format out "(in-package #:hoge-package)")
  (format out "(defun hoge-func ())"))

(assert (eql (find-handler "hoge.hoge-package::hoge-func")
             (fdefinition (find-symbol "HOGE-FUNC" "HOGE-PACKAGE"))))


;; ros script pattern

;stub

;; Lisp form pattern

(assert (eq (find-handler "(progn 1 2 3 4 5 'identity)")
            #'cl-user::identity))

(progn
  (delete-package "HOGE-PACKAGE")
  (assert (eq (find-handler "(load \"hoge.lisp\") 'hoge-package::hoge-func")
	      (fdefinition (find-symbol "HOGE-FUNC" "HOGE-PACKAGE")))))
|#
