(in-package :aws-lambda-runtime)

(defun find-handler-from-aws-standard-format (handler-string)
  "Tries to find a handler by AWS Lambda's standard format.
 See `find-handler''s docstring"
  (let* ((dot-pos (position #\. handler-string))
	 (file-name (subseq handler-string 0 dot-pos))
	 (symbol-name (subseq handler-string (1+ dot-pos))))
    (when (equal "" file-name)
      (error "AWS standard format handler does not contains filename: ~A"
	     handler-string))
    (with-standard-io-syntax
      (load file-name)
      (read-from-string symbol-name))))

(defun find-handler-from-roswell-script (handler-string)
  "Tries to find a handler from Roswell script name.
 See `find-handler''s docstring"
  ;; The official loader of roswell script is `roswell:script'
  ;; function, but I think that is hacky.  I manually reads it to find
  ;; the main function.
  ;; 
  ;; 1. `load' it without the first line.
  ;; (To avoid reader errors, I load it at first.)
  (with-open-file (in handler-string)
    (read-line in)			; skip the first line.
    (with-standard-io-syntax
      (load in)))
  ;; 2. find `in-package' form to get the package of `main'.
  (with-open-file (in handler-string)
    (read-line in)			; skip the first line.
    (with-standard-io-syntax
      (loop with main-package = :cl-user
	 for form = (read in nil 'eof)
	 until (eq form 'eof)
	 when (and (listp form)
		   (eq (first form) 'in-package))
	 do (setf main-package (second form))
	 finally
	   (return
	     (let* ((sym (find-symbol "MAIN" main-package))
		    (func (alexandria:ensure-function sym)))
	       ;; Connect `*standard-output*' to AWS-lambda's return value.
	       (lambda (&rest args)
		 (with-output-to-string (*standard-output*)
		   (apply func args)))))))))

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

* Roswell script file name.

  If `handler-string' ends with \".ros\", tries to `cl:load' the file
  as a Roswell script. This runtime calls its main function with two
  args (data and headers) and returns the string written to
  `*standard-output*' as AWS-Lambda's result.

* AWS Lambda's standard format: \"<file>.<method>\"

  Tries to `cl:load' the <file> and find a symbol denoted by <method>.
  <method> is read as a symbol. If no package marker, it is read in
  the `CL-USER' package.

* Any number of Lisp forms.

  Otherwise, `handler-string' is considered as a string contains Lisp forms.
  `find-handler' evaluates the forms in order with `cl:eval' (wow!),
  and uses the result of the last form.
  (Because AWS Lambda's 'handler' parameter cannot contain any spaces,
  you need very hacky codes.)"
  (assert (and handler-string
	       (not (equal handler-string ""))
	       (every #'graphic-char-p handler-string))
	  () "Invalid format for AWS Lambda function's handler: ~A" handler-string)
  (let* ((lisp-like?
	  (or (char= #\' (char handler-string 0))  ; If starts with quite, it is a quoted form.
	      (loop for c across handler-string
		 ;; If contains '()', I assume it is a Lisp form.
		 thereis (member c '(#\( #\))))))
	 (found-handler
	  (cond ((and (not lisp-like?)
		      (string-suffix-p ".ros" handler-string))
		 (find-handler-from-roswell-script handler-string))
		((and (not lisp-like?)
		      (find #\. handler-string))
		 (find-handler-from-aws-standard-format handler-string))
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
|#
