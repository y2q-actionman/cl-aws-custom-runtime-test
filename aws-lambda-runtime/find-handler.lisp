(in-package :aws-lambda-runtime)

(defun find-handler-from-aws-standard-format (handler-string)
  "Tries to find a handler by AWS Lambda's standard format.
 See `find-handler''s docstring"
  (let* ((dot-pos (position #\. handler-string))
	 (file-name (subseq handler-string 0 dot-pos)))
    (when (equal "" file-name)
      (error "AWS standard format handler \"~A\" does not contains filename"
	     handler-string))
    (with-standard-io-syntax
      (load file-name)
      (ensure-function
       (read-from-string handler-string nil nil :start (1+ dot-pos))))))

(defun ensure-roswell-runtime ()
  "Ensures a package named 'ros' existence, for loading a ros script.
If such a package does not exist, a package named
'fake-roswell-runtime' is created.  This package provides
`ros:ensure-asdf' symbol."
  (let ((ros-package
	 (or (find-package :ros)
	     (make-package 'fake-roswell-runtime :use nil :nicknames '(:ros)))))
    (unless (find-symbol "ENSURE-ASDF" ros-package)
      (let ((ensure-asdf-sym (intern "ENSURE-ASDF" ros-package)))
	;; Roswell's `ros:ensure-asdf' returns T if ASDF was loaded.
	;; Because I assume this runtime has ASDF in the Lisp image, I always return T.
	(setf (symbol-function ensure-asdf-sym) (constantly t))
	(export ensure-asdf-sym ros-package)))
    ros-package))

(defun load-script-body (stream &optional (main-symbol-name "MAIN"))
  "Evaluate Lisp forms from STREAM and returns the main symbol."
  ;; This function is splited, for supporting cl-launch after.
  (loop with main-symbol = nil
     for form = (read stream nil 'eof)
     until (eq form 'eof)
     do (let ((val (eval form)))
	  (when (and (symbolp val) ; Checks whether the returned value is from `cl:defun'.
		     (string-equal (symbol-name val) main-symbol-name))
	    (setf main-symbol val)))
     finally
       (return (or main-symbol
		   ;; Use the last `*package*' changed by `eval'.
		   (find-symbol main-symbol-name)))))

(defun wrap-script-main-to-aws-lambda-convention (function)
  "Wraps the main function of scripts to follow calling convensions
 described in docstrings of `find-handler'."
  (lambda (string *HEADER-ALIST*)
    (with-input-from-string (*standard-input* string)
      (with-output-to-string (*standard-output*)
	(funcall function)))))

(defun find-handler-from-roswell-script (handler-string)
  "Tries to find a handler from a ros script name.
See `find-handler''s docstring."
  (ensure-roswell-runtime)
  (let* ((main
	  ;; The official loader of roswell script is `roswell:script'
	  ;; function, but I think that is difficult to use.
	  ;; I manually reads it to find the main function.
	  (with-open-file (in handler-string)
	    (with-standard-io-syntax
	      (let ((first-line (read-line in))) ; skip the first line.
		(assert (starts-with-subseq "#!" first-line)
			() "No shebang in the roswell script: ~A" in)
		(load-script-body in)))))
	 (func (ensure-function main)))
    (wrap-script-main-to-aws-lambda-convention func)))

(defun find-handler-from-lisp-forms (handler-string)
  "Tries to find a handler from lisp forms.
 See `find-handler''s docstring"
  (with-input-from-string (in handler-string)
    (with-standard-io-syntax
      (loop with ret
	 for form = (read in nil 'eof)
	 until (eq form 'eof)
	 ;; I `eval' forms one-by-one because I want to affect it to subsequent forms.
	 do (setf ret (eval form))
	 finally
	   (return (ensure-function ret)))))) ; If not funcallble, raises an error.

(defun find-handler (handler-string)
  "Find a handler from AWS-Lambda function's --handler parameter.
HANDLER-STRING is read as following:

* AWS Lambda's standard format: \"<file>.<method>\"

  Tries to `cl:load' the <file> and find a symbol denoted by <method>.
  <method> is read in the standard syntax. (If no package marker, it
  is read in the `CL-USER' package.) The symbol will be called with
  two args (request data and headers) and its return value will be
  returned as AWS Lambda function's return value.

* A script file name. (currently, only for roswell scripts.)

  If there is a file named same with HANDLER-STRING, this runtime
  tries to load the file and find its main function.

  The main function will be called with no arguments. At calling,
  AWS-Lambda's request body is bound to `*standard-input*' and context
  are bound to `aws-lambda-runtime:*HEADER-ALIST*',and strings written to
  `*standard-output*' are used as AWS-Lambda's result.

* Any number of Lisp forms.

  Otherwise, HANDLER-STRING is considered as a string contains Lisp forms.
  `find-handler' evaluates the forms in order with `cl:eval' (wow!),
  and uses the result of the last form.
  (Because AWS Lambda's 'handler' parameter cannot contain any spaces,
  you need very crafted codes.)

  The last value will be called like the <method> of AWS Lambda's standard format."
  (assert (and handler-string
	       (not (equal handler-string ""))
	       (every #'graphic-char-p handler-string))
	  () "Invalid format for AWS Lambda function's handler: ~A" handler-string)
  (let ((lisp-like?
	 (or (char= #\' (char handler-string 0)) ; If starts with quite, it is a quoted form.
	     (loop for c across handler-string
		;; If contains '()', I assume it is a Lisp form.
		thereis (member c '(#\( #\)))))))
    (cond ((and (not lisp-like?)
		(probe-file handler-string)
		(ends-with-subseq ".ros" handler-string))
	   (find-handler-from-roswell-script handler-string))
	  ((and (not lisp-like?)
		(find #\. handler-string))
	   (find-handler-from-aws-standard-format handler-string))
	  (t
	   (find-handler-from-lisp-forms handler-string)))))
