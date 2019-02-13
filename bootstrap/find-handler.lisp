(in-package :aws-bootstrap)

(defgeneric default-handler (data headers)
  (:documentation "`aws-bootstrap''s default hander for AWS lambda invocation.
This runtime itself does not defines any methods.  (So, users must
define this, or `no-applicable-method' will be signaled.)"))

(defun find-handler (handler-string)
  "Find a handler from AWS-Lambda function's --handler parameter.
`handler-string' must be one of them:

* `nil' or an empty string.

  Uses `aws-bootstrap:default-handler'.

* A notation of a symbol. (Like \"cl:princ-to-string\").

  The symbol will be `funcall''ed.

* Any number of Lisp forms.

  Evaluates forms in order with `cl:eval' (wow!), and uses result of the last form.

  (In this style, you can call any functions, so I think you can
  `load' and execute any codes (e.g. compiled fasls)."
  ;; Checks 1st pattern.
  (when (or (null handler-string)
	    (equal handler-string ""))
    (return-from find-handler 'default-handler))
  (with-input-from-string (in handler-string)
    (loop with first-form = (read in)
       with second-form = (read in nil 'eof)
       initially
	 ;; Checks 2nd pattern.
	 (when (and (symbolp first-form) (eq second-form 'eof))
	   (return first-form))
       ;; from here, 3rd pattern
       for form = first-form then next-form
       for next-form = second-form then (read in nil 'eof)
       as ret = (eval form)
       when (eq next-form 'eof)		; when read completely, returns the last result.
       return ret)))
