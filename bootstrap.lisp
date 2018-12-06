(defpackage aws-bootstrap-test
  (:use :cl)
  (:export #:bootstrap))

(in-package :aws-bootstrap-test)

(defvar *_HANDLER*)
(defvar *LAMBDA-TASK-ROOT*)
(defvar *AWS-LAMBDA-RUNTIME-API*)

(defun main-loop (handler)
  (let ((next-endpoint
	 (format nil "http://~A/2018-06-01/runtime/invocation/next" *AWS-LAMBDA-RUNTIME-API*)))
    (loop
       for (body status headers . nil)
	 = (multiple-value-list (drakma:http-request next-endpoint))
       as request-id = (cdr (assoc :Lambda-Runtime-Aws-Request-Id headers))
       as response = (funcall handler body headers)
       as response-endpoint
	 = (format nil "http://~A/2018-06-01/runtime/invocation/~A/response"
		   *AWS-LAMBDA-RUNTIME-API* request-id)
       do (drakma:http-request response-endpoint
			       :method :POST
			       :content response))))

(defun default-handler (data &optional headers)
  (declare (ignore headers))
  (format nil "The default hander was called. Request is: '~A'" data))

(defun find-handler (handler-string)
  "Find handler. Consider as: <filename>#<symbol>"
  (let* ((sharp-position (position #\# handler-string))
	 (load-name (if sharp-position
			(subseq handler-string 0 sharp-position)))
	 (symbol-name (if sharp-position
			  (subseq handler-string (1+ sharp-position) nil)
			  handler-string)))
    (when (and load-name
	       (probe-file load-name))
      (load load-name))
    (if (and symbol-name
	     (plusp (length symbol-name))
	     (not (equal symbol-name "default-handler")))
	(with-standard-io-syntax
	  (read-from-string symbol-name))
	'default-handler)))

(defun bootstrap ()
  (setf drakma:*drakma-default-external-format* :utf-8)
  ;; For seeing JSON as text. (https://blog.kyanny.me/entry/2017/08/18/031415)
  (push '("application" . "json") drakma:*text-content-types*)

  ;; load env variables
  (setf *_HANDLER* (sb-ext:posix-getenv "_HANDLER")
	*LAMBDA-TASK-ROOT* (sb-ext:posix-getenv "LAMBDA_TASK_ROOT")
	*AWS-LAMBDA-RUNTIME-API* (sb-ext:posix-getenv "AWS_LAMBDA_RUNTIME_API"))
  (sb-posix:chdir *LAMBDA-TASK-ROOT*)

  ;; goto main loop
  (main-loop (find-handler *_HANDLER*)))
