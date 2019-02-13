;;; This file implements AWS Lambda Runtime Interface.
;;; See: https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html

(in-package :aws-lambda-runtime)

(defun make-next-invocation-path ()
  "Makes an URI for getting a next event."
  (format nil "http://~A/2018-06-01/runtime/invocation/next"
	  *AWS-LAMBDA-RUNTIME-API*))

(defun make-invocation-response-path (request-id)
  "Makes an URI for for reporting invocation response"
  (format nil "http://~A/2018-06-01/runtime/invocation/~A/response"
	  *AWS-LAMBDA-RUNTIME-API* request-id))

(defun make-invocation-error-path (request-id)
  "Makes an URI for for reporting invocation error"
  (format nil "http://~A/2018-06-01/runtime/invocation/~A/error"
	  *AWS-LAMBDA-RUNTIME-API* request-id))

(defun make-initialization-error-path ()
  "Makes an URI for reporing initialization error."
  (format nil "http://~A/2018-06-01/runtime/init/error" *AWS-LAMBDA-RUNTIME-API*))

(defun make-error-response-contents (condition)
  "Makes JSON string used for reporting invocation or initialization errors"
  (let ((error-message (princ-to-string condition))
	(error-type (type-of condition)))
    (format nil "{\"errorMessage\" : \"~A\", \"errorType\" : \"~A\"}"
	    error-message error-type)))


(defun main-loop (handler)
  "This custom runtime's main loop.
This function contiues to retrieve an event, funcall `handler' with
two arg (the event and HTTP headers), and send `handler''s result back."
  (loop with next-invocation-path = (make-next-invocation-path)
     for (body status headers . nil)
       = (multiple-value-list (drakma:http-request next-invocation-path))
     as request-id = (cdr (assoc :Lambda-Runtime-Aws-Request-Id headers))
     as response-path = (make-invocation-response-path request-id) ; TODO: buffering
     as error-path = (make-invocation-error-path request-id) ; TODO: buffering
     do (handler-case
	    (let ((response (funcall handler body headers)))
	      (drakma:http-request response-path
				   :method :POST
				   :content response))
	  (error (condition)
	    (drakma:http-request error-path
				 :method :POST
				 :content (make-error-response-contents condition))))))

(defun bootstrap ()
  "Initialize this runtime, find a handler function, and go to `main-loop'."
  (let (handler)
    (handler-case
	(progn
	  (load-aws-lambda-environmental-variables)
	  (setf drakma:*drakma-default-external-format* :utf-8)
	  
	  ;; For seeing JSON as text. (https://blog.kyanny.me/entry/2017/08/18/031415)
	  (push '("application" . "json") drakma:*text-content-types*)
	  
	  ;; To drop IPv6 support, I patch `usocket:get-hosts-by-name'.
	  (patch-get-hosts-by-name)

	  ;; Move cwd to the AWS lambda function code.
	  ;; I think this is convetnient to `load' them.
	  (sb-posix:chdir *LAMBDA-TASK-ROOT*) 

	  ;; find this function's handler
	  (setf handler (or (find-handler *_HANDLER*)
			    (error "No handler found. (specified by ~A)" *_HANDLER*))))
      (error (condition)
	;; Calls AWS Lambda's initialization error API.
	(drakma:http-request (make-initialization-error-path)
			     :method :POST
			     :content (make-error-response-contents condition))
	;; and exits immediately.
	(return-from bootstrap nil)))
    ;; goto main loop
    (main-loop handler)))
