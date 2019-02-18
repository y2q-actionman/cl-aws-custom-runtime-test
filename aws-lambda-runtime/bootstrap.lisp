;;; This file implements AWS Lambda Runtime Interface.
;;; See: https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html

(in-package :aws-lambda-runtime)

(defun make-next-invocation-path (&optional out)
  "Makes an URI for getting a next event."
  (format out "http://~A/2018-06-01/runtime/invocation/next"
	  *AWS-LAMBDA-RUNTIME-API*))

(defun make-invocation-response-path (request-id &optional out)
  "Makes an URI for for reporting invocation response"
  (format out "http://~A/2018-06-01/runtime/invocation/~A/response"
	  *AWS-LAMBDA-RUNTIME-API* request-id))

(defun make-invocation-error-path (request-id &optional out)
  "Makes an URI for for reporting invocation error"
  (format out "http://~A/2018-06-01/runtime/invocation/~A/error"
	  *AWS-LAMBDA-RUNTIME-API* request-id))

(defun make-initialization-error-path (&optional out)
  "Makes an URI for reporing initialization error."
  (format out "http://~A/2018-06-01/runtime/init/error" *AWS-LAMBDA-RUNTIME-API*))

(defconstant +aws-lambda-path-default-buffer-length+ 128
  "Default buffer length of making path URI.")

(defun make-error-response-contents (condition)
  "Makes JSON string used for reporting invocation or initialization errors"
  (let ((error-message (princ-to-string condition))
	(error-type (type-of condition)))
    (format nil "{\"errorMessage\" : \"~A\", \"errorType\" : \"~A\"}"
	    error-message error-type)))


(defun main-loop (handler)
  "This custom runtime's main loop.
This function contiues to retrieve an event, funcall HANDLER with
two arg (the event and HTTP headers), and send HANDLER's result back."
  (prog ((next-invocation-path (make-next-invocation-path))
	 (consecutive-weird-error-count 0)
	 (path-buffer (make-array +aws-lambda-path-default-buffer-length+
				  :element-type 'character
				  :fill-pointer 0 :adjustable t)))
   next-invocation
   (flet ((handle-weird-error (e)
	    (when (> (incf consecutive-weird-error-count) 3)
	      ;; give up..
	      (format *debug-io* "consecutive ~A weird errors" consecutive-weird-error-count)
	      (error e))
	    (format *debug-io* "retrying by ~A" e)
	    (go next-invocation)))
     (multiple-value-bind (body status headers)
	 (handler-case
	     (drakma:http-request next-invocation-path)
	   #+sbcl
	   (simple-error (e)
	     (let ((error-string (princ-to-string e)))
	       (when (and (search "Syscall poll(2) failed" error-string)
			  (search "Operation not permitted" error-string))
		 ;; This is a weird case; we caught EPERM on poll(2)
		 ;; at receiving response after sending a request.
		 ;; 
		 ;; I don't know how to treat this correctly, but I
		 ;; someday tried to restart this loop, surprisingly
		 ;; this code returned correct results. However, same
		 ;; errors are produced every time. (very strange...)
		 (handle-weird-error e)))
	     (error e)))		; otherwise, resignal it.
       (ecase status
	 (200 t)			; ok
	 (403
	  (handle-weird-error "next-invocation API returned 403."))
	 (500  ; runtime-api spec says 'Runtime should exit promptly.'
	  ()
	  (error "next-invocation API returned 500. aborted.")))
       ;; Process the request.
       (let ((request-id (cdr (assoc :Lambda-Runtime-Aws-Request-Id headers))))
	 (setf (fill-pointer path-buffer) 0)
	 (handler-case
	     (let ((response (funcall handler body headers)))
	       (make-invocation-response-path request-id path-buffer)
	       (drakma:http-request path-buffer
				    :method :POST
				    :content response))
	   (error (condition)
	     (make-invocation-error-path request-id path-buffer)
	     (drakma:http-request path-buffer
				  :method :POST
				  :content (make-error-response-contents condition)))))))
   ;; goto next
   (setf consecutive-weird-error-count 0)
   (go next-invocation)))

(defun bootstrap ()
  "Initialize this runtime, find a handler function, and go to `main-loop'."
  (let (handler)
    (handler-case
	(progn
	  (load-aws-lambda-environmental-variables)
	  (setf drakma:*drakma-default-external-format* :utf-8)
	  
	  ;; For seeing JSON as text. (https://blog.kyanny.me/entry/2017/08/18/031415)
	  (push '("application" . "json") drakma:*text-content-types*)

	  ;; Move cwd to the AWS lambda function code.
	  ;; I think this is convenient to `load' them.
	  (uiop:chdir *LAMBDA-TASK-ROOT*) 

	  ;; find this function's handler
	  (setf handler (find-handler *_HANDLER*)))
      (error (condition)
	;; Calls AWS Lambda's initialization error API.
	(drakma:http-request (make-initialization-error-path)
			     :method :POST
			     :content (make-error-response-contents condition))
	;; and exits immediately.
	(return-from bootstrap nil)))
    ;; goto main loop
    (main-loop handler)))
