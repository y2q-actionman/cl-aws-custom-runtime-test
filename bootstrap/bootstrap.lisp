(in-package :aws-bootstrap)

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

(defun bootstrap ()
  (patch-get-hosts-by-name)
  
  (setf drakma:*drakma-default-external-format* :utf-8)
  ;; For seeing JSON as text. (https://blog.kyanny.me/entry/2017/08/18/031415)
  (push '("application" . "json") drakma:*text-content-types*)

  (load-aws-lambda-environmental-variables)
  (sb-posix:chdir *LAMBDA-TASK-ROOT*)	; TODO: use this in `find-handler'

  ;; goto main loop
  (main-loop (find-handler *_HANDLER*)))
