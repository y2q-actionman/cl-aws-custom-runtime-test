(in-package :aws-bootstrap)

(defun default-handler (data &optional headers)
  (declare (ignore headers))
  (format nil "The default hander was called. Data is: '~A'" data))

(defun find-handler (handler-string)
  "Find handler. Consider as: <filename>#<symbol>"
  (let* ((sharp-position (position #\# handler-string))
	 ;; TODO: use `split-sequence'
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
	     (not (equal symbol-name "default-handler")) ; TODO: FIXME: required?
	     )
	(with-standard-io-syntax
	  (read-from-string symbol-name))
	'default-handler)))
