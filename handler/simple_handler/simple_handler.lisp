(in-package :cl-user)

(defun simple-handler (data headers)
  (declare (ignore headers))
  (format nil "The test simple-handler was called. Request is: '~A'" data))
