(in-package :cl-user)

(defun outer-handler (data &optional headers)
  (declare (ignore headers))
  (format nil "I am outer-handler, request ~A" data))
