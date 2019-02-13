(in-package :cl-user)

(defmethod aws-lambda-runtime:default-handler (data headers)
  (declare (ignore headers))
  (format nil "The default hander was called. Request is: '~A'" data)))
