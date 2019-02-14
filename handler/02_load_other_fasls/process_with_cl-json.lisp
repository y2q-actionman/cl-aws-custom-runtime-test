(in-package :cl-user)

;;; Load all fasls
(eval-when (:load-toplevel :execute)
  (mapcar #'load (directory (make-pathname :name :wild :type "fasl"
					   :defaults *load-pathname*))))

(defun test-parse-handler (data headers)
  (declare (ignore headers))
  (let ((json (cl-json:decode-json-from-string data)))
    (format nil "Parsed json: '~A'" json)))
