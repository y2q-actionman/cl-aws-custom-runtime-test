(in-package :cl-user)

(defun jsown-handler (data headers)
  (declare (ignore headers))
  (let ((json (jsown:parse data)))
    (jsown:do-json-keys (key val) json
      (when (integerp val)
	(setf (jsown:val json key) (format nil "~/jp-numeral:jp/" val))))
    (jsown:to-json json)))
