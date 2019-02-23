(in-package :cl-user)

;;; Load all fasls
(eval-when (:load-toplevel :execute)
  (aws-lambda-function-util:load-fasls-in-directory *load-pathname*))

(defun test-parse-handler (data headers)
  (declare (ignore headers))
  (loop with json = (cl-json:decode-json-from-string data)
     for (key . val) in json
     collect (cons key
		   (if (integerp val)
		       (format nil "~/jp-numeral:jp/" val)
		       val))
     into ret-json
     finally (return (cl-json:encode-json-to-string ret-json))))
