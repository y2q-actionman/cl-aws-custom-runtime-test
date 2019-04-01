(defsystem #:aws-lambda-runtime
  :description "Test and proof-of-concept codes for using SBCL on AWS Lambda."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:alexandria
	       #:usocket ; comes with drakma
	       #:drakma
	       #:uiop)
  :components
  ((:file "package")
   (:file "lambda-env-vars" :depends-on ("package"))
   (:file "find-handler" :depends-on ("package"))
   (:file "bootstrap"
	  :depends-on ("lambda-env-vars" "find-handler"))))
