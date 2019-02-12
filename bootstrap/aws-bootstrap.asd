(in-package :cl-user)

(asdf:defsystem :aws-bootstrap
  :description "Test (and proof-of-concept) codes for using SBCL on AWS Lambda"
  :license "WTFPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:drakma :usocket)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "usocket-patch-drop-ipv6" :depends-on ("package"))
     (:file "lambda-env-vars" :depends-on ("package"))
     (:file "find-handler" :depends-on ("package"))
     (:file "bootstrap"
	    :depends-on ("usocket-patch-drop-ipv6" "lambda-env-vars" "find-handler"))))))
