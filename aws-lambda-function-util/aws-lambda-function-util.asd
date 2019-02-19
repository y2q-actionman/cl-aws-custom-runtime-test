(in-package :cl-user)

(asdf:defsystem :aws-lambda-runtime
  :description "Some utilities for building an AWS Lambda function"
  :license "WTFPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:cl-fad :asdf)
  :components
  ((:file "aws-lambda-function-util.lisp")))
