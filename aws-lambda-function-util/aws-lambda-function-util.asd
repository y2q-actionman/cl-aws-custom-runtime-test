(in-package :cl-user)

(asdf:defsystem #:aws-lambda-function-util
  :description "Some utilities for building an AWS Lambda function."
  :license "WTFPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:cl-fad #:asdf #:quicklisp)
  :components
  ((:file "aws-lambda-function-util")))
