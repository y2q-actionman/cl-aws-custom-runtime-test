(defsystem #:aws-lambda-function-util
  :description "Some utilities for building an AWS Lambda function."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:cl-fad #:asdf #:quicklisp)
  :components
  ((:file "aws-lambda-function-util")))
