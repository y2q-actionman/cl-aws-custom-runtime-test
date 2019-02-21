(in-package :cl-user)

(asdf:defsystem #:aws-lambda-runtime-builtin-libraries
  :description "Additional builtin libraries into the AWS Lambda custom runtime."
  :license "WTFPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on
  ( ;; Enumerate built-in libraries here.
   :cl-json
   :st-json
   :yason
   :jsown
   :jonathan		    ; I surprised this lib has 8 dependencies.
   :json-streams
   :com.gigamonkeys.json))
