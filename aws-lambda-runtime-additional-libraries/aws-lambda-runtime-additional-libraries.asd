(defsystem #:aws-lambda-runtime-additional-libraries
  :description "Additional libraries built into the AWS Lambda custom runtime."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on
  ( ;; Enumerate built-in libraries here.
   :cl-json
   :st-json
   :yason
   :jsown
   ;; :jonathan		    ; I surprised this lib has 8 dependencies.
   :json-streams
   :com.gigamonkeys.json))
