(in-package :cl-user)

(defpackage aws-bootstrap
  (:use :cl)
  (:export
   #:*_HANDLER*
   #:*AWS-REGION*
   #:*AWS-EXECUTION-ENV*
   #:*AWS-LAMBDA-FUNCTION-NAME*
   #:*AWS-LAMBDA-FUNCTION-MEMORY-SIZE*
   #:*AWS-LAMBDA-FUNCTION-VERSION*
   #:*AWS-LAMBDA-LOG-GROUP-NAME*
   #:*AWS-LAMBDA-LOG-STREAM-NAME*
   #:*AWS-ACCESS-KEY-ID*
   #:*AWS-SECRET-ACCESS-KEY*
   #:*AWS-SESSION-TOKEN*
   #:*LANG*
   #:*TZ*
   #:*LAMBDA-TASK-ROOT*
   #:*LAMBDA-RUNTIME-DIR*
   #:*PATH*
   #:*LD-LIBRARY-PATH*
   #:*AWS-LAMBDA-RUNTIME-API*
   #:bootstrap))
