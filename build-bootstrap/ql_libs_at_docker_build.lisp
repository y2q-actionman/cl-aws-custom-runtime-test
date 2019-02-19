(in-package :cl-user)

;;; for bootstrap ifself.
;; (ql:quickload :aws-lambda-runtime) ; loaded by this call.

;;; for loading Roswell
;; (ql:quickload "roswell") ; loaded by this call.

;;; JSON Libs
;;; See: https://sites.google.com/site/sabraonthehill/home/json-libraries
(ql:quickload :cl-json)
(ql:quickload :st-json)
(ql:quickload :yason)
(ql:quickload :jsown)
(ql:quickload :jonathan)   ; I surprised this lib has 8 dependencies.
(ql:quickload :json-streams)
(ql:quickload :com.gigamonkeys.json)
;;; TODO: add a package like `:aws-lambda-runtime-builtin-libraries' ?
