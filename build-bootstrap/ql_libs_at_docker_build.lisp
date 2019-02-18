;;; for bootstrap ifself.
(ql:quickload :alexandria)
(ql:quickload :drakma)
(ql:quickload :uiop)
(ql:quickload :cl-fad)

;;; for loading Roswell
(ql:quickload :simple-date-time)
(ql:quickload :plump)
(ql:quickload :split-sequence)
(ql:quickload :zip)

;;; JSON Libs
;;; See: https://sites.google.com/site/sabraonthehill/home/json-libraries
(ql:quickload :cl-json)
(ql:quickload :st-json)
(ql:quickload :yason)
(ql:quickload :jsown)
(ql:quickload :jonathan)   ; I surprised this lib has 8 dependencies.
(ql:quickload :json-streams)
(ql:quickload :com.gigamonkeys.json)
