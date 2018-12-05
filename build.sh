#!/bin/bash

/usr/local/bin/sbcl --script <<EOF
(in-package :cl-user)

(load (merge-pathnames "quicklisp/setup.lisp"
       (user-homedir-pathname)))

(ql:quickload "drakma")
;; (ql:quickload "cl-json")

(load "/work/bootstrap.lisp")

(sb-ext:save-lisp-and-die
 "bootstrap"
 :executable t
 :toplevel 'bootstrap)
EOF

zip aws_lambda_example.zip bootstrap
