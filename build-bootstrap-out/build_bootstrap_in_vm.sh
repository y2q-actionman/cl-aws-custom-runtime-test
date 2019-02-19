#!/bin/sh

ZIP_NAME=$1
BIN_NAME="bootstrap"

# Makes a 'bootstrap' binary with SBCL.
# This code does following:
#  1. Load required quicklisp libraries.
#  2. Load lisp codes of roswell, because I need its lisp codes for loading roswell scripts.
#     (What I want is its Lisp code ('ros' package) only, but
#     currently I must fetch its Lisp-implementation-management
#     feature together, needlessly.)
#  3. Load our custom runtime.
#  4. Makes a single binary named $BIN_NAME. To start as a bootstrap,
#     I specified :toplevel to our bootstrap function.

/usr/local/bin/sbcl \
    --non-interactive \
    --eval "(ql:quickload '#:aws-lambda-runtime-builtin-libraries)" \
    --eval "(ql:quickload '#:roswell)" \
    --eval "(ql:quickload '#:aws-lambda-runtime)" \
    --eval "(sb-ext:save-lisp-and-die \"$BIN_NAME\" :executable t :toplevel 'aws-lambda-runtime::bootstrap)"

# Make a zip file from the binary. This will be uploaded to AWS Lambda as a custom runtime.
zip /out/$ZIP_NAME $BIN_NAME

# cleanup
rm $BIN_NAME
