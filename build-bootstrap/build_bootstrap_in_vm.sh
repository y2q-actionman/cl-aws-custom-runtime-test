#!/bin/sh

BIN_NAME="bootstrap"

# Makes a 'bootstrap' binary with SBCL.
# This code does following:
#  1. Load required quicklisp libraries.
#  2. Load lisp codes in roswell, because I need its lisp codes for
#     loading roswell scripts.  (Currently, I load roswell's
#     Lisp-implementation-management feature with them. I don't need it..)
#  3. Load our custom runtime.
#  4. Makes a single binary named $BIN_NAME. To start as a bootstrap,
#     I specified :toplevel to our bootstrap function.

/usr/local/bin/sbcl \
    --non-interactive \
    --load "/work/ql_libs_at_docker_build.lisp" \
    --eval "(asdf:load-asd \"/work/roswell-19.1.10.96/roswell.asd\")" \
    --eval "(asdf:load-system \"roswell\")" \
    --load "/aws-lambda-runtime/aws-lambda-runtime.asd" \
    --eval "(asdf:load-system :aws-lambda-runtime)" \
    --eval "(sb-ext:save-lisp-and-die \"$BIN_NAME\" :executable t :toplevel 'aws-lambda-runtime::bootstrap)"

# Make a zip file from the binary. This will be uploaded to AWS Lambda as a custom runtime.
zip /out/aws_lambda_bootstrap.zip $BIN_NAME

# cleanup
rm $BIN_NAME
