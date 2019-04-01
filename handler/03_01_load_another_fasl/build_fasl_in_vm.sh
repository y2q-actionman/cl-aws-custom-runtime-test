#!/bin/sh

# These variables are assumed to be passed as environment.
# - ASD_FILE
# - ASD_SYSTEM_NAME

THIS_DIR=`dirname $0`
cd $THIS_DIR

sbcl --non-interactive \
     --eval "(ql:quickload :aws-lambda-function-util)" \
     --eval "(asdf:load-asd \"${ASD_FILE}\")" \
     --eval "(ql:quickload ${ASD_SYSTEM_NAME})" \
     --eval "(aws-lambda-function-util:build-monolithic-fasl ${ASD_SYSTEM_NAME} \"$THIS_DIR\")"
