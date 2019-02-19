#!/bin/sh

THIS_DIR=`dirname $0`
cd $THIS_DIR

MAIN_LISP_FILE=process_with_cl-json.lisp
ZIP_FILE=load_other_fasls.zip

sbcl --non-interactive \
     --eval "(ql:quickload :aws-lambda-function-util)" \
     --load needed_libs.asd \
     --eval "(ql:quickload :needed-libs)" \
     --eval "(aws-lambda-function-util:build-monolithic-fasl :needed-libs \"$THIS_DIR\")"

zip $ZIP_FILE *.fasl $MAIN_LISP_FILE
