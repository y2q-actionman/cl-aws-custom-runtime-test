#!/bin/sh

THIS_DIR=`dirname $0`
cd $THIS_DIR

MAIN_LISP_FILE=process_with_cl-json.lisp
ZIP_FILE=load_other_fasls.zip

sbcl --non-interactive \
     --load build_monolithic_fasl.lisp \
     --load needed_libs.asd \
     --eval "(build-monilithic-fasl :needed-libs \"$THIS_DIR\")"

zip $ZIP_FILE *.fasl $MAIN_LISP_FILE
