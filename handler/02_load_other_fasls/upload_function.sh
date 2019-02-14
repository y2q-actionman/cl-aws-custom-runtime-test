#!/bin/bash

MAIN_LISP_FILE=process_with_cl-json.lisp
ZIP_FILE=load_other_fasls.zip

# build zip in VM
rm $ZIP_FILE

docker run --rm \
       -v `pwd`:/out \
       test /out/build_zip_in_vm.sh


# upload
LAMBDA_FUNC_NAME=${LAMBDA_FUNC_NAME:-"load_other_fasls"}
LAMBDA_ROLE=${LAMBDA_ROLE:-""}
LAMBDA_LAYER=${LAMBDA_LAYER:-""}

aws lambda create-function \
    --function-name $LAMBDA_FUNC_NAME \
    --zip-file fileb://$ZIP_FILE \
    --handler "process_with_cl-json.test-parse-handler" \
    --runtime provided \
    --role $LAMBDA_ROLE \
    --layers $LAMBDA_LAYER
