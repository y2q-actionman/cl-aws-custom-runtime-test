#!/bin/bash

cd `dirname $0`

LAMBDA_FUNC_NAME=${LAMBDA_FUNC_NAME:-"ros_script_handler"}
LAMBDA_ROLE=${LAMBDA_ROLE:-""}
LAMBDA_LAYER=${LAMBDA_LAYER:-""}
ZIP_FILE=$LAMBDA_FUNC_NAME.zip

zip -u $ZIP_FILE *.ros

aws lambda delete-function \
    --function-name $LAMBDA_FUNC_NAME

aws lambda create-function \
    --function-name $LAMBDA_FUNC_NAME \
    --zip-file fileb://$ZIP_FILE \
    --handler "hello.ros" \
    --runtime provided \
    --role $LAMBDA_ROLE \
    --layers $LAMBDA_LAYER
