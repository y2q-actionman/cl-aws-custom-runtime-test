#!/bin/bash

LAMBDA_FUNC_NAME=${LAMBDA_FUNC_NAME:-"ros_script_handler"}
LAMBDA_ROLE=${LAMBDA_ROLE:-""}
LAMBDA_LAYER=${LAMBDA_LAYER:-""}

ZIP_FILE=ros_script_handler.zip

rm $ZIP_FILE
zip $ZIP_FILE empty.ros hello.ros

aws lambda create-function \
    --function-name $LAMBDA_FUNC_NAME \
    --zip-file fileb://$ZIP_FILE \
    --handler "hello.ros" \
    --runtime provided \
    --role $LAMBDA_ROLE \
    --layers $LAMBDA_LAYER
