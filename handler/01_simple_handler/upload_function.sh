#!/bin/bash

LAMBDA_FUNC_NAME=${LAMBDA_FUNC_NAME:-"simple_handler"}
LAMBDA_ROLE=${LAMBDA_ROLE:-""}
LAMBDA_LAYER=${LAMBDA_LAYER:-""}

ZIP_FILE=simple_handler.zip

zip $ZIP_FILE simple_handler.lisp

aws lambda create-function \
    --function-name $LAMBDA_FUNC_NAME \
    --zip-file fileb://$ZIP_FILE \
    --handler "simple_handler.cl-user::simple-handler" \
    --runtime provided \
    --role $LAMBDA_ROLE \
    --layers $LAMBDA_LAYER
