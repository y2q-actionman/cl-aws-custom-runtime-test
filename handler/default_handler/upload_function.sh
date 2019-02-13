#!/bin/bash

LAMBDA_FUNC_NAME=${LAMBDA_FUNC_NAME:-"use-default-handler"}
LAMBDA_ROLE=${LAMBDA_ROLE:-""}
LAMBDA_LAYER=${LAMBDA_LAYER:-""}

ZIP_FILE=use_default_handler.zip

zip $ZIP_FILE use_default_handler.lisp

aws lambda create-function \
    --function-name $LAMBDA_FUNC_NAME \
    --zip-file fileb://$ZIP_FILE \
    --handler "'aws-lambda-runtime:default-handler" \
    --runtime provided \
    --role $LAMBDA_ROLE \
    --layers $LAMBDA_LAYER
