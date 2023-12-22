#!/bin/bash

cd `dirname $0`

LAMBDA_FUNC_NAME=${LAMBDA_FUNC_NAME:-"ros_script_handler"}
LAMBDA_ROLE=${LAMBDA_ROLE:-""}	# Please specify your IAM role here.

# Please specify your cunstom runtime layer's ARN.
# Like: LAMBDA_LAYER="arn:aws:lambda:ca-central-1:************:layer:lisp-layer:23"
LAMBDA_LAYER=${LAMBDA_LAYER:-""}

ZIP_FILE=$LAMBDA_FUNC_NAME.zip

zip -u $ZIP_FILE *.ros

aws lambda delete-function \
    --function-name $LAMBDA_FUNC_NAME

aws lambda create-function \
    --function-name $LAMBDA_FUNC_NAME \
    --zip-file fileb://$ZIP_FILE \
    --handler "hello.ros" \
    --runtime provided.al2 \
    --role $LAMBDA_ROLE \
    --layers $LAMBDA_LAYER
