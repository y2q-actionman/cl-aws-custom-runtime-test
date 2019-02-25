#!/bin/bash

cd `dirname $0`

LAMBDA_FUNC_NAME=${LAMBDA_FUNC_NAME:-"one_big_fasl"}
LAMBDA_ROLE=${LAMBDA_ROLE:-""}
LAMBDA_LAYER=${LAMBDA_LAYER:-""}
ZIP_FILE=$LAMBDA_FUNC_NAME.zip

# build fasls in VM
ASD_FILE=one-big-fasl-example.asd
ASD_SYSTEM_NAME=":${ASD_FILE%.*}"

docker run --rm \
       --env ASD_FILE=${ASD_FILE} \
       --env ASD_SYSTEM_NAME=${ASD_SYSTEM_NAME} \
       -v `pwd`:/out \
       cl-aws-buildenv /out/build_fasl_in_vm.sh

FASL_FILE=$(find . -name '*.fasl' -exec basename {} \;)

# make a zip.
# This script assumes fasls are built on this directory.
zip -u $ZIP_FILE $FASL_FILE

# upload
aws lambda delete-function \
    --function-name $LAMBDA_FUNC_NAME

aws lambda create-function \
    --function-name $LAMBDA_FUNC_NAME \
    --zip-file fileb://$ZIP_FILE \
    --handler "${FASL_FILE%.*}.jsown-handler" \
    --runtime provided \
    --role $LAMBDA_ROLE \
    --layers $LAMBDA_LAYER
