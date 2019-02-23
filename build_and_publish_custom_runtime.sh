#!/bin/sh

cd `dirname $0`

# build a VM.
VM_NAME="test"
docker build -t $VM_NAME .

# make a zip file.
ZIP_NAME="aws_lambda_bootstrap.zip"
docker run --rm \
       -v `pwd`/build-bootstrap-out:/out \
       $VM_NAME /out/build_bootstrap_in_vm.sh $ZIP_NAME

# publish as a AWS custom function layer
LAYER_NAME="lisp-layer"
aws lambda publish-layer-version \
    --zip-file fileb://build-bootstrap-out/$ZIP_NAME \
    --layer-name $LAYER_NAME
