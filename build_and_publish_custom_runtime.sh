#!/bin/sh

if [ -z "$1" ]; then
    _layer_name="lisp-layer"
else
    _layer_name=$1
fi

if [ -z "$2" ]; then
    _sbcl_version="2.2.2"
else
    _sbcl_version=$1
fi


cd `dirname $0`

# build a VM.
VM_NAME="cl-aws-buildenv"
docker build -t $VM_NAME .

# make a zip file.
ZIP_NAME="aws_lambda_bootstrap.zip"
docker run --rm \
       -v `pwd`/build-bootstrap-out:/out \
       $VM_NAME /out/build_bootstrap_in_vm.sh $ZIP_NAME

# publish as a AWS custom function layer
LAYER_NAME="$_layer_name"
aws lambda publish-layer-version \
    --zip-file fileb://build-bootstrap-out/$ZIP_NAME \
    --layer-name $LAYER_NAME
