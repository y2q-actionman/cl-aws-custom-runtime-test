#!/bin/sh

cd `dirname $0`

LAYER_NAME="lisp-layer"

# build VM.
docker build -t test .

# make a zip file.
docker run --rm \
       -v `pwd`:/out \
       -v `pwd`/../aws-lambda-runtime:/aws-lambda-runtime \
       test /out/build_bootstrap_in_vm.sh

# publish as a AWS custom function layer
aws lambda publish-layer-version \
    --zip-file fileb://aws_lambda_bootstrap.zip \
    --layer-name $LAYER_NAME
