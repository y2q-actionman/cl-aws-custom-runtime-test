#!/bin/sh

LAYER_NAME="lisp-layer"

aws lambda publish-layer-version \
    --zip-file fileb://aws_lambda_bootstrap.zip \
    --layer-name $LAYER_NAME
