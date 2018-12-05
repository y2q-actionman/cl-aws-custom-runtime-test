#!/bin/sh

aws lambda delete-function --function-name lisp-runtime

aws lambda create-function --function-name lisp-runtime --zip-file fileb://out/aws_lambda_example.zip  --handler currently.ignored --runtime provided --role $ROLE
