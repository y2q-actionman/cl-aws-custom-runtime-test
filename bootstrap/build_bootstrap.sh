#!/bin/bash

/usr/local/bin/sbcl \
    --non-interactive \
    --load "/work/ql_libs_at_docker_build.lisp" \
    --load "/out/bootstrap.lisp" \
    --eval "(sb-ext:save-lisp-and-die \"bootstrap\" :executable t :toplevel 'aws-bootstrap-test:bootstrap)"

zip /out/aws_lambda_bootstrap.zip bootstrap

rm bootstrap
