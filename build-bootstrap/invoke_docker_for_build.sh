#!/bin/sh

cd `dirname $0`

# build VM.
docker build -t test .

# make a zip file.
docker run --rm \
       -v `pwd`:/out \
       -v `pwd`/../aws-lambda-runtime:/aws-lambda-runtime \
       test /out/build_bootstrap_in_vm.sh
