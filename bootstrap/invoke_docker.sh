#!/bin/sh

# build VM.
docker build -t test .

# make a zip file.
docker run --rm -v `pwd`:/out test /out/build_bootstrap.sh
