#!/bin/sh

docker build -t test .

# docker run -v `pwd`/out:/out -w /out -it test
# /work/build.sh

docker run -v `pwd`/out:/out -w /out test /work/build.sh
