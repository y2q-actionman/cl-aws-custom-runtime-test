An trivial example of using Common Lisp (sbcl) on AWS lambda
======

This is experimental codes for using SBCL as a custom runtime on AWS lambda.

# Requirements

- docker
- AMI roles for deploying and execiting on AWS lambda.

# Architecture

## softwares

- Docker :: For building binaries on AWS lambda environment.
- SBCL :: For AWS lambda bootstrap and all processiong of handlers.
- Quicklisp :: For getting libraries (drakma, cl-json, etc.)

## this repo files

- Dockerfile

	provides build environment.

	1. get and builds SBCL and quicklisp, and install.
	2. get some quicklisp libs.
	3. copies files below.

- install_ql_libs.lisp

	Installs some quicklisp libs at Docker build time.

	FIXME: rename to bootstrap-libs, and split to Docker build and bootstrap build.

	(Such a file is:: Installs some quicklisp libs at bootstrap build time.)

- bootstrap.lisp

	Works as bootstrap.

- build.sh

	Build "bootstrap" binary from `bootstrap.lisp`, and zip-s them.

	TODO: compile handler libs as fasls, and compose it.


# Usage

## Move cwd to this repo.

``` shell
cd <somewhere>
```

## Build Docker container

``` shell
docker build -t test .
```

## Build bootstrap and delivery zip file by docker.

`build.sh` and next code assumes '/out' to output dir. (FIXME)

``` shell
docker run -v `pwd`/out:/out -w /out test /work/build.sh
```

## Upload built zip to AWS lambda

This code depends AWS-cli environment variables, and uses `$role` to AMI role.

``` shell
aws lambda create-function --function-name lisp-runtime \
	--zip-file fileb://out/aws_lambda_example.zip \
	--runtime provided \
	--handler currently.ignored \
	--role $ROLE
```

## Runs it!

(stub)
(screenshot here)

## (If renew bootstrap)

Delete it and create again.

``` shell
aws lambda delete-function --function-name lisp-runtime
```

# References

- https://docs.aws.amazon.com/ja_jp/lambda/latest/dg/runtimes-custom.html

	Official references.

- https://docs.aws.amazon.com/ja_jp/lambda/latest/dg/runtimes-walkthrough.html

	Official tutorial on bash. This repo's code mainly derived from it.

- https://www.m3tech.blog/entry/aws-lambda-custom-runtime

	An example of Nim language. In Japanese.


# TODO

- specify output dir by the host script

- support `$_HANDLER`

  -> loading fasls by `$_HANDLER` contents, and run.
  Reads it as `<filename>.<symbol-name of :cl-user package>`

- consider where to place libraries. I think there are three places:
  1. Into Dockerfile, resulting contained into bootstrap.
  2. Into bootstrap.
  3. Handler's fasl.


# License

Copyright (c) 2018 YOKOTA Yuki <y2q.actionman@gmail.com>

This program is free software. It comes without any warranty, to the extent permitted by applicable law. You can redistribute it and/or modify it under the terms of the Do What The Fuck You Want To Public License, Version 2, as published by Sam Hocevar. See the COPYING file for more details.