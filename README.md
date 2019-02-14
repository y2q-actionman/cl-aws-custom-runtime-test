An example of using Common Lisp (sbcl) as a custom runtime on AWS lambda
======

(2019-2-13) Now updating.
TODO List:
- Update this README.




This is experimental codes for using SBCL as a custom runtime on AWS lambda.

# Features

- You can write AWS lambda function with Common Lisp.
- Normal lisp file, compiled fasls, and Roswell scripts are ready to use.
- You can use any Lisp libraries! (if built correctly.)

# Requirements

- Docker :: For building binaries on AWS lambda environment (aka Amazon Linux). 
- Proper AMI roles :: for deploying and execiting on AWS lambda.

# Architecture

## softwares

- Docker
- SBCL :: For AWS lambda bootstrap and all processiong of handlers.
- Quicklisp :: For getting libraries (drakma, cl-json, etc.)

## this repo files

## `/aws-lambda-runtime`

## `/build-bootstrap`

- Dockerfile

	provides build environment.

	1. get and builds SBCL and quicklisp, and install.
	2. get some quicklisp libs.
	3. copies files below.

- install_ql_libs.lisp

	Installs some quicklisp libs at Docker build time.

	FIXME: rename to bootstrap-libs, and split to Docker build and bootstrap build.

	(Such a file is:: Installs some quicklisp libs at bootstrap build time.)


- build.sh

	Build "bootstrap" binary from `bootstrap.lisp`, and zip-s them.

	TODO: compile handler libs as fasls, and compose it.
	
## `/handler`

- bootstrap.lisp

	Works as bootstrap.


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
	
- https://qiita.com/snmsts@github/items/ab6888f35f3d1237fae5

	Uses Common Lisp on AWS, on pipes of node.js.


# TODO

- specify output dir by the host script

- support `$_HANDLER`

  -> loading fasls by `$_HANDLER` contents, and run.
  Reads it as `<filename>.<symbol-name of :cl-user package>`

- consider where to place libraries. I think there are three places:
  1. Into Dockerfile, resulting contained into bootstrap.
  2. Into bootstrap.
  3. Handler's fasl.


# History

## 2019-02-15

Many enhancements:

- (DONE) Trying to fix weird `EPERM` error. (Drop IPv6 support, restarting poll(2), etc.)
- (DONE) pick all env variables
- (DONE) invocation error and initialiation error handling (https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html)
- (DONE) Support new handler type -- AWS Lambda's standard format, and Lisp forms.
- (DONE) rename to aws-lambda-runtime?
- (DONE) Fix and move Docker related files
- (DONE) Roswell script loader.
- (DONE) Use uiop for getenv.
- (DONE) New examples: use new `find-handler`.
- (DONE) New examples: monolithic fasl examples.
- (DONE) New examples: roswell script.


## 2018-12-06

https://github.com/y2q-actionman/cl-aws-custom-runtime-test/releases/tag/2018-12-06-cl-advent-calendar

First version. This is only experimental -- does not have any
error-handling codes. However, quite simple.

Please see [This article (japanese)](http://y2q-actionman.hatenablog.com/entry/2018/12/06/AWS_Lambda_%E3%81%AE_Custom_Runtime_%E3%81%A8%E3%81%97%E3%81%A6_Common_Lisp_%28sbcl%29_%E3%82%92%E4%BD%BF%E3%81%86)

## Related Works

(stub)

- new one
- windymelt
- ECL
- Mr.Sano 

## How to get AWS-lambda contexts.

AWS-lambda contexts come into two parts.

1. Environmental variables.

  They are held in varibles exported by `aws-lambda-runtime` package.
  Please see _lambda-env-vars.lisp_.
  
2. Http headers.

   They are passed into the second argument of your handler function
   as an alist (because I uses `Drakma`.)
   
   Example:
```lisp
    ((:CONTENT-TYPE . "application/json")
       (:LAMBDA-RUNTIME-AWS-REQUEST-ID
        . "092aafa5-c7cd-46ea-b305-d2e4d3b77b37")
       (:LAMBDA-RUNTIME-DEADLINE-MS . "1550171924243")
       (:LAMBDA-RUNTIME-INVOKED-FUNCTION-ARN
        . "arn:aws:lambda:************:***********:function:ros_script_handler") ; masked...
       (:LAMBDA-RUNTIME-TRACE-ID
        . "Root=1-5c65bf11-b8f197fac0ac10e8bfc0b55a;Parent=019bf6fb19d9378c;Sampled=0")
       (:DATE . "Thu, 14 Feb 2019 19:18:41 GMT") (:CONTENT-LENGTH . "51")
       (:CONNECTION . "close"))
```

## Where to place new libraries?

- Build a new runtime with libraries

Pros: Simpler AWS-lambda function codes, faster startup.
Cons: You must manage many runtimes.

- Ships AWS-lambda function codes with a built FASL.

Pros: You can share runtimes with other AWS-lambda functions.
Cons: Slower Startup. AWS-lambda function codes must `load` it.






# License

Copyright (c) 2018 YOKOTA Yuki

This program is free software. It comes without any warranty, to the extent permitted by applicable law. You can redistribute it and/or modify it under the terms of the Do What The Fuck You Want To Public License, Version 2, as published by Sam Hocevar. See the COPYING file for more details.





