An example of using Common Lisp (sbcl) as a custom runtime on AWS lambda
======

This is an example for using SBCL as a custom runtime on AWS lambda.

# Overview

## Features

- You can write AWS lambda function with Common Lisp.
- Normal lisp files, compiled fasls, and Roswell scripts are ready to use.
- You can use any Lisp libraries (if it built correctly.)

## Requirements

- Docker :: For building binaries on AWS lambda environment (aka Amazon Linux). 
- Proper AMI roles :: for deploying and execiting on AWS lambda.
- AWS CLI :: for uploading AWS Lambda function.

## Contents of this repository

| directory name                              | description                                                                            |
|---------------------------------------------|----------------------------------------------------------------------------------------|
| `/aws-lambda-runtime/`                      | Lisp implementation of AWS Lambda custom runtime.                                      |
| `/aws-lambda-function-util/`                | Some utilities for building Lisp AWS Lambda function.                                  |
| `/aws-lambda-runtime-additional-libraries/` | Libraries not used by `aws-lambda-runtime` but built into our custom runtime together. |
| `/build-bootstrap-out/`                     | Scripts for building the runtime to a zip file.                                        |
| `/handler/`                                 | Some examples of AWS Lambda functions in Lisp.                                         |

# How to use

1. Build a custom runtime and publish it as a AWS Lambda layer.
2. Write your code as a AWS Lambda function.
3. Run!

## About codes in `/aws-lambda-runtime/`

(This section explains my cunstom runtime implementation.  If you just want to make a AWS Lambda function, please skip!)

`/aws-lambda-runtime/` contains codes for using Common Lisp as a AWS custom runtime.

This code uses these libraries:

- alexandia
- uiop :: for `getenv` and `chdir`
- drakma :: for main HTTP connection.

This code consists of these files:

| file name              | description                                                         |
|------------------------|---------------------------------------------------------------------|
| aws-lambda-runtime.asd | the ASDF defsystem.                                                 |
| package.lisp           | a package definition for this code, named `aws-lambda-runtime`.     |
| lambda-env-vars.lisp   | variables holding environmental variables of AWS Lambda enviroment. |
| find-handler.lisp      | codes for parsing AWS Lambda's 'handler' parameter.                 |
| bootstrap.lisp         | bootstraps and the main loop.                                       |

bootstrap.lisp does almost all tasks as a AWS Lambda's custom runtime.

- `bootstrap` function does initialization tasks -- retrieve settings, initializes, and starts `main-loop`.
- `main-loop` function does processing tasks -- get an event, calls your handler with it, and write its result back.


You can load this code in the following code:

``` lisp
(load "aws-lambda-runtime/aws-lambda-runtime.asd")
(asdf:load-system :aws-lambda-runtime)
```

((At developing, I load this code into my personal machine.
It does not work because there are no environmental variables or HTTP endpoints provided by AWS Lambda's environment, but it is suitable for test a small subsystem, like find-handler.lisp))

## Build a custom runtime

Let's build a new custom runtime on Amazon Linux environment.

All building process is written in `build_all.sh` file.
In this section, I'll explain what the script does.

### Make a Dockerfile.

`build-bootstrap/Dockerfile` is a definition of buiding environment.
This Dockerfile does following:

1. starts with the original amazonlinux.
1. Gets SBCL from the official repository, and installs it. (I've tried `yum` of amazonlinux, but it does not have sbcl.)
3. Gets quicklisp and install it.
4. Gets and installs some libraries needed by `aws-custom-runtime`, `aws-lambda-function-util`, and `aws-lambda-runtime-additional-libraries`..

### Build a Docker VM.

To build a Docker VM named `test`, do following:

``` shell
docker build -t test .
```
(this is a part of `build_all.sh`)

### Build a AWS custom runtime.

Here, what you to do is:

1. Start the Docker VM.
2. In the VM, build our runtime to a single binary named **bootstrap**.
3. Makes a zip file contains the **bootstrap** file.

``` shell
docker run --rm \
       -v `pwd`/build-bootstrap-out:/out \
       test /out/build_bootstrap_in_vm.sh
```
(this is a part of `build_all.sh`)

This code starts the VM and calls `build-bootstrap-out/build_bootstrap_in_vm.sh`.
This script does following:

1. Loads required libraries and our custom runtime.
1. Makes a single binary with `sb-ext:save-lisp-and-die` feature.
   I named it to **bootstrap** and restarts with
   `aws-bootstrap-test:bootstrap`, which is our bootstrap function.
1. `zip` it.

After that, `aws_lambda_bootstrap.zip` will be made.

(Additionaly, this script makes a text file lists built-in libraries.
This is only for provide some information to writers of AWS Lambda function.

### Publish is as a AWS Lambda's custom function layer.

Upload the zip as a AWS Lambda's layer.

``` shell
aws lambda publish-layer-version \
    --layer-name lisp-layer \
    --zip-file fileb://aws_lambda_bootstrap.zip
```
(this is a part of `build_all.sh`)


(TODO: Add a screenshot!)


## Makes a AWS Lambda function using Lisp.

(stub. I am currently writing it.)

### Example 1 : most simple one.

(stub. Please see `handler/01_simple_handler/`)

### Example 2 : ships with other libraries.

(stub. Please see `handler/02_load_other_fasls/`)

- variant 1 : with one fasl.
- variant 2 : only one big fasl.
- variant 3 : ql-bundle

### Example 3 : Using a roswell script

(stub. Please see `handler/03_roswell_script_text/`)

### How to get AWS-lambda contexts.

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

### Where to place new libraries?

- Build a new runtime with libraries

Pros: Simpler AWS-lambda function codes, faster startup.
Cons: You must manage many runtimes.

- Ships AWS-lambda function codes with a built FASL.

Pros: You can share runtimes with other AWS-lambda functions.
Cons: Slower Startup. AWS-lambda function codes must `load` it.

- Use ql-bundle

(TODO: make a new example)


# Known problems

- EPERM

	IPv6?
	poll(2)?
	
# TODO

## Use a keep-alive socket at getting requests.

For speeding up, use a keep-alive socket at getting requests.
  
## At processing requests by a handler, makes a new thread for it.

This reduces latency for getting the next request.  If making a thread
is too heavy, a kind of producer-consumer pattern can also be
considered.

## Add a way to change text encoding.

Add a environmental variable to change
`*drakma-default-external-format*`.

## Add other calling conventions for handlers.

Currently I get the whole body of request as a string and pass it to
the handler.  But, Drakma has a way to get a stream containing the
body of requests. so I thought passing the stream to handlers is one
way.
(If I implement this feature, I'll bind the stream to
`*standard-input*`, like the convention for scripts.)

Additionally, I think I want to follow [@windymelt's lambda-over-lambda](https://github.com/windymelt/lambda-over-lambda/blob/a4a074787009f36c0ea2a4853c0890c13f976a55/templates/template.ros#L50) convention, it uses `jsown`.
  
(I'll use environmental variables for selecting conventions;
`:string`, `:standard-io`, or `:jsown`.)

## Add cl-launch script support

# References

- https://docs.aws.amazon.com/ja_jp/lambda/latest/dg/runtimes-custom.html

	Official references.

- https://docs.aws.amazon.com/ja_jp/lambda/latest/dg/runtimes-walkthrough.html

	Official tutorial on bash. This repo's code mainly derived from it.

- https://www.m3tech.blog/entry/aws-lambda-custom-runtime

	An example of Nim language. In Japanese.
	
# Related Works

- https://qiita.com/snmsts@github/items/ab6888f35f3d1237fae5

  Uses Common Lisp on AWS, on pipes of node.js, by @snmsts.

- https://dev.classmethod.jp/cloud/aws/lambda-custom-runtime-common-lisp/

  A custom runtime example using ECL.
  This code uses a shell script for bootstrap, invokes a build Lisp binary.

- https://github.com/windymelt/lambda-over-lambda

  This code convert a roswell script to another script works as a AWS Lambda custom function.

- https://github.com/fisxoj/cl-aws-lambda

  A new custom runtime implementation.

# History

## 2019-02-21

- (DONE) Move test codes
- (DONE) Include JSON libs into runtime at build time.
- (DONE) Moved `build-monolithic-fasl` and fasl loaders.
- (DONE) Changed Dockerfile to include aws-lambda-runtime related codes.
- (DONE) List up ql builtin libs after building binary.
- (DONE) rename `aws-lambda-runtime-builtin-libraries` to `aws-lambda-runtime-additional-libraries`
- (DONE) Updating roswell script reader.
  - (DONE) Uses the last package if 'main' not found.
  - (DONE) small fake roswell runtime, removed Roswell itself from VM and runtime.
  - (DONE) change calling convention; use `*standard-input*` and a special value.
- (DONE) Build with JSON libraries?
  
- add new sample -- ql bundle
- add new sample -- wcs or jp-numeral

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

# License

Copyright (c) 2018 YOKOTA Yuki

This program is free software. It comes without any warranty, to the extent permitted by applicablep law. You can redistribute it and/or modify it under the terms of the Do What The Fuck You Want To Public License, Version 2, as published by Sam Hocevar. See the COPYING file for more details.
