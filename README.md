An example of using Common Lisp (sbcl) as a custom runtime on AWS lambda
======

This is an example for using SBCL as a custom runtime on AWS lambda.

# Overview

## Features

- You can write AWS lambda function with Common Lisp.
- Normal lisp files, compiled fasls, and Roswell scripts are ready to use.
- You can use any Lisp libraries (if it built correctly.)

A Screenshot of AWS Lambda function console is here:
![load-another-fasl exec screenshot](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/wiki/images/load_another_fasl_exec.png)
<!-- FIXME: use other image! -->

## News (2023-12-25)

Changed Dockerfile to use Amazon Linux 2 instead of Amazon Linux 1, which will be deprecated at Jan 8, 2024.
For older news, see "History "below.

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

# Build and publish a custom runtime.

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

All building process is written in `build_and_publish_custom_runtime.sh` file.
In this section, I'll explain what the script does.

### About the Dockerfile.

`build-bootstrap/Dockerfile` is a definition of buiding environment.
This Dockerfile does following:

1. It starts with the amazonlinux.
2. It gets SBCL from the official repository, and installs it. (I've tried `yum` of amazonlinux, but it does not have sbcl.)
3. It gets Quicklisp and install it.
4. It copies runtime sources (`aws-lambda-runtime`, `aws-lambda-function-util`, and `aws-lambda-runtime-additional-libraries`), and install them with some libraries fetched via Quicklisp.

### Build a Docker VM.

To build a Docker VM named `cl-aws-buildenv`, do following:

``` shell
docker build -t cl-aws-buildenv .
```
(this is a part of `build_and_publish_custom_runtime.sh`)

### Build a AWS custom runtime.

Here, what you to do is:

1. Start the Docker VM.
2. In the VM, build your runtime to a single binary named **bootstrap**.
3. Makes a zip file contains the **bootstrap** file.

``` shell
docker run --rm \
       -v `pwd`/build-bootstrap-out:/out \
       cl-aws-buildenv /out/build_bootstrap_in_vm.sh
```
(this is a part of `build_and_publish_custom_runtime.sh`)

This code starts the VM and calls `build-bootstrap-out/build_bootstrap_in_vm.sh`.
This script does following:

1. Load required libraries and our custom runtime.
1. Make a single binary with `sb-ext:save-lisp-and-die` feature.
   I named it to **bootstrap** and restarts with
   `aws-bootstrap-test:bootstrap`, which is our bootstrap function.
1. `zip` it.

After that, `aws_lambda_bootstrap.zip` will be made.

(Additionaly, this script makes a text file lists built-in libraries.
This is only for provide some information to writers of AWS Lambda function.)

(TODO: I will clean *build_bootstrap_in_vm.sh* by splitting Lisp codes to another file.)

### Publish it is as a AWS Lambda's custom function layer.

Upload the zip as a AWS Lambda's layer.

``` shell
aws lambda publish-layer-version \
    --layer-name lisp-layer \
    --zip-file fileb://aws_lambda_bootstrap.zip
```
(this is a part of `build_and_publish_custom_runtime.sh`)

This command makes a new AWS custom runtime layer, named **lisp-layer**.

After that, the custom layer is uploaded like this image:
![AWS lambda lisp-layer](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/wiki/images/lambda_layer.png)
(Because I debugged and updated the layer many times, its version is 23!)

# Makes a AWS Lambda function using Lisp.

Using the **lisp-layer**, you can run any Lisp codes in AWS Lambda.
In this section, I explain this runtime's calling conventions and show some examples.

## Calling Conventions

### How to find the entry point.

The entry point of your AWS lambda functions is specified by 'handler' parameter.
This custom runtime reads this parameter as following:

#### AWS Lambda's [standard syntax](https://docs.aws.amazon.com/lambda/latest/dg/runtimes-custom.html) : `<file>.<method>`.

The `file` is loaded by `cl:load`, and `method` is read as a symbol and `funcall`'ed for every request.
The symbol is `funcall`'ed with two arguments, request data and HTTP headers, and its return value will be returned as AWS Lambda function's return value.

#### A script file name.

In this case, 'handler' parameter is considered as a file name. The specified file is loaded, and its *main* function is called with no arguments.
When *main* is called, `*standard-input*` is bound to the request, `aws-lambda-runtime:*HEADER-ALIST*` is bound to HTTP headers, and strings written to `*standard-output*` are used as AWS-Lambda's result.

#### other patterns

For more information, please see [`aws-lambda-runtime:find-handler`'s docstring](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/master/aws-lambda-runtime/find-handler.lisp#L85)

### How to get AWS-Lambda contexts.

AWS-Lambda contexts come from two parts.

1. Environmental variables.

  They are bound to special varibles in `aws-lambda-runtime` package.
  Please see _lambda-env-vars.lisp_.
  
2. HTTP headers.

   They are bound to `aws-lambda-runtime:*HEADER-ALIST*` as an alist
   (because I uses `Drakma`.)
   
   In addition, when AWS Lambda's standard syntax is used at 'handler',
   it is passed at the second argument of your handler function.
   
   HTTP header example:
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

## Example 1 : A simple one.

A simple example is in  **handler/01_simple_handler/**.

[simple_handler.lisp](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/master/handler/01_simple_handler/simple_handler.lisp) file contains `simple-handler` function. It will be called.

[upload_function.sh](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/master/handler/01_simple_handler/upload_function.sh) upload it with two steps:

1. Make a zip file containing the *simple_handler.lisp* file.
2. Upload it as a AWS Lambda function.

At uploading, I specify `--runtime` and `--layers` parameter. to use the **lisp-layer** runtime.

And I used `--handler` parameter like this:

```
    --handler "simple_handler.cl-user::simple-handler"
```

It is AWS Lambda's standard syntax. This says "Load `simple_handler`, and call `cl-user::simple-handler` for each request."

After uploading, the new AWS Lambda function looks like this:
![uploaded simple handler screenshot](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/wiki/images/simple_handler_uploaded.png)

(Thanks to the AWS-Lambda console, you can edit your code in it!)

I run it with the console's test:
![executed test in simple handler screenshot](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/wiki/images/simple_handler_executed.png)

## Example 2 : Using scripts.

An example using roswell scripts is in  **handler/02_01_roswell_script_text/**.

I wrote these scripts:

- [empty.ros](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/master/handler/02_01_roswell_script_text/empty.ros) does nothing. (It is only for testing.) 
- [hello.ros](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/master/handler/02_01_roswell_script_text/hello.ros) prints "Hello, World" message.
- [echo.ros](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/master/handler/02_01_roswell_script_text/echo.ros) returns the request as-is.

[upload_function.sh](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/master/handler/02_01_roswell_script_text/upload_function.sh) works like *simple-handler*'s one, except zips these **.ros** files, and specifies `--handler` like this:

```
    --handler "hello.ros"
```

It is script-file syntax. This says "Load `hello.ros`, and call its `main` function for each request."

After uploading, the new AWS Lambda function looks like this. You can see three files:
![uploaded ros handler screenshot](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/wiki/images/ros_handler_uploaded.png)

I run it with the console's test:

![executed hello.ros screenshot](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/wiki/images/ros_handler_exec_hello.png)

And I change 'handler' to `echo.ros` and run it again.

![executed echo.ros screenshot](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/wiki/images/ros_handler_exec_echo.png)
(TODO: this screenshot executed 'echo.ros' but still shows 'hello.ros'. I should show 'echo.ros'...)

`echo.ros` copies the request to the response, this screenshot looks so.

## Example 3 : Ships with other libraries.

### Where to place new libraries?

I decided to build this custom runtime [with some JSON libs](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/master/aws-lambda-runtime-additional-libraries/aws-lambda-runtime-additional-libraries.asd). But there are obviously short of real-world programmings.
So, I consider how to ship my code with other libraries. I think some ways:

#### Build a new runtime with libraries.

Re-building a new runtime with wanted libraries.

It makes handler codes simpler and faster at starting up.
But you must manage many AWS custom function layers.

#### Ships AWS-lambda function codes with a built FASL.

Making a FASL with wanted libraries, and ships a lisp file with the FASL.

In this approach, you can share same runtimes in many AWS Lambda functions.
But this makes startup slower, and the handler file must `load` the FASL correctly.

I wrote an example for this case.

#### Makes a single FASL contains all.

This is a variant of second approach. Making a FASL with wanted libraries *AND* handler definitions.

#### Use `ql-bundle`

I've not tried this, but I think it is good.

### A lisp file and one fasl contains libraries.

An example using a fasl is in  **handler/03_01_load_another_fasl/**.

[needed-libs-example.asd](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/master/handler/03_01_load_another_fasl/needed-libs-example.asd) file defines a `defsystem` for making FASL with wanted libraries. In this example, I used [`jp-numeral`](https://github.com/y2q-actionman/jp-numeral) (A joke lib converts integers to Japanese numerals.)

[upload_function.sh](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/master/handler/01_simple_handler/upload_function.sh) does three works:

1. Makes a monolithic fasl from **needed-libs-example.asd**, using [build_fasl_in_vm.sh](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/master/handler/03_01_load_another_fasl/build_fasl_in_vm.sh) in **cl-aws-buildenv** VM and . (In this phase, you must build fasl with this VM. Without this, [FASL error will be raised](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/wiki/images/fasl_error.png).)
2. Make a zip file containing the fasl and *main.lisp* file.
3. Upload it as a AWS Lambda function.

In [main.lisp](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/master/handler/03_01_load_another_fasl/main.lisp) file, [it loads the built fasl at load time](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/master/handler/03_01_load_another_fasl/main.lisp#L4).

I specifies `--handler` like this:

```
    --handler "main.test-parse-handler"
```

Because of this, this runtime does below at start:

1. Loads main.lisp file.
2. At loading it, the build fasl will be loaded.
3. Uses `test-parse-handler` for handling requests.

After uploading, the new AWS Lambda function looks like this:
![uploaded load-another-fasl screenshot](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/wiki/images/load_another_fasl_uploaded.png)

I changed test data for using integers:

![load-another-fasl test screenshot](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/wiki/images/load_another_fasl_test.png)

And run it:

![load-another-fasl exec screenshot](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/wiki/images/load_another_fasl_exec.png)

It seems I successed to use another library.

### One big fasl.

I wrote another example, which makes a big fasl with libraries AND handler codes.
There are in **handler/03_02_one_big_fasl//**.

# Known problems

I sometimes saw `EPERM` on poll(2) ([Screenshot is here](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/wiki/images/EPERM_on_poll.png)).
I don't now what causes this, but I noticed that retrying to get requests works well. [This code](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/9d548a8db5dcd7399818bf8f25a08920c7bc48d1/aws-lambda-runtime/bootstrap.lisp#L63) is written for this, but I want to know how to fix it correctly.

(@windymelt suggested `EPERM` is caused by IPv6 socket, referring to [Erlang's case](https://blog.marshallbrekka.com/post/2016-06-10/erlang-on-aws-lambda/).  [In his article (Japanese)](https://blog.3qe.us/entry/2019/02/06/013916), he tried to drop IPv6 support and made a working custom runtime with Clozure CL. I wrote codes for this case with SBCL ([unused/usocket-patch-drop-ipv6.lisp](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/blob/master/aws-lambda-runtime/unused/usocket-patch-drop-ipv6.lisp)), but failed to work. I thought that if IPv6 effects, `EPERM` should be reported by socket(2), not poll(2).)

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

## Add ql-bundle example.

## Add cl-launch script support.

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

## 2023-12-25

- Changed Dockerfile to use Amazon Linux 2 instead of Amazon Linux 1, which will be [deprecated at Jan 8, 2024](https://docs.aws.amazon.com/en_us/lambda/latest/dg/images-create.html#runtimes-images-provided)
- Updated SBCL to 1.5.5. This is the newest version can be built on Amazon Linux 2.

## 2019-02-26

Many enhancements:

- Trying to fix weird `EPERM` error. (Drop IPv6 support, restarting poll(2), etc.)
- Picks all env variables.
- invocation error and initialiation error handling (https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html)
- Support new handler type -- AWS Lambda's standard format, roswell script, and Lisp forms.
- Include JSON libs into runtime at build time.
- New examples: monolithic fasl examples.
- New examples: roswell script.

## 2018-12-06

[First version is here.](https://github.com/y2q-actionman/cl-aws-custom-runtime-test/releases/tag/2018-12-06-cl-advent-calendar).
This is only experimental -- does not have any error-handling codes. However, quite simple.

About this version, please see [This article (japanese)](http://y2q-actionman.hatenablog.com/entry/2018/12/06/AWS_Lambda_%E3%81%AE_Custom_Runtime_%E3%81%A8%E3%81%97%E3%81%A6_Common_Lisp_%28sbcl%29_%E3%82%92%E4%BD%BF%E3%81%86)

# License

The MIT License. See LICENSE file.
