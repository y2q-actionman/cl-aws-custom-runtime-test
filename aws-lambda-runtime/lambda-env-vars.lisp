;;; Load AWS Lambda environmental variables.
;;; See: https://docs.aws.amazon.com/lambda/latest/dg/current-supported-versions.html

(in-package :aws-lambda-runtime)

(defvar *_HANDLER* nil
  "The handler location configured on the function. (Used by `bootstrap')")

(defvar *AWS-REGION* nil
  "The AWS region where the Lambda function is executed.")

(defvar *AWS-EXECUTION-ENV* nil
  "The runtime identifier, prefixed by 'AWS_Lambda_'. For example, 'AWS_Lambda_java8'.")

(defvar *AWS-LAMBDA-FUNCTION-NAME* nil
  "The name of the function.")

(defvar *AWS-LAMBDA-FUNCTION-MEMORY-SIZE* nil
  "The amount of memory available to the function in MB.")

(defvar *AWS-LAMBDA-FUNCTION-VERSION* nil
  "The version of the function being executed.")

(defvar *AWS-LAMBDA-LOG-GROUP-NAME* nil
  "The name of the Amazon CloudWatch Logs group and stream for the function.")

(defvar *AWS-LAMBDA-LOG-STREAM-NAME* nil
  "The name of the Amazon CloudWatch Logs group and stream for the function.")

(defvar *AWS-ACCESS-KEY-ID* nil
  "Access keys obtained from the function's execution role.")

(defvar *AWS-SECRET-ACCESS-KEY* nil
  "Access keys obtained from the function's execution role.")

(defvar *AWS-SESSION-TOKEN* nil
  "Access keys obtained from the function's execution role.")

(defvar *LANG* nil
  "'en_US.UTF-8'. This is the locale of the runtime.")

(defvar *TZ* nil
  "The environment's timezone (UTC). The execution environment uses NTP to synchronize the system clock.")

(defvar *LAMBDA-TASK-ROOT* nil
  "The path to your Lambda function code. (Used by `bootstrap')")

(defvar *LAMBDA-RUNTIME-DIR* nil
  "The path to runtime libraries.")

(defvar *PATH* nil
  "'/usr/local/bin:/usr/bin/:/bin:/opt/bin'.")

(defvar *LD-LIBRARY-PATH* nil
  "'/lib64:/usr/lib64:$LAMBDA_RUNTIME_DIR:$LAMBDA_RUNTIME_DIR/lib:$LAMBDA_TASK_ROOT:$LAMBDA_TASK_ROOT/lib:/opt/lib'.")

(defvar *AWS-LAMBDA-RUNTIME-API* nil
  "(custom runtime) The host and port of the runtime API. (Used by `bootstrap')")

(defun load-aws-lambda-environmental-variables ()
  (setf *_HANDLER* (uiop:getenv "_HANDLER")
	*AWS-REGION* (uiop:getenv "AWS_REGION")
	*AWS-EXECUTION-ENV* (uiop:getenv "AWS_EXECUTION_ENV")
	*AWS-LAMBDA-FUNCTION-NAME* (uiop:getenv "AWS_LAMBDA_FUNCTION_NAME")
	*AWS-LAMBDA-FUNCTION-MEMORY-SIZE* (uiop:getenv "AWS_LAMBDA_FUNCTION_MEMORY_SIZE")
	*AWS-LAMBDA-FUNCTION-VERSION* (uiop:getenv "AWS_LAMBDA_FUNCTION_VERSION")
	*AWS-LAMBDA-LOG-GROUP-NAME* (uiop:getenv "AWS_LAMBDA_LOG_GROUP_NAME")
	*AWS-LAMBDA-LOG-STREAM-NAME* (uiop:getenv "AWS_LAMBDA_LOG_STREAM_NAME")
	*AWS-ACCESS-KEY-ID* (uiop:getenv "AWS_ACCESS_KEY_ID")
	*AWS-SECRET-ACCESS-KEY* (uiop:getenv "AWS_SECRET_ACCESS_KEY")
	*AWS-SESSION-TOKEN* (uiop:getenv "AWS_SESSION_TOKEN")
	*LANG* (uiop:getenv "LANG")
	*TZ* (uiop:getenv "TZ")
	*LAMBDA-TASK-ROOT* (uiop:getenv "LAMBDA_TASK_ROOT")
	*LAMBDA-RUNTIME-DIR* (uiop:getenv "LAMBDA_RUNTIME_DIR")
	*PATH* (uiop:getenv "PATH")
	*LD-LIBRARY-PATH* (uiop:getenv "LD_LIBRARY_PATH")
	*AWS-LAMBDA-RUNTIME-API* (uiop:getenv "AWS_LAMBDA_RUNTIME_API")))
