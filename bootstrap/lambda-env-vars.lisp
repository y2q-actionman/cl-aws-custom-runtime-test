;;; Load AWS Lambda environmental variables.
;;; See: https://docs.aws.amazon.com/lambda/latest/dg/current-supported-versions.html

(in-package :aws-bootstrap)

(defmacro define-aws-lambda-env-var (name getenv-name &optional doc)
  `(defvar ,name
     (load-time-value (sb-ext:posix-getenv ,getenv-name))
     ,doc))

(define-aws-lambda-env-var *_HANDLER* "_HANDLER" ; used by bootstrap.
  "The handler location configured on the function.")

(define-aws-lambda-env-var *AWS-REGION* "AWS_REGION"
  "The AWS region where the Lambda function is executed.")

(define-aws-lambda-env-var *AWS-EXECUTION-ENV* "AWS_EXECUTION_ENV"
  "The runtime identifier, prefixed by 'AWS_Lambda_'. For example, 'AWS_Lambda_java8'.")

(define-aws-lambda-env-var *AWS-LAMBDA-FUNCTION-NAME* "AWS_LAMBDA_FUNCTION_NAME"
  "The name of the function.")

(define-aws-lambda-env-var *AWS-LAMBDA-FUNCTION-MEMORY-SIZE* "AWS_LAMBDA_FUNCTION_MEMORY_SIZE"
  "The amount of memory available to the function in MB.")

(define-aws-lambda-env-var *AWS-LAMBDA-FUNCTION-VERSION* "AWS_LAMBDA_FUNCTION_VERSION"
  "The version of the function being executed.")

(define-aws-lambda-env-var *AWS-LAMBDA-LOG-GROUP-NAME* "AWS_LAMBDA_LOG_GROUP_NAME"
  "The name of the Amazon CloudWatch Logs group and stream for the function.")

(define-aws-lambda-env-var *AWS-LAMBDA-LOG-STREAM-NAME* "AWS_LAMBDA_LOG_STREAM_NAME"
  "The name of the Amazon CloudWatch Logs group and stream for the function.")

(define-aws-lambda-env-var *AWS-ACCESS-KEY-ID* "AWS_ACCESS_KEY_ID"
  "Access keys obtained from the function's execution role.")

(define-aws-lambda-env-var *AWS-SECRET-ACCESS-KEY* "AWS_SECRET_ACCESS_KEY"
  "Access keys obtained from the function's execution role.")

(define-aws-lambda-env-var *AWS-SESSION-TOKEN* "AWS_SESSION_TOKEN"
  "Access keys obtained from the function's execution role.")

(define-aws-lambda-env-var *LANG* "LANG"
  "'en_US.UTF-8'. This is the locale of the runtime.")

(define-aws-lambda-env-var *TZ* "TZ"
  "The environment's timezone (UTC). The execution environment uses NTP to synchronize the system clock.")

(define-aws-lambda-env-var *LAMBDA-TASK-ROOT* "LAMBDA_TASK_ROOT" ; used by bootstrap.
  "The path to your Lambda function code.")

(define-aws-lambda-env-var *LAMBDA-RUNTIME-DIR* "LAMBDA_RUNTIME_DIR"
  "The path to runtime libraries.")

(define-aws-lambda-env-var *PATH* "PATH"
  "'/usr/local/bin:/usr/bin/:/bin:/opt/bin'.")

(define-aws-lambda-env-var *LD-LIBRARY-PATH* "LD_LIBRARY_PATH"
  "'/lib64:/usr/lib64:$LAMBDA_RUNTIME_DIR:$LAMBDA_RUNTIME_DIR/lib:$LAMBDA_TASK_ROOT:$LAMBDA_TASK_ROOT/lib:/opt/lib'.")

(define-aws-lambda-env-var *AWS-LAMBDA-RUNTIME-API* "AWS_LAMBDA_RUNTIME_API" ; used by bootstrap.
  "(custom runtime) The host and port of the runtime API.")
