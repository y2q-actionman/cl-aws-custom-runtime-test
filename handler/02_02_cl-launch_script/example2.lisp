":" ; exec cl-launch -Q -sm aws-lambda-function-util -- "$@"

(uiop:define-package :test
  (:mix :cl))

(in-package :test)

(defun main (argv)
  )

;; from https://github.com/fare/fare-scripts/blob/6794c06c54f8adbd08fda55c49d3075a0318e64c/unmime.lisp#L1
