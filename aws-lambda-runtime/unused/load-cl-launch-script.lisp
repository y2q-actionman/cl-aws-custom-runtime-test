(in-package :aws-lambda-runtime)

(defmacro string-multi-ecase ((string_) &body clauses_)
  "Like `ecase' except works only for string."
  (loop with string = (gensym "string")
     for (clause-key . clause-body) in clauses_
     as clause-key-list = (if (listp clause-key)
			      clause-key
			      (list clause-key))
     as cond-clause = `((member ,string ',clause-key-list :test #'string=)
			,@clause-body)
     collect cond-clause into cond-clauses
     finally (return `(let ((,string ,string_))
			(cond ,@cond-clauses
			      (t (error "Not matched with string-multi-ecase ~A" ,string)))))))

(defun parse-cl-launch-command-line (first-line)
  ;; assumes this is called by `cl-launch-script'
  (let ((cmd-lines (split-sequence:split-sequence " " first-line)))
    (loop for cmd = (pop cmd-lines)
       while cmd-lines
       if (or (and (string-prefix-p "#!" cmd)
		   (string-suffix-p "cl" cmd))
	      (string= cmd "cl-launch"))
       do (loop-finish))
    (loop with restart = nil
       with entry = nil
       with init-forms = nil
       for arg = (pop cmd-lines)
       while cmd-lines
       do (flet ((pop-and-read ()
		   (read-from-string (pop cmd-lines))))
	    (string-multi-ecase (arg)
	      (("-?" "-h" "--help"
		     "-H" "--more-help"
		     "-V" "--version"
		     "-x" "--execute")
	       (warn "Ignored cl-launch parameter ~A" arg))
	      (("-u" "--update"
		     "-w" "--wrap"
		     "-l" "--lisp"
		     "-m" "--image"
		     "-f" "--file"
		     "-S" "--source-registory"
		     "-DE" "--dispatched-entry"
		     "-F" "--final"
		     "-I" "--include" "+I" "--no-include"
		     "-R" "--rc" "+R" "--no-rc"
		     "-Q" "--quicklisp" "+Q" "--no-quicklisp"
		     "-b" "--clbuild" "+b" "--no-clbuild"
		     "-v" "--verbose" "-q" "--quiet"
		     "-o" "--output"
		     "-d" "--dump")
	       (warn "Ignored cl-launch parameter ~A ~A" arg (pop cmd-lines)))
	      (("-L" "--load")
	       (load (pop cmd-lines)))
	      (("-s" "--system" "--load-system")
	       (ql:quickload (pop cmd-lines)))
	      (("-p" "--package")
	       (setf *package* (find-package (pop-and-read))))
	      (("-sp" "--system-package")
	       (let ((next-arg (pop cmd-lines)))
		 (setf cmd-lines (list* "-s" next-arg "-p" next-arg cmd-lines))))
	      (("-e" "--eval")
	       (eval (pop-and-read)))
	      ("--require"
	       (require (pop cmd-lines)))
	      (("-r" "--restart")
	       (setf restart (pop-and-read)))
	      (("-E" "--entry")
	       (setf entry (pop-and-read)))
	      (("-i" "--init")
	       (push (pop-and-read) init-forms))
	      (("-ip" "--print")
	       (push `(cl:princ ,(pop-and-read)) init-forms))
	      (("-iw" "--write")
	       (push `(cl:write ,(pop-and-read)) init-forms))
	      ("-X"
	       (loop for i = (pop cmd-lines)
		  while i
		  until (string= i "--")))))
       finally
	 (eval `(progn ,@ (nreverse init-forms)))
	 (return (values entry restart)))))

(defun find-handler-from-cl-launch-file (handler-string)
  "Tries to find a handler from a cl-launchscript name.
See `find-handler''s docstring."
  (let (entry restart main-of-body)
    (with-open-file (in handler-string)
      (with-standard-io-syntax
	(let ((first-line (read-line in)))
	  (unless (or (string-prefix-p "#!/usr/bin/cl" first-line)
		      (search "cl-launch" first-line))
	    (error "Unknown file type: ~A" handler-string))
	  (setf (values entry restart)
		(parse-cl-launch-command-line first-line)))
	(setf main-of-body
	      (load-script-body in))))
    (wrap-script-main-to-aws-lambda-convention
     (cond (entry			; arity 1
	    (let ((func (ensure-function entry)))
	      (lambda () (funcall func nil))))
	   (restart			; arity 0
	    (ensure-function restart))
	   (main-of-body		; arity (assumed 1)
	    (let ((func (ensure-function main-of-body)))
	      (lambda () (funcall func nil))))))))
