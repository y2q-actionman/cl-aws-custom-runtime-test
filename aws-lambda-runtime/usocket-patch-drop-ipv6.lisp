;;; To avoid failures of poll(2) on EPERM, I drop IPv6 support.
;;; Please see:
;;; https://blog.marshallbrekka.com/post/2016-06-10/erlang-on-aws-lambda/

(in-package :aws-lambda-runtime)

(defun get-hosts-by-name-and-remove-ipv6 (name)
  "Calls the original `usocket:get-hosts-by-name' (via `original-get-host-by-name')
and removes IPv6 addresses, for supress making IPv6 sockets."
  (loop with all-addrs = (funcall 'original-get-host-by-name name)
     for addr in all-addrs
     unless (= 16 (length addr))	; see usocket's sbcl code.
     collect addr))

(defun patch-get-hosts-by-name ()
  "Replace `usocket:get-hosts-by-name' with our `get-hosts-by-name-and-remove-ipv6'"
  (unless (fboundp 'original-get-host-by-name)
    (setf (fdefinition 'original-get-host-by-name) ; saves the original one.
	  (fdefinition 'usocket:get-hosts-by-name))
    (setf (fdefinition 'usocket:get-hosts-by-name) ; overwrite with mine.
	  (fdefinition 'get-hosts-by-name-and-remove-ipv6))))
