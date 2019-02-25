(asdf:defsystem :one-big-fasl-example
  :description "An example for building one fasl from all srcs."
  :license "WTFPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:jsown ; This is not requied because our bootstrap shipped with this.
	       #:jp-numeral)
  :components
  ((:file "main")))
