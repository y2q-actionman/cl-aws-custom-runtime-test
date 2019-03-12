(asdf:defsystem :needed-libs-example
  :description "An example of building a fasl from libraries."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (;; #:cl-json ; this is not requied because our bootstrap shipped with this.
	       #:jp-numeral))
