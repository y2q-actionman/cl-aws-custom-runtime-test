(asdf:defsystem :needed-libs
  :description "an example for building fasl."
  :depends-on (;; #:cl-json ; this is not requied because out bootstrap shipped with this.
	       #:jp-numeral))
