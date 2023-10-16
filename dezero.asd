(in-package :asdf-user)

(defsystem "dezero"
  :version "0.0.1"
  :author "elderica"
  :licence "MIT"
  :depends-on ("closer-mop" "numcl")
  :components ((:module "src"
		:components ((:file "lib")
			     (:file "main" :depends-on ("lib"))))))
