(defsystem "cl-yassg-tests"
  :description "Test suite for the cl-yassg system"
  :author "oaguy1 <oaguy1@gmail.com>"
  :version "0.0.1"
  :depends-on (:cl-yassg :lisp-unit)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-cl-yassg")))))
