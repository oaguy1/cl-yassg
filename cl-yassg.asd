(defsystem "cl-yassg"
  :description "Yet Another Static Site Generator, this time in Common Lisp"
  :author "oaguy1 <oaguy1@gmail.com>"
  :version "0.0.1"
  :license "BSD"
  :serial t
  :depends-on (#:3bmd #:3bmd-ext-code-blocks #:str)
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "cl-yassg")))))
