(defpackage :cl-yassg-tests
  (:use :common-lisp
        :lisp-unit
        :cl-yassg)
  (:export #:run))


(in-package :cl-yassg-tests)
(defun run ()
  "Run all unit tests for cl-yassg"
  (lisp-unit:run-tests :all :cl-yassg-tests))
