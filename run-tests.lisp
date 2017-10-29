(load "./system.lisp")
(ql:quickload :prove)
(defpackage cl-cryptominisat-test
  (:use :cl
        :prove
        :cl-cryptominisat))
(prove:run "test.lisp")
