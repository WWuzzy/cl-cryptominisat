(ql:quickload :cffi)
(ql:quickload :cffi-libffi)
(defpackage :cl-cryptominisat
  (:use :cffi :cl)
  (:export
    create-clause))
