(defsystem "cl-cryptominisat"
  :description "Common Lisp Cryptominisat library bindings."
  :version "0.1.0"
  :author "Hynek Urban <hynek.urban@gmail.com>"
  :licence "MIT"
  :depends-on ("cffi" "cffi-libffi")
  :serial t
  :components ((:file "package")
               (:file "cryptominisat"))
  :in-order-to ((test-op (test-op cl-cryptominisat-test))))
