(defsystem "cl-cryptominisat-test"
  :depends-on ("cl-cryptominisat" "prove")
  :defsystem-depends-on (:prove-asdf)
  :serial t
  :components ((:file "package-test")
               (:test-file "tests"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
