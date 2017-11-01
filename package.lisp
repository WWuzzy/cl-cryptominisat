(defpackage :cl-cryptominisat
  (:use :cffi :cl)
  (:export
    add-clause
    add-new-vars
    add-xor-clause
    create-solver
    destroy-solver
    get-conflict
    get-model
    get-vars-cnt
    set-num-threads
    solve
    solve-with-assumptions))
