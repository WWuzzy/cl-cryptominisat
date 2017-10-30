(load "./cryptominisat.lisp")
(in-package :cl-cryptominisat-test)

(plan nil)

(defmacro with-solver (var &body body)
  `(let ((,var (cl-cryptominisat::create-solver)))
     ,@body
     (cl-cryptominisat::destroy-solver ,var)))

(diag "=== encode-literal ===")
(is (cl-cryptominisat::encode-literal '(4)) 8 "should properly encode positive literals")
(is (cl-cryptominisat::encode-literal '(4 T)) 9 "should properly encode negative literals")

(diag "=== get-vars-cnt ===")
(with-solver solver
  (cl-cryptominisat::add-new-vars solver 22)
  (is (cl-cryptominisat::get-vars-cnt solver) 22 "should return the declared number of variables")
  (cl-cryptominisat::add-new-vars solver 11)
  (is (cl-cryptominisat::get-vars-cnt solver) 33 "should return the total after adding more variables"))

(diag "=== solve ===")
(with-solver solver
  (cl-cryptominisat::add-new-vars solver 2)
  (cl-cryptominisat::add-clause solver '((0) (1)))
  (cl-cryptominisat::add-clause solver '((0 T) (1)))
  (cl-cryptominisat::set-num-threads solver 1)
  (is (cl-cryptominisat::solve solver) T "should return T when a solution is found")
  (cl-cryptominisat::add-clause solver '((0 T) (1 T)))
  (cl-cryptominisat::add-clause solver '((0) (1 T)))
  (is (cl-cryptominisat::solve solver) NIL "should return NIL when a solution cannot be found"))

(diag "=== solve-with-assumptions ===")
(with-solver solver
  (cl-cryptominisat::add-new-vars solver 2)
  (cl-cryptominisat::add-clause solver '((0) (1)))
  (cl-cryptominisat::add-clause solver '((0 T) (1)))
  (is (cl-cryptominisat::solve-with-assumptions solver '((1 T))) NIL "should return NIL when a solution cannot be found")
  (is (cl-cryptominisat::solve-with-assumptions solver '((0 T))) T "should return T when a solution is found"))

(diag "=== get-model ===")
(with-solver solver
  (cl-cryptominisat::add-new-vars solver 2)
  (cl-cryptominisat::add-clause solver '((0 T)))
  (cl-cryptominisat::add-clause solver '((0) (1)))
  (cl-cryptominisat::solve solver)
  (is (cl-cryptominisat::get-model solver) '(NIL T) "should return the assignment of truth values satisfying the SAT problem"))

(diag "=== get-conflict ===")
(with-solver solver
  (cl-cryptominisat::add-new-vars solver 2)
  (cl-cryptominisat::add-clause solver '((0 T)))
  (cl-cryptominisat::add-clause solver '((0) (1 T)))
  (cl-cryptominisat::solve-with-assumptions solver '((1)))
  (is (cl-cryptominisat::get-conflict solver) '((1 T)) "should return the found conflict"))

(diag "=== add-xor-clause ===")
(with-solver solver
  (cl-cryptominisat::add-new-vars solver 2)
  (cl-cryptominisat::add-xor-clause solver '(0 1) T)
  (cl-cryptominisat::add-clause solver '((1 T)))
  (cl-cryptominisat::solve solver)
  (is (cl-cryptominisat::get-model solver) '(T NIL) "should add a XOR clause"))

(finalize)
