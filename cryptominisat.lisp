(in-package :cl-cryptominisat)

(define-foreign-library libcryptominisat
  (:unix "libcryptominisat5.so")
  (t (:default "libcryptominisat5.so")))
(load-foreign-library 'libcryptominisat)

; TODO: add_xor_clause?

; Describe the C interface.

(defcstruct SATSolver)
(defcstruct c-lit- (x :uint32))
(defcstruct c-lbool- (x :uint8))
(defctype c-lit (:struct c-lit-))
(defctype c-lbool (:struct c-lbool-))

(defcstruct slice-lbool- (vals (:pointer c-lbool)) (num-vals :uint))
(defctype slice-lbool (:struct slice-lbool-))

(defcstruct slice-lit- (vals (:pointer c-lit)) (num-vals :uint))
(defctype slice-lit (:struct slice-lit-))

(defcfun "cmsat_new" (:pointer (:struct SATSolver)))

(defcfun "cmsat_free" :void (solver (:pointer (:struct SATSolver))))

(defcfun "cmsat_set_num_threads"
  :void
  (self (:pointer (:struct SATSolver))) (n :unsigned-int))

(defcfun "cmsat_nvars" :unsigned-int (self (:pointer (:struct SATSolver))))

(defcfun "cmsat_new_vars"
  :void
  (self (:pointer (:struct SATSolver))) (n :unsigned-int))

(defcfun "cmsat_add_clause"
  :bool
  (self (:pointer (:struct SATSolver))) (lits (:pointer c-lit)) (n :unsigned-int))

(defcfun "cmsat_solve"
  c-lbool
  (self (:pointer (:struct SATSolver))))

(defcfun "cmsat_solve_with_assumptions"
  c-lbool
  (self (:pointer (:struct SATSolver))) (assumptions (:pointer c-lit)) (num-assumptions :unsigned-int))

(defcfun "cmsat_get_model"
  slice-lbool
  (self (:pointer (:struct SATSolver))))

(defcfun "cmsat_get_conflict"
  slice-lit
  (self (:pointer (:struct SATSolver))))



; Helper methods.
(defun encode-literal (literal)
  "Encode literal to a single integer as expected by cryptominisat."
  (+ (* 2 (car literal)) (if (cadr literal) 1 0)))

(defun create-c-clause (literals)
  "Create a clause from a list of the given literals.

   A literal is itself a list of the form (number, is_inverted).
   The number starts with 0. The second parameter is optional
   and defaults to nil."

  (let ((i 0) (clause (foreign-alloc 'c-lit :count (length literals)))) ; TODO: free
    (dolist (literal literals)
      (setf (foreign-slot-value (mem-aref clause 'c-lit i) 'c-lit 'x) (encode-literal literal))
      (setf i (1+ i)))
  clause))

; Define the Lisp interface.

(defun create-solver ()
  "Create a new instance of SAT solver."
  (cmsat-new))

(defun destroy-solver (solver)
  "Destroy solver instance, freeing up the memory."
  (cmsat-free solver))

(defun get-vars-cnt (solver)
  "Return the total number of declared SAT variables."
  (cmsat-nvars solver))

(defun add-new-vars (solver cnt)
  "Add <cnt> new vars to the solver"
  (cmsat-new-vars solver cnt))

(defun set-num-threads (solver cnt)
  "Set the number of threads."
  (cmsat-set-num-threads solver cnt))

(defun add-clause (solver clause)
  "Add a clause to the solver.

   The clause should be a list of literals.

   Literals are represented by lists of the form '(<literal_id>, <is_inverted>), where:

   <literal_id> is a non-negative integer
   <is_inverted> is an optional argument; if True, the literal is considered negative,
        otherwise it is positive.
  "
  (cmsat-add-clause solver (create-c-clause clause) (length clause)))

(defun solve (solver)
  "Call the solution routine."
  (getf (cmsat-solve solver) 'x))

(defun solve-with-assumptions (solver assumptions)
  "Call the solution routine, providing the assumptions.

   Assumptions are represented by a list of literals.

   Literals are represented by lists of the form '(<literal_id>, <is_inverted>), where:

   <literal_id> is a non-negative integer
   <is_inverted> is an optional argument; if True, the literal is considered negative,
        otherwise it is positive.
  "
  ; TODO: rename "create-c-clause" to something else
  (getf (cmsat-solve-with-assumptions solver (create-c-clause assumptions) (length clause)) 'x))

(defun get-model (solver)
  "Get model satysfying the problem.

   Solution is a list of boolean values (T or NIL) corresponding to the
   individual literals.
  "
  ; TODO: decode l_True/l_False
  (let* ((model (cmsat-get-model solver))
         (num-vals (getf model 'num-vals)))
    (loop for i from 0 to (1- num-vals)
          collect (foreign-slot-value (mem-aref (getf model 'vals) 'c-lbool i) 'c-lbool 'x))))

(defun get-conflict (solver)
  "Return the found conflict in the SAT problem."
  ; TODO: decode literals.
  (let* ((conflict (cmsat-get-conflict solver))
         (num-vals (getf conflict 'num-vals)))
    (loop for i from 0 to (1- num-vals)
          collect (foreign-slot-value (mem-aref (getf model 'vals) 'c-lit i) 'c-lit 'x))))


; Use the interface
(defvar solver)
(setq solver (create-solver))
(set-num-threads solver 3)
(add-new-vars solver 3)


(let ((clause1 '((0) (1)))
      (clause2 '((0 T) (1)))
      (clause3 '((0) (1 T))))
  (add-clause solver clause1)
  (add-clause solver clause2)
  (add-clause solver clause3))


(format T "Solved: ~A~%" (solve solver))
(format T "Model: ~{~A~^, ~}~%" (get-model solver))


;(close-foreign-library 'libcryptominisat)
