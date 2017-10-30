(in-package :cl-cryptominisat)

(define-foreign-library libcryptominisat
  (:darwin "libcryptominisat.dylib")
  (:unix "libcryptominisat5.so")
  (t (:default "libcryptominisat5")))

(use-foreign-library libcryptominisat)

; TODO:
;   - input validation (parameters as well as uninitialized solver)

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

(defcfun "cmsat_add_xor_clause"
  :bool
  (self (:pointer (:struct SATSolver))) (vars (:pointer :unsigned-int)) (n :unsigned-int) (rhs :bool))

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

(defun decode-boolean (c-boolean)
  "Decode cyrptominisats L_True and L_False to T and NIL, respectively."
  (case c-boolean (0 T)
                  (1 NIL)
                  (otherwise (error 'error (format nil "Unexpected boolean value: ~A " c-boolean)))))

(defun encode-literal (literal)
  "Encode literal to a single integer as expected by cryptominisat."
  (+ (* 2 (car literal)) (if (cadr literal) 1 0)))

(defun decode-literal (c-literal-int)
  "Decode literal from cryptominisats internal representation."
  (multiple-value-bind (q r) (floor c-literal-int 2)
    (if (eq r 0) `(,q) `(,q T))))

(defun convert-to-c-literals (literals)
  "Create a clause from a list of the given literals.

   A literal is itself a list of the form (number, is_inverted).
   The number starts with 0. The second parameter is optional
   and defaults to nil."

  (let ((i 0) (clause (foreign-alloc 'c-lit :count (length literals))))
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
  (let* ((c-literals (convert-to-c-literals clause))
         (result (cmsat-add-clause solver c-literals (length clause))))
    (foreign-free c-literals)
    result))

(defun add-xor-clause (solver vars rhs)
  "Add a XOR clause to the solver.

   The XOR clause is specified by

     - a list of vars in the XOR clause (e.g. '(0 1 2 3))
     - the right hand symbol (T / NIL)
  "
  (let* ((i 0)
         (num-vars (length vars))
         (clause (foreign-alloc :unsigned-int :count num-vars))
         (result nil))
    (dolist (var vars)
      (setf (mem-aref clause :unsigned-int i) var)
      (setf i (1+ i)))
    (setf result (cmsat-add-xor-clause solver clause num-vars rhs))
    (foreign-free clause)
    result))

(defun solve (solver)
  "Call the solution routine."
  (decode-boolean (getf (cmsat-solve solver) 'x)))

(defun solve-with-assumptions (solver assumptions)
  "Call the solution routine, providing the assumptions.

   Assumptions are represented by a list of literals.

   Literals are represented by lists of the form '(<literal_id>, <is_inverted>), where:

   <literal_id> is a non-negative integer
   <is_inverted> is an optional argument; if True, the literal is considered negative,
        otherwise it is positive.
  "
  (let* ((c-assumptions (convert-to-c-literals assumptions))
         (solution (cmsat-solve-with-assumptions solver c-assumptions (length assumptions))))
    (decode-boolean (getf solution 'x))))

(defun get-model (solver)
  "Get model satysfying the problem.

   Solution is a list of boolean values (T or NIL) corresponding to truth
   assignments to the individual literals.
  "
  (let* ((model (cmsat-get-model solver))
         (num-vals (getf model 'num-vals)))
    (mapcar #'decode-boolean
      (loop for i from 0 to (1- num-vals)
            collect (foreign-slot-value (mem-aref (getf model 'vals) 'c-lbool i) 'c-lbool 'x)))))

(defun get-conflict (solver)
  "Return conflicts with respect to the provided assumptions."
  (let* ((conflict (cmsat-get-conflict solver))
         (num-vals (getf conflict 'num-vals))
         (vals (getf conflict 'vals)))
    (mapcar #'decode-literal
      (loop for i from 0 to (1- num-vals)
            collect (foreign-slot-value (mem-aref vals 'c-lit i) 'c-lit 'x)))))
