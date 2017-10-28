(in-package :cl-cryptominisat)

(define-foreign-library libcryptominisat
  (:unix "libcryptominisat5.so")
  (t (:default "libcryptominisat5.so")))
(load-foreign-library 'libcryptominisat)

; Describe the C interface.

(defcstruct SATSolver)
(defcstruct c-lit- (x :uint32))
(defcstruct c-lbool- (x :uint8))
(defctype c-lit (:struct c-lit-))
(defctype c-lbool (:struct c-lbool-))

(defcstruct slice-lbool- (vals (:pointer c-lbool)) (num-vals :uint))
(defctype slice-lbool (:struct slice-lbool-))

(defcfun "cmsat_new" (:pointer (:struct SATSolver)))

(defcfun "cmsat_set_num_threads"
  :void
  (self (:pointer (:struct SATSolver))) (n :unsigned-int))

(defcfun "cmsat_new_vars"
  :void
  (self (:pointer (:struct SATSolver))) (n :unsigned-int))

(defcfun "cmsat_add_clause"
  :bool
  (self (:pointer (:struct SATSolver))) (lits (:pointer c-lit)) (n :unsigned-int))

(defcfun "cmsat_solve"
  c-lbool
  (self (:pointer (:struct SATSolver))))

(defcfun "cmsat_get_model"
  slice-lbool
  (self (:pointer (:struct SATSolver))))


; Define the Lisp interface.

(defun encode-literal (literal)
  "Encode literal to a single integer as expected by cryptominisat."
  (+ (* 2 (car literal)) (if (cadr literal) 1 0)))

(defun create-clause (literals)
  "Create a clause from a list of the given literals.

   A literal is itself a list of the form (number, is_inverted).
   The number starts with 0. The second parameter is optional
   and defaults to nil."

  (let ((i 0) (clause (foreign-alloc 'c-lit :count (length literals)))) ; TODO: free
    (dolist (literal literals)
      (setf (foreign-slot-value (mem-aref clause 'c-lit i) 'c-lit 'x) (encode-literal literal))
      (setf i (1+ i)))
  clause))


; Use the interface
(defvar solver)
(setq solver (cmsat-new))
(cmsat-set-num-threads solver 3)
(cmsat-new-vars solver 3)


(let ((clause1 (create-clause '((0) (1))))
      (clause2 (create-clause '((0 T) (1))))
      (clause3 (create-clause '((0) (1 T)))))
  (cmsat-add-clause solver clause1 2)
  (cmsat-add-clause solver clause2 2)
  (cmsat-add-clause solver clause3 2))


(let ((solution (cmsat-solve solver)))
  (format T "Solved: ~A~%" (getf solution 'x)))

(let ((model (cmsat-get-model solver)))
  (format T "num-vals: ~A~%" (getf model 'num-vals))
  (format T "vals 0: ~A~%" (foreign-slot-value (mem-aref (getf model 'vals) 'c-lbool 0) 'c-lbool 'x))
  (format T "vals 1: ~A~%" (foreign-slot-value (mem-aref (getf model 'vals) 'c-lbool 1) 'c-lbool 'x))
  (format T "vals 2: ~A~%" (foreign-slot-value (mem-aref (getf model 'vals) 'c-lbool 2) 'c-lbool 'x)))



(close-foreign-library 'libcryptominisat)
