# CL-CRYPTOMINISAT — A Common Lisp wrapper for CryptoMiniSat

This is a Common Lisp wrapper for the library provided by
[CryptoMiniSat](https://github.com/msoos/cryptominisat/), an advanced SAT
solver.

It has been tested on Linux with SBCL against CryptoMiniSat v5.0.1.
Theoretically, it should work with other Common Lisp implementations as well as
operating systems. No such tests were performed, though.

## Prerequisities

The library `libcryptominisat5` should be installed on your system.

## Example usage

CL-CryptoMiniSat is just a thin layer above the C interface of CryptoMiniSat.
Usage is therefore very similar to what is already documented [here](https://github.com/msoos/cryptominisat#library-usage).

Example:

```
(asdf:load-system :cl-cryptominisat)

(let ((solver (cl-cryptominisat::create-solver)))
  ; Let's use 4 threads.
  (cl-cryptominisat::set-num-threads solver 4)

  ; We need 3 variables.
  (cl-cryptominisat::add-new-vars solver 3)

  ; Adds "-2 0".
  (cl-cryptominisat::add-clause solver '((1 T)))

  ; Adds "-1 2 3 0".
  (cl-cryptominisat::add-clause solver '((0 T) (1) (2)))

  (when (cl-cryptominisat::solve solver)
    (format T "Solution is: ~A~%" (cl-cryptominisat::get-model solver))))
```

For the full list of provided functions as well as their documentation, please
see `cryptominisat.lisp`.

## Tests

To execute the provided tests, run `(asdf:test-system :cl-cryptominisat)`.
