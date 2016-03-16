(in-package :trivialib.bdd)

;;;; shared among bdd, zdd

;; (declaim (inline interpret))
;; (ftype interpret (op) boolean (dd?) (dd?))

(defun interpret (op bool thing)
  "interpret is non-consing."
  (case op
    (and
     (and bool thing))
    (or
     (or bool thing))
    (xor
     (xor bool thing))))
