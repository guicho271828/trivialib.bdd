(in-package :optima-bdd)

;;;; hash-table
;;;; these codes are shared among bdd, zdd

;; (declaim (inline dd-hash (setf dd-hash)))

#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; sxhash in sbcl always returns the same value for
  ;; the structures of the same class. To alleviate this
  ;; problem, we may be able to compare the raw address,
  ;; but this is dangerous because GC may move the
  ;; object. Therefore, I added a slot called `id',
  ;; which is created by a good random number generator.
  (declaim (inline dd-sxhash dd-sxhash-2))
  (ftype dd-sxhash (dd?) fixnum)
  (defun dd-sxhash (x)
    (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
    (declare (inline sxhash symbolp))
    (if (symbolp x)
        (sxhash (the symbol x))
        (dd-id x)))

  (ftype dd-sxhash-2 (dd?) (dd?) fixnum)
  (defun dd-sxhash-2 (hi lo)
    (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
    (declare (inline sb-int::mix))
    (sb-int::mix (dd-sxhash hi)
                 (dd-sxhash lo)))

  (ftype dd-sxhash-struct dd fixnum)
  (defun dd-sxhash-struct (dd)
    (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
    (dd-sxhash-2 (dd-true  dd)
                 (dd-false dd)))

  (ftype dd-sxhash-cons (cons (dd?) (dd?)) fixnum)
  (defun dd-sxhash-cons (cons)
    (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
    (dd-sxhash-2 (car cons)
                 (cdr cons)))

  (ftype dd-equal dd dd boolean)
  (defun dd-equal (dd1 dd2)
    (and (= (dd-tag  dd1)
            (dd-tag  dd2))
         (eq (dd-true  dd1)
             (dd-true  dd2))
         (eq (dd-false dd1)
             (dd-false dd2)))))

#+nil SB-EXT:DEFINE-HASH-TABLE-TEST
#+nil (make-hash-table)
(declaim (type hash-table *db*))
(defvar *db* (tg:make-weak-hash-table
              :weakness :value
              :test #'equalp)
  "db to look up in order to avoid the creation of redundunt
nodes. This actually need not be a hash-table.
Declaring this hash table as `weak' seems good.
")

;; (ftype dd-hash (dd?) (dd?) (dd?))
;; (ftype (setf dd-hash) (dd?) (dd?) (dd?) (dd?))
;; (defun dd-hash (hi lo)
;;   (gethash (cons hi lo) *db*))
;; (defun (setf dd-hash) (new-value hi lo)
;;   (setf (gethash (cons hi lo) *db*) new-value))


