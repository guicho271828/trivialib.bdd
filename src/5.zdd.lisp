
;;;; Zero-Suppressed BDDs for Set Manipulation in Combinatorial Problems. Minato, S
(in-package :optima-bdd)

;;;; proclamations

(declaim (inline zdd-apply zdd*))

;; almost same as BDD-apply
(ftype zdd-apply (op) (dd?) (dd?) (dd?))
(defun zdd-apply (op f g)
  (declare (notinline zdd-apply))
  (multiple-value-match (values f g)
    (((boolean) (boolean)) (interpret op f g))
    ;; if we ever reach a condition where one of the arguments is a
    ;; terminal vertex representing the “dominant” value for operation
    ;; (op ) (e.g., 1 for OR and O for AND), then we can stop the recursion
    ;; and return an appropriately labeled terminal vertex.
    (((boolean) (dd))
     (interpret op f g))
    (((dd) (boolean))
     (interpret op g f))
    ;; Otherwise, let variable x be the splitting variable, defined as
    ;; the minimum of variables var( rg ) and var(rg).  
    (((dd f-tag f-hi f-lo)
      (dd g-tag g-hi g-lo))
     (cond
       ((< f-tag g-tag)
        (zdd* f-tag
             (zdd-apply op f-hi g)
             (zdd-apply op f-lo g)))
       ((> f-tag g-tag)
        (zdd* g-tag
             (zdd-apply op f g-hi)
             (zdd-apply op f g-lo)))
       (t
        (zdd* f-tag
              (zdd-apply op f-hi g-hi)
              (zdd-apply op f-lo g-lo)))))))
(declaim (notinline zdd-apply))

;; FIXME: according to Bryant et at, memoizing the results of zdd-apply is
;; effective. Therefore using function-cache:defcached is appropriate.


;;;; duplicated node detection, reduction rules

(ftype zdd* fixnum (dd?) (dd?) (dd?))
(defun zdd* (tag hi lo)
  (if (null hi)
      lo
      (let ((dd (dd tag hi lo)))
        (or (gethash dd *db*)
            (setf (gethash dd *db*) dd)))))

;;;; RESTRICT implementation from Bryant et.al.

(ftype zdd-restrict (dd?) fixnum boolean (dd?))
(defun zdd-restrict (zdd tag value)
  (match zdd
    ((dd (guard y (< y tag)) hi lo)
     (zdd* y
           (zdd-restrict hi tag value)
           (zdd-restrict lo tag value)))
    ((dd (= tag) hi lo)
     (if value hi lo))))

;;;; XOR, NOT

(ftype zdd-not (dd?) (dd?))
(defun zdd-not (f)
  (match f
    ((dd tag hi lo) (zdd* tag (zdd-not hi) (zdd-not lo)))
    ((boolean)      (not f))))

(ftype (zdd-xor zdd-and zdd-or) (dd?) (dd?) (dd?))
 (defun zdd-xor (f g)
  (declare (inline zdd-apply))
  (zdd-apply 'xor f g))
(defun zdd-or  (f g)
  (declare (inline zdd-apply))
  (zdd-apply 'or  f g))
(defun zdd-and (f g)
  (declare (inline zdd-apply))
  (zdd-apply 'and f g))

;;;; construct a ZDD

(ftype zdd/bitvec simple-bit-vector (dd?))
(defun zdd/bitvec (bv)
  "See bdd/bitvec ."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((len (length bv)))
    (labels ((rec (i)
               (if (= i len)
                   t
                   (case (aref bv i)
                     (1
                      (zdd* i (rec (1+ i)) nil))
                     (0
                      (zdd* i nil (rec (1+ i))))))))
      (rec 0))))

(ftype zdd-restrict/bitvec (dd?) simple-bit-vector (dd?))
(defun zdd-restrict/bitvec (zdd bv)
  "See bdd-restrict/bitvec ."
  (let ((len (length bv)))
    (labels ((rec (i zdd)
               (if (= i len)
                   zdd
                   (match zdd
                     ((boolean) zdd)
                     ((dd (= i) hi lo)
                      (rec (1+ i) (if (plusp (aref bv i)) hi lo)))
                     ((dd (> i) _ _)
                      (rec (1+ i) zdd))
                     ((dd (< i) _ _)
                      (rec (1+ i) zdd))))))
      (rec 0 zdd))))
