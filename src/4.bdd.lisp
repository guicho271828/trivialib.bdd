
;;;; Symbolic Boolean Manipulation with Ordered Binary-Decision Kliagrams RANDALE, BRYANT, 1986
(in-package :trivialib.bdd)

;;;; APPLY implementation from Bryant et.al.

;; Suppose functions f and g are represented by OBDDS with root vertices rf
;; and rg, respectively.
(ftype bdd-apply (op) (dd?) (dd?) (dd?))
(defun bdd-apply (op f g)
  (match* (f g)
    ;;   For the case where both rf and rg are terminal
    ;; vertices, the recursion terminates by returning an appropriately labeled
    ;; terminal vertex.
    ;;  In our example, this occurs for the evaluations A4, B3
    ;; and A5, B4.
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
        (bdd* f-tag
             (bdd-apply op f-hi g)
             (bdd-apply op f-lo g)))
       ((> f-tag g-tag)
        (bdd* g-tag
             (bdd-apply op f g-hi)
             (bdd-apply op f g-lo)))
       (t
        (bdd* f-tag
              (bdd-apply op f-hi g-hi)
              (bdd-apply op f-lo g-lo)))))))

;; FIXME: according to Bryant et at, memoizing the results of bdd-apply is
;; effective. Therefore using function-cache:defcached is appropriate.


;;;; duplicated node detection, reduction rules

(ftype bdd* fixnum (dd?) (dd?) (dd?))
(defun bdd* (tag hi lo)
  (if (eq hi lo)
      hi
      (let ((dd (dd tag hi lo)))
        (or (gethash dd *db*)
            (setf (gethash dd *db*) dd)))))

;;;; RESTRICT implementation from Bryant et.al.

(ftype bdd-restrict (dd?) fixnum boolean (dd?))
(defun bdd-restrict (bdd tag value)
  (match bdd
    ((dd (guard y (< y tag)) hi lo)
     (bdd* y
           (bdd-restrict hi tag value)
           (bdd-restrict lo tag value)))
    ((dd (= tag) hi lo)
     (if value hi lo))))

