
;;;; Symbolic Boolean Manipulation with Ordered Binary-Decision Kliagrams RANDALE, BRYANT, 1986
(in-package :trivialib.bdd)

; Symbolic Boolean Manipulation with Ordered Binary-Decision Kliagrams
; RANDALE, BRYANT, 1986

;;;; proclamations

(declaim (inline bdd-apply bdd*))

;;;; APPLY implementation from Bryant et.al.

;; Suppose functions f and g are represented by OBDDS with root vertices rf
;; and rg, respectively.
(ftype bdd-apply (op) (dd?) (dd?) (dd?))
(defun bdd-apply (op f g)
  (declare (notinline bdd-apply))
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
(declaim (notinline bdd-apply))

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

;;;; XOR, NOT

(ftype bdd-not (dd?) (dd?))
(defun bdd-not (f)
  (match f
    ((dd tag hi lo) (bdd* tag (bdd-not hi) (bdd-not lo)))
    ((boolean)      (not f))))

(ftype (bdd-xor bdd-and bdd-or) (dd?) (dd?) (dd?))
 (defun bdd-xor (f g)
  (declare (inline bdd-apply))
  (bdd-apply 'xor f g))
(defun bdd-or  (f g)
  (declare (inline bdd-apply))
  (bdd-apply 'or  f g))
(defun bdd-and (f g)
  (declare (inline bdd-apply))
  (bdd-apply 'and f g))

;;;; construct a BDD

(ftype bdd/bitvec simple-bit-vector (dd?))
(defun bdd/bitvec (bv)
  "Returns a bdd that returns true if all inputs matches the 0/1 elements of bit-vector.
The ordering of OBDD follows the index of the vector.

Usecase: given 3 bit vectors, bvs = (#*011 #*101 #*010),

 (reduce (curry #'bdd-apply 'or) bvs :key #'bdd/bitvec)

will return the OBDD which returns true for any of those inputs.
To check the satisfyability of the bit vector, use bdd-restrict/bitvec .
"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((len (length bv)))
    (labels ((rec (i)
               (if (= i len)
                   t
                   (case (aref bv i)
                     (1
                      (bdd* i (rec (1+ i)) nil))
                     (0
                      (bdd* i nil (rec (1+ i))))))))
      (rec 0))))

(ftype bdd-restrict/bitvec (dd?) simple-bit-vector (dd?))
(defun bdd-restrict/bitvec (bdd bv)
  "Restrict the bdd by each value in the bit vector. If more bits remains
in the bdd, it returns a bdd. If the restriction is sufficient for reducing
the bdd into terminal node, it returns the reduced results immediately."
  (let ((len (length bv)))
    (labels ((rec (i bdd)
               (if (= i len)
                   bdd
                   (match bdd
                     ((boolean) bdd)
                     ((dd (= i) hi lo)
                      (rec (1+ i) (if (plusp (aref bv i)) hi lo)))
                     ((dd (> i) _ _)
                      (rec (1+ i) bdd))
                     ((dd (< i) _ _)
                      (rec (1+ i) bdd))))))
      (rec 0 bdd))))

;;;; extensions

;; In recent years many refinements  to the 
;; basic  OBDD  structure  have  been 
;; reported. These include using a single, 
;; multirooted  graph to represent all of the 
;; functions  required  [Brace et al. 1990;  
;; Karplus 1989; Minato et al. 1990; Reeves  
;; and Irwin  1987], adding labels to the  
;; arcs to denote Boolean negation [Brace  
;; et al. 1990; Karplus  1989; Minato et al. 
;; 1990; Madre and Billon 1988], and gen- 
;; eralizing  the concept to other  finite 
;; domains [Srinivasan  et al. 1990]. These 
;; refinements  yield significant  savings in 
;; the memory requirement—generally  the 
;; most critical  resource  in determining 
;; the complexity  of the problems that can 
;; be solved. Applications  that require gen- 
;; erating over 1 million OBD D vertices are 
;; now routinely  performed on workstation 
;; computers. 
