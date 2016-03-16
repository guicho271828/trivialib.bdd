
(in-package :trivialib.bdd)

(immutable-struct:defstruct odd
  "structure representing a whole Decision Diagram"
  (root (leaf nil) :type (or node leaf))
  (variables *variables* :type sequence)
  (node-cache *node-cache* :type hash-table))

;; (defmethod print-object ((odd odd) s)
;;   (print-unreadable-object (odd s :type t :identity t)
;;     ;; (princ :root s)
;;     (princ (odd-root odd) s)))

(defmacro with-odd-context ((&key variables &allow-other-keys) &body body)
  `(let ((*node-cache* (tg:make-weak-hash-table :weakness :value :test #'equalp))
         (*variables* ,variables))
     ,@body))

(defun-ematch* odd-compatible-p (odd1 odd2)
  (((odd :variables vars1 :node-cache node-cache1) (odd :variables vars2 :node-cache node-cache2))
   (and
    ;; (eq (type-of odd1) (type-of odd2))
    (eq node-cache1 node-cache2)
    (eq vars1 vars2))))

(defun-ematch* odd-apply (odd1 odd2 node-generator op-leaf)
  (((odd root variables node-cache) (odd :root root2))
   (assert (odd-compatible-p odd1 odd2) nil "ODD ~a and ~a are incompatible!" odd1 odd2)
   (odd (node-apply root root2 node-generator op-leaf) variables node-cache)))

(defun node-apply (f g node-generator op-leaf)
  (labels ((rec (f g)
             (match* (f g)
               (((leaf :content c1) (leaf :content c2))
                (leaf (funcall op-leaf c1 c2)))
               (((leaf) (node variable true false))
                (funcall node-generator
                         variable
                         (rec f true)
                         (rec f false)))
               (((node variable true false) (leaf))
                (funcall node-generator
                         variable
                         (rec g true)
                         (rec g false)))
               (((node f-variable f-hi f-lo)
                 (node g-variable g-hi g-lo))
                (typecase (- f-variable g-variable)
                  ((integer * -1)
                   (funcall node-generator
                            f-variable
                            (rec f-hi g)
                            (rec f-lo g)))
                  ((integer 1 *)
                   (funcall node-generator
                            g-variable
                            (rec f g-hi)
                            (rec f g-lo)))
                  ((integer 0 0)
                   (funcall node-generator
                            f-variable
                            (rec f-hi g-hi)
                            (rec f-lo g-lo))))))))
    (rec f g)))

(defun bdd (variable true false)
  "Node generation & pruning rule for BDD. Use it as NODE-GENERATOR argument to ODD-APPLY"
  (if (eq true false)
      true
      (ensure-gethash (vector variable true false)
                      *node-cache*
                      (make-node :variable variable :true true :false false))))

(defun zdd (variable true false)
  "Node generation & pruning rule for ZDD. Use it as NODE-GENERATOR argument to ODD-APPLY"
  (if (eq true (leaf nil))
      false
      (ensure-gethash (vector variable true false)
                      *node-cache*
                      (make-node :variable variable :true true :false false))))
