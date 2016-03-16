
(in-package :trivialib.bdd)

(immutable-struct:defstruct odd
  "structure representing a whole Decision Diagram"
  (root (leaf nil) :type (or node leaf))
  (variables nil :type sequence)
  (node-cache (or *node-cache* (tg:make-weak-hash-table :weakness :value :test #'equalp))))

(defun-ematch* odd-compatible-p (odd1 odd2)
  (((odd :variables vars1 :node-cache node-cache1) (odd :variables vars2 :node-cache node-cache2))
   (and
    (eq (type-of odd1) (type-of odd2))
    (eq node-cache1 node-cache2)
    (eq vars1 vars2))))

(defun-ematch* odd-apply (odd1 odd2 op-internal op-leaf)
  (((odd root variables node-cache) (odd :root root2))
   (assert (odd-compatible-p odd1 odd2) nil "ODD ~a and ~a are incompatible!" odd1 odd2)
   (odd (node-apply root root2 op-internal op-leaf) variables node-cache)))


(defun node-apply (f g op-internal op-leaf)
  (match* (f g)
    (((leaf :content c1) (leaf :content c2))
     (funcall op-leaf c1 c2))
    (((leaf) (node variable true false))
     (node variable
           (node-apply f true op-internal op-leaf)
           (node-apply f false op-internal op-leaf)))
    (((node variable true false) (leaf))
     (node variable
           (node-apply g true op-internal op-leaf)
           (node-apply g false op-internal op-leaf)))
    (((node f-variable f-hi f-lo)
      (node g-variable g-hi g-lo))
     (typecase (- f-variable g-variable)
       ((integer * -1)
        (node f-variable
              (node-apply f-hi g op-internal op-leaf)
              (node-apply f-lo g op-internal op-leaf)))
       ((integer 1 *)
        (node g-variable
              (node-apply f g-hi op-internal op-leaf)
              (node-apply f g-lo op-internal op-leaf)))
       ((integer 0 0)
        (node f-variable
              (node-apply f-hi g-hi op-internal op-leaf)
              (node-apply f-lo g-lo op-internal op-leaf)))))))



