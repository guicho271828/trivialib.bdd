(in-package :trivialib.bdd)

(defun bdd-node (variable true false)
  "Node generation & pruning rule for BDD. Use it as NODE-GENERATOR argument to ODD-APPLY"
  (if (eq true false)
      true
      (ensure-gethash (vector variable true false)
                      *node-cache*
                      (make-node :variable variable :true true :false false))))


(defun bdd-apply (f g op-leaf)
  (labels ((rec (f g)
             (match* (f g)
               (((leaf :content c1) (leaf :content c2))
                (leaf (funcall op-leaf c1 c2)))
               (((leaf) (node variable true false))
                (bdd-node
                         variable
                         (rec f true)
                         (rec f false)))
               (((node variable true false) (leaf))
                (bdd-node
                         variable
                         (rec g true)
                         (rec g false)))
               (((node f-variable f-hi f-lo)
                 (node g-variable g-hi g-lo))
                (typecase (- f-variable g-variable)
                  ((integer * -1)
                   (bdd-node
                            f-variable
                            (rec f-hi g)
                            (rec f-lo g)))
                  ((integer 1 *)
                   (bdd-node
                            g-variable
                            (rec f g-hi)
                            (rec f g-lo)))
                  ((integer 0 0)
                   (bdd-node
                            f-variable
                            (rec f-hi g-hi)
                            (rec f-lo g-lo))))))))
    (rec f g)))


