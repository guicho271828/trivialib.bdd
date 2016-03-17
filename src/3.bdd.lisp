(in-package :trivialib.bdd)

(defun bdd-node (variable hi lo)
  "Node generation & pruning rule for BDD. Use it as NODE-GENERATOR argument to ODD-APPLY"
  (if (eq hi lo)
      hi
      (ensure-gethash (vector variable hi lo)
                      *node-cache*
                      (make-node :variable variable :hi hi :lo lo))))

(defun bdd-apply (f g op-leaf)
  (labels ((rec (f g)
             (match* (f g)
               (((leaf :content c1) (leaf :content c2))
                (leaf (funcall op-leaf c1 c2)))
               (((leaf) (node variable hi lo))
                (bdd-node variable
                          (rec f hi)
                          (rec f lo)))
               (((node variable hi lo) (leaf))
                (bdd-node variable
                          (rec g hi)
                          (rec g lo)))
               (((node f-variable f-hi f-lo)
                 (node g-variable g-hi g-lo))
                (typecase (the fixnum (- f-variable g-variable))
                  ((integer * -1)       ; (< f-variable g-variable)
                   (bdd-node f-variable
                             (rec f-hi g)
                             (rec f-lo g)))
                  ((integer 1 *)        ; (> f-variable g-variable)
                   (bdd-node g-variable
                             (rec f g-hi)
                             (rec f g-lo)))
                  ((integer 0 0)        ; (= f-variable g-variable)
                   (bdd-node f-variable
                             (rec f-hi g-hi)
                             (rec f-lo g-lo))))))))
    (rec f g)))

(defun unit (variable)
  ;; we already know the hi/lo branch aren't the same and the HI node
  ;; is not (leaf nil)
  (let ((hi (leaf t))
        (lo (leaf nil)))
    (ensure-gethash (vector variable hi lo)
                    *node-cache*
                    (make-node :variable variable :hi hi :lo lo))))

(defun !unit (variable)
  ;; we already know the hi/lo branch aren't the same and the HI node
  ;; is not (leaf nil)
  (let ((hi (leaf t))
        (lo (leaf nil)))
    (ensure-gethash (vector variable lo hi)
                    *node-cache*
                    (make-node :variable variable :hi lo :lo hi))))

(defun bdd (root &optional (variables *variables*) (node-cache *node-cache*) (operation *operation*))
  (odd root variables node-cache operation))
