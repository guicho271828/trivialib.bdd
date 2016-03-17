
(in-package :trivialib.bdd)

(defun zdd-node (variable hi lo)
  "Node generation & pruning rule for ZDD. Use it as NODE-GENERATOR argument to ODD-APPLY"
  (if (eq hi (leaf nil))
      lo
      (ensure-gethash (vector variable hi lo)
                      *node-cache*
                      (make-node :variable variable :hi hi :lo lo))))

(defun zdd-apply (f g op-leaf)
  (labels ((rec (f g)
             (match* (f g)
               (((leaf :content c1) (leaf :content c2))
                (leaf (funcall op-leaf c1 c2)))
               (((leaf) (node variable hi lo))
                (zdd-node variable
                          (rec f hi)
                          (rec f lo)))
               (((node variable hi lo) (leaf))
                (zdd-node variable
                          (rec g hi)
                          (rec g lo)))
               (((node f-variable f-hi f-lo)
                 (node g-variable g-hi g-lo))
                (typecase (the fixnum (- f-variable g-variable))
                  ((integer * -1)       ; (< f-variable g-variable)
                   (zdd-node f-variable
                             ;; f-hi contains the subsets which include f-variable.
                             ;; there are no such subsets in g, since f-variable < g-variable.
                             ;; thus it is a valid abstraction to give (leaf nil).
                             (rec f-hi (leaf nil))
                             (rec f-lo g)))
                  ((integer 1 *)        ; (> f-variable g-variable)
                   (zdd-node g-variable
                             (rec g-hi (leaf nil))
                             (rec g-lo f)))
                  ((integer 0 0)        ; (= f-variable g-variable)
                   (zdd-node f-variable
                             (rec f-hi g-hi)
                             (rec f-lo g-lo))))))))
    (rec f g)))


(ftype change fixnum (or node leaf))
(defun change (f variable)
  (match f
    ((node f-variable f-hi f-lo)
     (typecase (the fixnum (- f-variable variable))
       ((integer * -1)       ; (< f-variable variable)
        (zdd-node f-variable
                  (change f-hi variable)
                  (change f-lo variable)))
       ((integer 1 *)        ; (> f-variable variable)
        ;; currently f does not contain VARIABLE, hence (> f-variable variable).
        ;; toggle this by the operation below:
        (zdd-node variable f (leaf nil)))
       ((integer 0 0)        ; (= f-variable variable)
        ;; swap lo and hi
        (zdd-node variable f-lo f-hi))))))

(defun zdd (root &optional (variables *variables*) (node-cache *node-cache*) (operation #'zdd-apply))
  (odd root variables node-cache operation))


(defun dont-care (f variable)
  (zdd-apply f (change f variable)
             (lambda (a b) (or a b))))

(defun singleton (variable)
  (change (leaf t) variable))



