
(in-package :trivialib.bdd)



(defun zdd-node (variable true false)
  "Node generation & pruning rule for ZDD. Use it as NODE-GENERATOR argument to ODD-APPLY"
  (if (eq true (leaf nil))
      false
      (ensure-gethash (vector variable true false)
                      *node-cache*
                      (make-node :variable variable :true true :false false))))

(defun zdd-apply (f g op-leaf)
  (labels ((rec (f g)
             (match* (f g)
               (((leaf :content nil) _) (funcall op-leaf g f))
               ((_ (leaf :content nil)) (funcall op-leaf g f))
               ((_ (eq f))              (funcall op-leaf g f))
               (((node f-variable f-hi f-lo)
                 (node g-variable g-hi g-lo))
                (cond ;; typecase (- f-variable g-variable)
                  ((< f-variable g-variable) ;;(integer * -1)
                   (zdd-node f-variable (rec f-hi (leaf nil)) (rec f-lo g)))
                  ((> f-variable g-variable) ;; (integer 1 *)
                   (zdd-node g-variable (rec g-hi (leaf nil)) (rec g-lo f)))
                  ((= f-variable g-variable)
                   (zdd-node f-variable (rec f-lo g-lo) (rec f-hi g-hi))))))))
    (rec f g)))

(defun zdd-union (f g)
  (match* (f g)
    (((leaf :content nil) _) g)
    ((_ (leaf :content nil)) f)
    ((_ (eq f))              f)))

(defun zdd-intersection (f g)
  (match* (f g)
    (((leaf :content nil) _) f)
    ((_ (leaf :content nil)) g)
    ((_ (eq f))              f)))

(defun zdd-diff (f g)
  (match* (f g)
    (((leaf :content nil) _) f)
    ((_ (leaf :content nil)) f)
    ((_ (eq f))              (leaf nil))))



;; (defun zdd (root &optional (variables *variables*) (node-cache *node-cache*))
;;   (odd root variables node-cache #'zdd-node))






