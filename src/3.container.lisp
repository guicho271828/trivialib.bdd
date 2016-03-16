
(in-package :trivialib.bdd)

(defun bdd-node (variable true false)
  "Node generation & pruning rule for BDD. Use it as NODE-GENERATOR argument to ODD-APPLY"
  (if (eq true false)
      true
      (ensure-gethash (vector variable true false)
                      *node-cache*
                      (make-node :variable variable :true true :false false))))

(defun zdd-node (variable true false)
  "Node generation & pruning rule for ZDD. Use it as NODE-GENERATOR argument to ODD-APPLY"
  (if (eq true (leaf nil))
      false
      (ensure-gethash (vector variable true false)
                      *node-cache*
                      (make-node :variable variable :true true :false false))))

(defvar *generator* #'bdd-node
  "Node generator function. BDD-NODE(default) or ZDD-NODE.")

(immutable-struct:defstruct odd
  "Structure representing a whole Decision Diagram.
Holds the context information such as variables and node-cache.
For different variable ordering, ODDs are incompatible."
  (root (leaf nil) :type (or node leaf))
  (variables *variables* :type sequence)
  (node-cache *node-cache* :type hash-table)
  (generator *generator* :type (function (fixnum (or node leaf) (or node leaf)) node)))

;; (defmethod print-object ((odd odd) s)
;;   (print-unreadable-object (odd s :type t :identity t)
;;     ;; (princ :root s)
;;     (princ (odd-root odd) s)))

(defmacro with-odd-context ((&key odd variables node-cache generator
                                  &allow-other-keys) &body body)
  "Execute BODY in a dynamic environment where *VARIABLES* , *NODE-CACHE* and *GENERATOR* are set.
When ODD is specified, its values are used by default, but are superseded by the specified ones."
  `(call-with-odd-context ,odd ,variables ,node-cache ,generator (lambda () ,@body)))

(defun call-with-odd-context (odd variables node-cache generator body-fn)
  (match odd
    ((odd :variables vs :node-cache nc :generator g)
     (let ((*variables* (or variables vs))
           (*node-cache* (or node-cache nc))
           (*generator* (or generator g *generator*)))
       (funcall body-fn)))
    (_
     (let ((*variables* variables)
           (*node-cache* (or node-cache (tg:make-weak-hash-table :weakness :value :test #'equalp)))
           (*generator* (or generator *generator*)))
       (funcall body-fn)))))

(defun-ematch* odd-compatible-p (odd1 odd2)
  (((odd :variables vars1
         :node-cache node-cache1
         :generator generator1)
    (odd :variables vars2
         :node-cache node-cache2
         :generator generator2))
   (and
    ;; (eq (type-of odd1) (type-of odd2))
    (eq node-cache1 node-cache2)
    (eq generator1 generator2)
    (equalp vars1 vars2))))

(defun-ematch* odd-apply (odd1 odd2 op-leaf)
  (((odd root variables node-cache generator) (odd :root root2))
   (assert (odd-compatible-p odd1 odd2) nil "ODD ~a and ~a are incompatible!" odd1 odd2)
   (odd (node-apply root root2 generator op-leaf) variables node-cache generator)))

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



