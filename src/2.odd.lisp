
(in-package :trivialib.bdd)

(defvar *operation*)
(setf (documentation '*operation* 'variable)
      "Generic operations for a decision diagram in the current context, either #'BDD-APPLY or #'ZDD-APPLY.")

(immutable-struct:defstruct odd
  "Structure representing a whole Decision Diagram.
Holds the context information such as variables and node-cache.
For different variable ordering, ODDs are incompatible."
  (root (leaf nil) :type (or node leaf))
  (variables *variables* :type sequence)
  (node-cache *node-cache* :type hash-table)
  (operation *operation* :type (function ((or node leaf) (or node leaf) (function (* *) *))
                                         (or node leaf))))

(defmacro with-odd-context ((&key default variables node-cache (operation #'bdd-apply)
                                  &allow-other-keys) &body body)
  "Execute BODY in a dynamic environment where *VARIABLES* , *NODE-CACHE* and *OPERATION* are set.
When DEFAULT is specified, its values are used by default, but are superseded by the specified ones."
  `(call-with-odd-context ,default ,variables ,node-cache ,operation (lambda () ,@body)))

(defun call-with-odd-context (default variables node-cache operation body-fn)
  "Function version of WITH-ODD-CONTEXT."
  (match default
    ((odd :variables vs :node-cache nc :operation op)
     (let ((*variables* (or variables vs))
           (*node-cache* (or node-cache nc))
           (*operation* (or operation op)))
       (funcall body-fn)))
    (_
     (let ((*variables* variables)
           (*node-cache* (or node-cache (tg:make-weak-hash-table :weakness :value :test #'equalp)))
           (*operation* operation))
       (funcall body-fn)))))

(defun odd-compatible-p (odd1 odd2)
  "Check if two ODDs have the same variable ordering, node-cache, and operations."
  (ematch* (odd1 odd2)
    (((odd :variables vars1
           :node-cache node-cache1
           :operation operation1)
      (odd :variables vars2
           :node-cache node-cache2
           :operation operation2))
     (and
      ;; (eq (type-of odd1) (type-of odd2))
      (eq node-cache1 node-cache2)
      (eq operation1 operation2)
      (equalp vars1 vars2)))))

(defun odd-apply (odd1 odd2 leaf-operation)
  "Applies the operation stored in ODD, recursively. At the leaf-level, applies LEAF-OPERATION."
  (ematch* (odd1 odd2)
    (((odd root variables node-cache operation) (odd :root root2))
     (assert (odd-compatible-p odd1 odd2) nil "ODD ~a and ~a are incompatible!" odd1 odd2)
     (odd (funcall operation root root2 leaf-operation) variables node-cache operation))))

