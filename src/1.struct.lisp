
(in-package :trivialib.bdd)

#+trivialib.bdd.debug
(defvar *id* 0)

(defstruct leaf
  "Leaf node of a decision diagram"
  (content nil)
  #+trivialib.bdd.debug
  (id (incf *id*)))

(declaim (type hash-table *leaf-cache*))
(defvar *leaf-cache* (tg:make-weak-hash-table :weakness :value :test #'eql)
  "hash table to look up a leaf node of a thing.")

(defun leaf (thing)
  "Return the leaf node in the hash table *leaf-cache*, creating an instance when required"
  (ensure-gethash thing *leaf-cache* (make-leaf :content thing)))

(defstruct node
  "lightweight node in Decision Diagram.
VARIABLE: an integer representing the index of a variable. cf. VARIABLES slot in structure DD
TRUE,FALSE: true/false pointer"
  (variable 0 :type fixnum)
  (true (leaf nil) :type (or node leaf))
  (false (leaf nil) :type (or node leaf))
  #+trivialib.bdd.debug
  (id (incf *id*)))

(defvar *node-cache*)
(declaim (type hash-table *node-cache*))

(setf (documentation '*node-cache* 'variable)
      "hash table to look up in order to avoid the creation of redundunt nodes.")

(defvar *variables*)
(declaim (type sequence *variables*))

(setf (documentation '*variables* 'variable)
      "ODD variables in the current context.")


(defpattern node (&optional variable true false)
  `(structure node :variable ,variable :true ,true :false ,false))


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
