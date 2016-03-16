
(in-package :trivialib.bdd)


(defstruct leaf
  "Leaf node of a decision diagram"
  (content nil))

(declaim (type hash-table *leaf-cache*))
(defvar *leaf-cache* (tg:make-weak-hash-table :weakness :value :test #'eql)
  "hash table to look up a leaf node of a thing.")

(defun leaf (thing)
  "Return the leaf node in the hash table *leaf-cache*, creating an instance when required"
  (ensure-gethash thing *leaf-cache* (make-leaf :content thing)))

(defstruct node
  "lightweight node in Decision Diagram
variable: an integer representing the index of a variable. cf. VARIABLES slot in structure DD
true,false: true/false pointer"
  (variable 0 :type fixnum)
  (true (leaf nil) :type (or node leaf))
  (false (leaf nil) :type (or node leaf)))

(declaim (type hash-table *node-cache*))
;; (tg:make-weak-hash-table :weakness :value :test #'equalp)
(defvar *node-cache*)

(setf (documentation '*node-cache* 'variable)
      "hash table to look up in order to avoid the creation of redundunt nodes.")

(defun node (variable true false)
  (ensure-gethash (vector variable true false)
                  *node-cache*
                  (make-node :variable variable :true true :false false)))


(defpattern node (&optional variable true false)
  `(structure node :variable ,variable :true ,true :false ,false))
