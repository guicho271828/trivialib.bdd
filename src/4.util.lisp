
(in-package :trivialib.bdd)

(defun bdd (root &optional (variables *variables*) (node-cache *node-cache*))
  (odd root variables node-cache #'bdd-node))

(defun zdd (root &optional (variables *variables*) (node-cache *node-cache*))
  (odd root variables node-cache #'zdd-node))

(defun unit (variable)
  ;; we already know the true/false branch aren't the same and the HI node
  ;; is not (leaf nil)
  (let ((true (leaf t))
        (false (leaf nil)))
    (ensure-gethash (vector variable true false)
                    *node-cache*
                    (make-node :variable variable :true true :false false))))

(defun !unit (variable)
  ;; we already know the true/false branch aren't the same and the HI node
  ;; is not (leaf nil)
  (let ((true (leaf t))
        (false (leaf nil)))
    (ensure-gethash (vector variable false true)
                    *node-cache*
                    (make-node :variable variable :true false :false true))))




