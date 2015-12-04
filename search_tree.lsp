(defpackage avl-tree
  (:nicknames :avl)
  (:use common-lisp))

(in-package :avl)

(defclass avl ()
  ((node :accessor node
         :initarg :node
         :initform nil)
   (left :accessor left
         :initarg :left
         :initform nil)
   (right :accessor right
          :initarg :right
          :initform nil)))


;;; -------------------------------------------------------------------------
;;; util methods
;;; -------------------------------------------------------------------------
(defmethod get-node ((tree avl))
  (node tree))

(defmethod get-node ((tree t))
  tree)

(defmethod get-left ((tree avl))
  (left tree))

(defmethod get-left ((tree t))
  nil)

(defmethod get-right ((tree avl))
  (right tree))

(defmethod get-right ((tree t))
  nil)

(defmethod get-rightmost ((tree avl))
  (if (right tree)
      (get-rightmost (right tree))
      (node tree)))

(defmethod get-rightmost ((tree t))
  tree)

(defmethod get-leftmost ((tree avl))
  (if (left tree)
      (get-leftmost (left tree))
      (node tree)))

(defmethod get-leftmost ((tree t))
  tree)

(defmethod high ((tree avl))
  (1+ (max (high (get-left tree)) (high (get-right tree)))))

(defmethod high ((tree t))
  1)

(defmethod get-balance ((tree avl))
  (- (high (get-right tree)) (high (get-left tree))))

(defmethod get-balance ((tree t))
  0)

(defmethod avl-empty? ((tree avl))
  (or (null (get-node tree))
      (and (null (get-left tree))
           (null (get-right tree)))))

(defmethod avl-keys ((tree avl))
  (let ((keys))
    (avl-traverse tree (lambda (x) (push x keys)))
    keys))

(defmethod avl-edges ((tree avl))
  (append
   (remove nil (list (if (get-left tree)
                         (list (get-node tree) (get-node (get-left tree)) :L))
                     (if (get-right tree)
                         (list (get-node tree) (get-node (get-right tree)) :R))))
   (avl-edges (get-left tree))
   (avl-edges (get-right tree))))

(defmethod avl-edges ((tree t))
  ())

(defmethod avl-balance ((tree avl))
  (cond
    ((< (get-balance tree) -1)
     (make-instance 'avl
                    :node (get-node (get-left tree))
                    :left (get-left (get-left tree))
                    :right (make-instance 'avl
                                          :node (get-node tree)
                                          :right (get-right tree)
                                          :left (get-right (get-left tree)))))
    ((> (get-balance tree) 1)
     (make-instance 'avl
                    :node (get-node (get-right tree))
                    :left (make-instance 'avl
                                         :node (get-node tree)
                                         :right (get-left (get-right tree))
                                         :left (get-left tree))
                    :right (get-right (get-right tree))))
    (t tree)))

(defmethod avl-balance ((tree t))
  tree)

(defmethod print-object ((tree avl) stream)
  (format stream "(~A (~A ~A))" (get-node tree) (get-left tree) (get-right tree)))

;;; -------------------------------------------------------------------------
;;; interface methods
;;; -------------------------------------------------------------------------
(defmethod avl-traverse ((tree avl) fn)
  (make-instance 'avl
                 :node (funcall fn (get-node tree))
                 :left (avl-traverse (get-left tree) fn)
                 :right (avl-traverse (get-right tree) fn)))

(defmethod avl-traverse ((tree t) fn)
  (if tree
      (funcall fn tree)))


(defmethod avl-find (key (tree avl))
  (if (= (get-node tree) key)
      tree
      (if (< key (get-node tree))
          (avl-find key (get-left tree))
          (avl-find key (get-right tree)))))

(defmethod avl-find (key (tree t))
  nil)


(defmethod avl-insert (key (tree avl))
  (avl-balance
   (if (< key (get-node tree))
       (make-instance 'avl
                      :node (get-node tree)
                      :left (avl-insert key (get-left tree))
                      :right(get-right tree))
       (make-instance 'avl
                      :node (get-node tree)
                      :left (get-left tree)
                      :right (avl-insert key (get-right tree))))))

(defmethod avl-insert (key (tree t))
  (if tree
      (if (> tree key)
          (make-instance 'avl
                         :node tree
                         :left key)
          (make-instance 'avl
                         :node tree
                         :right key))
      key))


(defmethod avl-remove (key (tree avl))
  (if (= (get-node tree) key)
      (cond
        ((and (null (get-left tree))
              (null (get-right tree)))
         nil)
        ((null (get-left tree))
         (make-instance 'avl
                        :node (get-node (get-right tree))
                        :left (get-left (get-right tree))
                        :right (get-right (get-right tree))))
        ((null (get-right tree))
         (make-instance 'avl
                        :node (get-node (get-left tree))
                        :left (get-left (get-left tree))
                        :right (get-right (get-left tree))))
        (t (avl-balance
            (if (>= (get-balance tree) 0)
                (let ((new-node (get-leftmost (get-right tree))))
                  (make-instance 'avl
                                 :node new-node
                                 :left (get-left tree)
                                 :right (avl-remove
                                         new-node
                                         (get-right tree))))
                (let ((new-node (get-rightmost (get-left tree))))
                  (make-instance 'avl
                                 :node new-node
                                 :left (avl-remove
                                        new-node
                                        (get-left tree))
                                 :right (get-right tree)))))))
      (if (< key (get-node tree))
          (make-instance 'avl
                         :node (get-node tree)
                         :left (avl-remove key (get-left tree))
                         :right (get-right tree))
          (make-instance 'avl
                         :node (get-node tree)
                         :left (get-left tree)
                         :right (avl-remove key (get-right tree))))))

(defmethod avl-remove (key (tree t))
  (unless (= key tree)
    tree))

;;; -------------------------------------------------------------------------
;;; converting to graph
;;; -------------------------------------------------------------------------
(defmethod avl->dot-helper ((g avl) s)
  (let ((id-map (make-hash-table))
        (i 0))
    (labels ((%get-id (el)
               (gethash el id-map))
             (%gen-id (el)
               (setf (gethash el id-map) i)
               (setf i (1+ i))
               (1- i))
             (%header ()
               (format s "digraph G {~%"))
             (%footer () (format s "}~%"))
             (%add-vertexes ()
               (mapc (lambda (x)
                       (format s "~a [label=\"~a\"];~%" (%gen-id x) x)) (reverse (avl-keys g))))
             (%add-edges ()
               (mapc (lambda (x)
                       (format s "~a -> ~a [label=\"~a\"];~%"
                               (%get-id (first x)) (%get-id (second x)) (third x))) (avl-edges g))))
      (%header)
      (%add-vertexes)
      (%add-edges)
      (%footer))))

(defmethod avl->dot ((tree avl) &key (path "~/OS_Lab4/tree"))
  (ensure-directories-exist path)
  (with-open-file (s (format nil "~A.dot" path) :direction :output :if-exists :supersede)
    (avl->dot-helper tree s))
  (asdf:run-shell-command "dot -Tsvg ~A.dot -o ~A.svg" path path))

(defmethod avl->dot ((tree t) &key path)
  (declare (ignore path)))


;;; -------------------------------------------------------------------------
;;; test
;;; -------------------------------------------------------------------------
(defun test ()
  (let ((tree))
    (loop for i from 0 to 20 do
         (setf tree (avl-insert i tree)))
    (avl->dot tree :path "~/OS_Lab4/tree-ins")
    (avl->dot (avl-find 5 tree) :path "~/OS_Lab4/tree-find")
    (avl->dot (avl-remove 5 tree) :path "~/OS_Lab4/tree-remv")
    (avl->dot (avl-remove
               9 (avl-remove 5 tree)) :path "~/OS_Lab4/tree-remv2")))

(test)
