#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- WB Tree
  Copyright (c) 2024 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package #:deterministic-arts.wbtree.internals)

(defconstant +weight+ 4)

(defgeneric configuration (object))     ; compare, constructor, empty, constructor-name, empty-name

(defun compare-function (object)
  (nth-value 0 (configuration object)))

(defstruct (api:node (:copier nil) (:conc-name nil) (:predicate api:nodep)
                     (:constructor nil))
  (api:key (error "missing") :read-only t)
  (api:value (error "missing") :read-only t)
  (api:size (error "missing") :type (integer 0) :read-only t)
  (api:left (error "missing") :read-only t)
  (api:right (error "missing") :read-only t))

(defun api:emptyp (object)
  (zerop (api:size object)))

(defmacro with-configuration (tree &body body)
  (let ((arg1 (gensym))
        (arg2 (gensym))
        (arg3 (gensym))
        (arg4 (gensym)))
    `(multiple-value-bind (compare make-node empty-node) (configuration ,tree)
       (declare (ignorable compare make-node empty-node))
       (macrolet ((compare (,arg1 ,arg2) `(funcall compare ,,arg1 ,,arg2))
                  (make-node (,arg1 ,arg2 ,arg3 ,arg4) `(funcall make-node ,,arg1 ,,arg2 ,, arg3 ,,arg4)))
         ,@body))))       

(defmacro with-node ((key api:value size left right) node &body body)
  (let ((node-var (gensym)))
    `(let ((,node-var ,node))
       (declare (ignorable ,node-var))
       (let (,@(unless (string= "_" key) (list `(,key (api:key ,node-var))))
             ,@(unless (string= "_" api:value) (list `(,api:value (api:value ,node-var))))
             ,@(unless (string= "_" size) (list `(,size (api:size ,node-var))))
             ,@(unless (string= "_" left) (list `(,left (api:left ,node-var))))
             ,@(unless (string= "_" right) (list `(,right (api:right ,node-var)))))
         ,@body))))

(defmacro if-let-aux ((&rest bindings) then else)
  (if (null bindings)
      then
      (let ((first (car bindings)))
        `(let (,first)
           (if ,(first first)
               (if-let-aux (,@(cdr bindings)) ,then ,else)
               ,else)))))

(defmacro if-let* ((&rest bindings) then else)
  (cond
    ((null bindings) else)
    ((null (cdr bindings))
     `(let ,bindings
        (if ,(caar bindings) ,then ,else)))
    (t (let ((else-label (gensym))
             (block-name (gensym)))
         `(block ,block-name
            (tagbody
               (return-from ,block-name (if-let-aux (,@bindings) ,then ,else))
               ,else-label
               (return-from ,block-name ,else)))))))

(defmacro named-let (name (&rest bindings) &body body)
  (let* ((repeat (gensym))
         (temps (loop for binding in bindings
                      collecting (list (if (atom binding) binding (first binding))
                                       (gensym)
                                       (if (atom binding) nil (second binding))))))
    `(block ,name
       (let (,@(mapcar #'cdr temps))
         (tagbody
            ,repeat
            (return-from ,name
              (macrolet ((,name (,@(mapcar #'second temps))
                           (list 'progn
                                 (list 'setq ,@(loop for (nil temp) in temps nconcing (list `',temp temp)))
                                 (list 'go ',repeat))))
                (let (,@(mapcar (lambda (e) (list (first e) (second e))) temps))
                  ,@body))))))))

(defun api:minimum-node (object)
  (named-let walk ((current object) (previous nil))
    (if (api:emptyp current) previous
        (walk (api:left current) current))))

(defun api:maximum-node (object)
  (named-let walk ((current object) (previous nil))
    (if (api:emptyp current) previous
        (walk (api:right current) current))))

(defun ceiling-node (key object compare)
  (named-let walk ((object object) (best nil))
    (if (api:emptyp object) best
        (let ((order (funcall compare (api:key object) key)))
          (cond
            ((minusp order) (walk (api:right object) best))
            ((plusp order) (walk (api:left object) object))
            (t object))))))

(defun floor-node (key object compare)
  (named-let walk ((object object) (best nil))
    (if (api:emptyp object) best
        (let ((order (funcall compare (api:key object) key)))
          (cond
            ((minusp order) (walk (api:right object) object))
            ((plusp order) (walk (api:left object) best))
            (t object))))))

(defun find-node (key object compare)
  (named-let walk ((object object))
    (if (api:emptyp object) nil
        (let ((order (funcall compare (api:key object) key)))
          (cond
            ((minusp order) (walk (api:right object)))
            ((plusp order) (walk (api:left object)))
            (t object))))))

(defun api:floor-node (key object)
  (floor-node key object (compare-function object)))

(defun api:ceiling-node (key object)
  (floor-node key object (compare-function object)))

(defun api:find-node (key object)
  (find-node key object (compare-function object)))

(defun api:floor-key (key object &optional default)
  (if-let* ((node (api:floor-node key object)))
    (values (api:key node) t)
    (values default nil)))

(defun api:ceiling-key (key object &optional default)
  (if-let* ((node (api:ceiling-node key object)))
    (values (api:key node) t)
    (values default nil)))

(defun api:minimum-key (object &optional default)
  (if-let* ((node (api:minimum-node object)))
    (values (api:key node) t)
    (values default nil)))

(defun api:maximum-key (object &optional default)
  (if-let* ((node (api:maximum-node object)))
    (values (api:key node) t)
    (values default nil)))
    
(defun api:find (key object &optional default)
  (if-let* ((node (api:find-node key object)))
    (values (api:value node) t)
    (values default nil)))


(defun rebalance (make-node key value left right)
  (flet
      ((rotate-once-left (key value left right)
         (with-node (key* value* _ left* right*) right
           (funcall make-node key* value*
                    (funcall make-node key value left left*)
                    right*)))
       (rotate-once-right (key value left right)
         (with-node (key* value* _ left* right*) left
           (funcall make-node key* value* 
                    left*
                    (funcall make-node key value right* right))))
       (rotate-twice-left (key value left right)
         (with-node (key* value* _ left* right*) right
           (with-node (key** value** _ left** right**) left*
             (funcall make-node key** value**
                      (funcall make-node key value left left**)
                      (funcall make-node key* value* right** right*)))))
       (rotate-twice-right (key value left right)
         (with-node (key* value* _ left* right*) left
           (with-node (key** value** _ left** right**) right*
             (funcall make-node key** value**
                      (funcall make-node key* value* left* left**)
                      (funcall make-node key value right** right))))))
    (let ((ln (api:size left))
          (rn (api:size right)))
      (cond ((< (+ ln rn) 2) (funcall make-node key value left right))
            ((> rn (* +weight+ ln)) 
             (with-node (_ _ _ rl rr) right
               (let ((rln (api:size rl))
                     (rrn (api:size rr)))
                 (if (< rln rrn)
                     (rotate-once-left key value left right)
                     (rotate-twice-left key value left right)))))
            ((> ln (* +weight+ rn))
             (with-node (_ _ _ ll lr) left
               (let ((lln (api:size ll))
                     (lrn (api:size lr)))
                 (if (< lrn lln)
                     (rotate-once-right key value left right)
                     (rotate-twice-right key value left right)))))
            (t (funcall make-node key value left right))))))

(defun api:update (key value object &key test)
  (with-configuration object
    (labels ((insert (node)
               (if (api:emptyp node) 
                   (values (make-node key value empty-node empty-node) t)
                   (with-node (nkey nvalue _ left right) node
                     (let ((order (compare key nkey)))
                     (cond
                         ((minusp order)
                          (multiple-value-bind (new-left change) (insert left)
                            (if (not change)
                                (values node nil)
                                (values (rebalance make-node nkey nvalue new-left right)
                                        change))))
                         ((plusp order)
                          (multiple-value-bind (new-right change) (insert right)
                            (if (not change)
                                (values node nil)
                                (values (rebalance make-node nkey nvalue left new-right)
                                        change))))
                         ((and test (funcall test value nvalue)) (values node nil))
                         (t (values (make-node key value left right) node))))))))
      (insert object))))

(defun delete-minimum (make-node node)
  (with-node (key value _ left right) node
    (if (api:emptyp left) right
        (rebalance make-node key value (delete-minimum make-node left) right))))

(defun delete* (make-node left right)
  (cond
    ((api:emptyp left) right)
    ((api:emptyp right) left)
    (t (let* ((min-node (api:minimum-node right))
              (min-key (api:key min-node))
              (min-value (api:value min-node)))
         (rebalance make-node min-key min-value left 
                    (delete-minimum make-node right))))))

(defun api:remove (key object)
  (with-configuration object
    (labels ((drop (node)
               (if (api:emptyp node)
                   (values node nil)
                   (with-node (nkey nvalue _ left right) node
                     (let ((order (compare key nkey)))
                       (cond
                         ((minusp order)
                          (multiple-value-bind (new-node old-node) (drop left)
                            (if (not old-node)
                                (values node nil)
                                (values (rebalance make-node nkey nvalue new-node right)
                                        old-node))))
                         ((plusp order)
                          (multiple-value-bind (new-node old-node) (drop right)
                            (if (not old-node)
                                (values node nil)
                                (values (rebalance make-node nkey nvalue left new-node)
                                        old-node))))
                         (t (values (delete* make-node left right)
                                    node))))))))
      (drop object))))

(defun pick-right (key left right)
  (declare (ignore key left))
  right)

(defun api:union (object1 object2 &key (combiner #'pick-right) (test #'eql))
  (with-configuration object1
    (labels
        ((insert (key value node)
           (if (api:emptyp node) 
               (make-node key value empty-node empty-node)
               (with-node (nkey nvalue _ left right) node
                 (let ((order (compare key nkey)))
                   (cond
                     ((minusp order)
                      (let ((new-left (insert key value left)))
                        (if (eq left new-left) node (rebalance make-node nkey nvalue new-left right))))
                     ((plusp order)
                      (let ((new-right (insert key value right)))
                        (if (eq right new-right) node (rebalance make-node nkey nvalue left new-right))))
                     ((funcall test value nvalue) node)
                     (t (make-node key value left right)))))))
         (concat-3 (key value left right) 
           (cond
             ((api:emptyp left) (insert key value right))
             ((api:emptyp right) (insert key value left))
             (t (with-node (k1 v1 n1 l1 r1) left
                  (with-node (k2 v2 n2 l2 r2) right
                    (cond
                      ((< (* +weight+ n1) n2) (rebalance make-node k2 v2 (concat-3 key value left l2) r2))
                      ((< (* +weight+ n2) n1) (rebalance make-node k1 v1 l1 (concat-3 key value r1 right)))
                      (t (make-node key value left right))))))))
         (split-lt (key value tree)
           (if (api:emptyp tree) 
               (values tree value)
               (with-node (k v _ l r) tree
                 (let ((order (compare key k)))
                   (cond
                     ((minusp order) (split-lt key value l))
                     ((plusp order) 
                      (multiple-value-bind (nr val) (split-lt key value r)
                        (values (concat-3 k v l nr) val)))
                     (t (values l (funcall combiner key v value))))))))
         (split-gt (key tree)
           (if (api:emptyp tree) tree
               (with-node (k v _ l r) tree
                 (let ((order (compare key k)))
                   (cond
                     ((minusp order) (concat-3 k v (split-gt key l) r))
                     ((plusp order) (split-gt key r))
                     (t r))))))
         (union* (tree1 tree2)
           (cond 
             ((eq tree1 tree2) tree1)
             ((api:emptyp tree2) tree1)
             ((api:emptyp tree1) tree2)
             (t (with-node (k v _ l r) tree2
                  (multiple-value-bind (l* v*) (split-lt k v tree1)
                    (let ((r* (split-gt k tree1)))
                      (concat-3 k v*
                                (union* l* l)
                                (union* r* r)))))))))
      (union* object1 object2))))

(defun api:intersection (object1 object2 &key (combiner #'pick-right) (test #'eql))
  (with-configuration object1
    (labels ((memberp (key value tree)
               (if (api:emptyp tree)
                   (values nil value)
                   (let* ((key* (api:key tree))
                          (order (compare key key*)))
                     (cond
                       ((minusp order) (memberp key value (api:left tree)))
                       ((plusp order) (memberp key value (api:right tree)))
                       (t (values t (funcall combiner key (api:value tree) value)))))))
             (insert (key value node)
               (if (api:emptyp node) 
                   (make-node key value empty-node empty-node)
                   (with-node (nkey nvalue _ left right) node
                     (let ((order (compare key nkey)))
                       (cond
                         ((minusp order)
                          (let ((new-left (insert key value left)))
                            (if (eq left new-left) node (rebalance make-node nkey nvalue new-left right))))
                         ((plusp order)
                          (let ((new-right (insert key value right)))
                            (if (eq right new-right) node (rebalance make-node nkey nvalue left new-right))))
                         ((funcall test value nvalue) node)
                         (t (make-node key value left right)))))))
             (concat (tree1 tree2)
               (cond
                 ((api:emptyp tree1) tree2)
                 ((api:emptyp tree2) tree1)
                 (t (with-node (k1 v1 n1 l1 r1) tree1
                      (with-node (k2 v2 n2 l2 r2) tree2
                        (cond ((< (* +weight+ n1) n2) (rebalance make-node k2 v2 (concat tree1 l2) r2))
                              ((< (* +weight+ n2) n1) (rebalance make-node k1 v1 l1 (concat r1 tree2)))
                              (t (let* ((min-node (api:minimum-node tree2))
                                        (min-key (api:key min-node))
                                        (min-value (api:value min-node)))
                                   (rebalance make-node min-key min-value tree1 (delete-minimum make-node tree2))))))))))
             (concat-3 (key value left right) 
               (cond
                 ((api:emptyp left) (insert key value right))
                 ((api:emptyp right) (insert key value left))
                 (t (with-node (k1 v1 n1 l1 r1) left
                      (with-node (k2 v2 n2 l2 r2) right
                        (cond
                          ((< (* +weight+ n1) n2) (rebalance make-node k2 v2 (concat-3 key value left l2) r2))
                          ((< (* +weight+ n2) n1) (rebalance make-node k1 v1 l1 (concat-3 key value r1 right)))
                          (t (make-node key value left right))))))))
             (split-lt (key tree)
               (if (api:emptyp tree) tree
                   (with-node (k v _ l r) tree
                     (let ((order (compare key k)))
                     (cond
                       ((minusp order) (split-lt key l))
                       ((plusp order) (concat-3 k v l (split-lt key r)))
                       (t l))))))
             (split-gt (key tree)
               (if (api:emptyp tree) tree
                   (with-node (k v _ l r) tree
                     (let ((order (compare key k)))
                     (cond
                       ((minusp order) (concat-3 k v (split-gt key l) r))
                       ((plusp order) (split-gt key r))
                       (t r))))))
             (intersect* (tree1 tree2)
               (cond 
                 ((eq tree1 tree2) tree1)
                 ((api:emptyp tree1) empty-node)
                 ((api:emptyp tree2) empty-node)
                 (t (with-node (k v _ l r) tree2
                      (let ((l* (split-lt k tree1))
                            (r* (split-gt k tree1)))
                        (multiple-value-bind (member value) (memberp k v tree1)
                          (if member
                              (concat-3 k value (intersect* l* l) (intersect* r* r))
                              (concat (intersect* l* l) (intersect* r* r))))))))))
      (intersect* object1 object2))))

(defun api:difference (object1 object2 &key (test #'eql))
  (with-configuration object1
    (labels ((insert (key value node)
               (if (api:emptyp node) 
                   (make-node key value empty-node empty-node)
                   (with-node (nkey nvalue _ left right) node
                     (let ((order (compare key nkey)))
                       (cond
                         ((minusp order)
                          (let ((new-left (insert key value left)))
                            (if (eq left new-left) node (rebalance make-node nkey nvalue new-left right))))
                         ((plusp order)
                          (let ((new-right (insert key value right)))
                            (if (eq right new-right) node (rebalance make-node nkey nvalue left new-right))))
                         ((funcall test value nvalue) node)
                         (t (make-node key value left right)))))))
             (concat (tree1 tree2)
               (cond
                 ((api:emptyp tree1) tree2)
                 ((api:emptyp tree2) tree1)
                 (t (with-node (k1 v1 n1 l1 r1) tree1
                      (with-node (k2 v2 n2 l2 r2) tree2
                        (cond ((< (* +weight+ n1) n2) (rebalance make-node k2 v2 (concat tree1 l2) r2))
                              ((< (* +weight+ n2) n1) (rebalance make-node k1 v1 l1 (concat r1 tree2)))
                              (t (let* ((min-node (api:minimum-node tree2))
                                        (min-key (api:key min-node))
                                        (min-value (api:value min-node)))
                                   (rebalance make-node min-key min-value tree1 (delete-minimum make-node tree2))))))))))
             (concat-3 (key value left right) 
               (cond
                 ((api:emptyp left) (insert key value right))
                 ((api:emptyp right) (insert key value left))
                 (t (with-node (k1 v1 n1 l1 r1) left
                      (with-node (k2 v2 n2 l2 r2) right
                        (cond
                          ((< (* +weight+ n1) n2) (rebalance make-node k2 v2 (concat-3 key value left l2) r2))
                          ((< (* +weight+ n2) n1) (rebalance make-node k1 v1 l1 (concat-3 key value r1 right)))
                          (t (make-node key value left right))))))))
             (split-lt (key tree)
               (if (api:emptyp tree) tree
                   (with-node (k v _ l r) tree
                     (let ((order (compare key k)))
                     (cond
                       ((minusp order) (split-lt key l))
                       ((plusp order) (concat-3 k v l (split-lt key r)))
                       (t l))))))
             (split-gt (key tree)
               (if (api:emptyp tree) tree
                   (with-node (k v _ l r) tree
                     (let ((order (compare key k)))
                     (cond
                       ((minusp order) (concat-3 k v (split-gt key l) r))
                       ((plusp order) (split-gt key r))
                       (t r))))))
             (difference* (tree1 tree2)
               (cond 
                 ((eq tree1 tree2) empty-node)
                 ((api:emptyp tree1) tree1)
                 ((api:emptyp tree2) tree1)
                 (t (with-node (k _ _ l r) tree2
                      (let ((l* (split-lt k tree1))
                            (r* (split-gt k tree1)))
                        (concat (difference* l* l)
                                (difference* r* r))))))))
      (difference* object1 object2))))

(defun api:equal (object1 object2 &key (test #'equal))
  (cond
    ((eq object1 object2) t)
    ((api:emptyp object1) (api:emptyp object2))
    ((api:emptyp object2) nil)
    (t (with-configuration object1
         (labels
             ((descend (node stack)
                (if (api:emptyp node) stack
                    (descend (api:left node) (cons node stack))))
              (next (stack)
                (values (car stack) (descend (api:right (car stack)) (cdr stack)))))
           (let ((stack1 (descend object1 nil))
                 (stack2 (descend object2 nil)))
             (loop
               (unless stack1 (return (not stack2)))
               (unless stack2 (return nil))
               (multiple-value-bind (head1 tail1) (next stack1)
                 (multiple-value-bind (head2 tail2) (next stack2)
                   (unless (zerop (compare (api:key head1) (api:key head2))) (return nil))
                   (unless (funcall test (api:value head1) (api:value head2)) (return nil))
                   (setf stack1 tail1)
                   (setf stack2 tail2))))))))))

(defmethod make-load-form ((object api:node) &optional environment)
  (declare (ignore environment))
  (multiple-value-bind (unused1 unused2 unused3 constructor empty) (configuration object)
    (declare (ignore unused1 unused2 unused3))
    (labels
        ((generate (node)
           (if (api:emptyp node)
               empty
               `(,constructor ',(api:key node) ',(api:value node) ,(generate (api:left node))
                              ,(generate (api:right node)) ,(api:size node)))))
      (generate object))))

(defun api:node-iterator (object
                          &key from-end range
                            (from nil have-from) (to nil have-to) (above nil have-above)
                            (below nil have-below))
  (with-configuration object
    (labels
        ((too-small (node) (or (api:emptyp node) (minusp (funcall range node))))
         (too-big (node) (or (api:emptyp node) (plusp (funcall range node))))
         (descend-f (node stack)
           (if (too-small node) stack
               (descend-f (api:left node) (cons node stack))))
         (descend-b (node stack)
           (if (too-big node) stack
               (descend-b (api:right node) (cons node stack))))
         (next-f (stack)
           (cond
             ((null stack) (values nil nil))
             ((too-big (car stack)) (values nil nil))
             (t (values (car stack) (descend-f (api:right (car stack)) (cdr stack))))))
         (next-b (stack)
           (cond
             ((null stack) (values nil nil))
             ((too-small (car stack)) (values nil nil))
             (t (values (car stack) (descend-b (api:left (car stack)) (cdr stack))))))
         (always-in-range-p (node) (declare (ignore node)) 0)
         (make-range-from-test (value)
           (lambda (node) (if (not (minusp (compare (api:key node) value))) 0 -1)))
         (make-range-above-test (value)
           (lambda (node) (if (plusp (compare (api:key node) value)) 0 -1)))
         (make-range-to-test (value)
           (lambda (node) (if (not (plusp (compare (api:key node) value))) 0 1)))
         (make-range-below-test (value)
           (lambda (node) (if (minusp (compare (api:key node) value)) 0 1)))
         (conjoin (test1 test2)
           (cond
             ((not test1) test2)
             ((not test2) test1)
             (t (lambda (node) (and (funcall test1 node) (funcall test2 node)))))))
      (when have-from (setf range (conjoin range (make-range-from-test from))))
      (when have-to (setf range (conjoin range (make-range-to-test to))))
      (when have-above (setf range (conjoin range (make-range-above-test above))))
      (when have-below (setf range (conjoin range (make-range-below-test below))))
      (unless range (setf range #'always-in-range-p))
      (let ((stack (if from-end (descend-b object nil) (descend-f object nil))))
        (if from-end
            (lambda ()
              (multiple-value-bind (node stack*) (next-b stack)
                (setf stack stack*)
                node))
            (lambda ()
              (multiple-value-bind (node stack*) (next-f stack)
                (setf stack stack*)
                node)))))))

(defun api:correlate-nodes (function object1 object2 &rest options)
  (with-configuration object1
    (let ((iter1 (apply #'api:node-iterator object1 options))
          (iter2 (apply #'api:node-iterator object2 options)))
      (named-let next ((head1 (funcall iter1)) (head2 (funcall iter2)))
        (cond
          ((not head1) (funcall function nil head2) (next nil (funcall iter2)))
          ((not head2) (funcall function head1 nil) (next (funcall iter1) nil))
          (t (let ((order (compare (api:key head1) (api:key head2))))
               (cond
                 ((minusp order) (funcall function head1 nil) (next (funcall iter1) head2))
                 ((plusp order) (funcall function nil head2) (next head1 (funcall iter2)))
                 (t (funcall function head1 head2)
                    (next (funcall iter1) (funcall iter2))))))))))
  nil)

(defmethod print-object ((object api:node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~D" :size (api:size object))))

(defun check-invariants (object)
  (with-configuration object
    (labels
        ((recurse (tree)
           (unless (api:emptyp tree)
             (with-node (key _ count left right) tree
               (unless (api:emptyp left)
                 (let ((left-key (api:key left)))
                   (unless (minusp (compare left-key key))
                     (cerror "try remaining nodes" "left child key is >= parent key")))
                 (recurse left))
               (unless (api:emptyp right)
                 (let ((right-key (api:key right)))
                   (unless (minusp (compare key right-key))
                     (cerror "try remaining nodes" "right child key is <= parent key")))
                 (recurse right))
               (let ((nleft (api:size left))
                     (nright (api:size right)))
                 (unless (= count (+ 1 nleft nright))
                   (cerror "try remaining nodes" "invalid tree size counter"))
                 (when (> (+ nleft nright) 2)
                   (unless (or (>= (* +weight+ nleft) nright)
                               (>= (* +weight+ nright) nleft))
                     (cerror "try remaining nodes" "weight invariant violated for node"))))))))
      (recurse object)
      object)))


(defun api:compare-from-lessp (function)
  (lambda (key1 key2)
    (cond
      ((funcall function key1 key2) -1)
      ((funcall function key2 key1) 1)
      (t 0))))

(defun api:compare-reals (object1 object2)
  (cond
    ((< object1 object2) -1)
    ((> object2 object2) 1)
    (t 0)))

(defun api:compare-strings (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let* ((end1 (or end1 (length string1)))
         (end2 (or end2 (length string2)))
         (len1 (- end1 start1))
         (len2 (- end2 start2))
         (clen (min len1 len2))
         (difference (mismatch string1 string2 :start1 start1 :end1 (+ start1 clen) :start2 start2 :end2 (+ start2 clen))))
    (if (not difference)
        (signum (- len1 len2))
        (signum (- (char-code (char string1 difference))
                   (char-code (char string2 (+ start2 (- difference start1)))))))))

(defun compare-strings (string1 string2)
  (let* ((len1 (length string1))
         (len2 (length string2))
         (clen (min len1 len2))
         (difference (mismatch string1 string2 :end1 clen :end2 clen)))
    (if (not difference)
        (signum (- len1 len2))
        (signum (- (char-code (char string1 difference))
                   (char-code (char string2 difference)))))))

(define-setf-expander api:find (key-form place &optional (default nil have-default)
                                &environment env)
  (multiple-value-bind (vars vals newval setter getter) (get-setf-expansion place env)
    (let ((key-temp (gensym))
          (default-temp (if have-default (gensym) nil))
          (value-temp (gensym))
          (new-value (car newval)))
      (if (cdr newval) (error "cannot expand form")
          (values (if have-default
                      (list* key-temp default-temp vars)
                      (cons key-temp vars))
                  (if have-default
                      (list* key-form default vals)
                      (cons key-form vals))
                  (list value-temp)
                  `(let ((,new-value (api:update ,key-temp ,value-temp ,getter)))
                     ,setter
                     ,value-temp)
                  `(api:find ,key-temp ,getter ,@(when have-default (list default-temp))))))))

(defmacro api:define (name &body clauses)
  (let* ((tree-info (or (get name 'wbtree-tree-info)
                        (list (gensym (format nil "~A-~A" #.(symbol-name '#:allocate) (symbol-name name)))
                              (gensym (format nil "+~A-~A+" #.(symbol-name '#:empty) (symbol-name name))))))
         (allocator (first tree-info))
         (empty-node (second tree-info))
         (object (gensym))
         (initial-entries (gensym))
         (result (gensym))
         (walk (gensym))
         compare-function (constructor 't) spread-constructor (predicate 't))
    
    (loop
      for (key . forms) in clauses
      do (ecase key
           ((:documentation))
           ((:predicate) (setf predicate (first forms)))
           ((:constructor) (setf constructor (first forms)))
           ((:constructor*) (setf spread-constructor (first forms)))
           ((:comparator) (setf compare-function (first forms)))))
    
    (when (eq predicate 't)
      (setf predicate (intern (concatenate 'string (symbol-name name)
                                           (if (position #\- (symbol-name name)) "-" "")
                                           #.(symbol-name '#:p)))))
    (when (eq constructor 't)
      (setf constructor (intern (concatenate 'string #.(symbol-name '#:make-)
                                             (symbol-name name)))))
    
    (when (eq spread-constructor 't)
      (setf spread-constructor (intern (concatenate 'string #.(symbol-name '#:make-)
                                                    (symbol-name name) "*"))))

    (unless compare-function
      (error "the option ~S is required" :comparator))
        
    `(progn
       
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'wbtree-tree-info) ',tree-info))

       (defstruct (,name (:copier nil) (:conc-name nil) (:include api:node)
                         ,@(when predicate `((:predicate ,predicate)))
                         (:constructor ,allocator (api:key api:value api:left api:right
                                                   &optional (api:size (+ 1 (api:size api:left) (api:size api:right)))))))

       (defvar ,empty-node (,allocator nil nil nil nil 0))

       ,@(when constructor
           `((defun ,constructor (&optional ,initial-entries)
               (named-let ,walk ((,initial-entries ,initial-entries)
                                 (,result ,empty-node))
                 (if (null ,initial-entries)
                     ,result
                     (,walk (cddr ,initial-entries)
                            (api:update (car ,initial-entries) (cadr ,initial-entries)
                                        ,result)))))))
       
       ,@(when spread-constructor
           `((defun ,spread-constructor (&rest ,initial-entries)
               (named-let ,walk ((,initial-entries ,initial-entries)
                                 (,result ,empty-node))
                 (if (null ,initial-entries)
                     ,result
                     (,walk (cddr ,initial-entries)
                            (api:update (car ,initial-entries) (cadr ,initial-entries)
                                        ,result)))))))
              
       (defmethod configuration ((,object ,name))
         (declare (ignore ,object))
         (values #',compare-function #',allocator ,empty-node
                 ',allocator ',empty-node))
       
       ',name)))

(defmacro api:do ((binding object-form &rest options) &body body)
  (let* ((iterator-var (gensym))
         (restart (gensym))
         (done (gensym))
         (node-var (if (atom binding) binding (gensym)))
         (key-var (if (atom binding) nil (first binding)))
         (value-var (if (atom binding) nil (second binding))))
    `(block nil
       (let ((,iterator-var (api:node-iterator ,object-form ,@options))
             ,node-var
             ,@(when key-var (list key-var))
             ,@(when value-var (list value-var)))
         (tagbody
            ,restart
            (setf ,node-var (funcall ,iterator-var))
            (unless ,node-var (go ,done))
            ,@(when key-var `((setf ,key-var (api:key ,node-var))))
            ,@(when value-var `((setf ,value-var (api:value ,node-var))))
            ,@body
            (go ,restart)
            ,done)))))

