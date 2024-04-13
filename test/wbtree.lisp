
(in-package #:deterministic-arts.wbtree.test)

(w:define string-tree
  (:comparator wi:compare-strings)
  (:constructor make-string-tree)
  (:constructor* string-tree)
  (:predicate string-tree-p))

(def-suite deterministic-arts.wbtree.test
  :description "Tests for the WB tree implementation")

(in-suite deterministic-arts.wbtree.test)

(test properties-of-empty-nodes
  (let ((node (string-tree)))
    (is (null (w:key node)))
    (is (null (w:value node)))
    (is (null (w:left node)))
    (is (null (w:right node)))
    (is (eql 0 (w:size node)))
    (is-true (w:emptyp node))))

(test adding-new-entries
  (let ((old-node (string-tree)))
    (multiple-value-bind (new-node change) (w:update "Hello" "World" old-node)
      (wi:check-invariants new-node)
      (is (eq change 't))
      (is (not (eq old-node new-node)))
      (is (equal (w:find "Hello" new-node) "World")))))

(test replacing-entries
  (let ((old-node (string-tree "Hello" "World")))
    (multiple-value-bind (new-node change) (w:update "Hello" "Universe" old-node)
      (wi:check-invariants new-node)
      (is-true (string-tree-p change))
      (is (equal (w:key change) "Hello"))
      (is (equal (w:value change) "World"))
      (is (not (eq old-node new-node)))
      (is (equal (w:find "Hello" new-node) "Universe")))))

(test not-replacing-equivalent-entries
  (let ((old-node (string-tree "Hello" "World")))
    (multiple-value-bind (new-node change) (w:update "Hello" "World" old-node :test #'string=)
      (wi:check-invariants new-node)
      (is-false change)
      (is (eq old-node new-node))
      (is (equal (w:find "Hello" new-node) "World")))))

(test removing-entries
  (let ((old-node (string-tree "Hello" "World" "Holla" "Mundo")))
    (wi:check-invariants old-node)
    (multiple-value-bind (new-node change) (w:remove "Holla" old-node)
      (is (string-tree-p change))
      (is (equal (w:key change) "Holla"))
      (is (equal (w:value change) "Mundo"))
      (is (not (eq old-node new-node)))
      (is (null (w:find-node "Holla" new-node))))))

(test removing-missing-entries
  (let ((old-node (string-tree "Hello" "World" "Holla" "Mundo")))
    (wi:check-invariants old-node)
    (multiple-value-bind (new-node change) (w:remove "Hallo" old-node)
      (is (null change))
      (is (eq old-node new-node)))))

(test union-of-trees
  (let* ((tree1 (string-tree "Hello" 1 "World" 2 "Holla" 1 "Mundo" 2))
         (tree2 (string-tree "Hello" 2 "Bonjour" 1 "Grüzi" 2 "Mundo" 1))
         (new-tree (w:union tree1 tree2 :combiner (lambda (key value1 value2) (declare (ignore key)) (max value1 value2)))))
    (wi:check-invariants new-tree)
    (is (eql 6 (w:size new-tree)))
    (is (eql 2 (w:find "Hello" new-tree)))
    (is (eql 2 (w:find "World" new-tree)))
    (is (eql 1 (w:find "Holla" new-tree)))
    (is (eql 2 (w:find "Mundo" new-tree)))
    (is (eql 1 (w:find "Bonjour" new-tree)))
    (is (eql 2 (w:find "Grüzi" new-tree)))))

(test intersection-of-trees
  (let* ((tree1 (string-tree "Hello" 1 "World" 2 "Holla" 1 "Mundo" 2))
         (tree2 (string-tree "Hello" 2 "Bonjour" 1 "Grüzi" 2 "Mundo" 1))
         (new-tree (w:intersection tree1 tree2 :combiner (lambda (key value1 value2) (declare (ignore key)) (min value1 value2)))))
    (wi:check-invariants new-tree)
    (is (eql 2 (w:size new-tree)))
    (is (eql 1 (w:find "Hello" new-tree)))
    (is (eql 1 (w:find "Mundo" new-tree)))))

(test difference-of-trees
  (let* ((tree1 (string-tree "Hello" t "World" t "Holla" t "Mundo" t))
         (tree2 (string-tree "Hello" t "Bonjour" t "Grüzi" t "Mundo" t))
         (new-tree (w:difference tree1 tree2)))
    (wi:check-invariants new-tree)
    (is (eql 2 (w:size new-tree)))
    (is-true (w:find "World" new-tree))
    (is-true (w:find "Holla" new-tree))))
