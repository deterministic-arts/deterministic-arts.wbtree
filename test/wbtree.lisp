
(in-package #:deterministic-arts.wbtree.test)

(w:define string-tree
  (:comparator wi:compare-strings)
  (:constructor make-string-tree)
  (:constructor* string-tree)
  (:predicate string-tree-p))

(defun collect-pairs* (object &optional options)
  (let ((iterator (w:node-iterator* object options)))
    (loop
      for node = (w:next-node iterator) then (w:next-node iterator) while node
      as key = (w:key node)
      as value = (w:value node)
      collecting (cons key value))))

(defun generate-strings (count min-key-length)
  (loop
    for number upfrom 0 below count
    collecting (format nil "~V,'0D" min-key-length number)))

(defun pick-matches (list &key from-end min max above below comparator)
  (let ((picks (loop
                 for string in list
                 when (and (or (not min) (string<= min string))
                           (or (not max) (string<= string max))
                           (or (not above) (string< above string))
                           (or (not below) (string< string below))
                           (or (not comparator) (zerop (funcall comparator string))))
                   collect string)))
    (if from-end (reverse picks) picks)))

(defun fisher-yates (sequence)
  (let* ((array (if (vectorp sequence) (copy-seq sequence) (map 'vector #'identity sequence)))
         (length (length array)))
    (loop
      for i downfrom (- length 1) to 1
      as j = (random (1+ i))
      do (rotatef (aref array i) (aref array j)))
    array))

(defun random-string-tree (node-count key-range key-length)
  (loop
    with result = (string-tree)
    while (< (w:size result) node-count)
    do (let* ((number (random key-range))
              (text (format nil "~V,'0D" key-length number)))
         (setf (w:find text result) number))
    finally (return result)))

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

(def-suite deterministic-arts.wbtree.test.iteration :in deterministic-arts.wbtree.test)

(test (empty-tree :suite deterministic-arts.wbtree.test.iteration)
  (is (null (collect (string-tree)))))

(test (iteration :suite deterministic-arts.wbtree.test.iteration)
  (let* ((sorted-keys (generate-strings 129 3))
         (lowers (list nil (car sorted-keys) (cadr sorted-keys)))
         (uppers (list nil (car (last sorted-keys 1)) (car (last sorted-keys 2))))
         (comparators (list nil (lambda (string)
                                  (cond
                                    ((string< string "005") -1)
                                    ((string> string "099") 1)
                                    (t 0)))))
         (scrambled-keys (fisher-yates sorted-keys))
         (tree (reduce (lambda (tree key) (w:update key key tree)) scrambled-keys :initial-value (string-tree))))
    (loop
      for from-end in '(nil t)
      do (loop
           for min in lowers
           do (loop
                for above in lowers
                do (loop
                     for max in uppers
                     do (loop
                          for below in uppers
                          do (loop
                               for comparator in comparators
                               do (let* ((selection (nconc (list :from-end from-end)
                                                           (when comparator (list :comparator comparator))
                                                           (when min (list :min min))
                                                           (when above (list :above above))
                                                           (when max (list :max max))
                                                           (when below (list :below below))))
                                         (expected-keys (apply #'pick-matches sorted-keys selection))
                                         (produced-keys (mapcar #'car (collect-pairs* tree selection))))
                                    (is (equal expected-keys produced-keys)
                                        "Checked with range options ~S" selection))))))))))
