
(defpackage #:deterministic-arts.wbtree
  (:use)
  (:export #:node #:nodep #:key #:value #:left #:right #:size #:emptyp #:find #:update
           #:find-node #:remove #:fold #:map #:union #:intersection #:difference #:iterator
           #:define #:minimum-node #:maximum-node #:floor-node #:ceiling-node #:minimum-key
           #:maximum-key #:floor-key #:ceiling-key #:equal #:node-iterator #:do #:correlate-nodes
           #:compare-from-lessp #:compare-strings #:compare-reals))

(defpackage #:deterministic-arts.wbtree.internals
  (:use #:common-lisp)
  (:local-nicknames (#:api #:deterministic-arts.wbtree))
  (:export #:check-invariants))
