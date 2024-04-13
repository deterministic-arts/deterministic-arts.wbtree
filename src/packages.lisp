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

(defpackage #:deterministic-arts.wbtree
  (:use)
  (:export #:node #:nodep #:key #:value #:left #:right #:size #:emptyp #:find #:update
           #:find-node #:remove #:fold #:map #:union #:intersection #:difference #:iterator
           #:define #:minimum-node #:maximum-node #:floor-node #:ceiling-node #:minimum-key
           #:maximum-key #:floor-key #:ceiling-key #:equal #:node-iterator #:do #:correlate-nodes
           #:compare-from-lessp #:compare-strings #:compare-reals #:next-node #:reduce
           #:reduce-nodes #:modify))

(defpackage #:deterministic-arts.wbtree.internals
  (:use #:common-lisp)
  (:local-nicknames (#:api #:deterministic-arts.wbtree))
  (:export #:check-invariants #:compare-strings))
