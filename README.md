
# Immutable Weight-Balanced Binary Search Trees

This library provides a weight-balanced binary search tree. The search
trees are immutable after construction. It is based on the paper "Implementing 
Sets Efficiently in a Functional Language" by S. Adams.

The library is the successor to the old `darts.lib.wbtree` system, available
via Quicklisp.

# Documentation

The system defines two packages, namely `deterministic-arts.wbtree` which
exports the names that make up the public API, and `deterministic-arts.wbtree.internals`
that hosts the actual implementation. Client code is expected to use only the
names exported by `deterministic-arts.wbtree`.

The following descriptions assume, that a (local or global) package nickname
`wbtree` has been established that points to `deterministic-arts.wbtree`.

## Concepts

Different from what's described in the underlying paper, this library does not
use a "strictly-less" predicate to order the keys in the search tree. Instead,
the library defines the concept of a "comparator" function, which is essentially
a function

```common-lisp
(lambda (object1 object2) ...)
```

that returns 

 - a negative integer, if `object1` is considered strictly less than `object2`
 - a positive integer, if `object1` is considered strictly greater than `object2`
 - zero, if `object1` and `object2` are equal
 
The utility function `wbtree:compare-from-lessp` can be used to derive a comparator
given a (strict) "lessp" predicate function, e.g., `(wbtree:compare-from-lessp #'string<)`
(but note, that this library provides an actual comparator function for strings, 
which is likely to be more efficient then the one that would be constructed by
this example.)

## Types

 - Type `wbtree:node` 
 
   This is an abstract structure type that acts as base type for all client 
   defined concrete search trees. There is no constructor for this kind of structure
   itself. It's main purpose is to be `:include`d into the actual node structures
   for client-defined subclasses.
   
   Client code **must not** derive subtypes (i.e., mention it in the `:include`
   option of a `defstruct` form) itself. Only `wbtree:define` must be used to derive
   new subtypes.
   
## Functions

 - Function `wbtree:ceiling-key` _key_ _object_ `&optional` _default_ &rarr; _key_ _indicator_
 
   Answers the smallest key in search tree _object_ that is greater than or equal 
   to _key_. If no such key exists, this function answers _default_. The _indicator_
   is true, if a suitable key could be found, and false otherwise.
 
 - Function `wbtree:ceiling-node` _key_ _object_ &rarr; _node_
 
   Answers the node with the smallest key in search tree _object_ that is greater 
   than or equal to _key_. If no such key exists, this function answers `nil`.
 
 - Function `wbtree:compare-from-lessp` _predicate_ &rarr; _comparator_
 
 - Function `wbtree:compare-reals` _number1_ _number2_ &rarr; _ordering_
 
 - Function `wbtree:compare-strings` _string1_ _string2_ `&key` _start1_ _end1_ _start2_ _end2_ &rarr; _ordering_
 
 - Function `wbtree:correlate-nodes` _function_ _object1_ _object2_ `&rest` _iterator-options_ &rarr; _unspecific_

 - Function `wbtree:difference` _object1_ _object2_ &rarr; _new-object_

 - Function `wbtree:emptyp` _object_ &rarr; _boolean_
 
   Answers true, if _object_ is empty, i.e., does not hold any key/value associations
   at all.

 - Function `wbtree:equal` _object1_ _object2_ `&key` _test_ &rarr; _boolean_
 
   Tests, whether search trees _object1_ and _object2_ are equal in the following 
   sense: both contain entries for the same keys, and the values associated with
   these entries compare equal under `test`. The default `test` function is `eql`.
   
   The effects are undefined, if _object1_ and _object2_ are not of the same
   concrete subtype of `wbtree:node`.

 - Function `wbtree:find` _key_ _object_ `&optional` _default_ &rarr; _value_ _indicator_

   Answers the value of the node in search tree _object_ whose key is equal to _key_ (as 
   is determined by the tree's comparator function.) If no matching node exists, this function
   returns _default_ as the primary value. The _indicator_ is true, if a suitable entry has
   been found, and false otherwise

 - Function `wbtree:find-node` _key_ _object_ &rarr; _node_
 
   Answers the node in search tree _object_ whose key is equal to _key_ (as is determined
   by the tree's comparator function.) If no matching node exists, this function answers
   `nil`

 - Function `wbtree:floor-key` _key_ _object_ `&optional` _default_ &rarr; _key_ _indicator_

   Answers the largest key in search tree _object_ that is less than or equal 
   to _key_. If no such key exists, this function answers _default_. The _indicator_
   is true, if a suitable key could be found, and false otherwise.

 - Function `wbtree:floor-node` _key_ _object_ &rarr; _node_

   Answers the node with the largest key in search tree _object_ that is less 
   than or equal to _key_. If no such key exists, this function answers `nil`.

 - Function `wbtree:intersection` _object1_ _object2_ `&key` _combiner_ &rarr; _new-object_

 - Function `wbtree:key` _node_ &rarr; _value_
 
   Answers the value of the key field of _node_. If _node_ is an empty tree node,
   the key is `nil` by definition (even for search tree subtypes for which `nil` is
   not actually a suitable key value.)
   
 - Function `wbtree:left` _node_ &rarr; _child_
 
   Answers the tree node that is the left child of _node_. The empty tree node has
   no left child and this function returns `nil` instead (it is the only node for which
   this function returns anything else but a `wbtree:node`)

 - Function `wbtree:maximum-key` _object_ `&optional` _default_ &rarr; _key_ _indicator_

 - Function `wbtree:maximum-node` _object_ &rarr; _node_
 
 - Function `wbtree:minimum-key` _object_ `&optional` _default_ &rarr; _key_ _indicator_
 
 - Function `wbtree:minimum-node` _object_ &rarr; _node_
 
 - Function `wbtree:modify` _key_ _function_ _object_ &rarr; _new-object_ _old-node_ _new-node_
 
 - Function `wbtree:next-node` _iterator_ &rarr; _node_
 
   Answers the next available node in _iterator_ and advances the iterators's
   internal state. If the iterator has reached the end of the range it iterates
   over (or has produced all nodes from the underlying tree), answers `nil`.
   
   Unlike almost all other functions exposed by this library, **this function has 
   side-effects** It modifies the iterator's internal state. Calling it multiple times 
   on an exhausted iterator is harmless, though.
 
 - Function `wbtree:node-iterator` _object_ `&key` _from-end_ _start_ _end_ _min_ _max_ _above_ _below_ _comparator_ &rarr; _iterator_
 
   Answers an iterator, that produces the nodes in search tree _object_. If _from-end_,
   the nodes will be generated in descending tree order, otherwise they will be produced
   in ascending order.
   
   The _comparator_ argument can be used to control the subset of nodes to be included in
   the iteration. It must be a function of a single argument, a tree node. The function
   returns a negative integer, if the node's key is too small to be included in the
   result, and a positive integer, if the node's key is too large. If the function
   returns 0, the node will be part of the iterator's result.
   
   Besides supplying a _range_ function, the caller can control the subset using any
   combination of the arguments _min_, _max_, _above_, _below_, _start_, and _end_ as
   follows:

     - _min_ excludes all nodes, whose keys are less than this value
     - _max_ excludes all nodes, whose keys are greater than this value
     - _above_ excludes all nodes, whose keys are less than or equal to this value
     - _below_ excludes all nodes, whose keys are greater than or equal to this value
     - _start_ if not _from-end_ behaves as _min_ otherwise behaves as _max_
     - _end_ if not _from-end_ behaves as _below_ otherwise behaves as _above_
     
   i.e., _min_, _max_, _above_, and _below_ are always defined in terms of the search
   tree's own comparator function, whereas _start_ and _end_ are sensitive to the 
   requested iteration order. 
     
   Any combination of the selection arguments can be supplied. The effective test acts
   as the conjunction of all supplied individual tests, i.e., all criteria must be 
   satisfied in order for a node to be selected. If criteria contradict each other,
   no nodes will be produced when the iteration is actually performed.
     
 - Function `wbtree:node-iterator*` _object_ `&optional` _options_ &rarr; _iterator_
 
   Like `wbtree:node-iterator` but takes its range selection and iteration order from
   the list _options_. The value of _options_ must be a property list. See `wbtree:node-iterator`
   for a description of supported options
      
 - Function `wbtree:remove` _key_ _object_ &rarr; _new-object_ _change_ 
   
   Answers a copy of search tree _object_, from which the entry for _key_ has been
   removed. If _object_ does not have an entry for _key_, this function does nothing.
   
   The primary return value _new-object_ is the updated search tree. It may be the
   same object as _object_, if _key_ is not present in _object_. The value of _change_ 
   describes the updates made as follows:
   
    - if `nil`, _key_ was not present in _object_, and hence no changes have been 
      made.
      
    - otherwise, _change_ is the `wbtree:node` instance from _object_ that has been
      removed by this operation

   Answers the tree node that is the left child of _node_. The empty tree node has
   no left child and this function returns `nil` instead (it is the only node for which
   this function returns anything else but a `wbtree:node`)

 - Function `wbtree:right` _node_ &rarr; _child_
 
   Answers the tree node that is the right child of _node_. The empty tree node has
   no children and this function returns `nil` instead (it is the only node for which
   this function returns anything else but a `wbtree:node`)

 - Function `wbtree:size` _node_ &rarr; _integer_
 
   Answers the size (i.e., number of key/value pairs it contains) of the search tree whose 
   root is _node_.
   
 - Function `wbtree:union` _object1_ _object2_ `&key` _combiner_ &rarr; _new-object_
 
 - Function `wbtree:update` _key_ _value_ _object_ `&key` _test_ &rarr; _new-object_ _change_
 
   Answers a copy of search tree _object_ in which the value associated with _key_ 
   compares equal to _value_ using _test_. If the _key_ is already associated in _object_
   with a suitable value, returns the original _object_.
   
   The primary return value _new-object_ is the updated search tree. The secondary
   value _change_ describes the updates made as follows:
   
    - if `t`, there was no entry for _key_ in _object_ at all, and a new one has been
      created and incorporated into _new-object_
      
    - if `nil`, a matching entry was already present in _object_, and _new-object_ is
      actually the same as _object_
      
    - otherwise, _change_ is the `wbtree:node` from instance from _object_, that has been 
      replaced by an updated version in _new-object_
      
   The default _test_ predicate is `eql`

 - Function `wbtree:value` _node_ &rarr; _value_
 
   Answers the value of the value field of _node_. If _node_ is an empty tree node,
   the value is `nil` by definition.

## Macros

 - Macro `wbtree:define` _name_ `&body` _description_ 

   Defines a new concrete search tree type. The _description_ forms in the body
   have the general format
   
   ```common-lisp
   (key form1 ...)
   ```
   
   and define the essential properties of the new search tree type. The following 
   options are defined:
   
    - `:predicate` _name_ 
    
      Supplies the name of the type predicate for the search tree type itself. 
      If _name_ is `nil` then no predicate is defined. If _name_ is `t`, a default
      name is generated. The symbol will be interned in the current package. The
      name is derived from the tree name by appending either `-p` or `p` depending
      on whether the type name already contains hyphens or not. Any other symbol
      is used as is.
      
      _name_ is not evaluated.
      
    - `:comparator` _function_
   
      Names the comparator function to be used. The _function_ must be something
      suitable for having `function` wrapped around it (i.e., either a symbol naming
      a function or a lambda expression.) This option is required.
     
      _function_ is not evaluated.
     
    - `:constructor` _name_
    
      Supplies a name for the constructor function with which new instances of this
      search type can be constructed. The generated function takes a single optional
      argument, a plist-style list of key/value pairs.
      
      If `nil` no constructor function is generated (but see `:constructor*` below).
      If `t` the name for the function will be derived from the tree type name by
      prepending `make-`. The symbol will be interned in the current package in this
      case. Any other symbol is used as is.
      
      _name_ is not evaluated.
      
    - `:constructor*` _name_
    
      Supplies a name for the alternative "spread" constructor function with which 
      new instances of this search type can be constructed. This one differs from the
      regular constructor in that it takes an arbitrary number of parameters via `&rest` 
      argument. As with `:constructor` the arguments are expected to be "plist"-style
      key/value pairs.
      
      If `nil` no constructor function is generated (but see `:constructor` above).
      If `t` the name for the function will be derived from the tree type name by
      prepending `make-` and appending `*`. The symbol will be interned in the current 
      package in this case. Any other symbol is used as is.
      
      The default value is `nil` (i.e., no spread constructor will be generated.)
      
      _name_ is not evaluated.
      
    - `:documentation` _string_
    
      Provides a documentation string for the new search tree type. This option is
      currently ignored when present. Future versions might provide access to the
      documentation string via `cl:documentation`.

 - Macro `wbtree:do` `(`_bindings_ _object_ `&rest` _options_ `)` `&body` _body_
 
   Iterates over all entries in the search tree produced by _object_. Iteration order
   and subset/range selection can be controlled by _options_ (see `wbtree:node-iterator`
   for details.) The default is to iterate over all nodes from the one with the smallest
   key to the one with the largested key in order.
   
   If _bindings_ is a symbol, it's taken to be a variable, and in each iteration 
   step, it will be bound to the node produced in that step. Otherwise, _bindings_
   must be a list of at least one and at most three variables:
   
   ```
   (key [value [node]])
   ```
   
   that will be bound to the node's key, value, and the node itself in each iteration
   step. 
   
   The expansion of this macro establishes an anonymous block around itself, from 
   which code in _body_ may `return` in order to stop iteration early and produce a
   result value other than `nil`. Further, the forms in _body_ are placed in a `tagbody`
   and can thus use labels and `go` to modify their control flow. 
 
## Example

```common-lisp
(wbtree:define command-map
  (:comparator (lambda (c1 c2) (signum (- (char-code c1) (char-code c2)))))
  (:constructor make-command-map)
  (:constructor* command-map)
  (:predicate command-map-p))

(defvar *global-commands* (command-map #\p 'print-report
                                       #\s 'save-data
                                       #\o 'load-data
                                       #\q 'quit-application
                                       #\space 'ignore
                                       #\newline 'ignore
                                       #\return 'ignore))
(defun main ()
  (loop
    for key = (progn (format *query-io* "~&> ") (force-output *query-io*) (read-char *query-io*))
    as command = (wbtree:find key *global-commands* 'beep-angrily)
    do (ecase command
         ((ignore))
         ((print-report) (print '(pretend me printing)))
         ((load-data) (print '(pretend me loading)))
         ((save-data) (print '(pretend me saving)))
         ((quit-application) (return))
         ((beep-angrily) (print '(pretend me beeping furiously))))))
```

(this is stupid...)
