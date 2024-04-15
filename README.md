
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
 
   Answers a comparator function whose implementation is derived from _predicate_
   which must be a function taking two arguments, which returns true, if the first
   argument is stricly less than the second one.
 
 - Function `wbtree:compare-reals` _number1_ _number2_ &rarr; _ordering_
 
   A comparator function, that compares `real` numbers.
 
 - Function `wbtree:compare-strings` _string1_ _string2_ `&key` _start1_ _end1_ _start2_ _end2_ &rarr; _ordering_
 
   Compares _string1_ and _string2_ (or substrings as identified by the bounding
   sequence index designators _start1_ / _end1_ for _string_1 and _start2_ / _end2_
   for _string2_), and returns a negative integer, if _string1_ is considered less 
   than _string2_, a positive integer, if _string1_ is greater than _string2_, or
   0, if the portions compared are equal. The comparison is case-sensitive.
 
 - Function `wbtree:correlate-nodes` _function_ _object1_ _object2_ `&rest` _iterator-options_ &rarr; _unspecific_

   Correlates the nodes of search trees _object1_ and _object2_ by their keys, and
   invokes the given function once per unique key found. The _function_ must accept
   two arguments, both of which will either be tree nodes or `nil` when invoked.
   
   In each invocation, the first argument will be the node from tree _object1_, and
   the second argument will be the node from tree _object2_. If a key is present only
   in one of the search trees, the corresponding argument for the other tree will be
   `nil`.
   
   Iteration order and range/subset selection can be controlled by the remaining
   arguments _iterator-options_. See `wbtree:iterator` for a description of supported
   options and their significance.

 - Function `wbtree:difference` _object1_ _object2_ &rarr; _new-object_

   Answers a copy of the search tree _object1_ from which all entries have been
   removed, whose keys match one of the keys in search tree _object2_.

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

   Answers a search tree of the same variety as _object1_ and _object2_, whose
   entries are formed by intersecting the key set of _object1_ with the key set
   of _object2_. The associated value is computed by invoking _combiner_ with
   three arguments: the key in question, its associated value in _object1_, and
   its associated value in _object2_. Whatever the _combiner_ returns will then
   be used as the associated value in the intersection.
   
   The default _combiner_ always simply picks the value from _object2_.

 - Function `wbtree:key` _node_ &rarr; _value_
 
   Answers the value of the key field of _node_. If _node_ is an empty tree node,
   the key is `nil` by definition (even for search tree subtypes for which `nil` is
   not actually a suitable key value.)
   
 - Function `wbtree:left` _node_ &rarr; _child_
 
   Answers the tree node that is the left child of _node_. The empty tree node has
   no left child and this function returns `nil` instead (it is the only node for which
   this function returns anything else but a `wbtree:node`)

 - Function `wbtree:maximum-key` _object_ `&optional` _default_ &rarr; _key_ _indicator_

   Answers the largest key in search tree _object_. If _object_ is empty, returns
   _default_ instead. The secondary value _indicator_ is true, if the first value
   was found in the tree, and false, if it has been defaulted.

 - Function `wbtree:maximum-node` _object_ &rarr; _node_
 
   Answers the node with the largest key present in search tree _object_. If the
   tree is empty, answer `nil` instead.
 
 - Function `wbtree:minimum-key` _object_ `&optional` _default_ &rarr; _key_ _indicator_
 
   Answers the smallest key in search tree _object_. If _object_ is empty, returns
   _default_ instead. The secondary value _indicator_ is true, if the first value
   was found in the tree, and false, if it has been defaulted.
 
 - Function `wbtree:minimum-node` _object_ &rarr; _node_
 
   Answers the node with the smallest key present in search tree _object_. If the
   tree is empty, answer `nil` instead.
 
 - Function `wbtree:modify` _key_ _function_ _object_ &rarr; _new-object_ _old-node_ _new-node_
 
   Computes a modification to the search tree _object_. Invokes _function_ on 
   the node matching _key_ (or `nil` if the key is not currently present in the
   search tree.) The function must accept two arguments. The first one will be 
   the value of _key_, and the second one will be the node found or `nil`, if 
   the key is not present.
   
   The function must return two values, _action_ and _value_. The _action_ 
   controls the modification to be made to _object_ and must be one of
   
    - `nil` do not modify anything
    - `:remove` remove the node from the tree; _value_ is ignored
    - `:update` update the key's associated value to _value_
    
   In theory, this function could be used to implement `wbtree:update` as well
   as `wbtree:remove`. The actual implementations are slightly more efficient,
   though.
 
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
   the iteration. It must be a function of a single argument, a key. The function
   returns a negative integer, if the key is too small to be included in the result, and 
   a positive integer, if the key is too large. If the function returns 0, the node associated 
   with that key will be part of the iterator's result.
   
   Besides supplying a _comparator_ function, the caller can control the subset using any
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
 
   Answers a search tree of the same variety as _object1_ and _object2_, whose
   entries are the entries of _object1_ and _object2_ combined. For keys only present
   in one of the input trees, the associated value found in that tree is used. For
   keys present in both, the `combiner` function is invoked with three arguments:
   the key in question, the associated value in _object1_, and the associated value
   in _object2_. The return value is then used as associated value in the result 
   produced by this function.
   
   The default _combiner_ is a function, which always picks the value found in _object2_.
 
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
   
# Comparisons

There are other libraries with similar goals available on Quicklisp. 

## FSet

The `FSet` library by Scott L. Burson has a similar feature set to this library. The
main trade-off that does not work well for me is, that all comparisons are performed
through a single global `compare` function. That means, that it is hard to have 
different behaviours for some underlying key type in the same lisp image (e.g.,
one tree with strings as keys that uses case-sensitive comparisons, and another one
that uses case-insensitive comparisons.) To achieve this, the application would have
to introduce a "wrapper" type

```common-lisp
(defstruct (case-insensitive-string (:copier nil) (:conc-name cistr-) (:constructor make-cistr (value)))
  (value "" :type string :read-only t))

(defmethod compare ((s1 case-insensitive-string) (s2 case-insensitive-string))
  (cond
    ((string-lessp s1 s2) :less)
    ((string-greaterp s1 s2) :greater)
    (t :equal)))
```

which I am not particularly fond of.

## Darts.Lib.WBTree

Is the predecessor to this library by the same author (me...) It makes similar
trade-offs to this library. The most important difference may be, that the old
library uses an is-strictly-less-than predicate to order the keys, whereas this
library uses "comparator" functions instead (see above.)

Darts.lib.wbtree is now officially deprecated for new projects, though. Since
I still have internal consumers for that library, it may receive maintenance
updates for some time. But new features will be added here.

## Others

There was at least one other library I had once found, that stored the comparison
function in every node if I remember correctly, and hence was more flexible than 
this one on paper. Unfortunately, I have never found a need for that much flexibility,
and prefer to not blow up the size of tree nodes even more.
 
# Example

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

# TODOs

 - There is currently a lot of code duplication in this library. The main
   reason for that is the need to allow internal helpers to access the comparison 
   and constructor functions, preferrably without requiring another round of
   generic dispatch.

   Right now, I decide to live with this. Depending on whether this turns out
   to be a maintenance burden, I may have to clean things up later, though.
   
