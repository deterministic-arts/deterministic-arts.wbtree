
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
   
## Functions

 - Function `wbtree:ceiling-key` _object_ `&optional` _default_ &rarr; _key_ _indicator_
 
 - Function `wbtree:ceiling-node` _object_ &rarr; _node_
 
 - Function `wbtree:compare-from-lessp` _predicate_ &rarr; _comparator_
 
 - Function `wbtree:compare-reals` _number1_ _number2_ &rarr; _ordering_
 
 - Function `wbtree:compare-strings` _string1_ _string2_ `&key` _start1_ _end1_ _start2_ _end2_ &rarr; _ordering_
 
 - Function `wbtree:correlate-nodes` _function_ _object1_ _object2_ `&rest` _iterator-options_ &rarr; _unspecific_

 - Function `wbtree:difference` _object1_ _object2_ `&key` _test_ &rarr; _new-object_

 - Function `wbtree:equal` _object1_ _object2_ `&key` _test_ &rarr; _boolean_

 - Function `wbtree:find` _key_ _object_ `&optional` _default_ &rarr; _value_ _indicator_

 - Function `wbtree:find-node` _key_ _object_ &rarr; _node_

 - Function `wbtree:floor-key` _object_ `&optional` _default_ &rarr; _key_ _indicator_

 - Function `wbtree:floor-node` _object_ &rarr; _node_

 - Function `wbtree:intersection` _object1_ _object2_ `&key` _combiner_ _test_ &rarr; _new-object_

 - Function `wbtree:maximum-key` _object_ `&optional` _default_ &rarr; _key_ _indicator_

 - Function `wbtree:maximum-node` _object_ &rarr; _node_
 
 - Function `wbtree:minimum-key` _object_ `&optional` _default_ &rarr; _key_ _indicator_
 
 - Function `wbtree:minimum-node` _object_ &rarr; _node_
 
 - Function `wbtree:node-iterator` _object_ `&key` _from-end_ _from_ _to_ _above_ _below_ _range_ &rarr; _iterator_
 
 - Function `wbtree:remove` _key_ _object_ &rarr; _new-object_ _change_ 
   
 - Function `wbtree:union` _object1_ _object2_ `&key` _combiner_ _test_ &rarr; _new-object_
 
 - Function `wbtree:update` _key_ _value_ _object_ `&key` _test_ &rarr; _new-object_ _change_

## Macros

 - Macro `wbtree:define` _name_ `&body` _description_ 
 
 - Macro `wbtree:do` `(`_bindings_ _object_ `&rest` _options_ `)` `&body` _body_
 
 
