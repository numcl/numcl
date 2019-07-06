#|

This file is a part of NUMCL project.
Copyright (c) 2019 IBM Corporation
SPDX-License-Identifier: LGPL-3.0-or-later

NUMCL is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

NUMCL is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
NUMCL.  If not, see <http://www.gnu.org/licenses/>.

|#


;; This file contains a symbol macro for the particularly long documentation
;; strings. In the function definitions, these are inserted by #. readmacro.

(in-package :numcl.impl)

;; not supporting ellipses for now.
;; docstring:
;; The symbol - has a special meaning that allows broadcasting,
;;  and can be used only once in each SPEC, similar to Numpy's ellipses (...).

(define-symbol-macro *einsum-documentation*
  "Performs Einstein's summation.
The SUBSCRIPT specification is significantly extended from that of Numpy
and can be seens as a full-brown DSL for array operations.

SUBSCRIPTS is a sequence of the form `(<SPEC>+ [-> <TRANSFORM>*] [-> [<SPEC>*])`.
The remaining arguments ARGS contain the input arrays and optionally the output arrays.

# SPEC

The first set of SPECs specifies the input subscripts,
and the second set of SPECs specifies the output subscripts.
Unlike Numpy, there can be multiple output subscripts:
It can performs multiple operations in the same loop, then return multiple values.
The symbol `->` can be a string and can belong to any package because it is compared by STRING=.

Each SPEC is an alphabetical string designator, such as a symbol IJK or a string \"IJK\",
where each alphabet is considered as an index. It signals a type-error when it contains any
non-alpha char. 
Note that a symbol NIL is interpreted as an empty list rather than N, I and L.

Alternatively, each SPEC can be a list that contains a list of symbols.
For example, `((i j) (j k) -> (i k))` and `(ij jk -> ik)` are equivalent.

When -> and the output SPECs are omitted, a single output is assumed and its spec is
a union of the input specs.
For example, `(ij jk)` is equivalent to `(ij jk -> ijk)`.
Note that `(ij jk)` and `(ij jk ->)` have the different meanings:
The latter sums up all elements.

# TRANSFORM

TRANSFORM is a list of element-wise operations.
The number of TRANSFORM should correspond to the number of outputs.
In each TRANSFORM, the elements in the input arrays can be referenced by $N, where N is a 1-indexed number.
Similarly the output array can be referred to by @N.

For example, `(ij ik -> (+ @1 (* $1 $2)) -> ik)` is equivalent to `(ij ik -> ik)` (a GEMM).

By default, TRANSFORM is `(+ @1 (* $1 ... $N))` for N inputs, which is equivalent to Einstein's summation.

# ARGS

The shape of each input array should unify against the corresponding input spec. For example,
with a spec IJI, the input array should be of rank 3 as well as
the 1st and the 3rd dimension of the input array should be the same.

The shape of each output array is determined by the corresponding output spec.
For example, if SUBSCRIPTS is `(ij jk -> ik)`, the output is an array of rank 2,
and the output shape has the same dimension as the first input in the first axis,
and the same dimension as the second input in the second axis.

If the output arrays are provided, their shapes and types are also checked against the
corresponding output spec.  The types should match the result of the numerical
operations on the elements of the input arrays.

The outputs are calculated in the following rule.

+ The output array types are calculated based on the TRANSFORM, and 
  the shapes are calcurated based on the SPEC and the input arrays.
+ The output arrays are allocated and initialized by zeros.
+ Einsum nests one loop for each index in the input specs.
  For example, `(ij jk -> ik)` results in a triple loop.
+ In the innermost loop, each array element is bound to `$1..$N` / `@1..@N`.
+ For each `@i`, `i`-th TRANSFORM is evaluated and assigned to `@i`.

+ If the same index appears multiple times in a single spec,
  they share the same value in each iteration.
  For example, `(ii -> i)` returns the diagonal elements of the matrix.

When TRANSFORMs are missing, it follows naturally from the default TRANSFORM values that

+ When an index used in the input spec is missing in the output spec,
  the axis is aggregated over the iteration by summation.
+ If the same index appears across the different input specs,
  the element values from the multiple input arrays are aggregated by multiplication.
  For example, `(ij jk -> ik)` will perform
  `(setf (aref a2 i k) (* (aref a0 i j) (aref a1 j k)))`
  when a0, a1 are the input arrays and a2 is the output array.

For example, (einsum '(ij jk) a b) is equivalent to:

```commonlisp
 (dotimes (i <max> <output>)
   (dotimes (j <max>)
     (dotimes (k <max>)
       (setf (aref <output> i j k) (* (aref a i j) (aref b j k))))))
```

# Performance

If SUBSCRIPTS is a constant, the compiler macro
builds an iterator function and make them inlined. Otherwise,
a new function is made in each call to einsum, resulting in a large bottleneck.
 (It could be memoized in the future.)

The nesting order of the loops are automatically decided based on the specs.
The order affects the memory access pattern and therefore the performance due to
the access locality. For example, when writing a GEMM which accesses three matrices
by `(setf (aref output i j) (* (aref a i k) (aref b k j)))`,
it is well known that ikj-loop is the fastest among other loops, e.g. ijk-loop.
EINSUM reorders the indices so that it maximizes the cache locality.
")

(define-symbol-macro *asarray-documentation*
    "Copy CONTENTS to a new array.
When CONTENTS is a multidimentional array, its elements are copied to a new array that guarantees the NUMCL assumption.
When CONTENTS is a nested sequence, it is traversed up to the depth that guarantees the sane shape for an array.
When elements are copied, it is coerced to TYPE.
When TYPE is not given, it is replaced with the float-contagion type deduced from the elements of CONTENTS.
It may return a 0-dimensional array with CONTENTS being the only element.

For example:

```lisp
;; a vector of two lists.
(asarray '((1) (1 2)))               -> #((1) (1 2))
;; a 2D array of 4 lists.
(asarray '(((1) (1 2)) ((3) (3 4)))) -> #2A(((1) (1 2)) ((3) (3 4)))

(asarray '((1 2) (3 4)))    -> #2A((1 2) (3 4))
(asarray #(#(1 2) #(3 4)))  -> #2A((1 2) (3 4))
(asarray #((1 2) (3 4)))    -> #2A((1 2) (3 4))
```

However, this behavior may not be ideal because the resulting shape could be affected by the lengths of the strings.

```lisp
(asarray #(#(1 2) #(3 4)))   -> #2A((1 2) (3 4))
(asarray #(#(1 2) #(3 4 5))) -> #(#(1 2) #(3 4 5))

(asarray #(\"aa\" \"aa\"))   -> #2A((#\a #\a) (#\a #\a))
(asarray #(\"aa\" \"aaa\"))  -> #(\"aa\" \"aaa\")
```

As a remedy to this problem, we allow TYPE to be a specifier for vector subtypes. Providing such a type specifier
will keep the leaf objects (e.g. strings) from split into individual elements.
We don't allow it to be a multidimentional array [at the moment.]

```lisp
(asarray #(#(1 2) #(3 4))   :type '(array fixnum (*))) -> #(#(1 2) #(3 4))
(asarray #(#(1 2) #(3 4 5)) :type '(array fixnum (*))) -> #(#(1 2) #(3 4 5))

(asarray #(\"aa\" \"aa\")  :type 'string)    -> #(\"aa\" \"aa\")
(asarray #(\"aa\" \"aaa\") :type 'string)    -> #(\"aa\" \"aaa\")

(asarray '((1 2) (3 4))   :type '(array fixnum (* *)))  -> error
```
")
