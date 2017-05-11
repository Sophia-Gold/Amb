# Amb

A Haskell version of LISP's `amb` operator for nondeterministic evaluation. An early predecessor to logic programming, `amb` was introduced by John McCarthy in his 1961 paper [_A Basis for a Mathematical Theory of Computation_](http://www-formal.stanford.edu/jmc/basis.html) as a means of exploring "non-computable functions."

Based on the Scheme version this DSL comes complete with its own command line REPL and only three commands.


Amb takes lists of strings, chars, ints, and floats, with an optional tag:
```
~>./Amb
AmbEval>>> amb [1,2,3]
[Amb {ambTag = Nothing, ambVal = [[1,2,3]]}]
AmbEval>>> amb foo ['a','b','c']
[Amb {ambTag = Just "foo", ambVal = ["abc"]},Amb {ambTag = Nothing, ambVal = [[1,2,3]]}]
```

Require takes parenthesized lambdas:
```
AmbEval>>> require (\x -> x /= [3,2,1])
[Require {reqTag = Nothing, reqVal = <function>}]
AmbEval>>> require foo (\y -> y == "cab" || "bac")
[Require {reqTag = Just "foo", reqVal = <function>},Require {reqTag = Nothing, reqVal = <function>}]
```

Eval, well...
```
AmbEval>>> eval
[["cab"],[[1,2,3],[2,1,3],[2,3,1],[3,1,2],[1,3,2]]]
AmbEval>>> quit
~>
````

----

_More examples coming soon..._