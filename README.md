# p423-compiler

Archive repo of a Scheme to x86-64 compiler in Scheme.

Supports:
- closures
- first-class procedures
- side effects with `set!`
- closure conversion, tail-call elimination, branch predication, constant folding, dead-code elimination, and several other optimizations

The supported BNF is listed as below.
```
Program   ::= Expr
Expr      ::= Constant
            | Var
            | (quote Datum)
            | (if Expr Expr)
            | (if Expr Expr Expr)
            | (and Expr *)
            | (or Expr *)
            | (begin Expr * Expr)
            | (lambda (Var *) Expr +)
            | (let ([Var Expr] *) Expr +)
            | (letrec ([Var Expr] *) Expr +)
            | (set! Var Expr)
            | (prim Expr *)
            | (Expr Expr *)
Datum     ::= Constant | (Datum *) | #(Datum *)
Constant  ::= fixnum | () | #t | #f
Var       ::= an arbitrary symbol
```

For more information, please refer to the [course page](http://homes.soic.indiana.edu/classes/spring2015/csci/p423-rrnewton/) of CSCI-P 423 at Indiana University. Unless otherwise specified, I only own the files under [`Compiler`](https://github.com/keyanzhang/p423-compiler/tree/master/Compiler).
