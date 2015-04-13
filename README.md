# p423-compiler

Archive repo of a Scheme to x86-64 compiler in Scheme.

Work in progress as of April 12, 2015 (Assignment 12): closure and first-class procedures. The supported BNF is listed as below.

```
Prog      ::= Expr
Expr      ::= (quote Immediate)
            | (let ([UVar Expr] *) Expr)
            | (letrec ((UVar (lambda (UVar *) Expr)) *) Expr)
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar
Immediate ::= fixnum | () | #t | #f
```


For more information, please refer to the [course page](http://homes.soic.indiana.edu/classes/spring2015/csci/p423-rrnewton/) of CSCI-P 423 at Indiana University. Unless otherwise specified, I only own the files under [`Compiler`](https://github.com/keyanzhang/p423-compiler/tree/master/Compiler).
