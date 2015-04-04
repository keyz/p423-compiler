# p423-compiler

Archive repo of a Scheme to x86-64 compiler in Scheme.

Work in progress as of March 29, 2015 (Assignment 10): the supported BNF is listed as below.
```
Prog      ::= (letrec ((Label (lambda (UVar *) Value)) *) Value)
Value     ::= (quote Immediate)
            | (let ([UVar Value] *) Value)
            | (if Pred Value Value)
            | (begin Effect * Value)
            | (ValPrim Value *)
            | (Value Value *)
            | UVar
            | Label
Pred      ::= (let ([UVar Value] *) Pred)
            | (true)
            | (false)
            | (if Pred Pred Pred)
            | (begin Effect * Pred)
            | (PredPrim Value *))
Effect    ::= (let ([UVar Value] *) Effect)
            | (nop)
            | (if Pred Effect Effect)
            | (begin Effect * Effect)
            | (EffectPrim Value *)
            | (Value Value *))
Immediate ::= fixnum | () | #t | #f
```

For more information, please refer to the [course page](http://homes.soic.indiana.edu/classes/spring2015/csci/p423-rrnewton/) of CSCI-P 423 at Indiana University. Unless otherwise specified, I don’t own any files besides the ones under `Compiler`.
