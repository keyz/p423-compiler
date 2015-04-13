#|

A12 - Apr 12, 2015
modified `common.ss` to support closure & first-class procedures:
- primitives += make-procedure procedure-code procedure-ref procedure-set! procedure?
- value-prim? += make-procedure procedure-code procedure-ref
- pred-prim? += procedure?
- effect-prim? += procedure-set!

A11 - Apr 03, 2015

pass: normalize-context

The goal of this pass is to recognize the three distinct contexts below and to rewrite the code so that each kind of expression appears only in contexts that are appropriate for that expression.

On input to this pass, any expression can appear in any of three contexts:
1. effect, where the resulting value is not needed, e.g., all but the last subexpression of a begin
2. predicate, where the expression determines flow of control, e.g., the test part of an if
3. value, where the value is needed, e.g., the right-hand side of a let.

Summary
- void from value-prim: (nop)
- uvar, label: only valid in Value
- Procedure calls: not ok in Pred
- (quote foo)
  - Pred: (if (eq? foo #f) (false) (true))
  - Effect: (nop)
  - Value: (quote foo)

And:
- Constants, i.e., quote expressions, are permitted only in value context. They are unnecessary in effect context and are simply dropped, i.e., replaced with (nop), in that context. In predicate context, #f is replaced by (false), and all nonfalse constants are replaced by (true).
- Variable references and label expressions are valid only in value context.
- Procedure calls are valid in both value and effect context but not in predicate context.

Input:
Prog      ::= (letrec ((Label (lambda (UVar *) Expr)) *) Expr)
Expr      ::= (quote Immediate)
            | (let ([UVar Expr] *) Expr)
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar 
            | Label
Immediate ::= fixnum | () | #t | #f

Output:
Prog      ::= (letrec ((Label (lambda (UVar *) Value)) *) Value)
Pred      ::= (let ([UVar Value] *) Pred)
            | (true)
            | (false)
            | (if Pred Pred Pred)
            | (begin Effect * Pred)
            | (PredPrim Value *)
Effect    ::= (let ([UVar Value] *) Effect)
            | (nop)
            | (if Pred Effect Effect)
            | (begin Effect * Effect)
            | (EffectPrim Value *)
            | (Value Value *)
Value     ::= (quote Immediate)
            | (let ([UVar Value] *) Value)
            | (if Pred Value Value)
            | (begin Effect * Value)
            | (ValPrim Value *)
            | (Value Value *)
            | UVar
            | Label
Immediate ::= fixnum | () | #t | #f

|#



(library (Compiler normalize-context)
  (export normalize-context)
  (import
    (chezscheme)
    (Framework helpers)
    (Framework match)
    (Compiler common))


  (define-who normalize-context
    (define make-nopless-begin
      (lambda (x*)
        (let ([x* (remove '(nop) x*)])
          (if (null? x*)
              '(nop)
              (make-begin x*)))))
    
    (define (Immediate imm)
      (match imm
        [,x (guard (memq imm '(#t #f ()))) x]
        [,n (guard (and (integer? imm)
                        (exact? imm)
                        (fixnum-range? imm)))
            n]
        [,el (errorf who "invalid Immediate ~s" el)]))

    (define (Value value)
      (match value
        [(quote ,[Immediate -> i]) `(quote ,i)] ;; context dependent
        [(let ([,uvar* ,[Value -> bind*]] ...) ,[body])
         `(let ([,uvar* ,bind*] ...) ,body)]
        [(if ,[Pred -> test] ,[then] ,[else])
         `(if ,test ,then ,else)]
        [(begin ,[Effect -> e*] ... ,[tail])
         (make-begin `(,e* ... ,tail))]
        [(,prim ,[Value -> arg*] ...) (guard (value-prim? prim)) ;; dep
         `(,prim ,arg* ...)]
        [(,prim ,[Value -> arg*] ...) (guard (effect-prim? prim)) ;; dep
         `(begin (,prim ,arg* ...) (void))]
        [(,prim ,[Value -> arg*] ...) (guard (pred-prim? prim)) ;; dep
         `(if (,prim ,arg* ...) '#t '#f)]
        [(,[Value -> rator] ,[Value -> rand*] ...) ;; dep
         `(,rator ,rand* ...)]
        [,uvar (guard (uvar? uvar)) uvar] ;; dep
        [,lab (guard (label? lab)) lab]
        [,else (errorf who "invalid Value ~s" else)]))

    (define (Pred pred)
      (match pred
        [(quote ,[Immediate -> i])  ;; context dependent
         (if (eq? i #f) '(false) '(true))]
        [(let ([,uvar* ,[Value -> bind*]] ...) ,[body])
         `(let ([,uvar* ,bind*] ...) ,body)]
        [(if ,[Pred -> test] ,[then] ,[else])
         `(if ,test ,then ,else)]
        [(begin ,[Effect -> e*] ... ,[tail])
         (make-begin `(,e* ... ,tail))]
        [(,prim ,[Value -> arg*] ...) (guard (value-prim? prim)) ;; dep
         `(if (eq? (,prim ,arg* ...) '#f) (false) (true))]
        [(,prim ,[Value -> arg*] ...) (guard (effect-prim? prim)) ;; dep
         `(begin (,prim ,arg* ...) (true))]
        [(,prim ,[Value -> arg*] ...) (guard (pred-prim? prim)) ;; dep
         `(,prim ,arg* ...)]
        [(,[Value -> rator] ,[Value -> rand*] ...) ;; dep
         `(if (eq? (,rator ,rand* ...) '#f) (false) (true))]
        [,uvar (guard (uvar? uvar))  ;; dep
               `(if (eq? ,uvar '#f) (false) (true))]
        [,lab (guard (label? lab)) lab]
        [,else (errorf who "invalid Value ~s" else)]))

    (define (Effect effect)
      (match effect
        [(quote ,[Immediate -> i])  ;; context dependent
         '(nop)]
        [(let ([,uvar* ,[Value -> bind*]] ...) ,[body])
         `(let ([,uvar* ,bind*] ...) ,body)]
        [(if ,[Pred -> test] ,[then] ,[else])
         `(if ,test ,then ,else)]
        [(begin ,[Effect -> e*] ... ,[tail])
         (make-begin `(,e* ... ,tail))]
        [(,prim ,[Effect -> arg*] ...) (guard (value-prim? prim)) ;; dep
         (make-nopless-begin `(,arg* ...))] ;;
        [(,prim ,[Value -> arg*] ...) (guard (effect-prim? prim)) ;; dep
         `(,prim ,arg* ...)]
        [(,prim ,[Effect -> arg*] ...) (guard (pred-prim? prim)) ;; dep
         (make-nopless-begin `(,arg* ...))] ;; 
        [(,[Value -> rator] ,[Value -> rand*] ...) ;; dep
         `(,rator ,rand* ...)]
        [,uvar (guard (uvar? uvar))  ;; dep
               '(nop)] ;; hmm.
        [,lab (guard (label? lab)) lab]
        [,else (errorf who "invalid Value ~s" else)]))


    (lambda (prog)
      (match prog
        [(letrec ([,label* (lambda (,uvar* ...) ,[Value -> val*])] ...) ,[Value -> val])
         `(letrec ([,label* (lambda (,uvar* ...) ,val*)] ...) ,val)]
        [,else (errorf who "invalid Program ~s" else)])))

  )
