#|

A12 - Apr 12, 2015
modified `common.ss` to support closure & first-class procedures:
- primitives += make-procedure procedure-code procedure-ref procedure-set! procedure?

A11 - Apr 03, 2015

pass: lift-letrec

This pass simply moves all letrec bindings from where they appear into a letrec expression wrapped around the outermost expression, removing all of the internal letrec expressions in the process.


Input: 
Prog      ::= Expr
Expr      ::= (quote Immediate)
            | (let ([UVar Expr] *) Expr)
            | (letrec ((Label (lambda (UVar *) Expr)) *) Expr)
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
Prog      ::= (letrec ((Label (lambda (UVar *) Expr)) *) Expr) ;; mod
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

|#


(library (Compiler lift-letrec)
  (export lift-letrec)
  (import
    (chezscheme)
    (Framework helpers)
    (Framework match)
    (Compiler common))

  (define-who lift-letrec

    (define foldl
      (lambda (f n ls)
        (cond
         [(null? ls) n]
         [else (foldl f (f n (car ls)) (cdr ls))])))

    (define top-letrecs '())

    (define (update-top-letrecs bind*)
      (set! top-letrecs
            (foldl (lambda (x y) (cons y x)) top-letrecs bind*)))
    
    (define (Immediate imm)
      (match imm
        [,x (guard (memq imm '(#t #f ()))) x]
        [,n (guard (and (integer? imm)
                        (exact? imm)
                        (fixnum-range? imm)))
            n]
        [,el (errorf who "invalid Immediate ~s" el)]))

    (define (Expr expr)
      (match expr
        [(quote ,[Immediate -> x]) `(quote ,x)]
        [(let ([,uvar ,[bind-e]] ...) ,[body-e])
         `(let ([,uvar ,bind-e] ...) ,body-e)]
        [(letrec ([,label* (lambda (,uvar* ...) ,[body*])] ...) ,[body])
         (begin
           (update-top-letrecs
            `([,label* (lambda (,uvar* ...) ,body*)] ...))
           body)]
        [(if ,[test] ,[then] ,[else])
         `(if ,test ,then ,else)]
        [(begin ,[e*] ... ,[e])
         `(begin ,e* ... ,e)]
        [(,prim ,[e*] ...) (guard (prim? prim))
         `(,prim ,e* ...)]
        [(,[rator] ,[rand*] ...)
         `(,rator ,rand* ...)]
        [,uvar (guard (uvar? uvar)) uvar]
        [,lab (guard (label? lab)) lab]
        [,el (errorf who "invalid Expr ~s" el)]))

    (lambda (prog)
      (set! top-letrecs '()) ;; hmm... should not need to manually reset this.
      (let ([new-expr (Expr prog)])
        `(letrec ,top-letrecs ,new-expr))))

  )
