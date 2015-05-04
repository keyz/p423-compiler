#|

A13 - Apr 17, 2015

pass: sanitize-binding-forms

This pass processes the `let` forms such that lambda expressions appear only as the right-hand side of a letrec binding and never as the right-hand side of a let binding

This transformation would not be sound if
- variables were not uniquely named, because the lifted letrec bindings could improperly capture free references in the letrec and let right-hand sides
- in the presence of call/cc and assignments, which could be used to expose the fact that only one location is created for each generated letrec binding rather than one for each time a continuation of a let right-hand side is invoked.


We partition the bindings of each let expression into the set that bind lambda expressions and the set that do not. We lift those that bind lambda expressions out and place them in a letrec expression wrapped around what remains of the let expression. and
- it should avoid producing let and letrec expressions that bind no variables -- just suppress them
- let and letrec expressions in the input of this pass might as well be eliminated if they do not bind any variables

Input:
Prog         ::= Expr
Expr         ::= (quote Immediate)
               | (let ([UVar LambdaOrExpr] *) Expr)
               | (letrec ([UVar Lamb] *) Expr)
               | (if Expr Expr Expr)
               | (begin Expr * Expr)
               | (ValPrim Expr *)
               | (EffectPrim Expr *)
               | (PredPrim Expr *)
               | (Expr Expr *)
               | UVar
Lamb         ::= (lambda (UVar *) Expr)
LambdaOrExpr ::= Lamb | Expr
Immediate    ::= fixnum | () | #t | #f

Output:
Prog         ::= Expr
Expr         ::= (quote Immediate)
               | (let ([UVar Expr] *) Expr)                      ;; mod
               | (letrec ([UVar (lambda (UVar *) Expr)] *) Expr) ;; mod
               | (if Expr Expr Expr)
               | (begin Expr * Expr)
               | (ValPrim Expr *)
               | (EffectPrim Expr *)
               | (PredPrim Expr *)
               | (Expr Expr *)
               | UVar
Immediate    ::= fixnum | () | #t | #f

|#

(library (Compiler sanitize-binding-forms)
  (export sanitize-binding-forms)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common))

  (define-who sanitize-binding-forms
    
    (define (partition names binds body)
      (let loop ([names names]
                 [binds binds]
                 [letrec-s (hset-init)]
                 [let-s (hset-init)])
        (cond
         [(null? names)
          (let ([new-let (hset->list let-s)]
                [new-letrec (hset->list letrec-s)])
            (cond
             [(and (null? new-let) (null? new-letrec)) body] ;; no binding
             [(null? new-letrec) `(let ,new-let ,body)] ;; let only
             [(null? new-let) `(letrec ,new-letrec ,body)] ;; letrec only
             [else `(let ,new-let (letrec ,new-letrec ,body))]))] ;; both
         [(not (pair? (car binds))) ;; '() or foo: bind through let
          (loop (cdr names) (cdr binds) letrec-s (hset-push `[,(car names) ,(car binds)] let-s))]
         [(eq? 'lambda (caar binds)) ;; lambda: bind through letrec
          (loop (cdr names) (cdr binds) (hset-push `[,(car names) ,(car binds)] letrec-s) let-s)]
         [else ;; an exp: bind through let
          (loop (cdr names) (cdr binds) letrec-s (hset-push `[,(car names) ,(car binds)] let-s))])))
    
    (define (Expr expr)
      (match expr
        [(quote ,i) (guard (immediate? i)) `(quote ,i)]
        [(let () ,[expr]) expr] ;; dummy case
        [(letrec () ,[expr]) expr] ;; dummy case
        [(lambda (,uvar* ...) ,[expr]) `(lambda (,uvar* ...) ,expr)] ;; cata on the body
        [(let ([,uvar* ,[expr*]] ...) ,[expr])
         (partition uvar* expr* expr)]
        [(letrec ([,name* (lambda (,uvar* ...) ,[expr*])] ...) ,[expr])
         `(letrec ([,name* (lambda (,uvar* ...) ,expr*)] ...) ,expr)]
        [(if ,[test] ,[then] ,[else])
         `(if ,test ,then ,else)]
        [(begin ,[expr*] ... ,[expr])
         (make-begin `(,expr* ... ,expr))] ;; for nested stuffs
        [(,prim ,[expr*] ...) (guard (prim? prim))
         `(,prim ,expr* ...)]
        [(,[rator] ,[rand*] ...)
         `(,rator ,rand* ...)]
        [,uvar (guard (uvar? uvar)) uvar]
        [,el (errorf who "Invalid Expr ~s" el)]))
    
    (lambda (prog)
      (Expr prog)))
  )





