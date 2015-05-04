#|

A13 - Apr 17, 2015

pass: remove-anonymous-lambda

This pass introduces a unique name for each lambda expression that does not appear as the right-hand side of a let or letrec expression.
(lambda (formal ...) body) -> 
(letrec ([anon.n (lambda (formal ...) body)])
  anon.n)

Although optimize-direct-call, which runs before this pass, eliminates anonymous lambda expressions that appear in operator position, we do not rely on this here since optimize-direct-call is an optimization pass and might not be enabled

Input Grammar:
Prog      ::= Expr
Expr      ::= (quote Immediate)
            | (let ([UVar Expr] *) Expr)
            | (letrec ([UVar Lamb] *) Expr)
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | Lamb
            | UVar
Lamb      ::= (lambda (UVar *) Expr)
Immediate ::= fixnum | () | #t | #f

Output Grammar:
Prog         ::= Expr
Expr         ::= (quote Immediate)
               | (let ([UVar LambdaOrExpr] *) Expr) ;; mod
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

|#

(library (Compiler remove-anonymous-lambda)
  (export remove-anonymous-lambda)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common))

  (define-who remove-anonymous-lambda
    
    (define (remove expr)
      (match expr
        [(lambda (,uvar* ...) ,[exp])
         (let ([anon (unique-name 'anon)])
           `(letrec ([,anon (lambda (,uvar* ...) ,exp)])
              ,anon))]
        [(quote ,i) (guard (immediate? i))
         `(quote ,i)]
        [(let ([,name* (lambda (,uvar* ...) ,[exp*])] ...) ,[exp]) ;; catch early
         `(let ([,name* (lambda (,uvar* ...) ,exp*)] ...) ,exp)]
        [(let ([,uvar* ,[exp*]] ...) ,[exp]) ;; exp*: non-lambda case
         `(let ([,uvar* ,exp*] ...) ,exp)]
        [(letrec ([,name* (lambda (,uvar* ...) ,[exp*])] ...) ,[exp])
         `(letrec ([,name* (lambda (,uvar* ...) ,exp*)] ...) ,exp)]
        [(if ,[test] ,[then] ,[else])
         `(if ,test ,then ,else)]
        [(begin ,[exp*] ... ,[exp])
         `(begin ,exp* ... ,exp)]
        [(,prim ,[exp*] ...) (guard (prim? prim))
         `(,prim ,exp* ...)]
        [(,[rator] ,[rand*] ...)
         `(,rator ,rand* ...)]
        [,uvar (guard (uvar? uvar)) uvar]
        [,el (errorf who "Invalid Expr ~s" el)]))

    (lambda (prog)
      (remove prog)))
  )




