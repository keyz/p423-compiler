#|

A13 - Apr 17, 2015

pass: optimize-direct-call

all anonymous functions calls (possibly created by a macro expander) become let bindings
- e.g. ((lambda (x) x) 3) -> (let ([x 3]) x)

why?
- avoids an unecessary heap allocation, an indirect jump, and, when there are free variables in the body, some additional indirect memory references. If the expression is evaluated frequently, the savings from this nearly trivial optimization can be significant

Input & Output Grammar:
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

|#

(library (Compiler optimize-direct-call)
  (export optimize-direct-call)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common))

  (define-who optimize-direct-call
    
    (define (optimize expr)
      (match expr
        [(lambda (,uvar* ...) ,[exp])
         `(lambda (,uvar* ...) ,exp)]
        [((lambda (,uvar* ...) ,[exp]) ,[rand*] ...)
         (if (= (length rand*) (length uvar*)) ;; lengths match?
             `(let ([,uvar* ,rand*] ...) ,exp)
             `((lambda (,uvar* ...) ,exp) ,rand* ...))]
        [(quote ,i) (guard (immediate? i))
         `(quote ,i)]
        [(let ([,uvar* ,[exp*]] ...) ,[exp])
         `(let ([,uvar* ,exp*] ...) ,exp)]
        [(letrec ([,uvar* ,[lamb*]] ...) ,[exp])
         `(letrec ([,uvar* ,lamb*] ...) ,exp)]
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
      (optimize prog)))
  )



