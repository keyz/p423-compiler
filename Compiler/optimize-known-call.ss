#|

A13 - Apr 17, 2015

pass: optimize-known-call

indirect call
- rather than calling the closed procedure directly, the code calls indirect through the closure
- suitable for anonymous calls, calls in which the call site does not know exactly which procedure is being invoked

This pass therefore converts calls to closures-bound variables into direct calls to the corresponding closed procedures. If f.n is a closures-bound variable, and f$n is the label of the corresponding closed procedure, then

(f.n e1 ...) ->
(f$n e1 ...)

Input & Output:
Prog      ::= Expr
Expr      ::= (quote Immediate)
            | (let ([UVar Expr] *) Expr)
            | (letrec ((Label (lambda (UVar *)
                                (bind-free (UVar *) Expr))) *)
                (closures ((UVar Label UVar *) *) Expr))       
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

(library (Compiler optimize-known-call)
  (export optimize-known-call)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common))

  (define-who optimize-known-call

    (define (lookup x table)
      (cond
       [(assq x table) => cadr]
       [else #f]))
    
    (define (Expr table)
      (lambda (expr)
        (match expr
          [(quote ,i) (guard (immediate? i)) `(quote ,i)]
          [(let ([,uvar* ,[expr*]] ...) ,[expr])
           `(let ([,uvar* ,expr*] ...) ,expr)]
          [(letrec ([,label* (lambda (,arg* ...)
                               (bind-free (,uvar* ...) ,[expr*]))] ...)
             (closures ([,cvar* ,clabel* ,cvar** ...] ...) ,[expr]))
           (let* ([binds `([,cvar* ,clabel*] ...)]
                  [table (append binds table)]
                  [expr* (map (Expr table) expr*)] ;; hmm. any chance of getting new bindings here?
                  [expr ((Expr table) expr)])
             `(letrec ([,label* (lambda (,arg* ...)
                                  (bind-free (,uvar* ...) ,expr*))] ...)
                (closures ([,cvar* ,clabel* ,cvar** ...] ...) ,expr)))]
          [(if ,[test] ,[then] ,[else])
           `(if ,test ,then ,else)]
          [(begin ,[expr*] ... ,[expr])
           `(begin ,expr* ... ,expr)]
          [(,prim ,[expr*] ...) (guard (prim? prim))
           `(,prim ,expr* ...)]
          [(,[rator] ,[rand*] ...)
           (let ([rator (or (lookup rator table) rator)])
             `(,rator ,rand* ...))]
          [,uvar (guard (uvar? uvar)) uvar]
          [,label (guard (label? label)) label]
          [,el (errorf who "Invalid Expr ~s" el)])))
    
    (lambda (prog)
      ((Expr '()) prog)))
  )






