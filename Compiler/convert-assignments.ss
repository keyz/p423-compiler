#|

A14 - Apr 18, 2014

pass: convert-assignments
This pass eliminates set! expressions and assigned variables.
1. introduces a let expression binding each assigned variable to a freshly allocated pair whose car is the original value of the variable
2. replaces each reference to an assigned variable with a call to car
3. replaces each assignment with a call to set-car!

1.
(assigned (x ...) expr) ->
(let ((x (cons t (void))) ...) expr)

and replace the binding of x by the enclosing lambda or let expression with an identical binding for t, where each t is a new unique name.

2.
Replace each reference x to an assigned variable with (car x)

3.
Replace each assignment (set! x e)

with (set-car! x e)

Input:
Prog      ::= Expr
Expr      ::= (quote Immediate)                  
            | (let ([UVar Expr] *) Body)
            | (letrec ([UVar Lamb] *) Body)        
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (set! UVar Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar
Lamb      ::= (lambda (UVar *) Body)
Body      ::= (assigned (UVar *) Expr)          
Datum     ::= Immediate | (Datum *) | #(Datum *)
Immediate ::= fixnum | () | #t | #f

Output:
Prog      ::= Expr
Expr      ::= (quote Immediate)                  
            | (let ([UVar Expr] *) Expr)         ;; mod
            | (letrec ([UVar Lamb] *) Expr)      ;; mod 
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar
Lamb      ::= (lambda (UVar *) Expr)             ;; mod
Datum     ::= Immediate | (Datum *) | #(Datum *)
Immediate ::= fixnum | () | #t | #f

|#



(library (Compiler convert-assignments)
  (export convert-assignments)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common))

  (define-who convert-assignments

    (define foldl
      (lambda (f n ls)
        (cond
         [(null? ls) n]
         [else (foldl f (f n (car ls)) (cdr ls))])))

    (define (Expr as-ls)
      (lambda (e)
        (match e
          [(quote ,d) (guard (immediate-with-d? d))
           `(quote ,d)]
          [(let ([,uvar* ,[expr*]] ...)
             (assigned ,asv*
               ,[(Expr (append asv* as-ls)) -> expr]))
           (let-values ([(new-uvar* binds*) (gen-bindings uvar* asv*)])
             (cond
              [(null? binds*)
               `(let ([,new-uvar* ,expr*] ...) ,expr)]
              [else
               `(let ([,new-uvar* ,expr*] ...) (let ,binds* ,expr))]))]
          [(letrec ([,uvar* ,[lamb*]] ...) ,[expr])
           `(letrec ([,uvar* ,lamb*] ...) ,expr)]
          [(lambda (,uvar* ...)
             (assigned ,asv*
               ,[(Expr (append asv* as-ls)) -> expr]))
           (let-values ([(new-uvar* binds*) (gen-bindings uvar* asv*)])
             (cond
              [(null? binds*)
               `(lambda (,new-uvar* ...) ,expr)]
              [else
               `(lambda (,new-uvar* ...) (let ,binds* ,expr))]))]
          [(if ,[test] ,[then] ,[else])
           `(if ,test ,then ,else)]
          [(begin ,[expr*] ... ,[expr])
           (make-begin `(,expr* ... ,expr))]
          [(set! ,uvar ,[expr])
           (if (memq uvar as-ls)
               `(set-car! ,uvar ,expr)
               `(set! ,uvar ,expr))]
          [(,prim ,[expr*] ...) (guard (prim? prim))
           `(,prim ,expr* ...)]
          [(,[rator] ,[rand*] ...)
           `(,rator ,rand* ...)]
          [,uvar (guard (uvar? uvar))
           (if (memq uvar as-ls)
                `(car ,uvar)
                uvar)]
          [,el (errorf who "Invalid Expr ~s" el)])))

    (define (gen-bindings uvar* asv*)
      (cond
       [(null? asv*) (values uvar* '())]
       [else
        (let* ([table (map (lambda (x)
                             (cons x
                                   (unique-name (string->symbol (extract-root x)))))
                           asv*)]
               [new-uvar* (map (lambda (x)
                                 (if (assq x table)
                                     (cdr (assq x table))
                                     x))
                               uvar*)]
               [binds* (map (lambda (x)
                              `[,(car x) (cons ,(cdr x) (void))])
                            table)])
          (values new-uvar* binds*))]))
    
    (lambda (prog)
      ((Expr '()) prog)))
  )

