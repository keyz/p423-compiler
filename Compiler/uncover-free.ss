#|
A12 - Apr 09, 2015

pass: uncover-free

This pass annotates each lambda expression with a list of its free variables. It does so via a new form, free, wrapped around each lambda body. If the formals and free variables of a lambda expression are combined, they account for all of the free variables of the lambda body. The set of free variables and the set of formals are disjoint.

Input:
Prog      ::= Expr
Expr      ::= (quote Immediate)
            | (let ([UVar Expr]*) Expr)
            | (letrec ((UVar (lambda (UVar *) Expr)) *) Expr)
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar 
Immediate ::= fixnum | () | #t | #f

Output:
Prog      ::= Expr
Expr      ::= (quote Immediate)
            | (let ([UVar Expr]*) Expr)
            | (letrec ((UVar (lambda (UVar *) (free (UVar *) Expr))) *) Expr)
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar 
Immediate ::= fixnum | () | #t | #f

|#

(library (Compiler uncover-free)
  (export uncover-free)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common))

  (define-who uncover-free

    (define (immediate? imm)
      (or (memq imm '(#t #f ()))
          (and (integer? imm)
               (exact? imm)
               (fixnum-range? imm))))

    (define (Expr expr)
      (match expr
        [(quote ,i) (guard (immediate? i))
         (values `(quote ,i) '())]
        [(let ([,uvar* ,[Expr -> exp* free*]] ...) ,[Expr -> exp free])
         ;; (+ (+ free*) (- free uvar*))
         (values `(let ([,uvar* ,exp*] ...) ,exp)
                 (union (apply union free*) 
                        (difference free uvar*)))]
        [(letrec ([,uvar* ,[Lambda -> lam* flam*]] ...) ,[Expr -> exp free])
         ;; (- (+ (+ flam*) free) uvar*)
         (values `(letrec ([,uvar* ,lam*] ...) ,exp)
                 (difference (union (apply union flam*) free) uvar*))]
        [(if ,[Expr -> test ftest]
             ,[Expr -> then fthen]
             ,[Expr -> else felse])
         ;; (+ ftest fthen felse)
         (values `(if ,test ,then ,else)
                 (union ftest fthen felse))]
        [(begin ,[Expr -> exp* free*] ... ,[Expr -> exp free])
         ;; (+ (+ free*) free)
         (values `(begin ,exp* ... ,exp)
                 (union (apply union free*) free))]
        [(,prim ,[Expr -> exp* free*] ...) (guard (prim? prim))
         ;; (+ free*)
         (values `(,prim ,exp* ...)
                 (apply union free*))]
        [(,[Expr -> rator frator] ,[Expr -> rand* frand*] ...)
         ;; (+ frator frand*)
         (values `(,rator ,rand* ...)
                 (apply union `(,frator ,@frand*)))]
        [,uvar (guard (uvar? uvar))
         ;; assume everybody is free: `(,uvar)
         (values uvar `(,uvar))]
        [,el (errorf who "Invalid Expr ~s" el)]))

    (define (Lambda lam)
      (match lam
        [(lambda (,uvar* ...) ,[Expr -> exp free])
         ;; (- free uvar*)
         (let ([free-ls (difference free uvar*)])
           (values `(lambda (,uvar* ...) (free ,free-ls ,exp))
                   free-ls))]
        [,el (errorf who "Invalid Lambda ~s" el)]))
    
    (lambda (prog)
      (match prog
        [,[Expr -> x free]
         ;; free doesn't matter here
         x])))

  )
