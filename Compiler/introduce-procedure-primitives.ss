#|
A12 - Apr 09, 2015

pass: introduce-procedure-primitives

This pass completes closure conversion by introducing concrete procedure-manipulation primitives and eliminating the bind-free and closures forms.
- (make-procedure code size) allocates a new procedure (closure) with code as its code pointer and size free-variable slots.
- (procedure-code proc) extracts the code pointer from proc.
- (procedure-ref proc i) extracts the value from the ith free-variable slot of proc.
    - replaces each reference to a free variable
    - index: the ordering of free vars in the `bind-free` form
- (procedure-set! proc i val) stores val in the ith free-variable slot of proc.

- Each closures form is expanded into calls to make-procedure and procedure-set!, with one call to make-procedure for each closure and as many calls to procedure-set! as are necessary to fill each of the closures
- The procedure-set! indices to use are determined by the ordering of free variables in the closures binding.
- Procedure calls are rewritten with procedure-code used to extract the pointer to the code to be invoked.

Input:
Prog      ::= Expr
Expr      ::= (quote Immediate)
            | (let ([UVar Expr]*) Expr)
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

Output:
Prog      ::= Expr
Expr      ::= (quote Immediate)
            | (let ([UVar Expr]*) Expr)
            | (letrec ((Label (lambda (UVar *) Expr)) *) Expr)  ;; mod    
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

(library (Compiler introduce-procedure-primitives)
  (export introduce-procedure-primitives)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common))

  (define-who introduce-procedure-primitives

    (define (index x ls)
      (let loop ([x x] [ls ls] [acc 0])
        (cond
         [(null? ls) #f]
         [(eq? x (car ls)) acc]
         [else (loop x (cdr ls) (add1 acc))])))

    (define (immediate? imm)
      (or (memq imm '(#t #f ()))
          (and (integer? imm)
               (exact? imm)
               (fixnum-range? imm))))

    ;; (procedure-ref proc i)
    ;; extracts the value from the ith free-variable slot of proc.
    (define (make-ref x ls) 
      (cond
       [(index x (cdr ls)) =>
        (lambda (i)
          `(procedure-ref ,(car ls) (quote ,i)))] ;; (car ls) === cp.*
       [else x]))

    ;; (procedure-set! proc i val)
    ;; stores val in the ith free-variable slot of proc.
    (define (make-set! x free)
      (let loop ([x x] [ls free])
        (cond
         [(null? ls) '()]
         [else (cons `(procedure-set! ,x (quote ,(index (car ls) free)) ,(car ls))
                     (loop x (cdr ls)))])))
    
    (define (Expr free-ls)
      (lambda (expr)
        (match expr
          [(quote ,i) (guard (immediate? i))
           `(quote ,i)]
          [(let ([,uvar* ,[exp*]] ...) ,[exp])
           `(let ([,uvar* ,exp*] ...) ,exp)]
          [(letrec ((,label* ,[Lambda -> lam*]) ...) ,[(Closure free-ls) -> expr])
           `(letrec ([,label* ,lam*] ...) ,expr)]
          [(if ,[test] ,[then] ,[else])
           `(if ,test ,then ,else)]
          [(begin ,[exp*] ... ,[exp])
           `(begin ,exp* ... ,exp)]
          [(,prim ,[exp*] ...) (guard (or (prim? prim) (label? prim))) ;; oh
           `(,prim ,exp* ...)]
          [(,[rator] ,[rator] ,[rand*] ...)
           `((procedure-code ,rator) ,rator ,rand* ...)]
          [,uvar (guard (uvar? uvar)) (make-ref uvar free-ls)]
          [,el (errorf who "Invalid Expr ~s" el)])))

    ;; Lambda doesn't need to carry a free-list
    (define Lambda 
      (lambda (lam) 
        (match lam
          [(lambda (,cp ,arg* ...)
             (bind-free (,cp ,freev* ...)
                        ,[(Expr `(,cp ,@freev*)) -> exp]))
           `(lambda (,cp ,arg* ...) ,exp)]
          [,el (errorf who "Invalid Lambda ~s" el)])))
    
    (define (Closure free-ls)
      (lambda (cls)
        (match cls
          [(closures ([,uvar* ,label* ,[(Expr free-ls) -> freev*] ...] ...)
                     ,[(Expr free-ls) -> exp])
           (let ([len* (map length freev*)]
                 [pset!* (map make-set! uvar* freev*)])
             `(let ([,uvar* (make-procedure ,label* (quote ,len*))] ...)
                (begin
                  ,pset!* ... ... ;; oh. found you.
                  ,exp)))]
          [,el (errorf who "Invalid Closure ~s" el)])))

    (lambda (prog)
      ((Expr '(?!hukarz*^)) prog)))
  )
