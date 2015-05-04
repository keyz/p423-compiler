#|

A14 - Apr 18, 2014

pass: uncover-assigned

Since the same variable can be accessed by more than one procedure, we need to place the values of each that is assigned in some location shared by the procedures, so that the effect of an assignment by one procedure can be observed by the others. Identifying the set of assigned variables is the first step in this process and is the job of uncover-assigned.

This pass locates all assigned variables and records each within the lambda, let, or letrec form that binds it. An assigned variable is one that appears somewhere within its scope on the left-hand side of a set! expression. To record this information, the body of each lambda, let, and letrec expression is wrapped in an assigned form listing the assigned variables bound by the expression. The assigned form is inserted even if the list of assigned variables is empty.

Input:
Prog      ::= Expr
Expr      ::= (quote Immediate)                  
            | (let ([UVar Expr] *) Body)
            | (letrec ([UVar Expr] *) Body)
            | (lambda (UVar *) Body)
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (set! UVar Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar
Body      ::= Expr
Datum     ::= Immediate | (Datum *) | #(Datum *)
Immediate ::= fixnum | () | #t | #f

Output:
Prog      ::= Expr
Expr      ::= (quote Immediate)                  
            | (let ([UVar Expr] *) Body)
            | (letrec ([UVar Expr] *) Body)
            | (lambda (UVar *) Body)
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (set! UVar Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar
Body      ::= (assigned (UVar *) Expr)            ;; mod
Datum     ::= Immediate | (Datum *) | #(Datum *)
Immediate ::= fixnum | () | #t | #f

|#

(library (Compiler uncover-assigned)
  (export uncover-assigned)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common))

  (define-who uncover-assigned
    (define (Expr expr)
      (match expr
        [(quote ,d) (guard (immediate-with-d? d)) ;; matchs Immediate + Datum
         (values `(quote ,d) '())] ;; nothing
        [(let ([,uvar* ,[exp* av*]] ...) ,[exp av])
         (let ([set (hintersection uvar* av)]) ;; (I uvar* av)
           (values
            `(let ([,uvar* ,exp*] ...)
               (assigned ,set ,exp))
            (hunion (apply hunion av*) (hdifference av uvar*))))] ;; (U (U av* ...) (- av uvar*))
        [(letrec ([,uvar* ,[exp* av*]] ...) ,[exp av])
         (let ([set (hintersection uvar* (hunion av (apply hunion av*)))])
           ;; (I uvar* (U av (U av* ...))
           (values
            `(letrec ([,uvar* ,exp*] ...)
               (assigned ,set ,exp))
            (hdifference (hunion av (apply hunion av*)) uvar*)))] ;; (- (U av (U av* ...)) uvar*)
        [(lambda (,uvar* ...) ,[exp av])
         (let ([set (hintersection uvar* av)]) ;; (I uvar* av)
           (values
            `(lambda (,uvar* ...)
               (assigned ,set ,exp))
            av))] ;; av
        [(if ,[test av1] ,[then av2] ,[else av3])
         (values
          `(if ,test ,then ,else)
          (hunion av1 av2 av3))] ;; (U av1 av2 av3)
        [(begin ,[exp* av*] ... ,[exp av])
         (values
          `(begin ,exp* ... ,exp)
          (hunion av (apply hunion av*)))] ;; (U av (U av* ...))
        [(set! ,uvar ,[exp av])
         (values
          `(set! ,uvar ,exp)
          (hunion `(,uvar) av))] ;; (U (uvar) av)
        [(,prim ,[exp* av*] ...) (guard (prim? prim))
         (values
          `(,prim ,exp* ...)
          (apply hunion av*))] ;; (U av* ...)
        [(,[rator av] ,[rand* av*] ...)
         (values
          `(,rator ,rand* ...)
          (hunion av (apply hunion av*)))] ;; (U av (U av* ...))
        [,uvar (guard (uvar? uvar)) (values uvar '())] ;; nothing
        [,el (errorf who "Invalid Expr ~s" el)]))

    (lambda (prog)
      (let-values ([(exp _) (Expr prog)])
        exp)))
  )

