#|

A8 - Mar 6, 2015

pass: flatten-set!

This pass rewrites set! expressions as necessary to push them inside if and begin expressions so that, in the output, the right-hand-side of each set! contains neither if nor begin expressions.

Assignments are converted into a form that more closely resembles assembly instructions. 

input grammar changes:
Tail    ::= (alloc Triv)              ;; +
          | (mref Triv Triv)          ;; +

Effect  ::= (mset! Triv Triv Triv)    ;; +

Value   ::= (alloc Triv)              ;; +
          | (mref Triv Triv)          ;; +


output grammar:
Program ::= (letrec ([Label (lambda (UVar *) Body)] *) Body)
Body    ::= (locals (UVar *) Tail)
Tail    ::= (if Pred Tail Tail)
          | (begin Effect * Tail)
          | (alloc Triv)                  ;; a8 new
          | (mref Triv Triv)              ;; a8 new
          | (Binop Triv Triv)      
          | (Triv Triv *)            
          | Triv
Pred    ::= (true)
          | (false)
          | (if Pred Pred Pred)
          | (begin Effect * Pred)
          | (Relop Triv Triv)
Effect  ::= (nop)
          | (set! UVar (alloc Triv))      ;; a8n, mod
          | (set! UVar (mref Triv Triv))  ;; a8n, mod
          | (set! UVar Triv)              ;;      mod
          | (set! UVar (Binop Triv Triv)) ;;      mod
          | (set! UVar (Triv Triv *))     ;;      mod
          | (mset! Triv Triv Triv)        ;; a8n, mod
          | (if Pred Effect Effect)
          | (begin Effect * Effect)
          | (Triv Triv *)                 ;;      mod
Triv    ::= UVar | Integer | Label

|#

(library (Compiler flatten-set!)
  (export flatten-set!)
  (import (chezscheme) (Framework helpers) (Framework match))

  (define-who flatten-set!

    (define (trivialize-set! lhs rhs)
      (match rhs
        [(alloc ,t) (guard (triv? t)) ;; a8 new
         `(set! ,lhs (alloc ,t))]
        [(mref ,t1 ,t2)               ;; a8 new
         (guard (and (triv? t1) (triv? t2)))
         `(set! ,lhs (mref ,t1 ,t2))]
        [(if ,[Pred -> test] ,[conseq] ,[altern])
         `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[rhs])
         (make-begin `(,ef* ... ,rhs))]
        [(,binop ,[Triv -> x] ,[Triv -> y]) 
         (guard (memq binop '(+ - * logand logor sra)))
         `(set! ,lhs (,binop ,x ,y))]
        [,tr (guard (triv? tr)) `(set! ,lhs ,tr)]
        [(,[Effect -> v] ,[Effect -> v*] ...) ;; (Triv Triv*)
         `(set! ,lhs (,v ,v* ...))]
        [,rhs (error who "invalid set! Rhs ~s" rhs)]))

    (define (triv? x) (or (uvar? x) (int64? x) (label? x)))

    (define (binop? x) (memq x '(+ - * logand logor sra)))

    (define (Triv t) (if (triv? t) t (error who "invalid Triv ~s" t)))

    (define (Effect ef) ;; + (mset! Triv Triv Triv)
      (match ef
        [(mset! ,t1 ,t2 ,t3) ;; a8 new
         (guard (and (triv? t1) (triv? t2) (triv? t3)))
         `(mset! ,t1 ,t2 ,t3)]
        [(nop) '(nop)]
        [(if ,[Pred -> test] ,[conseq] ,[altern])
         `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[ef])
         (make-begin `(,ef* ... ,ef))]
        [(set! ,var ,val) (trivialize-set! var val)]
        ;; since we dont have the Value matcher anymore,
        ;; Effect needs to handle the binop case
        [(,binop ,[v1] ,[v2]) (guard (binop? binop)) ;; (binop Triv Triv)
         `(,binop ,v1 ,v2)]
        [,t (guard (triv? t)) t] ;; Triv
        [(,[t] ,[t*] ...) `(,t ,t* ...)] ;; (Triv Triv*)
        [,ef (error who "invalid Effect ~s" ef)]))

    (define (Pred pr)
      (match pr
        [(true) '(true)]
        [(false) '(false)]
        [(if ,[test] ,[conseq] ,[altern])
         `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[pr])
         (make-begin `(,ef* ... ,pr))]
        [(,relop ,[Triv -> x] ,[Triv -> y])
         (guard (memq relop '(< <= = >= >)))
         `(,relop ,x ,y)]
        [,pr (error who "invalid Pred ~s" pr)]))

    (define (Tail tail) ;; (alloc Triv) | (mref Triv Triv)
      (match tail
        [(alloc ,t) (guard (triv? t))
         `(alloc ,t)] ;; a8 new
        [(mref ,t1 ,t2)
         (guard (and (triv? t1) (triv? t2)))
         `(mref ,t1 ,t2)] ;; a8 new
        [(if ,[Pred -> test] ,[conseq] ,[altern])
         `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[tail])
         (make-begin `(,ef* ... ,tail))]
        [(,binop ,[Triv -> x] ,[Triv -> y])
         (guard (memq binop '(+ - * logand logor sra)))
         `(,binop ,x ,y)]
        [(,[Triv -> rator] ,[Triv -> rand*] ...) `(,rator ,rand* ...)]
        [,tr (guard (triv? tr)) tr]
        [,tail (error who "invalid Tail ~s" tail)]))

    (define (Body bd)
      (match bd
        [(locals (,uvar* ...) ,[Tail -> tail])
         `(locals (,uvar* ...) ,tail)]
        [,bd (error who "invalid Body ~s" bd)]))

    (lambda (x)
      (match x
        [(letrec ([,label* (lambda (,fml** ...) ,[Body -> bd*])] ...)
           ,[Body -> bd])
         `(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)]
        [,x (error who "invalid Program ~s" x)])))

  )
