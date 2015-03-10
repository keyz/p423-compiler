#|

A8 - Mar 6, 2015

pass: remove-complex-opera*

This pass removes nested primitive calls from within procedure calls and other primitive calls, making the argument values "trivial".

The subexpressions of primitive calls and procedure calls are now Triv expressions rather than Value expressions. In order to carry this out, each nontrivial Value must be assigned outside of the call to a fresh unique variable.

The set of new unique variables introduced during this process must be added to the locals list enclosing the body.

input grammar changes:
Tail    ::= (alloc Value)             ;; a8n
          | (mref  Value Value)       ;; a8n

Effect  ::= (mset! Value Value Value) ;; a8n

Value   ::= (alloc Value)             ;; a8n
          | (mref  Value Value)       ;; a8n

output grammar:
Program ::= (letrec ([Label (lambda (UVar *) Body)] *) Body)
Body    ::= (locals (UVar *) Tail)
Tail    ::= (if Pred Tail Tail)
          | (begin Effect * Tail)
          | (alloc Triv)              ;; a8n, mod
          | (mref Triv Triv)          ;; a8n, mod
          | (Binop Triv Triv)         ;;      mod
          | (Triv Triv *)             ;;      mod
          | Triv
Pred    ::= (true)
          | (false)
          | (if Pred Pred Pred)
          | (begin Effect * Pred)
          | (Relop Triv Triv)         ;;      mod
Effect  ::= (nop)
          | (set! UVar Value)
          | (mset! Triv Triv Triv)    ;; a8n, mod
          | (if Pred Effect Effect)
          | (begin Effect * Effect)
          | (Triv Triv *)             ;;      mod
Value   ::= (if Pred Value Value)
          | (begin Effect * Value)
          | (alloc Triv)              ;; a8n, mod
          | (mref Triv Triv)          ;; a8n, mod
          | (Binop Triv Triv)         ;;      mod
          | (Triv Triv *)             ;;      mod
          | Triv
Triv    ::= UVar | Integer | Label

|#

(library (Compiler remove-complex-opera*)
  (export remove-complex-opera*)
  (import (chezscheme) (Framework helpers) (Framework match))

  (define-who remove-complex-opera*
    
    (define (Body bd)
      
      (define new-local* '())
      
      (define (new-t)
        (let ([t (unique-name 't)])
          (set! new-local* (cons t new-local*))
          t))
      
      (define (trivialize-call expr*)
        (let-values ([(call set*) (break-down-expr* expr*)])
          (make-begin `(,@set* ,call))))
      
      (define (break-down-expr* expr*)
        (match expr*
          [() (values '() '())]
          [(,s . ,[rest* set*]) 
           (guard (simple? s)) ;; now captures alloc, mref, mset
           (values `(,s ,rest* ...) set*)]
          [(,[Value -> expr] . ,[rest* set*])
           (let ([t (new-t)]) 
             (values `(,t ,rest* ...) `((set! ,t ,expr) ,set* ...)))]
          [,expr* (errorf who "invalid Expr ~s" expr*)]))
      
      (define (simple? x) ;; 
        (or (uvar? x) (label? x) (and (integer? x) (exact? x))
            (memq x '(+ - * logand logor sra)) (memq x '(= < <= > >=))

            (ormap (lambda (s) (eq? s x)) '(alloc mref mset!)) ;; a8n
            ))
      
      (define (triv? x) (or (uvar? x) (int64? x) (label? x)))
      
      (define (Value val) ;; + (alloc Value) | (mref Value Value)
        (match val
          [(alloc ,v) (trivialize-call `(alloc ,v))] ;; a8n
          [(mref ,v1 ,v2) (trivialize-call `(mref ,v1 ,v2))] ;; a8n
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[val])
           (make-begin `(,ef* ... ,val))]
          [(,binop ,x ,y)
           (guard (memq binop '(+ - * logand logor sra)))
           (trivialize-call `(,binop ,x ,y))]
          [,tr (guard (triv? tr)) tr]
          [(,rator ,rand* ...) (trivialize-call `(,rator ,rand* ...))]
          [,val (errorf who "invalid Value ~s" val)]))
      
      (define (Effect ef) ;; + (mset! Value Value Value)
        (match ef
          [(mset! ,v1 ,v2 ,v3) ;; a8 new
           (trivialize-call `(mset! ,v1 ,v2 ,v3))] 
          [(nop) '(nop)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[ef*] ... ,[ef])
           (make-begin `(,ef* ... ,ef))]
          [(set! ,var ,[Value -> val]) `(set! ,var ,val)]
          [(,rator ,rand* ...) (trivialize-call `(,rator ,rand* ...))]
          [,ef (errorf who "invalid Effect ~s" ef)]))
      
      (define (Pred pr)
        (match pr
          [(true) '(true)]
          [(false) '(false)]
          [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
          [(,relop ,x ,y)
           (guard (memq relop '(< <= = >= >)))
           (trivialize-call `(,relop ,x ,y))]
          [,pr (errorf who "invalid Pred ~s" pr)]))
      
      (define (Tail tail) ;; + (alloc Value) | (mref Value Value)
        (match tail
          [(alloc ,v) (trivialize-call `(alloc ,v))]
          [(mref ,v1 ,v2) (trivialize-call `(mref ,v1 ,v2))]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
          [(,binop ,x ,y)
           (guard (memq binop '(+ - * logand logor sra)))
           (trivialize-call `(,binop ,x ,y))]
          [(,rator ,rand* ...) (trivialize-call `(,rator ,rand* ...))]
          [,tr (guard (triv? tr)) tr]
          [,tail (errorf who "invalid Tail ~s" tail)]))

      (match bd
        [(locals (,local* ...) ,[Tail -> tail])
         `(locals (,local* ... ,new-local* ...) ,tail)]
        [,bd (errorf who "invalid Body ~s" bd)]))

    (lambda (x)
      (match x
        [(letrec ([,label* (lambda (,fml** ...) ,[Body -> bd*])] ...) 
           ,[Body -> bd])
         `(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)]
        [,x (errorf who "invalid Program ~s" x)])))

  )
