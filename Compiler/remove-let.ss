#|

A9 - Mar 28, 2015

pass: remove-let

This pass replaces each let expression in the input program with set! expressions, i.e., performs a conversion like the following:

(let ([x e] ...) body) <graphic> (begin (set! x e) ... new-body)

where new-body is the result of recursively processing body.

Input:
Prog   ::= (letrec ((Label (lambda (UVar *) Body)) *) Body)   
Body   ::= (locals (UVar *) Tail)                             
Tail   ::= (let ([UVar Value] *) Tail)
         | (if Pred Tail Tail)
         | (begin Effect * Tail)      
         | (alloc Value)
         | (mref  Value Value)
         | (Binop Value Value)
         | (Value Value *)
         | Triv
Pred   ::= (let ([UVar Value] *) Pred)
         | (true)
         | (false)
         | (if Pred Pred Pred)
         | (begin Effect * Pred)
         | (Relop Value Value)
Effect ::= (let ([UVar Value]*) Effect)
         | (nop)
         | (mset! Value Value Value)
         | (if Pred Effect Effect)
         | (begin Effect * Effect)
         | (Value Value *)
Value  ::= (let ([UVar Value] *) Value)
         | (if Pred Value Value)
         | (begin Effect * Value)
         | (alloc Value)
         | (mref  Value Value)
         | (Binop Value Value)
         | (Value Value *)
         | Triv
Triv   ::= UVar | Integer | Label

Output: UIL
Prog    ::= (letrec ([Label (lambda (UVar *) Body)] *) Body)
Body    ::= (locals (UVar *) Tail)
Tail    ::= (if Pred Tail Tail)
          | (begin Effect * Tail)
          | (alloc Value)             
          | (mref  Value Value)       
          | (Binop Value Value)
          | (Value Value *)
          | Triv
Pred    ::= (true)
          | (false)
          | (if Pred Pred Pred)
          | (begin Effect * Pred)
          | (Relop Value Value)
Effect  ::= (nop)
          | (set! UVar Value)
          | (mset! Value Value Value) 
          | (if Pred Effect Effect)
          | (begin Effect * Effect)
          | (Value Value *)
Value   ::= (if Pred Value Value)
          | (begin Effect * Value)
          | (alloc Value)           
          | (mref  Value Value)      
          | (Binop Value Value)
          | (Value Value *)
          | Triv
Triv    ::= UVar | Integer | Label

|#

(library (Compiler remove-let)
  (export remove-let)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common))

  (define-who remove-let

    (define (Body body)

      (define (Value value)
        (match value
          [(let ([,uvar* ,[Value -> value*]] ...) ,[v])
           (make-begin `((set! ,uvar* ,value*) ... ,v))] ;;
          [(if ,[Pred -> test] ,[then] ,[else]) `(if ,test ,then ,else)]
          [(begin ,[Effect -> ef*] ... ,[v])
           (make-begin `(,ef* ... ,v))] ;; 
          [(alloc ,[v]) `(alloc ,v)]
          [(,binop ,[v1] ,[v2]) (guard (binop? binop))
           `(,binop ,v1 ,v2)]
          [(,[rator] ,[rand*] ...)
           `(,rator ,rand* ...)]
          [,t (guard (triv? t)) t]
          [,else (errorf who "invalid Value ~s" else)]))

      (define (Effect effect)
        (match effect
          [(let ([,uvar* ,[Value -> value*]] ...) ,[e])
           (make-begin `((set! ,uvar* ,value*) ... ,e))] ;;
          [(nop) '(nop)]
          [(mset! ,[Value -> v1] ,[Value -> v2] ,[Value -> v3])
           `(mset! ,v1 ,v2 ,v3)]
          [(if ,[Pred -> test] ,[then] ,[else]) `(if ,test ,then ,else)]
          [(begin ,[ef*] ... ,[ef])
           (make-begin `(,ef* ... ,ef))] ;;
          [(,[Value -> rator] ,[Value -> rand*] ...)
           `(,rator ,rand* ...)]
          [,else (errorf who "invalid Effect ~s" else)]))

      (define (Pred pred)
        (match pred
          [(let ([,uvar* ,[Value -> value*]] ...) ,[p])
           (make-begin `((set! ,uvar* ,value*) ... ,p))] ;;
          [(true) '(true)]
          [(false) '(false)]
          [(if ,[test] ,[then] ,[else]) `(if ,test ,then ,else)]
          [(begin ,[Effect -> ef*] ... ,[p])
           (make-begin `(,ef* ... ,p))] ;;
          [(,relop ,[Value -> v1] ,[Value -> v2]) (guard (relop? relop))
           `(,relop ,v1 ,v2)]
          [,else (errorf who "invalid Pred ~s" else)]))

      (define (Tail tail)
        (match tail
          [(let ([,uvar* ,[Value -> value*]] ...) ,[t])
           (make-begin `((set! ,uvar* ,value*) ... ,t))] ;;
          [(if ,[Pred -> test] ,[then] ,[else])
           `(if ,test ,then ,else)]
          [(begin ,[Effect -> ef*] ... ,[t])
           (make-begin `(,ef* ... ,t))] ;;
          [(alloc ,[Value -> v]) `(alloc ,v)]
          [(,binop ,[Value -> v1] ,[Value -> v2]) (guard (binop? binop))
           `(,binop ,v1 ,v2)]
          [(,[Value -> rator] ,[Value -> rand*] ...)
           `(,rator ,rand* ...)]
          [,t (guard (triv? t)) t]
          [,else (errorf who "invalid Tail ~s" else)]))
      
      (match body
        [(locals (,uvar* ...) ,[Tail -> tail])
         `(locals (,uvar* ...) ,tail)]
        [,else (errorf who "invalid Body ~s" else)]))
    
    (lambda (exp)
      (match exp
        [(letrec ([,label* (lambda (,uvar* ...) ,[Body -> b*])] ...) ,[Body -> b])
         `(letrec ([,label* (lambda (,uvar* ...) ,b*)] ...) ,b)]
        [,el (errorf who "invalid Program ~s" el)]))))

