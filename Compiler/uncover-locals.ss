#|

A9 - Mar 28, 2015

pass: uncover-locals

This pass scans through each lambda or letrec body to find all variables bound by let expressions within the body and records these variables in a locals form wrapped around the body.

Input:
Prog   ::= (letrec ((Label (lambda (UVar *) Tail)) *) Tail)
Tail   ::= (let ([UVar Value]*) Tail)
         | (if Pred Tail Tail)
         | (begin Effect * Tail)      
         | (alloc Value)
         | (mref  Value Value)
         | (Binop Value Value)
         | (Value Value *)
         | Triv
Pred   ::= (let ([UVar Value]*) Pred)
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
Value  ::= (let ([UVar Value]*) Value)
         | (if Pred Value Value)
         | (begin Effect * Value)
         | (alloc Value)
         | (mref  Value Value)
         | (Binop Value Value)
         | (Value Value *)
         | Triv
Triv   ::= UVar | Integer | Label


Output:
Prog   ::= (letrec ((Label (lambda (UVar *) Body)) *) Body)    ;; mod
Body   ::= (locals (UVar *) Tail)                              ;; mod
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

|#

(library (Compiler uncover-locals)
  (export uncover-locals)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common))

  (define-who uncover-locals
    
    (define (Body body)

      (define (Value value)
        (match value
          [(let ([,uvar* ,[Value -> value*]] ...) ,[v])
           (apply hset-union (list->hset uvar*) v value*)]
          [(if ,[Pred -> test] ,[then] ,[else]) (hset-union test then else)]
          [(begin ,[Effect -> ef*] ... ,[v]) (apply hset-union v ef*)]
          [(alloc ,[v]) v]
          [(,binop ,[v1] ,[v2]) (guard (binop? binop))
           (hset-union v1 v2)]
          [(,[rator] ,[rand*] ...)
           (apply hset-union rator rand*)]
          [,t (guard (triv? t)) (hset-init)]
          [,else (errorf who "invalid Value ~s" else)]))
      
      (define (Effect effect)
        (match effect
          [(let ([,uvar* ,[Value -> value*]] ...) ,[e])
           (apply hset-union (list->hset uvar*) e value*)]
          [(nop) (hset-init)]
          [(mset! ,[Value -> v1] ,[Value -> v2] ,[Value -> v3])
           (hset-union v1 v2 v3)]
          [(if ,[Pred -> test] ,[then] ,[else]) (hset-union test then else)]
          [(begin ,[ef*] ... ,[ef]) (apply hset-union ef ef*)]
          [(,[Value -> rator] ,[Value -> rand*] ...)
           (apply hset-union rator rand*)]
          [,else (errorf who "invalid Effect ~s" else)]))

      (define (Pred pred)
        (match pred
          [(let ([,uvar* ,[Value -> value*]] ...) ,[p])
           (apply hset-union (list->hset uvar*) p value*)]
          [(true) (hset-init)]
          [(false) (hset-init)]
          [(if ,[test] ,[then] ,[else]) (hset-union test then else)]
          [(begin ,[Effect -> ef*] ... ,[p]) (apply hset-union p ef*)]
          [(,relop ,[Value -> v1] ,[Value -> v2]) (guard (relop? relop))
           (hset-union v1 v2)]
          [,else (errorf who "invalid Pred ~s" else)]))

      (define (Tail tail)
        (match tail
          [(let ([,uvar* ,[Value -> value*]] ...) ,[t])
           (apply hset-union (list->hset uvar*) t value*)]
          [(if ,[Pred -> test] ,[then] ,[else]) (hset-union test then else)]
          [(begin ,[Effect -> ef*] ... ,[t]) (apply hset-union t ef*)]
          [(alloc ,[Value -> v]) v]
          [(,binop ,[Value -> v1] ,[Value -> v2]) (guard (binop? binop))
           (hset-union v1 v2)]
          [(,[Value -> rator] ,[Value -> rand*] ...)
           (apply hset-union rator rand*)]
          [,t (guard (triv? t)) (hset-init)]
          [,else (errorf who "invalid Tail ~s" else)]))

      `(locals ,(hset->list (Tail body)) ,body))

    (lambda (exp)
      (match exp
        [(letrec ([,label* (lambda (,uvar* ...) ,[Body -> tail*])] ...) ,[Body -> tail])
         `(letrec ([,label* (lambda (,uvar* ...) ,tail*)] ...) ,tail)]
        [,el (errorf who "invalid Program ~s" el)])))

  )
