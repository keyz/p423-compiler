#|

A8 - Mar 6, 2015

pass: discard-call-live

This pass discards the Loc* list included in each call.

Input Grammar:
Program  ::= (letrec ((Label (lambda () Body)) *) Body) 
Body     ::= (locals (UVar *)                            
               (ulocals (UVar *)
                 (spills (UVar *)
                   (locate ((UVar FVar) *) 
                     (frame-conflict ((UVar Var *) *) 
                       Tail)))))
           | (locate ((UVar Loc) *) Tail)                
Tail     ::= (if Pred Tail Tail)
           | (begin Effect * Tail)
           | (Triv Loc *)                                
Pred     ::= (true)
           | (false)
           | (if Pred Pred Pred)
           | (begin Effect * Pred)
           | (Relop Triv Triv)
Effect   ::= (nop)
           | (set! Var Triv)                            
           | (set! Var (Binop Triv Triv))               
           | (set! Var (mref Triv Triv))                 ;; a8 new
           | (return-point Label Tail)                  
           | (mset! Triv Triv Triv)                      ;; a8 new
           | (if Pred Effect Effect)
           | (begin Effect * Effect)
Triv     ::= Var                                        
           | Integer | Label
Loc      ::= Reg | FVar                                 
Var      ::= UVar | Loc

Output Grammar:
Program  ::= (letrec ((Label (lambda () Body)) *) Body) 
Body     ::= (locate ((UVar Loc) *) Tail)                
Tail     ::= (if Pred Tail Tail)
           | (begin Effect * Tail)
           | (Triv)                                      ;;      mod
Pred     ::= (true)
           | (false)
           | (if Pred Pred Pred)
           | (begin Effect * Pred)
           | (Relop Triv Triv)
Effect   ::= (nop)
           | (set! Var Triv)                            
           | (set! Var (Binop Triv Triv))               
           | (set! Var (mref Triv Triv))                 ;; a8 new
           | (return-point Label Tail)                  
           | (mset! Triv Triv Triv)                      ;; a8 new
           | (if Pred Effect Effect)
           | (begin Effect * Effect)
Triv     ::= Var                                        
           | Integer | Label
Loc      ::= Reg | FVar                                 
Var      ::= UVar | Loc

|#

(library (Compiler discard-call-live)
  (export discard-call-live)
  (import (chezscheme)
    (Framework helpers)
    (Framework match))

  (define-who discard-call-live

    (define Tail
      (lambda (tail)
        (match tail
          [(begin ,[Effect -> ef*] ... ,[tail]) `(begin ,ef* ... ,tail)]
          [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(,t ,live* ...) `(,t)] ;; discard it
          [,tail (errorf who "invalid Tail ~s" tail)])))

    (define Effect
      (lambda (ef)
        (match ef
          [(mset! ,base ,offset ,t) ;; a8 new
           `(mset! ,base ,offset ,t)]
          [(set! ,v (mref ,base ,offset)) ;; a8 new
           `(set! ,v (mref ,base ,offset))]
          [(nop) '(nop)]
          [(set! ,x ,y) `(set! ,x ,y)]
          [(set! ,x (,binop ,y ,z)) `(set! ,x (,binop ,y ,z))]
          [(begin ,[ef*] ... ,[ef]) `(begin ,ef* ... ,ef)] ;;
          [(if ,[Pred -> test] ,[ef-c] ,[ef-a])
           `(if ,test ,ef-c ,ef-a)]
          [(return-point ,label ,[Tail -> tail]) 
           `(return-point ,label ,tail)]
          [,ef (errorf who "invalid Effect ~s" ef)])))   

    (define Pred
      (lambda (pr)
        (match pr
          [(true) '(true)]
          [(false) '(false)]
          [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[pr]) `(begin ,ef* ... ,pr)]
          [(,relop ,x ,y) `(,relop ,x ,y)]
          [,pr (errorf who "invalid Pred ~s" pr)])))
    
    (define Body
      (lambda (bd)
        (match bd
          [(locate ([,uvar* ,loc*] ...) ,[Tail -> tail])
           `(locate ([,uvar* ,loc*] ...) ,tail)]
          [,bd (errorf who "invalid Body ~s" bd)])))

    (lambda (x)
      (match x
        [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
         `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
        [,x (errorf who "invalid Program ~s" x)])))  
  
  )
