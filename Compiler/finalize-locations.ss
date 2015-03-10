#|

A8 - Mar 6, 2015

pass: finalize-locations

This pass replaces each occurrence of a uvar in the body of each locate form with the corresponding Loc. As it does so, it removes useless assignments, i.e., converts any assignment (set! x y) to (nop) if x and y resolve to the same location.

Input Grammar:
Program  ::= (letrec ((Label (lambda () Body)) *) Body) 
Body     ::= (locate ((UVar Loc) *) Tail)                
Tail     ::= (if Pred Tail Tail)
           | (begin Effect * Tail)
           | (Triv)                                      
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
Program  ::= (letrec ((Label (lambda () Tail)) *) Tail)  ;;      mod
Tail     ::= (if Pred Tail Tail)
           | (begin Effect * Tail)
           | (Triv)                                      
Pred     ::= (true)
           | (false)
           | (if Pred Pred Pred)
           | (begin Effect * Pred)
           | (Relop Triv Triv)
Effect   ::= (nop)
           | (set! Loc Triv)                             ;;      mod
           | (set! Loc (Binop Triv Triv))                ;;      mod
           | (set! Loc (mref Triv Triv))                 ;; a8n, mod
           | (return-point Label Tail)                  
           | (mset! Triv Triv Triv)                      ;; a8n
           | (if Pred Effect Effect)
           | (begin Effect * Effect)
Triv     ::= Loc                                         ;;      mod
           | Integer | Label
Loc      ::= Reg | FVar                                 

|#

(library (Compiler finalize-locations)
  (export finalize-locations)
  (import (chezscheme)
    (Framework helpers)
    (Framework match))

  (define-who finalize-locations

    (define Var
      (lambda (env)
        (lambda (v)
          (if (uvar? v) (cdr (assq v env)) v))))

    (define Triv
      (lambda (env)
        (lambda (t)
          (if (uvar? t) (cdr (assq t env)) t))))

    (define Pred
      (lambda (env)
        (lambda (pr)
          (match pr
            [(true) '(true)]
            [(false) '(false)]
            [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
            [(begin ,[(Effect env) -> ef*] ... ,[pr]) `(begin ,ef* ... ,pr)]
            [(,relop ,[(Triv env) -> x] ,[(Triv env) -> y]) `(,relop ,x ,y)]
            [,pr (error who "invalid Pred ~s" pr)]))))

    (define Effect
      (lambda (env)
        (lambda (ef)
          (match ef
            [(mset! ,[(Triv env) -> base] ;; a8 new
                    ,[(Triv env) -> offset]
                    ,[(Triv env) -> t]) 
             `(mset! ,base ,offset ,t)]
            [(set! ,[(Var env) -> v]
                   (mref ,[(Triv env) -> base]
                         ,[(Triv env) -> offset])) ;; a8 new
             `(set! ,v (mref ,base ,offset))]
            [(nop) '(nop)]
            [(set! ,[(Var env) -> x]
                   (,binop ,[(Triv env) -> y] ,[(Triv env) -> z]))
             `(set! ,x (,binop ,y ,z))]
            [(set! ,[(Var env) -> x] ,[(Triv env) -> y]) 
             (if (eq? x y) '(nop) `(set! ,x ,y))]
            [(begin ,[ef] ,[ef*] ...) `(begin ,ef ,ef* ...)]
            [(if ,[(Pred env) -> test] ,[conseq] ,[altern])
             `(if ,test ,conseq ,altern)]
            [(return-point ,label ,[(Tail env) -> tail])
             `(return-point ,label ,tail)]
            [,ef (error who "invalid Effect ~s" ef)]))))

    (define Tail
      (lambda (env)
        (lambda (tail)
          (match tail
            [(begin ,[(Effect env) -> ef*] ... ,[tail]) `(begin ,ef* ... ,tail)]
            [(if ,[(Pred env) -> test] ,[conseq] ,[altern])
             `(if ,test ,conseq ,altern)]
            [(,[(Triv env) -> t]) `(,t)]
            [,tail (error who "invalid Tail ~s" tail)]))))

    (define Body
      (lambda (bd)
        (match bd
          [(locate ([,uvar* ,loc*] ...) ,[(Tail (map cons uvar* loc*)) -> tail])
           tail]
          [,bd (error who "invalid Body ~s" bd)])))

    (lambda (x)
      (match x
        [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
         `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
        [,x (error who "invalid Program ~s" x)])))
  
  )
