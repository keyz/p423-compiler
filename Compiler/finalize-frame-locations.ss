#|

A8 - Mar 6, 2015

pass: finalize-frame-locations

This pass is called after assign-frame to replace each occurrence of a frame-allocated uvar with the corresponding frame variable, based on the locate form. It is essentially the same as finalize-locations, except that the Body forms again come in complete and incomplete forms, while the Body forms for the original are always in the complete form. Furthermore, this pass does not discard the locate form because the locations assigned to frame variables on this iteration of the register and frame allocation passes need to be taken into account by the next, if more variables are spilled. Thus, the output-language grammar for this pass is the same as the output-language grammar for the preceding pass.

Moreover, `(set! fvi fvi)` becomes `(nop)`, which will be washed away later by expose-basic-blocks.

Input Grammar:
Program ::= (letrec ((Label (lambda () Body)) *) Body) 
Body    ::= (locals (UVar *)                            
              (ulocals (UVar *)
                (locate ((UVar FVar) *) 
                  (frame-conflict ((UVar Var *) *)
                    Tail))))
          | (locate ((UVar Loc) *) Tail)                
Tail    ::= (if Pred Tail Tail)
          | (begin Effect * Tail)
          | (Triv Var *)  ;; Note 'Var' not 'Loc'.     
Pred    ::= (true)
          | (false)
          | (if Pred Pred Pred)
          | (begin Effect * Pred)
          | (Relop Triv Triv)
Effect  ::= (nop)
          | (set! Var Triv)                            
          | (set! Var (Binop Triv Triv))               
          | (set! Var (mref Triv Triv))                 ;; a8 new
          | (return-point Label Tail)                  
          | (mset! Triv Triv Triv)                      ;; a8 new
          | (if Pred Effect Effect)
          | (begin Effect * Effect)
Triv    ::= Var                                        
          | Integer | Label
Loc     ::= Reg | FVar                                 
Var     ::= UVar | Loc                                 

Output Grammar:
Program ::= (letrec ((Label (lambda () Body)) *) Body) 
Body    ::= (locals (UVar *)                            
              (ulocals (UVar *)
                (locate ((UVar FVar) *) 
                  (frame-conflict ((UVar Var *) *)
                    Tail))))
          | (locate ((UVar Loc) *) Tail)                
Tail    ::= (if Pred Tail Tail)
          | (begin Effect * Tail)
          | (Triv Loc *)                                ;;      mod
Pred    ::= (true)
          | (false)
          | (if Pred Pred Pred)
          | (begin Effect * Pred)
          | (Relop Triv Triv)
Effect  ::= (nop)
          | (set! Var Triv)                            
          | (set! Var (Binop Triv Triv))               
          | (set! Var (mref Triv Triv))                 ;; a8 new
          | (return-point Label Tail)                  
          | (mset! Triv Triv Triv)                      ;; a8 new
          | (if Pred Effect Effect)
          | (begin Effect * Effect)
Triv    ::= Var                                        
          | Integer | Label
Loc     ::= Reg | FVar                                 
Var     ::= UVar | Loc                                 

|#

(library (Compiler finalize-frame-locations)
  (export finalize-frame-locations)
  (import (chezscheme) (Framework match) (Framework helpers))

  (define-who finalize-frame-locations
    
    (define Var ;; lookup
      (lambda (env)
        (lambda (v)
          (cond
           [(and (uvar? v) (assq v env)) => cdr]
           [else v]))))
    
    (define Triv Var)
    
    (define Pred
      (lambda (env)
        (lambda (pr)
          (match pr
            [(true) '(true)]
            [(false) '(false)]
            [(if ,[test] ,[conseq] ,[altern])
             `(if ,test ,conseq ,altern)]
            [(begin ,[(Effect env) -> ef*] ... ,[pr])
             `(begin ,ef* ... ,pr)]
            [(,relop ,[(Triv env) -> x] ,[(Triv env) -> y])
             `(,relop ,x ,y)]
            [,pr (error who "invalid Pred ~s" pr)]))))
    
    (define Effect
      (lambda (env)
        (lambda (ef)
          (match ef
            [(mset! ,[(Triv env) -> base-t] ;; a8 new
                    ,[(Triv env) -> offset-t]
                    ,[(Triv env) -> t])
             `(mset! ,base-t ,offset-t ,t)]
            [(set! ,[(Triv env) -> var] ;; a8 new
                   (mref ,[(Triv env) -> base-t]
                         ,[(Triv env) -> offset-t]))
             `(set! ,var (mref ,base-t ,offset-t))]
            [(nop) '(nop)]
            [(set! ,[(Var env) -> x]
                   (,binop ,[(Triv env) -> y] ,[(Triv env) -> z]))
             `(set! ,x (,binop ,y ,z))]
            [(set! ,[(Var env) -> x] ,[(Triv env) -> y])
             (if (eq? y x) `(nop) `(set! ,x ,y))]
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
            [(begin ,[(Effect env) -> ef*] ... ,[tail])
             `(begin ,ef* ... ,tail)]
            [(if ,[(Pred env) -> test] ,[conseq] ,[altern])
             `(if ,test ,conseq ,altern)]
            [(,[(Triv env) -> t] ,[(Triv env) -> live*] ...)
             `(,t ,live* ...)]
            [,tail (error who "invalid Tail ~s" tail)]))))
    
    (define Body
      (lambda (bd)
        (match bd
          [(locals (,local* ...)
             (ulocals (,ulocal* ...)
               (locate ([,uvar* ,loc*] ...)
                 (frame-conflict ,ct ,[(Tail (map cons uvar* loc*)) -> tail]))))
           `(locals (,local* ...)
              (ulocals (,ulocal* ...)
                (locate ([,uvar* ,loc*] ...)
                  (frame-conflict ,ct ,tail))))]
          [(locate ([,uvar* ,loc*] ...) ,tail)
           `(locate ([,uvar* ,loc*] ...) ,tail)]
          [,bd (error who "invalid Body ~s" bd)])))
    (lambda (x)
      (match x
        [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
         `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
        [,x (error who "invalid Program ~s" x)])))  

  )
