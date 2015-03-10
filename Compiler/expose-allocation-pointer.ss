#|

A8 - Mar 6, 2015

pass: expose-allocation-pointer

This pass eliminates the `(set! Var (alloc Triv))` form in `Effect`.

Input Grammar:
Program ::= (letrec ((Label (lambda () Body)) *) Body) 
Body    ::= (locals (UVar *)                           
	      (new-frames (Frame *)
	        Tail))
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
          | (set! Var (alloc Triv))                    ;; found you!
          | (set! Var (mref Triv Triv))                
          | (return-point Label Tail)                  
          | (mset! Triv Triv Triv)                     
          | (if Pred Effect Effect)
          | (begin Effect * Effect)
Triv    ::= Var                                        
          | Integer | Label
Loc     ::= Reg | FVar                                 
Var     ::= UVar | Loc                                 
Frame   ::= (Uvar *)                                   


Output Grammar:
Program ::= (letrec ((Label (lambda () Body)) *) Body) 
Body    ::= (locals (UVar *)                           
	      (new-frames (Frame *)
	        Tail))
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
          | (set! Var (mref Triv Triv))                
          | (return-point Label Tail)                  
          | (mset! Triv Triv Triv)                     
          | (if Pred Effect Effect)
          | (begin Effect * Effect)
Triv    ::= Var                                        
          | Integer | Label
Loc     ::= Reg | FVar                                 
Var     ::= UVar | Loc                                 
Frame   ::= (Uvar *)  


|#


(library (Compiler expose-allocation-pointer)
  (export expose-allocation-pointer)
  (import (chezscheme) (Framework helpers) (Framework match))

  (define-who expose-allocation-pointer

    (define (var? v)
      (or (uvar? v) (register? v) (frame-var? v)))
    
    (define (Var v)
      (if (var? v)
          v
          (errorf who "invalid Var ~s" v)))

    (define (triv? x) (or (var? x) (int64? x) (label? x)))
    
    (define (Triv t) (if (triv? t) t (errorf who "invalid Triv ~s" t)))

    (define (Effect ef)
      (match ef
        [(nop) '(nop)]
        [(begin ,[ef*] ... ,[ef])
         (make-begin `(,ef* ... ,ef))]
        [(if ,[Pred -> test] ,[conseq] ,[altern])
         `(if ,test ,conseq ,altern)]
        [(mset! ,[Triv -> t1] ,[Triv -> t2] ,[Triv -> t3])  ;; a8 new
         `(mset! ,t1 ,t2 ,t3)]
        [(return-point ,label ,[Tail -> t])
         `(return-point ,label ,t)]
        [(set! ,var (alloc ,[Triv -> t])) ;; a8 new
         (make-begin
          `((set! ,var ,allocation-pointer-register)
            (set! ,allocation-pointer-register
                  (+ ,allocation-pointer-register ,t))))]
        [(set! ,var (mref ,[Triv -> t1] ,[Triv -> t2])) ;; a8 new
         `(set! ,var (mref ,t1 ,t2))]          
        [(set! ,var (,binop ,[Triv -> x] ,[Triv -> y]))
         (guard (memq binop '(+ - * logand logor sra)))           
         `(set! ,var (,binop ,x ,y))]
        [(set! ,var ,[Triv -> tr])
         `(set! ,var ,tr)]
        [,ef (errorf who "invalid Effect ~s" ef)]))

    (define (Pred pr)
      (match pr
        [(true) '(true)]
        [(false) '(false)]
        [(if ,[test] ,[conseq] ,[altern])
         `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[pred])
         (make-begin `(,ef* ... ,pred))]
        [(,relop ,[Triv -> x] ,[Triv -> y])
         (guard (memq relop '(< <= = >= >)))
         `(,relop ,x ,y)]
        [,x (errorf who "invalid Pred ~s" x)]))

    (define (Tail t)
      (match t
        [(if ,[Pred -> test] ,[conseq] ,[altern])
         `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[tail])
         (make-begin `(,ef* ... ,tail))]
        [(,[Triv -> rator] ,[Var -> var*] ...)
         `(,rator ,var* ...)]
        [,x (errorf who "invalid Tail ~s" x)]))

    (define (Body b)
      (match b
        [(locals (,uvar* ...)                           
           (new-frames (,frame* ...) ,[Tail -> t]))
         `(locals (,uvar* ...)                           
            (new-frames (,frame* ...) ,t))]
        [,x (errorf who "invalid Body ~s" x)]))

    (lambda (x)
      (match x
        [(letrec ([,label* (lambda () ,[Body -> body*])] ...)
           ,[Body -> body])
         `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
        [,x (errorf who "invalid Program ~s" x)])))
  )
