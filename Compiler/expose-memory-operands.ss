#|

A8 - Mar 6, 2015

pass: expose-memory-operands

This pass removes `mref` and `mset!` forms from `Effect`. Depending on the type of `offset`, the expression is converted to an `Index` or `Disp`.
                     
Output Grammar:
Program  ::= (letrec ((Label (lambda () Tail)) *) Tail)  ;;      
Tail     ::= (if Pred Tail Tail)
           | (begin Effect * Tail)
           | (Triv)                                      
Pred     ::= (true)
           | (false)
           | (if Pred Pred Pred)
           | (begin Effect * Pred)
           | (Relop Triv Triv)
Effect   ::= (nop)
           | (set! Loc Triv)                             ;;      
           | (set! Loc (Binop Triv Triv))                ;;      
           | (return-point Label Tail)                  
           | (if Pred Effect Effect)
           | (begin Effect * Effect)
Triv     ::= Loc                                         ;;      
           | Integer | Label
Loc      ::= Reg | Disp | Ind                            ;;      mod

|#

(library (Compiler expose-memory-operands)
  (export expose-memory-operands)
  (import (chezscheme)
    (Framework helpers)
    (Framework match))

  (define-who expose-memory-operands

    (define (make-index-or-disp base offset)
      (if (register? offset)
          (make-index-opnd base offset)
          (make-disp-opnd base offset)))
    
    (define Pred
      (lambda (pr)
        (match pr
          [(true) '(true)]
          [(false) '(false)]
          [(if ,[test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[test])
           (make-begin `(,ef* ... ,test))]
          [(,relop ,tr1 ,tr2)
           `(,relop ,tr1 ,tr2)]
          [,pr (error who "invalid Pred ~s" pr)])))

    (define Effect
      (lambda (st)
        (match st
          [(mset! ,base ,offset ,t) ;; a8 new
           `(set! ,(make-index-or-disp base offset) ,t)]
          [(set! ,var (mref ,base ,offset)) ;; a8 new
           `(set! ,var ,(make-index-or-disp base offset))]
          [(nop) '(nop)]
          [(begin ,[ef] ,[ef*] ...)
           (make-begin `(,ef ,ef* ...))]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(return-point ,label ,[Tail -> tail])
           `(return-point ,label ,tail)]
          [(set! ,var (,binop ,t1 ,t2))
           `(set! ,var (,binop ,t1 ,t2))]
          [(set! ,var ,t)
           `(set! ,var ,t)]
          [,st (error who "invalid syntax for Effect ~s" st)])))

    (define Tail
      (lambda (tail)
        (match tail
          [(begin ,[Effect -> ef*] ... ,[tail])
           (make-begin `(,ef* ... ,tail))]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(,t) `(,t)]
          [,tail (error who "invalid syntax for Tail ~s" tail)])))

    (lambda (program)
      (match program
        [(letrec ([,label* (lambda () ,[Tail -> tail*])] ...)
           ,[Tail -> tail])
         `(letrec ([,label* (lambda () ,tail*)] ...) ,tail)]
        [,program (error who "invalid syntax for Program: ~s" program)])))
  
  )

