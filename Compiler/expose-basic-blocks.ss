#|

A8 - Mar 6, 2015

pass: expose-basic-blocks

nothin' changed.
                     
Input Grammar:
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
Loc      ::= Reg | Disp | Ind                            ;;

Output Grammar:
Program  ::= (letrec ((Label (lambda () Tail)) *) Tail)  ;;      
Tail     ::= (if (Relop Triv Triv) (Label) (Label))      ;;      mod
           | (begin Effect * Tail)
           | (Triv)                                      
Effect   ::= (set! Loc Triv)                             ;;      
           | (set! Loc (Binop Triv Triv))                ;;      
Triv     ::= Loc                                         ;;      
           | Integer | Label
Loc      ::= Reg | Disp | Ind                            ;;

|#

(library (Compiler expose-basic-blocks)
  (export expose-basic-blocks)
  (import (chezscheme)
    (Framework helpers)
    (Framework match))

  (define-who expose-basic-blocks
    
    (define (Tail x)
      (match x
        [(if ,pred ,[conseq cb*] ,[altern ab*])
         (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
           (let-values ([(pred pb*) (Pred pred clab alab)])
             (values pred
                     `(,pb* ...
                            [,clab (lambda () ,conseq)]
                            ,cb* ...
                            [,alab (lambda () ,altern)]
                            ,ab* ...))))]
        [(begin ,effect* ... ,[tail tb*])
         (let-values ([(x xb*) (Effect* effect* `(,tail))])
           (values x `(,xb* ... ,tb* ...)))]
        [(,triv) (values `(,triv) '())]
        [,x (error who "invalid Tail ~s" x)]))
    
    (define (Pred x tlab flab)
      (match x
        [(true) (values `(,tlab) '())]
        [(false) (values `(,flab) '())]
        [(if ,pred ,[conseq cb*] ,[altern ab*])
         (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
           (let-values ([(pred pb*) (Pred pred clab alab)])
             (values pred
                     `(,pb* ...
                            [,clab (lambda () ,conseq)]
                            ,cb* ...
                            [,alab (lambda () ,altern)]
                            ,ab* ...))))]
        [(begin ,effect* ... ,[pred pb*])
         (let-values ([(x xb*) (Effect* effect* `(,pred))])
           (values x `(,xb* ... ,pb* ...)))]
        [(,relop ,triv1 ,triv2)
         (values `(if (,relop ,triv1 ,triv2) (,tlab) (,flab)) '())]
        [,x (error who "invalid Pred ~s" x)]))
    
    (define (Effect* x* rest*)
      (match x*
        [() (values (make-begin rest*) '())]
        [(,x* ... ,x) (Effect x* x rest*)]))
    
    (define (Effect x* x rest*)
      (match x
        [(nop) (Effect* x* rest*)]
        [(set! ,lhs ,rhs) (Effect* x* `((set! ,lhs ,rhs) ,rest* ...))]
        [(if ,pred ,conseq ,altern)
         (let ([clab (unique-label 'c)]
               [alab (unique-label 'a)]
               [jlab (unique-label 'j)])
           (let-values ([(conseq cb*) (Effect '() conseq `((,jlab)))]
                        [(altern ab*) (Effect '() altern `((,jlab)))]
                        [(pred pb*) (Pred pred clab alab)])
             (let-values ([(x xb*) (Effect* x* `(,pred))])
               (values x
                       `(,xb* ...
                              ,pb* ...
                              [,clab (lambda () ,conseq)]
                              ,cb* ...
                              [,alab (lambda () ,altern)]
                              ,ab* ...
                              [,jlab (lambda () ,(make-begin rest*))])))))]
        [(begin ,effect* ...) (Effect* `(,x* ... ,effect* ...) rest*)]

        [(return-point ,rp-lab ,tail) ;; new
         (let*-values ([(tail tb*) (Tail tail)]
                       [(ef eb*) (Effect* x* (cdr tail))])
           (values ef
                   `(,eb* ...
                          ,tb* ...
                          [,rp-lab (lambda ()
                                     ,(make-begin rest*))])))]
        
        [,x (error who "invalid Effect ~s" x)]))

    (lambda (x)
      (match x
        [(letrec ([,label* (lambda () ,[Tail -> tail* b**])] ...) ,[Tail -> tail b*])
         `(letrec ([,label* (lambda () ,tail*)] ... ,b** ... ... ,b* ...) ,tail)]
        [,x (error who "invalid Program ~s" x)])))
  
  )
