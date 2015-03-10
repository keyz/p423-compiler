#|

A8 - Mar 6, 2015

pass: impose-calling-conventions

This pass imposes the calling conventions on the output code. It arranges for each argument to be passed in a register or on the stack, as appropriate, and the return value to be returned in a register. After it is done, lambda expressions no longer have explicit formal parameters, and both calls and returns are reduced to the equivalent of jumps. The programs produced by this pass are in the same language as the input language for uncover-frame-conflict

input grammar changes:
Effect  ::= (set! uvar (alloc Triv))      ;; +
          | (set! uvar (mref Triv Triv))  ;; +
          | (mset! Triv Triv Triv)        ;; +

Tail    ::= (alloc Triv)                  ;; +
          | (mref Triv Triv)              ;; +

output grammar:

Program ::= (letrec ((Label (lambda () Body)) *) Body) ;;      mod
Body    ::= (locals (UVar *)                           ;;      mod
	      (new-frames (Frame *)
	        Tail))
Tail    ::= (if Pred Tail Tail)
          | (begin Effect * Tail)
          | (Triv Var *)  ;; Note 'Var' not 'Loc'.     ;;      mod
Pred    ::= (true)
          | (false)
          | (if Pred Pred Pred)
          | (begin Effect * Pred)
          | (Relop Triv Triv)
Effect  ::= (nop)
          | (set! Var Triv)                            ;;      mod
          | (set! Var (Binop Triv Triv))               ;;      mod
          | (set! Var (alloc Triv))                    ;; a8n, mod
          | (set! Var (mref Triv Triv))                ;; a8n, mod
          | (return-point Label Tail)                  ;;      mod
          | (mset! Triv Triv Triv)                     ;; a8n
          | (if Pred Effect Effect)
          | (begin Effect * Effect)
Triv    ::= Var                                        ;;      mod
          | Integer | Label
Loc     ::= Reg | FVar                                 ;;      mod
Var     ::= UVar | Loc                                 ;;      mod
Frame   ::= (Uvar *)                                   ;;      mod

|#

(library (Compiler impose-calling-conventions)
  (export impose-calling-conventions)
  (import (chezscheme) (Framework helpers) (Framework match))

  (define-who impose-calling-conventions

    (define (Body bd fml*)

      (define new-f '())
      
      (define (argument-locations args)
        (let f ([args args] [regs parameter-registers] [fv-idx 0])
          (cond
           [(null? args) '()]
           [(null? regs) 
            (cons (index->frame-var fv-idx)
                  (f (cdr args) regs (+ fv-idx 1)))]
           [else (cons (car regs)
                       (f (cdr args) (cdr regs) fv-idx))])))

      (define (f-locations args)
        (let f ([args args] [regs parameter-registers])
          (cond
           [(null? args) '()]
           [(null? regs) 
            (cons (unique-name 'nfv)
                  (f (cdr args) regs))]
           [else (cons (car regs)
                       (f (cdr args) (cdr regs)))])))
      
      (define (triv? x) (or (uvar? x) (int64? x) (label? x)))
      
      (define (Triv t) (if (triv? t) t (errorf who "invalid Triv ~s" t)))

      (define (Effect ef)
        ;; + (set! uvar (alloc Triv))
        ;; + (set! uvar (mref Triv Triv))
        ;; + (mset! Triv Triv Triv)
        (match ef
          [(mset! ,[Triv -> t1] ,[Triv -> t2] ,[Triv -> t3]) ;; a8 new
           `(mset! ,t1 ,t2 ,t3)]
          [(set! ,var (alloc ,[Triv -> t])) ;; a8 new
           `(set! ,var (alloc ,t))]
          [(set! ,var (mref ,[Triv -> t1] ,[Triv -> t2])) ;; a8 new
           `(set! ,var (mref ,t1 ,t2))]          
          [(nop) '(nop)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[ef*] ... ,[ef])
           (make-begin `(,ef* ... ,ef))]
          [(set! ,var (,binop ,[Triv -> x] ,[Triv -> y]))
           (guard (memq binop '(+ - * logand logor sra)))           
           `(set! ,var (,binop ,x ,y))]
          [(set! ,var (,rator ,rand* ...))
           (make-begin
            `(,(Effect `(,rator ,rand* ...))
              (set! ,var ,return-value-register)))]
          [(set! ,var ,[Triv -> tr])
           `(set! ,var ,tr)]
          [(,rator ,rand* ...)
           (let* ([rp (unique-label 'rp)]
                  [loc* (f-locations rand*)]
                  [rand* (reverse rand*)]
                  [loc-rand* (reverse loc*)])
             (set! new-f (cons (filter uvar? loc*) new-f))
             `(return-point ,rp
                            ,(make-begin
                              `((set! ,loc-rand* ,rand*) ...
                                (set! ,return-address-register ,rp)
                                (,rator ,return-address-register
                                        ,frame-pointer-register
                                        ;; ,allocation-pointer-register
                                        ,loc* ...)))))]
          [,ef (errorf who "invalid Effect ~s" ef)]))
      
      (define (Pred pr)
        (match pr
          [(true) '(true)]
          [(false) '(false)]
          [(if ,[test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[pr])
           (make-begin `(,ef* ... ,pr))]
          [(,relop ,[Triv -> x] ,[Triv -> y])
           `(,relop ,x ,y)]
          [,pr (errorf who "invalid Pred ~s" pr)]))
      
      (define (Tail tail rp) ;; + (alloc Triv) | (mref Triv Triv)
        (match tail
          [(alloc ,[Triv -> t]) ;; a8 new
           (make-begin
            `((set! ,return-value-register (alloc ,t))
              (,rp ,frame-pointer-register
                   ,allocation-pointer-register
                   ,return-value-register)))]
          [(mref ,[Triv -> t1] , [Triv -> t2])  ;; a8 new
           (make-begin
            `((set! ,return-value-register (mref ,t1 ,t2))
              (,rp ,frame-pointer-register
                   ,allocation-pointer-register
                   ,return-value-register)))]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[tail])
           (make-begin `(,ef* ... ,tail))]
          [(,binop ,[Triv -> x] ,[Triv -> y])
           (guard (memq binop '(+ - * logand logor sra)))
           (make-begin `((set! ,return-value-register (,binop ,x ,y))
                         (,rp ,frame-pointer-register
                              ,allocation-pointer-register ;; a8 new 
                              ,return-value-register)))]
          [(,[Triv -> rator] ,[Triv -> rand*] ...)
           (let ([loc* (argument-locations rand*)])
             (let ([rand* (reverse rand*)]
                   [loc-rand* (reverse loc*)])
               (make-begin
                `((set! ,loc-rand* ,rand*) ...
                  (set! ,return-address-register ,rp)
                  (,rator ,return-address-register
                          ,frame-pointer-register
                          ,allocation-pointer-register ;; a8 new
                          ,loc* ...)))))]
          [,tr (guard (triv? tr))
               (make-begin `((set! ,return-value-register ,tr)
                             (,rp ,frame-pointer-register
                                  ,allocation-pointer-register ;; a8 new
                                  ,return-value-register)))]
          [,tail (errorf who "invalid Tail ~s" tail)]))
      
      (match bd
        [(locals (,local* ...) ,tail)
         (let ([rp (unique-name 'rp)]
               [fml-loc* (argument-locations fml*)])
           (let ([tail (Tail tail rp)])
             `(locals (,rp ,fml* ... ,local* ... ,new-f ... ...)
                (new-frames
                  ,new-f
                  ,(make-begin
                    `((set! ,rp ,return-address-register)
                      (set! ,fml* ,fml-loc*) ...
                      ,tail))))))]
        [,bd (errorf who "invalid Body ~s" bd)]))
    
    (lambda (x)
      (match x
        [(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)
         (let ([bd* (map Body bd* fml**)]
               [bd (Body bd '())])
           `(letrec ([,label* (lambda () ,bd*)] ...) ,bd))]
        [,x (errorf who "invalid Program ~s" x)])))

  )
