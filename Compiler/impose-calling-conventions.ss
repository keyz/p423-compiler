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

    (define (argument-locations fmls idx->fv)
      (let f ([fmls fmls] [regs parameter-registers] [fv-idx 0])
        (cond
         [(null? fmls) '()]
         [(null? regs)
          ;; force left-to-right order so that new-frame-variable suffixes are numbered in order
          (let ([fv (idx->fv fv-idx)])
            (cons (idx->fv fv-idx) (f (cdr fmls) regs (+ fv-idx 1))))]
         [else (cons (car regs) (f (cdr fmls) (cdr regs) fv-idx))])))

    (define (index->new-frame-var idx) (unique-name 'nfv))

    (define (Body bd fml*)

      (define new-frame-var** '())

      (define (triv? x) (or (uvar? x) (int64? x) (label? x)))

      (define (Triv t) (if (triv? t) t (error who "invalid Triv ~s" t)))

      (define (Effect ef)
        (match ef
          [(nop) '(nop)]
          [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
          [(set! ,var (,primop ,[Triv -> rand*] ...))
           (guard (memq primop '(alloc mref + - logand logor * sra)))
           `(set! ,var (,primop ,rand* ...))]
          [(set! ,var (,rator ,rand* ...))
           (make-begin
            `(,(Effect `(,rator ,rand* ...))
              (set! ,var ,return-value-register)))]
          [(set! ,var ,[Triv -> tr]) `(set! ,var ,tr)]
          [(mset! ,[Triv -> x] ,[Triv -> y] ,[Triv -> z]) `(mset! ,x ,y ,z)]
          [(,[Triv -> rator] ,[Triv -> rand*] ...)
           (let ([rplab (unique-label 'rp)]
                 [loc* (argument-locations rand* index->new-frame-var)])
             (let ([rand* (reverse rand*)]
                   [loc-rand* (reverse loc*)]
                   [nfv* (remp register? loc*)])
               (set! new-frame-var** (cons nfv* new-frame-var**))
               `(return-point ,rplab
                              (begin
                                (set! ,loc-rand* ,rand*) ...
                                (set! ,return-address-register ,rplab)
                                (,rator ,return-address-register
                                        ,frame-pointer-register ,allocation-pointer-register
                                        ,loc* ...)))))]
          [,ef (error who "invalid Effect ~s" ef)]))

      (define (Pred pr)
        (match pr
          [(true) '(true)]
          [(false) '(false)]
          [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
          [(,relop ,[Triv -> x] ,[Triv -> y]) `(,relop ,x ,y)]
          [,pr (error who "invalid Pred ~s" pr)]))

      (define (Tail tail rp)
        (match tail
          [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
          [(,primop ,[Triv -> rand*] ...)
           (guard (memq primop '(alloc mref + - * logand logor sra)))
           (make-begin `((set! ,return-value-register (,primop ,rand* ...))
                         (,rp ,frame-pointer-register
                              ,allocation-pointer-register ,return-value-register)))]
          [(,[Triv -> rator] ,[Triv -> rand*] ...)
           (let ([loc* (argument-locations rand* index->frame-var)])
             (let ([rand* (reverse rand*)] [loc-rand* (reverse loc*)])
               (make-begin 
                `((set! ,loc-rand* ,rand*) ...  
                  (set! ,return-address-register ,rp)
                  (,rator ,return-address-register ,frame-pointer-register
                          ,allocation-pointer-register ,loc* ...)))))]
          [,tr (guard (triv? tr))
               (make-begin `((set! ,return-value-register ,tr)
                             (,rp ,frame-pointer-register
                                  ,allocation-pointer-register
                                  ,return-value-register)))]
          [,tail (error who "invalid Tail ~s" tail)]))

      (match bd
        [(locals (,local* ...) ,tail)
         (let ([rp (unique-name 'rp)]
               [fml-loc* (argument-locations fml* index->frame-var)])
           (let ([tail (Tail tail rp)])
             `(locals (,rp ,fml* ... ,local* ... ,new-frame-var** ... ...)
                (new-frames (,new-frame-var** ...)
                  ,(make-begin 
                    `((set! ,rp ,return-address-register)
                      (set! ,fml* ,fml-loc*) ...
                      ,tail))))))]
        [,bd (error who "invalid Body ~s" bd)]))

    (lambda (x)
      (match x
        [(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)
         (let ([bd* (map Body bd* fml**)] [bd (Body bd '())])
           `(letrec ([,label* (lambda () ,bd*)] ...) ,bd))]
        [,x (error who "invalid Program ~s" x)])))

  )
