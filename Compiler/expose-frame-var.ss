#|

A8 - Mar 6, 2015

pass: expose-frame-var

This pass converts occurrences of the frame variables fv0, fv1, etc., into displacement mode operands, with rbp as the base register and an offset based on the frame variable's index. Since our words are 64 bits, i.e., 8 bytes, the offset for fvi should be 8i, e.g., 0, 8, 16, etc., for fv0, fv1, fv2, etc.

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
           | (set! Loc (mref Triv Triv))                 ;; a8 new
           | (return-point Label Tail)                  
           | (mset! Triv Triv Triv)                      ;; a8 new
           | (if Pred Effect Effect)
           | (begin Effect * Effect)
Triv     ::= Loc                                         ;;      
           | Integer | Label
Loc      ::= Reg | FVar                                 

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
           | (set! Loc (mref Triv Triv))                 ;; a8 new
           | (return-point Label Tail)                  
           | (mset! Triv Triv Triv)                      ;; a8 new
           | (if Pred Effect Effect)
           | (begin Effect * Effect)
Triv     ::= Loc                                         ;;      
           | Integer | Label
Loc      ::= Reg | Disp                                  ;;      mod

|#

(library (Compiler expose-frame-var)
  (export expose-frame-var)
  (import (chezscheme)
    (Framework helpers)
    (Framework match))

;;; expose-frame-var traverses the scheme source in the same grammar
;;; accepted by verify-scheme and changes frame-vars in the form
;;; fv0, fv1, etc. into explicit integer offsets from the register
;;; pointing to the frame-pointer register. To accomplish this,
;;; expose-frame-var makes use of make-disp-opnd which creates a
;;; displacement operand record expressing a register and fixed
;;; number displacment each displacement is the original frame var
;;; number multiplied by the word size (8 for 64-bit target machine)
;;; to get the byte offset.
;;; (i.e. fv0 => (make-disp-opnd frame-pointer-register 0)
;;;       fv1 => (make-disp-opnd frame-pointer-register 8)
;;;       fv2 => (make-disp-opnd frame-pointer-register 16)
;;;       fv3 => (make-disp-opnd frame-pointer-register 24)
;;;       ... well you get the idea.)
;;;
;;; Note: we use shift left by word-shift (3 for 64-bit target
;;; machine) to calculate the multiplication.

  (define-who expose-frame-var

    (define current-offset 0)

    (define (fpr? x)
      (eqv? x frame-pointer-register))
    
    (define Triv
      (lambda (t)
        (if (frame-var? t)
            (make-disp-opnd frame-pointer-register
                            (+ (ash (frame-var->index t) word-shift)
                               current-offset))
            t)))
    
    (define Pred
      (lambda (pr)
        (match pr
          [(true) '(true)]
          [(false) '(false)]
          [(if ,[test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[test])
           (make-begin `(,ef* ... ,test))]
          [(,relop ,[Triv -> tr1] ,[Triv -> tr2])
           `(,relop ,tr1 ,tr2)]
          [,pr (error who "invalid Pred ~s" pr)])))

    (define Effect
      (lambda (st)
        (match st
          [(mset! ,base ,offset ,t) `(mset! ,base ,offset ,t)] ;; a8 new
          [(set! ,var (mref ,base ,offset)) ;; a8 new
           `(set! ,var (mref ,base ,offset))]
          [(nop) '(nop)]
          [(begin ,[ef] ,[ef*] ...)
           (make-begin `(,ef ,ef* ...))]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(set! ,fp (+ ,fp ,n)) (guard (fpr? fp))
           (begin
             (set! current-offset (+ current-offset n))
             `(set! ,fp (+ ,fp ,n)))]
          [(set! ,fp (- ,fp ,n)) (guard (fpr? fp))
           (begin
             (set! current-offset (- current-offset n))
             `(set! ,fp (- ,fp ,n)))]
          [(return-point ,label ,[Tail -> tail])
           `(return-point ,label ,tail)]
          [(set! ,[Triv -> var] (,binop ,[Triv -> t1] ,[Triv -> t2]))
           `(set! ,var (,binop ,t1 ,t2))]
          [(set! ,[Triv -> var] ,[Triv -> t])
           `(set! ,var ,t)]
          [,st (error who "invalid syntax for Effect ~s" st)])))

    (define Tail
      (lambda (tail)
        (match tail
          [(,[Triv -> t]) `(,t)]
          [(begin ,[Effect -> ef*] ... ,[tail])
           (make-begin `(,ef* ... ,tail))]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [,tail (error who "invalid syntax for Tail ~s" tail)])))

    (lambda (program)
      (match program
        [(letrec ([,label* (lambda () ,[Tail -> tail*])] ...) ,[Tail -> tail])
         `(letrec ([,label* (lambda () ,tail*)] ...) ,tail)]
        [,program (error who "invalid syntax for Program: ~s" program)])))
  
  )
