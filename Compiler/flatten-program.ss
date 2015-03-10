#|

A8 - Mar 6, 2015

pass: flatten-program

nothin' changed.

Input Grammar:
Program  ::= (letrec ((Label (lambda () Tail)) *) Tail) 
Tail     ::= (if (Relop Triv Triv) (Label) (Label))     
           | (begin Effect * Tail)
           | (Triv)                                      
Effect   ::= (set! Loc Triv)                            
           | (set! Loc (Binop Triv Triv))               
Triv     ::= Loc                                        
           | Integer | Label
Loc      ::= Reg | Disp | Ind                             ;; a8 new

Output Grammar:
Prog      ::= (code Statement * Statement)                ;; mod
Statement ::= (set! Loc Triv)                             ;; mod
            | (set! Loc (Binop Triv Triv))
            | (if (Relop Triv Triv) (jump Label))         ;; mod
            | (if (not (Relop Triv Triv)) (jump Label))   ;; mod
            | (jump Triv)                                 ;; mod
            | Label                                       ;; mod
Triv      ::= Loc                         
            | Integer | Label
Loc       ::= Reg | Disp | Ind            

|#

(library (Compiler flatten-program)
  (export flatten-program)
  (import (chezscheme)
    (Framework helpers)
    (Framework match))

  (define-who flatten-program
    (define Effect
      (lambda (ef)
        (match ef
          [(set! ,var ,rhs) `((set! ,var ,rhs))]
          [,ef (error who "invalid Effect ~s" ef)])))

    (define Tail
      (lambda (tail next-label)
        (match tail
          [(,t) (if (eq? t next-label) '() `((jump ,t)))]
          [(if ,test (,tlab) (,flab))
           (cond
            [(eq? flab next-label) `((if ,test (jump ,tlab)))]
            [(eq? tlab next-label) `((if (not ,test) (jump ,flab)))]
            [else `((if ,test (jump ,tlab)) (jump ,flab))])]
          [(begin ,[Effect -> ef-code**] ... ,[tail-code*])
           `(,ef-code** ... ... ,tail-code* ...)]
          [,tail (error who "invalid Tail ~s" tail)])))

    (lambda (x)
      (match x
        [(letrec ([,label* (lambda () ,tail*)] ...) ,tail)
         `(code
           ,@(let f ([tail tail] [label* label*] [tail* tail*])
               (if (null? tail*)
                   (Tail tail #f)
                   `(,(Tail tail (car label*)) ...
                     ,(car label*)
                     ,(f (car tail*) (cdr label*) (cdr tail*)) ...))))]
        [,x (error who "invalid Program ~s" x)])))

  )
