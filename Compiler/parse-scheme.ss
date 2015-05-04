#|

A15 - May 2, 2015

pass: parse-scheme


This pass has several tasks to perform:

- verify that the syntax of the input program is correct;
- verify that there are no unbound variables;
- convert all variables to unique variables, handling the shadowing of identifiers (other variables, keyword names, and primitive names) correctly;
- convert unquoted constants into quoted constants;
- verify that each constant and quoted datum is well formed, with each fixnum in the fixnum range;
- rewrite not calls, and expressions, or expressions, and one-armed if expressions in terms of the other language expressions.

Input Grammar:
Program   ::= Expr
Expr      ::= Constant
            | Var
            | (quote Datum)
            | (if Expr Expr)
            | (if Expr Expr Expr)
            | (and Expr *)
            | (or Expr *)
            | (begin Expr * Expr)
            | (lambda (Var *) Expr +)
            | (let ([Var Expr] *) Expr +)
            | (letrec ([Var Expr] *) Expr +)
            | (set! Var Expr)
            | (prim Expr *)
            | (Expr Expr *)
Body      ::= Expr
Datum     ::= constant | (Datum *) | #(Datum *)
Constant  ::= fixnum | () | #t | #f
Var       ::= an arbitrary symbol

Output Grammar:
Prog      ::= Expr
Expr      ::= (quote Immediate)
              (let ([UVar Expr] *) Body)
              (letrec ([UVar Expr] *) Body)
              (lambda (UVar *) Body)
              (if Expr Expr Expr)
              (begin Expr * Expr)
              (set! UVar Expr)
              (ValPrim Expr *)
              (EffectPrim Expr *)
              (PredPrim Expr *)
              (Expr Expr *)
              UVar
Body      ::= Expr
Datum     ::= Immediate | (Datum *) | #(Datum *)
Immediate ::= fixnum | () | #t | #f

|#

(library (Compiler parse-scheme)
  (export parse-scheme)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common))

  (define-who parse-scheme
    (define primitives
      '((+ . 2) (- . 2) (* . 2) (<= . 2) (< . 2) (= . 2)
        (>= . 2) (> . 2) (boolean? . 1) (car . 1) (cdr . 1)
        (cons . 2) (eq? . 2) (fixnum? . 1) (make-vector . 1)
        (null? . 1) (pair? . 1) (procedure? . 1) (set-car! . 2)
        (set-cdr! . 2) (vector? . 1) (vector-length . 1)
        (vector-ref . 2) (vector-set! . 3) (void . 0)))
 
    ;; #t, #f, (), num, (datum* ...), #(datum* ...)
    (define (datum? x)
      (define (constant? x)
        (or (memq x '(#t #f ()))
            (and (and (integer? x) (exact? x))
                 (or (fixnum-range? x)
                     (errorf who "integer ~s is out of fixnum range" x)))))
      (or (constant? x)
          (if (pair? x)
              (and (datum? (car x)) (datum? (cdr x)))
              (and (vector? x) (andmap datum? (vector->list x))))))
    
    (define verify-x-list
      (lambda (x* x? what)
        (let loop ([x* x*] [idx* '()])
          (unless (null? x*)
            (let ([x (car x*)] [x* (cdr x*)])
              (unless (x? x)
                (errorf who "invalid ~s ~s found" what x))
              (let ([idx (extract-suffix x)])
                (when (member idx idx*)
                  (errorf who "non-unique ~s suffix ~s found" what idx))
                (loop x* (cons idx idx*))))))))

    (define (all-?-and-no-dups ls p? what)
      ;; for invalid cases like
      ;; (lambda (x x) ...) or
      ;; (let/rec ([x 1] [x 2]) ...)
      (let loop ([ls ls])
        (cond
         [(null? ls) (values)]
         [else
          (if (p? (car ls))
              (if (memq (car ls) (cdr ls))
                  (errorf who "Duplicate ~s found at ~s" what ls)
                  (loop (cdr ls)))
              (errorf who "Invalid ~s found at ~s" what ls))])))
    
    (define (Program x)
      (define all-uvar* '())
      (define (Expr env uvar*)
        (lambda (x)
          (match x
            [,d (guard (datum? d))
                `(quote ,d)]
            [,v (guard (symbol? v))
                (cond
                 [(assq v env) => cdr]
                 [else (errorf who "unbound identifier ~s" v)])]
            [(,f ,[rand*] ...) (guard (assq f env))
             ;; tricky!
             ;; for letrec, let, lambda ... rebindings
             `(,((Expr env uvar*) f) ,rand* ...)]
            [(quote ,d)
             (unless (datum? d)
               (errorf who "invalid datum ~s" d))
             `(quote ,d)]
            [(let ([,v* ,[expr*]] ...) ,e ,e* ...)
             (all-?-and-no-dups v* symbol? 'bindings)
             (let* ([new-uvar* (map (lambda (x) (unique-name x)) v*)]
                    [new-env (map cons v* new-uvar*)])
               (set! all-uvar* (append new-uvar* all-uvar*))
               (if (null? e*)
                   `(let ([,new-uvar* ,expr*] ...)
                      ,((Expr (append new-env env) (append new-uvar* uvar*))
                        e))
                   `(let ([,new-uvar* ,expr*] ...)
                      (begin ,@(map (Expr (append new-env env) (append new-uvar* uvar*))
                             (cons e e*))))))]
            [(letrec ([,v* ,expr*] ...) ,e ,e* ...)
             (all-?-and-no-dups v* symbol? 'bindings)
             (let* ([new-uvar* (map (lambda (x) (unique-name x)) v*)]
                    [new-env (map cons v* new-uvar*)])
               (set! all-uvar* (append new-uvar* all-uvar*))
               (if (null? e*)
                   `(letrec ([,new-uvar* ,(map (Expr (append new-env env) (append new-uvar* uvar*)) expr*)] ...)
                      ,((Expr (append new-env env) (append new-uvar* uvar*)) e))
                   `(letrec ([,new-uvar* ,(map (Expr (append new-env env) (append new-uvar* uvar*)) expr*)] ...)
                      (begin ,@(map (Expr (append new-env env) (append new-uvar* uvar*)) (cons e e*))))))]
            [(lambda (,v* ...) ,e ,e* ...)
             (all-?-and-no-dups v* symbol? 'parameters)
             (let* ([new-uvar* (map (lambda (x) (unique-name x)) v*)]
                    [new-env (map cons v* new-uvar*)])
               (set! all-uvar* (append new-uvar* all-uvar*))
               (if (null? e*)
                   `(lambda (,new-uvar* ...)
                      ,((Expr (append new-env env) (append new-uvar* uvar*)) e))
                   `(lambda (,new-uvar* ...)
                      (begin ,@(map (Expr (append new-env env) (append new-uvar* uvar*)) (cons e e*))))))]
            [(and ,expr* ...)
             (cond
              [(null? expr*) '#t]
              [(= (length expr*) 1)
               ((Expr env uvar*) (car expr*))]
              [else
               `(if ,((Expr env uvar*) (car expr*))
                    ,((Expr env uvar*) `(and ,(cdr expr*) ...))
                    '#f)])]
            [(or ,expr* ...)
             (cond
              [(null? expr*) '#f]
              [(= (length expr*) 1)
               ((Expr env uvar*) (car expr*))]
              [else
               (let ([tmp (unique-name 'tmp)])
                 `(let ([,tmp ,((Expr env uvar*) (car expr*))])
                    (if ,tmp
                        ,tmp
                        ,((Expr env uvar*) `(or ,(cdr expr*) ...)))))])]
            [(not ,[expr])
             `(if ,expr '#f '#t)]
            [(if ,[test] ,[then])
             `(if ,test ,then (void))]
            [(if ,[test] ,[then] ,[else])
             `(if ,test ,then ,else)]
            [(begin ,[expr*] ... ,[expr])
             `(begin ,expr* ... ,expr)]
            [(set! ,var ,[expr])
             (if (assq var env)
                 `(set! ,(cdr (assq var env)) ,expr)
                 (errorf who "unbound uvar ~s" var))]
            [(,prim ,[expr*] ...) (guard (assq prim primitives))
             (unless (= (length expr*) (cdr (assq prim primitives)))
               (errorf who "incorrect argument count: ~s" x))
             `(,prim ,expr* ...)]
            [(,[rator] ,[rand*] ...)
             `(,rator ,rand* ...)]
            [,el (errorf who "Invalid Expr ~s" el)])))

      (let ([x ((Expr '() '()) x)])
        (verify-x-list all-uvar* uvar? 'uvar)
        x))

    (lambda (x)
      (Program x)))
  )
