#|

A14 - Apr 18, 2014

pass: purify-letrec

This pass rewrites letrec expressions such that letrec expressions become pure: only bind unassigned variables to lambda expressions.

impure examples:

(letrec ((f (lambda (x) (+ x 1)))
         (g (lambda (y) (f (f y)))))
  (set! f (lambda (x) (- x 1)))
  (+ (f 1) (g 1)))

(letrec ((f (cons (lambda (x) x) 17))
         (g (lambda (y) ((car f) ((car f) y)))))
  (+ ((car f) (cdr f)) (g 1)))


Approach:
(letrec ([x e] ...) (assigned (x! ...) body))

1. CATA on e and body
2. Partition the bindings [x e] ... into three sets:
    - [xs es] ... simple
    - [xl el] ... lambda
    - [xc ec] ... complex
3. Produce a set of nested let and letrec expressions from the partitioned bindings and body as shown below:
(let ([xs es] ...)
  (assigned ()
    (let ([xc (void)] ...)
      (assigned (xc ...)
        (letrec ([xl el] ...)
          (let ([xt ec] ...)
            (assigned ()
              (set! xc xt)
              ...))
          body)))))
where xt ... are fresh temporaries. Produce the innermost let only if [xc ec] ... is nonempty, and produce begin expressions where needed.

A binding [x e] is considered

- simple: if x is not assigned (not in the list (x! ...)) and e is a simple expression;
- lambda: if x is not assigned and e is a lambda expression; and
- complex: otherwise.

Input:
Prog      ::= Expr
Expr      ::= (quote Immediate)                  
            | (let ([UVar Expr] *) Body)
            | (letrec ([UVar Expr] *) Body)
            | (lambda (UVar *) Body)
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (set! UVar Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar
Body      ::= (assigned (UVar *) Expr)          
Datum     ::= Immediate | (Datum *) | #(Datum *)
Immediate ::= fixnum | () | #t | #f

Output:
Prog      ::= Expr
Expr      ::= (quote Immediate)                  
            | (let ([UVar Expr] *) Body)
            | (letrec ([UVar Lamb] *) Body)       ;; mod     
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (set! UVar Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar
Lamb      ::= (lambda (UVar *) Body)              ;; mod
Body      ::= (assigned (UVar *) Expr)          
Datum     ::= Immediate | (Datum *) | #(Datum *)
Immediate ::= fixnum | () | #t | #f

|#

(library (Compiler purify-letrec)
  (export purify-letrec)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common))

  (define-who purify-letrec

    (define foldl
      (lambda (f n ls)
        (cond
         [(null? ls) n]
         [else (foldl f (f n (car ls)) (cdr ls))])))
    
    (define (simple? letrec-vars e)
      (call/cc ;; wow. such call/cc. so stylish.
       (lambda (root)

         (define (loop letrec-vars in-lam? cc)
           (lambda (e)
             (match e
               [(quote ,d) (guard (immediate-with-d? d)) #t]
               [(let ([,uvar* ,[expr*]] ...) (assigned (,asv* ...) ,[expr]))
                #t]
               [(letrec ([,uvar* ,expr*] ...) ,[body])
                (let* ([new-lvars (hunion uvar* letrec-vars)]
                       [_ (map (loop new-lvars in-lam? cc) expr*)])
                  #t)]
               [(lambda (,uvar* ...)
                  (assigned (,asv* ...)
                    ,[(loop letrec-vars #t cc) -> body]))
                #t]
               [(if ,[test] ,[then] ,[else])
                #t]
               [(begin ,[expr*] ... ,[expr])
                #t]
               [(set! ,uvar ,[expr])
                #t]
               [(,prim ,[expr*] ...) (guard (prim? prim))
                #t]
               [(,[rator] ,[rand*] ...)
                (if in-lam?
                    #t
                    (cc #f))] ;; found you! bye.
               [,uvar (guard (uvar? uvar))
                (if (memq uvar letrec-vars)
                    (cc #f) ;; found you! bye.
                    #t)]
               [,el (errorf who "Invalid Stuff in simple? ~s" el)])))

         ((loop letrec-vars #f root) e))))

    (define (not-assigned? x as-ls)
      (not (memq x as-ls)))
    
    (define (partition bind as-ls letrec-vars)
      (let loop ([bind bind]
                 [simple '()]
                 [lam '()]
                 [complex '()])
        (cond
         [(null? bind) (values simple lam complex)]
         [else
          (let ([x (caar bind)] [e (cadar bind)])
            (cond
             [(and (simple? letrec-vars e)
                   (not-assigned? x as-ls))
              (loop (cdr bind)
                    (cons (car bind) simple)
                    lam
                    complex)]
             [(and (not-assigned? x as-ls)
                   (eq? 'lambda (car e)))
              (loop (cdr bind)
                    simple
                    (cons (car bind) lam)
                    complex)]
             [else
              (loop (cdr bind)
                    simple
                    lam
                    (cons (car bind) complex))]))])))

    (define (Expr expr)
      (match expr
        [(quote ,d) (guard (immediate-with-d? d))
         `(quote ,d)]
        [(let ([,uvar* ,[expr*]] ...) (assigned (,asv* ...) ,[expr]))
         `(let ([,uvar* ,expr*] ...) (assigned (,asv* ...) ,expr))]
        [(letrec ([,uvar* ,[expr*]] ...)
           (assigned (,asv* ...) ,[expr]))
         (let-values ([(simple lam complex)
                       (partition `([,uvar* ,expr*] ...)
                                  `(,asv* ...)
                                  uvar*)])
           (let* ([x_c (map car complex)]
                  [e_c (map cadr complex)]
                  [x_t (foldl (lambda (acc var)
                                (cons (unique-name (string->symbol (extract-root var))) acc))
                              '()
                              x_c)])        
             (cond
              [(null? complex)
               `(let ,simple
                  (assigned ()
                    (let ([,x_c (void)] ...)
                      (assigned ,x_c
                        (letrec ,lam
                          ,expr)))))]
              [else
               `(let ,simple
                  (assigned ()
                    (let ([,x_c (void)] ...)
                      (assigned ,x_c
                        (letrec ,lam
                          (let ([,x_t ,e_c] ...)
                            (assigned ()
                              ,(make-begin
                                `((set! ,x_c ,x_t) ... ,expr)))))))))])))]
        [(lambda (,uvar* ...) (assigned (,asv* ...) ,[expr]))
         `(lambda (,uvar* ...) (assigned (,asv* ...) ,expr))]
        [(if ,[test] ,[then] ,[else])
         `(if ,test ,then ,else)]
        [(begin ,[expr*] ... ,[expr])
         `(begin ,expr* ... ,expr)]
        [(set! ,uvar ,[expr])
         `(set! ,uvar ,expr)]
        [(,prim ,[expr*] ...) (guard (prim? prim))
         `(,prim ,expr* ...)]
        [(,[rator] ,[rand*] ...)
         `(,rator ,rand* ...)]
        [,uvar (guard (uvar? uvar)) uvar]
        [,el (errorf who "Invalid Expr ~s" el)]))
    
    (lambda (prog)
      (Expr prog)))
  )

